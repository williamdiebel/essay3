setwd("~/Dropbox/NetZero WD Shared Folder/Data")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Takes the latest CDP SCP member names, cleans them, and matches them to the
# existing dataset.
# Preamble ####
# Necessary packages
library(tidyverse)
library(fuzzyjoin)
library(stringdist)
library(text2vec)
library(Matrix)
# Reading in data.
cdp_scp_members <- readxl::read_excel(
  "~/Dropbox/My Mac (Will’s Laptop)/Documents/Data/CDP/SupplyChainMembers_cleaned.xlsx", 
  sheet = "2021 - 2023")
rrpanel_sbti_commitments_2024 <- readRDS("rrpanel_sbti_commitments_2024.rds")
rrpanel_merging_table <- rrpanel_sbti_commitments_2024 %>%
  select(gvkey, conm)
rrpanel_merging_table <- distinct(rrpanel_merging_table)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Name cleaning and exact matching ####

  ## Setting up the name cleaning pipeline ####
  
  # Dictionary of common abbreviations to normalize (pattern -> replacement).
  # Using regex word boundaries (\\b) to match whole words (case-insensitive 
  # because we'll uppercase first).
  
suffix_map <- c(
  "\\bCO\\b"   = "COMPANY",
  "\\bCORP\\b" = "CORPORATION",
  "\\bLTD\\b"  = "LIMITED",
  "\\bINC\\b"  = "INCORPORATED",
  "\\bMGMT\\b"  = "MANAGEMENT",
  "\\bINTL\\b"  = "INTERNATIONAL",
  "\\bINT\\b"  = "INTERNATIONAL",
  "\\bHLDGS\\b"  = "HOLDINGS",
  "\\bHLDG\\b"  = "HOLDING",
  "\\bGRP\\b"  = "GROUP",
  "\\bTECH\\b"  = "TECHNOLOGY"
  # Add more as needed
)

clean_company_names <- function(names) {
  names %>%
    # 1. Remove punctuation by replacing with empty string:
      # Replace punctuation with space to avoid merging words
    str_replace_all("[[:punct:]]", " ") %>%    
    # 2. Convert to uppercase:
    str_to_upper() %>%
    # 3. Trim whitespace and collapse multiple spaces into one:
    str_squish() %>%
    # 4. Replace common abbreviations/suffixes using the mapping:
    str_replace_all(suffix_map) %>%
    # 5. Trim again in case replacements introduced any extra spaces:
    str_squish()
}

  ## Implementing exact matching with the above functions ####

cdp_scp_members_cleaned <- cdp_scp_members %>% 
  mutate(name_std = clean_company_names(cdp_scp_name))
rrpanel_merging_table_cleaned <- rrpanel_merging_table %>% 
  mutate(name_std = clean_company_names(conm))

# Perform an exact inner join on the standardized name
exact_matches <- inner_join(
  cdp_scp_members_cleaned, 
  rrpanel_merging_table_cleaned, 
  by = "name_std", 
  suffix = c("_cdp_scp_members_cleaned", 
             "_rrpanel_merging_table_cleaned"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Fuzzy matching for remaining cases ####
# Identify unmatched entries after exact matching:
unmatched_cdp_scp_members_cleaned <- anti_join(
  cdp_scp_members_cleaned, 
  rrpanel_merging_table_cleaned, 
  by = "name_std")
unmatched_rrpanel_merging_table_cleaned <- anti_join(
  rrpanel_merging_table_cleaned, 
  cdp_scp_members_cleaned, by = "name_std")

# Fuzzy join on the standardized name, using a string distance threshold:
unmatched_cdp   <- unmatched_cdp_scp_members_cleaned  %>%
  mutate(row_id_cdp = row_number())

unmatched_rr    <- unmatched_rrpanel_merging_table_cleaned %>%
  mutate(row_id_rr  = row_number())

# ------------------------------------------------------------------
# 2.  Build a single character-3-gram TF-IDF matrix ----------------
# combine the two name vectors so TF & IDF are learned on the
# *same* vocabulary
all_names <- c(unmatched_cdp$name_std, unmatched_rr$name_std)

# tokeniser: split into overlapping 3-grams (soft-TFIDF works best
# on character n-grams for short strings such as company names)
char3_tokeniser <- function(x) {
  # x is a character vector (length ≥ 1). We must return a list.
  lapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(character(0))   # skip NA/empty
    s <- gsub("\\s+", "", s)                           # drop spaces
    L <- nchar(s)
    if (L < 3) return(s)                               # too short → itself
    substring(s, 1:(L - 2), 3:L)                       # all overlapping 3-grams
  })
}


it  <- itoken(all_names,
              tokenizer   = char3_tokeniser,
              progressbar = FALSE)

vocab      <- create_vocabulary(it)          # ✓ no error
vectoriser <- vocab_vectorizer(vocab)
dtm        <- create_dtm(it, vectoriser)
tfidf      <- TfIdf$new()
dtm_tfidf  <- tfidf$fit_transform(dtm)


# ------------------------------------------------------------------
# 3.  Split back into the two groups -------------------------------
n_cdp <- nrow(unmatched_cdp)               # first block of rows
cdp_mat <- dtm_tfidf[1:n_cdp, ]
rr_mat  <- dtm_tfidf[(n_cdp + 1):nrow(dtm_tfidf), ]


# -------------------- 4. cosine similarity -------------------------
# L2-normalise each row once
cdp_norm <- text2vec::normalize(cdp_mat, "l2")
rr_norm  <- text2vec::normalize(rr_mat,  "l2")

# sparse cross-product: CDP × RR  (N_cdp × N_rr)
sim_full <- cdp_norm %*% t(rr_norm)   # <-- keep this master copy
# ------------------------------------------------------------------

# 4b. choose a cut-off and slice a *copy* --------------------------
cutoff   <- 0.65          # try 0.80, 0.85, …; change freely

sim_mat  <- sim_full      # copy the full matrix
sim_mat@x[ sim_mat@x < cutoff ] <- 0
sim_mat  <- drop0(sim_mat)          # drop structural zeros
# ------------------------------------------------------------------

# 5.  Convert to tidy pairs ----------------------------------------
matches_long <- summary(sim_mat) |>
  as_tibble() |>
  rename(row_id_cdp = i,      # summary() is already 1-based
         row_id_rr  = j,
         cosine_sim = x)

# ------------------------------------------------------------------
# 6.  Join back to full rows + add a match-type flag ---------------
tfidf_matches <- matches_long %>%
  inner_join(unmatched_cdp, by = "row_id_cdp") %>%
  inner_join(unmatched_rr,  by = "row_id_rr", suffix = c("_cdp", "_rr")) %>%
  select(-row_id_cdp, -row_id_rr) %>%
  mutate(match_type     = "TFIDF",
         match_distance = 1 - cosine_sim)   # optional: distance
tfidf_matches %>% select(name_std_cdp, name_std_rr) %>% view()

# ------------------------------------------------------------------
# 7.  Inspect / combine as desired ---------------------------------
all_matches <- bind_rows(exact_matches, tfidf_matches)
write_excel_csv(all_matches, "all_matches.csv") # exporting for manual cleaning
all_matches_cleaned <- read_csv("all_matches_cleaned.csv",
                                col_types = "ncc") #importing the cleaned dataset
cdp_scp_members_gvkey <- merge(cdp_scp_members, all_matches_cleaned)
saveRDS(cdp_scp_members_gvkey, "cdp_scp_members_gvkey.rds")
  # inspecting firms that did not match
cdp_scp_members %>% filter(!(cdp_scp_members$cdp_scp_name %in% 
                               cdp_scp_members_gvkey$cdp_scp_name)) %>% view()
# many firms such as L'Oreal, Walmart, Microsoft are not matched
# my guess is that these firms are in the exclude_all table as they
# had approved targets prior to 2022.
exclude_all <- readRDS("exclude_all.rds")
exclude_all$company_name[str_which(exclude_all$company_name, "NIKE")] # yes
exclude_all$company_name[str_which(exclude_all$company_name, "Walmart")] # yes
exclude_all$company_name[str_which(exclude_all$company_name, "Microsoft")] # yes
# ------------------------------------------------------------------
# 8.  adding cdp_sc_name to panel ---------------------------
rrpanel_sbti_commitments_cdp_scp_members_2024 <- merge(
  rrpanel_sbti_commitments_2024,
  cdp_scp_members_gvkey,
  all.x = TRUE)
saveRDS(rrpanel_sbti_commitments_cdp_scp_members_2024,
        "rrpanel_sbti_commitments_cdp_scp_members_2024.rds")
# 9.  data exporting and other misc tasks ---------------------------

  # doing a quick exloratory check on how many cdp scp members are in the 
  # updated panel (which excludes the "exclude_all" firms)
  rrpanel_sbti_commitments_cdp_scp_members_2024 %>% filter(!(is.na(cdp_scp_name))
                                                           |
                                                             cdp_sc_member == 1) %>%
    select(gvkey) %>% n_distinct() # [1] 89
  # compared to the panel with all firms
  rrpanel <- readRDS("rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds")
  rrpanel %>% filter(cdp_sc_member == 1) %>% select(gvkey) %>% 
    n_distinct() # [1] 186

  # quick check on the number of firms in the updated panel with sbti 
  # commitments 
  rrpanel_sbti_commitments_cdp_scp_members_2024 %>% 
    filter(!is.na(initial_commitment_year)) %>% select(gvkey) %>% 
    n_distinct() # [1] 973
  # this number is about the same magnitude as the number of exluded firms that
  # already had targets set prior to 2022 (680).
  
# outputting a list of gkveys to use in subsequent data collection (2021 - 2023)
  gvkey_2021_2023 <- rrpanel_sbti_commitments_cdp_scp_members_2024 %>% filter(year == 2020) %>% 
    select(gvkey)
  
  # importantly, I need to pad each gvkey with zeros if an element is fewer than
  # 6 characters. This is an artifact of excel handling the data and needs to
  # be corrected prior to querying wrds.
  saveRDS(gvkey_2021_2023, "gvkey_2021_2023.rds")
  gvkey_2021_2023 <- gvkey_2021_2023 %>% 
    mutate(gvkey = str_pad(gvkey, width = 6, side = "left", pad = "0"))
  write_lines(gvkey_2021_2023$gvkey, "gvkey_2021_2023.txt")
  