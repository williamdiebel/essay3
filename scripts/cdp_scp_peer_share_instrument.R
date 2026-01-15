setwd("~/Dropbox/NetZero WD Shared Folder/Data")
library(tidyverse)
library(data.table)
library(fixest)
library(sandwich)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Preamble ####
# The purpose of this script is twofold:
#   1. build the panel extension (2021 - 2022) for target setting firms (prior
#      to 2022, as contained in the exclude_all.rds file). 
#   2. create the cdp_peer_share instrument.
#   3. run the 2SLS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Step by step overview ####
#   1. import exclude_all to get a list of the appropriate gvkeys
#   2. extract extant 2007 - 2020 panel from: 
#      rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds
#   3. match firms to cdp scp member data in 2021, 2022
#   4. create extended version of cdp_sc_member variable
#   5. add target_setter_exclude == 1 variable for all target setter observations
#   6. merge extended panel with panel_extended.rds
#   7. compute the peer share instrument
#   8. save and export as panel_extended_instrument.rds
#   9. run the 2SLS model
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   1. import exclude_all to get a list of the appropriate gvkeys ####
exclude_all <- readRDS("exclude_all.rds")
gvkeys <- unique(exclude_all$gvkey)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   2. extract extant 2007 - 2020 panel ####
#      from: rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds
panel_extant <- readRDS("rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds")
panel_extant <- panel_extant %>% filter(gvkey %in% gvkeys)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   3. match firms to cdp scp member data in 2021, 2022 ####
cdp_scp_members <- readxl::read_excel(
  "~/Dropbox/My Mac (Will’s Laptop)/Documents/Data/CDP/SupplyChainMembers_cleaned.xlsx", 
  sheet = "2021 - 2023")
panel_merging_table <- panel_extant %>% select(gvkey, conm)
panel_merging_table <- distinct(panel_merging_table)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Name cleaning and exact matching ####

  ### Setting up the name cleaning pipeline ####
  
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

  ### Implementing exact matching with the above functions ####

cdp_scp_members_cleaned <- cdp_scp_members %>% 
  mutate(name_std = clean_company_names(cdp_scp_name))
panel_merging_table_cleaned <- panel_merging_table %>% 
  mutate(name_std = clean_company_names(conm))

# Perform an exact inner join on the standardized name
exact_matches <- inner_join(
  cdp_scp_members_cleaned, 
  panel_merging_table_cleaned, 
  by = "name_std", 
  suffix = c("_cdp_scp_members_cleaned", 
             "_panel_merging_table_cleaned"))

  # 63 unique firms are identified/matched
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Fuzzy matching for remaining cases ####
# Identify unmatched entries after exact matching:
unmatched_cdp_scp_members_cleaned <- anti_join(
  cdp_scp_members_cleaned, 
  panel_merging_table_cleaned, 
  by = "name_std")
unmatched_panel_merging_table_cleaned <- anti_join(
  panel_merging_table_cleaned, 
  cdp_scp_members_cleaned, by = "name_std")

# Fuzzy join on the standardized name, using a string distance threshold:
unmatched_cdp   <- unmatched_cdp_scp_members_cleaned  %>%
  mutate(row_id_cdp = row_number())

unmatched_rr    <- unmatched_panel_merging_table_cleaned %>%
  mutate(row_id_rr  = row_number())

### Build a single character-3-gram TF-IDF matrix ----------------
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

### Split back into the two groups -------------------------------
n_cdp <- nrow(unmatched_cdp)               # first block of rows
cdp_mat <- dtm_tfidf[1:n_cdp, ]
rr_mat  <- dtm_tfidf[(n_cdp + 1):nrow(dtm_tfidf), ]


#### cosine similarity -------------------------
# L2-normalise each row once
cdp_norm <- text2vec::normalize(cdp_mat, "l2")
rr_norm  <- text2vec::normalize(rr_mat,  "l2")

# sparse cross-product: CDP × RR  (N_cdp × N_rr)
sim_full <- cdp_norm %*% t(rr_norm)   # <-- keep this master copy

##### choose a cut-off and slice a *copy* --------------------------
cutoff   <- 0.65          # try 0.80, 0.85, …; change freely

sim_mat  <- sim_full      # copy the full matrix
sim_mat@x[ sim_mat@x < cutoff ] <- 0
sim_mat  <- drop0(sim_mat)          # drop structural zeros

##### Convert to tidy pairs ----------------------------------------
matches_long <- summary(sim_mat) |>
  as_tibble() |>
  rename(row_id_cdp = i,      # summary() is already 1-based
         row_id_rr  = j,
         cosine_sim = x)

##### Join back to full rows + add a match-type flag ---------------
tfidf_matches <- matches_long %>%
  inner_join(unmatched_cdp, by = "row_id_cdp") %>%
  inner_join(unmatched_rr,  by = "row_id_rr", suffix = c("_cdp", "_rr")) %>%
  select(-row_id_cdp, -row_id_rr) %>%
  mutate(match_type     = "TFIDF",
         match_distance = 1 - cosine_sim)   # optional: distance
tfidf_matches %>% select(name_std_cdp, name_std_rr) %>% view()

###### Inspect / combine as desired ---------------------------------
all_matches <- bind_rows(exact_matches, tfidf_matches)
write_excel_csv(all_matches, "all_matches_target_setters.csv") # exporting for manual cleaning
all_matches_cleaned <- read_csv("all_matches_target_setters_cleaned.csv",
                                col_types = "ncc") #importing the cleaned dataset
cdp_scp_members_gvkey <- merge(cdp_scp_members, all_matches_cleaned)
saveRDS(cdp_scp_members_gvkey, "cdp_scp_members_gvkey_target_setters.rds")
  # inspecting target setters that did not match
panel_merging_table_cleaned %>% filter(!
                                         (panel_merging_table_cleaned$gvkey %in% 
                               cdp_scp_members_gvkey$gvkey)
                               ) %>% 
  select(conm) %>% 
  unique() %>% 
  view()

# Examples: "L'OREAL SA", "ADIDAS AG", "FERROVIAL SA"
  
  # first, let's check whether L'Oreal, Adidas, and Ferrovial can be manually
  # identified in cdp_scp_members_cleaned. If so, then I need to manually
  # add their gvkeys to the cdp_scp_members_gvkey table.

  cdp_scp_members_cleaned$cdp_scp_name[
    str_which(cdp_scp_members_cleaned$cdp_scp_name, "L’Oréal")
  ] # L’Oréal
  cdp_scp_members_cleaned$cdp_scp_name[
    str_which(cdp_scp_members_cleaned$cdp_scp_name, "L'O")
  ] # L'Oréal {{{
      # two different forms -- this is particularly tricky (notice apostrophe 
      # discrepency. need to address both below.
  
  cdp_scp_members_cleaned$cdp_scp_name[
    str_which(cdp_scp_members_cleaned$cdp_scp_name, "Adidas")
  ] # none, all good
  cdp_scp_members_cleaned$cdp_scp_name[
    str_which(cdp_scp_members_cleaned$cdp_scp_name, "Ferrovial")
  ] # none, all good
  cdp_scp_members_cleaned$cdp_scp_name[
    str_which(cdp_scp_members_cleaned$cdp_scp_name, "Interface")
  ] # none, all good
  cdp_scp_members_cleaned$cdp_scp_name[
    str_which(cdp_scp_members_cleaned$cdp_scp_name, "Tesco")
  ] # none, all good

  # before understanding how to address potential false negatives, to recap, 
  # what i've done so far is:
  # combined
    panel_merging_table_cleaned # this
  # and 
    cdp_scp_members_cleaned # this
  # to produce
    cdp_scp_members_gvkey # this.
  # so, the key task, if L'Oreal is in both - is to extract year and 
  # cdp_scp_name from cdp_scp_members_cleaned and gvkey from 
  # panel_merging_table_cleaned to add to scp_scp_members_gvkey
    cdp_scp_members_gvkey <- rbind(cdp_scp_members_gvkey,
    cdp_scp_members_cleaned %>% filter(cdp_scp_name == "L’Oréal") %>% 
      select(year, cdp_scp_name) %>% mutate(gvkey = unique(
        panel_merging_table_cleaned$gvkey[which(
          panel_merging_table_cleaned$conm == "L'OREAL SA")
          ]
        )
      )
    )
    cdp_scp_members_gvkey <- rbind(cdp_scp_members_gvkey,
    cdp_scp_members_cleaned %>% filter(cdp_scp_name == "L'Oréal {{{") %>% 
      select(year, cdp_scp_name) %>% mutate(gvkey = unique(
        panel_merging_table_cleaned$gvkey[which(
          panel_merging_table_cleaned$conm == "L'OREAL SA")
          ]
        )
      )
    )
    
# 4. create extended version of cdp_sc_member variable ####

    # first, extending the panel to 2021 - 2022

    # creates a framework with necessary demographic variables (1 ob for each
    # distinct firm)
  panel_extended_target_setters <- panel_extant %>% select(gvkey, conm,
                                                           reprisk_id,
                                                           factset_id,
                                                           FourDigitName,
                                                           headquarter_country)
  panel_extended_target_setters <- distinct(panel_extended_target_setters)
    # doubling the table so each firm has 2 obs (one for 2021, one for 2022)
panel_extended_target_setters <- rbind(panel_extended_target_setters,
                                       panel_extended_target_setters)
    # adding in the year variable, writes 2021 once for each firm, then 2022
    # once for each firm.
panel_extended_target_setters$year <- c(rep(2021, 680), rep(2022, 680))

    # next, i can create the cdp_sc_member variable by merging the extended
    # panel with cdp_scp_members_gvkey (merge by gvkey and year to add the
    # name). Those with non NA values, cdp_sc_member == 1, while those with NA
    # values, will have cdp_sc_member == 0.
panel_extended_target_setters <- merge(panel_extended_target_setters,
      cdp_scp_members_gvkey,
      all.x = T) 
panel_extended_target_setters$cdp_sc_member <- ifelse(is.na(
  panel_extended_target_setters$cdp_scp_name),
  0,
  1)
    # Then, I can merge the extended panel with the extant panel and export
    # as the extended panel for target setters.
panel_extended_target_setters <- bind_rows(
  panel_extended_target_setters,
  panel_extant
)

#   5. add target_setter_exclude == 1 variable for all target setter observations ####
        # adding the target_setter_exclude variable
panel_extended_target_setters$target_setter_exclude <- rep(
  1, nrow(panel_extended_target_setters))
saveRDS(panel_extended_target_setters, "panel_extended_target_setters.rds")
#   6. merge extended panel with panel_extended.rds ####

panel_extended_complete <- bind_rows(
  panel_extended_target_setters,
  panel_extended
)

panel_extended_complete <- panel_extended_complete %>% arrange(gvkey, year)

saveRDS(panel_extended_complete, "panel_extended_complete.rds")
panel_extended_complete <- readRDS("panel_extended_complete.rds")
#   7. compute the peer share instrument ####
# converting to data table for speed
setDT(panel_extended_complete)
  # (a) industry-year peer share EXCLUDING focal firm
panel_extended_complete[ , peer_cdp_share :=
                      ifelse(.N == 1, NA_real_,     # single-firm industry-year
                             (sum(cdp_sc_member, na.rm = TRUE) - cdp_sc_member) /
                               (.N - 1)),
                    by = .(FourDigitName, year)]
  # (b) lag by one year for each firm
panel_extended_complete[ , peer_cdp_share_lag := shift(peer_cdp_share, n = 1, type = "lag"),
                    by = gvkey]

  # replace NA from first year or single-firm groups with 0
panel_extended_complete[is.na(peer_cdp_share_lag), peer_cdp_share_lag := 0]

  # quick sanity check
summary(panel_extended_complete$peer_cdp_share)

#   8. save and export as panel_extended_instrument.rds ####
panel_extended_instrument <- panel_extended_complete
saveRDS(panel_extended_instrument, "panel_extended_instrument.rds")
complete_data_2022 <- readRDS("complete_data_2022.rds")
panel_extended_instrument_pruned <- panel_extended_instrument %>%
  select(gvkey, year, peer_cdp_share, peer_cdp_share_lag)
complete_data_2022_instrument <- complete_data_2022 %>%
  left_join(panel_extended_instrument_pruned,
            by = c("gvkey", "year")) %>% ungroup()
saveRDS(complete_data_2022_instrument, "complete_data_2022_instrument.rds")
complete_data_2022_instrument <- readRDS("complete_data_2022_instrument.rds")

#   9. run the 2SLS model ####

## First-stage: Instrument relevance test ####
# Controls identical to baseline model
first_stage <- feols(
  cdp_sc_member ~ peer_cdp_share_lag +
    esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
    tll_lt_w1_at_w1 |                          # fixed effects
    headquarter_country + as.factor(year),
  data    = complete_data_2022_instrument,
  cluster = ~ gvkey
)

# Print concise table
summary(first_stage)

# Extract instrument F-statistic (or t-stat)
#  For a single IV, F = t^2

t_iv <- coeftable(first_stage)["peer_cdp_share_lag", 't value']
F_iv <- t_iv^2
cat("\nFirst-stage strength:\n  t-stat =", round(t_iv, 2),
    " →  F-stat =", round(F_iv, 2), "\n")

# Rule of thumb: F ≥ 10 ⇒ strong instrument

    # First-stage strength:
    #   t-stat = 3.43  →  F-stat = 11.79

# Second‐stage: 2SLS of SBTi commitment on instrumented CDP SCP membership ####

# install.packages("lfe")  # if you haven’t already
library(lfe)

# 1) Coerce FEs to factors (just in case)
complete_data_2022_instrument <- complete_data_2022_instrument %>%
  mutate(
    headquarter_country = as.factor(headquarter_country),
    FourDigitName       = as.factor(FourDigitName),
    year                = as.factor(year),
    gvkey                = as.factor(gvkey)
  )

# 2) 2SLS with felm, clustering on gvkey via the 4th pipe
second_stage_felm <- felm(
  # (1) structural eq: DV ~ exogenous
  sbti_commitment_lead1 ~ 
    esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
    tll_lt_w1_at_w1
  # (2) fixed effects
  | headquarter_country + year
  # (3) IV part: endogenous ~ instrument
  | (cdp_sc_member ~ peer_cdp_share_lag)
  # (4) cluster on gvkey 
  | gvkey,
  data = complete_data_2022_instrument
)

summary(second_stage_felm)

# 10. Baseline models ####
# Main fixed effects LPM model specifications ####
## Country fixed effects ####
summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents_highreach +
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | headquarter_country + year,
    data = complete_data_2022,
    cluster = ~gvkey
  )
)
## Adds industry fixed effects ####
summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents_highreach +
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | headquarter_country + FourDigitName + year,
    data = complete_data_2022,
    cluster = ~gvkey
  )
)
## Adds firm fixed effects ####

summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents_highreach + 
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | gvkey + year,
    data = complete_data_2022,
    cluster = ~gvkey
  )
)

# 11. Tobit models ####


