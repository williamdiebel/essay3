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
#   7. compute the peer share instruments ####
# converting to data table for speed
setDT(panel_extended_complete)

# ──────────────────────────────────────────────────────────────────────────────
# INSTRUMENT 1: Industry-based peer share (peer_cdp_share_lag)
# ──────────────────────────────────────────────────────────────────────────────
# This instrument captures the share of firms in the SAME INDUSTRY-YEAR that are
# CDP Supply Chain members, EXCLUDING the focal firm itself.
#
# Why exclude the focal firm?
#   → We want to measure PEER behavior, not the firm's own choice
#   → This avoids mechanical correlation (reflection problem)
#
# Logic:
#   1. For each industry-year combination, count total CDP members
#   2. Subtract 1 if the focal firm is a member
#   3. Divide by (N - 1) where N is total firms in that industry-year
#   4. Lag by one year to ensure temporal precedence (peers influence future)
# ──────────────────────────────────────────────────────────────────────────────

  # (a) industry-year peer share EXCLUDING focal firm
panel_extended_complete[ , peer_cdp_share :=
                      ifelse(.N == 1, NA_real_,     # single-firm industry-year
                             (sum(cdp_sc_member, na.rm = TRUE) - cdp_sc_member) /
                               (.N - 1)),
                    by = .(FourDigitName, year)]

  # (b) lag by one year for each firm (so we use LAST year's peer behavior)
panel_extended_complete[ , peer_cdp_share_lag := shift(peer_cdp_share, n = 1, type = "lag"),
                    by = gvkey]

  # (c) replace NA from first year or single-firm groups with 0
  #     (conservative approach: missing = no peer influence)
panel_extended_complete[is.na(peer_cdp_share_lag), peer_cdp_share_lag := 0]

  # Quick sanity check: what's the distribution of industry peer shares?
cat("\n──────────────────────────────────────────────────────────────\n")
cat("Summary of INDUSTRY-based peer CDP share (lagged):\n")
cat("──────────────────────────────────────────────────────────────\n")
summary(panel_extended_complete$peer_cdp_share_lag)

# ──────────────────────────────────────────────────────────────────────────────
# INSTRUMENT 2: Country-based peer share (peer_cdp_share_country_lag)
# ──────────────────────────────────────────────────────────────────────────────
# This instrument captures the share of firms in the SAME COUNTRY-YEAR that are
# CDP Supply Chain members, EXCLUDING the focal firm itself.
#
# Why use country as an alternative peer group?
#   → Country-level institutional pressures (regulations, norms, media)
#   → Complements industry-based instrument for overidentification tests
#   → Different "peer" definition may have independent variation
#
# Logic: Identical to industry instrument but groups by country instead
# ──────────────────────────────────────────────────────────────────────────────

  # (a) country-year peer share EXCLUDING focal firm
panel_extended_complete[ , peer_cdp_share_country :=
                      ifelse(.N == 1, NA_real_,     # single-firm country-year
                             (sum(cdp_sc_member, na.rm = TRUE) - cdp_sc_member) /
                               (.N - 1)),
                    by = .(headquarter_country, year)]

  # (b) lag by one year for each firm (use LAST year's country peers)
panel_extended_complete[ , peer_cdp_share_country_lag := shift(peer_cdp_share_country, n = 1, type = "lag"),
                    by = gvkey]

  # (c) replace NA from first year or single-firm groups with 0
panel_extended_complete[is.na(peer_cdp_share_country_lag), peer_cdp_share_country_lag := 0]

  # Quick sanity check: what's the distribution of country peer shares?
cat("\n──────────────────────────────────────────────────────────────\n")
cat("Summary of COUNTRY-based peer CDP share (lagged):\n")
cat("──────────────────────────────────────────────────────────────\n")
summary(panel_extended_complete$peer_cdp_share_country_lag)

#   8. save and export as panel_extended_instrument.rds ####
panel_extended_instrument <- panel_extended_complete
saveRDS(panel_extended_instrument, "panel_extended_instrument.rds")
complete_data_2022 <- readRDS("complete_data_2022.rds")

# Select BOTH instruments (industry and country) to merge into main dataset
panel_extended_instrument_pruned <- panel_extended_instrument %>%
  select(gvkey, year, peer_cdp_share, peer_cdp_share_lag,
         peer_cdp_share_country, peer_cdp_share_country_lag)

# Merge instruments into the main analysis dataset
complete_data_2022_instrument <- complete_data_2022 %>%
  left_join(panel_extended_instrument_pruned,
            by = c("gvkey", "year")) %>% ungroup()

saveRDS(complete_data_2022_instrument, "complete_data_2022_instrument.rds")
complete_data_2022_instrument <- readRDS("complete_data_2022_instrument.rds")

#   9. run the 2SLS model with BOTH instruments ####

# ══════════════════════════════════════════════════════════════════════════════
# TWO-STAGE LEAST SQUARES (2SLS) ESTIMATION
# ══════════════════════════════════════════════════════════════════════════════
# Endogenous variable: cdp_sc_member (CDP Supply Chain Program membership)
# Instruments:
#   1. peer_cdp_share_lag (industry peers' CDP membership)
#   2. peer_cdp_share_country_lag (country peers' CDP membership)
# Outcome: sbti_commitment_lead1 (Science-Based Targets initiative commitment)
# ══════════════════════════════════════════════════════════════════════════════

# Load required packages for IV estimation
library(lfe)      # For felm() function (2SLS with fixed effects)
library(AER)      # For ivreg() and diagnostic tests
library(sandwich) # For robust standard errors
library(lmtest)   # For coefficient tests

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                         FIRST-STAGE REGRESSIONS                              \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# FIRST-STAGE: Test instrument RELEVANCE
# ──────────────────────────────────────────────────────────────────────────────
# Question: Do our instruments predict CDP membership?
#
# We regress the endogenous variable (cdp_sc_member) on BOTH instruments
# plus all control variables and fixed effects.
#
# What we're looking for:
#   ✓ Instruments should be STATISTICALLY SIGNIFICANT predictors
#   ✓ F-statistic should be > 10 (rule of thumb for "strong" instruments)
#   ✓ Higher F-stats = less weak instrument bias in 2SLS
# ──────────────────────────────────────────────────────────────────────────────

# First-stage regression with BOTH instruments
first_stage <- feols(
  cdp_sc_member ~ peer_cdp_share_lag + peer_cdp_share_country_lag +
    # Control variables (same as in baseline model):
    esc_incidents_highreach +              # Environmental incidents
    e_disc_coalesced_zeros + e_disc_missing +  # Emissions disclosure
    log(scope1_zeros + 1) + scope1_missing +   # Scope 1 emissions
    roa_oibdp_w1_at_w1 +                   # Return on assets
    at_usd_winsorized_1_log +              # Firm size (log assets)
    tll_lt_w1_at_w1 |                      # Leverage
    # Fixed effects:
    headquarter_country + as.factor(year), # Country and year FE
  data    = complete_data_2022_instrument,
  cluster = ~ gvkey                        # Cluster SE by firm
)

# Display first-stage results
cat("First-Stage Regression Results:\n")
cat("────────────────────────────────────────────────────────────────────────────\n")
summary(first_stage)

# ──────────────────────────────────────────────────────────────────────────────
# INSTRUMENT STRENGTH TEST (F-test)
# ──────────────────────────────────────────────────────────────────────────────
# With multiple instruments, we use a JOINT F-test:
#   H0: Both instruments have coefficient = 0 (jointly)
#   HA: At least one instrument predicts CDP membership
#
# Rule of thumb (Stock & Yogo 2005):
#   F > 10  → instruments are "strong" (acceptable bias < 10%)
#   F < 10  → "weak instruments" problem (biased 2SLS estimates)
# ──────────────────────────────────────────────────────────────────────────────

# Extract coefficients for BOTH instruments
coef_table <- coeftable(first_stage)

# Individual t-statistics for each instrument
t_industry <- coef_table["peer_cdp_share_lag", "t value"]
t_country  <- coef_table["peer_cdp_share_country_lag", "t value"]

# Joint F-test using Wald test
# Test: both instrument coefficients are jointly zero
wald_test <- wald(first_stage,
                  c("peer_cdp_share_lag = 0",
                    "peer_cdp_share_country_lag = 0"))

# The F-statistic for joint significance
F_joint <- wald_test$stat

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                    INSTRUMENT STRENGTH DIAGNOSTICS                           \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")
cat("Individual t-statistics:\n")
cat("  Industry instrument (peer_cdp_share_lag):     t =", round(t_industry, 3), "\n")
cat("  Country instrument (peer_cdp_share_country_lag): t =", round(t_country, 3), "\n\n")
cat("Joint F-test for both instruments:\n")
cat("  F-statistic =", round(F_joint, 2), "\n")
cat("  Rule of thumb: F > 10 indicates strong instruments\n")

if (F_joint > 10) {
  cat("  ✓ PASS: Instruments are sufficiently strong (F > 10)\n")
} else {
  cat("  ✗ WARNING: Weak instruments detected (F < 10)\n")
  cat("    → 2SLS estimates may be biased toward OLS\n")
}

cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# SECOND-STAGE: 2SLS Estimation
# ──────────────────────────────────────────────────────────────────────────────
# Now we use the PREDICTED values of cdp_sc_member from the first stage
# to estimate its effect on SBTi commitment.
#
# Why 2SLS?
#   → Removes endogeneity bias from cdp_sc_member
#   → Uses only the variation in CDP membership explained by peer behavior
#   → Gives us a "cleaner" causal estimate
# ──────────────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                        SECOND-STAGE REGRESSION (2SLS)                        \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

# Ensure factor variables are properly coded
complete_data_2022_instrument <- complete_data_2022_instrument %>%
  mutate(
    headquarter_country = as.factor(headquarter_country),
    FourDigitName       = as.factor(FourDigitName),
    year                = as.factor(year),
    gvkey               = as.factor(gvkey)
  )

# 2SLS estimation using felm (handles fixed effects efficiently)
# Syntax:
#   outcome ~ exogenous | fixed_effects | (endogenous ~ instruments) | cluster
second_stage <- felm(
  # Outcome and exogenous controls:
  sbti_commitment_lead1 ~
    esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
    tll_lt_w1_at_w1
  # Fixed effects:
  | headquarter_country + year
  # IV specification (endogenous ~ instruments):
  | (cdp_sc_member ~ peer_cdp_share_lag + peer_cdp_share_country_lag)
  # Cluster standard errors by firm:
  | gvkey,
  data = complete_data_2022_instrument
)

# Display second-stage results
cat("Second-Stage (2SLS) Results:\n")
cat("────────────────────────────────────────────────────────────────────────────\n")
summary(second_stage)

# ──────────────────────────────────────────────────────────────────────────────
# OVERIDENTIFICATION TEST (Sargan-Hansen J-test)
# ──────────────────────────────────────────────────────────────────────────────
# We have 2 instruments for 1 endogenous variable → system is OVERIDENTIFIED
# This allows us to test whether instruments are VALID (uncorrelated with errors)
#
# Sargan-Hansen J-test:
#   H0: All instruments are valid (uncorrelated with the error term)
#   HA: At least one instrument is invalid (correlated with errors)
#
# What we want:
#   ✓ FAIL to reject H0 (p > 0.05) → instruments appear valid
#   ✗ Reject H0 (p < 0.05) → at least one instrument is suspect
#
# CAVEAT: This test has low power, so passing it doesn't guarantee validity
# ──────────────────────────────────────────────────────────────────────────────

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                  OVERIDENTIFICATION TEST (Sargan-Hansen J)                   \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

# We need to use ivreg() from AER package for the Sargan test
# (felm doesn't provide this diagnostic)

# Prepare data: remove observations with missing values
data_complete <- complete_data_2022_instrument %>%
  filter(!is.na(sbti_commitment_lead1),
         !is.na(cdp_sc_member),
         !is.na(peer_cdp_share_lag),
         !is.na(peer_cdp_share_country_lag)) %>%
  mutate(year = as.factor(year),
         headquarter_country = as.factor(headquarter_country))

# Run 2SLS using ivreg (without fixed effects for simplicity in diagnostics)
# NOTE: This is just for the J-test; main results use felm with FE above
iv_model <- ivreg(
  sbti_commitment_lead1 ~ cdp_sc_member +
    esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
    tll_lt_w1_at_w1 +
    year + headquarter_country |  # Include FE as regressors
    # Instruments (include all exogenous + instruments):
    esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
    tll_lt_w1_at_w1 +
    year + headquarter_country +
    peer_cdp_share_lag + peer_cdp_share_country_lag,
  data = data_complete
)

# Conduct Sargan-Hansen test
# This tests whether the overidentifying restrictions are valid
# (i.e., whether our 2 instruments are truly exogenous)
tryCatch({
  # Get residuals from 2SLS
  resid_2sls <- residuals(iv_model)

  # Regress residuals on ALL instruments and exogenous variables
  aux_reg <- lm(resid_2sls ~ peer_cdp_share_lag + peer_cdp_share_country_lag +
                  esc_incidents_highreach +
                  e_disc_coalesced_zeros + e_disc_missing +
                  log(scope1_zeros + 1) + scope1_missing +
                  roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
                  tll_lt_w1_at_w1 +
                  year + headquarter_country,
                data = data_complete)

  # Compute J-statistic: n * R²
  n <- nobs(aux_reg)
  r_squared <- summary(aux_reg)$r.squared
  J_stat <- n * r_squared

  # Degrees of freedom = number of overidentifying restrictions
  # We have 2 instruments for 1 endogenous variable → df = 2 - 1 = 1
  df_J <- 1

  # P-value from chi-squared distribution
  p_value_J <- 1 - pchisq(J_stat, df = df_J)

  cat("Sargan-Hansen J-test for overidentifying restrictions:\n")
  cat("────────────────────────────────────────────────────────────────────────────\n")
  cat("  J-statistic =", round(J_stat, 3), "\n")
  cat("  Degrees of freedom =", df_J, "\n")
  cat("  P-value =", round(p_value_J, 4), "\n\n")
  cat("Interpretation:\n")
  cat("  H0: Both instruments are valid (exogenous)\n")
  if (p_value_J > 0.05) {
    cat("  ✓ PASS: Cannot reject H0 (p > 0.05)\n")
    cat("    → No evidence against instrument validity\n")
    cat("    → Both instruments appear to satisfy exclusion restriction\n")
  } else {
    cat("  ✗ WARNING: Reject H0 (p < 0.05)\n")
    cat("    → Evidence that at least one instrument may be invalid\n")
    cat("    → One or both instruments may be correlated with the error term\n")
  }
  cat("\n")
  cat("CAVEAT: J-test has low power. Passing doesn't prove validity,\n")
  cat("        but failing raises serious concerns about instrument exogeneity.\n")

  # Store for export
  j_test_results <- data.frame(
    Statistic = "J-statistic",
    Value = round(J_stat, 3),
    DF = df_J,
    P_value = round(p_value_J, 4),
    Interpretation = ifelse(p_value_J > 0.05,
                           "Cannot reject H0: Instruments appear valid",
                           "Reject H0: Evidence of invalid instrument(s)")
  )

}, error = function(e) {
  cat("Note: Could not compute Sargan-Hansen test.\n")
  cat("This may occur if there are too few degrees of freedom or convergence issues.\n")
  j_test_results <- NULL
})

# ══════════════════════════════════════════════════════════════════════════════
#                        EXPORT RESULTS TO MARKDOWN TABLES
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                        EXPORTING RESULTS TO TABLES/                          \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

# Create tables directory if it doesn't exist
if (!dir.exists("tables")) {
  dir.create("tables")
}

# ──────────────────────────────────────────────────────────────────────────────
# TABLE 1: First-Stage Results
# ──────────────────────────────────────────────────────────────────────────────

# Extract first-stage coefficients and statistics
first_stage_coef <- coeftable(first_stage)

# Create a clean table for the instruments only
first_stage_table <- data.frame(
  Variable = c("Industry peer share (lagged)", "Country peer share (lagged)"),
  Coefficient = c(
    round(first_stage_coef["peer_cdp_share_lag", "Estimate"], 4),
    round(first_stage_coef["peer_cdp_share_country_lag", "Estimate"], 4)
  ),
  Std_Error = c(
    round(first_stage_coef["peer_cdp_share_lag", "Std. Error"], 4),
    round(first_stage_coef["peer_cdp_share_country_lag", "Std. Error"], 4)
  ),
  t_value = c(
    round(first_stage_coef["peer_cdp_share_lag", "t value"], 3),
    round(first_stage_coef["peer_cdp_share_country_lag", "t value"], 3)
  ),
  p_value = c(
    format.pval(first_stage_coef["peer_cdp_share_lag", "Pr(>|t|)"], digits = 3),
    format.pval(first_stage_coef["peer_cdp_share_country_lag", "Pr(>|t|)"], digits = 3)
  ),
  stringsAsFactors = FALSE
)

# Write to markdown
cat("\nExporting First-Stage Results...\n")
writeLines(
  c("# First-Stage Regression Results",
    "",
    "**Dependent Variable:** CDP Supply Chain Member (cdp_sc_member)",
    "",
    "**Model:** Linear probability model with country and year fixed effects",
    "",
    "**Instruments:**",
    "",
    paste0("| Variable | Coefficient | Std. Error | t-value | p-value |"),
    paste0("|----------|-------------|------------|---------|---------|"),
    paste0("| ", first_stage_table$Variable, " | ",
           first_stage_table$Coefficient, " | ",
           first_stage_table$Std_Error, " | ",
           first_stage_table$t_value, " | ",
           first_stage_table$p_value, " |"),
    "",
    "**Note:** Standard errors clustered by firm (gvkey). All control variables included.",
    ""
  ),
  "tables/first_stage_results.md"
)
cat("  ✓ Saved to tables/first_stage_results.md\n")

# ──────────────────────────────────────────────────────────────────────────────
# TABLE 2: Instrument Strength Diagnostics
# ──────────────────────────────────────────────────────────────────────────────

# Create diagnostics table
diagnostics_table <- data.frame(
  Test = c(
    "Industry instrument t-statistic",
    "Country instrument t-statistic",
    "Joint F-statistic (both instruments)",
    "Stock-Yogo critical value (10% bias)",
    "Strength assessment"
  ),
  Value = c(
    round(t_industry, 3),
    round(t_country, 3),
    round(F_joint, 2),
    "19.93",  # Stock-Yogo critical value for 2 instruments
    ifelse(F_joint > 10, "PASS: Strong instruments", "WARNING: Weak instruments")
  ),
  stringsAsFactors = FALSE
)

cat("\nExporting Instrument Strength Diagnostics...\n")
writeLines(
  c("# Instrument Strength Diagnostics",
    "",
    "**Test for Weak Instruments**",
    "",
    paste0("| Test | Value |"),
    paste0("|------|-------|"),
    paste0("| ", diagnostics_table$Test, " | ", diagnostics_table$Value, " |"),
    "",
    "**Interpretation:**",
    "",
    "- **Rule of thumb (Stock & Yogo 2005):** F > 10 indicates instruments are sufficiently strong",
    "- **Stock-Yogo critical value:** For 2 instruments and 1 endogenous variable, F > 19.93 ensures max 10% bias",
    paste0("- **Result:** ", ifelse(F_joint > 10,
                                   "Instruments are strong enough (F > 10)",
                                   "WARNING: Weak instruments detected (F < 10)")),
    "",
    "**What this means:**",
    "",
    "A strong first-stage F-statistic indicates that our instruments (peer CDP membership shares)",
    "have sufficient predictive power for the endogenous variable (firm CDP membership).",
    "This reduces weak instrument bias in the 2SLS estimates.",
    ""
  ),
  "tables/instrument_strength.md"
)
cat("  ✓ Saved to tables/instrument_strength.md\n")

# ──────────────────────────────────────────────────────────────────────────────
# TABLE 3: Second-Stage (2SLS) Results
# ──────────────────────────────────────────────────────────────────────────────

# Extract second-stage coefficient for instrumented CDP variable
second_stage_coef <- coeftable(second_stage)

# Get the instrumented CDP variable row (it may be named differently)
# felm typically names it with backticks
cdp_var_name <- grep("cdp_sc_member", rownames(second_stage_coef), value = TRUE)[1]

second_stage_table <- data.frame(
  Variable = "CDP Supply Chain Member (instrumented)",
  Coefficient = round(second_stage_coef[cdp_var_name, "Estimate"], 4),
  Std_Error = round(second_stage_coef[cdp_var_name, "Std. Error"], 4),
  t_value = round(second_stage_coef[cdp_var_name, "t value"], 3),
  p_value = format.pval(second_stage_coef[cdp_var_name, "Pr(>|t|)"], digits = 3),
  stringsAsFactors = FALSE
)

cat("\nExporting Second-Stage (2SLS) Results...\n")
writeLines(
  c("# Second-Stage Regression Results (2SLS)",
    "",
    "**Dependent Variable:** SBTi Commitment (lead 1 year)",
    "",
    "**Method:** Two-Stage Least Squares (2SLS) with country and year fixed effects",
    "",
    "**Instrumented Variable:**",
    "",
    paste0("| Variable | Coefficient | Std. Error | t-value | p-value |"),
    paste0("|----------|-------------|------------|---------|---------|"),
    paste0("| ", second_stage_table$Variable, " | ",
           second_stage_table$Coefficient, " | ",
           second_stage_table$Std_Error, " | ",
           second_stage_table$t_value, " | ",
           second_stage_table$p_value, " |"),
    "",
    "**Instruments used:**",
    "- Industry peer CDP share (lagged)",
    "- Country peer CDP share (lagged)",
    "",
    "**Interpretation:**",
    "",
    paste0("The coefficient ", second_stage_table$Coefficient,
           " represents the causal effect of CDP Supply Chain"),
    "membership on the probability of making an SBTi commitment in the following year,",
    "after accounting for endogeneity using peer-based instruments.",
    "",
    "**Note:** Standard errors clustered by firm (gvkey). All control variables included.",
    ""
  ),
  "tables/second_stage_results.md"
)
cat("  ✓ Saved to tables/second_stage_results.md\n")

# ──────────────────────────────────────────────────────────────────────────────
# TABLE 4: Sargan-Hansen Overidentification Test
# ──────────────────────────────────────────────────────────────────────────────

if (exists("j_test_results") && !is.null(j_test_results)) {
  cat("\nExporting Sargan-Hansen J-test Results...\n")
  writeLines(
    c("# Sargan-Hansen Overidentification Test",
      "",
      "**Test for Instrument Validity**",
      "",
      paste0("| Statistic | Value | DF | p-value | Interpretation |"),
      paste0("|-----------|-------|----|---------:|----------------|"),
      paste0("| ", j_test_results$Statistic, " | ",
             j_test_results$Value, " | ",
             j_test_results$DF, " | ",
             j_test_results$P_value, " | ",
             j_test_results$Interpretation, " |"),
      "",
      "**What this test does:**",
      "",
      "The Sargan-Hansen J-test checks whether our instruments are valid (exogenous).",
      "With 2 instruments for 1 endogenous variable, we have 1 overidentifying restriction",
      "that can be tested.",
      "",
      "**Hypotheses:**",
      "- H0: Both instruments are valid (uncorrelated with the error term)",
      "- HA: At least one instrument is invalid (correlated with the error term)",
      "",
      "**Interpretation:**",
      paste0("- p-value = ", j_test_results$P_value),
      ifelse(j_test_results$P_value > 0.05,
             "- ✓ PASS: Cannot reject H0 (p > 0.05)",
             "- ✗ WARNING: Reject H0 (p < 0.05)"),
      ifelse(j_test_results$P_value > 0.05,
             "- No evidence against instrument validity",
             "- Evidence that at least one instrument may be invalid"),
      "",
      "**Important caveat:**",
      "",
      "The J-test has relatively low statistical power. Failing to reject H0 doesn't *prove*",
      "the instruments are valid, but rejecting H0 would raise serious concerns about",
      "instrument exogeneity and the validity of the 2SLS estimates.",
      ""
    ),
    "tables/sargan_hansen_test.md"
  )
  cat("  ✓ Saved to tables/sargan_hansen_test.md\n")
} else {
  cat("\nNote: Sargan-Hansen test could not be computed.\n")
}

# ──────────────────────────────────────────────────────────────────────────────
# TABLE 5: Summary of All IV Diagnostic Tests
# ──────────────────────────────────────────────────────────────────────────────

cat("\nExporting Summary of Diagnostics...\n")

summary_lines <- c(
  "# Summary of Instrumental Variable Diagnostics",
  "",
  "## Overview",
  "",
  "This document summarizes the key diagnostic tests for the 2SLS estimation of the effect",
  "of CDP Supply Chain membership on SBTi commitment.",
  "",
  "## Instruments Used",
  "",
  "1. **Industry peer share (lagged):** Share of firms in the same industry-year that are CDP members",
  "2. **Country peer share (lagged):** Share of firms in the same country-year that are CDP members",
  "",
  "Both instruments exclude the focal firm from the peer calculation to avoid mechanical correlation.",
  "",
  "## Diagnostic Test Results",
  "",
  "### 1. Instrument Relevance (First-Stage F-test)",
  "",
  paste0("- **Joint F-statistic:** ", round(F_joint, 2)),
  paste0("- **Stock-Yogo critical value (10% bias):** 19.93"),
  paste0("- **Assessment:** ", ifelse(F_joint > 10, "✓ PASS - Strong instruments", "✗ WARNING - Weak instruments")),
  "",
  ifelse(F_joint > 10,
         "The F-statistic exceeds the rule-of-thumb threshold of 10, indicating our instruments are sufficiently strong.",
         "WARNING: The F-statistic is below 10, suggesting potential weak instrument bias."),
  ""
)

# Add J-test results if available
if (exists("j_test_results") && !is.null(j_test_results)) {
  summary_lines <- c(summary_lines,
    "### 2. Instrument Validity (Sargan-Hansen J-test)",
    "",
    paste0("- **J-statistic:** ", j_test_results$Value),
    paste0("- **p-value:** ", j_test_results$P_value),
    paste0("- **Assessment:** ", ifelse(j_test_results$P_value > 0.05,
                                       "✓ PASS - No evidence against validity",
                                       "✗ WARNING - Possible invalid instrument")),
    "",
    ifelse(j_test_results$P_value > 0.05,
           "Cannot reject the null hypothesis that instruments are valid (p > 0.05).",
           "The null hypothesis of valid instruments is rejected (p < 0.05), raising concerns."),
    ""
  )
}

summary_lines <- c(summary_lines,
  "## Key Takeaways",
  "",
  paste0("1. **Instrument strength:** ", ifelse(F_joint > 10, "Sufficient", "Insufficient")),
  paste0("2. **Instrument validity:** ",
         ifelse(exists("j_test_results") && !is.null(j_test_results) && j_test_results$P_value > 0.05,
                "No evidence against exogeneity",
                "See detailed results above")),
  "3. **Conclusion:** ",
  "",
  ifelse(F_joint > 10 && exists("j_test_results") && !is.null(j_test_results) && j_test_results$P_value > 0.05,
         "Both diagnostic tests pass, supporting the validity of the 2SLS estimates.",
         "Review individual test results for potential concerns about instrument quality."),
  "",
  "## Additional Files",
  "",
  "See the following files for detailed results:",
  "- `first_stage_results.md` - First-stage regression coefficients",
  "- `instrument_strength.md` - Detailed F-test diagnostics",
  "- `second_stage_results.md` - 2SLS coefficient estimates",
  "- `sargan_hansen_test.md` - Overidentification test results",
  ""
)

writeLines(summary_lines, "tables/iv_diagnostics_summary.md")
cat("  ✓ Saved to tables/iv_diagnostics_summary.md\n")

cat("\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("All results exported to tables/ directory\n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

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


