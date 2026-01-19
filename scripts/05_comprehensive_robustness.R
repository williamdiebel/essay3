# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script: 05_comprehensive_robustness.R
# Purpose: Comprehensive robustness testing for dual-pathway hypotheses (LPM)
#
# Research Question:
#   H1: Internal intervention (CDP SC) → Risk awareness → SBTi commitment
#   H2: External intervention (Incidents) → Risk awareness → SBTi commitment
#   H3: Interaction - External matters MORE when internal capability exists
#
# Strategy:
#   Phase 1: Alternative incident measures (highreach, highsev, all)
#   Phase 2: Transformations (log, binary)
#   Phase 3: Interaction effects (KEY - complementarity test for H3)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load packages ####
library(tidyverse)
library(data.table)
library(fixest)      # For feols (fast FE estimation)
library(lfe)         # For felm (IV with FE)
library(broom)       # For tidy output

# Set working directory ####
if (!dir.exists("cleaned_data")) {
  stop("Error: Run from project root directory")
}

# Load data ####
cat("Loading data...\n")
data <- readRDS("cleaned_data/complete_data_2022_instrument_country.rds")
setDT(data)

cat("  Observations:", nrow(data), "\n")
cat("  Firms:", uniqueN(data$gvkey), "\n")
cat("  Years:", min(data$year), "-", max(data$year), "\n\n")

# Convert factors ####
data[, `:=`(
  headquarter_country = as.factor(headquarter_country),
  FourDigitName = as.factor(FourDigitName),
  year = as.factor(year),
  gvkey = as.factor(gvkey)
)]

# Define controls formula ####
controls <- c("e_disc_coalesced_zeros", "e_disc_missing",
              "log(scope1_zeros + 1)", "scope1_missing",
              "roa_oibdp_w1_at_w1", "at_usd_winsorized_1_log",
              "tll_lt_w1_at_w1")
controls_formula <- paste(controls, collapse = " + ")

# Storage for results ####
results_list <- list()

################################################################################
# DATA PREPARATION
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("DATA PREPARATION\n")
cat(strrep("=", 80) %+% "\n\n")

# Identify complete cases for all variables needed ####
cat("Identifying complete cases for all variables needed...\n")
cat("  Original observations:", nrow(data), "\n")

# Variables needed for analysis
vars_needed <- c("sbti_commitment_lead1", "cdp_sc_member", "peer_cdp_share_country_lag",
                 "esc_incidents_highreach", "esc_incidents_highsev", "esc_incidents",
                 "e_disc_coalesced_zeros", "e_disc_missing",
                 "scope1_zeros", "scope1_missing",
                 "roa_oibdp_w1_at_w1", "at_usd_winsorized_1_log", "tll_lt_w1_at_w1",
                 "headquarter_country", "FourDigitName", "year", "gvkey")

# Filter to complete cases
data_complete <- data[complete.cases(data[, ..vars_needed]), ]

cat("  Complete-case observations:", nrow(data_complete), "\n")
cat("  Observations dropped:", nrow(data) - nrow(data_complete), "\n")
cat("  Retention rate:", round(nrow(data_complete) / nrow(data) * 100, 1), "%\n\n")

# Remove singleton FE groups (only needed for FE estimation) ####
cat("Removing singleton fixed effect groups...\n")

# Count observations per country-year FE group
data_complete[, fe_group_size := .N, by = .(headquarter_country, year)]
cat("  Singleton FE groups (n=1):", sum(data_complete$fe_group_size == 1), "obs\n")
data_complete <- data_complete[fe_group_size > 1]
data_complete[, fe_group_size := NULL]

cat("  Observations after removing singletons:", nrow(data_complete), "\n\n")

# First-stage IV regression for control function ####
cat("Running first-stage IV regression for control function...\n")

first_stage <- feols(
  cdp_sc_member ~ peer_cdp_share_country_lag +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("  First-stage F-statistic:",
    round(coeftable(first_stage)["peer_cdp_share_country_lag", "t value"]^2, 2), "\n")
cat("  Instrument coefficient:",
    round(coef(first_stage)["peer_cdp_share_country_lag"], 4), "\n")
cat("  Observations used:", first_stage$nobs, "\n")

# Add residuals for control function approach
data_complete$fs_resid <- residuals(first_stage)

cat("  Control function residuals added successfully\n\n")

# Baseline outcome rate ####
cat("Baseline outcome statistics:\n")
cat("  SBTi commitment rate:",
    round(mean(data_complete$sbti_commitment_lead1) * 100, 2), "%\n\n")

################################################################################
# PHASE 1: ALTERNATIVE INCIDENT MEASURES
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PHASE 1: ALTERNATIVE INCIDENT MEASURES (LPM-IV)\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Motivation: Test robustness across different incident operationalizations\n")
cat("Testing: High reach, high severity, all incidents\n\n")

# Model 1.1: High Reach Incidents ####
cat("--- Model 1.1: LPM-IV with esc_incidents_highreach ---\n\n")

lpm_highreach <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(lpm_highreach)["cdp_sc_member"], 4),
    ", SE:", round(se(lpm_highreach)["cdp_sc_member"], 4),
    ", p:", round(pvalue(lpm_highreach)["cdp_sc_member"], 4), "\n")
cat("  Incidents coefficient:", round(coef(lpm_highreach)["esc_incidents_highreach"], 4),
    ", SE:", round(se(lpm_highreach)["esc_incidents_highreach"], 4),
    ", p:", round(pvalue(lpm_highreach)["esc_incidents_highreach"], 4), "\n")
cat("  Control function (fs_resid):", round(coef(lpm_highreach)["fs_resid"], 4),
    ", p:", round(pvalue(lpm_highreach)["fs_resid"], 4), "\n\n")

results_list$lpm_highreach <- list(
  model = "LPM-IV",
  incident_measure = "highreach",
  cdp_coef = coef(lpm_highreach)["cdp_sc_member"],
  cdp_se = se(lpm_highreach)["cdp_sc_member"],
  cdp_p = pvalue(lpm_highreach)["cdp_sc_member"],
  incident_coef = coef(lpm_highreach)["esc_incidents_highreach"],
  incident_se = se(lpm_highreach)["esc_incidents_highreach"],
  incident_p = pvalue(lpm_highreach)["esc_incidents_highreach"],
  cf_resid_p = pvalue(lpm_highreach)["fs_resid"],
  nobs = lpm_highreach$nobs
)

# Model 1.2: High Severity Incidents ####
cat("--- Model 1.2: LPM-IV with esc_incidents_highsev ---\n\n")

lpm_highsev <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents_highsev +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(lpm_highsev)["cdp_sc_member"], 4),
    ", SE:", round(se(lpm_highsev)["cdp_sc_member"], 4),
    ", p:", round(pvalue(lpm_highsev)["cdp_sc_member"], 4), "\n")
cat("  Incidents coefficient:", round(coef(lpm_highsev)["esc_incidents_highsev"], 4),
    ", SE:", round(se(lpm_highsev)["esc_incidents_highsev"], 4),
    ", p:", round(pvalue(lpm_highsev)["esc_incidents_highsev"], 4), "\n")
cat("  Control function (fs_resid):", round(coef(lpm_highsev)["fs_resid"], 4),
    ", p:", round(pvalue(lpm_highsev)["fs_resid"], 4), "\n\n")

results_list$lpm_highsev <- list(
  model = "LPM-IV",
  incident_measure = "highsev",
  cdp_coef = coef(lpm_highsev)["cdp_sc_member"],
  cdp_se = se(lpm_highsev)["cdp_sc_member"],
  cdp_p = pvalue(lpm_highsev)["cdp_sc_member"],
  incident_coef = coef(lpm_highsev)["esc_incidents_highsev"],
  incident_se = se(lpm_highsev)["esc_incidents_highsev"],
  incident_p = pvalue(lpm_highsev)["esc_incidents_highsev"],
  cf_resid_p = pvalue(lpm_highsev)["fs_resid"],
  nobs = lpm_highsev$nobs
)

# Model 1.3: All Incidents ####
cat("--- Model 1.3: LPM-IV with esc_incidents (all) ---\n\n")

lpm_all <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(lpm_all)["cdp_sc_member"], 4),
    ", SE:", round(se(lpm_all)["cdp_sc_member"], 4),
    ", p:", round(pvalue(lpm_all)["cdp_sc_member"], 4), "\n")
cat("  Incidents coefficient:", round(coef(lpm_all)["esc_incidents"], 4),
    ", SE:", round(se(lpm_all)["esc_incidents"], 4),
    ", p:", round(pvalue(lpm_all)["esc_incidents"], 4), "\n")
cat("  Control function (fs_resid):", round(coef(lpm_all)["fs_resid"], 4),
    ", p:", round(pvalue(lpm_all)["fs_resid"], 4), "\n\n")

results_list$lpm_all <- list(
  model = "LPM-IV",
  incident_measure = "all",
  cdp_coef = coef(lpm_all)["cdp_sc_member"],
  cdp_se = se(lpm_all)["cdp_sc_member"],
  cdp_p = pvalue(lpm_all)["cdp_sc_member"],
  incident_coef = coef(lpm_all)["esc_incidents"],
  incident_se = se(lpm_all)["esc_incidents"],
  incident_p = pvalue(lpm_all)["esc_incidents"],
  cf_resid_p = pvalue(lpm_all)["fs_resid"],
  nobs = lpm_all$nobs
)

################################################################################
# PHASE 2: TRANSFORMATIONS
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PHASE 2: ALTERNATIVE TRANSFORMATIONS\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Motivation: Test sensitivity to functional form\n")
cat("Testing: Log transformation, binary indicator\n\n")

# Model 2.1: Log Transformation ####
cat("--- Model 2.1: LPM-IV with log(esc_incidents_highreach + 1) ---\n\n")

data_complete$log_incidents_highreach <- log(data_complete$esc_incidents_highreach + 1)

lpm_log <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + log_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(lpm_log)["cdp_sc_member"], 4),
    ", SE:", round(se(lpm_log)["cdp_sc_member"], 4),
    ", p:", round(pvalue(lpm_log)["cdp_sc_member"], 4), "\n")
cat("  Log incidents coefficient:", round(coef(lpm_log)["log_incidents_highreach"], 4),
    ", SE:", round(se(lpm_log)["log_incidents_highreach"], 4),
    ", p:", round(pvalue(lpm_log)["log_incidents_highreach"], 4), "\n\n")

results_list$lpm_log <- list(
  model = "LPM-IV",
  incident_measure = "log_highreach",
  cdp_coef = coef(lpm_log)["cdp_sc_member"],
  cdp_se = se(lpm_log)["cdp_sc_member"],
  cdp_p = pvalue(lpm_log)["cdp_sc_member"],
  incident_coef = coef(lpm_log)["log_incidents_highreach"],
  incident_se = se(lpm_log)["log_incidents_highreach"],
  incident_p = pvalue(lpm_log)["log_incidents_highreach"],
  cf_resid_p = pvalue(lpm_log)["fs_resid"],
  nobs = lpm_log$nobs
)

# Model 2.2: Binary Indicator ####
cat("--- Model 2.2: LPM-IV with any_incident_highreach (binary) ---\n\n")

data_complete$any_incident_highreach <- as.numeric(data_complete$esc_incidents_highreach > 0)

lpm_binary <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + any_incident_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(lpm_binary)["cdp_sc_member"], 4),
    ", SE:", round(se(lpm_binary)["cdp_sc_member"], 4),
    ", p:", round(pvalue(lpm_binary)["cdp_sc_member"], 4), "\n")
cat("  Binary incident coefficient:", round(coef(lpm_binary)["any_incident_highreach"], 4),
    ", SE:", round(se(lpm_binary)["any_incident_highreach"], 4),
    ", p:", round(pvalue(lpm_binary)["any_incident_highreach"], 4), "\n\n")

results_list$lpm_binary <- list(
  model = "LPM-IV",
  incident_measure = "binary_highreach",
  cdp_coef = coef(lpm_binary)["cdp_sc_member"],
  cdp_se = se(lpm_binary)["cdp_sc_member"],
  cdp_p = pvalue(lpm_binary)["cdp_sc_member"],
  incident_coef = coef(lpm_binary)["any_incident_highreach"],
  incident_se = se(lpm_binary)["any_incident_highreach"],
  incident_p = pvalue(lpm_binary)["any_incident_highreach"],
  cf_resid_p = pvalue(lpm_binary)["fs_resid"],
  nobs = lpm_binary$nobs
)

################################################################################
# PHASE 3: INTERACTION EFFECTS (KEY TEST FOR H3)
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PHASE 3: INTERACTION EFFECTS - COMPLEMENTARITY TEST\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Motivation: Test H3 - Do incidents matter MORE when CDP SC capability exists?\n")
cat("Theoretical prediction: Positive interaction (complementarity)\n\n")

# Model 3.1: Interaction with High Reach Incidents ####
cat("--- Model 3.1: LPM-IV with CDP SC × High Reach Incidents ---\n\n")

lpm_interact_highreach <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents_highreach +
    cdp_sc_member:esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC (main effect):", round(coef(lpm_interact_highreach)["cdp_sc_member"], 4),
    ", p:", round(pvalue(lpm_interact_highreach)["cdp_sc_member"], 4), "\n")
cat("  Incidents (main effect):", round(coef(lpm_interact_highreach)["esc_incidents_highreach"], 4),
    ", p:", round(pvalue(lpm_interact_highreach)["esc_incidents_highreach"], 4), "\n")
cat("  INTERACTION:", round(coef(lpm_interact_highreach)["cdp_sc_member:esc_incidents_highreach"], 4),
    ", p:", round(pvalue(lpm_interact_highreach)["cdp_sc_member:esc_incidents_highreach"], 4), "\n")

# Interpretation guidance
if (pvalue(lpm_interact_highreach)["cdp_sc_member:esc_incidents_highreach"] < 0.05) {
  if (coef(lpm_interact_highreach)["cdp_sc_member:esc_incidents_highreach"] > 0) {
    cat("\n** INTERPRETATION: Positive, significant interaction - SUPPORTS H3 **\n")
    cat("   Incidents increase SBTi commitment MORE when firm has CDP SC membership\n")
    cat("   This suggests COMPLEMENTARITY between internal and external interventions\n\n")
  } else {
    cat("\n** INTERPRETATION: Negative, significant interaction - CONTRADICTS H3 **\n")
    cat("   Incidents increase SBTi commitment LESS when firm has CDP SC membership\n")
    cat("   This suggests SUBSTITUTION, not complementarity\n\n")
  }
} else {
  cat("\n** INTERPRETATION: Non-significant interaction - NO EVIDENCE for H3 **\n")
  cat("   Effect of incidents does not depend on CDP SC membership\n")
  cat("   Internal and external interventions operate independently\n\n")
}

results_list$lpm_interact_highreach <- list(
  model = "LPM-IV-Interact",
  incident_measure = "highreach",
  cdp_coef = coef(lpm_interact_highreach)["cdp_sc_member"],
  cdp_se = se(lpm_interact_highreach)["cdp_sc_member"],
  cdp_p = pvalue(lpm_interact_highreach)["cdp_sc_member"],
  incident_coef = coef(lpm_interact_highreach)["esc_incidents_highreach"],
  incident_se = se(lpm_interact_highreach)["esc_incidents_highreach"],
  incident_p = pvalue(lpm_interact_highreach)["esc_incidents_highreach"],
  interaction_coef = coef(lpm_interact_highreach)["cdp_sc_member:esc_incidents_highreach"],
  interaction_se = se(lpm_interact_highreach)["cdp_sc_member:esc_incidents_highreach"],
  interaction_p = pvalue(lpm_interact_highreach)["cdp_sc_member:esc_incidents_highreach"],
  cf_resid_p = pvalue(lpm_interact_highreach)["fs_resid"],
  nobs = lpm_interact_highreach$nobs
)

# Model 3.2: Interaction with High Severity Incidents ####
cat("--- Model 3.2: LPM-IV with CDP SC × High Severity Incidents ---\n\n")

lpm_interact_highsev <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents_highsev +
    cdp_sc_member:esc_incidents_highsev +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  INTERACTION:", round(coef(lpm_interact_highsev)["cdp_sc_member:esc_incidents_highsev"], 4),
    ", p:", round(pvalue(lpm_interact_highsev)["cdp_sc_member:esc_incidents_highsev"], 4), "\n\n")

results_list$lpm_interact_highsev <- list(
  model = "LPM-IV-Interact",
  incident_measure = "highsev",
  interaction_coef = coef(lpm_interact_highsev)["cdp_sc_member:esc_incidents_highsev"],
  interaction_se = se(lpm_interact_highsev)["cdp_sc_member:esc_incidents_highsev"],
  interaction_p = pvalue(lpm_interact_highsev)["cdp_sc_member:esc_incidents_highsev"],
  nobs = lpm_interact_highsev$nobs
)

# Model 3.3: Interaction with All Incidents ####
cat("--- Model 3.3: LPM-IV with CDP SC × All Incidents ---\n\n")

lpm_interact_all <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents +
    cdp_sc_member:esc_incidents +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  INTERACTION:", round(coef(lpm_interact_all)["cdp_sc_member:esc_incidents"], 4),
    ", p:", round(pvalue(lpm_interact_all)["cdp_sc_member:esc_incidents"], 4), "\n\n")

results_list$lpm_interact_all <- list(
  model = "LPM-IV-Interact",
  incident_measure = "all",
  interaction_coef = coef(lpm_interact_all)["cdp_sc_member:esc_incidents"],
  interaction_se = se(lpm_interact_all)["cdp_sc_member:esc_incidents"],
  interaction_p = pvalue(lpm_interact_all)["cdp_sc_member:esc_incidents"],
  nobs = lpm_interact_all$nobs
)

# Model 3.4: Interaction with Binary Indicator ####
cat("--- Model 3.4: LPM-IV with CDP SC × Any Incident (binary) ---\n\n")

lpm_interact_binary <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + any_incident_highreach +
    cdp_sc_member:any_incident_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  INTERACTION:", round(coef(lpm_interact_binary)["cdp_sc_member:any_incident_highreach"], 4),
    ", p:", round(pvalue(lpm_interact_binary)["cdp_sc_member:any_incident_highreach"], 4), "\n\n")

results_list$lpm_interact_binary <- list(
  model = "LPM-IV-Interact",
  incident_measure = "binary",
  interaction_coef = coef(lpm_interact_binary)["cdp_sc_member:any_incident_highreach"],
  interaction_se = se(lpm_interact_binary)["cdp_sc_member:any_incident_highreach"],
  interaction_p = pvalue(lpm_interact_binary)["cdp_sc_member:any_incident_highreach"],
  nobs = lpm_interact_binary$nobs
)

################################################################################
# SUMMARY TABLE
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("SUMMARY OF RESULTS\n")
cat(strrep("=", 80) %+% "\n\n")

# Convert results to data frame for easy viewing
results_df <- bind_rows(results_list, .id = "specification")

# Create summary table
summary_table <- results_df %>%
  select(specification, incident_measure,
         cdp_coef, cdp_p, incident_coef, incident_p,
         interaction_coef, interaction_p, nobs) %>%
  mutate(
    cdp_sig = case_when(
      is.na(cdp_p) ~ "",
      cdp_p < 0.01 ~ "***",
      cdp_p < 0.05 ~ "**",
      cdp_p < 0.10 ~ "*",
      TRUE ~ ""
    ),
    incident_sig = case_when(
      is.na(incident_p) ~ "",
      incident_p < 0.01 ~ "***",
      incident_p < 0.05 ~ "**",
      incident_p < 0.10 ~ "*",
      TRUE ~ ""
    ),
    interaction_sig = case_when(
      is.na(interaction_p) ~ "",
      interaction_p < 0.01 ~ "***",
      interaction_p < 0.05 ~ "**",
      interaction_p < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

cat("Key Findings:\n\n")
cat("1. CDP SC Effect (H1):\n")
cat("   Range: [",
    round(min(summary_table$cdp_coef, na.rm = TRUE), 4), ",",
    round(max(summary_table$cdp_coef, na.rm = TRUE), 4), "]\n")
cat("   Significant in", sum(summary_table$cdp_p < 0.05, na.rm = TRUE), "of",
    sum(!is.na(summary_table$cdp_coef)), "specifications\n\n")

cat("2. Incidents Effect (H2):\n")
cat("   Range: [",
    round(min(summary_table$incident_coef, na.rm = TRUE), 4), ",",
    round(max(summary_table$incident_coef, na.rm = TRUE), 4), "]\n")
cat("   Significant in", sum(summary_table$incident_p < 0.05, na.rm = TRUE), "of",
    sum(!is.na(summary_table$incident_coef)), "specifications\n\n")

cat("3. Interaction Effect (H3 - COMPLEMENTARITY):\n")
cat("   Range: [",
    round(min(summary_table$interaction_coef, na.rm = TRUE), 4), ",",
    round(max(summary_table$interaction_coef, na.rm = TRUE), 4), "]\n")
cat("   Significant in", sum(summary_table$interaction_p < 0.05, na.rm = TRUE), "of",
    sum(!is.na(summary_table$interaction_coef)), "specifications\n")
cat("   Positive in", sum(summary_table$interaction_coef > 0, na.rm = TRUE), "of",
    sum(!is.na(summary_table$interaction_coef)), "specifications\n\n")

# Save results
write.csv(summary_table, "results_comprehensive_robustness_lpm.csv", row.names = FALSE)
cat("Results saved to: results_comprehensive_robustness_lpm.csv\n\n")

cat("=" %+% strrep("=", 79) %+% "\n")
cat("ANALYSIS COMPLETE\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")
