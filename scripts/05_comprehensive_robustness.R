# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script: 05_comprehensive_robustness.R
# Purpose: Comprehensive robustness testing for dual-pathway hypotheses (LPM-2SLS)
#
# Research Question:
#   H1: Internal intervention (CDP SC) → Risk awareness → SBTi commitment
#   H2: External intervention (Incidents) → Risk awareness → SBTi commitment
#
# Strategy:
#   Phase 1: Alternative incident measures (highreach, highsev, all)
#   Phase 2: Transformations (log, binary)
#   Phase 3: Diagnostic checks (predicted values, reduced form, etc.)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load packages ####
library(tidyverse)
library(data.table)
library(fixest)      # For feols (fast FE estimation)
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

################################################################################
# CRITICAL FIX: CENSOR PANEL AT COMMITMENT TIME
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("CENSORING PANEL AT COMMITMENT TIME\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

cat("CRITICAL ISSUE: If firms stay in panel after committing, we get spurious correlation\n")
cat("SOLUTION: Drop all observations AFTER first commitment (keep commitment year itself)\n\n")

# Identify first commitment year for each firm
data[, first_commit_year := ifelse(any(sbti_commitment_lead1 == 1, na.rm = TRUE),
                                     min(year[sbti_commitment_lead1 == 1], na.rm = TRUE),
                                     NA_integer_),
     by = gvkey]

# Count observations before censoring
obs_before <- nrow(data)

# Keep observations up to and including first commitment year
# If firm never commits, keep all observations
data <- data[is.na(first_commit_year) | year <= first_commit_year]

obs_after <- nrow(data)

cat("  Observations before censoring:", obs_before, "\n")
cat("  Observations after censoring:", obs_after, "\n")
cat("  Observations dropped:", obs_before - obs_after, "\n")
cat("  Retention rate:", round(obs_after / obs_before * 100, 1), "%\n\n")

# Clean up temporary variable
data[, first_commit_year := NULL]

################################################################################
# DATA PREPARATION
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("DATA PREPARATION\n")
cat(strrep("=", 80) %+% "\n\n")

# Convert factors ####
data[, `:=`(
  headquarter_country = as.factor(headquarter_country),
  FourDigitName = as.factor(FourDigitName),
  year = as.factor(year),
  gvkey = as.factor(gvkey)
)]

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

# Remove singleton FE groups ####
cat("Removing singleton fixed effect groups...\n")

# Count observations per country-year FE group
data_complete[, fe_group_size := .N, by = .(headquarter_country, year)]
cat("  Singleton FE groups (n=1):", sum(data_complete$fe_group_size == 1), "obs\n")
data_complete <- data_complete[fe_group_size > 1]
data_complete[, fe_group_size := NULL]

cat("  Observations after removing singletons:", nrow(data_complete), "\n\n")

# Baseline outcome rate ####
cat("Baseline outcome statistics:\n")
cat("  SBTi commitment rate:",
    round(mean(data_complete$sbti_commitment_lead1) * 100, 2), "%\n")
cat("  Number of commitments:", sum(data_complete$sbti_commitment_lead1), "\n")
cat("  Number of firm-years:", nrow(data_complete), "\n\n")

################################################################################
# DIAGNOSTICS: FIRST-STAGE AND REDUCED FORM
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("DIAGNOSTIC CHECKS\n")
cat(strrep("=", 80) %+% "\n\n")

# Define controls formula ####
controls <- c("e_disc_coalesced_zeros", "e_disc_missing",
              "log(scope1_zeros + 1)", "scope1_missing",
              "roa_oibdp_w1_at_w1", "at_usd_winsorized_1_log",
              "tll_lt_w1_at_w1")
controls_formula <- paste(controls, collapse = " + ")

# First-stage regression ####
cat("--- First-Stage Regression ---\n")
cat("Endogenous: cdp_sc_member\n")
cat("Instrument: peer_cdp_share_country_lag\n\n")

first_stage <- feols(
  as.formula(paste("cdp_sc_member ~ peer_cdp_share_country_lag +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("First-stage results:\n")
cat("  Instrument coefficient:",
    round(coef(first_stage)["peer_cdp_share_country_lag"], 4), "\n")
cat("  Instrument t-statistic:",
    round(coeftable(first_stage)["peer_cdp_share_country_lag", "t value"], 2), "\n")
cat("  First-stage F-statistic:",
    round(coeftable(first_stage)["peer_cdp_share_country_lag", "t value"]^2, 2), "\n")

if (coeftable(first_stage)["peer_cdp_share_country_lag", "t value"]^2 < 10) {
  cat("  *** WARNING: F < 10 suggests WEAK INSTRUMENT ***\n")
} else {
  cat("  (F > 10 - instrument appears strong)\n")
}
cat("\n")

# Reduced form (instrument → outcome directly) ####
cat("--- Reduced Form Regression ---\n")
cat("Outcome: sbti_commitment_lead1\n")
cat("Key predictor: peer_cdp_share_country_lag (instrument)\n\n")

reduced_form <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ peer_cdp_share_country_lag + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Reduced form results:\n")
cat("  Instrument effect on outcome:",
    round(coef(reduced_form)["peer_cdp_share_country_lag"], 4), "\n")
cat("  T-statistic:",
    round(coeftable(reduced_form)["peer_cdp_share_country_lag", "t value"], 2), "\n")
cat("  P-value:",
    round(pvalue(reduced_form)["peer_cdp_share_country_lag"], 4), "\n\n")

cat("  Incidents effect (reduced form):",
    round(coef(reduced_form)["esc_incidents_highreach"], 4), "\n")
cat("  P-value:",
    round(pvalue(reduced_form)["esc_incidents_highreach"], 4), "\n\n")

# Calculate implied 2SLS coefficient manually
implied_2sls <- coef(reduced_form)["peer_cdp_share_country_lag"] /
                coef(first_stage)["peer_cdp_share_country_lag"]
cat("Implied 2SLS coefficient (reduced form / first stage):", round(implied_2sls, 4), "\n\n")

################################################################################
# PHASE 1: ALTERNATIVE INCIDENT MEASURES (2SLS)
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PHASE 1: ALTERNATIVE INCIDENT MEASURES (LPM-2SLS)\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Motivation: Test robustness across different incident operationalizations\n")
cat("Method: Standard 2SLS with feols\n")
cat("Testing: High reach, high severity, all incidents\n\n")

# Storage for results ####
results_list <- list()

# Model 1.1: High Reach Incidents ####
cat("--- Model 1.1: 2SLS with esc_incidents_highreach ---\n\n")

# 2SLS using feols syntax: outcome ~ exog | FE | endog ~ instrument
tsls_highreach <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(tsls_highreach)["fit_cdp_sc_member"], 4),
    ", SE:", round(se(tsls_highreach)["fit_cdp_sc_member"], 4),
    ", p:", round(pvalue(tsls_highreach)["fit_cdp_sc_member"], 4), "\n")
cat("  Incidents coefficient:", round(coef(tsls_highreach)["esc_incidents_highreach"], 4),
    ", SE:", round(se(tsls_highreach)["esc_incidents_highreach"], 4),
    ", p:", round(pvalue(tsls_highreach)["esc_incidents_highreach"], 4), "\n\n")

# Check predicted values
pred_highreach <- predict(tsls_highreach)
cat("Predicted value diagnostics:\n")
cat("  Mean:", round(mean(pred_highreach), 4), "\n")
cat("  Min:", round(min(pred_highreach), 4), "\n")
cat("  Max:", round(max(pred_highreach), 4), "\n")
cat("  # < 0:", sum(pred_highreach < 0), "\n")
cat("  # > 1:", sum(pred_highreach > 1), "\n\n")

results_list$tsls_highreach <- list(
  model = "2SLS",
  incident_measure = "highreach",
  cdp_coef = coef(tsls_highreach)["fit_cdp_sc_member"],
  cdp_se = se(tsls_highreach)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls_highreach)["fit_cdp_sc_member"],
  incident_coef = coef(tsls_highreach)["esc_incidents_highreach"],
  incident_se = se(tsls_highreach)["esc_incidents_highreach"],
  incident_p = pvalue(tsls_highreach)["esc_incidents_highreach"],
  pred_mean = mean(pred_highreach),
  pred_neg = sum(pred_highreach < 0),
  pred_over1 = sum(pred_highreach > 1),
  nobs = tsls_highreach$nobs
)

# Model 1.2: High Severity Incidents ####
cat("--- Model 1.2: 2SLS with esc_incidents_highsev ---\n\n")

tsls_highsev <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents_highsev +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(tsls_highsev)["fit_cdp_sc_member"], 4),
    ", SE:", round(se(tsls_highsev)["fit_cdp_sc_member"], 4),
    ", p:", round(pvalue(tsls_highsev)["fit_cdp_sc_member"], 4), "\n")
cat("  Incidents coefficient:", round(coef(tsls_highsev)["esc_incidents_highsev"], 4),
    ", SE:", round(se(tsls_highsev)["esc_incidents_highsev"], 4),
    ", p:", round(pvalue(tsls_highsev)["esc_incidents_highsev"], 4), "\n\n")

results_list$tsls_highsev <- list(
  model = "2SLS",
  incident_measure = "highsev",
  cdp_coef = coef(tsls_highsev)["fit_cdp_sc_member"],
  cdp_se = se(tsls_highsev)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls_highsev)["fit_cdp_sc_member"],
  incident_coef = coef(tsls_highsev)["esc_incidents_highsev"],
  incident_se = se(tsls_highsev)["esc_incidents_highsev"],
  incident_p = pvalue(tsls_highsev)["esc_incidents_highsev"],
  nobs = tsls_highsev$nobs
)

# Model 1.3: All Incidents ####
cat("--- Model 1.3: 2SLS with esc_incidents (all) ---\n\n")

tsls_all <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(tsls_all)["fit_cdp_sc_member"], 4),
    ", SE:", round(se(tsls_all)["fit_cdp_sc_member"], 4),
    ", p:", round(pvalue(tsls_all)["fit_cdp_sc_member"], 4), "\n")
cat("  Incidents coefficient:", round(coef(tsls_all)["esc_incidents"], 4),
    ", SE:", round(se(tsls_all)["esc_incidents"], 4),
    ", p:", round(pvalue(tsls_all)["esc_incidents"], 4), "\n\n")

results_list$tsls_all <- list(
  model = "2SLS",
  incident_measure = "all",
  cdp_coef = coef(tsls_all)["fit_cdp_sc_member"],
  cdp_se = se(tsls_all)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls_all)["fit_cdp_sc_member"],
  incident_coef = coef(tsls_all)["esc_incidents"],
  incident_se = se(tsls_all)["esc_incidents"],
  incident_p = pvalue(tsls_all)["esc_incidents"],
  nobs = tsls_all$nobs
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
cat("--- Model 2.1: 2SLS with log(esc_incidents_highreach + 1) ---\n\n")

data_complete$log_incidents_highreach <- log(data_complete$esc_incidents_highreach + 1)

tsls_log <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ log_incidents_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(tsls_log)["fit_cdp_sc_member"], 4),
    ", SE:", round(se(tsls_log)["fit_cdp_sc_member"], 4),
    ", p:", round(pvalue(tsls_log)["fit_cdp_sc_member"], 4), "\n")
cat("  Log incidents coefficient:", round(coef(tsls_log)["log_incidents_highreach"], 4),
    ", SE:", round(se(tsls_log)["log_incidents_highreach"], 4),
    ", p:", round(pvalue(tsls_log)["log_incidents_highreach"], 4), "\n\n")

results_list$tsls_log <- list(
  model = "2SLS",
  incident_measure = "log_highreach",
  cdp_coef = coef(tsls_log)["fit_cdp_sc_member"],
  cdp_se = se(tsls_log)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls_log)["fit_cdp_sc_member"],
  incident_coef = coef(tsls_log)["log_incidents_highreach"],
  incident_se = se(tsls_log)["log_incidents_highreach"],
  incident_p = pvalue(tsls_log)["log_incidents_highreach"],
  nobs = tsls_log$nobs
)

# Model 2.2: Binary Indicator ####
cat("--- Model 2.2: 2SLS with any_incident_highreach (binary) ---\n\n")

data_complete$any_incident_highreach <- as.numeric(data_complete$esc_incidents_highreach > 0)

tsls_binary <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ any_incident_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Results:\n")
cat("  CDP SC coefficient:", round(coef(tsls_binary)["fit_cdp_sc_member"], 4),
    ", SE:", round(se(tsls_binary)["fit_cdp_sc_member"], 4),
    ", p:", round(pvalue(tsls_binary)["fit_cdp_sc_member"], 4), "\n")
cat("  Binary incident coefficient:", round(coef(tsls_binary)["any_incident_highreach"], 4),
    ", SE:", round(se(tsls_binary)["any_incident_highreach"], 4),
    ", p:", round(pvalue(tsls_binary)["any_incident_highreach"], 4), "\n\n")

results_list$tsls_binary <- list(
  model = "2SLS",
  incident_measure = "binary_highreach",
  cdp_coef = coef(tsls_binary)["fit_cdp_sc_member"],
  cdp_se = se(tsls_binary)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls_binary)["fit_cdp_sc_member"],
  incident_coef = coef(tsls_binary)["any_incident_highreach"],
  incident_se = se(tsls_binary)["any_incident_highreach"],
  incident_p = pvalue(tsls_binary)["any_incident_highreach"],
  nobs = tsls_binary$nobs
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
         cdp_coef, cdp_p, incident_coef, incident_p, nobs,
         pred_mean, pred_neg, pred_over1) %>%
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
    )
  )

cat("Key Findings:\n\n")
cat("1. CDP SC Effect (H1):\n")
cat("   Range: [",
    round(min(summary_table$cdp_coef, na.rm = TRUE), 4), ",",
    round(max(summary_table$cdp_coef, na.rm = TRUE), 4), "]\n")
cat("   Significant in", sum(summary_table$cdp_p < 0.05, na.rm = TRUE), "of",
    sum(!is.na(summary_table$cdp_coef)), "specifications\n")

# Calculate interpretation
baseline <- mean(data_complete$sbti_commitment_lead1)
cdp_effect <- mean(summary_table$cdp_coef, na.rm = TRUE)
cat("   Baseline rate:", round(baseline * 100, 2), "%\n")
cat("   Mean coefficient:", round(cdp_effect, 4), "\n")
cat("   Implied probability:", round((baseline + cdp_effect) * 100, 2), "%\n")
if (baseline + cdp_effect > 1) {
  cat("   *** WARNING: Implied probability > 100% - IMPLAUSIBLE ***\n")
}
cat("\n")

cat("2. Incidents Effect (H2):\n")
cat("   Range: [",
    round(min(summary_table$incident_coef, na.rm = TRUE), 4), ",",
    round(max(summary_table$incident_coef, na.rm = TRUE), 4), "]\n")
cat("   Significant in", sum(summary_table$incident_p < 0.05, na.rm = TRUE), "of",
    sum(!is.na(summary_table$incident_coef)), "specifications\n\n")

cat("3. Prediction Quality:\n")
if (any(!is.na(summary_table$pred_neg))) {
  cat("   Negative predictions:", max(summary_table$pred_neg, na.rm = TRUE), "observations\n")
  cat("   Over-1 predictions:", max(summary_table$pred_over1, na.rm = TRUE), "observations\n")
  cat("   Mean predicted prob:", round(mean(summary_table$pred_mean, na.rm = TRUE), 4), "\n\n")
}

# Save results
write.csv(summary_table, "results_comprehensive_robustness_2sls.csv", row.names = FALSE)
cat("Results saved to: results_comprehensive_robustness_2sls.csv\n\n")

cat("=" %+% strrep("=", 79) %+% "\n")
cat("ANALYSIS COMPLETE\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")
