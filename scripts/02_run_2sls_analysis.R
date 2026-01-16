# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script: 02_run_2sls_analysis.R
# Purpose: Run Two-Stage Least Squares (2SLS) regressions with comprehensive
#          diagnostics for instrumental variables analysis
#
# Input:
#   - complete_data_2022_instrument_country.rds
#
# Output:
#   - Console output with regression results and diagnostic tests
#   - (Optional) Export results to tables
#
# Description:
#   This script estimates the causal effect of CDP Supply Chain membership on
#   SBTi commitment using instrumental variables. It includes:
#   - First-stage regressions with F-tests for weak instruments
#   - Second-stage 2SLS estimates
#   - Sargan-Hansen J-test for overidentification
#   - Wu-Hausman endogeneity test
#   - Multiple specifications (different instruments, incident measures, FE)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load required packages ####
library(tidyverse)
library(fixest)      # For feols (faster fixed effects estimation)
library(lfe)         # For felm (IV with fixed effects)
library(AER)         # For ivreg and diagnostic tests
library(sandwich)    # For robust standard errors
library(lmtest)      # For hypothesis tests

# Set working directory to cleaned_data folder ####
# Note: Run this script from the project root directory (essay3/)
if (!dir.exists("cleaned_data")) {
  stop("Error: cleaned_data directory not found. Please run this script from the project root.")
}
setwd("cleaned_data")

# Load data ####
cat("Loading complete_data_2022_instrument_country.rds...\n")
data <- readRDS("complete_data_2022_instrument_country.rds")

cat("  - Observations:", nrow(data), "\n")
cat("  - Unique firms:", length(unique(data$gvkey)), "\n")
cat("  - Year range:", min(data$year), "-", max(data$year), "\n\n")

# Convert categorical variables to factors ####
data <- data %>%
  mutate(
    headquarter_country = as.factor(headquarter_country),
    FourDigitName = as.factor(FourDigitName),
    year = as.factor(year),
    gvkey = as.factor(gvkey)
  )

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PART 1: FIRST-STAGE REGRESSIONS AND INSTRUMENT STRENGTH TESTS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("=" %+% strrep("=", 79) %+% "\n")
cat("PART 1: FIRST-STAGE REGRESSIONS - INSTRUMENT RELEVANCE\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# Define control formula (used across all specifications)
controls_formula <- "esc_incidents_highreach +
                     e_disc_coalesced_zeros + e_disc_missing +
                     log(scope1_zeros + 1) + scope1_missing +
                     roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
                     tll_lt_w1_at_w1"

# First-stage: Industry instrument only ####
cat("--- First-Stage 1: Industry-level instrument (peer_cdp_share_lag) ---\n\n")

first_stage_industry <- feols(
  as.formula(paste("cdp_sc_member ~ peer_cdp_share_lag +", controls_formula,
                   "| headquarter_country + year")),
  data = data,
  cluster = ~ gvkey
)

summary(first_stage_industry)

# Extract F-statistic for instrument
t_industry <- coeftable(first_stage_industry)["peer_cdp_share_lag", "t value"]
F_industry <- t_industry^2
cat("\n*** First-stage strength (Industry instrument) ***\n")
cat("    t-statistic =", round(t_industry, 3), "\n")
cat("    F-statistic =", round(F_industry, 3), "\n")
if (F_industry >= 10) {
  cat("    ✓ Strong instrument (F >= 10)\n")
} else {
  cat("    ✗ Weak instrument warning (F < 10)\n")
}
cat("\n")

# First-stage: Country instrument only ####
cat("\n--- First-Stage 2: Country-level instrument (peer_cdp_share_country_lag) ---\n\n")

first_stage_country <- feols(
  as.formula(paste("cdp_sc_member ~ peer_cdp_share_country_lag +", controls_formula,
                   "| headquarter_country + year")),
  data = data,
  cluster = ~ gvkey
)

summary(first_stage_country)

# Extract F-statistic for instrument
t_country <- coeftable(first_stage_country)["peer_cdp_share_country_lag", "t value"]
F_country <- t_country^2
cat("\n*** First-stage strength (Country instrument) ***\n")
cat("    t-statistic =", round(t_country, 3), "\n")
cat("    F-statistic =", round(F_country, 3), "\n")
if (F_country >= 10) {
  cat("    ✓ Strong instrument (F >= 10)\n")
} else {
  cat("    ✗ Weak instrument warning (F < 10)\n")
}
cat("\n")

# First-stage: Both instruments ####
cat("\n--- First-Stage 3: Both instruments (overidentified) ---\n\n")

first_stage_both <- feols(
  as.formula(paste("cdp_sc_member ~ peer_cdp_share_lag + peer_cdp_share_country_lag +",
                   controls_formula, "| headquarter_country + year")),
  data = data,
  cluster = ~ gvkey
)

summary(first_stage_both)

# Joint F-test for both instruments
# For feols, we can use wald test
wald_result <- wald(first_stage_both,
                    vcov = "cluster",
                    keep = c("peer_cdp_share_lag", "peer_cdp_share_country_lag"))
cat("\n*** Joint F-test for both instruments ***\n")
print(wald_result)
cat("\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PART 2: SECOND-STAGE 2SLS REGRESSIONS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PART 2: SECOND-STAGE 2SLS REGRESSIONS\n")
cat(strrep("=", 80) %+% "\n\n")

# Model 1: Industry instrument, high-reach incidents ####
cat("--- Model 1: Industry instrument + esc_incidents_highreach ---\n\n")

model_1 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_formula,
                   "| headquarter_country + year",
                   "| (cdp_sc_member ~ peer_cdp_share_lag)",
                   "| gvkey")),
  data = data
)

summary(model_1)
cat("\n")

# Model 2: Country instrument, high-reach incidents ####
cat("\n--- Model 2: Country instrument + esc_incidents_highreach ---\n\n")

model_2 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_formula,
                   "| headquarter_country + year",
                   "| (cdp_sc_member ~ peer_cdp_share_country_lag)",
                   "| gvkey")),
  data = data
)

summary(model_2)
cat("\n")

# Model 3: Both instruments (overidentified), high-reach incidents ####
cat("\n--- Model 3: Both instruments (overidentified) + esc_incidents_highreach ---\n\n")

model_3 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_formula,
                   "| headquarter_country + year",
                   "| (cdp_sc_member ~ peer_cdp_share_lag + peer_cdp_share_country_lag)",
                   "| gvkey")),
  data = data
)

summary(model_3)
cat("\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PART 3: ALTERNATIVE SPECIFICATIONS - INCIDENT MEASURES ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PART 3: ALTERNATIVE INCIDENT MEASURES (using industry instrument)\n")
cat(strrep("=", 80) %+% "\n\n")

# Define controls for each incident type
controls_all_incidents <- "esc_incidents +
                           e_disc_coalesced_zeros + e_disc_missing +
                           log(scope1_zeros + 1) + scope1_missing +
                           roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
                           tll_lt_w1_at_w1"

controls_highsev <- "esc_incidents_highsev +
                     e_disc_coalesced_zeros + e_disc_missing +
                     log(scope1_zeros + 1) + scope1_missing +
                     roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
                     tll_lt_w1_at_w1"

# Model 4: All incidents (esc_incidents) ####
cat("--- Model 4: Industry instrument + esc_incidents (all) ---\n\n")

model_4 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_all_incidents,
                   "| headquarter_country + year",
                   "| (cdp_sc_member ~ peer_cdp_share_lag)",
                   "| gvkey")),
  data = data
)

summary(model_4)
cat("\n")

# Model 5: High-severity incidents (esc_incidents_highsev) ####
cat("\n--- Model 5: Industry instrument + esc_incidents_highsev ---\n\n")

model_5 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_highsev,
                   "| headquarter_country + year",
                   "| (cdp_sc_member ~ peer_cdp_share_lag)",
                   "| gvkey")),
  data = data
)

summary(model_5)
cat("\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PART 4: ALTERNATIVE FIXED EFFECTS SPECIFICATIONS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PART 4: ALTERNATIVE FIXED EFFECTS (using industry instrument + highreach)\n")
cat(strrep("=", 80) %+% "\n\n")

# Model 6: Country + Year FE (no firm FE) ####
cat("--- Model 6: Country + Year FE ---\n\n")

model_6 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_formula,
                   "| headquarter_country + year",
                   "| (cdp_sc_member ~ peer_cdp_share_lag)",
                   "| gvkey")),  # Still cluster by firm
  data = data
)

summary(model_6)
cat("\n")

# Model 7: Country + Industry + Year FE ####
cat("\n--- Model 7: Country + Industry + Year FE ---\n\n")

model_7 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_formula,
                   "| headquarter_country + FourDigitName + year",
                   "| (cdp_sc_member ~ peer_cdp_share_lag)",
                   "| gvkey")),
  data = data
)

summary(model_7)
cat("\n")

# Model 8: Firm + Year FE ####
cat("\n--- Model 8: Firm + Year FE ---\n\n")

model_8 <- felm(
  as.formula(paste("sbti_commitment_lead1 ~", controls_formula,
                   "| gvkey + year",
                   "| (cdp_sc_member ~ peer_cdp_share_lag)",
                   "| gvkey")),
  data = data
)

summary(model_8)
cat("\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PART 5: DIAGNOSTIC TESTS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PART 5: DIAGNOSTIC TESTS\n")
cat(strrep("=", 80) %+% "\n\n")

# Note: felm from lfe package doesn't provide built-in overidentification tests
# We'll use AER::ivreg for more comprehensive diagnostics

cat("Re-estimating selected models with AER::ivreg for diagnostic tests...\n\n")

# Prepare data for ivreg (need to manually create FE dummies or demean)
# For simplicity, we'll run without fixed effects for diagnostic purposes
# Note: In practice, you may want to demean the data by FE groups first

# Model for diagnostics: Industry instrument ####
cat("--- Diagnostic Tests: Model with Industry Instrument ---\n\n")

# Create formula for ivreg (no fixed effects for diagnostics)
# ivreg syntax: y ~ exog + endog | exog + instruments
ivreg_formula_industry <- as.formula(
  paste("sbti_commitment_lead1 ~", controls_formula, "+ cdp_sc_member |",
        controls_formula, "+ peer_cdp_share_lag")
)

model_diag_industry <- ivreg(
  ivreg_formula_industry,
  data = data
)

summary(model_diag_industry, diagnostics = TRUE)

cat("\n*** Weak Instruments Test (First-stage F) ***\n")
# Extract first-stage F-stat from diagnostics
summary_diag <- summary(model_diag_industry, diagnostics = TRUE)
print(summary_diag$diagnostics["Weak instruments", , drop = FALSE])

cat("\n*** Wu-Hausman Test for Endogeneity ***\n")
print(summary_diag$diagnostics["Wu-Hausman", , drop = FALSE])
cat("(Null hypothesis: cdp_sc_member is exogenous)\n")
cat("(Reject null if p < 0.05 → endogeneity present)\n")

cat("\n")

# Model for diagnostics: Both instruments (overidentification) ####
cat("\n--- Diagnostic Tests: Model with Both Instruments (Overidentified) ---\n\n")

# ivreg syntax: y ~ exog + endog | exog + instruments
ivreg_formula_both <- as.formula(
  paste("sbti_commitment_lead1 ~", controls_formula, "+ cdp_sc_member |",
        controls_formula, "+ peer_cdp_share_lag + peer_cdp_share_country_lag")
)

model_diag_both <- ivreg(
  ivreg_formula_both,
  data = data
)

summary(model_diag_both, diagnostics = TRUE)

cat("\n*** Weak Instruments Test (First-stage F) ***\n")
summary_diag_both <- summary(model_diag_both, diagnostics = TRUE)
print(summary_diag_both$diagnostics["Weak instruments", , drop = FALSE])

cat("\n*** Sargan-Hansen J-Test for Overidentification ***\n")
print(summary_diag_both$diagnostics["Sargan", , drop = FALSE])
cat("(Null hypothesis: instruments are valid)\n")
cat("(Fail to reject null if p > 0.05 → instruments valid)\n")

cat("\n*** Wu-Hausman Test for Endogeneity ***\n")
print(summary_diag_both$diagnostics["Wu-Hausman", , drop = FALSE])
cat("(Null hypothesis: cdp_sc_member is exogenous)\n")
cat("(Reject null if p < 0.05 → endogeneity present)\n")

cat("\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PART 6: BASELINE OLS MODELS FOR COMPARISON ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PART 6: BASELINE OLS MODELS (for comparison with 2SLS)\n")
cat(strrep("=", 80) %+% "\n\n")

# OLS Model 1: Country + Year FE ####
cat("--- OLS Model 1: Country + Year FE ---\n\n")

ols_1 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member +", controls_formula,
                   "| headquarter_country + year")),
  data = data,
  cluster = ~ gvkey
)

summary(ols_1)
cat("\n")

# OLS Model 2: Country + Industry + Year FE ####
cat("\n--- OLS Model 2: Country + Industry + Year FE ---\n\n")

ols_2 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member +", controls_formula,
                   "| headquarter_country + FourDigitName + year")),
  data = data,
  cluster = ~ gvkey
)

summary(ols_2)
cat("\n")

# OLS Model 3: Firm + Year FE ####
cat("\n--- OLS Model 3: Firm + Year FE ---\n\n")

ols_3 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member +", controls_formula,
                   "| gvkey + year")),
  data = data,
  cluster = ~ gvkey
)

summary(ols_3)
cat("\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# SUMMARY TABLE ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("SUMMARY OF KEY RESULTS\n")
cat(strrep("=", 80) %+% "\n\n")

# Extract coefficients for cdp_sc_member across models
results_summary <- data.frame(
  Model = c("OLS: Country+Year FE",
            "OLS: Country+Industry+Year FE",
            "OLS: Firm+Year FE",
            "2SLS: Industry IV, Country+Year FE",
            "2SLS: Country IV, Country+Year FE",
            "2SLS: Both IVs, Country+Year FE",
            "2SLS: Industry IV, Country+Industry+Year FE",
            "2SLS: Industry IV, Firm+Year FE"),
  Coefficient = c(
    coef(ols_1)["cdp_sc_member"],
    coef(ols_2)["cdp_sc_member"],
    coef(ols_3)["cdp_sc_member"],
    coef(model_1)["`cdp_sc_member(fit)`"],
    coef(model_2)["`cdp_sc_member(fit)`"],
    coef(model_3)["`cdp_sc_member(fit)`"],
    coef(model_7)["`cdp_sc_member(fit)`"],
    coef(model_8)["`cdp_sc_member(fit)`"]
  ),
  Std_Error = c(
    se(ols_1)["cdp_sc_member"],
    se(ols_2)["cdp_sc_member"],
    se(ols_3)["cdp_sc_member"],
    se(model_1)["`cdp_sc_member(fit)`"],
    se(model_2)["`cdp_sc_member(fit)`"],
    se(model_3)["`cdp_sc_member(fit)`"],
    se(model_7)["`cdp_sc_member(fit)`"],
    se(model_8)["`cdp_sc_member(fit)`"]
  )
)

results_summary$t_stat <- results_summary$Coefficient / results_summary$Std_Error
results_summary$p_value <- 2 * pt(-abs(results_summary$t_stat),
                                   df = nrow(data) - 50)  # Approximate df

print(results_summary, digits = 4)

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 80) %+% "\n")
