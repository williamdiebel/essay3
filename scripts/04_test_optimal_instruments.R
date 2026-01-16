# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script: 04_test_optimal_instruments.R
# Purpose: Test optimal instrument combination for strong + valid 2SLS analysis
#
# Strategy: Combination 4 (Recommended)
#   - peer_cdp_share_country_lag (existing, proven strong F=21.2)
#   - e_disc_lag2 (temporal, path dependence mechanism)
#   - industry_incidents_excl_lag (industry dynamics, external shock mechanism)
#
# Expected outcome:
#   - Strong instruments (joint F > 10)
#   - Valid instruments (Sargan p > 0.10)
#   - Conceptually distinct mechanisms (low correlation)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load required packages ####
library(tidyverse)
library(data.table)
library(fixest)
library(lfe)
library(AER)
library(sandwich)
library(lmtest)

# Set working directory ####
if (!dir.exists("cleaned_data")) {
  stop("Error: cleaned_data directory not found. Please run this script from the project root.")
}
setwd("cleaned_data")

################################################################################
# PART 1: LOAD DATA AND CONSTRUCT KEY INSTRUMENTS
################################################################################

cat("\\n================================================================================\\n")
cat("PART 1: DATA LOADING AND INSTRUMENT CONSTRUCTION\\n")
cat("================================================================================\\n\\n")

# Load base dataset ####
cat("Loading complete_data_2022_instrument_country.rds...\\n")
data <- readRDS("complete_data_2022_instrument_country.rds")
setDT(data)
cat("  - Loaded", nrow(data), "observations\\n")
cat("  - Unique firms:", uniqueN(data$gvkey), "\\n")
cat("  - Years:", min(data$year), "to", max(data$year), "\\n\\n")

# Construct Instrument 1: e_disc_lag2 ####
cat("Constructing Instrument 1: e_disc_lag2 (environmental disclosure, t-2)\\n")
cat("  Mechanism: Path dependence - firms with higher past disclosure more likely to join CDP\\n")
cat("  Exclusion: Past disclosure predetermined relative to current SBTi decision\\n\\n")

data[, e_disc_lag2 := shift(e_disc_coalesced_zeros, n = 2, type = "lag"),
     by = gvkey]

# Check coverage
pct_nonmissing <- sum(!is.na(data$e_disc_lag2)) / nrow(data) * 100
cat("  - Non-missing values:", round(pct_nonmissing, 1), "%\\n")
cat("  - Mean:", round(mean(data$e_disc_lag2, na.rm = TRUE), 3), "\\n")
cat("  - SD:", round(sd(data$e_disc_lag2, na.rm = TRUE), 3), "\\n\\n")

# Construct Instrument 2: industry_incidents_excl_lag ####
cat("Constructing Instrument 2: industry_incidents_excl_lag (industry incidents excl. own, t-1)\\n")
cat("  Mechanism: Industry-level incidents create legitimacy pressure for voluntary programs\\n")
cat("  Exclusion: Other firms' incidents don't directly affect own SBTi decision\\n\\n")

# Calculate industry-year total incidents
data[, industry_year_incidents := sum(esc_incidents_highreach, na.rm = TRUE),
     by = .(FourDigitName, year)]

# Subtract own firm incidents
data[, industry_incidents_excl := industry_year_incidents - esc_incidents_highreach]

# Lag by one year
data[, industry_incidents_excl_lag := shift(industry_incidents_excl, n = 1, type = "lag"),
     by = gvkey]

# Check coverage
pct_nonmissing2 <- sum(!is.na(data$industry_incidents_excl_lag)) / nrow(data) * 100
cat("  - Non-missing values:", round(pct_nonmissing2, 1), "%\\n")
cat("  - Mean:", round(mean(data$industry_incidents_excl_lag, na.rm = TRUE), 3), "\\n")
cat("  - SD:", round(sd(data$industry_incidents_excl_lag, na.rm = TRUE), 3), "\\n\\n")

# Verify Existing Instrument: peer_cdp_share_country_lag ####
cat("Verifying Existing Instrument: peer_cdp_share_country_lag\\n")
cat("  Mechanism: Country-level peer effects\\n")
cat("  Prior result: F=21.2 (strong)\\n\\n")

pct_nonmissing3 <- sum(!is.na(data$peer_cdp_share_country_lag)) / nrow(data) * 100
cat("  - Non-missing values:", round(pct_nonmissing3, 1), "%\\n")
cat("  - Mean:", round(mean(data$peer_cdp_share_country_lag, na.rm = TRUE), 3), "\\n")
cat("  - SD:", round(sd(data$peer_cdp_share_country_lag, na.rm = TRUE), 3), "\\n\\n")

# Create complete-case dataset ####
cat("Creating complete-case dataset with all three instruments...\\n")

# Select complete cases
data_complete <- data[!is.na(e_disc_lag2) &
                      !is.na(industry_incidents_excl_lag) &
                      !is.na(peer_cdp_share_country_lag) &
                      !is.na(sbti_commitment_lead1) &
                      !is.na(cdp_sc_member)]

cat("  - Complete cases:", nrow(data_complete), "observations\\n")
cat("  - Unique firms:", uniqueN(data_complete$gvkey), "\\n")
cat("  - Sample retention:", round(nrow(data_complete) / nrow(data) * 100, 1), "%\\n\\n")

# Instrument correlation matrix ####
cat("Instrument Correlation Matrix:\\n")
cat("(Low correlation = distinct mechanisms = good for overidentification)\\n\\n")

instruments <- c("peer_cdp_share_country_lag", "e_disc_lag2", "industry_incidents_excl_lag")
cor_matrix <- cor(data_complete[, instruments, with = FALSE])
print(round(cor_matrix, 3))
cat("\\n")

# Flag if any correlations are too high
max_cor <- max(abs(cor_matrix[upper.tri(cor_matrix)]))
if (max_cor > 0.7) {
  cat("  WARNING: Some instruments highly correlated (|r| > 0.7)\\n")
  cat("  This may reduce power of overidentification test\\n\\n")
} else {
  cat("  ✓ All pairwise correlations < 0.7 (good for overidentification)\\n\\n")
}

# Convert back to data.frame for felm/ivreg
data <- as.data.frame(data_complete)

################################################################################
# PART 2: FIRST-STAGE REGRESSIONS
################################################################################

cat("\\n================================================================================\\n")
cat("PART 2: FIRST-STAGE REGRESSIONS\\n")
cat("================================================================================\\n\\n")

# Define controls
controls_formula <- "esc_incidents_highreach +
  e_disc_coalesced_zeros + e_disc_missing +
  log(scope1_zeros + 1) + scope1_missing +
  roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1"

# First-stage: Test each instrument individually ####
cat("Testing each instrument individually (for comparison):\\n\\n")

# Instrument 1: country peer share
cat("Instrument 1: peer_cdp_share_country_lag\\n")
first_stage_1 <- feols(
  cdp_sc_member ~ peer_cdp_share_country_lag +
    esc_incidents_highreach + e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  data = data,
  cluster = ~ gvkey
)

t_1 <- coeftable(first_stage_1)["peer_cdp_share_country_lag", "t value"]
F_1 <- t_1^2
cat("  - Coefficient:", round(coef(first_stage_1)["peer_cdp_share_country_lag"], 3), "\\n")
cat("  - t-statistic:", round(t_1, 2), "\\n")
cat("  - F-statistic:", round(F_1, 2), "\\n")
cat("  - Result:", ifelse(F_1 > 10, "STRONG ✓", "WEAK ✗"), "\\n\\n")

# Instrument 2: e_disc_lag2
cat("Instrument 2: e_disc_lag2\\n")
first_stage_2 <- feols(
  cdp_sc_member ~ e_disc_lag2 +
    esc_incidents_highreach + e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  data = data,
  cluster = ~ gvkey
)

t_2 <- coeftable(first_stage_2)["e_disc_lag2", "t value"]
F_2 <- t_2^2
cat("  - Coefficient:", round(coef(first_stage_2)["e_disc_lag2"], 3), "\\n")
cat("  - t-statistic:", round(t_2, 2), "\\n")
cat("  - F-statistic:", round(F_2, 2), "\\n")
cat("  - Result:", ifelse(F_2 > 10, "STRONG ✓", "WEAK ✗"), "\\n\\n")

# Instrument 3: industry_incidents_excl_lag
cat("Instrument 3: industry_incidents_excl_lag\\n")
first_stage_3 <- feols(
  cdp_sc_member ~ industry_incidents_excl_lag +
    esc_incidents_highreach + e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  data = data,
  cluster = ~ gvkey
)

t_3 <- coeftable(first_stage_3)["industry_incidents_excl_lag", "t value"]
F_3 <- t_3^2
cat("  - Coefficient:", round(coef(first_stage_3)["industry_incidents_excl_lag"], 3), "\\n")
cat("  - t-statistic:", round(t_3, 2), "\\n")
cat("  - F-statistic:", round(F_3, 2), "\\n")
cat("  - Result:", ifelse(F_3 > 10, "STRONG ✓", "WEAK ✗"), "\\n\\n")

# First-stage: All three instruments jointly ####
cat("Testing all three instruments JOINTLY:\\n\\n")

first_stage_all <- feols(
  cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2 + industry_incidents_excl_lag +
    esc_incidents_highreach + e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1 |
    headquarter_country + year,
  data = data,
  cluster = ~ gvkey
)

# Joint F-test using Wald test
wald_result <- wald(first_stage_all,
                    c("peer_cdp_share_country_lag", "e_disc_lag2", "industry_incidents_excl_lag"))

cat("Joint F-test for all three instruments:\\n")
cat("  - F-statistic:", round(wald_result$stat, 2), "\\n")
cat("  - p-value:", format.pval(wald_result$p, digits = 3), "\\n")
cat("  - Result:", ifelse(wald_result$stat > 10, "STRONG ✓", "WEAK ✗"), "\\n\\n")

cat("Individual coefficients in joint first-stage:\\n")
for (inst in instruments) {
  coef_val <- coef(first_stage_all)[inst]
  se_val <- se(first_stage_all)[inst]
  t_val <- coef_val / se_val
  p_val <- 2 * pt(abs(t_val), df = first_stage_all$nobs - length(coef(first_stage_all)), lower.tail = FALSE)

  cat("  -", inst, ":\\n")
  cat("    Coefficient:", round(coef_val, 4), "\\n")
  cat("    SE:", round(se_val, 4), "\\n")
  cat("    t-statistic:", round(t_val, 2), "\\n")
  cat("    p-value:", format.pval(p_val, digits = 3), "\\n\\n")
}

################################################################################
# PART 3: SECOND-STAGE 2SLS ESTIMATION
################################################################################

cat("\\n================================================================================\\n")
cat("PART 3: SECOND-STAGE 2SLS ESTIMATION\\n")
cat("================================================================================\\n\\n")

# Model 1: Country peer share only (baseline for comparison) ####
cat("Model 1: Country peer share only (just-identified, baseline)\\n\\n")

model_1 <- felm(
  sbti_commitment_lead1 ~ esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1
  | headquarter_country + year
  | (cdp_sc_member ~ peer_cdp_share_country_lag)
  | gvkey,
  data = data
)

summary(model_1)

# Model 2: All three instruments (over-identified) ####
cat("\\n\\nModel 2: All three instruments (over-identified, MAIN MODEL)\\n\\n")

model_2 <- felm(
  sbti_commitment_lead1 ~ esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + tll_lt_w1_at_w1
  | headquarter_country + year
  | (cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2 + industry_incidents_excl_lag)
  | gvkey,
  data = data
)

summary(model_2)

################################################################################
# PART 4: DIAGNOSTIC TESTS
################################################################################

cat("\\n================================================================================\\n")
cat("PART 4: DIAGNOSTIC TESTS\\n")
cat("================================================================================\\n\\n")

# Alternative approach: Demean data to absorb fixed effects ####
# This avoids the formula length issue with creating all dummy variables
cat("Preparing data for diagnostic tests (demeaning to absorb fixed effects)...\\n\\n")

# Variables to demean
vars_to_demean <- c("sbti_commitment_lead1", "cdp_sc_member",
                    "esc_incidents_highreach", "e_disc_coalesced_zeros", "e_disc_missing",
                    "scope1_zeros", "scope1_missing",
                    "roa_oibdp_w1_at_w1", "at_usd_winsorized_1_log", "tll_lt_w1_at_w1",
                    "peer_cdp_share_country_lag", "e_disc_lag2", "industry_incidents_excl_lag")

# Convert to data.table for efficient group operations
setDT(data)

# Demean by country-year groups
data_demeaned <- copy(data)
for (var in vars_to_demean) {
  # Calculate country-year means
  data_demeaned[, paste0(var, "_mean_cy") := mean(get(var), na.rm = TRUE),
                by = .(headquarter_country, year)]
  # Demean
  data_demeaned[, paste0(var, "_dm") := get(var) - get(paste0(var, "_mean_cy"))]
}

# Convert back to data.frame for ivreg
data_demeaned_df <- as.data.frame(data_demeaned)

# Build ivreg formula with demeaned variables (no FE needed since absorbed)
cat("Running ivreg with demeaned data...\\n\\n")

ivreg_formula <- as.formula(
  paste("sbti_commitment_lead1_dm ~",
        "esc_incidents_highreach_dm + e_disc_coalesced_zeros_dm + e_disc_missing_dm +",
        "log(scope1_zeros_dm + 1) + scope1_missing_dm +",
        "roa_oibdp_w1_at_w1_dm + at_usd_winsorized_1_log_dm + tll_lt_w1_at_w1_dm +",
        "cdp_sc_member_dm |",
        "esc_incidents_highreach_dm + e_disc_coalesced_zeros_dm + e_disc_missing_dm +",
        "log(scope1_zeros_dm + 1) + scope1_missing_dm +",
        "roa_oibdp_w1_at_w1_dm + at_usd_winsorized_1_log_dm + tll_lt_w1_at_w1_dm +",
        "peer_cdp_share_country_lag_dm + e_disc_lag2_dm + industry_incidents_excl_lag_dm")
)

# Run ivreg
model_diag <- ivreg(ivreg_formula, data = data_demeaned_df)

# Get diagnostics
summary_diag <- summary(model_diag, diagnostics = TRUE)

cat("Diagnostic Tests for Model 2 (three instruments):\\n\\n")

cat("WEAK INSTRUMENTS TEST:\\n")
weak_test <- summary_diag$diagnostics["Weak instruments", , drop = FALSE]
print(weak_test)
cat("\\n  Interpretation:\\n")
cat("  - Null hypothesis: Instruments are weak\\n")
cat("  - If p < 0.05: REJECT null → instruments are strong ✓\\n")
cat("  - Result:", ifelse(weak_test[, "p-value"] < 0.05, "STRONG ✓", "WEAK ✗"), "\\n\\n")

cat("SARGAN-HANSEN J-TEST (OVERIDENTIFICATION):\\n")
sargan_test <- summary_diag$diagnostics["Sargan", , drop = FALSE]
print(sargan_test)
cat("\\n  Interpretation:\\n")
cat("  - Null hypothesis: Instruments are valid (exogenous)\\n")
cat("  - If p > 0.10: FAIL TO REJECT null → instruments are valid ✓\\n")
cat("  - If p < 0.05: REJECT null → at least one instrument is invalid ✗\\n")
cat("  - Result:", ifelse(sargan_test[, "p-value"] > 0.10, "VALID ✓", "INVALID ✗"), "\\n\\n")

cat("WU-HAUSMAN ENDOGENEITY TEST:\\n")
wu_hausman_test <- summary_diag$diagnostics["Wu-Hausman", , drop = FALSE]
print(wu_hausman_test)
cat("\\n  Interpretation:\\n")
cat("  - Null hypothesis: CDP membership is exogenous (OLS is consistent)\\n")
cat("  - If p < 0.05: REJECT null → CDP is endogenous, 2SLS needed ✓\\n")
cat("  - Result:", ifelse(wu_hausman_test[, "p-value"] < 0.05,
                          "ENDOGENOUS (2SLS needed) ✓",
                          "EXOGENOUS (OLS sufficient)"), "\\n\\n")

################################################################################
# PART 5: RESULTS SUMMARY
################################################################################

cat("\\n================================================================================\\n")
cat("RESULTS SUMMARY\\n")
cat("================================================================================\\n\\n")

# Extract key results
coef_model1 <- summary(model_1)$coefficients["cdp_sc_member", "Estimate"]
se_model1 <- summary(model_1)$coefficients["cdp_sc_member", "Cluster s.e."]
p_model1 <- summary(model_1)$coefficients["cdp_sc_member", "Pr(>|t|)"]

coef_model2 <- summary(model_2)$coefficients["cdp_sc_member", "Estimate"]
se_model2 <- summary(model_2)$coefficients["cdp_sc_member", "Cluster s.e."]
p_model2 <- summary(model_2)$coefficients["cdp_sc_member", "Pr(>|t|)"]

cat("COMPARISON TABLE:\\n\\n")
cat(sprintf("%-50s %15s %15s\\n", "Specification", "Model 1", "Model 2"))
cat(sprintf("%-50s %15s %15s\\n", "", "(Just-ID)", "(Over-ID)"))
cat(strrep("-", 80), "\\n")

cat(sprintf("%-50s %15s %15s\\n", "Instruments:",
            "Country peer", "Country + Hist + Ind"))

cat(sprintf("%-50s %15.3f %15.3f\\n", "CDP SC → SBTi coefficient", coef_model1, coef_model2))
cat(sprintf("%-50s %15.3f %15.3f\\n", "Cluster SE", se_model1, se_model2))
cat(sprintf("%-50s %15s %15s\\n", "p-value",
            format.pval(p_model1, digits = 3),
            format.pval(p_model2, digits = 3)))

cat(sprintf("%-50s %15.2f %15.2f\\n", "First-stage F-statistic", F_1, wald_result$stat))
cat(sprintf("%-50s %15s %15s\\n", "Weak instruments test",
            "N/A",
            ifelse(weak_test[, "p-value"] < 0.05, "PASS ✓", "FAIL ✗")))

cat(sprintf("%-50s %15s %15s\\n", "Sargan-Hansen J-test (p-value)",
            "N/A (just-ID)",
            format.pval(sargan_test[, "p-value"], digits = 3)))
cat(sprintf("%-50s %15s %15s\\n", "Instrument validity",
            "Untestable",
            ifelse(sargan_test[, "p-value"] > 0.10, "VALID ✓", "INVALID ✗")))

cat(sprintf("%-50s %15.3f %15.3f\\n", "Wu-Hausman test statistic",
            NA, wu_hausman_test[, "statistic"]))
cat(sprintf("%-50s %15s %15s\\n", "Endogeneity confirmed",
            "N/A",
            ifelse(wu_hausman_test[, "p-value"] < 0.05, "YES ✓", "NO")))

cat(sprintf("%-50s %15d %15d\\n", "N observations", model_1$N, model_2$N))

cat(strrep("=", 80), "\\n\\n")

# Final assessment
cat("FINAL ASSESSMENT:\\n\\n")

if (wald_result$stat > 10 && sargan_test[, "p-value"] > 0.10) {
  cat("✓✓✓ SUCCESS! ✓✓✓\\n\\n")
  cat("Model 2 achieves BOTH goals:\\n")
  cat("  1. STRONG instruments: Joint F =", round(wald_result$stat, 2), "> 10 ✓\\n")
  cat("  2. VALID instruments: Sargan p =", round(sargan_test[, "p-value"], 3), "> 0.10 ✓\\n\\n")
  cat("This specification resolves the validity-strength trade-off.\\n")
  cat("You now have an over-identified, defensible, and strong 2SLS analysis.\\n\\n")

  cat("Substantive interpretation:\\n")
  cat("  - CDP SC membership increases probability of SBTi commitment by",
      round(coef_model2 * 100, 1), "percentage points\\n")
  cat("  - Effect is", ifelse(p_model2 < 0.001, "highly significant (p<0.001)",
                               ifelse(p_model2 < 0.01, "significant (p<0.01)",
                                      ifelse(p_model2 < 0.05, "significant (p<0.05)",
                                             "not significant"))), "\\n")
} else if (wald_result$stat > 10 && sargan_test[, "p-value"] <= 0.10) {
  cat("⚠ PARTIAL SUCCESS ⚠\\n\\n")
  cat("  - Instruments are STRONG (F =", round(wald_result$stat, 2), ") ✓\\n")
  cat("  - But Sargan test FAILED (p =", round(sargan_test[, "p-value"], 3), ") ✗\\n\\n")
  cat("At least one instrument violates the exclusion restriction.\\n")
  cat("Recommendation: Test alternative instrument combinations or revert to Model 1.\\n")
} else if (wald_result$stat <= 10 && sargan_test[, "p-value"] > 0.10) {
  cat("⚠ PARTIAL SUCCESS ⚠\\n\\n")
  cat("  - Instruments are VALID (Sargan p =", round(sargan_test[, "p-value"], 3), ") ✓\\n")
  cat("  - But instruments are WEAK (F =", round(wald_result$stat, 2), ") ✗\\n\\n")
  cat("Weak instruments can bias 2SLS estimates toward OLS.\\n")
  cat("Recommendation: Test alternative instruments or revert to Model 1.\\n")
} else {
  cat("✗ FAILED ✗\\n\\n")
  cat("  - Instruments are WEAK (F =", round(wald_result$stat, 2), ") ✗\\n")
  cat("  - Instruments are INVALID (Sargan p =", round(sargan_test[, "p-value"], 3), ") ✗\\n\\n")
  cat("Recommendation: Revert to Model 1 or explore alternative instruments.\\n")
}

cat("\\n================================================================================\\n")
cat("END OF ANALYSIS\\n")
cat("================================================================================\\n")
