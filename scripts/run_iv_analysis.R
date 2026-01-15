# ══════════════════════════════════════════════════════════════════════════════
# STANDALONE IV ANALYSIS SCRIPT
# ══════════════════════════════════════════════════════════════════════════════
# This script runs 2SLS analysis using the pre-created instrument dataset
# Input:  cleaned_data/complete_data_2022_instrument.rds
# Output: Console output + markdown tables in tables/
# ══════════════════════════════════════════════════════════════════════════════

# Clear workspace
rm(list = ls())

# Load required packages
cat("Loading required packages...\n")
library(tidyverse)   # For data manipulation
library(fixest)      # For feols() regression with fixed effects
library(lfe)         # For felm() 2SLS with fixed effects
library(AER)         # For ivreg() and diagnostic tests
library(sandwich)    # For robust standard errors
library(lmtest)      # For coefficient tests

# ══════════════════════════════════════════════════════════════════════════════
# LOAD DATA
# ══════════════════════════════════════════════════════════════════════════════

cat("\nLoading data from cleaned_data/complete_data_2022_instrument.rds...\n")
complete_data_2022_instrument <- readRDS("cleaned_data/complete_data_2022_instrument.rds")

cat("  ✓ Data loaded successfully\n")
cat("  → Observations:", nrow(complete_data_2022_instrument), "\n")
cat("  → Variables:", ncol(complete_data_2022_instrument), "\n\n")

# Check that instruments exist
if (!("peer_cdp_share_lag" %in% names(complete_data_2022_instrument))) {
  stop("ERROR: peer_cdp_share_lag not found in dataset. Please run the full script first.")
}
if (!("peer_cdp_share_country_lag" %in% names(complete_data_2022_instrument))) {
  stop("ERROR: peer_cdp_share_country_lag not found in dataset. Please run the full script first.")
}

cat("  ✓ Both instruments found in dataset\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# FIRST-STAGE REGRESSION
# ══════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                         FIRST-STAGE REGRESSION                                \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

cat("Question: Do our instruments predict CDP Supply Chain membership?\n\n")

# First-stage regression with BOTH instruments
first_stage <- feols(
  cdp_sc_member ~ peer_cdp_share_lag + peer_cdp_share_country_lag +
    # Control variables:
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

# Display results
summary(first_stage)

# ══════════════════════════════════════════════════════════════════════════════
# INSTRUMENT STRENGTH TEST (F-test)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                    INSTRUMENT STRENGTH DIAGNOSTICS                           \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

# Extract coefficients for BOTH instruments
coef_table <- coeftable(first_stage)

# Individual t-statistics for each instrument
t_industry <- coef_table["peer_cdp_share_lag", "t value"]
t_country  <- coef_table["peer_cdp_share_country_lag", "t value"]

# Joint F-test using Wald test
wald_test <- wald(first_stage,
                  c("peer_cdp_share_lag = 0",
                    "peer_cdp_share_country_lag = 0"))

F_joint <- wald_test$stat

cat("Individual t-statistics:\n")
cat("  Industry instrument (peer_cdp_share_lag):        t =", round(t_industry, 3), "\n")
cat("  Country instrument (peer_cdp_share_country_lag): t =", round(t_country, 3), "\n\n")
cat("Joint F-test for both instruments:\n")
cat("  F-statistic =", round(F_joint, 2), "\n")
cat("  Rule of thumb: F > 10 indicates strong instruments\n\n")

if (F_joint > 10) {
  cat("  ✓✓✓ PASS: Instruments are sufficiently strong (F > 10)\n")
  cat("      → Your instruments have good predictive power\n")
  cat("      → 2SLS estimates should have minimal weak instrument bias\n")
} else {
  cat("  ✗✗✗ WARNING: Weak instruments detected (F < 10)\n")
  cat("      → 2SLS estimates may be biased toward OLS\n")
  cat("      → Consider finding stronger instruments\n")
}

cat("\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECOND-STAGE: 2SLS ESTIMATION
# ══════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                        SECOND-STAGE REGRESSION (2SLS)                        \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

cat("Now estimating the CAUSAL effect of CDP membership on SBTi commitment...\n\n")

# Ensure factor variables are properly coded
complete_data_2022_instrument <- complete_data_2022_instrument %>%
  mutate(
    headquarter_country = as.factor(headquarter_country),
    FourDigitName       = as.factor(FourDigitName),
    year                = as.factor(year),
    gvkey               = as.factor(gvkey)
  )

# 2SLS estimation using felm
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

# Display results
summary(second_stage)

# Extract key coefficient
second_stage_coef <- coeftable(second_stage)
cdp_var_name <- grep("cdp_sc_member", rownames(second_stage_coef), value = TRUE)[1]

coef_est <- second_stage_coef[cdp_var_name, "Estimate"]
coef_se  <- second_stage_coef[cdp_var_name, "Std. Error"]
coef_t   <- second_stage_coef[cdp_var_name, "t value"]
coef_p   <- second_stage_coef[cdp_var_name, "Pr(>|t|)"]

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                         KEY RESULT (2SLS COEFFICIENT)                        \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")
cat("Effect of CDP Supply Chain Membership on SBTi Commitment:\n\n")
cat("  Coefficient:  ", sprintf("%.4f", coef_est), "\n")
cat("  Std. Error:   ", sprintf("%.4f", coef_se), "\n")
cat("  t-statistic:  ", sprintf("%.3f", coef_t), "\n")
cat("  p-value:      ", sprintf("%.4f", coef_p), "\n\n")

if (coef_p < 0.001) {
  cat("  ✓✓✓ HIGHLY SIGNIFICANT (p < 0.001) ***\n")
} else if (coef_p < 0.01) {
  cat("  ✓✓ SIGNIFICANT (p < 0.01) **\n")
} else if (coef_p < 0.05) {
  cat("  ✓ SIGNIFICANT (p < 0.05) *\n")
} else if (coef_p < 0.10) {
  cat("  ~ MARGINALLY SIGNIFICANT (p < 0.10)\n")
} else {
  cat("  ✗ NOT SIGNIFICANT (p >= 0.10)\n")
}

cat("\nInterpretation:\n")
if (coef_est > 0 && coef_p < 0.05) {
  cat("  → CDP Supply Chain membership INCREASES the probability of SBTi commitment\n")
  cat("  → Effect size: ", round(coef_est * 100, 2), " percentage points\n", sep = "")
  cat("  → This is the CAUSAL effect (endogeneity-adjusted)\n")
} else if (coef_est < 0 && coef_p < 0.05) {
  cat("  → CDP Supply Chain membership DECREASES the probability of SBTi commitment\n")
  cat("  → Effect size: ", round(abs(coef_est) * 100, 2), " percentage points\n", sep = "")
  cat("  → (Unexpected direction - check data)\n")
} else {
  cat("  → No statistically significant effect detected\n")
  cat("  → Cannot reject the null hypothesis of no effect\n")
}

cat("\n")

# ══════════════════════════════════════════════════════════════════════════════
# SARGAN-HANSEN OVERIDENTIFICATION TEST
# ══════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                  OVERIDENTIFICATION TEST (Sargan-Hansen J)                   \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

cat("Testing whether our instruments are valid (exogenous)...\n\n")

# Prepare data
data_complete <- complete_data_2022_instrument %>%
  filter(!is.na(sbti_commitment_lead1),
         !is.na(cdp_sc_member),
         !is.na(peer_cdp_share_lag),
         !is.na(peer_cdp_share_country_lag)) %>%
  mutate(year = as.factor(year),
         headquarter_country = as.factor(headquarter_country))

# Run 2SLS using ivreg for diagnostics
iv_model <- ivreg(
  sbti_commitment_lead1 ~ cdp_sc_member +
    esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
    tll_lt_w1_at_w1 +
    year + headquarter_country |
    esc_incidents_highreach +
    e_disc_coalesced_zeros + e_disc_missing +
    log(scope1_zeros + 1) + scope1_missing +
    roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
    tll_lt_w1_at_w1 +
    year + headquarter_country +
    peer_cdp_share_lag + peer_cdp_share_country_lag,
  data = data_complete
)

# Sargan-Hansen test
j_test_results <- tryCatch({
  resid_2sls <- residuals(iv_model)

  aux_reg <- lm(resid_2sls ~ peer_cdp_share_lag + peer_cdp_share_country_lag +
                  esc_incidents_highreach +
                  e_disc_coalesced_zeros + e_disc_missing +
                  log(scope1_zeros + 1) + scope1_missing +
                  roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log +
                  tll_lt_w1_at_w1 +
                  year + headquarter_country,
                data = data_complete)

  n <- nobs(aux_reg)
  r_squared <- summary(aux_reg)$r.squared
  J_stat <- n * r_squared
  df_J <- 1
  p_value_J <- 1 - pchisq(J_stat, df = df_J)

  cat("J-statistic:       ", round(J_stat, 3), "\n")
  cat("Degrees of freedom:", df_J, "\n")
  cat("P-value:           ", round(p_value_J, 4), "\n\n")

  cat("H0: Both instruments are valid (exogenous)\n\n")

  if (p_value_J > 0.05) {
    cat("  ✓✓✓ PASS: Cannot reject H0 (p > 0.05)\n")
    cat("      → No evidence against instrument validity\n")
    cat("      → Both instruments appear to satisfy the exclusion restriction\n")
    cat("      → Your 2SLS estimates are likely credible\n")
  } else {
    cat("  ✗✗✗ WARNING: Reject H0 (p < 0.05)\n")
    cat("      → Evidence that at least one instrument may be invalid\n")
    cat("      → One or both instruments may be correlated with the error term\n")
    cat("      → 2SLS estimates may still be biased\n")
  }

  cat("\nCAVEAT: J-test has low power. Passing doesn't prove validity,\n")
  cat("        but failing raises serious concerns.\n\n")

  data.frame(
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
  cat("Error:", e$message, "\n\n")
  NULL
})

# ══════════════════════════════════════════════════════════════════════════════
# EXPORT RESULTS TO MARKDOWN TABLES
# ══════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                        EXPORTING RESULTS TO TABLES/                          \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

# Create tables directory
if (!dir.exists("tables")) {
  dir.create("tables")
  cat("  → Created tables/ directory\n")
}

# TABLE 1: First-Stage Results
first_stage_coef_table <- coeftable(first_stage)
first_stage_table <- data.frame(
  Variable = c("Industry peer share (lagged)", "Country peer share (lagged)"),
  Coefficient = c(
    round(first_stage_coef_table["peer_cdp_share_lag", "Estimate"], 4),
    round(first_stage_coef_table["peer_cdp_share_country_lag", "Estimate"], 4)
  ),
  Std_Error = c(
    round(first_stage_coef_table["peer_cdp_share_lag", "Std. Error"], 4),
    round(first_stage_coef_table["peer_cdp_share_country_lag", "Std. Error"], 4)
  ),
  t_value = c(
    round(first_stage_coef_table["peer_cdp_share_lag", "t value"], 3),
    round(first_stage_coef_table["peer_cdp_share_country_lag", "t value"], 3)
  ),
  p_value = c(
    format.pval(first_stage_coef_table["peer_cdp_share_lag", "Pr(>|t|)"], digits = 3),
    format.pval(first_stage_coef_table["peer_cdp_share_country_lag", "Pr(>|t|)"], digits = 3)
  ),
  stringsAsFactors = FALSE
)

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
cat("  ✓ Saved tables/first_stage_results.md\n")

# TABLE 2: Instrument Strength
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
    "19.93",
    ifelse(F_joint > 10, "PASS: Strong instruments", "WARNING: Weak instruments")
  ),
  stringsAsFactors = FALSE
)

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
    ""
  ),
  "tables/instrument_strength.md"
)
cat("  ✓ Saved tables/instrument_strength.md\n")

# TABLE 3: Second-Stage Results
second_stage_table <- data.frame(
  Variable = "CDP Supply Chain Member (instrumented)",
  Coefficient = round(coef_est, 4),
  Std_Error = round(coef_se, 4),
  t_value = round(coef_t, 3),
  p_value = format.pval(coef_p, digits = 3),
  Significant = ifelse(coef_p < 0.05, "Yes ***", "No"),
  stringsAsFactors = FALSE
)

writeLines(
  c("# Second-Stage Regression Results (2SLS)",
    "",
    "**Dependent Variable:** SBTi Commitment (lead 1 year)",
    "",
    "**Method:** Two-Stage Least Squares (2SLS) with country and year fixed effects",
    "",
    "**Instrumented Variable:**",
    "",
    paste0("| Variable | Coefficient | Std. Error | t-value | p-value | Significant? |"),
    paste0("|----------|-------------|------------|---------|---------|--------------|"),
    paste0("| ", second_stage_table$Variable, " | ",
           second_stage_table$Coefficient, " | ",
           second_stage_table$Std_Error, " | ",
           second_stage_table$t_value, " | ",
           second_stage_table$p_value, " | ",
           second_stage_table$Significant, " |"),
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
    ""
  ),
  "tables/second_stage_results.md"
)
cat("  ✓ Saved tables/second_stage_results.md\n")

# TABLE 4: Sargan-Hansen test
if (!is.null(j_test_results)) {
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
      "",
      "**Interpretation:**",
      paste0("- p-value = ", j_test_results$P_value),
      ifelse(j_test_results$P_value > 0.05,
             "- ✓ PASS: Cannot reject H0 (p > 0.05) - instruments appear valid",
             "- ✗ WARNING: Reject H0 (p < 0.05) - evidence of invalid instrument(s)"),
      ""
    ),
    "tables/sargan_hansen_test.md"
  )
  cat("  ✓ Saved tables/sargan_hansen_test.md\n")
}

# TABLE 5: Summary
summary_lines <- c(
  "# Summary of Instrumental Variable Diagnostics",
  "",
  "## Key Results",
  "",
  paste0("**1. Instrument Strength:** ", ifelse(F_joint > 10, "✓ PASS", "✗ FAIL")),
  paste0("   - Joint F-statistic: ", round(F_joint, 2)),
  paste0("   - Assessment: ", ifelse(F_joint > 10, "Strong instruments", "Weak instruments")),
  ""
)

if (!is.null(j_test_results)) {
  summary_lines <- c(summary_lines,
    paste0("**2. Instrument Validity:** ", ifelse(j_test_results$P_value > 0.05, "✓ PASS", "✗ FAIL")),
    paste0("   - J-statistic: ", j_test_results$Value),
    paste0("   - p-value: ", j_test_results$P_value),
    paste0("   - Assessment: ", ifelse(j_test_results$P_value > 0.05,
                                       "No evidence against validity",
                                       "Possible invalid instrument")),
    ""
  )
}

summary_lines <- c(summary_lines,
  paste0("**3. Main Effect (2SLS):**"),
  paste0("   - Coefficient: ", round(coef_est, 4)),
  paste0("   - p-value: ", round(coef_p, 4)),
  paste0("   - Significant: ", ifelse(coef_p < 0.05, "Yes ***", "No")),
  "",
  "## Conclusion",
  "",
  ifelse(F_joint > 10 && coef_p < 0.05,
         "The instruments are strong, and CDP membership has a statistically significant causal effect on SBTi commitment.",
         ifelse(F_joint <= 10,
                "WARNING: Weak instruments detected. Results may be unreliable.",
                "Instruments are strong, but the main effect is not statistically significant."))
)

writeLines(summary_lines, "tables/iv_diagnostics_summary.md")
cat("  ✓ Saved tables/iv_diagnostics_summary.md\n")

# ══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY
# ══════════════════════════════════════════════════════════════════════════════

cat("\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("                              ANALYSIS COMPLETE                                \n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")

cat("SUMMARY OF FINDINGS:\n\n")

cat("1. INSTRUMENT STRENGTH:\n")
cat("   F-statistic =", round(F_joint, 2), "\n")
if (F_joint > 10) {
  cat("   ✓✓✓ STRONG INSTRUMENTS - Results are reliable\n\n")
} else {
  cat("   ✗✗✗ WEAK INSTRUMENTS - Results may be unreliable\n\n")
}

if (!is.null(j_test_results)) {
  cat("2. INSTRUMENT VALIDITY:\n")
  cat("   p-value =", round(j_test_results$P_value, 4), "\n")
  if (j_test_results$P_value > 0.05) {
    cat("   ✓✓✓ VALID INSTRUMENTS - No evidence against exogeneity\n\n")
  } else {
    cat("   ✗✗✗ QUESTIONABLE VALIDITY - Possible endogeneity issues\n\n")
  }
}

cat("3. MAIN CAUSAL EFFECT:\n")
cat("   Coefficient =", round(coef_est, 4), "\n")
cat("   p-value =", round(coef_p, 4), "\n")
if (coef_p < 0.05) {
  cat("   ✓✓✓ STATISTICALLY SIGNIFICANT\n")
  cat("   Effect: CDP membership", ifelse(coef_est > 0, "INCREASES", "DECREASES"),
      "SBTi commitment by", round(abs(coef_est) * 100, 2), "percentage points\n\n")
} else {
  cat("   ✗ NOT SIGNIFICANT - Cannot detect a causal effect\n\n")
}

cat("All results have been exported to the tables/ directory.\n")
cat("See tables/iv_diagnostics_summary.md for a complete overview.\n\n")

cat("══════════════════════════════════════════════════════════════════════════════\n\n")
