# ==============================================================================
# Simplified IV Analysis Script
# ==============================================================================
# This script:
# 1. Loads existing complete_data_2022_instrument.rds
# 2. Adds country-based peer instrument
# 3. Runs 2SLS regression with both instruments
# 4. Performs diagnostic tests (F-test, J-test)
# 5. Exports results to tables/
# ==============================================================================

library(tidyverse)
library(data.table)
library(lfe)      # For felm() function (2SLS with fixed effects)
library(AER)      # For ivreg() and diagnostic tests
library(sandwich) # For robust standard errors
library(lmtest)   # For coefficient tests

# Set working directory to data folder
setwd("~/Dropbox/NetZero WD Shared Folder/Data")

cat("\n=== Loading Data ===\n")
# Load the existing dataset
complete_data_2022_instrument <- readRDS("complete_data_2022_instrument.rds")
setDT(complete_data_2022_instrument)

cat("Dataset loaded:", nrow(complete_data_2022_instrument), "rows,",
    ncol(complete_data_2022_instrument), "columns\n")

# ==============================================================================
# Add Country-Based Peer Instrument
# ==============================================================================

cat("\n=== Creating Country-Based Peer Instrument ===\n")

# Create country-based peer share instrument
# Logic: For each country-year, compute the share of firms in CDP SCP
# excluding the focal firm itself
complete_data_2022_instrument[, peer_cdp_share_country :=
                                ifelse(.N == 1, NA_real_,
                                       (sum(cdp_sc_member, na.rm = TRUE) - cdp_sc_member) /
                                         (.N - 1)),
                              by = .(headquarter_country, year)]

# Lag the country-based peer share by 1 year
complete_data_2022_instrument[, peer_cdp_share_country_lag :=
                                shift(peer_cdp_share_country, n = 1, type = "lag"),
                              by = gvkey]

# Replace missing values with 0 (firms with no peers in their country)
complete_data_2022_instrument[is.na(peer_cdp_share_country_lag),
                              peer_cdp_share_country_lag := 0]

cat("Country instrument created successfully!\n")
cat("Summary statistics:\n")
print(summary(complete_data_2022_instrument$peer_cdp_share_country_lag))

# Save the updated dataset
cat("\nSaving updated dataset...\n")
saveRDS(complete_data_2022_instrument, "complete_data_2022_instrument.rds")
cat("Dataset saved!\n")

# ==============================================================================
# Two-Stage Least Squares (2SLS) Regression Analysis
# ==============================================================================

cat("\n=== Starting 2SLS Regression Analysis ===\n")

# Convert to data.frame for regression functions
analysis_data <- as.data.frame(complete_data_2022_instrument)

# Define the regression formula components
# Endogenous variable: cdp_sc_member
# Instruments: peer_cdp_share_lag (industry), peer_cdp_share_country_lag (country)
# Outcome: sbti_commitment
# Controls and fixed effects will be added

cat("\n--- First Stage Regression ---\n")
cat("Regressing CDP membership on both instruments...\n\n")

# First stage: cdp_sc_member ~ instruments + controls + fixed effects
first_stage <- felm(
  cdp_sc_member ~ peer_cdp_share_lag + peer_cdp_share_country_lag +
    log(at) + leverage + roa + log_sales_growth + capex_ratio +
    log(firm_age) + log(1 + analyst_coverage) + institutional_ownership |
    gvkey + year + sic_2 | 0 | gvkey,
  data = analysis_data
)

summary(first_stage)

# ==============================================================================
# Instrument Strength Test: Joint F-test
# ==============================================================================

cat("\n=== Testing Instrument Strength (Joint F-test) ===\n")

# Extract coefficients and variance-covariance matrix for instruments
beta_instruments <- coef(first_stage)[c("peer_cdp_share_lag", "peer_cdp_share_country_lag")]
vcov_instruments <- vcov(first_stage)[c("peer_cdp_share_lag", "peer_cdp_share_country_lag"),
                                      c("peer_cdp_share_lag", "peer_cdp_share_country_lag")]

# Compute Wald F-statistic: β' * V^(-1) * β / k
# where k = number of instruments (2)
wald_stat <- as.numeric(t(beta_instruments) %*% solve(vcov_instruments) %*% beta_instruments)
f_stat <- wald_stat / 2  # Divide by number of instruments

cat("\nJoint F-statistic for instruments:", round(f_stat, 2), "\n")
cat("Stock-Yogo critical value (10% maximal IV size, 2 instruments):", 19.93, "\n")

if (f_stat > 10) {
  cat("✓ PASS: F >", round(f_stat, 2), "- Instruments are sufficiently strong.\n")
} else {
  cat("✗ WARNING: F =", round(f_stat, 2), "< 10 - Weak instrument concern.\n")
}

# ==============================================================================
# Second Stage Regression
# ==============================================================================

cat("\n--- Second Stage Regression ---\n")
cat("Estimating causal effect of CDP membership on SBTi commitment...\n\n")

# Second stage using felm (2SLS with fixed effects)
second_stage <- felm(
  sbti_commitment ~ log(at) + leverage + roa + log_sales_growth + capex_ratio +
    log(firm_age) + log(1 + analyst_coverage) + institutional_ownership |
    gvkey + year + sic_2 |
    (cdp_sc_member ~ peer_cdp_share_lag + peer_cdp_share_country_lag) |
    gvkey,
  data = analysis_data
)

summary(second_stage)

# ==============================================================================
# Overidentification Test: Sargan-Hansen J-test
# ==============================================================================

cat("\n=== Testing Overidentifying Restrictions (Sargan-Hansen J-test) ===\n")

# For J-test, we need to use ivreg from AER package
# Create formula for ivreg
iv_formula <- sbti_commitment ~ log(at) + leverage + roa + log_sales_growth +
  capex_ratio + log(firm_age) + log(1 + analyst_coverage) +
  institutional_ownership + factor(gvkey) + factor(year) + factor(sic_2) |
  log(at) + leverage + roa + log_sales_growth + capex_ratio +
  log(firm_age) + log(1 + analyst_coverage) + institutional_ownership +
  factor(gvkey) + factor(year) + factor(sic_2) +
  peer_cdp_share_lag + peer_cdp_share_country_lag

# Fit IV model
iv_model <- ivreg(iv_formula, data = analysis_data)

# Compute Sargan-Hansen J-test
# This tests whether instruments are uncorrelated with the error term
residuals_iv <- residuals(iv_model)

# Auxiliary regression: residuals on all instruments and controls
aux_formula <- residuals_iv ~ peer_cdp_share_lag + peer_cdp_share_country_lag +
  log(at) + leverage + roa + log_sales_growth + capex_ratio +
  log(firm_age) + log(1 + analyst_coverage) + institutional_ownership +
  factor(gvkey) + factor(year) + factor(sic_2)

aux_reg <- lm(aux_formula, data = analysis_data)
r_squared <- summary(aux_reg)$r.squared
n <- nobs(aux_reg)
j_stat <- n * r_squared

# J-statistic follows chi-squared distribution with (# instruments - # endogenous) df
df_j <- 2 - 1  # 2 instruments, 1 endogenous variable
p_value <- 1 - pchisq(j_stat, df = df_j)

cat("\nSargan-Hansen J-statistic:", round(j_stat, 3), "\n")
cat("Degrees of freedom:", df_j, "\n")
cat("P-value:", round(p_value, 4), "\n")

if (p_value > 0.05) {
  cat("✓ PASS: Cannot reject null - Instruments appear valid.\n")
} else {
  cat("✗ WARNING: Reject null (p < 0.05) - Potential instrument validity concern.\n")
}

# ==============================================================================
# Export Results to Tables
# ==============================================================================

cat("\n=== Exporting Results to Markdown Tables ===\n")

# Create tables directory if it doesn't exist
if (!dir.exists("tables")) {
  dir.create("tables")
}

# Helper function to create markdown table
create_md_table <- function(df, filename, title) {
  # Add header
  output <- paste0("# ", title, "\n\n")

  # Create table header
  output <- paste0(output, "| ", paste(names(df), collapse = " | "), " |\n")
  output <- paste0(output, "| ", paste(rep("---", ncol(df)), collapse = " | "), " |\n")

  # Add rows
  for (i in 1:nrow(df)) {
    output <- paste0(output, "| ", paste(df[i,], collapse = " | "), " |\n")
  }

  writeLines(output, filename)
  cat("Saved:", filename, "\n")
}

# 1. First Stage Results
first_stage_results <- data.frame(
  Variable = c("Industry Peer Share (lag)", "Country Peer Share (lag)",
               "log(Assets)", "Leverage", "ROA", "Sales Growth",
               "CAPEX Ratio", "log(Firm Age)", "log(Analyst Coverage)",
               "Institutional Ownership"),
  Coefficient = round(coef(first_stage)[1:10], 4),
  Std_Error = round(summary(first_stage)$coefficients[1:10, 2], 4),
  t_value = round(summary(first_stage)$coefficients[1:10, 3], 2),
  p_value = format.pval(summary(first_stage)$coefficients[1:10, 4], digits = 3)
)

create_md_table(
  first_stage_results,
  "tables/first_stage_results.md",
  "First Stage: CDP Membership Regression"
)

# 2. Instrument Diagnostics
diagnostics <- data.frame(
  Test = c("Joint F-statistic", "Stock-Yogo Critical Value (10%)",
           "Sargan-Hansen J-statistic", "J-test p-value"),
  Value = c(round(f_stat, 2), 19.93, round(j_stat, 3), round(p_value, 4)),
  Interpretation = c(
    ifelse(f_stat > 10, "Strong instruments", "Weak instruments"),
    "Threshold for 10% maximal IV size",
    ifelse(p_value > 0.05, "Instruments valid", "Validity concern"),
    ifelse(p_value > 0.05, "Pass (p > 0.05)", "Fail (p < 0.05)")
  )
)

create_md_table(
  diagnostics,
  "tables/instrument_diagnostics.md",
  "Instrument Diagnostic Tests"
)

# 3. Second Stage Results
second_stage_results <- data.frame(
  Variable = c("CDP Membership (instrumented)", "log(Assets)", "Leverage",
               "ROA", "Sales Growth", "CAPEX Ratio", "log(Firm Age)",
               "log(Analyst Coverage)", "Institutional Ownership"),
  Coefficient = round(coef(second_stage)[1:9], 4),
  Std_Error = round(summary(second_stage)$coefficients[1:9, 2], 4),
  t_value = round(summary(second_stage)$coefficients[1:9, 3], 2),
  p_value = format.pval(summary(second_stage)$coefficients[1:9, 4], digits = 3)
)

create_md_table(
  second_stage_results,
  "tables/second_stage_results.md",
  "Second Stage: SBTi Commitment (2SLS)"
)

cat("\n=== Analysis Complete! ===\n")
cat("Results saved to tables/ directory:\n")
cat("  - first_stage_results.md\n")
cat("  - instrument_diagnostics.md\n")
cat("  - second_stage_results.md\n")
