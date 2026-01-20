# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script: 06_specification_grid.R
# Purpose: Systematic specification search across instruments and methods
#
# Goal: Understand sensitivity of results to:
#   1. Instrument choice (peer_cdp, e_disc_lag2, industry_incidents, combinations)
#   2. Estimation method (2SLS vs control function)
#   3. Incident measures (highreach, highsev, all)
#
# This will help identify which specifications are robust and which are fragile
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load packages ####
library(tidyverse)
library(data.table)
library(fixest)
library(broom)

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
# PANEL CENSORING AT COMMITMENT TIME
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("PANEL CENSORING\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# Identify first commitment year for each firm
data[, first_commit_year := ifelse(any(sbti_commitment_lead1 == 1, na.rm = TRUE),
                                     min(year[sbti_commitment_lead1 == 1], na.rm = TRUE),
                                     NA_integer_),
     by = gvkey]

obs_before <- nrow(data)
data <- data[is.na(first_commit_year) | year <= first_commit_year]
obs_after <- nrow(data)

cat("  Observations dropped:", obs_before - obs_after, "\n")
cat("  Retention rate:", round(obs_after / obs_before * 100, 1), "%\n\n")

data[, first_commit_year := NULL]

################################################################################
# DATA PREPARATION
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("DATA PREPARATION\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# Convert factors
data[, `:=`(
  headquarter_country = as.factor(headquarter_country),
  FourDigitName = as.factor(FourDigitName),
  year = as.factor(year),
  gvkey = as.factor(gvkey)
)]

# Variables needed for analysis
vars_needed <- c("sbti_commitment_lead1", "cdp_sc_member",
                 "peer_cdp_share_country_lag", "e_disc_lag2", "industry_incidents_excl_lag",
                 "esc_incidents_highreach", "esc_incidents_highsev", "esc_incidents",
                 "e_disc_coalesced_zeros", "e_disc_missing",
                 "scope1_zeros", "scope1_missing",
                 "roa_oibdp_w1_at_w1", "at_usd_winsorized_1_log", "tll_lt_w1_at_w1",
                 "headquarter_country", "FourDigitName", "year", "gvkey")

# Filter to complete cases
data_complete <- data[complete.cases(data[, ..vars_needed]), ]

# Remove singletons
data_complete[, fe_group_size := .N, by = .(headquarter_country, year)]
data_complete <- data_complete[fe_group_size > 1]
data_complete[, fe_group_size := NULL]

cat("  Final sample size:", nrow(data_complete), "\n")
cat("  SBTi commitment rate:", round(mean(data_complete$sbti_commitment_lead1) * 100, 2), "%\n")
cat("  Number of commitments:", sum(data_complete$sbti_commitment_lead1), "\n\n")

# Define controls
controls <- c("e_disc_coalesced_zeros", "e_disc_missing",
              "log(scope1_zeros + 1)", "scope1_missing",
              "roa_oibdp_w1_at_w1", "at_usd_winsorized_1_log",
              "tll_lt_w1_at_w1")
controls_formula <- paste(controls, collapse = " + ")

################################################################################
# SPECIFICATION GRID
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("SYSTEMATIC SPECIFICATION GRID\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Testing combinations of:\n")
cat("  - Instruments: peer_cdp, e_disc, industry_inc, pairs, all three\n")
cat("  - Methods: 2SLS, Control Function\n")
cat("  - Incidents: highreach (baseline)\n\n")

# Storage for results
results_grid <- list()
spec_counter <- 1

################################################################################
# INSTRUMENT 1: peer_cdp_share_country_lag ONLY
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("INSTRUMENT 1: peer_cdp_share_country_lag (ONLY)\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# First-stage
fs1 <- feols(
  as.formula(paste("cdp_sc_member ~ peer_cdp_share_country_lag +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

fs1_f <- coeftable(fs1)["peer_cdp_share_country_lag", "t value"]^2
cat("First-stage F-statistic:", round(fs1_f, 2), "\n")

# Reduced form
rf1 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ peer_cdp_share_country_lag + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

rf1_coef <- coef(rf1)["peer_cdp_share_country_lag"]
cat("Reduced form coefficient:", round(rf1_coef, 4), "\n\n")

# Method A: 2SLS
cat("--- Method A: Standard 2SLS ---\n")
tsls1 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(tsls1)["fit_cdp_sc_member"], 4),
    "(", round(se(tsls1)["fit_cdp_sc_member"], 4), "), p =",
    round(pvalue(tsls1)["fit_cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(tsls1)["esc_incidents_highreach"], 4),
    "(", round(se(tsls1)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(tsls1)["esc_incidents_highreach"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "1A",
  instruments = "peer_cdp",
  method = "2SLS",
  fs_f = fs1_f,
  rf_coef = rf1_coef,
  cdp_coef = coef(tsls1)["fit_cdp_sc_member"],
  cdp_se = se(tsls1)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls1)["fit_cdp_sc_member"],
  inc_coef = coef(tsls1)["esc_incidents_highreach"],
  inc_se = se(tsls1)["esc_incidents_highreach"],
  inc_p = pvalue(tsls1)["esc_incidents_highreach"],
  nobs = tsls1$nobs
)
spec_counter <- spec_counter + 1

# Method B: Control Function
cat("--- Method B: Control Function ---\n")
data_complete$fs_resid1 <- residuals(fs1)

cf1 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member + fs_resid1 + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(cf1)["cdp_sc_member"], 4),
    "(", round(se(cf1)["cdp_sc_member"], 4), "), p =",
    round(pvalue(cf1)["cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(cf1)["esc_incidents_highreach"], 4),
    "(", round(se(cf1)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(cf1)["esc_incidents_highreach"], 4), "\n")
cat("Control fn:", round(coef(cf1)["fs_resid1"], 4), ", p =",
    round(pvalue(cf1)["fs_resid1"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "1B",
  instruments = "peer_cdp",
  method = "CF",
  fs_f = fs1_f,
  rf_coef = rf1_coef,
  cdp_coef = coef(cf1)["cdp_sc_member"],
  cdp_se = se(cf1)["cdp_sc_member"],
  cdp_p = pvalue(cf1)["cdp_sc_member"],
  inc_coef = coef(cf1)["esc_incidents_highreach"],
  inc_se = se(cf1)["esc_incidents_highreach"],
  inc_p = pvalue(cf1)["esc_incidents_highreach"],
  cf_p = pvalue(cf1)["fs_resid1"],
  nobs = cf1$nobs
)
spec_counter <- spec_counter + 1

################################################################################
# INSTRUMENT 2: e_disc_lag2 ONLY
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("INSTRUMENT 2: e_disc_lag2 (ONLY)\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# First-stage
fs2 <- feols(
  as.formula(paste("cdp_sc_member ~ e_disc_lag2 +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

fs2_f <- coeftable(fs2)["e_disc_lag2", "t value"]^2
cat("First-stage F-statistic:", round(fs2_f, 2), "\n")

# Reduced form
rf2 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ e_disc_lag2 + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

rf2_coef <- coef(rf2)["e_disc_lag2"]
cat("Reduced form coefficient:", round(rf2_coef, 4), "\n\n")

# Method A: 2SLS
cat("--- Method A: Standard 2SLS ---\n")
tsls2 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ e_disc_lag2")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(tsls2)["fit_cdp_sc_member"], 4),
    "(", round(se(tsls2)["fit_cdp_sc_member"], 4), "), p =",
    round(pvalue(tsls2)["fit_cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(tsls2)["esc_incidents_highreach"], 4),
    "(", round(se(tsls2)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(tsls2)["esc_incidents_highreach"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "2A",
  instruments = "e_disc_lag2",
  method = "2SLS",
  fs_f = fs2_f,
  rf_coef = rf2_coef,
  cdp_coef = coef(tsls2)["fit_cdp_sc_member"],
  cdp_se = se(tsls2)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls2)["fit_cdp_sc_member"],
  inc_coef = coef(tsls2)["esc_incidents_highreach"],
  inc_se = se(tsls2)["esc_incidents_highreach"],
  inc_p = pvalue(tsls2)["esc_incidents_highreach"],
  nobs = tsls2$nobs
)
spec_counter <- spec_counter + 1

# Method B: Control Function
cat("--- Method B: Control Function ---\n")
data_complete$fs_resid2 <- residuals(fs2)

cf2 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member + fs_resid2 + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(cf2)["cdp_sc_member"], 4),
    "(", round(se(cf2)["cdp_sc_member"], 4), "), p =",
    round(pvalue(cf2)["cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(cf2)["esc_incidents_highreach"], 4),
    "(", round(se(cf2)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(cf2)["esc_incidents_highreach"], 4), "\n")
cat("Control fn:", round(coef(cf2)["fs_resid2"], 4), ", p =",
    round(pvalue(cf2)["fs_resid2"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "2B",
  instruments = "e_disc_lag2",
  method = "CF",
  fs_f = fs2_f,
  rf_coef = rf2_coef,
  cdp_coef = coef(cf2)["cdp_sc_member"],
  cdp_se = se(cf2)["cdp_sc_member"],
  cdp_p = pvalue(cf2)["cdp_sc_member"],
  inc_coef = coef(cf2)["esc_incidents_highreach"],
  inc_se = se(cf2)["esc_incidents_highreach"],
  inc_p = pvalue(cf2)["esc_incidents_highreach"],
  cf_p = pvalue(cf2)["fs_resid2"],
  nobs = cf2$nobs
)
spec_counter <- spec_counter + 1

################################################################################
# INSTRUMENT 3: industry_incidents_excl_lag ONLY
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("INSTRUMENT 3: industry_incidents_excl_lag (ONLY)\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# First-stage
fs3 <- feols(
  as.formula(paste("cdp_sc_member ~ industry_incidents_excl_lag +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

fs3_f <- coeftable(fs3)["industry_incidents_excl_lag", "t value"]^2
cat("First-stage F-statistic:", round(fs3_f, 2), "\n")

# Reduced form
rf3 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ industry_incidents_excl_lag + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

rf3_coef <- coef(rf3)["industry_incidents_excl_lag"]
cat("Reduced form coefficient:", round(rf3_coef, 4), "\n\n")

# Method A: 2SLS
cat("--- Method A: Standard 2SLS ---\n")
tsls3 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ industry_incidents_excl_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(tsls3)["fit_cdp_sc_member"], 4),
    "(", round(se(tsls3)["fit_cdp_sc_member"], 4), "), p =",
    round(pvalue(tsls3)["fit_cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(tsls3)["esc_incidents_highreach"], 4),
    "(", round(se(tsls3)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(tsls3)["esc_incidents_highreach"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "3A",
  instruments = "industry_inc",
  method = "2SLS",
  fs_f = fs3_f,
  rf_coef = rf3_coef,
  cdp_coef = coef(tsls3)["fit_cdp_sc_member"],
  cdp_se = se(tsls3)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls3)["fit_cdp_sc_member"],
  inc_coef = coef(tsls3)["esc_incidents_highreach"],
  inc_se = se(tsls3)["esc_incidents_highreach"],
  inc_p = pvalue(tsls3)["esc_incidents_highreach"],
  nobs = tsls3$nobs
)
spec_counter <- spec_counter + 1

# Method B: Control Function
cat("--- Method B: Control Function ---\n")
data_complete$fs_resid3 <- residuals(fs3)

cf3 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member + fs_resid3 + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(cf3)["cdp_sc_member"], 4),
    "(", round(se(cf3)["cdp_sc_member"], 4), "), p =",
    round(pvalue(cf3)["cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(cf3)["esc_incidents_highreach"], 4),
    "(", round(se(cf3)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(cf3)["esc_incidents_highreach"], 4), "\n")
cat("Control fn:", round(coef(cf3)["fs_resid3"], 4), ", p =",
    round(pvalue(cf3)["fs_resid3"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "3B",
  instruments = "industry_inc",
  method = "CF",
  fs_f = fs3_f,
  rf_coef = rf3_coef,
  cdp_coef = coef(cf3)["cdp_sc_member"],
  cdp_se = se(cf3)["cdp_sc_member"],
  cdp_p = pvalue(cf3)["cdp_sc_member"],
  inc_coef = coef(cf3)["esc_incidents_highreach"],
  inc_se = se(cf3)["esc_incidents_highreach"],
  inc_p = pvalue(cf3)["esc_incidents_highreach"],
  cf_p = pvalue(cf3)["fs_resid3"],
  nobs = cf3$nobs
)
spec_counter <- spec_counter + 1

################################################################################
# INSTRUMENT 4: peer_cdp + e_disc_lag2 (TWO INSTRUMENTS)
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("INSTRUMENT 4: peer_cdp + e_disc_lag2 (TWO)\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# First-stage
fs4 <- feols(
  as.formula(paste("cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2 +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

# Joint F-test approximation (use max of individual t^2)
fs4_f <- max(coeftable(fs4)["peer_cdp_share_country_lag", "t value"]^2,
             coeftable(fs4)["e_disc_lag2", "t value"]^2)
cat("First-stage F-statistic (max):", round(fs4_f, 2), "\n")

# Reduced form
rf4 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ peer_cdp_share_country_lag + e_disc_lag2 + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Reduced form (peer_cdp):", round(coef(rf4)["peer_cdp_share_country_lag"], 4), "\n")
cat("Reduced form (e_disc):", round(coef(rf4)["e_disc_lag2"], 4), "\n\n")

# Method A: 2SLS
cat("--- Method A: Standard 2SLS ---\n")
tsls4 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(tsls4)["fit_cdp_sc_member"], 4),
    "(", round(se(tsls4)["fit_cdp_sc_member"], 4), "), p =",
    round(pvalue(tsls4)["fit_cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(tsls4)["esc_incidents_highreach"], 4),
    "(", round(se(tsls4)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(tsls4)["esc_incidents_highreach"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "4A",
  instruments = "peer_cdp + e_disc",
  method = "2SLS",
  fs_f = fs4_f,
  rf_coef = coef(rf4)["peer_cdp_share_country_lag"],
  cdp_coef = coef(tsls4)["fit_cdp_sc_member"],
  cdp_se = se(tsls4)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls4)["fit_cdp_sc_member"],
  inc_coef = coef(tsls4)["esc_incidents_highreach"],
  inc_se = se(tsls4)["esc_incidents_highreach"],
  inc_p = pvalue(tsls4)["esc_incidents_highreach"],
  nobs = tsls4$nobs
)
spec_counter <- spec_counter + 1

# Method B: Control Function
cat("--- Method B: Control Function ---\n")
data_complete$fs_resid4 <- residuals(fs4)

cf4 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member + fs_resid4 + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(cf4)["cdp_sc_member"], 4),
    "(", round(se(cf4)["cdp_sc_member"], 4), "), p =",
    round(pvalue(cf4)["cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(cf4)["esc_incidents_highreach"], 4),
    "(", round(se(cf4)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(cf4)["esc_incidents_highreach"], 4), "\n")
cat("Control fn:", round(coef(cf4)["fs_resid4"], 4), ", p =",
    round(pvalue(cf4)["fs_resid4"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "4B",
  instruments = "peer_cdp + e_disc",
  method = "CF",
  fs_f = fs4_f,
  rf_coef = coef(rf4)["peer_cdp_share_country_lag"],
  cdp_coef = coef(cf4)["cdp_sc_member"],
  cdp_se = se(cf4)["cdp_sc_member"],
  cdp_p = pvalue(cf4)["cdp_sc_member"],
  inc_coef = coef(cf4)["esc_incidents_highreach"],
  inc_se = se(cf4)["esc_incidents_highreach"],
  inc_p = pvalue(cf4)["esc_incidents_highreach"],
  cf_p = pvalue(cf4)["fs_resid4"],
  nobs = cf4$nobs
)
spec_counter <- spec_counter + 1

################################################################################
# INSTRUMENT 5: ALL THREE INSTRUMENTS
################################################################################

cat("=" %+% strrep("=", 79) %+% "\n")
cat("INSTRUMENT 5: peer_cdp + e_disc + industry_inc (THREE)\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")

# First-stage
fs5 <- feols(
  as.formula(paste("cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2 + industry_incidents_excl_lag +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

fs5_f <- max(coeftable(fs5)["peer_cdp_share_country_lag", "t value"]^2,
             coeftable(fs5)["e_disc_lag2", "t value"]^2,
             coeftable(fs5)["industry_incidents_excl_lag", "t value"]^2)
cat("First-stage F-statistic (max):", round(fs5_f, 2), "\n")

# Reduced form
rf5 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ peer_cdp_share_country_lag + e_disc_lag2 + industry_incidents_excl_lag + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("Reduced form (peer_cdp):", round(coef(rf5)["peer_cdp_share_country_lag"], 4), "\n")
cat("Reduced form (e_disc):", round(coef(rf5)["e_disc_lag2"], 4), "\n")
cat("Reduced form (industry_inc):", round(coef(rf5)["industry_incidents_excl_lag"], 4), "\n\n")

# Method A: 2SLS
cat("--- Method A: Standard 2SLS ---\n")
tsls5 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ esc_incidents_highreach +",
                   controls_formula,
                   "| headquarter_country + year",
                   "| cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2 + industry_incidents_excl_lag")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(tsls5)["fit_cdp_sc_member"], 4),
    "(", round(se(tsls5)["fit_cdp_sc_member"], 4), "), p =",
    round(pvalue(tsls5)["fit_cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(tsls5)["esc_incidents_highreach"], 4),
    "(", round(se(tsls5)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(tsls5)["esc_incidents_highreach"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "5A",
  instruments = "all_three",
  method = "2SLS",
  fs_f = fs5_f,
  rf_coef = coef(rf5)["peer_cdp_share_country_lag"],
  cdp_coef = coef(tsls5)["fit_cdp_sc_member"],
  cdp_se = se(tsls5)["fit_cdp_sc_member"],
  cdp_p = pvalue(tsls5)["fit_cdp_sc_member"],
  inc_coef = coef(tsls5)["esc_incidents_highreach"],
  inc_se = se(tsls5)["esc_incidents_highreach"],
  inc_p = pvalue(tsls5)["esc_incidents_highreach"],
  nobs = tsls5$nobs
)
spec_counter <- spec_counter + 1

# Method B: Control Function
cat("--- Method B: Control Function ---\n")
data_complete$fs_resid5 <- residuals(fs5)

cf5 <- feols(
  as.formula(paste("sbti_commitment_lead1 ~ cdp_sc_member + fs_resid5 + esc_incidents_highreach +",
                   controls_formula, "| headquarter_country + year")),
  cluster = ~ gvkey,
  data = data_complete
)

cat("CDP SC:", round(coef(cf5)["cdp_sc_member"], 4),
    "(", round(se(cf5)["cdp_sc_member"], 4), "), p =",
    round(pvalue(cf5)["cdp_sc_member"], 4), "\n")
cat("Incidents:", round(coef(cf5)["esc_incidents_highreach"], 4),
    "(", round(se(cf5)["esc_incidents_highreach"], 4), "), p =",
    round(pvalue(cf5)["esc_incidents_highreach"], 4), "\n")
cat("Control fn:", round(coef(cf5)["fs_resid5"], 4), ", p =",
    round(pvalue(cf5)["fs_resid5"], 4), "\n\n")

results_grid[[spec_counter]] <- list(
  spec = "5B",
  instruments = "all_three",
  method = "CF",
  fs_f = fs5_f,
  rf_coef = coef(rf5)["peer_cdp_share_country_lag"],
  cdp_coef = coef(cf5)["cdp_sc_member"],
  cdp_se = se(cf5)["cdp_sc_member"],
  cdp_p = pvalue(cf5)["cdp_sc_member"],
  inc_coef = coef(cf5)["esc_incidents_highreach"],
  inc_se = se(cf5)["esc_incidents_highreach"],
  inc_p = pvalue(cf5)["esc_incidents_highreach"],
  cf_p = pvalue(cf5)["fs_resid5"],
  nobs = cf5$nobs
)
spec_counter <- spec_counter + 1

################################################################################
# COMPARISON TABLE
################################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("SPECIFICATION COMPARISON TABLE\n")
cat(strrep("=", 80) %+% "\n\n")

# Convert to data frame
results_df <- bind_rows(results_grid)

# Print formatted table
cat(sprintf("%-6s %-20s %-6s %8s %8s %10s %10s %10s %10s\n",
            "Spec", "Instruments", "Method", "F-stat", "RF_coef",
            "CDP_coef", "CDP_p", "Inc_coef", "Inc_p"))
cat(strrep("-", 100), "\n")

for (i in 1:nrow(results_df)) {
  row <- results_df[i, ]
  cat(sprintf("%-6s %-20s %-6s %8.2f %8.4f %10.4f %10.4f %10.4f %10.4f\n",
              row$spec, row$instruments, row$method,
              row$fs_f, row$rf_coef,
              row$cdp_coef, row$cdp_p,
              row$inc_coef, row$inc_p))
}

cat("\n")

# Summary statistics
cat("Summary:\n\n")

cat("CDP SC Coefficient:\n")
cat("  Range:", round(min(results_df$cdp_coef, na.rm = TRUE), 4), "to",
    round(max(results_df$cdp_coef, na.rm = TRUE), 4), "\n")
cat("  Mean:", round(mean(results_df$cdp_coef, na.rm = TRUE), 4), "\n")
cat("  Median:", round(median(results_df$cdp_coef, na.rm = TRUE), 4), "\n")
cat("  Significant (p<0.05):", sum(results_df$cdp_p < 0.05, na.rm = TRUE), "of", nrow(results_df), "\n\n")

cat("Incidents Coefficient:\n")
cat("  Range:", round(min(results_df$inc_coef, na.rm = TRUE), 4), "to",
    round(max(results_df$inc_coef, na.rm = TRUE), 4), "\n")
cat("  Mean:", round(mean(results_df$inc_coef, na.rm = TRUE), 4), "\n")
cat("  Median:", round(median(results_df$inc_coef, na.rm = TRUE), 4), "\n")
cat("  Significant (p<0.05):", sum(results_df$inc_p < 0.05, na.rm = TRUE), "of", nrow(results_df), "\n\n")

cat("First-Stage Strength:\n")
cat("  Weak instruments (F<10):", sum(results_df$fs_f < 10, na.rm = TRUE), "of", nrow(results_df), "\n")
cat("  Strong instruments (F>10):", sum(results_df$fs_f >= 10, na.rm = TRUE), "of", nrow(results_df), "\n\n")

# Save results
write.csv(results_df, "results_specification_grid.csv", row.names = FALSE)
cat("Results saved to: results_specification_grid.csv\n\n")

cat("=" %+% strrep("=", 79) %+% "\n")
cat("ANALYSIS COMPLETE\n")
cat("=" %+% strrep("=", 79) %+% "\n\n")
