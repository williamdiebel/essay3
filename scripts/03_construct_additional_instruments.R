# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script: 03_construct_additional_instruments.R
# Purpose: Construct additional instruments to resolve strength-validity trade-off
#
# Background:
#   Current instruments face a dilemma:
#   - Country IV alone: Strong (F=21.2) but can't test validity
#   - Industry + Country IVs: Strong (F=23.2) but invalid (Sargan p<0.001)
#   - Cross-domain IVs: Valid (Sargan p=0.19) but weak (F=3.5)
#
# Goal:
#   Construct 2-3 additional instruments that, when combined with country-level
#   peer share, produce an over-identified specification that is:
#   1. Strong (F > 10)
#   2. Valid (passes Sargan-Hansen test)
#
# Strategy:
#   This script implements instrument construction for data likely available
#   in your dataset based on data sources (Bloomberg, Compustat, FactSet, RepRisk)
#
# Instructions:
#   1. Review the "Data Requirements" section below
#   2. Uncomment the sections for which you have data
#   3. Comment out sections for unavailable data
#   4. Run the script to generate instruments
#   5. Update and run script 02 to test new instrument combinations
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load required packages ####
library(tidyverse)
library(data.table)

# Set working directory ####
if (!dir.exists("cleaned_data")) {
  stop("Error: cleaned_data directory not found. Please run this script from the project root.")
}
setwd("cleaned_data")

# Load base dataset ####
cat("Loading complete_data_2022_instrument_country.rds...\\n")
data <- readRDS("complete_data_2022_instrument_country.rds")
setDT(data)
cat("  - Loaded", nrow(data), "observations\\n")

# Create backup of original data
data_original <- copy(data)

################################################################################
# INSTRUMENT CATEGORY 1: Historical/Temporal Instruments
################################################################################
# These instruments use lagged firm characteristics as predictors of CDP
# membership. The exclusion restriction relies on controlling for current
# period values.
#
# Required data: Historical values (t-2, t-3) of environmental disclosure,
#                emissions, or other firm characteristics
################################################################################

cat("\\n=== CONSTRUCTING TEMPORAL INSTRUMENTS ===\\n")

# Check if we have sufficient historical data ####
# We need at least 2-3 years of history for each firm
years_per_firm <- data[, .(n_years = .N), by = gvkey]
cat("  - Median years per firm:", median(years_per_firm$n_years), "\\n")
cat("  - Firms with 3+ years:", sum(years_per_firm$n_years >= 3), "\\n")

# Temporal Instrument 1: Environmental disclosure (t-2) ####
# Logic: Past disclosure predicts CDP membership but is predetermined relative
#        to current SBTi decision (conditional on current disclosure)
cat("\\nConstructing: e_disc_lag2 (environmental disclosure, t-2)...\\n")

data[, e_disc_lag2 := shift(e_disc_coalesced_zeros, n = 2, type = "lag"),
     by = gvkey]

# Replace NAs with 0 for firms without 2-year history
data[is.na(e_disc_lag2), e_disc_lag2 := 0]

cat("  - Summary statistics:\\n")
print(summary(data$e_disc_lag2))

# Temporal Instrument 2: Change in disclosure (early period t-3 to t-1) ####
# Logic: Firms that improved disclosure early are more likely to join CDP SC
#        Early improvement trajectory is predetermined
cat("\\nConstructing: delta_e_disc_early (disclosure change, t-3 to t-1)...\\n")

data[, e_disc_lag1 := shift(e_disc_coalesced_zeros, n = 1, type = "lag"),
     by = gvkey]
data[, e_disc_lag3 := shift(e_disc_coalesced_zeros, n = 3, type = "lag"),
     by = gvkey]

data[, delta_e_disc_early := e_disc_lag1 - e_disc_lag3]

# Replace NAs with 0
data[is.na(delta_e_disc_early), delta_e_disc_early := 0]

cat("  - Summary statistics:\\n")
print(summary(data$delta_e_disc_early))

# Temporal Instrument 3: Historical emissions trajectory (t-3) ####
# Logic: Past emissions predict future CDP membership (path dependence)
#        Predetermined relative to current SBTi decision
cat("\\nConstructing: log_scope1_lag3 (emissions, t-3)...\\n")

data[, scope1_lag3 := shift(scope1_zeros, n = 3, type = "lag"),
     by = gvkey]

data[, log_scope1_lag3 := log(scope1_lag3 + 1)]

# Replace NAs with 0
data[is.na(log_scope1_lag3), log_scope1_lag3 := 0]

cat("  - Summary statistics:\\n")
print(summary(data$log_scope1_lag3))

################################################################################
# INSTRUMENT CATEGORY 2: Industry Dynamics Instruments
################################################################################
# These instruments use industry-level aggregates (excluding own firm) as
# predictors. The exclusion restriction relies on industry-level shocks not
# directly affecting individual firm SBTi decisions.
#
# Required data: Industry classifications (already have FourDigitName),
#                firm-level incident/emissions data (already have)
################################################################################

cat("\\n=== CONSTRUCTING INDUSTRY DYNAMICS INSTRUMENTS ===\\n")

# Industry Instrument 1: Industry environmental incidents (excl. own firm) ####
# Logic: Industry-level incidents create pressure for all firms to join
#        voluntary programs (legitimacy seeking)
# Exclusion: Other firms' incidents don't directly cause focal firm's SBTi
#            commitment (only through CDP membership)
cat("\\nConstructing: industry_incidents_excl_lag (industry incidents excl. own, t-1)...\\n")

# Calculate industry-year total incidents
data[, industry_year_incidents := sum(esc_incidents_highreach, na.rm = TRUE),
     by = .(FourDigitName, year)]

# Subtract own firm incidents
data[, industry_incidents_excl := industry_year_incidents - esc_incidents_highreach]

# Lag by one year
data[, industry_incidents_excl_lag := shift(industry_incidents_excl, n = 1, type = "lag"),
     by = gvkey]

# Replace NAs with 0
data[is.na(industry_incidents_excl_lag), industry_incidents_excl_lag := 0]

cat("  - Summary statistics:\\n")
print(summary(data$industry_incidents_excl_lag))

# Industry Instrument 2: Industry average disclosure (excl. own firm) ####
# Logic: Industry disclosure norms predict individual firm CDP membership
# Exclusion: Industry average disclosure (excluding own firm) doesn't directly
#            affect own SBTi decision conditional on own disclosure
cat("\\nConstructing: industry_avg_disc_excl_lag (industry avg disclosure excl. own, t-1)...\\n")

# Calculate industry-year average disclosure
data[, industry_year_total_disc := sum(e_disc_coalesced_zeros, na.rm = TRUE),
     by = .(FourDigitName, year)]
data[, industry_year_n := .N,
     by = .(FourDigitName, year)]

# Calculate average excluding own firm
data[, industry_avg_disc_excl :=
       (industry_year_total_disc - e_disc_coalesced_zeros) / (industry_year_n - 1)]

# Lag by one year
data[, industry_avg_disc_excl_lag := shift(industry_avg_disc_excl, n = 1, type = "lag"),
     by = gvkey]

# Replace NAs with 0
data[is.na(industry_avg_disc_excl_lag), industry_avg_disc_excl_lag := 0]

cat("  - Summary statistics:\\n")
print(summary(data$industry_avg_disc_excl_lag))

# Industry Instrument 3: Industry emissions intensity (excl. own firm) ####
# Logic: Emissions-intensive industries face more pressure to join voluntary
#        climate programs
# Exclusion: Industry average emissions (excl. own) affects program membership
#            but not individual climate target setting
cat("\\nConstructing: industry_avg_emissions_excl_lag (industry avg emissions excl. own, t-1)...\\n")

# Calculate industry-year average emissions
data[, industry_year_total_emissions := sum(scope1_zeros, na.rm = TRUE),
     by = .(FourDigitName, year)]

# Calculate average excluding own firm
data[, industry_avg_emissions_excl :=
       (industry_year_total_emissions - scope1_zeros) / (industry_year_n - 1)]

# Log transformation
data[, industry_avg_emissions_excl_log := log(industry_avg_emissions_excl + 1)]

# Lag by one year
data[, industry_avg_emissions_excl_lag := shift(industry_avg_emissions_excl_log, n = 1, type = "lag"),
     by = gvkey]

# Replace NAs with 0
data[is.na(industry_avg_emissions_excl_lag), industry_avg_emissions_excl_lag := 0]

cat("  - Summary statistics:\\n")
print(summary(data$industry_avg_emissions_excl_lag))

################################################################################
# INSTRUMENT CATEGORY 3: Geographic/Regional Instruments
################################################################################
# These instruments use regional peer effects (broader than country but more
# specific than global)
#
# Required data: Country classifications (already have headquarter_country)
################################################################################

cat("\\n=== CONSTRUCTING GEOGRAPHIC INSTRUMENTS ===\\n")

# Define regions based on country ####
# You may need to customize this mapping based on your specific countries
cat("\\nCreating regional classifications...\\n")

# Create a region mapping (customize as needed)
# This is a starter - you should verify/update based on your actual countries
data[, region := case_when(
  headquarter_country %in% c("United States", "Canada", "Mexico", "USA", "US") ~ "North America",
  headquarter_country %in% c("United Kingdom", "Germany", "France", "Italy", "Spain",
                             "Netherlands", "Belgium", "Switzerland", "Sweden", "Norway",
                             "Denmark", "Finland", "Ireland", "Austria", "Portugal",
                             "UK", "Great Britain") ~ "Europe",
  headquarter_country %in% c("Japan", "China", "South Korea", "Taiwan", "Singapore",
                             "Hong Kong", "India", "Malaysia", "Thailand", "Indonesia") ~ "Asia Pacific",
  headquarter_country %in% c("Australia", "New Zealand") ~ "Oceania",
  headquarter_country %in% c("Brazil", "Argentina", "Chile", "Colombia", "Peru") ~ "Latin America",
  TRUE ~ "Other"
)]

cat("  - Region distribution:\\n")
print(table(data$region))

# Regional Instrument 1: Regional peer CDP share (excl. own country) ####
# Logic: Regional diffusion of CDP SC membership affects country-level adoption
# Exclusion: Regional peer effects (excl. own country) operate through country
#            adoption, not directly on firm SBTi decisions
cat("\\nConstructing: regional_peer_cdp_excl_country_lag (regional peers excl. own country, t-1)...\\n")

# Calculate region-year totals
data[, region_year_cdp := sum(cdp_sc_member, na.rm = TRUE),
     by = .(region, year)]
data[, region_year_n := .N,
     by = .(region, year)]

# Calculate country-year totals
data[, country_year_cdp := sum(cdp_sc_member, na.rm = TRUE),
     by = .(headquarter_country, year)]
data[, country_year_n := .N,
     by = .(headquarter_country, year)]

# Regional peer share excluding own country
data[, regional_peer_cdp_excl_country :=
       (region_year_cdp - country_year_cdp - cdp_sc_member) /
       (region_year_n - country_year_n - 1)]

# Handle edge cases (single-country regions, etc.)
data[is.na(regional_peer_cdp_excl_country) |
       is.infinite(regional_peer_cdp_excl_country),
     regional_peer_cdp_excl_country := 0]

# Lag by one year
data[, regional_peer_cdp_excl_country_lag :=
       shift(regional_peer_cdp_excl_country, n = 1, type = "lag"),
     by = gvkey]

# Replace NAs with 0
data[is.na(regional_peer_cdp_excl_country_lag),
     regional_peer_cdp_excl_country_lag := 0]

cat("  - Summary statistics:\\n")
print(summary(data$regional_peer_cdp_excl_country_lag))

################################################################################
# INSTRUMENT CATEGORY 4: Firm Size/Visibility Instruments
################################################################################
# These instruments use firm visibility/size measures that predict CDP
# membership but may be excludable from direct effects on SBTi
#
# Required data: Total assets (already have), potentially market cap
################################################################################

cat("\\n=== CONSTRUCTING FIRM VISIBILITY INSTRUMENTS ===\\n")

# Firm Size Instrument: Lagged firm size (t-2) ####
# Logic: Firm size predicts CDP membership (larger firms face more pressure)
# Exclusion: Past firm size affects current CDP membership but doesn't directly
#            cause current SBTi commitment (conditional on current size)
# NOTE: This is a weaker instrument theoretically, but may add strength
cat("\\nConstructing: log_assets_lag2 (firm size, t-2)...\\n")

data[, at_usd_winsorized_1_lag2 := shift(at_usd_winsorized_1_log, n = 2, type = "lag"),
     by = gvkey]

# Replace NAs with median
data[is.na(at_usd_winsorized_1_lag2),
     at_usd_winsorized_1_lag2 := median(at_usd_winsorized_1_log, na.rm = TRUE)]

cat("  - Summary statistics:\\n")
print(summary(data$at_usd_winsorized_1_lag2))

################################################################################
# SUMMARY AND CORRELATION ANALYSIS
################################################################################

cat("\\n=== INSTRUMENT SUMMARY ===\\n")

# List all newly created instruments
new_instruments <- c(
  # Temporal
  "e_disc_lag2",
  "delta_e_disc_early",
  "log_scope1_lag3",
  # Industry dynamics
  "industry_incidents_excl_lag",
  "industry_avg_disc_excl_lag",
  "industry_avg_emissions_excl_lag",
  # Geographic
  "regional_peer_cdp_excl_country_lag",
  # Firm visibility
  "at_usd_winsorized_1_lag2"
)

# Existing instruments for comparison
existing_instruments <- c(
  "peer_cdp_share_lag",
  "peer_cdp_share_country_lag"
)

# All instruments
all_instruments <- c(existing_instruments, new_instruments)

cat("\\nNew instruments created:\\n")
for (var in new_instruments) {
  cat("  -", var, "\\n")
}

# Correlation matrix for all instruments ####
cat("\\nComputing correlation matrix for all instruments...\\n")

# Get complete cases for correlation
complete_cases <- data[, c("gvkey", "year", all_instruments), with = FALSE]
complete_cases <- complete_cases[complete.cases(complete_cases)]

cat("  - Observations with all instruments:", nrow(complete_cases), "\\n")

# Compute correlation matrix
cor_matrix <- cor(complete_cases[, all_instruments, with = FALSE])

cat("\\nCorrelation matrix (existing + new instruments):\\n")
print(round(cor_matrix, 3))

# Flag highly correlated instruments (> 0.7)
cat("\\nHighly correlated instrument pairs (|r| > 0.7):\\n")
for (i in 1:(length(all_instruments) - 1)) {
  for (j in (i + 1):length(all_instruments)) {
    cor_val <- cor_matrix[i, j]
    if (abs(cor_val) > 0.7) {
      cat("  -", all_instruments[i], "~", all_instruments[j], ": r =",
          round(cor_val, 3), "\\n")
    }
  }
}

# Descriptive statistics for all instruments ####
cat("\\nDescriptive statistics for all instruments:\\n")
summary_stats <- complete_cases[, lapply(.SD, function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE))
}), .SDcols = all_instruments]

print(summary_stats)

################################################################################
# SAVE OUTPUT
################################################################################

cat("\\n=== SAVING OUTPUT ===\\n")

# Save dataset with all instruments
cat("\\nSaving complete_data_2022_all_instruments.rds...\\n")
saveRDS(data, "complete_data_2022_all_instruments.rds")

# Save instrument list for reference
instrument_list <- data.frame(
  instrument_name = new_instruments,
  category = c(
    rep("Temporal", 3),
    rep("Industry Dynamics", 3),
    rep("Geographic", 1),
    rep("Firm Visibility", 1)
  ),
  stringsAsFactors = FALSE
)

write.csv(instrument_list, "instrument_list.csv", row.names = FALSE)

cat("\\n✓ Script completed successfully!\\n")
cat("\\nOutput files:\\n")
cat("  - complete_data_2022_all_instruments.rds (", nrow(data), " observations)\\n")
cat("  - instrument_list.csv (", nrow(instrument_list), " new instruments)\\n")

cat("\\nNext steps:\\n")
cat("  1. Review correlation matrix to identify instrument combinations\\n")
cat("  2. Update script 02 to test new instrument specifications\\n")
cat("  3. Run first-stage F-tests for each instrument\\n")
cat("  4. Test over-identified specifications with Sargan-Hansen test\\n")

################################################################################
# RECOMMENDED INSTRUMENT COMBINATIONS FOR TESTING
################################################################################

cat("\\n=== RECOMMENDED INSTRUMENT COMBINATIONS ===\\n")

cat("\\nBased on theoretical considerations, test these combinations:\\n")

cat("\\nCombination 1 (Temporal + Country Peer):\\n")
cat("  - peer_cdp_share_country_lag\\n")
cat("  - e_disc_lag2\\n")
cat("  - delta_e_disc_early\\n")
cat("  Justification: Different mechanisms (peer effects vs. path dependence)\\n")

cat("\\nCombination 2 (Industry Dynamics + Country Peer):\\n")
cat("  - peer_cdp_share_country_lag\\n")
cat("  - industry_incidents_excl_lag\\n")
cat("  - industry_avg_disc_excl_lag\\n")
cat("  Justification: Different levels (country vs. industry) with controls\\n")

cat("\\nCombination 3 (Regional + Temporal):\\n")
cat("  - peer_cdp_share_country_lag\\n")
cat("  - regional_peer_cdp_excl_country_lag\\n")
cat("  - e_disc_lag2\\n")
cat("  Justification: Different geographic scales + historical variation\\n")

cat("\\nCombination 4 (Best of Each Category):\\n")
cat("  - peer_cdp_share_country_lag (existing, strongest)\\n")
cat("  - e_disc_lag2 (temporal, path dependence)\\n")
cat("  - industry_incidents_excl_lag (industry dynamics, external shock)\\n")
cat("  Justification: Conceptually distinct, likely low correlation\\n")

cat("\\n=== END OF SCRIPT ===\\n")
