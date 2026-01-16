# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script: 01_create_instrument_country.R
# Purpose: Generate complete_data_2022_instrument_country.rds with country-level
#          peer share instruments
#
# Input:
#   - panel_extended_instrument.rds (population-level with industry instruments)
#   - complete_data_2022.rds (complete cases for filtering)
#
# Output:
#   - complete_data_2022_instrument_country.rds (complete cases + all instruments)
#
# Description:
#   This script creates country-level peer share instruments to complement the
#   existing industry-level instruments. Country-level instruments provide
#   geographic variation in CDP Supply Chain adoption that may be useful for
#   identification and robustness checks.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load required packages ####
library(tidyverse)
library(data.table)

# Set working directory to cleaned_data folder ####
# Note: Run this script from the project root directory (essay3/)
if (!dir.exists("cleaned_data")) {
  stop("Error: cleaned_data directory not found. Please run this script from the project root.")
}
setwd("cleaned_data")

# Load data ####
cat("Loading data files...\n")

# Load population-level data with industry instruments
panel_extended_instrument <- readRDS("panel_extended_instrument.rds")
cat("  - Loaded panel_extended_instrument.rds:",
    nrow(panel_extended_instrument), "observations\n")

# Load complete cases data for filtering
complete_data_2022 <- readRDS("complete_data_2022.rds")
cat("  - Loaded complete_data_2022.rds:",
    nrow(complete_data_2022), "observations\n")

# Convert to data.table for efficient computation ####
setDT(panel_extended_instrument)

# Create country-level peer share instruments ####
cat("\nCreating country-level peer share instruments...\n")

# (a) Country-year peer share EXCLUDING focal firm
panel_extended_instrument[, peer_cdp_share_country :=
  ifelse(.N == 1,                      # single-firm country-year
         NA_real_,
         (sum(cdp_sc_member, na.rm = TRUE) - cdp_sc_member) / (.N - 1)),
  by = .(headquarter_country, year)]

# (b) Lag by one year for each firm
panel_extended_instrument[, peer_cdp_share_country_lag :=
                            shift(peer_cdp_share_country, n = 1, type = "lag"),
                          by = gvkey]

# Replace NA from first year or single-firm groups with 0
panel_extended_instrument[is.na(peer_cdp_share_country_lag),
                          peer_cdp_share_country_lag := 0]

# Sanity checks ####
cat("\nSummary statistics for new instruments:\n")
cat("\npeer_cdp_share_country (current year):\n")
print(summary(panel_extended_instrument$peer_cdp_share_country))

cat("\npeer_cdp_share_country_lag (one-year lag):\n")
print(summary(panel_extended_instrument$peer_cdp_share_country_lag))

# Check correlation between industry and country instruments
cat("\nCorrelation between industry and country instruments:\n")
cor_matrix <- panel_extended_instrument[
  !is.na(peer_cdp_share_lag) & !is.na(peer_cdp_share_country_lag),
  cor(cbind(peer_cdp_share_lag, peer_cdp_share_country_lag))
]
print(cor_matrix)

# Filter to complete cases only ####
cat("\nFiltering to complete cases...\n")

# Create unique identifier for merging
complete_data_2022$merge_id <- paste(complete_data_2022$gvkey,
                                     complete_data_2022$year,
                                     sep = "_")
panel_extended_instrument$merge_id <- paste(panel_extended_instrument$gvkey,
                                            panel_extended_instrument$year,
                                            sep = "_")

# Filter panel_extended_instrument to observations in complete_data_2022
complete_data_2022_instrument_country <- panel_extended_instrument %>%
  filter(merge_id %in% complete_data_2022$merge_id) %>%
  select(-merge_id)  # Remove temporary merge ID

cat("  - Retained", nrow(complete_data_2022_instrument_country),
    "observations after filtering\n")

# Verify we have the expected number of observations
if (nrow(complete_data_2022_instrument_country) != nrow(complete_data_2022)) {
  warning("Number of observations doesn't match complete_data_2022! Expected: ",
          nrow(complete_data_2022), " Got: ",
          nrow(complete_data_2022_instrument_country))
} else {
  cat("  ✓ Observation count matches complete_data_2022\n")
}

# Verify all instruments are present ####
cat("\nVerifying instrument variables:\n")
required_instruments <- c("peer_cdp_share",
                          "peer_cdp_share_lag",
                          "peer_cdp_share_country",
                          "peer_cdp_share_country_lag")

for (var in required_instruments) {
  if (var %in% names(complete_data_2022_instrument_country)) {
    cat("  ✓", var, "present\n")
  } else {
    stop("ERROR: Missing required variable: ", var)
  }
}

# Final summary statistics ####
cat("\nFinal dataset summary:\n")
cat("  - Total observations:", nrow(complete_data_2022_instrument_country), "\n")
cat("  - Unique firms:",
    length(unique(complete_data_2022_instrument_country$gvkey)), "\n")
cat("  - Year range:",
    min(complete_data_2022_instrument_country$year), "-",
    max(complete_data_2022_instrument_country$year), "\n")
cat("  - Countries:",
    length(unique(complete_data_2022_instrument_country$headquarter_country)), "\n")
cat("  - Industries (4-digit GICS):",
    length(unique(complete_data_2022_instrument_country$FourDigitName)), "\n")

# Cross-tabulation of instruments by year ####
cat("\nInstrument coverage by year:\n")
yearly_stats <- complete_data_2022_instrument_country %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    mean_peer_share_industry = mean(peer_cdp_share_lag, na.rm = TRUE),
    mean_peer_share_country = mean(peer_cdp_share_country_lag, na.rm = TRUE),
    .groups = 'drop'
  )
print(yearly_stats, n = Inf)

# Save output ####
cat("\nSaving complete_data_2022_instrument_country.rds...\n")
saveRDS(complete_data_2022_instrument_country,
        "complete_data_2022_instrument_country.rds")

cat("\n✓ Script completed successfully!\n")
cat("\nOutput file: complete_data_2022_instrument_country.rds\n")
cat("Location: /home/user/essay3/cleaned_data/\n")
