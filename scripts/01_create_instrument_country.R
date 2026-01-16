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

# Create cross-domain instruments for robustness ####
cat("\nCreating cross-domain instruments (industry excl. own country, country excl. own industry)...\n")

# (c) Industry peer share EXCLUDING own country
# For firm i in industry j and country c, calculate peer share in industry j
# across all countries EXCEPT country c
panel_extended_instrument[, peer_cdp_share_industry_exclcountry :=
  {
    # For each industry-year, sum CDP members and firms excluding own country
    industry_year_totals <- .SD[, .(
      total_cdp = sum(cdp_sc_member, na.rm = TRUE),
      total_firms = .N
    ), by = .(FourDigitName, year)]

    # For each country-industry-year, sum CDP members and firms in own country
    country_industry_year_totals <- .SD[, .(
      country_cdp = sum(cdp_sc_member, na.rm = TRUE),
      country_firms = .N
    ), by = .(headquarter_country, FourDigitName, year)]

    # Merge to get both totals
    merged <- merge(.SD, industry_year_totals,
                   by = c("FourDigitName", "year"), all.x = TRUE)
    merged <- merge(merged, country_industry_year_totals,
                   by = c("headquarter_country", "FourDigitName", "year"), all.x = TRUE)

    # Calculate: (industry total - country total - self) / (industry firms - country firms - 1)
    # Exclude self from numerator and denominator
    other_country_cdp <- merged$total_cdp - merged$country_cdp - merged$cdp_sc_member
    other_country_firms <- merged$total_firms - merged$country_firms - 1

    # Return peer share (handle edge cases)
    ifelse(other_country_firms > 0,
           other_country_cdp / other_country_firms,
           NA_real_)
  }]

# (d) Country peer share EXCLUDING own industry
# For firm i in country c and industry j, calculate peer share in country c
# across all industries EXCEPT industry j
panel_extended_instrument[, peer_cdp_share_country_excludindustry :=
  {
    # For each country-year, sum CDP members and firms excluding own industry
    country_year_totals <- .SD[, .(
      total_cdp = sum(cdp_sc_member, na.rm = TRUE),
      total_firms = .N
    ), by = .(headquarter_country, year)]

    # For each country-industry-year, sum CDP members and firms in own industry
    country_industry_year_totals <- .SD[, .(
      industry_cdp = sum(cdp_sc_member, na.rm = TRUE),
      industry_firms = .N
    ), by = .(headquarter_country, FourDigitName, year)]

    # Merge to get both totals
    merged <- merge(.SD, country_year_totals,
                   by = c("headquarter_country", "year"), all.x = TRUE)
    merged <- merge(merged, country_industry_year_totals,
                   by = c("headquarter_country", "FourDigitName", "year"), all.x = TRUE)

    # Calculate: (country total - industry total - self) / (country firms - industry firms - 1)
    other_industry_cdp <- merged$total_cdp - merged$industry_cdp - merged$cdp_sc_member
    other_industry_firms <- merged$total_firms - merged$industry_firms - 1

    # Return peer share (handle edge cases)
    ifelse(other_industry_firms > 0,
           other_industry_cdp / other_industry_firms,
           NA_real_)
  }]

# (e) Lag the cross-domain instruments
panel_extended_instrument[, peer_cdp_share_industry_exclcountry_lag :=
                            shift(peer_cdp_share_industry_exclcountry, n = 1, type = "lag"),
                          by = gvkey]

panel_extended_instrument[, peer_cdp_share_country_excludindustry_lag :=
                            shift(peer_cdp_share_country_excludindustry, n = 1, type = "lag"),
                          by = gvkey]

# Replace NAs with 0 for lagged cross-domain instruments
panel_extended_instrument[is.na(peer_cdp_share_industry_exclcountry_lag),
                          peer_cdp_share_industry_exclcountry_lag := 0]
panel_extended_instrument[is.na(peer_cdp_share_country_excludindustry_lag),
                          peer_cdp_share_country_excludindustry_lag := 0]

# Sanity checks ####
cat("\nSummary statistics for all instruments:\n")
cat("\npeer_cdp_share_country (current year):\n")
print(summary(panel_extended_instrument$peer_cdp_share_country))

cat("\npeer_cdp_share_country_lag (one-year lag):\n")
print(summary(panel_extended_instrument$peer_cdp_share_country_lag))

cat("\npeer_cdp_share_industry_exclcountry_lag (industry excl. own country):\n")
print(summary(panel_extended_instrument$peer_cdp_share_industry_exclcountry_lag))

cat("\npeer_cdp_share_country_excludindustry_lag (country excl. own industry):\n")
print(summary(panel_extended_instrument$peer_cdp_share_country_excludindustry_lag))

# Check correlations between all instruments
cat("\nCorrelation matrix for all instruments:\n")
cor_matrix <- panel_extended_instrument[
  !is.na(peer_cdp_share_lag) &
  !is.na(peer_cdp_share_country_lag) &
  !is.na(peer_cdp_share_industry_exclcountry_lag) &
  !is.na(peer_cdp_share_country_excludindustry_lag),
  cor(cbind(peer_cdp_share_lag,
            peer_cdp_share_country_lag,
            peer_cdp_share_industry_exclcountry_lag,
            peer_cdp_share_country_excludindustry_lag))
]
colnames(cor_matrix) <- c("Industry", "Country", "Ind_ExclCtry", "Ctry_ExclInd")
rownames(cor_matrix) <- c("Industry", "Country", "Ind_ExclCtry", "Ctry_ExclInd")
print(round(cor_matrix, 3))

# Merge instruments with complete cases data ####
cat("\nMerging instruments with complete_data_2022...\n")

# Extract instrument variables from panel_extended_instrument
instruments_only <- panel_extended_instrument %>%
  select(gvkey, year,
         peer_cdp_share, peer_cdp_share_lag,
         peer_cdp_share_country, peer_cdp_share_country_lag,
         peer_cdp_share_industry_exclcountry, peer_cdp_share_industry_exclcountry_lag,
         peer_cdp_share_country_excludindustry, peer_cdp_share_country_excludindustry_lag)

# Merge with complete_data_2022 to get all analysis variables + instruments
complete_data_2022_instrument_country <- complete_data_2022 %>%
  left_join(instruments_only, by = c("gvkey", "year"))

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

# Verify all required variables are present ####
cat("\nVerifying required variables:\n")

# Check instruments
required_instruments <- c("peer_cdp_share",
                          "peer_cdp_share_lag",
                          "peer_cdp_share_country",
                          "peer_cdp_share_country_lag",
                          "peer_cdp_share_industry_exclcountry",
                          "peer_cdp_share_industry_exclcountry_lag",
                          "peer_cdp_share_country_excludindustry",
                          "peer_cdp_share_country_excludindustry_lag")

for (var in required_instruments) {
  if (var %in% names(complete_data_2022_instrument_country)) {
    cat("  ✓", var, "present\n")
  } else {
    stop("ERROR: Missing required instrument: ", var)
  }
}

# Check key analysis variables
required_vars <- c("sbti_commitment_lead1", "cdp_sc_member",
                   "esc_incidents_highreach", "e_disc_coalesced_zeros")

for (var in required_vars) {
  if (var %in% names(complete_data_2022_instrument_country)) {
    cat("  ✓", var, "present\n")
  } else {
    stop("ERROR: Missing required analysis variable: ", var)
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
