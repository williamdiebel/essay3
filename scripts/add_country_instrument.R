# ══════════════════════════════════════════════════════════════════════════════
# ADD COUNTRY-BASED PEER INSTRUMENT TO EXISTING DATA
# ══════════════════════════════════════════════════════════════════════════════
# This script adds the country-based peer instrument to the existing
# complete_data_2022_instrument.rds file
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(data.table)

cat("Loading existing data...\n")
complete_data_2022_instrument <- readRDS("cleaned_data/complete_data_2022_instrument.rds")
cat("  ✓ Data loaded:", nrow(complete_data_2022_instrument), "rows\n\n")

# Convert to data.table for speed
setDT(complete_data_2022_instrument)

cat("Creating country-based peer instrument...\n")

# ──────────────────────────────────────────────────────────────────────────────
# Country-based peer share (peer_cdp_share_country_lag)
# ──────────────────────────────────────────────────────────────────────────────
# Calculate the share of firms in the SAME COUNTRY-YEAR that are CDP members,
# EXCLUDING the focal firm itself
# ──────────────────────────────────────────────────────────────────────────────

# (a) country-year peer share EXCLUDING focal firm
complete_data_2022_instrument[ , peer_cdp_share_country :=
                      ifelse(.N == 1, NA_real_,     # single-firm country-year
                             (sum(cdp_sc_member, na.rm = TRUE) - cdp_sc_member) /
                               (.N - 1)),
                    by = .(headquarter_country, year)]

# (b) lag by one year for each firm (use LAST year's country peers)
complete_data_2022_instrument[ , peer_cdp_share_country_lag := shift(peer_cdp_share_country, n = 1, type = "lag"),
                    by = gvkey]

# (c) replace NA from first year or single-firm groups with 0
complete_data_2022_instrument[is.na(peer_cdp_share_country_lag), peer_cdp_share_country_lag := 0]

cat("  ✓ Country instrument created\n\n")

# Quick summary
cat("Summary of country-based peer CDP share (lagged):\n")
summary(complete_data_2022_instrument$peer_cdp_share_country_lag)

cat("\n")

# Convert back to tibble for consistency
complete_data_2022_instrument <- as_tibble(complete_data_2022_instrument)

# Save updated dataset
cat("Saving updated dataset...\n")
saveRDS(complete_data_2022_instrument, "cleaned_data/complete_data_2022_instrument.rds")
cat("  ✓ Saved to cleaned_data/complete_data_2022_instrument.rds\n\n")

cat("══════════════════════════════════════════════════════════════════════════════\n")
cat("DONE! Country instrument successfully added.\n")
cat("You can now run scripts/run_iv_analysis.R\n")
cat("══════════════════════════════════════════════════════════════════════════════\n")
