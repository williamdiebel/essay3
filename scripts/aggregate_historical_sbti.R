# =============================================================================
# Aggregate Historical SBTi Commitment Data
# =============================================================================
# Purpose: Extract reliable commitment dates for firms in exclude_all.rds by
#          analyzing historical cross-sectional SBTi snapshots (2015-2022)
#
# Input:
#   - exclude_all.rds: 680 firms with "Targets Set" status needing commitment dates
#   - archived_sbti_data/Raw*_preprocessed.csv: Historical SBTi snapshots
#   - sbti_commitments_reliable.rds: Existing reliable commitments (830 firms)
#
# Output:
#   - exclude_all_with_commitment_dates.rds: Full dataset with recovered dates
#   - commitment_date_lookup.csv: gvkey -> earliest_commitment_year mapping
#   - unmatched_firms_for_review.csv: Firms requiring manual review
#   - sbti_commitments_unified.rds: Combined reliable + recovered commitments
#
# Author: Claude Code
# Date: January 2026
# =============================================================================

library(tidyverse)
library(stringdist)  # For fuzzy matching

# =============================================================================
# 1. Load Data
# =============================================================================

cat("=============================================================================\n")
cat("Loading data...\n")
cat("=============================================================================\n\n")

# Load exclude_all.rds
exclude_firms <- readRDS("/Users/william.diebel/Dropbox/NetZero WD Shared Folder/Data/exclude_all.rds")
cat("Loaded exclude_all.rds:", nrow(exclude_firms), "firms\n")

# Load all preprocessed historical files
hist_path <- "/Users/william.diebel/Dropbox/NetZero WD Shared Folder/Data/archived_sbti_data"
files <- list.files(hist_path, pattern = "_preprocessed.csv", full.names = TRUE)
historical <- do.call(rbind, lapply(files, read.csv))
cat("Loaded historical data:", nrow(historical), "observations from", length(files), "files\n")
cat("Years covered:", paste(sort(unique(historical$year)), collapse = ", "), "\n\n")

# Load existing reliable commitments
reliable_path <- "/Users/william.diebel/Dropbox/Mac (2)/Documents/GitHub/essay3/cleaned_data/sbti_commitments_reliable.rds"
if (file.exists(reliable_path)) {
  reliable_existing <- readRDS(reliable_path)
  cat("Loaded existing reliable commitments:", nrow(reliable_existing), "firms\n\n")
} else {
  reliable_existing <- NULL
  cat("Note: sbti_commitments_reliable.rds not found - will create new unified file\n\n")
}

# =============================================================================
# 2. Standardize Names for Matching
# =============================================================================

cat("=============================================================================\n")
cat("Standardizing company names...\n")
cat("=============================================================================\n\n")

clean_company_name <- function(name) {
  name <- tolower(trimws(name))
  # Remove common suffixes
  name <- gsub("\\s*(,?\\s*(inc\\.?|incorporated|corp\\.?|corporation|ltd\\.?|limited|plc|llc|llp|s\\.?a\\.?|s\\.?a\\.?s\\.?|a\\.?g\\.?|n\\.?v\\.?|b\\.?v\\.?|gmbh|co\\.?|company|group|holdings?|international|intl\\.?))\\s*$", "", name, ignore.case = TRUE)
  # Remove parenthetical info
  name <- gsub("\\s*\\([^)]*\\)\\s*", " ", name)
  # Standardize punctuation
  name <- gsub("[.,;:'\"&]", " ", name)
  # Collapse whitespace
  name <- gsub("\\s+", " ", name)
  trimws(name)
}

# Apply cleaning
exclude_firms <- exclude_firms %>%
  mutate(
    name_clean = clean_company_name(company_name),
    name_original = company_name
  )

historical <- historical %>%
  mutate(
    name_clean = clean_company_name(company_name_sbti_historical),
    name_original = company_name_sbti_historical,
    # Standardize empty strings to NA
    isin_sbti_historical = ifelse(isin_sbti_historical == "", NA, isin_sbti_historical),
    lei_sbti_historical = ifelse(lei_sbti_historical == "", NA, lei_sbti_historical)
  )

# =============================================================================
# 3. Build Historical Summary by Firm
# =============================================================================

cat("=============================================================================\n")
cat("Building historical summary...\n")
cat("=============================================================================\n\n")

# For each unique firm in historical data, find:
# - Earliest year appearing as "Committed"
# - Earliest year appearing as "Targets Set"
# - All identifiers (ISIN, LEI, names)

hist_summary <- historical %>%
  group_by(name_clean) %>%
  summarise(
    # Commitment info
    ever_committed = any(status == "Committed"),
    earliest_committed_year = if(any(status == "Committed")) min(year[status == "Committed"]) else NA_integer_,

    # Targets Set info
    ever_targets_set = any(status %in% c("Targets Set", "Targets set")),
    earliest_targets_set_year = if(any(status %in% c("Targets Set", "Targets set")))
      min(year[status %in% c("Targets Set", "Targets set")]) else NA_integer_,

    # Identifiers (take first non-NA)
    isin_hist = first(na.omit(isin_sbti_historical)),
    lei_hist = first(na.omit(lei_sbti_historical)),

    # Name variants
    name_variants = paste(unique(name_original), collapse = " | "),

    # All years observed
    years_observed = paste(sort(unique(year)), collapse = ", "),

    .groups = "drop"
  )

cat("Historical summary created:", nrow(hist_summary), "unique firms\n")
cat("  - Firms ever Committed:", sum(hist_summary$ever_committed), "\n")
cat("  - Firms ever Targets Set:", sum(hist_summary$ever_targets_set), "\n\n")

# =============================================================================
# 4. Match exclude_all firms to historical data
# =============================================================================

cat("=============================================================================\n")
cat("Matching exclude_all firms to historical data...\n")
cat("=============================================================================\n\n")

# Initialize match tracking
exclude_firms <- exclude_firms %>%
  mutate(
    match_method = NA_character_,
    matched_name_hist = NA_character_,
    earliest_committed_year = NA_integer_,
    earliest_targets_set_year = NA_integer_,
    commitment_date_source = NA_character_,
    final_commitment_year = NA_integer_
  )

# -----------------------------------------------------------------------------
# Step 4a: Match by ISIN
# -----------------------------------------------------------------------------

cat("Step 4a: Matching by ISIN...\n")

# Create ISIN lookup from historical
isin_lookup <- hist_summary %>%
  filter(!is.na(isin_hist)) %>%
  select(isin_hist, name_clean, earliest_committed_year, earliest_targets_set_year)

# Match
for (i in 1:nrow(exclude_firms)) {
  if (!is.na(exclude_firms$isin[i])) {
    match <- isin_lookup %>% filter(isin_hist == exclude_firms$isin[i])
    if (nrow(match) > 0) {
      exclude_firms$match_method[i] <- "ISIN"
      exclude_firms$matched_name_hist[i] <- match$name_clean[1]
      exclude_firms$earliest_committed_year[i] <- match$earliest_committed_year[1]
      exclude_firms$earliest_targets_set_year[i] <- match$earliest_targets_set_year[1]
    }
  }
}

cat("  Matched by ISIN:", sum(exclude_firms$match_method == "ISIN", na.rm = TRUE), "\n")

# -----------------------------------------------------------------------------
# Step 4b: Match by LEI (for unmatched)
# -----------------------------------------------------------------------------

cat("Step 4b: Matching by LEI...\n")

lei_lookup <- hist_summary %>%
  filter(!is.na(lei_hist)) %>%
  select(lei_hist, name_clean, earliest_committed_year, earliest_targets_set_year)

for (i in 1:nrow(exclude_firms)) {
  if (is.na(exclude_firms$match_method[i]) && !is.na(exclude_firms$lei[i])) {
    match <- lei_lookup %>% filter(lei_hist == exclude_firms$lei[i])
    if (nrow(match) > 0) {
      exclude_firms$match_method[i] <- "LEI"
      exclude_firms$matched_name_hist[i] <- match$name_clean[1]
      exclude_firms$earliest_committed_year[i] <- match$earliest_committed_year[1]
      exclude_firms$earliest_targets_set_year[i] <- match$earliest_targets_set_year[1]
    }
  }
}

cat("  Matched by LEI:", sum(exclude_firms$match_method == "LEI", na.rm = TRUE), "\n")

# -----------------------------------------------------------------------------
# Step 4c: Match by exact cleaned name (for unmatched)
# -----------------------------------------------------------------------------

cat("Step 4c: Matching by exact cleaned name...\n")

name_lookup <- hist_summary %>%
  select(name_clean, earliest_committed_year, earliest_targets_set_year)

for (i in 1:nrow(exclude_firms)) {
  if (is.na(exclude_firms$match_method[i])) {
    match <- name_lookup %>% filter(name_clean == exclude_firms$name_clean[i])
    if (nrow(match) > 0) {
      exclude_firms$match_method[i] <- "exact_name"
      exclude_firms$matched_name_hist[i] <- exclude_firms$name_clean[i]
      exclude_firms$earliest_committed_year[i] <- match$earliest_committed_year[1]
      exclude_firms$earliest_targets_set_year[i] <- match$earliest_targets_set_year[1]
    }
  }
}

cat("  Matched by exact name:", sum(exclude_firms$match_method == "exact_name", na.rm = TRUE), "\n")

# -----------------------------------------------------------------------------
# Step 4d: Fuzzy name matching (for remaining unmatched)
# -----------------------------------------------------------------------------

cat("Step 4d: Fuzzy name matching for remaining unmatched...\n")

unmatched_idx <- which(is.na(exclude_firms$match_method))
cat("  Firms remaining to match:", length(unmatched_idx), "\n")

# Fuzzy matching parameters
max_dist <- 0.15  # Maximum string distance (15%)

fuzzy_matches <- data.frame(
  exclude_idx = integer(),
  exclude_name = character(),
  hist_name = character(),
  distance = numeric(),
  stringsAsFactors = FALSE
)

hist_names <- unique(hist_summary$name_clean)

for (i in unmatched_idx) {
  target_name <- exclude_firms$name_clean[i]

  # Calculate string distances using Jaro-Winkler
  distances <- stringdist(target_name, hist_names, method = "jw")

  # Find best match
  best_idx <- which.min(distances)
  best_dist <- distances[best_idx]

  if (best_dist <= max_dist) {
    best_name <- hist_names[best_idx]
    match_info <- hist_summary %>% filter(name_clean == best_name)

    exclude_firms$match_method[i] <- "fuzzy_name"
    exclude_firms$matched_name_hist[i] <- best_name
    exclude_firms$earliest_committed_year[i] <- match_info$earliest_committed_year[1]
    exclude_firms$earliest_targets_set_year[i] <- match_info$earliest_targets_set_year[1]

    fuzzy_matches <- rbind(fuzzy_matches, data.frame(
      exclude_idx = i,
      exclude_name = exclude_firms$name_original[i],
      hist_name = match_info$name_variants[1],
      distance = round(best_dist, 3),
      stringsAsFactors = FALSE
    ))
  }
}

cat("  Matched by fuzzy name:", sum(exclude_firms$match_method == "fuzzy_name", na.rm = TRUE), "\n")

# =============================================================================
# 5. Determine Final Commitment Year
# =============================================================================

cat("\n=============================================================================\n")
cat("Determining final commitment year...\n")
cat("=============================================================================\n\n")

for (i in 1:nrow(exclude_firms)) {
  if (!is.na(exclude_firms$earliest_committed_year[i])) {
    # Case 1: Firm appeared as "Committed" in historical data
    exclude_firms$final_commitment_year[i] <- exclude_firms$earliest_committed_year[i]
    exclude_firms$commitment_date_source[i] <- "historical_committed"

  } else if (!is.na(exclude_firms$earliest_targets_set_year[i])) {
    # Case 2: Firm only appeared as "Targets Set" - committed before that year
    # Use (earliest_targets_set_year - 1) as upper bound for commitment
    exclude_firms$final_commitment_year[i] <- exclude_firms$earliest_targets_set_year[i] - 1
    exclude_firms$commitment_date_source[i] <- "inferred_from_targets_set"

  } else if (!is.na(exclude_firms$match_method[i])) {
    # Case 3: Matched but no commitment/targets info (shouldn't happen often)
    exclude_firms$commitment_date_source[i] <- "matched_no_date"

  } else {
    # Case 4: No match found
    exclude_firms$commitment_date_source[i] <- "unmatched"
  }
}

# Summary
cat("Commitment date sources:\n")
print(table(exclude_firms$commitment_date_source, useNA = "ifany"))

cat("\nFinal commitment year distribution:\n")
print(table(exclude_firms$final_commitment_year, useNA = "ifany"))

# =============================================================================
# 6. Summary Statistics
# =============================================================================

cat("\n=============================================================================\n")
cat("Match Summary\n")
cat("=============================================================================\n\n")

cat("Match method breakdown:\n")
print(table(exclude_firms$match_method, useNA = "ifany"))

matched_count <- sum(!is.na(exclude_firms$match_method))
unmatched_count <- sum(is.na(exclude_firms$match_method))

cat("\nOverall: ", matched_count, " matched (", round(100*matched_count/nrow(exclude_firms), 1),
    "%), ", unmatched_count, " unmatched\n", sep = "")

# Compare to original date field
cat("\n=============================================================================\n")
cat("Comparison: Recovered vs Original Dates\n")
cat("=============================================================================\n\n")

comparison <- exclude_firms %>%
  filter(!is.na(final_commitment_year)) %>%
  mutate(
    original_year = as.integer(format(date, "%Y")),
    year_diff = final_commitment_year - original_year
  )

cat("For firms with recovered commitment dates:\n")
cat("  - Same as original:", sum(comparison$year_diff == 0, na.rm = TRUE), "\n")
cat("  - Earlier than original:", sum(comparison$year_diff < 0, na.rm = TRUE), "\n")
cat("  - Later than original:", sum(comparison$year_diff > 0, na.rm = TRUE), "\n")

cat("\nYear difference distribution:\n")
print(table(comparison$year_diff))

# =============================================================================
# 7. Create Output Files
# =============================================================================

cat("\n=============================================================================\n")
cat("Creating output files...\n")
cat("=============================================================================\n\n")

output_dir <- "/Users/william.diebel/Dropbox/Mac (2)/Documents/GitHub/essay3/cleaned_data"

# 7a. Full dataset with commitment dates
exclude_output <- exclude_firms %>%
  select(
    # Original identifiers
    sbti_id, gvkey, isin, lei, company_name,
    # Original SBTi info
    near_term_target_status, near_term_target_classification, date,
    location, region, sector,
    # Match results
    match_method, matched_name_hist,
    earliest_committed_year, earliest_targets_set_year,
    commitment_date_source, final_commitment_year
  )

saveRDS(exclude_output, file.path(output_dir, "exclude_all_with_commitment_dates.rds"))
cat("Saved: exclude_all_with_commitment_dates.rds\n")

# 7b. Lookup table (gvkey -> commitment year)
lookup_table <- exclude_firms %>%
  filter(!is.na(final_commitment_year)) %>%
  select(gvkey, company_name, final_commitment_year, commitment_date_source, match_method) %>%
  arrange(gvkey)

write.csv(lookup_table, file.path(output_dir, "commitment_date_lookup.csv"), row.names = FALSE)
cat("Saved: commitment_date_lookup.csv (", nrow(lookup_table), " firms)\n", sep = "")

# 7c. Unmatched firms for manual review
unmatched_review <- exclude_firms %>%
  filter(is.na(match_method)) %>%
  select(
    sbti_id, gvkey, isin, lei, company_name, name_clean,
    date, location, sector
  ) %>%
  arrange(company_name)

write.csv(unmatched_review, file.path(output_dir, "unmatched_firms_for_review.csv"), row.names = FALSE)
cat("Saved: unmatched_firms_for_review.csv (", nrow(unmatched_review), " firms)\n", sep = "")

# 7d. Fuzzy matches for review
if (nrow(fuzzy_matches) > 0) {
  fuzzy_review <- fuzzy_matches %>%
    left_join(
      exclude_firms %>% select(exclude_name = name_original, gvkey, isin, final_commitment_year),
      by = "exclude_name"
    ) %>%
    arrange(desc(distance))

  write.csv(fuzzy_review, file.path(output_dir, "fuzzy_matches_for_review.csv"), row.names = FALSE)
  cat("Saved: fuzzy_matches_for_review.csv (", nrow(fuzzy_review), " matches)\n", sep = "")
}

# 7e. Unified commitments file (combine with existing reliable commitments)
recovered_commitments <- exclude_firms %>%
  filter(!is.na(final_commitment_year)) %>%
  transmute(
    gvkey = gvkey,
    company_name = company_name,
    isin = isin,
    sbti_id = sbti_id,
    commitment_year = final_commitment_year,
    commitment_date_source = commitment_date_source,
    match_method = match_method,
    source_file = "exclude_all_recovered"
  )

cat("\nRecovered commitments:", nrow(recovered_commitments), "firms\n")

if (!is.null(reliable_existing)) {
  # Check for overlaps
  overlap_gvkey <- intersect(recovered_commitments$gvkey, reliable_existing$gvkey)
  cat("Overlapping gvkeys with existing reliable:", length(overlap_gvkey), "\n")

  # For overlaps, keep the earlier date
  if (length(overlap_gvkey) > 0) {
    cat("Note: For overlapping firms, will keep earliest commitment year\n")
  }
}

saveRDS(recovered_commitments, file.path(output_dir, "recovered_commitments_from_exclude.rds"))
cat("Saved: recovered_commitments_from_exclude.rds\n")

# =============================================================================
# 8. Summary Report
# =============================================================================

cat("\n=============================================================================\n")
cat("FINAL SUMMARY\n")
cat("=============================================================================\n\n")

cat("Input: ", nrow(exclude_firms), " firms from exclude_all.rds\n\n", sep = "")

cat("Matching Results:\n")
cat("  - ISIN matches:       ", sprintf("%3d", sum(exclude_firms$match_method == "ISIN", na.rm = TRUE)), "\n")
cat("  - LEI matches:        ", sprintf("%3d", sum(exclude_firms$match_method == "LEI", na.rm = TRUE)), "\n")
cat("  - Exact name matches: ", sprintf("%3d", sum(exclude_firms$match_method == "exact_name", na.rm = TRUE)), "\n")
cat("  - Fuzzy name matches: ", sprintf("%3d", sum(exclude_firms$match_method == "fuzzy_name", na.rm = TRUE)), "\n")
cat("  - TOTAL MATCHED:      ", sprintf("%3d", matched_count), " (", round(100*matched_count/nrow(exclude_firms), 1), "%)\n", sep = "")
cat("  - Unmatched:          ", sprintf("%3d", unmatched_count), " (", round(100*unmatched_count/nrow(exclude_firms), 1), "%)\n\n", sep = "")

cat("Commitment Date Recovery:\n")
cat("  - From historical Committed status: ", sprintf("%3d", sum(exclude_firms$commitment_date_source == "historical_committed", na.rm = TRUE)), "\n")
cat("  - Inferred from Targets Set year:   ", sprintf("%3d", sum(exclude_firms$commitment_date_source == "inferred_from_targets_set", na.rm = TRUE)), "\n")
cat("  - Unable to determine:              ", sprintf("%3d", sum(exclude_firms$commitment_date_source %in% c("unmatched", "matched_no_date"), na.rm = TRUE)), "\n\n")

cat("Output Files:\n")
cat("  1. exclude_all_with_commitment_dates.rds - Full dataset with recovered dates\n")
cat("  2. commitment_date_lookup.csv            - gvkey -> commitment year mapping\n")
cat("  3. unmatched_firms_for_review.csv        - Firms needing manual review\n")
cat("  4. fuzzy_matches_for_review.csv          - Fuzzy matches for verification\n")
cat("  5. recovered_commitments_from_exclude.rds - Ready for merging with reliable commitments\n")

cat("\n=============================================================================\n")
cat("Script complete.\n")
cat("=============================================================================\n")
