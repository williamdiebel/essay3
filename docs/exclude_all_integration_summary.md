# Exclude_All Firms Integration Summary

**Date:** 2025-01-28
**Author:** William Diebel (with Claude Code assistance)

## Overview

Successfully integrated 680 exclude_all firms back into the main panel workflow by recovering their SBTi commitment dates and creating a unified base panel.

## What Was Done

### 1. Recovered SBTi Commitment Dates for Exclude_All Firms

The 680 firms that were previously excluded (`exclude_all`) had missing `initial_commitment_year` values because they were excluded early in the data pipeline. We recovered their commitment dates by:

- Matching firms to historical SBTi data using ISIN identifiers
- Cross-referencing with the current SBTi database
- Inferring commitment years from target-setting dates where direct dates were unavailable

**Output files created in `essay3/cleaned_data/`:**
- `recovered_commitments_from_exclude.rds` - 680 firms with recovered dates
- `exclude_all_with_commitment_dates.rds` - Full firm details with dates
- `sbti_commitments_unified.rds` / `.csv` - Unified commitment lookup
- `commitment_date_lookup.csv` - Reference lookup table

### 2. Created Unified Base Panel Script

Created `00_create_unified_base_panel.R` in the shared R Scripts folder that:

1. Loads the current panel (`rrpanel_sbti_commitments_2024.rds`) - 13,947 firms
2. Extracts exclude_all firms from source (`rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds`) - 691 firms
3. Joins recovered commitment dates (680 of 691 firms)
4. Combines into unified panel with `source_panel` indicator

**Output:** `rrpanel_sbti_commitments_unified_2024.rds`

## Results Summary

| Metric | Before | After |
|--------|--------|-------|
| Total firms in panel | 13,947 | 14,638 |
| Firms with SBTi commitment dates | 973 | 1,653 |
| Panel rows (firm-years) | 161,979 | 171,280 |

### Commitment Date Sources for Recovered Firms

| Source | Count |
|--------|-------|
| `historical_committed` | ~400 |
| `inferred_from_targets_set` | ~280 |

## Items Flagged for Manual Review

### 1. Eleven Firms Without Recovered Dates

11 of the 691 exclude_all firms could not be matched to SBTi commitment data:

```r
# To identify these firms:
unified <- readRDS("~/Dropbox/NetZero WD Shared Folder/Data/rrpanel_sbti_commitments_unified_2024.rds")
no_dates <- unified %>%
  filter(source_panel == "exclude_all", is.na(initial_commitment_year)) %>%
  distinct(gvkey, conm)
```

**Action:** Review whether these 11 firms should remain in the panel or if commitment dates can be manually verified.

### 2. Inferred Commitment Dates

~280 firms have commitment dates inferred from their target-setting date rather than a direct "Committed" status date. These are flagged with `commitment_date_source = "inferred_from_targets_set"`.

**Action:** Consider sensitivity analysis excluding inferred dates if commitment timing is critical.

### 3. Data Gaps for Exclude_All Firms (2021-2023)

The exclude_all firms currently only have data through 2020. They are missing:
- 2021-2023 Compustat financials
- 2021-2023 RepRisk incident data
- 2021-2023 Bloomberg ESG scores
- 2021-2022 CDP SCP membership matching

**Action:** Must run the full downstream pipeline to populate these fields.

## Workflow: Next Steps

The unified panel must now go through the complete data enrichment pipeline:

### Step 1: CDP SCP Member Matching
**Script:** `updating_cdp_scp.R`
**Purpose:** Match firms to CDP Supply Chain Program membership status
**Modifications needed:**
- Update input to read `rrpanel_sbti_commitments_unified_2024.rds`
- Ensure matching logic handles the 691 new firms

### Step 2: Panel Extension (2021-2023 Data)
**Script:** `panel_extension.R`
**Purpose:** Add Compustat, RepRisk, and Bloomberg data for 2021-2023
**Modifications needed:**
- Update input to use output from Step 1
- Verify all 14,638 firms are processed
- May need to re-pull raw data if exclude_all firms weren't in original extracts

### Step 3: Complete Case Filtering & Lead DV Creation
**Script:** `analysis_05_2025.R`
**Purpose:** Filter to complete cases, create lead outcome variables
**Modifications needed:**
- Update input to use output from Step 2
- Verify filtering criteria don't inadvertently drop recovered firms

### Step 4: Instrument Construction
**Script:** `cdp_scp_peer_share_instrument.R`
**Purpose:** Construct peer share instruments for 2SLS
**Modifications needed:**
- Update input to use output from Step 3
- The expanded firm count should improve instrument precision

## File Locations

### Scripts
- `~/Dropbox/NetZero WD Shared Folder/R Scripts/00_create_unified_base_panel.R`

### Data Files
| File | Location |
|------|----------|
| Unified panel | `~/Dropbox/NetZero WD Shared Folder/Data/rrpanel_sbti_commitments_unified_2024.rds` |
| Recovered commitments | `essay3/cleaned_data/recovered_commitments_from_exclude.rds` |
| Commitment lookup | `essay3/cleaned_data/commitment_date_lookup.csv` |

## Validation Checks Performed

- [x] No duplicate gvkey-year combinations in unified panel
- [x] All 680 recovered firms successfully matched to panel
- [x] Year range preserved (2007-2020)
- [x] Column structure consistent between current and exclude_all firms
- [x] `source_panel` indicator added to track firm origin

## Code to Reproduce

```r
# Run the unified panel creation
source("~/Dropbox/NetZero WD Shared Folder/R Scripts/00_create_unified_base_panel.R")

# Verify results
unified <- readRDS("~/Dropbox/NetZero WD Shared Folder/Data/rrpanel_sbti_commitments_unified_2024.rds")
table(unified$source_panel)
# current: 161,979 | exclude_all: 9,301
```
