# Data Processing Documentation: Panel Extension and SBTi Commitment Data (2025 Update)

## Overview

This document provides comprehensive documentation of the data processing steps undertaken to extend the panel dataset from 2007-2020 to 2007-2022 (allowing capture of commitments through end of 2023 using a 1-year lag approach) and to address critical data quality issues in SBTi commitment dating.

**Key Objective**: Extend the panel to incorporate data for all covariates until 2022, while ensuring reliable identification of initial SBTi commitment dates.

---

## Critical Data Challenge: SBTi Target Date Updates

### The Problem

When using the most current SBTi data (downloaded April 2025), we encountered a significant data quality issue:

**Target dates for firms were retroactively updated when their SBTi targets were revised.**

This prevented reliable identification of the initial commitment year for firms with validated targets ("targets set"). The SBTi database structure includes:

1. **Commitments**: Firms that have committed to setting science-based targets (24-month window to validate)
2. **Targets Set**: Firms whose targets have been validated by SBTi
3. **Removed**: Commitments that expired or were withdrawn

**The Issue**: When firms revise their validated targets (which many do), the SBTi database overwrites the original target date with the revised target date. This makes it impossible to identify when these firms originally committed to/set their targets.

### The Solution

We implemented a multi-vintage data approach:

1. **Excluded all firms with targets set as of 2022** from the analysis
2. **Excluded committed firms with missing commitment dates**
3. **Retained only firms with active commitments** that have reliable, non-changing commitment dates

**Rationale**:
- **Commitment dates do not change** in the SBTi data (unlike target dates)
- Firms that had only committed (not yet set validated targets) as of data collection points maintain accurate commitment dates
- By using SBTi data collected at three time points (2022, 2023, 2025), we can reliably identify when firms made their initial commitments

**Result**: All firms with an active commitment in 2022 and forward are captured based on SBTi commitment/target data collected in 2022, 2023, and 2025.

---

## Data Processing Workflow

The data processing was executed through **six R scripts** in chronological order:

### 1. `sbti_data_cleaning_2025.R` (First)

**Purpose**: Clean and categorize the April 2025 SBTi download

**Key Operations**:
- Downloaded SBTi target-level data (April 2025): 24,203 total observations
- Standardized missing data coding (converted "NA" strings to true NA values)
- Categorized observations into three mutually exclusive types:
  - **Targets**: 19,017 observations (firms with validated targets)
  - **Commitments**: 3,786 observations (firms committed but not yet validated)
  - **Removed**: 1,400 observations (expired/withdrawn commitments, 94% expired)

**Output datasets created**:
- `net_zero_sbti_targets.rds` (3,072 net-zero targets, 2,012 unique firms)
- `scope_3_sbti_targets.rds` (8,934 scope 3 targets, 3,669 unique firms)
- `all_sbti_targets.rds` (19,017 total targets, 7,326 unique firms)
- `all_sbti_targets_and_commitments.rds` (22,803 targets + commitments, 9,717 unique firms)
- `sbti_removed_commitments.rds` (1,400 removed, 988 unique firms)
- **`sbti_2024_merging_table.rds`** (10,405 unique firms with identifiers for merging)

**Key Finding**: 300 firms appeared in both removed and active/target categories, indicating firms that removed initial commitments but later re-committed.

---

### 2. `merging_sbti_2024.R` (Second)

**Purpose**: Match SBTi firms to panel data (gvkey identifiers from Compustat/FactSet)

**Matching Strategy** (hierarchical, cascading):
1. **By ISIN** (International Securities Identification Number) - most reliable
2. **By exact company name** (conm variable from Compustat)
3. **By exact alternative name** (name variable from FactSet)
4. **By cleaned company name** - extensive name cleaning to remove common suffixes/prefixes:
   - Removed: LIMITED, LTD, INCORPORATED, INC, CORPORATION, CORP, COMPANY, CO, PLC, LLC, HOLDINGS, etc.
   - Standardized: PWR→POWER, MFG→MANUFACTURING, TECH→TECHNOLOGY, SVCS→SERVICES, etc.
   - Removed special characters, numbers, parenthetical information
5. **By fuzzy matching** (for remaining unmatched firms)

**Data Source**:
- SBTi firm identifiers from `sbti_2024_merging_table.rds`
- Panel firm identifiers from `rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds`

**Output**:
- `all_gvkey_sbti_matches_lei_prior.rds` - mapping table linking sbti_id to gvkey

**Match Quality**: Comparison with prior matches from earlier analysis showed 1,484 previously matched unique firms as reference point.

---

### 3. `isolating_commitments_05_25.R` (Third) **[CRITICAL SCRIPT]**

**Purpose**: Extract reliable commitment dates by combining three SBTi data vintages

**Data Sources**:
- `sbti_2022`: Downloaded October 2022 by co-author
- `sbti_2023`: Downloaded April 2023 by primary author
- `sbti_2025`: Downloaded April 2025 by primary author (processed in script 1)

**Why Three Vintages?**
- **Target dates change** when firms revise their validated targets (overwritten in database)
- **Commitment dates remain stable** - firms that committed but haven't validated yet maintain original dates
- By comparing across vintages, we can identify firms whose commitment dates are reliable

**Key Processing Steps**:

1. **Assign sbti_id to historical datasets** (2022, 2023):
   - Match by ISIN → by LEI → by company name
   - Handle duplicates by selecting earliest commitment date per firm
   - Result: 2022 data matched to 2,311 unique sbti_id values; 2023 data matched to 2,867 unique sbti_id values

2. **Extract commitment-only observations**:
   - Filter for observations where status is "Active" or "Extended" (committed but not yet validated)
   - Exclude observations with status "Target set" (these have changing dates)
   - Exclude observations with missing commitment dates

3. **Combine vintages to create master commitment list**:
   - Pool all commitment observations from 2022, 2023, and 2025 datasets
   - For firms appearing in multiple vintages, retain earliest commitment date
   - Cross-validate commitment dates across vintages

4. **Quality checks**:
   - Verify no firms with "targets set" as of 2022 are included (these have unreliable dates)
   - Confirm all retained firms have non-missing commitment dates
   - Check for consistency of commitment dates across vintages for same firms

**Output**:
- `sbti_commitments_reliable.rds` - master list of firms with reliable commitment dates
- Dataset includes commitment dates from 2015-2023

**Commitment Distribution** (final reliable commitments):
```
2015:   4 commitments
2016:   6 commitments
2017:   1 commitment
2018:   8 commitments
2019:  17 commitments
2020:  54 commitments
2021: 245 commitments
2022: 339 commitments
2023: 156 commitments
---
Total: 830 unique commitments with reliable dates
```

**Critical Exclusions**:
- All firms with validated targets as of 2022 (unreliable dating due to target revisions)
- Firms with missing commitment dates
- This conservative approach ensures data quality at the cost of sample size

---

### 4. `updating_cdp_scp.R` (Fourth)

**Purpose**: Update CDP Supply Chain Program (CDP SCP) membership data through 2022

**Background**:
- CDP SCP is the key explanatory variable (environmental supply chain orientation)
- Original panel had CDP SCP data through 2020
- Extension requires CDP SCP membership for 2021-2022

**Data Sources**:
- Hand-collected CDP SCP annual member lists (2008-2022)
- Each annual report lists member firms for that year

**Processing**:
1. **Extract member lists** from CDP SCP annual reports (2021, 2022)
2. **Clean company names** using same standardization as SBTi matching
3. **Match to panel firms** (gvkey):
   - By exact name match
   - By cleaned name match
   - Manual verification of ambiguous matches
4. **Create panel structure**:
   - Binary indicator: cdp_sc_member = 1 if firm was member in year t, 0 otherwise
   - Carry forward membership (once a member, assumed to continue unless explicitly dropped)

**Output**:
- Updated `cdp_sc_member` variable for years 2021-2022
- On average, each member requests disclosures from 108 suppliers (some reach 700+ suppliers annually)
- About 50% of requested suppliers respond; 50% of respondents disclose publicly

**Quality Check**:
- Verified consistency with prior years (2008-2020)
- Cross-checked member counts against CDP's published member statistics
- Identified 186 unique CDP SCP member firms in panel

---

### 5. `panel_extension.R` (Fifth)

**Purpose**: Extend all panel variables from 2020 to 2022

**Data Sources**:
- **Bloomberg**: Environmental disclosure scores, emissions data (updated download 2025)
- **Compustat**: Financial variables (ROA, total assets, leverage) - 2021, 2022 data
- **RepRisk**: Environmental incidents through 2022
- **FactSet**: Firm identifiers and industry classifications

**Variables Extended**:

**Environmental Variables**:
- `e_disc_coalesced_zeros`: Environmental disclosure score (Bloomberg ESG disclosure score)
- `e_disc_missing`: Indicator for missing environmental disclosure
- `scope1_zeros`: Scope 1 GHG emissions (direct emissions)
- `scope1_missing`: Indicator for missing Scope 1 data
- `esc_incidents`: Environmental supply chain incidents count (RepRisk)
- `esc_incidents_highreach`: High-reach environmental incidents (>100k reach)
- `esc_incidents_highsev`: High-severity environmental incidents

**Financial Variables**:
- `roa_oibdp_w1_at_w1`: Return on assets (winsorized at 1%)
- `at_usd_winsorized_1_log`: Log of total assets in USD (winsorized at 1%)
- `tll_lt_w1_at_w1`: Leverage ratio (total liabilities / total assets, winsorized at 1%)

**Firm Identifiers** (verified/updated):
- `gvkey`: Compustat Global Company Key
- `factset_id`: FactSet Entity ID
- `primary_isin`: Primary ISIN
- `reprisk_id`: RepRisk company ID
- `conm`: Company name from Compustat
- `name`: Company name from FactSet
- `headquarter_country`: Country of headquarters
- `FourDigitName`: 4-digit GICS industry classification

**Processing Steps**:

1. **Download updated data** for 2021-2022:
   - Bloomberg terminal downloads for environmental variables
   - Compustat North America + Global downloads for financials
   - RepRisk API/database extracts for incidents

2. **Apply consistent variable definitions**:
   - Use same variable construction as 2007-2020 period
   - Maintain same winsorization thresholds (1% for financial variables)
   - Apply same missing data handling (indicator variables)

3. **Handle missing data**:
   - Create indicator variables for systematically missing data (e.g., `e_disc_missing`, `scope1_missing`)
   - Code missing values as zeros where appropriate (e.g., `e_disc_coalesced_zeros`)
   - Document missing data patterns for 2021-2022 vs. earlier years

4. **Verify consistency**:
   - Check variable distributions for 2021-2022 vs. 2019-2020 (expect similar patterns)
   - Identify and investigate outliers
   - Validate firm identifiers (gvkey) remain consistent

5. **Create final extended panel**:
   - Append 2021-2022 data to existing 2007-2020 panel
   - Verify no duplicate firm-year observations
   - Final structure: unbalanced panel, 2007-2022

**Output**:
- `panel_extended_complete.rds`: Population-level dataset (204,070 firm-year observations, 14,627 firms)
- Includes all firms regardless of data completeness
- Years: 2007-2022 (16 years)

---

### 6. `analysis_05_2025.R` (Sixth/Final)

**Purpose**: Create analysis-ready dataset with complete cases and lag structure

**Key Operations**:

1. **Merge SBTi commitment data**:
   - Join `sbti_commitments_reliable.rds` (from script 3) with extended panel
   - Match on gvkey using sbti-gvkey mapping (from script 2)
   - Create commitment indicator: `sbti_committed = 1` if firm committed in year t or earlier

2. **Create lead dependent variable**:
   ```r
   sbti_commitment_lead1 = sbti_committed_{t+1}
   ```
   - This captures whether firm makes SBTi commitment in following year
   - Allows use of time t variables as predictors of time t+1 commitment

3. **Apply lag structure for instruments** (consistent with IV strategy):
   - `peer_cdp_share_lag`: Industry peer CDP share at t-1
   - `peer_cdp_share_country_lag`: Country peer CDP share at t-1
   - `e_disc_lag2`: Environmental disclosure at t-2
   - `industry_incidents_excl_lag`: Industry incidents (excluding own) at t-1

4. **Filter to complete cases**:
   - Require non-missing values for:
     - Dependent variable: `sbti_commitment_lead1`
     - Key explanatory variable: `cdp_sc_member`
     - All control variables
     - All instruments (where applicable)
   - Result: 154,062 firm-year observations → 128,244 complete cases with all instruments

5. **Create analysis subsets**:
   - `complete_data_2022.rds`: Complete cases for baseline analysis (154,062 obs, 12,993 firms)
   - `complete_data_2022_instrument_country.rds`: Complete cases with all instruments (used in current IV analysis)

**Sample Retention**:
- Population: 204,070 firm-year observations
- Complete cases (all variables): 154,062 (75.5% retention)
- Complete cases (with all instruments): 128,244 (83.2% of complete cases, 62.8% of population)

**Output Datasets**:
- `complete_data_2022.rds`: Analysis-ready data for descriptive and baseline models
- `complete_data_2022_instrument_country.rds`: IV analysis-ready with all instruments
- Both saved in `cleaned_data/` folder

---

## Key Changes from Original Manuscript Data (2020)

### Sample Size and Coverage

| Dimension | Original (2020) | Extended (2022) | Change |
|-----------|----------------|-----------------|--------|
| **Time Period** | 2007-2020 (14 years) | 2007-2022 (16 years) | +2 years |
| **Population Obs** | 171,280 | 204,070 | +32,790 obs (+19%) |
| **Unique Firms** | 14,638 | 14,627 | -11 firms (data cleaning) |
| **Complete Cases** | ~140,000 (est.) | 154,062 | +14,062 obs |
| **Countries** | 115 | 115 | No change |
| **Industries** | 24 (4-digit GICS) | 24 | No change |

### SBTi Commitment Data

| Measure | Original (2020) | Extended (2022) | Change |
|---------|----------------|-----------------|--------|
| **Data Source** | Single SBTi download | Three vintages (2022, 2023, 2025) | Multi-vintage validation |
| **Commitment Types** | All commitments + targets | **Commitments only** (targets excluded) | More conservative |
| **Date Reliability** | Not validated | **Cross-validated across vintages** | Higher quality |
| **Firms with Targets Set (2022)** | Included | **Excluded** | Prevents dating errors |
| **Total Commitments** | 1,484 (as of data collection) | 830 reliable commitments | More conservative count |
| **Latest Commitment Year** | 2020 | 2023 | +3 years of data |

**Critical Difference**: The original manuscript likely included firms with validated targets whose commitment dates may have been overwritten by target revision dates. The extended dataset uses only firms with active (non-validated) commitments that have stable, reliable commitment dates.

### Variable Updates

**New/Modified Variables**:
- All environmental and financial variables extended through 2022
- SBTi commitment measure now based on reliable multi-vintage data
- Lead structure allows capturing commitments through end of 2023 (using 2022 predictors → 2023 outcome)

**Instruments** (new for IV analysis):
- `peer_cdp_share_country_lag`: Country-level peer CDP adoption (t-1)
- `e_disc_lag2`: Historical environmental disclosure (t-2)
- `industry_incidents_excl_lag`: Industry incidents excluding own firm (t-1)

---

## Data Quality Assurances

### SBTi Commitment Data Quality

✓ **Multi-vintage validation**: Cross-checked commitment dates across 2022, 2023, and 2025 downloads
✓ **Conservative approach**: Excluded all firms with validated targets as of 2022 to prevent dating errors
✓ **Missing data handling**: Excluded all commitments with missing dates
✓ **Consistency checks**: Verified commitment dates don't change across vintages for same firms

### Panel Data Quality

✓ **Variable consistency**: All variables use same definitions and construction as 2007-2020 period
✓ **Outlier treatment**: Same winsorization approaches applied to 2021-2022 as earlier years
✓ **Missing data patterns**: Documented and handled consistently with earlier years
✓ **Firm identifiers**: Verified gvkey, factset_id, reprisk_id consistency across time

### Merge Quality

✓ **Multiple matching strategies**: Hierarchical matching by ISIN → LEI → name reduces match errors
✓ **Name cleaning**: Extensive standardization reduces false non-matches
✓ **Manual verification**: Ambiguous matches reviewed and verified
✓ **Duplicate handling**: Systematic approach to duplicates (earliest date selection)

---

## Implications for Analysis

### Strengths of Updated Dataset

1. **Longer time series**: 2007-2022 provides more variation, especially for recent commitments (2021-2023)
2. **Higher quality dependent variable**: Multi-vintage validation ensures accurate commitment timing
3. **Conservative approach reduces false positives**: Only firms with reliable commitment dates included
4. **Enables instrumental variables analysis**: Extended panel allows proper lag structure for instruments

### Limitations and Trade-offs

1. **Smaller SBTi sample**: 830 reliable commitments vs. 1,484 in original
   - **Why**: Excluded firms with validated targets (unreliable dating)
   - **Impact**: More conservative estimates, lower false positive rate

2. **Right-censoring**: Commitments made after 2023 not captured
   - **Why**: Using 2022 data with 1-year lead structure
   - **Impact**: Recent commitments (2024+) excluded

3. **Sample selection**: Only firms with complete data on all variables
   - **Why**: Required for regression analysis
   - **Impact**: 62.8% of population in full IV analysis (128,244 / 204,070)

4. **Target-setters excluded**: Firms that set validated targets before 2022 not in commitment sample
   - **Why**: Cannot reliably date their initial commitment due to target revision overwrites
   - **Impact**: Analysis focuses on commitment stage, not target validation stage

---

## Recommended Manuscript Updates

### Data and Methods Section

**Current text** (from 2020 manuscript):
> "To test our hypotheses, we assemble a unique panel dataset from various sources, including SBTi, CDP, Reprisk, Bloomberg, and Compustat. Specifically, our analyses include the population of Compustat and RepRisk firms from 2007 to 2020, comprising 171,280 firm-year observations, to examine variance in firm-year SBTi commitments."

**Proposed revision**:
> "To test our hypotheses, we assembled a unique panel dataset from various sources, including SBTi, CDP, RepRisk, Bloomberg, and Compustat. Our analyses include the population of Compustat and RepRisk firms from 2007 to 2022, comprising 204,070 firm-year observations across 14,627 unique firms, 115 countries, and 24 industries (four-digit GICS classification).
>
> **SBTi Commitment Data and Quality Assurance**
>
> We collected SBTi data at three time points (October 2022, April 2023, and April 2025) to ensure reliable identification of initial commitment dates. A critical data quality issue emerged: when firms revise their validated science-based targets, the SBTi database overwrites the original target date with the revised target date. This prevents reliable identification of when these firms originally committed to setting targets.
>
> To address this issue, we implemented a multi-vintage data approach. We retained only firms with active commitments (not yet validated as "targets set") whose commitment dates remain stable across data vintages. We excluded: (1) all firms with validated targets as of our October 2022 data collection (as their commitment dates may have been overwritten), and (2) all firms with missing commitment dates. This conservative approach ensures data quality and accurate temporal ordering but reduces our SBTi sample size.
>
> Our final dataset includes 830 reliable SBTi commitments from 2015 to 2023, with the following annual distribution: 2015 (4), 2016 (6), 2017 (1), 2018 (8), 2019 (17), 2020 (54), 2021 (245), 2022 (339), and 2023 (156). The concentration of commitments in 2021-2022 reflects the accelerating corporate engagement with science-based targets following the Paris Agreement and growing stakeholder pressure for climate action.
>
> We merged SBTi data with our panel using a hierarchical matching strategy: first by International Securities Identification Number (ISIN), then by Legal Entity Identifier (LEI), and finally by standardized company names. This multi-step approach maximizes match quality while minimizing false matches.
>
> **Dependent Variable Construction**
>
> Our dependent variable, `sbti_commitment_lead1`, captures whether a firm makes an SBTi commitment in year t+1. This lead structure allows us to use year t characteristics as predictors of year t+1 commitment decisions, ensuring proper temporal ordering and enabling instrumental variables estimation. Using 2022 data with a one-year lead, we can identify commitments made through the end of 2023.
>
> **Complete Case Analysis**
>
> Our regression analyses use complete-case samples with non-missing data for all variables. For baseline models, this yields 154,062 firm-year observations across 12,993 unique firms (75.5% of the population panel). For instrumental variables models requiring additional lagged instruments, the complete-case sample includes 128,244 observations across 10,970 firms (62.8% of population, 83.2% of baseline complete cases)."

### Additional Methods Detail (IV Analysis)

**New section to add**:
> "**Instrumental Variables Estimation**
>
> To address endogeneity concerns regarding CDP Supply Chain membership, we employ a two-stage least squares (2SLS) approach with three instruments that exploit distinct causal mechanisms:
>
> 1. **Country-level peer effects** (`peer_cdp_share_country_lag`): The proportion of firms in the same country (excluding the focal firm) that were CDP SC members in year t-1. This instrument captures country-level institutional pressures and peer diffusion effects.
>
> 2. **Historical environmental disclosure** (`e_disc_lag2`): The firm's environmental disclosure score lagged two years (t-2). This instrument exploits path dependence in voluntary environmental program participation while being predetermined relative to current SBTi commitment decisions (conditional on current disclosure levels).
>
> 3. **Industry environmental incidents** (`industry_incidents_excl_lag`): The sum of environmental incidents for all other firms in the same four-digit GICS industry (excluding the focal firm) in year t-1. This instrument captures industry-level legitimacy pressures while we control for the focal firm's own incidents to isolate spillover effects from direct effects.
>
> These instruments are conceptually distinct (maximum pairwise correlation r = 0.145) and exploit different sources of variation: peer effects (country-level), path dependence (firm historical behavior), and external shocks (industry incidents). We test instrument strength using first-stage F-statistics and joint weak instruments tests, and we test instrument validity using the Sargan-Hansen J-test for overidentification restrictions. All instruments are lagged to strengthen the exclusion restriction that they affect SBTi commitments only through CDP SC membership.
>
> Our instrumental variables strategy follows established practices in the corporate sustainability literature (Flammer et al., 2019; Kim & Davis, 2016) while addressing the specific challenges of our research context. The use of industry incidents (excluding own firm) as an instrument while controlling for own incidents as a covariate represents ideal research design: we account for firm-specific incident risk while isolating industry-level legitimacy spillovers that operate through CDP SC membership."

---

## Files and Locations

### Scripts (in `/scripts_2025/`)
1. `sbti_data_cleaning_2025.R`
2. `merging_sbti_2024.R`
3. `isolating_commitments_05_25.R`
4. `updating_cdp_scp.R`
5. `panel_extension.R`
6. `analysis_05_2025.R`

### Output Data (in `/cleaned_data/`)
- `panel_extended_complete.rds` - Population-level panel (204,070 obs, 2007-2022)
- `complete_data_2022.rds` - Complete cases for baseline analysis (154,062 obs)
- `complete_data_2022_instrument_country.rds` - IV analysis-ready (128,244 obs with all instruments)
- `sbti_commitments_reliable.rds` - Master list of reliable SBTi commitments (830 firms)
- `sbti_2024_merging_table.rds` - SBTi firm identifiers for merging (10,405 firms)

### Intermediate Files (in `/cleaned_data/`)
- Various output files from individual scripts (targets, commitments, matches)
- Documented in comments within each script

---

## References for Methodology

- **Multi-vintage data validation**: Similar to diff-in-diff event study approaches that require accurate event timing (e.g., Flammer, 2015)
- **Conservative sample construction**: Follows Larcker & Rusticus (2010) recommendations for IV analysis
- **Hierarchical matching**: Standard approach in firm-level panel construction (e.g., Kogan et al., 2017)
- **Lead dependent variable**: Common in corporate event prediction (e.g., corporate acquisition targets, bankruptcy prediction)

---

## Contact and Questions

For questions about data processing decisions or to request access to intermediate datasets:
- William Diebel (primary author)
- Robert D. Klassen (co-author)

---

*Document created: January 2026*
*Last updated: January 2026*
*Version: 1.0*
