# Dataset Documentation

This document describes all datasets used in the instrumental variables analysis and their relationships.

## Dataset Hierarchy

```
panel_extended_complete.rds (Population-level)
    ↓
panel_extended_instrument.rds (+ Industry-level instruments)
    ↓
complete_data_2022.rds (Complete cases only)
    ↓
complete_data_2022_instrument.rds (Complete cases + Industry instruments)
    ↓
complete_data_2022_instrument_country.rds (Complete cases + Industry & Country instruments)
```

## Dataset Descriptions

### 1. panel_extended_complete.rds

**Purpose**: Population-level dataset serving as the foundation for all analyses

**Observations**: 204,070 firm-year observations

**Firms**: 14,627 unique firms

**Time Period**: 2007-2022

**Key Features**:
- Includes all firms regardless of data completeness
- Contains core variables: firm identifiers, industry classifications, financial data, environmental disclosure, emissions data
- Serves as the base for creating peer share instruments

**Use**:
- Input for generating peer share instruments
- Reference population for instrument calculations

---

### 2. panel_extended_instrument.rds

**Purpose**: Population-level dataset with industry-level peer share instruments added

**Observations**: 204,070 firm-year observations

**Firms**: 14,627 unique firms

**Time Period**: 2007-2022

**Additional Variables** (compared to panel_extended_complete.rds):
- `peer_cdp_share`: Industry-level CDP Supply Chain member share (current year)
- `peer_cdp_share_lag`: Industry-level CDP Supply Chain member share (lagged one year)

**Instrument Calculation**:
- Industry defined by 4-digit GICS classification (FourDigitName)
- Peer share excludes focal firm to avoid mechanical correlation
- Calculated separately for each industry-year combination

**Use**:
- Source for creating complete_data_2022_instrument.rds
- Source for creating complete_data_2022_instrument_country.rds

---

### 3. complete_data_2022.rds

**Purpose**: Analysis-ready dataset containing only observations with complete data for all key variables

**Observations**: 154,062 firm-year observations

**Firms**: 12,993 unique firms

**Time Period**: 2007-2022 (subset of panel_extended_complete.rds with complete data)

**Filtering Criteria**:
- Complete data for all variables included in the analysis
- No missing values for key explanatory variables, controls, or dependent variable

**Variables Included**:
- Dependent variable: sbti_commitment_lead1
- Explanatory variables: cdp_sc_member, esc_incidents, esc_incidents_highreach, esc_incidents_highsev
- Controls: e_disc_coalesced_zeros, e_disc_missing, scope1_zeros, scope1_missing, roa_oibdp_w1_at_w1, at_usd_winsorized_1_log, tll_lt_w1_at_w1
- Identifiers: gvkey, year, headquarter_country, FourDigitName, conm, reprisk_id, factset_id

**Use**:
- Baseline OLS/fixed effects regressions (non-IV models)
- Determines which observations to retain when adding instruments

---

### 4. complete_data_2022_instrument.rds

**Purpose**: Complete-cases dataset with industry-level peer share instruments

**Observations**: 154,062 firm-year observations (matches complete_data_2022.rds)

**Firms**: 12,993 unique firms

**Construction**:
1. Start with panel_extended_instrument.rds
2. Retain only observations that appear in complete_data_2022.rds
3. Result: Complete-cases data with industry-level instruments

**Additional Variables** (compared to complete_data_2022.rds):
- `peer_cdp_share`: Industry-level peer share (current year)
- `peer_cdp_share_lag`: Industry-level peer share (lagged one year)

**Use**:
- 2SLS regressions using industry-level instruments
- First-stage regressions to test instrument relevance
- Baseline IV specifications

---

### 5. complete_data_2022_instrument_country.rds

**Purpose**: Complete-cases dataset with both industry-level AND country-level peer share instruments

**Observations**: 154,062 firm-year observations (matches complete_data_2022.rds)

**Firms**: 12,993 unique firms

**Construction**:
1. Start with panel_extended_instrument.rds (has population-level data)
2. Create country-level peer share instruments:
   - `peer_cdp_share_country`: Country-level peer share (current year)
   - `peer_cdp_share_country_lag`: Country-level peer share (lagged one year)
3. Retain only observations that appear in complete_data_2022.rds

**All Variables**:
- All variables from complete_data_2022.rds
- Industry instruments: peer_cdp_share, peer_cdp_share_lag
- Country instruments: peer_cdp_share_country, peer_cdp_share_country_lag

**Instrument Details**:

*Industry-level instruments*:
- Aggregation: 4-digit GICS industry (FourDigitName) × year
- Excludes focal firm from calculation

*Country-level instruments*:
- Aggregation: Headquarter country × year
- Excludes focal firm from calculation

**Use**:
- 2SLS regressions using country-level instruments
- 2SLS regressions using multiple instruments (industry + country)
- Overidentification tests (Sargan-Hansen J-test)
- Robustness checks with alternative instrument specifications

---

## Data Flow for Analysis

### Creating the Country-Level Instrument Dataset

**Script**: `01_create_instrument_country.R`

**Steps**:
1. Load panel_extended_instrument.rds (population-level with industry instruments)
2. Calculate country-level peer share instruments:
   ```r
   peer_cdp_share_country =
     (sum(cdp_sc_member in country-year) - focal_firm_cdp_sc_member) /
     (N_firms_in_country_year - 1)
   ```
3. Create lagged version (peer_cdp_share_country_lag) by firm
4. Merge with complete_data_2022.rds to retain only complete cases
5. Save as complete_data_2022_instrument_country.rds

### Running 2SLS Analysis

**Script**: `02_run_2sls_analysis.R`

**Steps**:
1. Load complete_data_2022_instrument_country.rds
2. First-stage regression: cdp_sc_member ~ instruments + controls + FE
3. Test instrument strength (F-statistic)
4. Second-stage regression: sbti_commitment_lead1 ~ fitted(cdp_sc_member) + controls + FE
5. Run diagnostic tests:
   - First-stage F-test (weak instrument test)
   - Sargan-Hansen J-test (overidentification test, if using 2+ instruments)
   - Wu-Hausman endogeneity test
6. Compare specifications:
   - Different incident measures (esc_incidents, esc_incidents_highreach, esc_incidents_highsev)
   - Different instrument sets (industry only, country only, both)
   - Different fixed effects (country, industry, firm)

---

## Variable Overlap Across Datasets

| Variable | panel_extended_complete | panel_extended_instrument | complete_data_2022 | complete_data_2022_instrument | complete_data_2022_instrument_country |
|----------|------------------------|---------------------------|-------------------|------------------------------|--------------------------------------|
| Core variables (DV, covariates, identifiers) | ✓ | ✓ | ✓ | ✓ | ✓ |
| peer_cdp_share | ✗ | ✓ | ✗ | ✓ | ✓ |
| peer_cdp_share_lag | ✗ | ✓ | ✗ | ✓ | ✓ |
| peer_cdp_share_country | ✗ | ✗ | ✗ | ✗ | ✓ |
| peer_cdp_share_country_lag | ✗ | ✗ | ✗ | ✗ | ✓ |

---

## Notes

1. **Sample Attrition**: The move from population-level (204,070 obs) to complete-cases (154,062 obs) represents a loss of ~24.5% of observations due to missing data

2. **Balanced vs Unbalanced Panel**: All datasets are unbalanced panels (firms may not appear in all years)

3. **Instrument Validity**:
   - Relevance tested via first-stage F-statistic (should be > 10)
   - Exogeneity assumed based on theoretical arguments (peer behavior plausibly excludes own behavior)
   - Overidentification tested via Sargan-Hansen J-test when using multiple instruments

4. **Missing Data Treatment**:
   - Missing values for continuous variables are handled with indicator variables (e_disc_missing, scope1_missing)
   - Zero values in scope1_zeros and e_disc_coalesced_zeros may represent true zeros or missing data

5. **File Locations**:
   - All .rds files stored in: `/home/user/essay3/cleaned_data/`
   - Scripts stored in: `/home/user/essay3/scripts/`
