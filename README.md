# Instrumental Variables Analysis: CDP Supply Chain Membership and SBTi Commitment

This repository contains code for conducting instrumental variables analysis examining the causal effect of CDP Supply Chain membership on Science Based Targets initiative (SBTi) commitment.

## Research Question

Does CDP Supply Chain membership causally affect firms' likelihood of making SBTi commitments, accounting for the endogeneity of voluntary CDP participation?

## Methodology

**Approach**: Two-Stage Least Squares (2SLS) regression with instrumental variables

**Endogenous Variable**: CDP Supply Chain membership (cdp_sc_member)

**Instruments**:
- Industry-level peer share: Proportion of industry peers who are CDP SC members
- Country-level peer share: Proportion of country peers who are CDP SC members

**Dependent Variable**: SBTi commitment in year t+1 (sbti_commitment_lead1)

**Key Covariates**: Environmental supply chain incidents (exogenous), environmental disclosure scores, emissions data, financial controls

## Repository Structure

```
essay3/
├── README.md                           # This file
├── data_dictionary/                    # Variable and dataset documentation
│   ├── README.md                       # Data dictionary overview
│   ├── variables.md                    # All variable definitions
│   └── datasets.md                     # Dataset descriptions and relationships
├── cleaned_data/                       # Data files (.rds format)
│   ├── panel_extended_complete.rds     # Population-level data (204,070 obs)
│   ├── panel_extended_instrument.rds   # Population + industry instruments
│   ├── complete_data_2022.rds          # Complete cases (154,062 obs)
│   └── complete_data_2022_instrument_country.rds  # Analysis dataset (created by script 01)
└── scripts/                            # Analysis scripts
    ├── 01_create_instrument_country.R  # Generate country-level instruments
    └── 02_run_2sls_analysis.R          # Run 2SLS regressions with diagnostics
```

## Workflow

### Step 1: Data Preparation

Before running the analysis, ensure the following data files are placed in the `cleaned_data/` directory:

1. **panel_extended_complete.rds** - Population-level dataset (204,070 firm-year observations, 14,627 firms)
2. **panel_extended_instrument.rds** - Population-level with industry-level peer share instruments
3. **complete_data_2022.rds** - Complete-cases dataset (154,062 observations, 12,993 firms)

### Step 2: Create Country-Level Instruments

Run the first script to generate the analysis dataset with both industry and country-level instruments:

```r
source("scripts/01_create_instrument_country.R")
```

**Output**: `complete_data_2022_instrument_country.rds` (154,062 observations with 4 instruments)

**What it does**:
- Calculates country-level peer share instruments (current and lagged)
- Merges with complete-cases data
- Validates instrument quality and coverage
- Saves analysis-ready dataset

### Step 3: Run 2SLS Analysis

Run the second script to estimate 2SLS models with comprehensive diagnostics:

```r
source("scripts/02_run_2sls_analysis.R")
```

**What it does**:
- **Part 1**: First-stage regressions testing instrument relevance (F-tests)
- **Part 2**: Second-stage 2SLS estimates with multiple instrument specifications
- **Part 3**: Alternative specifications using different environmental incident measures
- **Part 4**: Alternative fixed effects specifications (country, industry, firm)
- **Part 5**: Diagnostic tests (weak instruments, Sargan-Hansen J-test, Wu-Hausman)
- **Part 6**: Baseline OLS models for comparison

## Key Variables

For complete variable descriptions, see `data_dictionary/variables.md`

### Dependent Variable
- **sbti_commitment_lead1**: SBTi commitment in year t+1 (binary)

### Endogenous Explanatory Variable
- **cdp_sc_member**: CDP Supply Chain membership in year t (binary)

### Instruments
- **peer_cdp_share_lag**: Industry-level peer CDP adoption rate (t-1)
- **peer_cdp_share_country_lag**: Country-level peer CDP adoption rate (t-1)

### Exogenous Explanatory Variables
- **esc_incidents**: Total environmental supply chain incidents
- **esc_incidents_highreach**: High-reach incidents (reputational harm)
- **esc_incidents_highsev**: High-severity incidents (stakeholder harm)

### Control Variables
- Environmental disclosure scores (e_disc_coalesced_zeros, e_disc_missing)
- Emissions data (scope1_zeros, scope1_missing)
- Financial controls (ROA, total assets, leverage)

### Fixed Effects
- **headquarter_country**: Country fixed effects
- **FourDigitName**: 4-digit GICS industry fixed effects
- **year**: Year fixed effects
- **gvkey**: Firm fixed effects

## Diagnostic Tests

The analysis includes comprehensive diagnostic tests:

1. **First-Stage F-Test**: Tests for weak instruments (rule of thumb: F > 10)
2. **Sargan-Hansen J-Test**: Tests overidentification restrictions when using multiple instruments
3. **Wu-Hausman Test**: Tests for endogeneity of CDP Supply Chain membership

## Data Sources

- **Compustat**: Financial data and firm identifiers
- **Bloomberg ESG**: Environmental disclosure scores and emissions data
- **RepRisk**: Environmental supply chain incidents data
- **CDP**: Supply Chain member lists
- **SBTi**: Science Based Targets commitments
- **GICS**: Industry classifications

## Dependencies

Required R packages:
```r
install.packages(c("tidyverse", "data.table", "fixest", "lfe", "AER", "sandwich", "lmtest"))
```

## Notes

- All standard errors are clustered at the firm level (gvkey)
- Financial variables are winsorized at the 1st and 99th percentiles
- Log transformations use log(x + 1) to handle zeros
- Missing data patterns are controlled with indicator variables
- Peer share instruments exclude the focal firm to avoid mechanical correlation

## Contact

For questions about the analysis or data, please refer to the comprehensive documentation in `data_dictionary/`.

## License

See LICENSE file for details.
