# Variable Dictionary

This document describes all variables used in the instrumental variables analysis of SBTi commitment.

## Dependent Variable

### sbti_commitment_lead1
- **Type**: Binary (0/1)
- **Description**: Leading indicator capturing whether the firm has made an SBTi (Science Based Targets initiative) commitment in year t+1
- **Use**: Primary dependent variable across all analyses

## Key Explanatory Variables

### cdp_sc_member
- **Type**: Binary (0/1)
- **Description**: Indicates whether a firm is a CDP Supply Chain member in the current period (year t)
- **Endogeneity**: This variable is endogenous because firms self-select into CDP Supply Chain membership
- **Treatment**: Instrumented in 2SLS regression using peer share instruments

### esc_incidents
- **Type**: Count (integer)
- **Description**: Number of environmental supply chain incidents associated with the focal firm in the given year
- **Source**: RepRisk data
- **Exogeneity**: Considered exogenous as incidents result from third-party coverage
- **Use**: Alternative specification in main analysis

### esc_incidents_highreach
- **Type**: Count (integer)
- **Description**: Number of high-reach environmental supply chain incidents associated with the focal firm in the given year
- **Source**: RepRisk data
- **Details**: High reach incidents appear in prominent global outlets (e.g., The New York Times, BBC News)
- **Interpretation**: Captures magnitude of harms to firms' reputations
- **Exogeneity**: Considered exogenous as incidents result from third-party coverage
- **Use**: Alternative specification in main analysis

### esc_incidents_highsev
- **Type**: Count (integer)
- **Description**: Number of high-severity environmental supply chain incidents associated with the focal firm in the given year
- **Source**: RepRisk data
- **Details**: Severity determined by magnitude, scale, and cause of incident (low/medium/high)
- **Interpretation**: Captures magnitude of harms to stakeholders
- **Exogeneity**: Considered exogenous as incidents result from third-party coverage
- **Use**: Alternative specification in main analysis

## Instrumental Variables

### peer_cdp_share
- **Type**: Continuous (0-1)
- **Description**: Industry-level peer share instrument (current year)
- **Calculation**: Ratio of CDP Supply Chain members in the firm's industry (4-digit GICS) to total firms in that industry, excluding the focal firm
- **Formula**: (sum(cdp_sc_member) - focal_firm_cdp_sc_member) / (N_industry_firms - 1)
- **Level**: Industry-year (by FourDigitName and year)
- **Use**: Alternative instrument specification

### peer_cdp_share_lag
- **Type**: Continuous (0-1)
- **Description**: One-year lagged version of industry-level peer share instrument
- **Calculation**: Previous year's value of peer_cdp_share for each firm
- **Use**: Primary instrument in 2SLS first stage

### peer_cdp_share_country
- **Type**: Continuous (0-1)
- **Description**: Country-level peer share instrument (current year)
- **Calculation**: Ratio of CDP Supply Chain members in the firm's country to total firms in that country, excluding the focal firm
- **Formula**: (sum(cdp_sc_member) - focal_firm_cdp_sc_member) / (N_country_firms - 1)
- **Level**: Country-year (by headquarter_country and year)
- **Use**: Alternative instrument specification

### peer_cdp_share_country_lag
- **Type**: Continuous (0-1)
- **Description**: One-year lagged version of country-level peer share instrument
- **Calculation**: Previous year's value of peer_cdp_share_country for each firm
- **Use**: Alternative instrument in 2SLS first stage

## Control Variables

### Environmental Disclosure

#### e_disc_coalesced_zeros
- **Type**: Continuous (0-100)
- **Description**: Bloomberg environmental disclosure score for each firm-year
- **Source**: Bloomberg ESG data
- **Details**: Zeros indicate either missing data or valid zero scores
- **Use**: Control variable for environmental transparency

#### e_disc_missing
- **Type**: Binary (0/1)
- **Description**: Indicator for missing environmental disclosure score from Bloomberg
- **Use**: Control for missing data patterns

### Emissions

#### scope1_zeros
- **Type**: Continuous (non-negative)
- **Description**: Scope 1 greenhouse gas emissions associated with each firm-year
- **Source**: Bloomberg ESG data
- **Units**: Metric tons CO2 equivalent
- **Details**: Zeros indicate either missing values or sufficiently small values
- **Transformation**: Used as log(scope1_zeros + 1) in regressions
- **Use**: Control variable for emissions intensity

#### scope1_missing
- **Type**: Binary (0/1)
- **Description**: Indicator for missing Scope 1 emissions data
- **Use**: Control for missing data patterns

### Financial Performance

#### roa_oibdp_w1_at_w1
- **Type**: Continuous (ratio)
- **Description**: Return on assets for each firm-year
- **Calculation**: Operating income before depreciation / Total assets
- **Winsorization**: Both numerator and denominator winsorized at 1st and 99th percentiles
- **Use**: Control variable for financial performance

#### at_usd_winsorized_1_log
- **Type**: Continuous (log scale)
- **Description**: Natural log of total assets
- **Source**: Compustat
- **Currency**: USD
- **Winsorization**: Winsorized at 1st and 99th percentiles before log transformation
- **Use**: Control variable for firm size

#### tll_lt_w1_at_w1
- **Type**: Continuous (ratio)
- **Description**: Financial leverage ratio for each firm-year
- **Calculation**: Total liabilities / Total assets
- **Winsorization**: Both numerator and denominator winsorized at 1st and 99th percentiles
- **Use**: Control variable for financial risk

## Identifiers and Fixed Effects Variables

### gvkey
- **Type**: Character/Integer
- **Description**: Unique firm identifier from Compustat
- **Use**: Panel identifier, clustering variable in regressions

### year
- **Type**: Integer
- **Description**: Calendar year of observation
- **Range**: Typically 2007-2022
- **Use**: Time dimension, year fixed effects

### headquarter_country
- **Type**: Character
- **Description**: Country where the firm's headquarters is located
- **Use**: Country fixed effects in regressions

### FourDigitName
- **Type**: Character
- **Description**: Four-digit GICS (Global Industry Classification Standard) industry classification
- **Use**: Industry fixed effects in regressions, industry-level peer share calculation

### conm
- **Type**: Character
- **Description**: Company name from Compustat
- **Use**: Matching and identification purposes

### reprisk_id
- **Type**: Character/Integer
- **Description**: Unique identifier from RepRisk database
- **Use**: Linking to environmental incident data

### factset_id
- **Type**: Character
- **Description**: Unique identifier from FactSet database
- **Use**: Cross-database linking

## Notes on Variable Construction

1. **Winsorization**: Financial variables are winsorized at the 1st and 99th percentiles to reduce the influence of extreme outliers

2. **Log Transformations**: Applied to highly skewed variables (scope1_zeros, total assets) using log(x + 1) to handle zeros

3. **Missing Data Indicators**: Separate binary indicators created for variables with substantial missing data patterns (e_disc_missing, scope1_missing)

4. **Peer Share Instruments**: Calculated excluding the focal firm to avoid mechanical correlation

5. **Temporal Structure**:
   - Most variables are contemporaneous (year t)
   - Dependent variable is forward-looking (year t+1)
   - Lagged instruments use year t-1
