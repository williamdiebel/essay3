# Data Dictionary

This directory contains comprehensive documentation of all variables and datasets used in the instrumental variables analysis of SBTi (Science Based Targets initiative) commitment.

## Contents

- **[variables.md](variables.md)**: Detailed descriptions of all variables used in the analysis, including:
  - Dependent variable (sbti_commitment_lead1)
  - Key explanatory variables (cdp_sc_member, environmental incidents)
  - Instrumental variables (peer share measures at industry and country levels)
  - Control variables (financial, environmental disclosure, emissions)
  - Identifiers and fixed effects variables

- **[datasets.md](datasets.md)**: Documentation of all datasets, including:
  - Dataset hierarchy and relationships
  - Sample sizes and composition
  - Variable lists for each dataset
  - Data flow for analysis
  - Construction procedures

## Quick Reference

### Main Datasets (in order of data flow)

1. **panel_extended_complete.rds** - Population-level (204,070 obs, 14,627 firms)
2. **panel_extended_instrument.rds** - Population + industry instruments (204,070 obs)
3. **complete_data_2022.rds** - Complete cases only (154,062 obs, 12,993 firms)
4. **complete_data_2022_instrument.rds** - Complete cases + industry instruments (154,062 obs)
5. **complete_data_2022_instrument_country.rds** - Complete cases + industry & country instruments (154,062 obs)

### Key Variables

**Dependent Variable**: sbti_commitment_lead1

**Endogenous Variable**: cdp_sc_member

**Instruments**:
- Industry-level: peer_cdp_share_lag
- Country-level: peer_cdp_share_country_lag

**Exogenous Explanatory Variables**:
- esc_incidents (general)
- esc_incidents_highreach (reputation measure)
- esc_incidents_highsev (stakeholder harm measure)

## Analysis Overview

This project uses Two-Stage Least Squares (2SLS) regression to estimate the causal effect of CDP Supply Chain membership (cdp_sc_member) on future SBTi commitment (sbti_commitment_lead1), addressing the endogeneity of voluntary CDP participation.

**First Stage**: Instrument cdp_sc_member using peer adoption rates (industry and/or country level)

**Second Stage**: Estimate effect of instrumented cdp_sc_member on sbti_commitment_lead1

**Diagnostics**:
- F-test for weak instruments (first stage)
- Sargan-Hansen J-test for overidentification (when using 2+ instruments)
- Wu-Hausman test for endogeneity

## For More Information

See the main project README at the repository root for analysis scripts and workflow.
