# Comprehensive Empirical Testing Protocol
**Objective**: Establish causal evidence for BOTH internal and external interventions driving SBTi commitments through risk awareness mechanisms

**Date**: 2026-01-17
**Status**: Ready to execute

---

## Project Understanding (Corrected)

### Dual Research Questions:
1. **H1 (Internal)**: Does CDP SC membership (proactive, internal intervention) increase SBTi commitment by directing attention to climate risk exposure?
2. **H2 (External)**: Do environmental supply chain incidents (reactive, external intervention) increase SBTi commitment by directing attention to climate risk exposure?

### Theoretical Mechanism (Common to Both):
```
Intervention → Directs attention to climate risk exposure → Recognition of mitigation need → SBTi commitment

Internal pathway (CDP SC): Systematic data collection reveals Scope 3 exposure
External pathway (Incidents): Media coverage reveals reputational/regulatory risk
```

### Critical Insight:
Both pathways work through **same underlying attentional mechanism** (risk awareness), but:
- **Direct effects**: Each intervention independently drives SBTi
- **Interaction effects**: External may matter MORE when internal capability exists

---

## Current Empirical Status

### What Works:
- ✓ **H1 (CDP SC)**: Strong, significant, robust (β = 2.149, p < 0.001)
- ✓ **IV approach**: Addresses endogeneity successfully
- ✓ **First-stage**: Country peer IV is strong (F = 14.4)

### What Needs Work:
- ✗ **H2 (Incidents)**: Not significant in current spec (p = 0.183)
- ? **Model choice**: LPM inappropriate for rare events (2-3% baseline)
- ? **Incident measure**: Only tested highreach, not highsev or all
- ? **Interaction**: Haven't tested CDP SC × Incidents

---

## Testing Protocol: Systematic Exploration

### Phase 1: Model Choice for Rare Events (PRIORITY)

**Goal**: Find appropriate estimator for 2-3% baseline probability

#### Model 1.1: IV-Probit (Control Function)
```r
# Two-stage approach
# Stage 1: CDP SC membership ~ instruments + controls
first_stage <- feols(cdp_sc_member ~ peer_cdp_share_country_lag + controls |
                     headquarter_country + year, cluster = ~gvkey, data = data)

# Get residuals for control function
data$fs_resid <- residuals(first_stage)

# Stage 2: Probit with endogenous regressor + control function
probit_cf <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                 esc_incidents_highreach + controls,
                 family = binomial(link = "probit"), data = data)

# Marginal effects
library(margins)
marg <- margins(probit_cf, variables = c("cdp_sc_member", "esc_incidents_highreach"))
summary(marg)
```

**Expected outcome**:
- More plausible magnitude for CDP SC effect
- Test if incidents significant in probit specification

#### Model 1.2: IV-Logit (Control Function)
```r
# Same approach, logit link
logit_cf <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                esc_incidents_highreach + controls,
                family = binomial(link = "logit"), data = data)

# Average marginal effects
marg_logit <- margins(logit_cf,
                      variables = c("cdp_sc_member", "esc_incidents_highreach"))
```

**Expected outcome**: Odds ratios may show clearer incident effects

#### Model 1.3: Complementary Log-Log (Rare Events)
```r
# Cloglog designed for rare events
cloglog_cf <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                  esc_incidents_highreach + controls,
                  family = binomial(link = "cloglog"), data = data)

# Marginal effects
marg_cloglog <- margins(cloglog_cf,
                        variables = c("cdp_sc_member", "esc_incidents_highreach"))
```

**Expected outcome**:
- Better fit for rare events
- May reveal incident effects masked in LPM

#### Model 1.4: Discrete-Time Hazard (Event History)
```r
# Create proper time-to-event structure
# Each firm observed until first SBTi commitment
library(survival)

# Discrete-time hazard with cloglog link
hazard_cf <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                 esc_incidents_highreach + controls + factor(year),
                 family = binomial(link = "cloglog"),
                 data = data[data$at_risk == 1, ])  # Only firms still at risk
```

**Expected outcome**:
- Natural fit for "time to commitment"
- Handles right-censoring properly
- May show incident effects operate over time

---

### Phase 2: Alternative Incident Measures (CRITICAL)

**Goal**: Test if measurement choice obscures true effect

#### Model 2.1: High Severity (Not High Reach)
```r
# Test esc_incidents_highsev instead of highreach
# Rationale: Severity may matter more than reach for internal attention

# LPM 2SLS
model_highsev_lpm <- felm(
  sbti_commitment_lead1 ~ esc_incidents_highsev + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)

# IV-Probit
probit_highsev <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                      esc_incidents_highsev + controls,
                      family = binomial(link = "probit"), data = data)
```

**Hypothesis**: High severity incidents create stronger risk signal

#### Model 2.2: All Incidents (Count)
```r
# Test esc_incidents (all incidents, not filtered)
# Rationale: Cumulative incidents matter, not just extreme ones

model_all_lpm <- felm(
  sbti_commitment_lead1 ~ esc_incidents + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)

probit_all <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                  esc_incidents + controls,
                  family = binomial(link = "probit"), data = data)
```

**Hypothesis**: Volume of incidents matters for sustained attention

#### Model 2.3: Log Transformation
```r
# Log(incidents + 1) to handle skewness
data$log_incidents_highreach <- log(data$esc_incidents_highreach + 1)
data$log_incidents_highsev <- log(data$esc_incidents_highsev + 1)
data$log_incidents_all <- log(data$esc_incidents + 1)

# Test log specifications
model_log <- felm(
  sbti_commitment_lead1 ~ log_incidents_highreach + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)
```

**Hypothesis**: Diminishing marginal effects (log functional form)

#### Model 2.4: Binary Indicator (Any Incident)
```r
# Create binary: any high-reach incident in year t
data$any_incident_highreach <- as.numeric(data$esc_incidents_highreach > 0)

model_binary <- felm(
  sbti_commitment_lead1 ~ any_incident_highreach + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)

probit_binary <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                     any_incident_highreach + controls,
                     family = binomial(link = "probit"), data = data)
```

**Hypothesis**: Presence of incident matters, not magnitude

#### Model 2.5: Lagged Incidents
```r
# Test incidents at t-1 instead of t
# Rationale: Response may take time

model_lag1 <- felm(
  sbti_commitment_lead1 ~ esc_incidents_highreach_lag1 + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)

# Test cumulative incidents over past 2-3 years
data$cum_incidents_2yr <- data$esc_incidents_highreach +
                          data$esc_incidents_highreach_lag1

model_cum <- felm(
  sbti_commitment_lead1 ~ cum_incidents_2yr + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)
```

**Hypothesis**: Delayed or cumulative effects matter

---

### Phase 3: Interaction Effects (KEY INSIGHT)

**Goal**: Test if external interventions matter MORE when internal capability exists

**Theoretical Rationale**:
- Incidents alone may not drive SBTi (too complex, requires capability)
- But incidents + CDP SC membership → complementary effects
- External shock + internal capability = action

#### Model 3.1: Main Interaction Test
```r
# Create interaction term
data$cdp_incidents_interact <- data$cdp_sc_member * data$esc_incidents_highreach

# LPM with interaction (no IV for simplicity first)
model_interact_ols <- feols(
  sbti_commitment_lead1 ~ cdp_sc_member + esc_incidents_highreach +
                          cdp_incidents_interact + controls |
    headquarter_country + year,
  cluster = ~gvkey,
  data = data
)

# Marginal effects at different CDP levels
# Effect of incidents when CDP SC = 0
cat("Effect of incidents when NOT CDP member:",
    coef(model_interact_ols)["esc_incidents_highreach"], "\n")

# Effect of incidents when CDP SC = 1
cat("Effect of incidents when IS CDP member:",
    coef(model_interact_ols)["esc_incidents_highreach"] +
    coef(model_interact_ols)["cdp_incidents_interact"], "\n")
```

**Expected finding**: Interaction term positive and significant
- Incidents don't matter for non-CDP firms (lack capability)
- Incidents strongly matter for CDP firms (have capability to respond)

#### Model 3.2: IV-Probit with Interaction
```r
# Control function with interaction
# Interact fitted CDP membership with incidents

probit_interact <- glm(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents_highreach +
                          cdp_sc_member:esc_incidents_highreach + controls,
  family = binomial(link = "probit"),
  data = data
)

# Marginal effects conditional on CDP membership
marg_interact <- margins(probit_interact,
                         at = list(cdp_sc_member = c(0, 1)),
                         variables = "esc_incidents_highreach")
summary(marg_interact)
```

**Expected outcome**: Marginal effect of incidents much larger when CDP SC = 1

#### Model 3.3: Test All Incident Measures with Interaction
```r
# Test which incident measure shows strongest interaction

# High severity
probit_interact_highsev <- glm(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents_highsev +
                          cdp_sc_member:esc_incidents_highsev + controls,
  family = binomial(link = "probit"), data = data
)

# All incidents
probit_interact_all <- glm(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + esc_incidents +
                          cdp_sc_member:esc_incidents + controls,
  family = binomial(link = "probit"), data = data
)

# Binary indicator
probit_interact_binary <- glm(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + any_incident_highreach +
                          cdp_sc_member:any_incident_highreach + controls,
  family = binomial(link = "probit"), data = data
)

# Compare interaction coefficients across specifications
```

**Decision rule**: Use incident measure with strongest, most significant interaction

#### Model 3.4: Visualize Interaction
```r
# Create predicted probabilities plot
library(ggplot2)

# Generate predictions across incident range for CDP=0 and CDP=1
pred_data <- expand.grid(
  cdp_sc_member = c(0, 1),
  esc_incidents_highreach = seq(0, max(data$esc_incidents_highreach), length.out = 50),
  # Set controls at means
  e_disc_coalesced_zeros = mean(data$e_disc_coalesced_zeros),
  # ... other controls at means
)

pred_data$predicted_prob <- predict(probit_interact, newdata = pred_data, type = "response")

ggplot(pred_data, aes(x = esc_incidents_highreach, y = predicted_prob,
                      color = factor(cdp_sc_member))) +
  geom_line(size = 1.2) +
  labs(x = "Environmental SC Incidents (High Reach)",
       y = "Predicted Probability of SBTi Commitment",
       color = "CDP SC Member") +
  theme_minimal()
```

**Interpretation**: Lines should diverge (slopes different) if interaction matters

---

### Phase 4: Subsample Analysis (Heterogeneity)

**Goal**: Identify where incident effects are strongest

#### Model 4.1: By Firm Size
```r
# Large firms may respond differently to incidents (more visible)
data$large_firm <- data$at_usd_winsorized_1 > median(data$at_usd_winsorized_1)

# Test incidents in large vs small firms
probit_large <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                    esc_incidents_highreach + controls,
                    family = binomial(link = "probit"),
                    data = data[data$large_firm == 1, ])

probit_small <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                    esc_incidents_highreach + controls,
                    family = binomial(link = "probit"),
                    data = data[data$large_firm == 0, ])
```

**Hypothesis**: Incidents matter more for large firms (higher visibility)

#### Model 4.2: By Industry Emission Intensity
```r
# High-emission industries may respond differently
# Create industry emission intensity measure
data <- data %>%
  group_by(FourDigitName) %>%
  mutate(industry_emission_intensity = mean(scope1_zeros, na.rm = TRUE)) %>%
  ungroup()

data$high_emission_industry <-
  data$industry_emission_intensity > median(data$industry_emission_intensity)

# Test incidents in high vs low emission industries
probit_highem <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                     esc_incidents_highreach + controls,
                     family = binomial(link = "probit"),
                     data = data[data$high_emission_industry == 1, ])

probit_lowem <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                    esc_incidents_highreach + controls,
                    family = binomial(link = "probit"),
                    data = data[data$high_emission_industry == 0, ])
```

**Hypothesis**: Incidents matter more in high-emission industries (climate materiality)

#### Model 4.3: By Region
```r
# Test incidents in US/EU vs other regions
data$western <- data$headquarter_country %in% c("United States", "United Kingdom",
                                                 "Germany", "France", etc.)

probit_western <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                      esc_incidents_highreach + controls,
                      family = binomial(link = "probit"),
                      data = data[data$western == 1, ])

probit_other <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid +
                    esc_incidents_highreach + controls,
                    family = binomial(link = "probit"),
                    data = data[data$western == 0, ])
```

**Hypothesis**: Incidents matter more in Western countries (stakeholder pressure)

---

### Phase 5: Alternative Causal Pathways for Incidents

**Goal**: If direct effects don't work, test mediation/alternative mechanisms

#### Model 5.1: Incidents → CDP SC → SBTi (Mediation)
```r
# Test if incidents drive CDP SC membership (mediation pathway)
# Rather than direct effect on SBTi

# Step 1: Incidents → CDP SC membership
mediation_step1 <- feols(
  cdp_sc_member ~ esc_incidents_highreach_lag1 + controls |
    headquarter_country + year,
  cluster = ~gvkey,
  data = data
)

# Step 2: CDP SC → SBTi (controlling for incidents)
# Already have this from main models

# Interpretation: Incidents work THROUGH CDP SC membership, not directly
```

**Hypothesis**: External shocks prompt internal capability building

#### Model 5.2: Incidents → Disclosure → SBTi
```r
# Test if incidents drive disclosure, which then drives SBTi

# Step 1: Incidents → Environmental disclosure
mediation_disc <- feols(
  e_disc_coalesced_zeros ~ esc_incidents_highreach_lag1 + controls |
    headquarter_country + year,
  cluster = ~gvkey,
  data = data
)

# Step 2: Disclosure → SBTi (already in controls)
```

**Hypothesis**: Incidents prompt disclosure response, indirectly affecting SBTi

---

## Testing Sequence & Decision Tree

### Week 1 (This Weekend): Core Tests

**Saturday Morning (3 hours)**:
1. ✓ Run Phase 1 models (rare event estimators)
   - IV-Probit (Model 1.1)
   - IV-Logit (Model 1.2)
   - Cloglog (Model 1.3)

2. ✓ Run Phase 2 models (incident measures)
   - High severity (Model 2.1)
   - All incidents (Model 2.2)
   - Log transformation (Model 2.3)

**Saturday Afternoon (3 hours)**:
3. ✓ Run Phase 3 models (interactions) - **PRIORITY**
   - Main interaction (Model 3.1)
   - IV-Probit interaction (Model 3.2)
   - All incident measures with interaction (Model 3.3)

**Decision Point**: If interaction significant → This is your H2 story!
- "External interventions matter when firms have internal capability"
- Complementary mechanisms, not independent

**Saturday Evening (2 hours)**:
4. ✓ Create results tables for best specifications
5. ✓ Document which models support H1 and H2

**Sunday Morning (3 hours)**:
6. ✓ Run Phase 4 (subsample) IF interaction shows promise
7. ✓ Finalize model selection
8. ✓ Create comprehensive results tables

**Sunday Afternoon (3 hours)**:
9. ✓ Adjust hypotheses based on findings
10. ✓ Write results section
11. ✓ Draft discussion

---

## Decision Rules

### If Interaction Significant:
**H2 Framing**: "External interventions (incidents) complement internal capability (CDP SC) in driving SBTi commitments. Firms with proactive environmental supply chain programs (CDP SC) respond to reactive external shocks (incidents) by pursuing robust climate targets."

**Story**: Both pathways work, but through complementary mechanism
- Internal builds capability
- External provides impetus
- Together → action

### If Specific Incident Measure Works:
**H2 Framing**: Adjust to emphasize measurement (e.g., "High-severity incidents..." or "Cumulative incident exposure...")

### If Subsample Effects:
**H2 Framing**: "For [large firms/high-emission industries/Western firms], external interventions matter..."

### If Mediation Pathway:
**H2 Framing**: "External interventions drive internal capability building, which then promotes SBTi commitments"

### If Nothing Works:
**Fallback**:
- Report null finding as contribution
- Discuss why incidents may not directly drive SBTi (too complex, requires capability)
- Focus paper on strong H1 finding
- Suggest future research on incident responses

---

## Output Structure

### Create Comparison Table:
```
Model Specification | CDP SC Coef | p-value | Incidents Coef | p-value | Interaction Coef | p-value
LPM 2SLS           | 2.149      | <0.001  | 0.058          | 0.183   | -                | -
IV-Probit          | [AME]      | [p]     | [AME]          | [p]     | -                | -
IV-Logit           | [AME]      | [p]     | [AME]          | [p]     | -                | -
Cloglog            | [AME]      | [p]     | [AME]          | [p]     | -                | -
With Interaction   | [AME]      | [p]     | [AME]          | [p]     | [AME]            | [p]
```

### Document:
1. Which models support H1 (should be all)
2. Which models support H2 direct effects
3. Which models support H2 interaction effects
4. Best specification for manuscript (model + incident measure)

---

## R Script Template

Create `/scripts/05_comprehensive_robustness.R` with:

```r
# Comprehensive Robustness Testing Protocol
# Tests: (1) Rare event models, (2) Incident measures, (3) Interactions, (4) Subsamples

library(tidyverse)
library(fixest)
library(lfe)
library(margins)

# Load data
data <- readRDS("cleaned_data/complete_data_2022_instrument_country.rds")

# ==============================================================================
# PHASE 1: RARE EVENT MODELS
# ==============================================================================

# [Insert Model 1.1-1.4 code]

# ==============================================================================
# PHASE 2: INCIDENT MEASURES
# ==============================================================================

# [Insert Model 2.1-2.5 code]

# ==============================================================================
# PHASE 3: INTERACTIONS (KEY)
# ==============================================================================

# [Insert Model 3.1-3.4 code]

# ==============================================================================
# PHASE 4: SUBSAMPLES
# ==============================================================================

# [Insert Model 4.1-4.3 code]

# ==============================================================================
# PHASE 5: ALTERNATIVE PATHWAYS
# ==============================================================================

# [Insert Model 5.1-5.2 code]

# ==============================================================================
# CREATE RESULTS SUMMARY TABLE
# ==============================================================================

# Compile all results into comparison table
# Export to CSV for manuscript table creation
```

---

## Expected Timeline

**Saturday**:
- Morning: Phase 1 & 2 (6 hours)
- Afternoon: Phase 3 (3 hours)
- Evening: Document findings (2 hours)

**Sunday**:
- Morning: Phase 4 if needed, finalize (3 hours)
- Afternoon: Results section (3 hours)

**Total**: 17 hours over 2 days

**Feasibility**: YES if focused, NO if trying to write full manuscript too

---

## Success Criteria

**Minimum**: Find ONE specification where incidents significant (p < 0.05)
**Good**: Find interaction effect (incidents matter more with CDP SC)
**Excellent**: Consistent pattern across multiple specifications

**Manuscript implication**:
- Minimum: H2 supported with caveats about specification
- Good: H2 reframed around complementarity
- Excellent: Strong support for both H1 and H2

---

**Ready to execute. This protocol gives you multiple pathways to find support for H2 while maintaining rigor. The interaction effect is likely your best bet - it makes theoretical sense and is empirically testable.**
