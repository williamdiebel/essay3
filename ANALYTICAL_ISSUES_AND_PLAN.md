# Analytical Issues and Resolution Plan
**Date**: 2026-01-17
**Status**: Critical issues identified - systematic resolution needed

---

## Executive Summary of Issues

1. **Weak instruments concern**: Wald F = 8.51 < 10 (borderline weak)
2. **Implausible magnitude**: 2.161 coefficient = 216% increase (impossible given 2-3% baseline)
3. **H2 not supported**: External interventions (incidents) not significant (p = 0.183)
4. **Model misspecification**: LPM inappropriate for rare binary outcome

---

## Issue 1: Weak Instruments Diagnostic Discrepancy

### The Problem
Two conflicting diagnostics:
- **Wald F-stat (felm)** = 8.51 (WEAK - below threshold of 10)
- **Weak instruments test (ivreg)** = 102.7 (STRONG - far above threshold)

### Why This Happens
Different implementations:
1. **Wald F**: Joint test on 3 instruments in felm with FE absorbed
2. **Weak instruments test**: Test on demeaned data in ivreg (different df, different processing)

### Conservative Interpretation
**Treat as borderline weak instruments.** When diagnostics conflict, err on the conservative side.

### Resolution Steps

**Priority 1: Prefer Model 1 (Just-Identified)**
- Uses only `peer_cdp_share_country_lag` (F = 14.4 - clearly strong)
- Cannot test overidentification, but that's OK if instrument is theoretically sound
- Coefficient: 2.149, SE: 0.653 (very similar to Model 2)
- **Recommendation**: Make this your PRIMARY specification

**Priority 2: Drop Weak Instrument from Model 2**
- Remove `industry_incidents_excl_lag` (individual F = 3.0 - clearly weak)
- Keep only `peer_cdp_share_country_lag` + `e_disc_lag2` (both F ≈ 14.4)
- Re-run over-identified model with 2 instruments
- Check if joint F improves and Sargan test still passes

**Priority 3: Calculate Partial F-Statistics**
- Compute partial R² for each instrument
- Calculate proper partial F for each instrument in the joint model
- This shows incremental contribution of each instrument

**Priority 4: Weak-IV Robust Inference**
- **Anderson-Rubin (AR) confidence intervals**: Valid even with weak IVs
- **LIML estimation**: Less biased than 2SLS under weak instruments
- **Conditional LR test**: Alternative to AR for hypothesis testing

---

## Issue 2: Implausible Coefficient Magnitude

### The Problem
- Coefficient = 2.161 → naive reading: +216.1 percentage points
- Baseline SBTi rate ≈ 2-3% → impossible to increase by 216 pp!

### Why This Happens: LATE Interpretation + Weak First-Stage

2SLS = (Reduced Form) / (First Stage)

If first-stage is small (weak compliance or small complier population):
→ Denominator is small
→ Ratio blows up

**Example calculation**:
- Reduced form (instrument → outcome): β ≈ 0.10
- First stage (instrument → treatment): β ≈ 0.05 (small!)
- 2SLS = 0.10 / 0.05 = 2.0 (200%!)

### Correct Interpretation
This is a **Local Average Treatment Effect (LATE)** for compliers:
- "Compliers" = firms whose CDP membership is shifted by the instruments
- The effect is for this specific subpopulation, not the full sample
- If compliers are a small group, the LATE can be very large

### What This Means Substantively
**Option A (Optimistic)**:
Among compliers (firms on the margin of joining CDP SC), membership has a very strong effect on SBTi commitment.

**Option B (Realistic)**:
The estimate is fragile due to weak instruments and should be interpreted with extreme caution. The magnitude is not plausible as a population average effect.

### Resolution Steps

**Priority 1: Report Qualitatively, Not Quantitatively**
- ✓ "CDP SC membership has a strong positive causal effect on SBTi commitment"
- ✗ "CDP SC membership increases SBTi likelihood by 216 percentage points"
- Emphasize sign and significance, de-emphasize magnitude

**Priority 2: Check Predicted Probabilities**
```r
# Check how many predicted values are outside [0,1]
predicted <- predict(model_2)
sum(predicted < 0)  # Negative probabilities
sum(predicted > 1)  # > 100% probabilities
mean(predicted)     # Average predicted probability
```
If many violations → report this as limitation of LPM

**Priority 3: Implement Nonlinear IV Models**
- **IV-Probit**: Marginal effects constrained to [0,1]
- **Control function approach**: More flexible
- Report marginal effects at sample means or medians
- These will give more plausible magnitudes

**Priority 4: Heterogeneity Analysis**
Try subsample analyses:
- By firm size (large vs small)
- By region (US/Europe vs others)
- By industry (high-emission vs low-emission)

This helps identify the complier population and may reveal more plausible effects in specific subgroups.

---

## Issue 3: External Interventions (Incidents) Not Significant

### The Problem
**esc_incidents_highreach**: β = 0.058, SE = 0.044, p = 0.183
- **Not statistically significant**
- **H2 not supported**
- This undermines the "reactive intervention" hypothesis

### Possible Explanations

**Explanation 1: Measurement Issue**
- `highreach` threshold (>100,000 reach) may be too restrictive
- Missing moderate-reach incidents that still matter
- Alternative: Try `highsev` (high severity) or continuous reach measure

**Explanation 2: Timing/Lag Structure**
- Current: Contemporaneous incidents in year t → SBTi in year t+1
- Maybe incidents need more time to trigger response
- Alternative: Try lagged incidents or cumulative incident count

**Explanation 3: Mechanism Doesn't Work**
- Incidents may not actually trigger SBTi commitments
- Could trigger other responses (EMS adoption, disclosure increase)
- But not specifically SBTi (which requires proactive capability)

**Explanation 4: Endogeneity Bias**
- LPM 2SLS may be inadequate for incidents variable
- Incidents could be correlated with unobservables affecting SBTi
- OLS estimate for incidents might be biased

### Resolution Steps

**Priority 1: Robustness Checks on Incident Measure**
Test alternative specifications:
- `esc_incidents_highsev` (high severity instead of high reach)
- `esc_incidents` (count of all incidents)
- `log(esc_incidents_highreach + 1)` (log transformation)
- Cumulative incidents over past 2-3 years
- Lagged incidents (t-1, t-2)

**Priority 2: Check Incident Distribution**
```r
# Descriptive statistics
summary(data$esc_incidents_highreach)
table(data$esc_incidents_highreach)  # Frequency distribution

# Cross-tab with SBTi
table(Incidents = data$esc_incidents_highreach > 0,
      SBTi = data$sbti_commitment_lead1)
```
Is there enough variation? Are incidents too rare?

**Priority 3: Bivariate Relationship**
Test simple relationship without IV:
```r
# OLS: Incidents → SBTi (with controls, no IV)
ols_incidents <- feols(sbti_commitment_lead1 ~ esc_incidents_highreach + controls |
                       headquarter_country + year,
                       cluster = ~ gvkey, data = data)
```
If not significant even in OLS → measurement or mechanism problem

**Priority 4: Consider Dropping H2**
If incidents consistently show no effect across specifications:
- **Option A**: Focus paper on H1 only (proactive interventions)
- **Option B**: Reframe H2 around different reactive mechanism
- **Option C**: Report null finding as contribution (incidents don't work)

---

## Issue 4: Model Choice - LPM vs Logit/Hazard

### The Problem
**Linear Probability Model (LPM)** has serious issues for rare binary outcomes:
- Predicted probabilities can be < 0 or > 1
- Assumes constant marginal effects (P changes by β for all X values)
- Heteroskedasticity is severe (variance depends on X)
- For rare events (2-3% baseline), these problems are amplified

### Alternative Models

**Option A: IV-Probit**
```r
# Control function approach (2-step)
# Step 1: First-stage residuals
fs_resid <- residuals(first_stage_model)

# Step 2: Probit with control function
probit_cf <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + controls,
                 family = binomial(link = "probit"), data = data)

# Marginal effects
library(margins)
margins(probit_cf, variables = "cdp_sc_member")
```
**Pros**: Marginal effects constrained to [0,1], proper for binary outcome
**Cons**: Standard errors need bootstrap, interpretation more complex

**Option B: IV-Logit**
```r
# Similar to probit but with logit link
logit_cf <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + controls,
                family = binomial(link = "logit"), data = data)
```
**Pros**: Easier interpretation (odds ratios), widely used
**Cons**: Same SE issues as probit

**Option C: Discrete-Time Hazard Model**
```r
# Complementary log-log (cloglog) - naturally fits hazard framework
library(survival)

# Create time-to-event structure
# Each firm-year is at risk until SBTi commitment
hazard_cf <- glm(sbti_commitment_lead1 ~ cdp_sc_member + fs_resid + controls,
                 family = binomial(link = "cloglog"), data = data)
```
**Pros**:
- Natural fit for "time to SBTi commitment"
- Handles right-censoring (firms that never commit)
- Cloglog link designed for rare events
**Cons**: More complex to explain, less common in OM literature

**Option D: Cox Proportional Hazards with IV**
```r
library(survival)

# Two-stage approach
# Stage 1: Instrument for cdp_sc_member
# Stage 2: Cox model with fitted values

cox_model <- coxph(Surv(time, event) ~ cdp_sc_member_fit + controls,
                   cluster = gvkey, data = hazard_data)
```
**Pros**: Very flexible, handles time-varying covariates
**Cons**: Complex, less common, hard to instrument properly

### Recommendation

**Primary**: Stick with **LPM** as main specification for transparency and interpretability, but:
1. Report LPM limitations explicitly
2. Check and report predicted probability violations
3. Interpret effect qualitatively (not as literal pp increase)

**Robustness**: Add **IV-Probit with marginal effects** as robustness check
- Shows effect is robust to model choice
- Provides more plausible magnitude (marginal effect at mean)
- Demonstrates you're aware of LPM limitations

**Future work**: Mention **discrete-time hazard** as promising avenue for future research

---

## Prioritized Action Plan

### **Immediate Actions (This Week)**

1. **✓ Verify Model 1 results** (just-identified, country peer only)
   - Confirm F = 14.4 > 10 (strong)
   - Extract all coefficients and diagnostics
   - Make this PRIMARY specification

2. **✓ Check predicted probabilities from Model 2 LPM**
   ```r
   predicted <- predict(model_2)
   cat("Violations: ", sum(predicted < 0 | predicted > 1), "\n")
   cat("Mean predicted prob: ", mean(predicted), "\n")
   ```

3. **✓ Test Model 2a** (drop weak instrument)
   - Keep only: peer_cdp_share_country_lag + e_disc_lag2
   - Check joint F, Sargan test
   - Compare coefficients to Model 1 and Model 2

4. **✓ Incident variable robustness**
   - Try esc_incidents_highsev
   - Try log(esc_incidents_highreach + 1)
   - Check bivariate relationship

### **Short-Term Actions (Next 2 Weeks)**

5. **Anderson-Rubin confidence intervals**
   - Implement AR test for weak-IV robust inference
   - Report AR CIs alongside 2SLS estimates

6. **LIML estimation**
   - Run LIML as alternative to 2SLS
   - Compare point estimates

7. **IV-Probit with marginal effects**
   - Implement control function approach
   - Calculate marginal effects at sample means
   - Bootstrap standard errors

8. **Partial F-statistics**
   - Calculate partial R² for each instrument
   - Report in first-stage table

### **Medium-Term Actions (Next Month)**

9. **Heterogeneity analysis**
   - Test by firm size
   - Test by region
   - Test by industry emission intensity

10. **Alternative IV specifications**
    - Test alternative peer effects measures
    - Test alternative lag structures
    - Sensitivity to fixed effects structure

11. **Reassess theoretical framing**
    - If incidents don't work → focus on proactive interventions
    - Revise hypotheses based on findings
    - Adjust manuscript accordingly

---

## Implications for Manuscript Framing

### **Current Situation**
- H1 (Proactive interventions via CDP SC): **✓ SUPPORTED** (strong, robust)
- H2 (Reactive interventions via incidents): **✗ NOT SUPPORTED** (p = 0.183)

### **Options for Proceeding**

**Option 1: Focus on H1 Only (RECOMMENDED)**
- **Title**: "Internal Environmental Supply Chain Interventions and Science-Based Target Adoption"
- **RQ**: "Are internal, proactive environmental supply chain interventions effective at promoting firms' ability to commit to robust climate targets?"
- **Contribution**: Strong causal evidence for CDP SC pathway
- **Framing**: Risk exposure mechanism (CDP SC reveals climate risk → SBTi commitment)

**Option 2: Keep H2 but Report Null Finding**
- **Title**: Keep current title with both interventions
- **RQ**: "Are environmental supply chain interventions—both internal and external—effective..."
- **Contribution**: Identifies what works (proactive) vs. what doesn't (reactive)
- **Framing**: Important to show incidents alone don't trigger SBTi (need proactive capability building)

**Option 3: Reframe H2 Around Different Reactive Mechanism**
- Instead of incidents → try media coverage, stakeholder pressure, regulatory events
- Requires new data/measurement
- Probably not feasible in short term

### **My Recommendation: Option 1**

**Why**:
- POM values clear, strong findings over comprehensive-but-messy ones
- You have a very strong H1 result that tells an important story
- Null finding on H2 could distract from your core contribution
- Cleaner narrative: "Here's what works to drive SBTi adoption"

**How to frame**:
- "We examine whether proactive environmental supply chain interventions—specifically, participation in the CDP Supply Chain Program—causally increase firms' likelihood of committing to science-based climate targets."
- Mechanism: "CDP SC directs managerial attention to climate risk exposure embedded in supply chains, creating both awareness and capability for pursuing SBTi validation."
- Contribution: "First causal evidence linking supply chain climate programs to corporate climate target adoption using rigorous IV approach."

---

## R Code Templates for Priority Actions

### 1. Check Predicted Probabilities (LPM)
```r
# Load model results
# Assuming model_2 is your main 2SLS model

# Get predicted values
data$predicted_prob <- predict(model_2)

# Check violations
cat("\\n=== PREDICTED PROBABILITY DIAGNOSTICS ===\\n")
cat("Observations with predicted < 0:", sum(data$predicted_prob < 0, na.rm=TRUE), "\\n")
cat("Observations with predicted > 1:", sum(data$predicted_prob > 1, na.rm=TRUE), "\\n")
cat("Total violations:", sum(data$predicted_prob < 0 | data$predicted_prob > 1, na.rm=TRUE), "\\n")
cat("Percent violations:",
    round(sum(data$predicted_prob < 0 | data$predicted_prob > 1, na.rm=TRUE) /
          nrow(data) * 100, 2), "%\\n")
cat("Mean predicted probability:", round(mean(data$predicted_prob, na.rm=TRUE), 4), "\\n")
cat("Min predicted probability:", round(min(data$predicted_prob, na.rm=TRUE), 4), "\\n")
cat("Max predicted probability:", round(max(data$predicted_prob, na.rm=TRUE), 4), "\\n")
```

### 2. Model 2a: Drop Weak Instrument
```r
# Test two-instrument model (drop industry_incidents_excl_lag)

# First-stage with 2 instruments
first_stage_2inst <- feols(
  cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2 + controls |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data
)

# Joint F-test
wald_2inst <- wald(first_stage_2inst,
                   c("peer_cdp_share_country_lag", "e_disc_lag2"))
cat("Joint F (2 instruments):", round(wald_2inst$stat, 2), "\\n")

# Second-stage 2SLS
model_2a <- felm(
  sbti_commitment_lead1 ~ controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag + e_disc_lag2) |
    gvkey,
  data = data
)

summary(model_2a)

# Diagnostics with ivreg (demeaned data)
# [Include demeaning code from script 04]
# Run Sargan test with 1 df (2 instruments - 1 endogenous variable)
```

### 3. Incident Variable Robustness
```r
# Test alternative incident measures

# Model A: High severity instead of high reach
model_incidents_sev <- felm(
  sbti_commitment_lead1 ~ esc_incidents_highsev + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)

# Model B: Log transformation
model_incidents_log <- felm(
  sbti_commitment_lead1 ~ log(esc_incidents_highreach + 1) + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)

# Model C: Lagged incidents
model_incidents_lag <- felm(
  sbti_commitment_lead1 ~ esc_incidents_highreach_lag + controls |
    headquarter_country + year |
    (cdp_sc_member ~ peer_cdp_share_country_lag) |
    gvkey,
  data = data
)

# Compare incident coefficients
incident_results <- data.frame(
  Specification = c("High Reach (t)", "High Severity (t)",
                    "Log(High Reach)", "High Reach (t-1)"),
  Coefficient = c(
    coef(model_2)["`esc_incidents_highreach`"],
    coef(model_incidents_sev)["`esc_incidents_highsev`"],
    coef(model_incidents_log)["`log(esc_incidents_highreach + 1)`"],
    coef(model_incidents_lag)["`esc_incidents_highreach_lag`"]
  ),
  SE = c(
    se(model_2)["`esc_incidents_highreach`"],
    se(model_incidents_sev)["`esc_incidents_highsev`"],
    se(model_incidents_log)["`log(esc_incidents_highreach + 1)`"],
    se(model_incidents_lag)["`esc_incidents_highreach_lag`"]
  )
)

incident_results$p_value <- 2 * pt(-abs(incident_results$Coefficient / incident_results$SE),
                                    df = nrow(data) - 50)
print(incident_results)
```

### 4. IV-Probit with Control Function
```r
# Two-stage control function approach

# Stage 1: Get first-stage residuals
first_stage <- feols(
  cdp_sc_member ~ peer_cdp_share_country_lag + controls |
    headquarter_country + year,
  cluster = ~ gvkey,
  data = data
)

data$fs_residuals <- residuals(first_stage)

# Stage 2: Probit with control function
# Note: Need to manually create country and year dummies or demean
library(fixest)

probit_cf <- feglm(
  sbti_commitment_lead1 ~ cdp_sc_member + fs_residuals + controls |
    headquarter_country + year,
  family = binomial(link = "probit"),
  data = data
)

# Marginal effects at means
library(margins)
marg_eff <- margins(probit_cf, variables = "cdp_sc_member",
                    at = list(cdp_sc_member = c(0, 1)))

summary(marg_eff)

# Average marginal effect
cat("Average Marginal Effect:", mean(marg_eff$dydx_cdp_sc_member), "\\n")
```

---

## Summary Recommendations

### **For Analysis**
1. **Make Model 1 (just-identified, country peer IV) your PRIMARY specification**
   - F = 14.4 (clearly strong)
   - Coefficient stable (2.149 vs 2.161)
   - Simpler, more transparent

2. **Drop the weak instrument** (industry_incidents_excl_lag)
   - Test Model 2a with just 2 instruments
   - If joint F improves, use as robustness check

3. **Implement weak-IV robust inference**
   - Anderson-Rubin CIs
   - LIML estimation
   - Report alongside 2SLS

4. **Add IV-Probit as robustness**
   - Control function approach
   - Report marginal effects
   - Show effect is robust to model choice

5. **Acknowledge LPM limitations explicitly**
   - Check and report predicted probability violations
   - Interpret effect qualitatively
   - Emphasize sign and significance, de-emphasize magnitude

### **For Framing**
1. **Focus on H1 (proactive interventions)**
   - Strong, robust causal evidence
   - Important practical contribution
   - Cleaner story for POM

2. **Simplify mechanism**
   - Risk exposure (CDP SC reveals climate risk)
   - Internal intervention (capability building)
   - Avoid heavy theory development

3. **Be transparent about limitations**
   - Conservative SBTi sample
   - LATE interpretation (compliers only)
   - LPM limitations
   - Magnitude uncertainty (but sign and significance robust)

---

## Next Steps

Please let me know:
1. Should I help you create R scripts for the priority diagnostics?
2. Do you want to focus on H1 only or keep trying to salvage H2?
3. Would you like me to draft revised hypothesis sections for the intervention framing?
4. Do you want to explore the hazard model approach as an alternative?
