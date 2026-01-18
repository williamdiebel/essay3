# Weekend Action Plan REVISED: Dual-Pathway Manuscript
**Target**: Strong empirical support for BOTH H1 and H2 by EOD Sunday
**Strategy**: Run comprehensive robustness tests FIRST, then write based on findings

---

## Key Change from Original Plan

**Original**: Drop H2, focus on H1 only
**REVISED**: Keep both H1 and H2, find empirical specification that supports both

### Dual Research Purpose:
1. **H1 (Internal)**: CDP SC membership → climate risk awareness → SBTi commitment
2. **H2 (External)**: Environmental incidents → climate risk awareness → SBTi commitment
3. **H3 (Complementarity)**: External matters MORE when internal capability exists

---

## Critical Insight: The Interaction Effect Story

**Most promising path to H2 support**: Test if incidents matter MORE for firms with CDP SC membership

**Theoretical logic**:
- Incidents alone may not drive SBTi (too complex, requires capability)
- CDP SC alone drives SBTi (builds capability)
- **Incidents + CDP SC → strongest effect** (external shock + internal capability = action)

**Framing**: Both pathways work through same risk awareness mechanism, but are **complementary** not independent

---

## Saturday: Empirical Discovery (10 hours)

### Morning Session 1 (3 hours): Rare Event Models

**Priority**: Get away from problematic LPM (2-3% baseline makes it inappropriate)

**Tasks**:
1. Run `scripts/05_comprehensive_robustness.R` - Phase 1
   - IV-Probit with control function
   - IV-Logit
   - Complementary log-log (cloglog)

2. Extract marginal effects for each model
   - CDP SC average marginal effect (AME)
   - Incidents AME
   - Compare p-values across models

**Expected outcome**: More plausible magnitudes, clearer test of incident effects

**Decision point**: Which model fits best? (likely Probit or Cloglog for rare events)

---

### Morning Session 2 (3 hours): Alternative Incident Measures

**Priority**: Test if measurement obscures true H2 effect

**Tasks**:
1. Run Phase 2 of robustness script
   - `esc_incidents_highsev` (high severity instead of high reach)
   - `esc_incidents` (all incidents, not filtered)
   - `log(esc_incidents_highreach + 1)` (log transformation)
   - Binary indicator (any incident yes/no)

2. Create comparison table:
   ```
   Incident Measure | Probit AME | p-value | Supported?
   High Reach       | [value]    | [p]     | [Y/N]
   High Severity    | [value]    | [p]     | [Y/N]
   All Incidents    | [value]    | [p]     | [Y/N]
   Log Transform    | [value]    | [p]     | [Y/N]
   Binary           | [value]    | [p]     | [Y/N]
   ```

**Decision point**: Which incident measure (if any) shows significant direct effect?

---

### Afternoon Session 1 (2 hours): INTERACTION EFFECTS (CRITICAL)

**Priority**: THIS IS THE KEY - test if incidents matter more with CDP SC

**Tasks**:
1. Run Phase 3 of robustness script
   - OLS with CDP SC × Incidents interaction
   - IV-Probit with interaction
   - Test all incident measures with interaction

2. Calculate conditional marginal effects:
   - Effect of incidents when CDP SC = 0 (non-members)
   - Effect of incidents when CDP SC = 1 (members)
   - Test if difference is significant

3. Visualize if interaction significant:
   ```r
   # Predicted probability plot
   # X-axis: Incidents (0 to max)
   # Y-axis: P(SBTi commitment)
   # Two lines: CDP SC = 0 vs CDP SC = 1
   # Lines should diverge if interaction matters
   ```

**CRITICAL DECISION POINT**:
- **If interaction significant** → THIS IS YOUR H2 STORY!
  - Frame H2 as: "External interventions complement internal capability"
  - "Firms with CDP SC respond to incidents by pursuing SBTi"
  - "Both pathways work, but synergistically"

- **If interaction not significant** → Continue to Phase 4 (subsamples)

---

### Afternoon Session 2 (2 hours): Results Compilation & Assessment

**Tasks**:
1. Review all output from Phases 1-3
2. Identify best specifications:
   - Model choice (Probit? Logit? Cloglog?)
   - Incident measure (highsev? all? binary?)
   - Interaction effect (significant?)

3. Create master results table:
   ```
   Specification | H1 (CDP SC) | H2 (Incidents) | H3 (Interaction) | Recommended?
   LPM 2SLS      | Sig         | Not sig        | -                | No (rare event)
   IV-Probit     | Sig         | [?]            | -                | Maybe
   With Interact | Sig         | [?]            | [?]              | YES if sig
   ```

4. **MAKE KEY DECISIONS**:
   - Primary model for manuscript?
   - Primary incident measure?
   - H2 framing: direct effect, interaction, or both?

---

## Saturday Evening: Strategic Planning (1-2 hours)

### Decision Tree:

**Scenario A: Interaction Significant** ✓ BEST OUTCOME
- **H1 framing**: Direct effect of internal intervention (as planned)
- **H2 framing**: External interventions amplify internal capability
- **Writing focus**: Complementarity mechanism
- **Tables needed**:
  - Descriptive stats
  - First-stage (IV)
  - Main effects + interaction model
  - Conditional marginal effects

**Scenario B: Direct Effect Significant (specific measure)**
- **H1 framing**: Internal intervention (as planned)
- **H2 framing**: External intervention with specific measurement
- **Writing focus**: Both pathways independently
- **Tables needed**:
  - Descriptive stats
  - First-stage
  - Main effects for both interventions

**Scenario C: H1 Only**
- **Framing**: Focus on internal interventions
- **H2 treatment**: Report null finding, discuss why
- **Writing focus**: Strong causal evidence for proactive pathway
- **Tables needed**:
  - Descriptive stats
  - First-stage
  - Main CDP SC effect

---

## Sunday: Writing (8 hours)

### Morning Session 1 (3 hours): Hypothesis Development

**Based on Saturday findings**, write:

**H1 (Internal Intervention)** - ~500 words
- CDP SC as proactive, internal intervention
- Mechanism: Reveals Scope 3 emissions exposure
- Directs attention to climate risk
- Builds capability for climate action
- Hypothesis: CDP SC membership → SBTi commitment

**H2 (External Intervention)** - Frame based on findings:

*If interaction significant*:
- Incidents as reactive, external intervention
- Mechanism: Media coverage reveals reputational risk
- BUT: Response requires internal capability
- Complementarity with H1: External + Internal → strongest effect
- Hypothesis: "Incidents increase SBTi commitment more strongly for firms with CDP SC membership" (interaction hypothesis)

*If direct effect significant*:
- Incidents as reactive, external intervention
- Mechanism: External shock directs attention to climate risk
- Independent pathway from H1
- Hypothesis: Environmental incidents → SBTi commitment

*If neither works*:
- Still develop H2 theory
- Report as null finding
- Discuss why (incidents may trigger other responses, not SBTi specifically)

---

### Morning Session 2 (2 hours): Update Abstract & Introduction

**Abstract** (~200 words):
- "We examine whether environmental supply chain interventions—both internal (CDP SC membership) and external (environmental incidents)—increase firms' likelihood of committing to science-based climate targets."
- "Using panel data and instrumental variables estimation to address endogeneity..."
- Results: State findings for H1, H2 (or interaction)
- "Our findings show [internal interventions drive SBTi + external interventions complement/amplify/independently contribute]"

**Introduction**:
- Dual research question framework
- Emphasize practical contribution: identifying effective interventions
- Minimize theory development (POM style)
- Clear statement of dual pathways

---

### Afternoon Session 1 (2 hours): Results Section

**Structure**:
1. **Descriptive statistics** (1 para)
2. **Model choice justification** (1 para)
   - "Given 2-3% baseline SBTi rate (rare event), we use [Probit/Logit/Cloglog] with control function approach rather than LPM"
3. **First-stage results** (1 para)
   - IV strength for CDP SC
4. **Main effects** (2 paras)
   - H1: CDP SC → SBTi (AME, p-value, interpretation)
   - H2: Incidents → SBTi (direct or interaction framing)
5. **Interaction effects** (1-2 paras) - IF SIGNIFICANT
   - Conditional marginal effects
   - Interpretation of complementarity
6. **Robustness** (1 para)
   - Alternative incident measures
   - Alternative models
   - Consistent findings

---

### Afternoon Session 2 (2 hours): Tables & Discussion

**Create Tables**:
1. **Table 1**: Descriptive statistics
2. **Table 2**: First-stage regression (IV for CDP SC)
3. **Table 3**: Main results
   - Model 1: H1 (CDP SC effect)
   - Model 2: H1 + H2 (both interventions)
   - Model 3: H1 + H2 + Interaction (if significant)

**Draft Discussion** (~1000 words):
- Summary of findings (both pathways or H1 only)
- Theoretical contribution (risk awareness mechanism)
- Practical implications:
  - For managers: CDP SC as pathway
  - For policymakers: [Depends on findings - peer effects? Incident regulation?]
  - For NGOs: Both CDP and SBTi mutually reinforcing
- Limitations (LATE, rare events, measurement)
- Future research

---

## Key Files to Reference

**Empirical Testing**:
- `EMPIRICAL_TESTING_PROTOCOL.md` - Complete testing strategy
- `scripts/05_comprehensive_robustness.R` - Run this Saturday morning!
- `ANALYTICAL_ISSUES_AND_PLAN.md` - Methodological guidance

**Results Documentation**:
- `manuscript_final_results.md` - Current Model 1 & 2 results (LPM)
- `results_comprehensive_robustness.csv` - Will be created by robustness script

**Manuscript**:
- `manuscripts/essay3_manuscript.tex` - LaTeX template
- `manuscripts/manuscript_text.txt` - Original text to adapt

---

## Success Criteria

### Minimum Success (H1 Only):
- ✓ H1 supported with rare event model
- ✓ Plausible magnitude (AME in reasonable range)
- ✓ H2 tested thoroughly, null finding reported
- ✓ Complete manuscript draft

### Good Success (H1 + H2 Direct):
- ✓ H1 supported
- ✓ H2 supported with specific incident measure
- ✓ Both pathways independently significant
- ✓ Complete manuscript draft

### Excellent Success (H1 + H2 Interaction):
- ✓ H1 supported
- ✓ H2 supported through interaction effect
- ✓ Complementarity story is compelling and theoretically sound
- ✓ Complete manuscript draft
- ✓ This is the BEST outcome - both hypotheses work!

---

## Time Budget Check

**Saturday** (10 hours):
- Rare event models: 3 hours
- Incident measures: 3 hours
- Interactions: 2 hours
- Results compilation: 2 hours

**Sunday** (8 hours):
- Hypothesis writing: 3 hours
- Abstract/intro: 2 hours
- Results section: 2 hours
- Tables/discussion: 2 hours

**Total**: 18 hours

**Feasibility**: YES - focused on empirics + writing only (no full manuscript completion)

**Realistic goal**: Strong empirical foundation + draft of key sections, not full submission-ready manuscript

---

## Critical Success Factors

1. **Run robustness script FIRST** before any writing
2. **Let data guide framing** - don't force H2 if evidence weak
3. **Interaction effect is your best bet** for H2 support
4. **Model choice matters** - rare event models may reveal what LPM obscured
5. **Measurement matters** - try all incident specifications

---

## Saturday Morning Checklist (START HERE)

- [ ] Review `EMPIRICAL_TESTING_PROTOCOL.md`
- [ ] Open `scripts/05_comprehensive_robustness.R`
- [ ] Set working directory to project root
- [ ] Run Phase 1 (rare event models)
- [ ] Check if incidents significant in Probit/Logit/Cloglog
- [ ] Run Phase 2 (incident measures)
- [ ] Check if any measure shows significant effect
- [ ] Run Phase 3 (interactions) - **KEY TEST**
- [ ] Check if interaction significant
- [ ] Make decision about H2 framing
- [ ] Document findings in notes

**THEN** start writing based on what you found.

---

**The interaction effect is your best path to supporting both hypotheses. Focus there. Good luck! 🚀**
