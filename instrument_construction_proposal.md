# Proposal: Additional Instrument Construction Strategies
## Resolving the Strength-Validity Trade-off in 2SLS Analysis

---

## Current Problem

Our 2SLS analysis faces a classic instrument validity-strength trade-off:

- **Country-level IV alone**: Strong (F=21.2) but cannot test validity (exactly identified)
- **Industry + Country IVs**: Strong (F=23.2) but **invalid** (Sargan p<0.001)
- **Cross-domain IVs**: Valid (Sargan p=0.19) but **weak** (F=3.5)

**Goal**: Construct additional instruments that, when combined with our existing instruments, produce an over-identified specification that is both:
1. **Strong**: First-stage F > 10
2. **Valid**: Passes Sargan-Hansen test (p > 0.10)

---

## Instrument Construction Framework

### What Makes a Valid Instrument?

A valid instrument must satisfy:

1. **Relevance**: Strongly predicts CDP membership (testable via F-statistic)
2. **Exclusion restriction**: Affects SBTi commitment *only* through CDP membership
3. **Independence**: Not mechanically correlated with the endogenous variable or error term

### Why Our Current Instruments Fail

**Industry + Country instruments fail Sargan test** because:
- Both capture peer effects that may have direct effects on SBTi commitment
- Country-level institutional pressures may directly influence climate target setting
- Industry-level norms may directly affect SBTi adoption (spillover effects)
- Common shocks (e.g., regulatory changes, investor pressure) affect both instruments

**Cross-domain instruments are weak** because:
- Excluding own country from industry peers removes most relevant variation
- Excluding own industry from country peers removes most relevant variation
- These instruments are too "distant" from the focal firm

---

## Proposed Instrument Categories

Below are five categories of potential instruments based on the IV literature. For each category, I describe:
- **Theoretical justification** for exclusion restriction
- **Types of variables** needed from your dataset
- **Construction method**
- **Examples from the literature**

---

### Category 1: Supply Chain Network Instruments

**Logic**: CDP Supply Chain program is driven by purchasing firms pressuring suppliers. Characteristics of a firm's *customers* should predict CDP membership but not directly affect the firm's own climate targets.

**Required data**:
- Customer/supplier relationships (e.g., from FactSet or Bloomberg)
- Customer characteristics: size, environmental performance, industry, country
- Historical purchasing relationships

**Potential instruments**:

1. **Customer environmental stringency** (lagged):
   ```
   avg_customer_e_disclosure_lag = mean(e_disc_coalesced_zeros of top customers, t-1)
   ```
   - **Relevance**: Environmentally proactive customers pressure suppliers to join CDP SC
   - **Exclusion**: Customer characteristics don't directly affect supplier's own SBTi decision
   - **Lag structure**: Use t-2 or t-3 to strengthen exclusion restriction

2. **Share of customers in high-scrutiny industries**:
   ```
   customer_highscrutiny_share = proportion of customers in consumer-facing industries
   ```
   - Consumer-facing industries (retail, food, apparel) face more stakeholder pressure
   - These customers are more likely to impose CDP SC requirements on suppliers

3. **Customer country environmental regulation**:
   ```
   avg_customer_country_envregulation = mean(environmental policy stringency in customer countries)
   ```
   - Customers in high-regulation countries transmit pressure to suppliers via CDP SC
   - Regulatory environment is exogenous to supplier firm decisions

**Literature support**: Di Maggio & Kacperczyk (2020) use supplier network characteristics as instruments; Giroud & Mueller (2019) use customer industry shocks.

---

### Category 2: Temporal/Historical Instruments

**Logic**: A firm's *past* behaviors or characteristics predict current CDP membership but are pre-determined relative to current SBTi decisions.

**Required data**:
- Historical environmental disclosure or certifications (t-2, t-3, or earlier)
- Past participation in other voluntary programs (e.g., UN Global Compact, GRI reporting)
- Historical emissions trajectories
- Historical stakeholder engagement scores

**Potential instruments**:

1. **Environmental disclosure score (t-3)**:
   ```
   e_disc_lag3 = e_disc_coalesced_zeros_{t-3}
   ```
   - **Relevance**: Past disclosure predicts future program participation (path dependence)
   - **Exclusion**: Disclosure 3 years ago doesn't directly cause SBTi commitment today (conditional on current controls)
   - **Key**: Must control for e_disc_{t} and e_disc_{t-1} to ensure exclusion

2. **Prior voluntary program participation** (historical):
   ```
   prior_vol_programs = count(UN Global Compact, GRI, ISO14001, etc.) in years t-3 to t-5
   ```
   - Firms with history of joining voluntary programs more likely to join CDP SC
   - Historical participation is pre-determined

3. **Change in environmental disclosure (early period)**:
   ```
   delta_e_disc_early = e_disc_{t-3} - e_disc_{t-5}
   ```
   - Firms improving disclosure early are more likely to join CDP SC later
   - Early trajectory is predetermined relative to current SBTi decision

**Literature support**: Cunningham et al. (2021) use historical firm behaviors as instruments; Larcker & Rusticus (2010) discuss lagged endogenous variables as instruments.

---

### Category 3: Market-Based Instruments

**Logic**: Investor and analyst characteristics affect firm visibility and pressure to join voluntary programs, but don't directly determine climate target commitments.

**Required data**:
- Institutional ownership data (e.g., from Thomson Reuters 13F filings)
- Analyst coverage
- ESG fund ownership
- Proxy voting patterns

**Potential instruments**:

1. **ESG institutional ownership (lagged)**:
   ```
   esg_inst_own_lag = share of equity held by ESG/SRI funds in year t-1
   ```
   - **Relevance**: ESG investors pressure firms to join CDP SC
   - **Exclusion**: ESG ownership affects voluntary disclosure programs but not direct climate actions (conditional on current financials and governance)
   - **Validation**: Can test if ESG ownership affects SBTi *conditional on* CDP membership (should be zero)

2. **Analyst coverage**:
   ```
   analyst_coverage = number of analysts covering the firm
   ```
   - More coverage → more scrutiny → more likely to join CDP SC to signal legitimacy
   - Analyst coverage reflects market interest, not firm's climate ambitions

3. **Passive institutional ownership**:
   ```
   passive_inst_own = share of equity held by index funds
   ```
   - Passive investors support CDP engagement but don't directly drive operational climate strategies
   - Passive ownership is driven by index inclusion, not firm climate policy

**Literature support**: Flammer et al. (2019) use institutional ownership as instruments for ESG activities; Kahn et al. (2021) use analyst coverage.

---

### Category 4: Geographic/Institutional Instruments

**Logic**: Firm location determines exposure to regulatory and institutional environments that affect CDP membership but can be plausibly excludable from direct effects on SBTi commitment when properly constructed.

**Required data**:
- Country-level environmental policy indices (e.g., OECD Environmental Policy Stringency Index)
- Country-level institutional quality measures (World Bank Governance Indicators)
- Regional climate policies (e.g., carbon pricing, renewable energy mandates)
- Stock exchange listing location

**Potential instruments**:

1. **Environmental policy stringency (country-level, lagged 2-3 years)**:
   ```
   env_policy_stringency_lag2 = OECD EPS index for headquarter country, t-2
   ```
   - **Relevance**: Policy environment affects voluntary program participation
   - **Exclusion**: Lag structure + country FE control for direct policy effects on SBTi
   - **Key**: Must include country FE and current policy controls

2. **Stock exchange environmental listing requirements**:
   ```
   exchange_esg_requirements = indicator for exchanges with mandatory ESG disclosure
   ```
   - Stock exchange rules affect voluntary disclosure participation
   - Exchange listing is predetermined and affects disclosure practices, not climate targets per se

3. **Regional peer effects (excluding own country)**:
   ```
   regional_peer_cdp_share = CDP SC adoption in geographic region excluding own country
   ```
   - Example: European firms (excl. Germany) for German firm
   - Regional diffusion affects local adoption via observation/learning
   - More distant than country peers → weaker but potentially more excludable

**Literature support**: Fisman & Wang (2017) use regional policy variation as instruments; Greenstone (2002) uses geographic proximity for peer effects.

---

### Category 5: Industry Dynamics Instruments

**Logic**: Time-varying industry shocks and characteristics affect CDP adoption rates but can be excludable from direct effects on individual firm SBTi decisions.

**Required data**:
- Industry-level environmental incidents or controversies
- Industry-level regulatory changes
- Industry trade association activities
- Industry-level stakeholder pressure indicators

**Potential instruments**:

1. **Industry environmental incidents (excluding own firm)**:
   ```
   industry_incidents_excl_lag = sum(esc_incidents of industry peers excluding focal firm, t-1)
   ```
   - **Relevance**: Industry-level incidents create pressure for all firms to join voluntary programs
   - **Exclusion**: Other firms' incidents don't directly cause focal firm's SBTi commitment (only through CDP membership)
   - **Control structure**: Must control for *own* firm incidents

2. **Industry exposure to environmental regulation**:
   ```
   industry_reg_exposure = measure of industry's exposure to carbon pricing, emissions regulations
   ```
   - Industries facing regulatory threats more likely to join voluntary programs preemptively
   - Regulatory exposure is industry-level, not firm-specific

3. **Industry trade association CDP promotion**:
   ```
   industry_assoc_cdp = indicator for whether industry trade association promotes CDP
   ```
   - Trade association activities drive CDP membership
   - Association-level activities don't directly determine individual firm climate targets

**Literature support**: Bertrand & Mullainathan (2003) use industry-level shocks; Giroud & Mueller (2015) use industry employment shocks.

---

## Combined Instrument Strategies

### Strategy A: Customer Network + Country Peer (Recommended if customer data available)

**Instruments**:
1. Average customer environmental disclosure (t-2)
2. Country-level peer share (t-1)

**Advantages**:
- Two conceptually distinct instruments (network vs. peer effects)
- Customer instrument is novel and plausibly excludable
- Both should be strong
- Low correlation expected between instruments

**Tests**:
- First-stage F should exceed 10 for joint test
- Sargan test should pass if exclusion restrictions hold
- Can test sensitivity to different customer definitions (top 5 vs. top 10)

---

### Strategy B: Historical + Market-based + Country Peer

**Instruments**:
1. Environmental disclosure score (t-3)
2. ESG institutional ownership (t-1)
3. Country-level peer share (t-1)

**Advantages**:
- Three instruments for over-identification testing
- Instruments capture different mechanisms (path dependence, investor pressure, peer effects)
- All are lagged to strengthen exclusion restriction

**Concerns**:
- Must carefully control for current-period equivalents (e_disc_t, inst_own_t)
- Need to verify instruments aren't too highly correlated

---

### Strategy C: Regional Variation + Industry Dynamics

**Instruments**:
1. Regional peer CDP share (excluding own country)
2. Industry environmental incidents (excluding own firm, lagged)
3. Environmental policy stringency (country-level, t-2)

**Advantages**:
- Exploits variation at multiple levels (region, industry, country-time)
- All instruments are "shifted" versions of potentially problematic instruments
- Policy instrument adds exogenous variation

**Concerns**:
- Regional peer instrument may be weak (similar to cross-domain issue)
- Need to ensure industry incidents instrument is strong enough

---

## Implementation Checklist

To implement any of these strategies, please confirm which of the following data are available in your dataset:

### Supply Chain Network Data
- [ ] Customer identifiers (gvkey or similar)
- [ ] Customer-supplier relationship data
- [ ] Customer environmental disclosure scores
- [ ] Customer financial characteristics
- [ ] Customer industry/country information

### Historical/Temporal Data
- [ ] Environmental disclosure scores for t-2, t-3, or earlier years
- [ ] Historical participation in voluntary programs (UN Global Compact, GRI, ISO14001, etc.)
- [ ] Historical emissions data (t-2, t-3, or earlier)
- [ ] Historical governance or stakeholder engagement scores

### Market-Based Data
- [ ] Institutional ownership data (total or by investor type)
- [ ] ESG/SRI fund ownership data
- [ ] Analyst coverage data
- [ ] Passive vs. active institutional ownership breakdown

### Geographic/Institutional Data
- [ ] Country-level environmental policy indices (OECD EPS or similar)
- [ ] Stock exchange listing information
- [ ] Country-level governance or institutional quality measures
- [ ] Regional classifications (e.g., Europe, Asia-Pacific, Americas)

### Industry Dynamics Data
- [ ] Industry-level aggregates of environmental incidents
- [ ] Industry regulatory exposure measures
- [ ] Industry trade association information
- [ ] Industry-level policy changes or shocks

---

## Next Steps

1. **Review the checklist above** and identify which types of data are available in your dataset

2. **Prioritize instrument categories** based on:
   - Data availability
   - Theoretical appeal of exclusion restriction
   - Expected instrument strength

3. **Implement instrument construction** for 2-3 selected instruments

4. **Run diagnostic tests**:
   - First-stage F-test for joint significance
   - Sargan-Hansen test for overidentification
   - Sensitivity analysis: test subsets of instruments

5. **Validate exclusion restriction** (where possible):
   - Test if instruments predict SBTi *conditional on* CDP membership (reduced form - 2SLS)
   - Test if instruments are correlated with time-varying unobservables

---

## Recommendation

If I had to guess which measures are most likely available and would work best, I would prioritize:

**First choice**: Customer network instruments (if available)
- Conceptually cleanest exclusion restriction
- Directly tied to CDP SC program mechanism (buyer pressure)
- Likely to be strong predictors

**Second choice**: ESG institutional ownership + historical disclosure
- Widely available in Bloomberg/Compustat
- Well-established in literature
- Can construct from existing data sources

**Third choice**: Country-level policy + regional peers
- Policy data publicly available (OECD)
- Can construct regional aggregates from existing data
- Adds exogenous variation

---

## Questions for You

To move forward efficiently, please let me know:

1. **Which categories of data** from the checklist above are available in your complete_data_2022.rds or related datasets?

2. **What other firm-level measures** exist in your data that haven't been used yet? (Even if you're not sure they'd work as instruments, I can assess)

3. **Do you have access to external data sources** (e.g., institutional ownership databases, policy indices) that could be merged with your firm-year panel?

Once I know what's available, I can immediately implement the most promising instrument construction strategy and re-run the 2SLS analysis.
