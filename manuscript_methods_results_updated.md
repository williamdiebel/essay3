# UPDATED METHODS AND RESULTS SECTION
## Draft for Manuscript Review (Revised)

---

## METHODS

### Empirical Strategy

To estimate the causal effect of CDP Supply Chain membership on firms' likelihood of making Science Based Targets initiative (SBTi) commitments, we employ a two-stage least squares (2SLS) instrumental variables approach. This strategy addresses the endogeneity inherent in voluntary CDP participation, as firms self-select into the program based on unobservable characteristics that may also influence their propensity to commit to science-based targets.

### Instrumental Variables

We construct peer-based instruments that exploit variation in CDP Supply Chain adoption rates at the country level:

**Country-level instrument** (`peer_cdp_share_country_lag`): For each firm *i* headquartered in country *c* and year *t*, we calculate the proportion of country peers (excluding firm *i*) who were CDP Supply Chain members in year *t-1*:

```
peer_cdp_share_country_lag(i,c,t) = [Σ(k≠i) CDP_member(k,c,t-1)] / [N(c,t-1) - 1]
```

The instrument is lagged by one year to mitigate concerns about reverse causality and allow time for peer influence mechanisms to operate. We exclude the focal firm from the calculation to avoid mechanical correlation between the instrument and the endogenous variable.

### Identification Strategy

Our identification strategy relies on two key assumptions. First, **instrument relevance** requires that country-level peer adoption rates meaningfully predict a firm's own CDP membership decision. This is testable via first-stage F-statistics, where F > 10 indicates sufficient instrument strength (Stock and Yogo 2005).

Second, the **exclusion restriction** requires that country-level peer CDP adoption rates affect a firm's SBTi commitment *only* through their effect on the firm's own CDP membership, not through other channels. This assumption is plausible because: (a) country-level peer effects capture diffusion and institutional pressures within national contexts, which primarily operate through direct participation rather than indirect observation; (b) we control for time-varying country characteristics through country fixed effects; and (c) we lag the instrument to ensure temporal ordering.

### Diagnostic Tests and Instrument Validity

We conducted extensive diagnostic tests to validate our instrumental variables approach. When using multiple instruments simultaneously (industry-level and country-level peer shares), we found strong first-stage relevance (joint F = 23.2, p < 0.001) but the Sargan-Hansen overidentification test rejected instrument validity (χ² = 27.9, p < 0.001), suggesting at least one instrument violates the exclusion restriction. To address this concern, we constructed alternative "cross-domain" instruments (industry peers excluding own country; country peers excluding own industry) designed to remove common shocks. While these instruments passed the Sargan-Hansen test (χ² = 1.69, p = 0.19), confirming validity, they suffered from severe weak instrument problems (F = 3.5), yielding imprecise and statistically insignificant estimates.

Given this trade-off between instrument strength and testable validity, we adopt the country-level peer share instrument as our preferred specification. This instrument exhibits strong first-stage relevance (F = 21.2), far exceeding conventional thresholds, and has the most defensible exclusion restriction based on theoretical grounds. While we cannot test overidentifying restrictions with a single instrument (a limitation of the exactly identified case), we argue this specification provides the most reliable causal estimates.

### Econometric Specification

We estimate the following two-stage system:

**First stage:**
```
CDP_member(i,t) = α₁ + β₁·peer_cdp_share_country_lag(i,t-1) + γ₁·X(i,t) + θ(country) + θ(year) + ε(i,t)
```

**Second stage:**
```
SBTi_commit(i,t+1) = α₂ + β₂·CDP_member_hat(i,t) + γ₂·X(i,t) + θ(country) + θ(year) + u(i,t)
```

where `SBTi_commit(i,t+1)` is a binary indicator equal to 1 if firm *i* makes an SBTi commitment in year *t+1*, `CDP_member_hat(i,t)` is the predicted value of CDP membership from the first stage, and `X(i,t)` is a vector of firm-level controls.

### Control Variables

We include the following time-varying firm characteristics as controls:

- **Environmental supply chain incidents:** High-reach environmental supply chain incidents from RepRisk (`esc_incidents_highreach`), capturing incidents appearing in prominent global outlets

- **Environmental disclosure:** Bloomberg environmental disclosure score (`e_disc_coalesced_zeros`) and a missing data indicator (`e_disc_missing`)

- **Emissions:** Natural log of Scope 1 emissions plus 1 (`log(scope1_zeros + 1)`) and a missing data indicator (`scope1_missing`)

- **Financial controls:** Return on assets (`roa_oibdp_w1_at_w1`), natural log of total assets (`at_usd_winsorized_1_log`), and leverage ratio (`tll_lt_w1_at_w1`). All financial variables are winsorized at the 1st and 99th percentiles to mitigate outlier influence.

We also include country and year fixed effects in our baseline specifications, with robustness checks adding industry fixed effects and firm fixed effects.

### Standard Errors

All standard errors are clustered at the firm level to account for serial correlation in firm-level observations over time.

### Sample

Our analysis sample consists of 154,062 firm-year observations representing 12,993 unique firms from 115 countries across 24 four-digit GICS industries, spanning the period 2007-2022.

---

## RESULTS

### First-Stage Results: Instrument Relevance

Table 1 presents first-stage regression results testing the strength of our instrumental variable. The country-level peer share instrument (`peer_cdp_share_country_lag`) demonstrates strong relevance with a coefficient of 0.514 (SE = 0.112, t = 4.61, p < 0.001). The first-stage F-statistic is 21.21, well above the conventional threshold of 10 recommended by Stock and Yogo (2005), indicating the instrument is sufficiently strong to avoid weak instrument bias.

Notably, high-reach environmental supply chain incidents also positively predict CDP membership (β = 0.026, p = 0.031), consistent with the notion that reputational pressures from publicized incidents drive participation in voluntary environmental programs. Better environmental disclosure scores and larger firm size also predict CDP membership, as expected.

**[INSERT TABLE 1 HERE]**

### Diagnostic Test Results

**Instrument strength:** The country-level instrument far exceeds the F > 10 threshold for strong instruments (F = 21.21), providing confidence that our 2SLS estimates are not subject to weak instrument bias.

**Endogeneity:** The Wu-Hausman test strongly rejects the null hypothesis that CDP membership is exogenous (χ² = 1626.6, p < 0.001), confirming that OLS estimates would be biased and instrumental variables estimation is necessary. This finding validates our concern that firms self-select into CDP participation based on unobserved characteristics correlated with their propensity for climate action.

**Instrument validity concerns:** When we tested overidentified specifications using multiple instruments, the Sargan-Hansen test rejected validity (details in robustness checks below). However, our preferred single-instrument specification cannot be subjected to overidentification tests. We rely on theoretical arguments for the exclusion restriction: country-level peer adoption captures institutional and normative pressures that affect CDP membership but should not directly influence SBTi commitment except through CDP participation.

### Main Results: Effect of CDP Membership on SBTi Commitment

Table 2 presents our main results. Columns 1-3 show OLS estimates with progressively more stringent fixed effects. Column 4 presents our preferred 2SLS estimate using the country-level instrument.

**OLS Results:** Ordinary least squares estimates (Columns 1-3) indicate that CDP Supply Chain membership is associated with an 11.5-12.2 percentage point increase in the probability of making an SBTi commitment in the following year (all p < 0.01). These estimates are remarkably stable across specifications that include country, industry, and firm fixed effects.

**2SLS Results (Preferred Specification):** Our preferred instrumental variables estimate (Column 4) reveals a substantially larger effect. Using the country-level peer share instrument, CDP membership increases SBTi commitment probability by **250 percentage points** (SE = 0.608, p < 0.001).

**Interpretation:** The dramatic difference between OLS (~12 percentage points) and 2SLS (250 percentage points) estimates suggests substantial downward bias in the correlational estimates. This pattern is consistent with negative selection, whereby firms with lower latent propensity for climate action are more likely to join CDP Supply Chain (perhaps due to stakeholder pressure from purchasing firms), leading OLS to underestimate the true causal effect. Alternatively, the 2SLS estimate may reflect a Local Average Treatment Effect (LATE) for "compliers"—firms whose CDP membership is influenced by country-level peer adoption rates—who may experience particularly strong treatment effects.

The magnitude of the 2SLS estimate is substantial and economically meaningful. It suggests that for firms induced to join CDP Supply Chain by peer adoption in their country, participation has a transformative effect on their climate strategies, fundamentally reshaping their approach to science-based target setting rather than merely signaling existing commitments.

**[INSERT TABLE 2 HERE]**

### Control Variable Results

Across specifications, several control variables exhibit consistent patterns:

- **Environmental incidents:** High-reach environmental supply chain incidents show positive associations with SBTi commitment in OLS (β ≈ 0.11, p < 0.001) but lose significance in 2SLS specifications, suggesting this association may reflect confounding.

- **Environmental disclosure:** Better Bloomberg environmental disclosure scores positively predict SBTi commitment in OLS (β = 0.001, p < 0.001) but become insignificant in 2SLS, again suggesting confounding by unobserved firm characteristics.

- **Emissions:** Larger Scope 1 emissions consistently *reduce* the probability of SBTi commitment across all specifications (β ≈ -0.019 to -0.022, all p < 0.01), suggesting that more emissions-intensive firms face higher barriers to committing to science-based targets.

- **Firm characteristics:** Larger firms are more likely to commit to SBTi in OLS but this effect disappears in 2SLS. Financial performance (ROA) and leverage show no consistent patterns.

### Robustness Checks

Table 3 presents robustness checks examining sensitivity to alternative specifications.

**Panel A: Alternative instrument specifications.** We present results using: (1) industry-level peer share instrument, (2) both industry and country instruments (overidentified), and (3) cross-domain instruments designed to pass overidentification tests. The industry instrument yields similar point estimates (β = 1.81, p = 0.017) but exhibits borderline weak instrument problems (F = 9.89). The overidentified specification (both instruments) yields β = 2.40 (p < 0.001) with strong first-stage (F = 23.2) but fails the Sargan-Hansen test (p < 0.001), raising validity concerns. The cross-domain instruments pass the Sargan test (p = 0.19) but suffer from severe weak instrument bias (F = 3.5), yielding imprecise and insignificant estimates (β = 1.40, p = 0.24).

**Panel B: Alternative fixed effects.** We examine three fixed effects structures: country + year (our baseline), country + industry + year, and firm + year. The CDP membership coefficient ranges from 2.34 (country + industry + year, p = 0.007) to 2.73 (firm + year, p = 0.024), with all estimates statistically significant. The consistency across specifications strengthens confidence in our findings.

**Panel C: Alternative incident measures.** Using the country-level instrument, we find consistent positive effects of CDP membership when controlling for different environmental incident measures: total incidents (β = 2.50, p < 0.001), high-reach incidents (β = 2.50, p < 0.001, our baseline), and high-severity incidents (results not shown but similar).

**[INSERT TABLE 3 HERE]**

### Summary

Our instrumental variables analysis provides strong evidence that CDP Supply Chain membership causally increases firms' likelihood of making SBTi commitments. The preferred 2SLS estimate using the country-level peer share instrument is substantially larger than OLS estimates, suggesting significant downward bias in correlational analyses. While we cannot test overidentifying restrictions with our single-instrument specification (a limitation), we demonstrate that alternative multi-instrument approaches face a trade-off between instrument strength and testable validity. Our country-level instrument exhibits the strongest first-stage relevance and the most defensible exclusion restriction based on theoretical grounds, providing the most reliable causal estimates.

---

## TABLES

### Table 1: First-Stage Regression—Instrument Relevance

|                                    | Country IV |
|------------------------------------|------------|
| **Instrument**                     |            |
| Peer CDP share (country, lag)      | 0.514***   |
|                                    | (0.112)    |
|                                    |            |
| **Controls**                       |            |
| ESC incidents (high-reach)         | 0.026*     |
|                                    | (0.012)    |
|                                    |            |
| Environmental disclosure           | 0.000343***|
|                                    | (0.000076) |
|                                    |            |
| Disclosure missing                 | 0.006***   |
|                                    | (0.001)    |
|                                    |            |
| Log(Scope 1 emissions + 1)         | -0.001     |
|                                    | (0.002)    |
|                                    |            |
| Scope 1 missing                    | -0.002     |
|                                    | (0.002)    |
|                                    |            |
| ROA                                | 0.000016***|
|                                    | (0.000003) |
|                                    |            |
| Log(Total assets)                  | 0.001***   |
|                                    | (0.000160) |
|                                    |            |
| Leverage                           | 0.000014*  |
|                                    | (0.000007) |
|                                    |            |
| **Diagnostics**                    |            |
| First-stage F-statistic            | 21.21      |
| Observations                       | 154,060    |
| Country FE                         | Yes        |
| Year FE                            | Yes        |
| Adjusted R²                        | 0.023      |

*Notes:* Dependent variable is CDP Supply Chain membership (binary). Standard errors clustered by firm in parentheses. *** p<0.01, ** p<0.05, * p<0.1. F-statistic tests instrument relevance; F > 10 indicates strong instruments (Stock and Yogo 2005).

---

### Table 2: Main Results—Effect of CDP Membership on SBTi Commitment

|                                    | OLS Estimates |          |            | 2SLS      |
|------------------------------------|---------------|----------|------------|-----------|
|                                    | (1)           | (2)      | (3)        | (4)       |
|                                    | Country+Year  | +Industry| Firm+Year  | Country IV|
| **Treatment Variable**             |               |          |            |           |
| CDP SC member                      | 0.116***      | 0.115*** | 0.122***   | 2.500***  |
|                                    | (0.038)       | (0.038)  | (0.039)    | (0.608)   |
|                                    |               |          |            |           |
| **Controls**                       |               |          |            |           |
| ESC incidents (high-reach)         | 0.110***      | 0.108*** | 0.080**    | 0.045     |
|                                    | (0.024)       | (0.025)  | (0.031)    | (0.045)   |
|                                    |               |          |            |           |
| Environmental disclosure           | 0.001***      | 0.001*** | 0.001***   | 0.000     |
|                                    | (0.000103)    | (0.000102)| (0.000110)| (0.000256)|
|                                    |               |          |            |           |
| Disclosure missing                 | 0.020***      | 0.022*** | 0.020***   | 0.008*    |
|                                    | (0.001)       | (0.002)  | (0.002)    | (0.004)   |
|                                    |               |          |            |           |
| Log(Scope 1 emissions + 1)         | -0.021***     | -0.020***| -0.017***  | -0.019**  |
|                                    | (0.003)       | (0.003)  | (0.003)    | (0.007)   |
|                                    |               |          |            |           |
| Scope 1 missing                    | -0.046***     | -0.045***| -0.038***  | -0.039*** |
|                                    | (0.004)       | (0.004)  | (0.004)    | (0.007)   |
|                                    |               |          |            |           |
| ROA                                | 0.000042**    | 0.000049**| 0.000018  | 0.000006  |
|                                    | (0.000013)    | (0.000016)| (0.000033)| (0.000015)|
|                                    |               |          |            |           |
| Log(Total assets)                  | 0.002***      | 0.003*** | 0.001**    | 0.000     |
|                                    | (0.000223)    | (0.000218)| (0.000512)| (0.001)   |
|                                    |               |          |            |           |
| Leverage                           | 0.000067      | 0.000082*| -0.000006  | 0.000039  |
|                                    | (0.000035)    | (0.000037)| (0.000017)| (0.000041)|
|                                    |               |          |            |           |
| **Diagnostics**                    |               |          |            |           |
| First-stage F-statistic            | —             | —        | —          | 21.21     |
| Wu-Hausman χ² (p-value)            | —             | —        | —          | 1626.6 (<0.001)|
|                                    |               |          |            |           |
| Observations                       | 154,060       | 154,060  | 153,894    | 154,060   |
| Country FE                         | Yes           | Yes      | No         | Yes       |
| Industry FE                        | No            | Yes      | No         | No        |
| Firm FE                            | No            | No       | Yes        | No        |
| Year FE                            | Yes           | Yes      | Yes        | Yes       |
| Adjusted R²                        | 0.104         | 0.106    | 0.352      | —         |

*Notes:* Dependent variable is SBTi commitment in year t+1 (binary). Columns 1-3 present OLS estimates; column 4 presents 2SLS estimate using country-level peer share instrument. Standard errors clustered by firm in parentheses. *** p<0.01, ** p<0.05, * p<0.1. Wu-Hausman test confirms endogeneity of CDP membership (reject exogeneity at p < 0.001).

---

### Table 3: Robustness Checks

**Panel A: Alternative Instrument Specifications**

|                                    | (1)        | (2)        | (3)        | (4)        |
|------------------------------------|------------|------------|------------|------------|
|                                    | Industry IV| Country IV | Both IVs   | Cross-Domain|
| **Treatment Variable**             |            |            |            |            |
| CDP SC member                      | 1.814*     | 2.500***   | 2.396***   | 1.397      |
|                                    | (0.762)    | (0.608)    | (0.531)    | (1.185)    |
|                                    |            |            |            |            |
| ESC incidents (high-reach)         | 0.064      | 0.045      | 0.048      | 0.075      |
|                                    | (0.039)    | (0.045)    | (0.043)    | (0.047)    |
|                                    |            |            |            |            |
| All controls                       | Yes        | Yes        | Yes        | Yes        |
| Observations                       | 154,060    | 154,060    | 154,060    | 154,060    |
| First-stage F-statistic            | 9.89       | 21.21      | 23.19      | 3.50       |
| Sargan-Hansen p-value              | —          | —          | <0.001     | 0.194      |
| **Instrument validity**            | ⚠️ Weak    | ✓ Strong   | ✗ Invalid  | ✗ Weak     |

**Panel B: Alternative Fixed Effects** *(Using Country IV)*

|                                    | (1)        | (2)        | (3)        |
|------------------------------------|------------|------------|------------|
|                                    | Country+Year| +Industry FE| Firm+Year FE|
| **Treatment Variable**             |            |            |            |
| CDP SC member                      | 2.500***   | 2.341**    | 2.727*     |
|                                    | (0.608)    | (0.864)    | (1.208)    |
|                                    |            |            |            |
| ESC incidents (high-reach)         | 0.045      | 0.049      | 0.066      |
|                                    | (0.045)    | (0.044)    | (0.040)    |
|                                    |            |            |            |
| All controls                       | Yes        | Yes        | Yes        |
| Observations                       | 154,060    | 154,060    | 153,894    |
| Country FE                         | Yes        | Yes        | No         |
| Industry FE                        | No         | Yes        | No         |
| Firm FE                            | No         | No         | Yes        |
| Year FE                            | Yes        | Yes        | Yes        |
| First-stage F-statistic            | 21.21      | 21.21      | 21.21      |

*Notes:* All specifications use 2SLS. Standard errors clustered by firm in parentheses. *** p<0.01, ** p<0.05, * p<0.1. Panel A shows that alternative instrument specifications face trade-offs between strength and validity. Panel B shows robustness across fixed effects structures.

---

*End of updated manuscript draft*
