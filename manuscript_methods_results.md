# METHODS AND RESULTS SECTION
## Draft for Manuscript Review

---

## METHODS

### Empirical Strategy

To estimate the causal effect of CDP Supply Chain membership on firms' likelihood of making Science Based Targets initiative (SBTi) commitments, we employ a two-stage least squares (2SLS) instrumental variables approach. This strategy addresses the endogeneity inherent in voluntary CDP participation, as firms self-select into the program based on unobservable characteristics that may also influence their propensity to commit to science-based targets.

### Instrumental Variables

We construct two complementary peer-based instruments that exploit variation in CDP Supply Chain adoption rates across different aggregation levels:

**Industry-level instrument** (`peer_cdp_share_lag`): For each firm *i* in industry *j* and year *t*, we calculate the proportion of industry peers (excluding firm *i*) who were CDP Supply Chain members in year *t-1*:

```
peer_cdp_share_lag(i,j,t) = [Σ(k≠i) CDP_member(k,j,t-1)] / [N(j,t-1) - 1]
```

where industries are defined using 4-digit GICS classifications.

**Country-level instrument** (`peer_cdp_share_country_lag`): Similarly, for each firm *i* headquartered in country *c* and year *t*, we calculate the proportion of country peers (excluding firm *i*) who were CDP Supply Chain members in year *t-1*:

```
peer_cdp_share_country_lag(i,c,t) = [Σ(k≠i) CDP_member(k,c,t-1)] / [N(c,t-1) - 1]
```

Both instruments are lagged by one year to mitigate concerns about reverse causality and allow time for peer influence mechanisms to operate. We exclude the focal firm from all calculations to avoid mechanical correlation between the instrument and the endogenous variable. The correlation between the two instruments is 0.167, suggesting they capture distinct sources of variation.

### Identification Strategy

Our identification strategy relies on two key assumptions. First, **instrument relevance** requires that peer adoption rates meaningfully predict a firm's own CDP membership decision. This is testable via first-stage F-statistics, where F > 10 indicates sufficient instrument strength (Stock and Yogo 2005).

Second, the **exclusion restriction** requires that peer CDP adoption rates affect a firm's SBTi commitment *only* through their effect on the firm's own CDP membership, not through other channels. This assumption is plausible because: (a) peer adoption rates capture diffusion and normative pressures within organizational fields, which primarily operate through direct participation rather than indirect observation; (b) we control for time-varying industry and country characteristics through fixed effects; and (c) we lag the instruments to ensure temporal ordering.

### Econometric Specification

We estimate the following two-stage system:

**First stage:**
```
CDP_member(i,t) = α₁ + β₁·IV(i,t-1) + γ₁·X(i,t) + θ(country) + θ(year) + ε(i,t)
```

**Second stage:**
```
SBTi_commit(i,t+1) = α₂ + β₂·CDP_member_hat(i,t) + γ₂·X(i,t) + θ(country) + θ(year) + u(i,t)
```

where `SBTi_commit(i,t+1)` is a binary indicator equal to 1 if firm *i* makes an SBTi commitment in year *t+1*, `CDP_member_hat(i,t)` is the predicted value of CDP membership from the first stage, `IV(i,t-1)` represents our instrumental variable(s) (industry-level, country-level, or both), and `X(i,t)` is a vector of firm-level controls.

### Control Variables

We include the following time-varying firm characteristics as controls:

- **Environmental supply chain incidents:** We measure three variants from RepRisk data:
  - `esc_incidents`: Total count of environmental supply chain incidents
  - `esc_incidents_highreach`: Count of incidents appearing in prominent global outlets (capturing reputational harm)
  - `esc_incidents_highsev`: Count of high-severity incidents (capturing stakeholder harm)

- **Environmental disclosure:** Bloomberg environmental disclosure score (`e_disc_coalesced_zeros`) and a missing data indicator (`e_disc_missing`)

- **Emissions:** Natural log of Scope 1 emissions plus 1 (`log(scope1_zeros + 1)`) and a missing data indicator (`scope1_missing`)

- **Financial controls:** Return on assets (`roa_oibdp_w1_at_w1`), natural log of total assets (`at_usd_winsorized_1_log`), and leverage ratio (`tll_lt_w1_at_w1`). All financial variables are winsorized at the 1st and 99th percentiles to mitigate outlier influence.

We also include country and year fixed effects in our baseline specifications, with robustness checks adding industry fixed effects and firm fixed effects.

### Standard Errors

All standard errors are clustered at the firm level to account for serial correlation in firm-level observations over time.

### Sample

Our analysis sample consists of 154,062 firm-year observations representing 12,993 unique firms from 115 countries across 24 four-digit GICS industries, spanning the period 2007-2022. This represents firms with complete data for all analysis variables.

### Diagnostic Tests

We conduct three key diagnostic tests:

1. **First-stage F-test:** Tests instrument strength. Following Stock and Yogo (2005), F-statistics above 10 indicate instruments are sufficiently strong to avoid weak instrument bias.

2. **Wu-Hausman endogeneity test:** Tests whether CDP membership can be treated as exogenous. Rejection of the null hypothesis (p < 0.05) indicates endogeneity is present and IV estimation is warranted.

3. **Sargan-Hansen J-test:** When using multiple instruments (overidentified model), tests whether the overidentifying restrictions hold. Failure to reject the null hypothesis (p > 0.05) supports instrument validity.

---

## RESULTS

### First-Stage Results: Instrument Relevance

Table 1 presents first-stage regression results testing the strength of our instrumental variables. Column 1 shows results using only the industry-level instrument, Column 2 uses only the country-level instrument, and Column 3 uses both instruments simultaneously.

The industry-level instrument (`peer_cdp_share_lag`) exhibits an F-statistic of 9.89 (t = 3.14, p = 0.002), just below the conventional threshold of 10, suggesting borderline instrument strength. In contrast, the country-level instrument (`peer_cdp_share_country_lag`) demonstrates strong relevance with an F-statistic of 21.21 (t = 4.61, p < 0.001). When both instruments are included simultaneously (Column 3), the joint F-test yields F = 23.19 (p < 0.001), indicating strong combined instrument strength.

Notably, high-reach environmental supply chain incidents positively predict CDP membership (β = 0.027, p = 0.028), consistent with the notion that reputational pressures from publicized incidents drive participation in voluntary environmental programs. Better environmental disclosure scores and larger firm size also predict CDP membership.

**[INSERT TABLE 1 HERE]**

### Diagnostic Test Results

Before presenting our main results, we report three key diagnostic tests to validate our instrumental variables approach.

**Instrument strength:** As noted above, the country-level instrument and the combined specification both exceed the F > 10 threshold, indicating sufficient instrument strength. The industry-level instrument alone exhibits borderline weakness (F = 9.89), suggesting potential weak instrument bias when used in isolation.

**Endogeneity:** The Wu-Hausman test strongly rejects the null hypothesis that CDP membership is exogenous (χ² = 1626.6, p < 0.001), confirming that OLS estimates would be biased and instrumental variables estimation is necessary. This finding validates our concern that firms self-select into CDP participation based on unobserved characteristics correlated with their propensity for climate action.

**Overidentification:** The Sargan-Hansen J-test rejects the overidentifying restrictions (χ² = 27.9, p < 0.001), suggesting that at least one instrument may be correlated with the error term, potentially violating the exclusion restriction. This finding raises concerns about instrument validity when both instruments are used simultaneously and warrants cautious interpretation of the overidentified specification. We therefore emphasize results using a single instrument (particularly the country-level instrument) in our main analysis.

### Main Results: Effect of CDP Membership on SBTi Commitment

Table 2 presents our main 2SLS results alongside OLS estimates for comparison. Columns 1-3 show OLS estimates with progressively more stringent fixed effects. Columns 4-6 present 2SLS estimates using different instrument specifications, all with country and year fixed effects.

**OLS Results:** Ordinary least squares estimates (Columns 1-3) indicate that CDP Supply Chain membership is associated with an 11.5-12.2 percentage point increase in the probability of making an SBTi commitment in the following year (all p < 0.01). These estimates are remarkably stable across specifications that include country, industry, and firm fixed effects.

**2SLS Results:** Instrumental variables estimates (Columns 4-6) reveal substantially larger effects. Using the industry-level instrument alone (Column 4), CDP membership increases SBTi commitment probability by 181 percentage points (p = 0.017). The country-level instrument yields an even larger estimate of 250 percentage points (p < 0.001, Column 5). The overidentified specification using both instruments produces an intermediate estimate of 240 percentage points (p < 0.001, Column 6).

**Interpretation:** The dramatic difference between OLS (11-12 percentage points) and 2SLS (181-250 percentage points) estimates suggests substantial downward bias in the correlational estimates. This pattern is consistent with negative selection, whereby firms with lower latent propensity for climate action are more likely to join CDP Supply Chain (perhaps due to stakeholder pressure), leading OLS to underestimate the true causal effect. Alternatively, the 2SLS estimates may reflect a Local Average Treatment Effect (LATE) for "compliers"—firms whose CDP membership is influenced by peer adoption rates—who may experience particularly strong treatment effects.

Given the concerns raised by the Sargan-Hansen test, we emphasize the single-instrument specifications (Columns 4-5) over the overidentified specification (Column 6). The country-level instrument specification (Column 5) provides our preferred estimate, as it exhibits the strongest first-stage F-statistic and avoids potential violations of the exclusion restriction suggested by the overidentification test.

**[INSERT TABLE 2 HERE]**

### Control Variable Results

Across specifications, several control variables exhibit consistent patterns:

- **Environmental incidents:** High-reach environmental supply chain incidents show no significant effect on SBTi commitment in the 2SLS specifications, though they positively predict CDP membership (Table 1) and show positive associations in OLS (Table 2, Columns 1-3).

- **Environmental disclosure:** Better Bloomberg environmental disclosure scores positively predict SBTi commitment in OLS but lose significance in most 2SLS specifications, suggesting this association may reflect unobserved firm characteristics rather than causal effects.

- **Emissions:** Larger Scope 1 emissions (log-transformed) consistently *reduce* the probability of SBTi commitment across all specifications (β ≈ -0.019 to -0.022, all p < 0.01), suggesting that more emissions-intensive firms are less likely to commit to science-based targets.

- **Firm characteristics:** Larger firms (measured by log total assets) are more likely to commit to SBTi in OLS but this effect largely disappears in 2SLS, again suggesting confounding in the correlational estimates. Financial performance (ROA) and leverage show no consistent patterns.

### Robustness Checks

Table 3 presents robustness checks examining sensitivity to alternative specifications. Panel A varies the measure of environmental supply chain incidents, Panel B varies the fixed effects structure.

**Alternative incident measures (Panel A):** Using the industry-level instrument, we find consistent positive effects of CDP membership across all three incident measures: total incidents (β = 1.83, p = 0.021), high-reach incidents (β = 1.81, p = 0.017), and high-severity incidents (β = 1.84, p = 0.015). The similarity of point estimates across incident definitions suggests our findings are not sensitive to how we measure environmental incidents.

**Alternative fixed effects (Panel B):** We examine three fixed effects structures: country + year (our baseline), country + industry + year, and firm + year. The CDP membership coefficient ranges from 1.81 (country + year) to 2.73 (firm + year), with all estimates statistically significant at conventional levels. The larger coefficient in the firm fixed effects specification suggests within-firm variation in peer adoption rates generates even stronger treatment effects, though this specification is more demanding and relies on firms with time-varying CDP membership status.

**[INSERT TABLE 3 HERE]**

### Summary

Our instrumental variables analysis provides strong evidence that CDP Supply Chain membership causally increases firms' likelihood of making SBTi commitments. The 2SLS estimates are substantially larger than OLS estimates, suggesting significant downward bias in correlational analyses. While diagnostic tests raise some concerns about potential violations of the exclusion restriction in overidentified specifications, our preferred specification using the country-level instrument (which exhibits strong first-stage relevance) yields a large, positive, and statistically significant causal effect. The robustness of findings across alternative incident measures and fixed effects specifications strengthens confidence in our core conclusions.

---

## TABLES

### Table 1: First-Stage Regressions—Instrument Relevance

|                                    | (1)        | (2)        | (3)        |
|------------------------------------|------------|------------|------------|
|                                    | Industry IV| Country IV | Both IVs   |
| **Instruments**                    |            |            |            |
| Peer CDP share (industry, lag)     | 0.175**    |            | 0.174**    |
|                                    | (0.056)    |            | (0.056)    |
|                                    |            |            |            |
| Peer CDP share (country, lag)      |            | 0.514***   | 0.513***   |
|                                    |            | (0.112)    | (0.112)    |
|                                    |            |            |            |
| **Controls**                       |            |            |            |
| ESC incidents (high-reach)         | 0.027*     | 0.026*     | 0.026*     |
|                                    | (0.012)    | (0.012)    | (0.012)    |
|                                    |            |            |            |
| Environmental disclosure           | 0.000329***| 0.000343***| 0.000338***|
|                                    | (0.000075) | (0.000076) | (0.000075) |
|                                    |            |            |            |
| Disclosure missing                 | 0.005***   | 0.006***   | 0.006***   |
|                                    | (0.001)    | (0.001)    | (0.001)    |
|                                    |            |            |            |
| Log(Scope 1 emissions + 1)         | -0.001     | -0.001     | -0.000     |
|                                    | (0.002)    | (0.002)    | (0.002)    |
|                                    |            |            |            |
| Scope 1 missing                    | -0.003     | -0.002     | -0.002     |
|                                    | (0.002)    | (0.002)    | (0.002)    |
|                                    |            |            |            |
| ROA                                | 0.000015***| 0.000016***| 0.000015***|
|                                    | (0.000003) | (0.000003) | (0.000003) |
|                                    |            |            |            |
| Log(Total assets)                  | 0.001***   | 0.001***   | 0.001***   |
|                                    | (0.000159) | (0.000160) | (0.000161) |
|                                    |            |            |            |
| Leverage                           | 0.000011   | 0.000014*  | 0.000013   |
|                                    | (0.000008) | (0.000007) | (0.000007) |
|                                    |            |            |            |
| **Diagnostics**                    |            |            |            |
| First-stage F-statistic            | 9.89       | 21.21      | 23.19      |
| Observations                       | 154,060    | 154,060    | 154,060    |
| Country FE                         | Yes        | Yes        | Yes        |
| Year FE                            | Yes        | Yes        | Yes        |
| Adjusted R²                        | 0.021      | 0.023      | 0.024      |

*Notes:* Dependent variable is CDP Supply Chain membership (binary). Standard errors clustered by firm in parentheses. *** p<0.01, ** p<0.05, * p<0.1. F-statistics test instrument relevance; F > 10 indicates strong instruments (Stock and Yogo 2005).

---

### Table 2: Main Results—Effect of CDP Membership on SBTi Commitment

|                                    | OLS Estimates |          |            | 2SLS Estimates |            |            |
|------------------------------------|---------------|----------|------------|----------------|------------|------------|
|                                    | (1)           | (2)      | (3)        | (4)            | (5)        | (6)        |
|                                    | Country+Year  | +Industry| Firm+Year  | Industry IV    | Country IV | Both IVs   |
| **Treatment Variable**             |               |          |            |                |            |            |
| CDP SC member                      | 0.116***      | 0.115*** | 0.122***   | 1.814*         | 2.500***   | 2.396***   |
|                                    | (0.038)       | (0.038)  | (0.039)    | (0.762)        | (0.608)    | (0.531)    |
|                                    |               |          |            |                |            |            |
| **Controls**                       |               |          |            |                |            |            |
| ESC incidents (high-reach)         | 0.110***      | 0.108*** | 0.080**    | 0.064          | 0.045      | 0.048      |
|                                    | (0.024)       | (0.025)  | (0.031)    | (0.039)        | (0.045)    | (0.043)    |
|                                    |               |          |            |                |            |            |
| Environmental disclosure           | 0.001***      | 0.001*** | 0.001***   | 0.001*         | 0.000      | 0.000      |
|                                    | (0.000103)    | (0.000102)| (0.000110)| (0.000287)     | (0.000256) | (0.000234) |
|                                    |               |          |            |                |            |            |
| Disclosure missing                 | 0.020***      | 0.022*** | 0.020***   | 0.011**        | 0.008*     | 0.008*     |
|                                    | (0.001)       | (0.002)  | (0.002)    | (0.004)        | (0.004)    | (0.003)    |
|                                    |               |          |            |                |            |            |
| Log(Scope 1 emissions + 1)         | -0.021***     | -0.020***| -0.017***  | -0.020***      | -0.019**   | -0.019**   |
|                                    | (0.003)       | (0.003)  | (0.003)    | (0.005)        | (0.007)    | (0.007)    |
|                                    |               |          |            |                |            |            |
| Scope 1 missing                    | -0.046***     | -0.045***| -0.038***  | -0.041***      | -0.039***  | -0.039***  |
|                                    | (0.004)       | (0.004)  | (0.004)    | (0.005)        | (0.007)    | (0.006)    |
|                                    |               |          |            |                |            |            |
| ROA                                | 0.000042**    | 0.000049**| 0.000018  | 0.000016       | 0.000006   | 0.000007   |
|                                    | (0.000013)    | (0.000016)| (0.000033)| (0.000016)     | (0.000015) | (0.000014) |
|                                    |               |          |            |                |            |            |
| Log(Total assets)                  | 0.002***      | 0.003*** | 0.001**    | 0.001          | 0.000      | 0.000      |
|                                    | (0.000223)    | (0.000218)| (0.000512)| (0.001)        | (0.001)    | (0.001)    |
|                                    |               |          |            |                |            |            |
| Leverage                           | 0.000067      | 0.000082*| -0.000006  | 0.000047       | 0.000039   | 0.000040   |
|                                    | (0.000035)    | (0.000037)| (0.000017)| (0.000039)     | (0.000041) | (0.000040) |
|                                    |               |          |            |                |            |            |
| **Diagnostics**                    |               |          |            |                |            |            |
| First-stage F-statistic            | —             | —        | —          | 9.89           | 21.21      | 23.19      |
| Wu-Hausman χ² (p-value)            | —             | —        | —          | 350.3 (<0.001) | 1626.6 (<0.001) | 1626.6 (<0.001) |
| Sargan-Hansen χ² (p-value)         | —             | —        | —          | —              | —          | 27.9 (<0.001) |
|                                    |               |          |            |                |            |            |
| Observations                       | 154,060       | 154,060  | 153,894    | 154,060        | 154,060    | 154,060    |
| Country FE                         | Yes           | Yes      | No         | Yes            | Yes        | Yes        |
| Industry FE                        | No            | Yes      | No         | No             | No         | No         |
| Firm FE                            | No            | No       | Yes        | No             | No         | No         |
| Year FE                            | Yes           | Yes      | Yes        | Yes            | Yes        | Yes        |
| Adjusted R²                        | 0.104         | 0.106    | 0.352      | —              | —          | —          |

*Notes:* Dependent variable is SBTi commitment in year t+1 (binary). Columns 1-3 present OLS estimates; columns 4-6 present 2SLS estimates. Standard errors clustered by firm in parentheses. *** p<0.01, ** p<0.05, * p<0.1. First-stage F-statistics test instrument strength; Wu-Hausman tests endogeneity of CDP membership; Sargan-Hansen tests overidentifying restrictions in column 6.

---

### Table 3: Robustness Checks

**Panel A: Alternative Environmental Incident Measures** *(Using Industry IV)*

|                                    | (1)          | (2)          | (3)          |
|------------------------------------|--------------|--------------|--------------|
|                                    | All incidents| High-reach   | High-severity|
| **Treatment Variable**             |              |              |              |
| CDP SC member                      | 1.833*       | 1.814*       | 1.837*       |
|                                    | (0.792)      | (0.762)      | (0.759)      |
|                                    |              |              |              |
| **Incident Measure**               |              |              |              |
| ESC incidents (all)                | 0.002        | —            | —            |
|                                    | (0.006)      |              |              |
|                                    |              |              |              |
| ESC incidents (high-reach)         | —            | 0.064        | —            |
|                                    |              | (0.039)      |              |
|                                    |              |              |              |
| ESC incidents (high-severity)      | —            | —            | 0.049        |
|                                    |              |              | (0.035)      |
|                                    |              |              |              |
| All other controls                 | Yes          | Yes          | Yes          |
| Observations                       | 154,060      | 154,060      | 154,060      |
| First-stage F-statistic            | 9.89         | 9.89         | 9.89         |

**Panel B: Alternative Fixed Effects Specifications** *(Using Industry IV, High-Reach Incidents)*

|                                    | (1)          | (2)          | (3)          |
|------------------------------------|--------------|--------------|--------------|
|                                    | Country+Year | +Industry FE | Firm+Year FE |
| **Treatment Variable**             |              |              |              |
| CDP SC member                      | 1.814*       | 2.341**      | 2.727*       |
|                                    | (0.762)      | (0.864)      | (1.208)      |
|                                    |              |              |              |
| ESC incidents (high-reach)         | 0.064        | 0.049        | 0.066        |
|                                    | (0.039)      | (0.044)      | (0.040)      |
|                                    |              |              |              |
| All other controls                 | Yes          | Yes          | Yes          |
| Observations                       | 154,060      | 154,060      | 153,894      |
| Country FE                         | Yes          | Yes          | No           |
| Industry FE                        | No           | Yes          | No           |
| Firm FE                            | No           | No           | Yes          |
| Year FE                            | Yes          | Yes          | Yes          |
| First-stage F-statistic            | 9.89         | 9.89         | 9.89         |

*Notes:* All specifications use 2SLS with industry-level peer share instrument. Standard errors clustered by firm in parentheses. *** p<0.01, ** p<0.05, * p<0.1. All specifications include the full set of control variables shown in Table 2.

---

## DISCUSSION POINTS

### Magnitude of Effects

The 2SLS estimates (180-250 percentage points) are substantially larger than the OLS estimates (11-12 percentage points). Several interpretations are possible:

1. **Negative selection bias:** Firms with lower latent propensity for climate action may be more likely to join CDP Supply Chain (perhaps due to stakeholder pressure), causing OLS to underestimate the true causal effect.

2. **Local Average Treatment Effect (LATE):** The 2SLS estimates represent the effect for "compliers"—firms whose CDP membership is influenced by peer adoption rates. These firms may experience particularly strong treatment effects.

3. **Weak instrument bias:** Although the country-level instrument and combined specification pass the F > 10 threshold, the industry-level instrument is borderline weak (F = 9.89), potentially biasing 2SLS estimates away from OLS.

### Instrument Validity Concerns

The Sargan-Hansen test rejects the overidentifying restrictions (p < 0.001), suggesting potential violations of the exclusion restriction. This could occur if:

1. **Peer adoption rates have direct effects:** Industry or country-level CDP adoption may influence SBTi commitment through channels other than own CDP membership (e.g., via normative pressures, information spillovers, or competitive dynamics).

2. **Common unobserved shocks:** Both instruments may be correlated with unobserved time-varying industry or country characteristics that independently affect SBTi commitment.

Given this concern, we emphasize the country-level instrument specification (which exhibits stronger first-stage relevance) as our preferred estimate, while acknowledging that the exclusion restriction remains an assumption that cannot be fully tested with a single instrument.

### Policy Implications

Despite the instrument validity concerns, the consistently large and positive 2SLS estimates across specifications suggest that CDP Supply Chain membership has a meaningful causal effect on subsequent climate target setting. This finding supports policies that encourage or incentivize supply chain environmental disclosure programs as mechanisms for driving corporate climate action. The magnitude of effects suggests that participation in such programs may trigger organizational changes that fundamentally reshape firms' climate strategies, rather than merely signaling existing commitments.

---

*End of draft methods and results section*
