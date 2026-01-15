# How to Run Your IV Analysis

## Quick Start (Recommended)

Since you already have the instrument dataset, use the **standalone script**:

```r
# In R or RStudio, from the essay3 directory:
setwd("~/path/to/essay3")  # Adjust path as needed
source("scripts/run_iv_analysis.R")
```

This will:
- Load your existing `cleaned_data/complete_data_2022_instrument.rds`
- Run the complete 2SLS analysis
- Print results to console with interpretations
- Export markdown tables to `tables/`

**Expected output:**
- Console output showing whether instruments are strong and valid
- Whether the CDP effect is significant
- 5 markdown files in `tables/` folder

---

## What You Have

### Files Created

1. **`scripts/cdp_scp_peer_share_instrument.R`** (UPDATED)
   - Full pipeline: data prep → instrument creation → 2SLS analysis
   - Use this if you need to recreate instruments from scratch
   - Requires all input data files in Dropbox folder

2. **`scripts/run_iv_analysis.R`** (NEW - USE THIS!)
   - Standalone script for quick results
   - Only needs `cleaned_data/complete_data_2022_instrument.rds`
   - No Dropbox dependencies

### New Instruments Added

Both instruments are already in your `complete_data_2022_instrument.rds`:

1. **`peer_cdp_share_lag`** (ORIGINAL)
   - Industry-based peer instrument
   - Share of firms in same industry-year that are CDP members

2. **`peer_cdp_share_country_lag`** (NEW)
   - Country-based peer instrument
   - Share of firms in same country-year that are CDP members

---

## Understanding the Output

When you run the script, you'll see:

### 1. First-Stage Results
Shows whether instruments predict CDP membership

### 2. F-Test (Instrument Strength)
```
F-statistic = X.XX
✓ PASS: Instruments are strong (if F > 10)
✗ WARNING: Weak instruments (if F < 10)
```

**What F > 10 means:** Your instruments have good predictive power, so the 2SLS estimates are reliable.

### 3. Second-Stage Results (THE MAIN RESULT)
```
Effect of CDP Supply Chain Membership on SBTi Commitment:
  Coefficient:   X.XXXX
  p-value:       X.XXX
  ✓ SIGNIFICANT / ✗ NOT SIGNIFICANT
```

**Interpretation:**
- **Positive coefficient + p < 0.05:** CDP membership causally increases SBTi commitment
- **Negative coefficient + p < 0.05:** CDP membership causally decreases SBTi commitment
- **p > 0.05:** No significant causal effect detected

### 4. Sargan-Hansen J-Test (Instrument Validity)
```
p-value = X.XXX
✓ PASS: Instruments appear valid (if p > 0.05)
✗ WARNING: Possible invalid instrument (if p < 0.05)
```

**What p > 0.05 means:** No evidence that your instruments are correlated with the error term (good!)

---

## Interpreting Your Results

### Ideal Scenario (all tests pass):
- ✓ F-statistic > 10 (strong instruments)
- ✓ J-test p > 0.05 (valid instruments)
- ✓ Main effect p < 0.05 (significant causal relationship)

**Conclusion:** You have credible causal evidence that CDP membership affects SBTi commitment.

### If F < 10 (weak instruments):
- Your 2SLS estimates may be biased toward OLS
- Consider finding stronger instruments or using different methods
- Results are less reliable

### If J-test p < 0.05 (invalid instruments):
- At least one instrument may be correlated with unobservables
- Exclusion restriction may be violated
- Consider whether instruments truly only affect outcome through CDP membership

---

## Exported Tables

After running, check `tables/` for:

1. `first_stage_results.md` - First-stage coefficients
2. `instrument_strength.md` - F-test diagnostics
3. `second_stage_results.md` - Main 2SLS coefficient
4. `sargan_hansen_test.md` - Overidentification test
5. `iv_diagnostics_summary.md` - Overall summary

These are formatted in markdown for easy copying into papers/presentations.

---

## Viewing Your Code

Your updated code is in two places:

1. **On your local machine:** Open `scripts/cdp_scp_peer_share_instrument.R` in any text editor
2. **On GitHub:** Visit https://github.com/williamdiebel/essay3/tree/claude/review-iv-analysis-y2rf6

---

## About Pull Requests (PR)

**Do NOT create a PR yet.**

Pull Requests are for merging code between branches. Right now you're just analyzing data on your feature branch.

**When to create a PR:**
- After you've verified the analysis works
- When you want to merge this code into your `main` branch
- If you want someone to review the code

**For now:** Just run the analysis and see the results!

---

## Questions?

**Q: I got an error about missing packages**
A: Install them with:
```r
install.packages(c("tidyverse", "fixest", "lfe", "AER", "sandwich", "lmtest"))
```

**Q: I got an error about missing data files**
A: Make sure you're in the `essay3` directory and `cleaned_data/complete_data_2022_instrument.rds` exists.

**Q: How do I interpret a coefficient of 0.0523?**
A: This means CDP membership increases the probability of SBTi commitment by 5.23 percentage points.

**Q: What if my instruments are weak (F < 10)?**
A: Your 2SLS estimates may not be reliable. Consider:
- Finding stronger instruments
- Using different peer definitions
- Trying alternative identification strategies
