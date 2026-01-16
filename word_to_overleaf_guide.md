# Guide: Converting Word Document to Overleaf

## Automated Approaches (Recommended)

### Option 1: Pandoc Conversion (Best for preserving structure)

**Step 1: Install Pandoc** (if not already installed)
```bash
# macOS
brew install pandoc

# Or download from https://pandoc.org/installing.html
```

**Step 2: Convert Word to LaTeX**
```bash
# Basic conversion
pandoc your_manuscript.docx -o manuscript.tex

# With better citation handling
pandoc your_manuscript.docx --bibliography=references.bib -o manuscript.tex

# With specific template
pandoc your_manuscript.docx --template=article.tex -o manuscript.tex
```

**Step 3: Upload to Overleaf**
1. Go to Overleaf (overleaf.com)
2. Click "New Project" → "Upload Project"
3. Upload the generated `.tex` file
4. Upload any figures/tables separately
5. Fix any formatting issues in Overleaf editor

### Option 2: Overleaf Direct Upload (Easiest but less control)

**Step 1: Save Word doc as RTF**
- In Word: File → Save As → Format: Rich Text Format (.rtf)

**Step 2: Upload to Overleaf**
1. Log into Overleaf
2. Create New Project → Upload Project
3. Drag and drop your .rtf file
4. Overleaf will automatically convert

**Limitations**:
- Tables may not convert perfectly
- Complex formatting might be lost
- Citations need manual fixing

### Option 3: Automated Script (For your specific manuscript)

I can create a Python script that:
1. Extracts content from your Word doc
2. Formats it in LaTeX
3. Inserts your new regression tables
4. Creates a complete Overleaf-ready project

Would you like me to create this script?

---

## Manual Approach (Most control, but slower)

### Step 1: Prepare Your Content

1. **Extract text sections** from Word:
   - Introduction
   - Literature Review
   - Methods
   - Results
   - Discussion
   - Conclusion

2. **Extract tables**: Copy to Excel/CSV for easier conversion

3. **Save figures**: Export as PDF or PNG

### Step 2: Create LaTeX Structure

```latex
\documentclass[12pt]{article}

% Packages
\usepackage{graphicx}
\usepackage{booktabs}
\usepackage{amsmath}
\usepackage{natbib}
\usepackage[margin=1in]{geometry}
\usepackage{setspace}
\doublespacing

\title{Your Title}
\author{Your Name}
\date{\today}

\begin{document}

\maketitle

\begin{abstract}
Your abstract here...
\end{abstract}

\section{Introduction}
Your introduction text...

\section{Literature Review}
...

\section{Methods}

\subsection{Data}
...

\subsection{Instrumental Variables Strategy}
...

\section{Results}

\subsection{First-Stage Results}

\begin{table}[htbp]
\centering
\caption{First-Stage Regression Results}
\label{tab:firststage}
\begin{tabular}{lcccc}
\toprule
Variable & Coefficient & SE & t-stat & p-value \\
\midrule
Country peer CDP share (t-1) & 0.534 & 0.144 & 3.70 & $<0.001$ \\
Environmental disclosure (t-2) & 0.0003 & 0.0001 & 3.69 & $<0.001$ \\
Industry incidents excl. own (t-1) & $-0.0002$ & 0.0001 & $-1.45$ & 0.147 \\
\midrule
Joint F-statistic & \multicolumn{4}{c}{8.51***} \\
Weak instruments test & \multicolumn{4}{c}{102.7***} \\
\bottomrule
\end{tabular}
\begin{tablenotes}
\small
\item Standard errors clustered at firm level. *** $p<0.001$, ** $p<0.01$, * $p<0.05$
\end{tablenotes}
\end{table}

\subsection{Second-Stage Results}

\begin{table}[htbp]
\centering
\caption{Second-Stage 2SLS Results}
\label{tab:secondstage}
\begin{tabular}{lcc}
\toprule
Variable & Model 1 & Model 2 \\
 & (Just-ID) & (Over-ID) \\
\midrule
CDP SC membership (fitted) & 2.149*** & 2.161*** \\
 & (0.653) & (0.500) \\
\midrule
Environmental incidents & 0.058 & 0.058 \\
 & (0.043) & (0.044) \\
% ... more rows
\midrule
Country FE & Yes & Yes \\
Year FE & Yes & Yes \\
\midrule
Observations & 128,244 & 128,244 \\
Firms & 10,970 & 10,970 \\
\midrule
Weak instruments test & - & 102.7*** \\
Sargan-Hansen J (p-value) & - & 0.752 \\
\bottomrule
\end{tabular}
\begin{tablenotes}
\small
\item Standard errors (clustered at firm level) in parentheses.
\item *** $p<0.001$, ** $p<0.01$, * $p<0.05$
\end{tablenotes}
\end{table}

\section{Discussion}
...

\bibliographystyle{apalike}
\bibliography{references}

\end{document}
```

### Step 3: Create Regression Tables in LaTeX

I can generate publication-ready LaTeX tables from your R results:

**Create this R script** (`generate_latex_tables.R`):

```r
library(stargazer)
library(fixest)

# Load your models (from script 04)
# Assuming model_1 and model_2 are already in environment

# Generate LaTeX table
stargazer(model_1, model_2,
          title = "Second-Stage 2SLS Results",
          column.labels = c("Just-ID", "Over-ID"),
          covariate.labels = c("CDP SC membership (fitted)",
                               "Environmental incidents",
                               "Environmental disclosure",
                               "Disclosure missing",
                               "Log(Scope 1 + 1)",
                               "Emissions missing",
                               "ROA",
                               "Log(Total assets)",
                               "Leverage"),
          add.lines = list(c("Country FE", "Yes", "Yes"),
                          c("Year FE", "Yes", "Yes"),
                          c("Weak instruments F", "-", "102.7***"),
                          c("Sargan J (p-value)", "-", "0.752")),
          type = "latex",
          out = "table_secondstage.tex")
```

---

## Quick Start: Automated Conversion Script

Save this as `convert_to_overleaf.sh`:

```bash
#!/bin/bash

# Step 1: Convert Word to LaTeX using pandoc
echo "Converting Word document to LaTeX..."
pandoc manuscript.docx -o manuscript.tex --standalone

# Step 2: Create project structure
mkdir -p overleaf_project/{figures,tables}
mv manuscript.tex overleaf_project/

# Step 3: Copy any figures
cp -r figures/* overleaf_project/figures/ 2>/dev/null || echo "No figures to copy"

# Step 4: Create zip for Overleaf upload
cd overleaf_project
zip -r ../overleaf_project.zip .
cd ..

echo "Done! Upload overleaf_project.zip to Overleaf"
echo "Or use individual files in overleaf_project/ folder"
```

**Usage**:
```bash
chmod +x convert_to_overleaf.sh
./convert_to_overleaf.sh
```

---

## Recommended Workflow for Your Manuscript

### Phase 1: Prepare Tables (R → LaTeX)

1. Add to your `scripts/04_test_optimal_instruments.R`:

```r
# At the end of the script, add:

# Export table for Overleaf
library(stargazer)

# Second-stage results table
stargazer(model_1, model_2,
          type = "latex",
          out = "../tables/table_2sls_results.tex",
          title = "Second-Stage 2SLS Results",
          column.labels = c("Just-ID", "Over-ID"),
          add.lines = list(
            c("Instruments", "Country peer", "Country + Hist + Ind"),
            c("First-stage F", "14.4", "8.51"),
            c("Weak instruments test", "-", "102.7***"),
            c("Sargan J (p-value)", "-", "0.752"),
            c("Wu-Hausman (p-value)", "-", "<0.001***")
          ))

print("LaTeX table saved to tables/table_2sls_results.tex")
```

### Phase 2: Convert Word Document

**Option A - Pandoc** (recommended):
```bash
pandoc your_manuscript.docx -o manuscript.tex --bibliography=references.bib
```

**Option B - Overleaf direct**:
1. Save Word doc as RTF
2. Upload to Overleaf

### Phase 3: Integrate Tables

In your LaTeX manuscript, add:

```latex
\input{tables/table_2sls_results.tex}
```

### Phase 4: Final Cleanup in Overleaf

1. Fix citations (convert to BibTeX format)
2. Adjust table positioning
3. Add figure references
4. Format according to journal requirements

---

## Tools Reference

### Pandoc Installation
```bash
# macOS
brew install pandoc

# Windows
choco install pandoc

# Linux
sudo apt-get install pandoc
```

### Useful Pandoc Options

```bash
# With bibliography
pandoc manuscript.docx --bibliography=references.bib --csl=apa.csl -o manuscript.tex

# With reference docx for styling
pandoc manuscript.docx --reference-doc=template.docx -o manuscript.tex

# Extract media (figures)
pandoc manuscript.docx --extract-media=./media -o manuscript.tex
```

### LaTeX Table Generators

- **Manual**: https://www.tablesgenerator.com/latex_tables
- **From CSV**: https://www.latex-tables.com/
- **R stargazer**: For regression tables
- **R xtable**: For custom tables

---

## Next Steps

**Tell me**:
1. Where is your Word document located? (I can convert it)
2. Do you want me to create a custom conversion script for your specific manuscript?
3. Do you have any figures/tables already prepared that need to be included?

I can automate the entire process to get you a publication-ready Overleaf project!
