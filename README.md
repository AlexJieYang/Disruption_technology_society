# Disruption, technology, and society

Research code for **Are disruptive papers more likely to impact technology and
society?**, by Alex J. Yang, Xiaohui Yan, Haotian Hu, Hanlin Hu, Jia Kong, and
Sanhong Deng. The scripts relate disruption measures to patent, clinical-trial,
news, and tweet citations.

## Workflow

Set `data_dir` for the paper-level analysis and field lookup tables, and
`regression_dir` for the study's year/field regression tables.

| Entry point | Analysis |
| --- | --- |
| `fig 3.ipynb` | Descriptive counts and probabilities by year and field |
| `reg 1.R` | Main and 5-/10-year robustness regressions |
| `fig 4.ipynb` | Year-specific regression figures |
| `fig 5.ipynb` | Field-specific regression figures |

Annual totals and positive-citation subsets are aligned by `Year` over
1950–2020. Field labels in figure 3 follow the field lookup table. Regression
figures parse coefficient estimates, standard errors, and significance markers;
their intervals use `1.96 × SE`. Year and field grouping use the
`Sample (Year)` and `Sample (FieldID)` labels in the corresponding tables.

For regression, install R packages `fixest`, `data.table`, and `dplyr`, start R
in the repository root, and use `source("reg 1.R")`.
`DISRUPTION_ANALYSIS_CSV` selects the paper-level CSV (default `data/P4Reg.csv`).
`DISRUPTION_OUTPUT_DIR` selects the destination (default `outputs/regression`).
Main tables are written there as `reg11–14.csv` and `reg21–24.csv`; window-specific
tables are written under `Robust/1/` and `Robust/2/`.

`Supplimentary material.pdf` contains the accompanying research material.

## Installation and configuration

Use Python 3.10 or 3.11. Create and activate a virtual environment with the command
for your operating system, then install the packages:

```bash
python -m venv .venv
python -m pip install -r requirements.txt
```

Copy `config.example.json` to `config.json`. Set `data_dir` to the study-data
directory and `output_dir` to the figure destination. Repositories with regression
figures also use `regression_dir` for regression tables. Relative paths resolve
from the repository root; absolute paths are accepted. `RESEARCH_CONFIG` selects
another configuration file.

Start `jupyter lab` inside the repository and open the desired notebook. Each
Python notebook includes its own initialization and data loading. The output
directories are created automatically. R entry points use the environment
variables described in their workflow sections.

## Source validation

```bash
python scripts/check_sources.py
```

This command validates Python syntax, notebook JSON, configuration JSON, and
empty notebook execution state. The GitHub Actions workflow performs the same
static checks, including R parsing when R is available. R entry points use an
R installation and their listed packages.

## Citation and license

See `CITATION.cff` for repository metadata and cite the associated research.
The MIT license covers code and documentation. Data, publications, and
third-party materials retain their respective source rights.

## Research abstract

In exploring the intersection of scholarly research with technological advancement and societal impact, our analysis delves into a vast repository of nearly 40 million research papers from the Microsoft Academic Graph (MAG), spanning 1950 to 2020 across 19 fields and 39,893 journals. Papers categorized with a lower disruption index, often considered transformative, demonstrate a weakened correlation with technological and societal domains. This hints at a bias against groundbreaking scientific pursuits. To unravel this trend, we introduce the concept of "disruptive citation," a nuanced metric gauging the absolute disruptive impact of papers. Notably, papers drawing higher disruptive citations exhibit a significant capacity to influence both technological advancements and societal spheres. Upon examining the heterogeneity across years and fields, we identify a bias against the disruption index predominantly in the last two decades and within STEM fields. However, the positive effects of disruptive impact remain consistent across all years and fields. These empirical findings illuminate the intricate relationship between disruptive scientific endeavors and their consequential influence on technology and society. By shedding light on these dynamics, our study seeks to enrich discussions regarding the recognition and role of groundbreaking scientific endeavors in shaping our world.

Keywords
Science of Science; Public science; Societal impact; Disruption; Disruptive citation.
