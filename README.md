# burn_vp

Comparison of vasopressor use in patients with severe burns.

## Layout

- `R/`: reusable functions
- `analysis/`: analysis scripts and reports
- `scripts/`: standalone utility scripts, not part of the analysis
- `data/raw/`, `data/processed/`: local data only, ignored by git
- `output/`: generated figures and tables, ignored by git
- `tests/testthat/`: unit tests
- `PROJECT_VERSION`: project name and version; scripts use it to recognise the project folder

## Data handling

Patient-level data must stay out of version control. Only code and de-identified aggregate results belong in this repository.

## Requirements

R 4.3.3. Packages are pinned in `renv.lock`; run `renv::restore()` after cloning.
