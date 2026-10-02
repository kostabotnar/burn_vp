# burn_vp

Comparison of vasopressor use in patients with severe burns.

## Layout

- `R/`: reusable functions
- `analysis/`: analysis scripts and reports
- `data/raw/`, `data/processed/`: local data only, ignored by git
- `output/`: generated figures and tables, ignored by git
- `tests/testthat/`: unit tests

## Data handling

Patient-level data must stay out of version control. Only code and de-identified aggregate results belong in this repository.

## Requirements

R (version to be pinned with `renv`).
