# burn_vp

Comparison of vasopressor use in patients with severe burns.

## Installation for NBC users

You need Windows, R 4.5.2 and an internet connection. Another R version will not work. If R 4.5.2 is not installed, get it from https://cran.r-project.org/bin/windows/base/old/4.5.2/

1. Download the project: on the GitHub page click Code, then Download ZIP. Or use the zip file you were sent.
2. Right-click the zip file and choose Properties. Tick Unblock and click OK. Do this before you extract the zip. Otherwise Windows shows warnings.
3. Extract the zip to a short folder on your own computer, for example `C:\burn_vp`. Do not use OneDrive.
4. Open the extracted folder and double-click `setup.bat`.
5. If Windows shows "Windows protected your PC", click More info, then Run anyway.
6. Wait. The first run takes several minutes and needs internet. Do not close the window.
7. When the window says SETUP COMPLETE, you are done. You do not need to run it again.
8. If the window says SETUP FAILED, send the log file to the project team. You find it in the folder `output\setup`.

If `setup.bat` is blocked: open R (Rgui), choose File, then Source R code..., and select `scripts\setup_environment.R` in the project folder. The result is the same, but R's temporary files then go to the normal Windows temporary folder.

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

R 4.5.2. Packages are pinned in `renv.lock`. NBC users: see "Installation for NBC users" above. Developers: run `renv::restore()` after cloning.
