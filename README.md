# burn_vp

Comparison of vasopressor use in patients with severe burns.

## Installation on Windows

You need Windows, R 4.5 (any 4.5.x version) and an internet connection. You do not install or change R. Setup checks the R version. If it does not fit, setup stops and shows a message. Then contact the project team.

Before you start, the project folder must be on your own computer, for example in `C:\burn_vp`. Use a short path without spaces. Do not use OneDrive. Setup.bat stops if the path has a space.

1. Open the project folder and double-click `setup.bat`.
2. If Windows shows "Windows protected your PC", click More info, then Run anyway.
3. Wait. The first run takes several minutes and needs internet. Do not close the window.
4. When the window says SETUP COMPLETE, you are done. You do not need to run it again.
5. If the window says SETUP FAILED, send the log file to the project team. You find it in the folder `output\setup`.

Setup removes its temporary files when it ends. It keeps the log file and the installed packages (in `renv\library`).

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

R 4.5 (any 4.5.x version). Packages are pinned in `renv.lock`. Windows users: see "Installation on Windows" above. Developers: run `renv::restore()` after cloning.
