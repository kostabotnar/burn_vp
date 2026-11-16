# Project instructions

## File output

- Code in this project writes files only inside the `output/` folder. It must not write to the home folder, the Desktop, the temporary folder, or any other location.
- This includes test or probe files used to check whether a folder is writable.
- Locate `output/` relative to the project root, not the working directory or the home folder.
- If `output/` does not exist, create it at the project root. Never create it anywhere else.
- `PROJECT_VERSION` at the root identifies the project folder; code locates the root by it and must not write if it is not found.
- Approved exceptions: renv writes `renv/library` and `renv/staging`. R's own session temporary folder stays in the Windows temporary folder when R is started from Rgui. Project code must not use `tempdir()` or `tempfile()` for its own files.

## Analysis data

- Every analysis, including profiling and the burn index, runs on de-identified data only (for UTMB: the `data/` folder of an `output/utmb/deid_<timestamp>` run). Never run an analysis on the raw extract in `data/`.
- If no de-identified data exists, do not run the analysis. Run the de-identification pipeline (`scripts/utmb/deid_utmb.R`) first, or ask the user to run it.
- The de-identification pipeline itself is the only code that reads the raw extract.
