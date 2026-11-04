# burn_vp

Comparison of vasopressor use in patients with severe burns.

## Installation on Windows

You need Windows, R 4.5 (any 4.5.x version) and an internet connection. You do not install or change R. The start file checks the R version. If it does not fit, it stops and shows a message. Then contact the project team.

Before you start, the project folder must be on your own computer, for example in `C:\burn_vp`. Use a short path. Do not use OneDrive.

1. Open R (Rgui) from the Start menu.
2. In R, choose File, then Source R code..., and select `start_burn_vp.R` in the project folder.
3. The first time, a window asks whether to run the one-time setup. Click Yes. Wait: it takes several minutes and needs internet. Do not close R.
4. When the R window (the console) says SETUP COMPLETE, the program window opens. You do not need to run the setup again.
5. If the R window says SETUP FAILED, send the log file to the project team. You find it in the folder `output\setup`.

The next times, repeat steps 1 and 2. The program window opens straight away.

Setup removes its temporary files when it ends. It keeps the log file and the installed packages (in `renv\library`). R's own temporary files stay in the normal Windows temporary folder, where R removes them.

## Running the quality control pipeline

The quality control (QC) pipeline checks one or more site data files in the NBC template layout and writes QC tables, a data profile, figures and an HTML report. Run setup first (see above).

The input is an Excel file (.xlsx). The pipeline reads the first sheet. Row 1 is free text, row 2 holds the column headers, and the data starts in row 3. The headers must match `metadata/codebook.csv` in name and order; if they do not, the pipeline stops and lists every difference. Put the file in `data\raw\` in the project folder, which git ignores.

### Using the program window

1. Start the program as described in "Installation on Windows": open R, choose File, then Source R code..., and select `start_burn_vp.R`. The program window opens. Keep R open while you work. When you close the program window, you return to R.
2. Click Add files... and select one or more Excel files.
3. Check the site name next to each file. It is taken from the file name up to the first underscore (`NBC_2026.xlsx` gives NBC). You can change it. Each file needs a different site name.
4. Click Run QC. The bar and the line below it show the progress. The window does not respond while a step runs; this is normal.
5. When the run is finished, a message shows the results folder. Click Open report to see the report in your web browser.

If something is wrong with a file, for example a column header, a message explains the problem and no results are written. To remove a file from the list, tick it and click Remove ticked files. The button Run analysis is not available yet. If the R window says the program stopped with an error, send a screenshot of it to the project team.

### Using the command line

This section is for the project team (developers). Workstations at Norway Burning Center may not allow the command prompt.

1. Open Command Prompt (press Windows, type `cmd`, press Enter).
2. Go to the project folder, for example `cd C:\burn_vp`.
3. Run the pipeline. Replace `4.5.2` with your R 4.5 version if it differs, and use your own file name:

   ```
   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts\run_qc_pipeline.R NBC=data\raw\NBC.xlsx
   ```

4. When it finishes, the window shows `Report:` followed by the path of `report.html`. Open that file in a web browser.

Each `SITE=path` argument gives a site name and its file. To check several sites in one run, list them all, for example `NBC=data\raw\NBC.xlsx UTMB=data\raw\UTMB.xlsx`. Relative paths are read from the project folder, so the command also works from another folder if you give the full path to `scripts\run_qc_pipeline.R`. Add `--label text` at the end to add a label to the run folder name.

Each run writes a new folder `output\runs\<date>_<time>[_<label>]\` with:

- `report.html`: the summary report
- `qc\`: QC findings, rule summary, missingness, values that could not be read, unrecognised category values, the recode log and the rule catalogue (CSV)
- `profile\`: variable summaries, site comparison, exposures and outcomes, correlations and counts of values masked after QC (CSV)
- `figures\`: the figures in the report (PNG)

The QC tables list row numbers and the original values from the data file, so treat the run folder as patient-level data and do not share or commit it. The data rows are numbered from the first data row (row 3 in Excel is row 1).

To try the pipeline without real data, generate a synthetic file with known errors and run the pipeline on it:

```
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts\generate_synthetic_nbc.R 100 1 --corrupt
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts\run_qc_pipeline.R NBC=output\generated\NBC_synthetic_n100_seed1_corrupt.xlsx --label demo
```

## Layout

- `R/`: reusable functions
- `analysis/`: analysis scripts and reports
- `scripts/`: standalone utility scripts, not part of the analysis
- `data/raw/`, `data/processed/`: local data only, ignored by git
- `output/`: generated figures and tables, ignored by git
- `tests/testthat/`: unit tests
- `start_burn_vp.R`: the start file for clinicians; source it in Rgui to run setup when needed and open the program window
- `PROJECT_VERSION`: project name and version; scripts use it to recognise the project folder

## Data handling

Patient-level data must stay out of version control. Only code and de-identified aggregate results belong in this repository.

## Requirements

R 4.5 (any 4.5.x version). Packages are pinned in `renv.lock`. Windows users: see "Installation on Windows" above. Developers: run `renv::restore()` after cloning.
