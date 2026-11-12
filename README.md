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

## Profiling the UTMB data

The raw UTMB extract (13 comma-separated files without an extension, from Epic Caboodle) goes in `data/`, which is ignored by version control. To profile it:

```
Rscript scripts/utmb/profile_utmb.R [--data <folder>] [--label <text>]
```

The default data folder is `data/` in the project folder; a relative `--data` path is resolved against the project folder, not the working directory. The run writes `output/utmb/profile_<date>_<time>[_<label>]/` with `column_profile.csv`, `table_stats.csv` (including read and profile seconds per table), `demographics_categorical.csv`, `demographics_numeric.csv` and `summary.txt` and `utmb_dictionary.csv`. These hold aggregate counts, but some columns list the most frequent values, so keep the run folder out of version control. `utmb_dictionary.csv` is a structure-only dictionary (column types, roles, identifier flags and descriptions, no data values). The script never writes outside `output/`; to update the committed copy `metadata/utmb_dictionary.csv`, review the file and copy it there by hand. The functions are in `R/utmb/`.

## Burn index events in the UTMB data

```
Rscript scripts/utmb/burn_index.R [--data <folder>] [--label <text>]
```

Burn records are `diagnoses` rows with ICD-10-CM `T31.*` or ICD-9-CM `948.*`. The index event is each patient's earliest burn record dated 2015-01-01 to 2023-12-31 (highest group when several records share the date). The group is the first digit after the dot: percent of body surface area burned in tenths (0 = under 10%, 9 = 90% or more). The run writes `output/utmb/burn_index_<date>_<time>[_<label>]/` with group, year and month distributions, demographics, index encounter types, burn records before the index event, a comparison with the crosswalk, `figures/` and `summary.txt`. Smoking status and race are the extract's current values, not values at the index date.

`index_events.csv` in the run folder is patient-level (one row per StudyID, with index date, code, encounter and age). It stays in `output/`, which is ignored by version control, and must not be shared. All other files are aggregate counts. The functions are in `R/utmb/burn_index.R`.

## De-identifying the UTMB data

```
Rscript scripts/utmb/deid_utmb.R [--data <folder>] [--label <text>] [--patient-key <column>] [--keys <folder>]
```

Patients get a new number. `--patient-key` names the column that identifies a patient in every table (default `PatientDurableKey`). The extract's own `StudyID` is not used or kept, because it is not known how it was assigned. Before anything is written the script checks that `demographics` exists and has exactly one row per patient (the key is never missing and never repeated), that every table has the key column, and that every key in another table is in `demographics`. If a check fails the script stops with table names and counts and writes nothing. Only the key column of each table is read for these checks. The patients of `demographics` are then numbered 1 to N in a random order (sorted by SHA-256 of the secret, "patient_id" and the key), and the number is written as the first column `StudyID` of every output table, so `profile_utmb.R` and `burn_index.R` work unchanged.

The run writes two folders under `output/utmb/`. `deid_<date>_<time>[_<label>]/` holds `data/` (the 13 tables with the same file names and format, so `profile_utmb.R --data output/utmb/deid_.../data` and `burn_index.R` work on them), `summary.txt` and `column_actions.csv` (counts only). `deid_keys_<date>_<time>[_<label>]/` holds `secret.txt` (the random run secret), `patients.csv` (new `StudyID`, the patient key under its own column name, `extract_StudyID` if the extract had a StudyID column, and `offset_days`) and `README.txt`. The keys folder can re-identify patients and dates: store it apart from the de-identified data and never share it. `--keys <folder>` reads `secret.txt` and `patients.csv` of an earlier keys folder: the same secret gives the same offsets (SHA-256 of the secret and the patient key) and pseudonyms, patients already in `patients.csv` keep their new `StudyID`, new patients are numbered from the largest id plus 1 in hash order, and patients missing from the new extract stay in the new `patients.csv`. A key with two ids or an id used twice in the old file is an error. The new keys folder is self-contained.

What happens to each column is listed in `metadata/utmb_deid_policy.csv` (table, column, action, domain, reason), which a person can review and edit. Actions: `keep`; `drop`; `shift_date`, `shift_instant` and `shift_datekey` (every date of a patient moves by the same number of days, between 1 and 30 earlier or later, never 0, so intervals stay exact and the time of day is kept); `remap` (a key is replaced by a 16 character pseudonym; columns with the same `domain` get the same pseudonym for the same value, so joins still work, and negative "unknown" values stay as they are); `keep_common_values` (a text value is kept only if it looks like a number, with an optional `<`, `>`, `<=` or `>=`, or if the same value occurs for at least 20 different patients in that column; used for lab `Value`, flowsheet `TimeValue`, medication order `Sig` and the `PrnReason` columns); `keep_typed_or_common_values` (flowsheet `Value`: kept as is when `RowValueType` is Custom List, Numeric Type, Blood Pressure or Temperature, otherwise the common-values rule applies). Counts of blanked values are in `column_actions.csv` and `summary.txt`. Identifiers, free text, ETL audit fields, the patient keys and the extract's StudyID are dropped, since the new StudyID identifies the patient. Rows marked `text, review` are kept text columns that need a human check, and `unclassified key, review` marks keys that were dropped because they could not be classified. Columns in the data that the policy does not list are dropped and reported. Rows without a patient key are dropped and counted. Placeholder dates (1900-01-01 and earlier, 9999-01-01 and later) are never shifted, so they stay recognisable as placeholders.

The run ends with a leak check on the written files: any cell equal to the same patient's original patient key, MRN, Epic id, patient name (any case), dropped identifier value, or original birth date in an unshifted column is counted (the output `StudyID` column is exempt; matching is per patient, using the new id, because Epic id spaces overlap, so another patient's id can equal a reference key by chance; columns ending in `NumericId` hold Epic record ids and are dropped as identifiers) in `summary.txt`, and the script exits with status 1 if there is any. Exact matches in catalog code columns (columns whose policy reason is "reference dimension key", and the flowsheet `FlowsheetRowEpicId` columns) do not fail the run, because a code shared by many patients can equal one patient's own case number or CSN by chance. They are listed in `summary.txt` under "Leak check: review items" and in `leak_review.csv` of the run folder (per column: matching cells, distinct colliding values, fewest patients that have any one of those values), counts only. Offsets and pseudonyms come from SHA-256 of the secret and the value, so nothing needs to be stored to keep the tables joinable. This needs `/dev/urandom` (Linux) for a new secret.

To rebuild the policy from `metadata/utmb_dictionary.csv`, run `Rscript scripts/utmb/deid_policy.R [--label <text>]`. It writes `output/utmb/deid_policy_<date>_<time>[_<label>]/utmb_deid_policy.csv`; review it and copy it by hand to `metadata/`. The functions are in `R/utmb/deid.R`.

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
