# nolint start: object_usage_linter. Functions from the other files are not visible here.
# Burn index event analysis for the UTMB extract. The selection functions take and return plain
# data frames so they can be tested on small in-memory tables. Only index_events.csv written by
# run_utmb_burn_index() is patient-level; every other output is an aggregate count or statistic.

burn_index_window <- as.Date(c("2015-01-01", "2023-12-31"))
burn_placeholder_cutoff <- as.Date("1900-01-01")
burn_encounter_categories <- c("hospital admission", "ED visit", "other", "no linked encounter")
burn_prior_bands <- c("0", "1", "2-5", "6-10", "more than 10")

# Meaning of the digit after the dot: percentage of total body surface area burned.
burn_group_labels <- stats::setNames(
  c("under 10%", "10-19%", "20-29%", "30-39%", "40-49%", "50-59%", "60-69%", "70-79%",
    "80-89%", "90% or more"),
  0:9
)

# "ICD-10" for T31.* coded as ICD-10-CM, "ICD-9" for 948.* coded as ICD-9-CM, otherwise NA.
burn_code_system <- function(code_type, code) {
  code <- toupper(trimws(as.character(code)))
  type <- toupper(trimws(as.character(code_type)))
  system <- rep(NA_character_, length(code))
  icd10 <- !is.na(type) & !is.na(code) & type == "ICD-10-CM" & grepl("^T31(\\.|$)", code)
  icd9 <- !is.na(type) & !is.na(code) & type == "ICD-9-CM" & grepl("^948(\\.|$)", code)
  system[icd10] <- "ICD-10"
  system[icd9] <- "ICD-9"
  system
}

# First digit after the dot of a T31.* or 948.* code (T31.21 gives 2, 948.0 gives 0), else NA.
burn_group <- function(code) {
  code <- toupper(trimws(as.character(code)))
  ok <- !is.na(code) & grepl("^(T31|948)\\.[0-9]", code)
  out <- rep(NA_integer_, length(code))
  out[ok] <- as.integer(sub("^[^.]*\\.([0-9]).*$", "\\1", code[ok]))
  out
}

burn_group_label <- function(group) {
  out <- unname(burn_group_labels[as.character(group)])
  out[is.na(out)] <- "unknown"
  out
}

utmb_flag <- function(x) !is.na(x) & toupper(trimws(as.character(x))) %in% c("1", "TRUE", "YES")

# Burn records from the diagnoses table: StudyID, code_system, Code, group, Type, date,
# EncounterKey. Missing and negative numeric encounter keys (Caboodle sentinels) become NA;
# non-numeric keys (pseudonyms from de-identified data) are kept.
extract_burn_records <- function(diagnoses) {
  dx <- as.data.frame(diagnoses)
  system <- burn_code_system(dx[["CodeType"]], dx[["Code"]])
  keep <- !is.na(system)
  dx <- dx[keep, , drop = FALSE]
  key <- trimws(as.character(dx[["EncounterKey"]]))
  key_num <- suppressWarnings(as.numeric(key))
  key[is.na(key) | key == "" | (!is.na(key_num) & key_num <= 0)] <- NA_character_
  record_type <- if ("Type" %in% names(dx)) dx[["Type"]] else rep(NA_character_, nrow(dx))
  data.frame(
    StudyID = as.character(dx[["StudyID"]]), code_system = system[keep],
    Code = toupper(trimws(as.character(dx[["Code"]]))), group = burn_group(dx[["Code"]]),
    Type = as.character(record_type), date = parse_utmb_dates(dx[["StartDate"]]),
    EncounterKey = key, stringsAsFactors = FALSE
  )
}

# One row per patient: the earliest burn record dated inside `window` (inclusive). On the index
# date the group is the highest group present; ties go to ICD-10, then the smaller code.
select_burn_index <- function(burn, window = burn_index_window) {
  burn <- as.data.frame(burn)
  in_window <- !is.na(burn$date) & burn$date >= window[1] & burn$date <= window[2]
  cand <- burn[in_window, , drop = FALSE]
  out <- data.frame(StudyID = character(), index_date = as.Date(character()),
                    index_code = character(), index_group = integer(),
                    code_system = character(), n_records_on_date = integer(),
                    n_groups_on_date = integer(), both_code_systems = logical(),
                    stringsAsFactors = FALSE)
  if (nrow(cand) == 0) return(out)

  by_date <- order(cand$StudyID, cand$date, method = "radix")
  first <- cand[by_date, , drop = FALSE]
  first <- first[!duplicated(first$StudyID), , drop = FALSE]
  on_date <- cand[cand$date == first$date[match(cand$StudyID, first$StudyID)], , drop = FALSE]

  group_rank <- ifelse(is.na(on_date$group), -1L, on_date$group)
  system_rank <- ifelse(on_date$code_system == "ICD-10", 0L, 1L)
  pick <- order(on_date$StudyID, -group_rank, system_rank, on_date$Code, method = "radix")
  chosen <- on_date[pick, , drop = FALSE]
  chosen <- chosen[!duplicated(chosen$StudyID), , drop = FALSE]

  per_id <- split(on_date, on_date$StudyID)[chosen$StudyID]
  data.frame(
    StudyID = chosen$StudyID, index_date = chosen$date, index_code = chosen$Code,
    index_group = chosen$group, code_system = chosen$code_system,
    n_records_on_date = vapply(per_id, nrow, integer(1), USE.NAMES = FALSE),
    n_groups_on_date = vapply(per_id, function(d) length(unique(d$group[!is.na(d$group)])),
                              integer(1), USE.NAMES = FALSE),
    both_code_systems = vapply(per_id, function(d) length(unique(d$code_system)) > 1,
                               logical(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

# Index encounter per index patient from burn records on the index date with a valid
# EncounterKey found in `encounters` (columns StudyID, EncounterKey, Type, PatientClass,
# AdmissionType, IsHospitalAdmission, IsEdVisit). Preference: hospital admission, ED visit,
# anything else, then the lowest EncounterKey.
select_index_encounter <- function(burn, index, encounters) {
  burn <- as.data.frame(burn)
  encounters <- as.data.frame(encounters)
  ids <- index$StudyID
  at <- match(burn$StudyID, ids)
  on_date <- !is.na(at) & !is.na(burn$date) & !is.na(burn$EncounterKey) &
    burn$date == index$index_date[at]
  cand <- unique(burn[on_date, c("StudyID", "EncounterKey"), drop = FALSE])
  n_valid <- as.integer(table(factor(cand$StudyID, levels = ids)))

  hit <- match(paste(cand$StudyID, cand$EncounterKey),
               paste(encounters$StudyID, encounters$EncounterKey))
  cand <- cand[!is.na(hit), , drop = FALSE]
  enc <- encounters[hit[!is.na(hit)], , drop = FALSE]
  category <- ifelse(utmb_flag(enc$IsHospitalAdmission), "hospital admission",
                     ifelse(utmb_flag(enc$IsEdVisit), "ED visit", "other"))
  rank <- match(category, burn_encounter_categories)
  pick <- order(cand$StudyID, rank, suppressWarnings(as.numeric(cand$EncounterKey)),
                cand$EncounterKey, method = "radix")
  best <- pick[!duplicated(cand$StudyID[pick])]
  n_cand <- as.integer(table(factor(cand$StudyID, levels = ids)))

  m <- match(ids, cand$StudyID[best])
  linked <- !is.na(m)
  take <- function(column) {
    v <- if (column %in% names(enc)) as.character(enc[[column]][best][m]) else rep(NA, length(m))
    v[!linked] <- NA_character_
    v
  }
  data.frame(
    StudyID = ids, index_encounter_key = take("EncounterKey"),
    n_candidate_encounters = n_cand, n_valid_keys_on_date = n_valid,
    encounter_category = ifelse(linked, category[best][m], "no linked encounter"),
    encounter_type = take("Type"), patient_class = take("PatientClass"),
    admission_type = take("AdmissionType"), stringsAsFactors = FALSE
  )
}

# Burn records dated strictly before the index date, per index patient. Records on placeholder
# dates (on or before 1900-01-01) count as prior records but are flagged separately and are
# left out of the earliest-prior-record date. Returns list(patients, rows).
count_prior_burn_records <- function(burn, index, placeholder_max = burn_placeholder_cutoff) {
  burn <- as.data.frame(burn)
  ids <- index$StudyID
  at <- match(burn$StudyID, ids)
  before <- !is.na(at) & !is.na(burn$date) & burn$date < index$index_date[at]
  rows <- burn[before, , drop = FALSE]
  rows$placeholder <- rows$date <= placeholder_max

  tally <- function(df) as.integer(table(factor(df$StudyID, levels = ids)))
  dates <- unique(rows[, c("StudyID", "date")])
  real <- rows[!rows$placeholder, , drop = FALSE]
  real <- real[order(real$StudyID, real$date, method = "radix"), , drop = FALSE]
  earliest <- real$date[match(ids, real$StudyID)]
  patients <- data.frame(
    StudyID = ids, n_prior_rows = tally(rows), n_prior_dates = tally(dates),
    n_prior_placeholder_rows = tally(rows[rows$placeholder, , drop = FALSE]),
    earliest_prior_date = earliest,
    years_prior_to_index = as.numeric(index$index_date - earliest) / 365.25,
    stringsAsFactors = FALSE
  )
  list(patients = patients, rows = rows)
}

burn_prior_band <- function(n) {
  band <- rep("more than 10", length(n))
  band[n <= 10] <- "6-10"
  band[n <= 5] <- "2-5"
  band[n == 1] <- "1"
  band[n == 0] <- "0"
  factor(band, levels = burn_prior_bands)
}

# One row per month of `window` (108 rows for 2015 to 2023), zeros included.
burn_month_table <- function(index_dates, window = burn_index_window) {
  months <- seq(window[1], as.Date(format(window[2], "%Y-%m-01")), by = "month")
  tab <- table(factor(format(index_dates, "%Y-%m"), levels = format(months, "%Y-%m")))
  n <- as.integer(tab)
  data.frame(year = as.integer(format(months, "%Y")), month = as.integer(format(months, "%m")),
             n = n, pct = burn_pct(n, sum(n)), stringsAsFactors = FALSE)
}

burn_year_month_matrix <- function(month_table) {
  years <- sort(unique(month_table$year))
  mat <- matrix(0L, nrow = length(years), ncol = 12,
                dimnames = list(years, month.abb))
  mat[cbind(match(month_table$year, years), month_table$month)] <- month_table$n
  out <- data.frame(year = years, mat, check.names = FALSE, stringsAsFactors = FALSE)
  out$total <- rowSums(mat)
  rownames(out) <- NULL
  out
}

burn_pct <- function(n, total) {
  if (total > 0) round(100 * n / total, 2) else rep(0, length(n))
}

burn_year_table <- function(index_dates, window = burn_index_window) {
  years <- seq(as.integer(format(window[1], "%Y")), as.integer(format(window[2], "%Y")))
  n <- as.integer(table(factor(as.integer(format(index_dates, "%Y")), levels = years)))
  data.frame(year = years, n = n, pct = burn_pct(n, sum(n)),
             cumulative_pct = burn_pct(cumsum(n), sum(n)), stringsAsFactors = FALSE)
}

# Groups 0 to 9 plus "unknown", for all index events and for each code system.
burn_group_table <- function(index) {
  parts <- lapply(c("all", "ICD-10", "ICD-9"), function(sys) {
    sub <- if (sys == "all") index else index[index$code_system == sys, , drop = FALSE]
    n <- as.integer(table(factor(ifelse(is.na(sub$index_group), "unknown",
                                        as.character(sub$index_group)),
                                 levels = c(as.character(0:9), "unknown"))))
    data.frame(code_system = sys, group = c(0:9, NA), label = burn_group_label(c(0:9, NA)),
               n = n, pct = burn_pct(n, sum(n)), stringsAsFactors = FALSE)
  })
  do.call(rbind, parts)
}

burn_level_table <- function(f, variable) {
  out <- tabulate_utmb_factor(f, variable)
  out$pct[is.nan(out$pct)] <- 0
  out
}

# Demographics of the index patients as list(categorical, numeric). `events` needs StudyID and
# index_date; `demographics` is the character table from the reader (may lack patients).
summarise_index_demographics <- function(events, demographics) {
  demo <- as.data.frame(demographics)
  at <- match(events$StudyID, demo$StudyID)
  pick <- function(column) {
    if (column %in% names(demo)) demo[[column]][at] else rep(NA_character_, nrow(events))
  }
  age <- utmb_age_years(pick("BirthDate"), events$index_date)
  n_negative_age <- sum(!is.na(age) & age < 0)
  age[!is.na(age) & age < 0] <- NA_real_
  death <- parse_utmb_dates(pick("DeathDate"))
  days_to_death <- as.numeric(death - events$index_date)
  within <- function(days) {
    ifelse(!is.na(days_to_death) & days_to_death >= 0 & days_to_death <= days, "yes", "no")
  }
  death_check <- ifelse(is.na(death), "no death date",
                        ifelse(days_to_death < 0, "death before index", "death on or after index"))
  cat_parts <- list(
    burn_level_table(utmb_age_band(age), "age_band"),
    tabulate_utmb_levels(pick("Sex"), "sex"),
    tabulate_utmb_levels(pick("FirstRace"), "race"),
    tabulate_utmb_levels(pick("MultiRacial"), "multiracial"),
    tabulate_utmb_levels(pick("Ethnicity"), "ethnicity"),
    tabulate_utmb_levels(pick("SmokingStatus"), "smoking_status"),
    tabulate_utmb_levels(ifelse(is.na(death), "no", "yes"), "deceased"),
    tabulate_utmb_levels(within(30), "death_within_30_days"),
    tabulate_utmb_levels(within(365), "death_within_365_days"),
    tabulate_utmb_levels(death_check, "death_date_check")
  )
  list(categorical = do.call(rbind, cat_parts),
       numeric = burn_stack(list(summarise_utmb_numeric(age, "age_at_index_years"))),
       age = age, n_negative_age = n_negative_age)
}

# Rows of an aggregate table with a leading "section" column, for stacking different layouts.
burn_stack <- function(parts) {
  out <- as.data.frame(data.table::rbindlist(parts, fill = TRUE, use.names = TRUE))
  stat_columns <- intersect(c("mean", "sd", "median", "q1", "q3", "min", "max"), names(out))
  out[stat_columns] <- lapply(out[stat_columns], round, 3)
  out
}

with_section <- function(df, section) {
  cbind(section = rep(section, nrow(df)), df, stringsAsFactors = FALSE)
}

# Aggregate description of prior burn records. `prior` comes from count_prior_burn_records().
summarise_prior_burn_records <- function(prior) {
  p <- prior$patients
  rows <- prior$rows
  with_prior <- p[p$n_prior_rows > 0, , drop = FALSE]
  flag <- function(x, variable) {
    burn_level_table(factor(ifelse(x, "yes", "no"), levels = c("yes", "no")), variable)
  }
  burn_stack(list(
    with_section(burn_level_table(burn_prior_band(p$n_prior_rows), "patients_by_prior_rows"),
                 "band"),
    with_section(burn_level_table(burn_prior_band(p$n_prior_dates), "patients_by_prior_dates"),
                 "band"),
    with_section(summarise_utmb_numeric(p$n_prior_rows, "n_prior_rows_all_patients"), "summary"),
    with_section(summarise_utmb_numeric(p$n_prior_dates, "n_prior_dates_all_patients"),
                 "summary"),
    with_section(summarise_utmb_numeric(with_prior$n_prior_rows, "n_prior_rows_with_prior"),
                 "summary"),
    with_section(summarise_utmb_numeric(with_prior$n_prior_dates, "n_prior_dates_with_prior"),
                 "summary"),
    with_section(summarise_utmb_numeric(p$years_prior_to_index,
                                        "years_earliest_prior_to_index"), "summary"),
    with_section(flag(p$n_prior_placeholder_rows > 0, "patients_with_placeholder_date_prior"),
                 "flag"),
    with_section(summarise_utmb_numeric(p$n_prior_placeholder_rows[p$n_prior_placeholder_rows > 0],
                                        "placeholder_date_prior_rows"), "summary"),
    with_section(tabulate_utmb_levels(rows$Type, "prior_rows_by_record_type"), "rows"),
    with_section(tabulate_utmb_levels(rows$code_system, "prior_rows_by_code_system"), "rows")
  ))
}

# Index date and group against the data team's crosswalk. `crosswalk` has StudyID, BurnCode and
# BurnCodeDate; a patient with several crosswalk rows is compared on the earliest dated one.
compare_burn_crosswalk <- function(index, crosswalk) {
  xw <- as.data.frame(crosswalk)
  xw$date <- parse_utmb_dates(xw$BurnCodeDate)
  xw <- xw[order(xw$StudyID, xw$date, method = "radix"), , drop = FALSE]
  n_duplicate <- sum(duplicated(xw$StudyID))
  xw <- xw[!duplicated(xw$StudyID), , drop = FALSE]
  at <- match(index$StudyID, xw$StudyID)
  xw_date <- xw$date[at]
  diff_days <- as.numeric(xw_date - index$index_date)
  date_status <- ifelse(is.na(at), "no crosswalk row",
                        ifelse(is.na(xw_date), "crosswalk date missing",
                               ifelse(diff_days == 0, "same date",
                                      ifelse(diff_days < 0, "crosswalk earlier",
                                             "crosswalk later"))))
  xw_group <- burn_group(xw$BurnCode[at])
  group_status <- function(keep) {
    status <- ifelse(is.na(at), "no crosswalk row",
                     ifelse(is.na(xw_group), "crosswalk group unknown",
                            ifelse(is.na(index$index_group), "index group unknown",
                                   ifelse(xw_group == index$index_group, "agree", "disagree"))))
    status[!keep] <- NA_character_
    factor(status[keep], levels = c("agree", "disagree", "crosswalk group unknown",
                                    "index group unknown", "no crosswalk row"))
  }
  same <- date_status == "same date"
  status_levels <- c("same date", "crosswalk earlier", "crosswalk later",
                     "crosswalk date missing", "no crosswalk row")
  not_indexed <- sum(!xw$StudyID %in% index$StudyID)
  table <- burn_stack(list(
    with_section(burn_level_table(factor(date_status, levels = status_levels),
                                  "index_date_vs_crosswalk_date"), "date"),
    with_section(summarise_utmb_numeric(diff_days, "crosswalk_minus_index_days"), "date"),
    with_section(summarise_utmb_numeric(diff_days[!is.na(diff_days) & diff_days != 0],
                                        "crosswalk_minus_index_days_when_different"), "date"),
    with_section(burn_level_table(group_status(rep(TRUE, nrow(index))),
                                  "index_group_vs_crosswalk_group"), "group"),
    with_section(burn_level_table(group_status(same), "index_group_vs_crosswalk_group_same_date"),
                 "group"),
    data.frame(section = "coverage", variable = "crosswalk_patients_without_index_event",
               level = "count", n = not_indexed, stringsAsFactors = FALSE)
  ))
  list(table = table, n_duplicate_crosswalk_rows = n_duplicate)
}

# Merges the pieces into the patient-level table written as index_events.csv.
build_index_events <- function(index, encounter, prior, age) {
  data.frame(
    StudyID = index$StudyID, index_date = format(index$index_date),
    index_code = index$index_code, index_group = index$index_group,
    code_system = index$code_system,
    index_encounter_key = encounter$index_encounter_key[match(index$StudyID, encounter$StudyID)],
    encounter_category = encounter$encounter_category[match(index$StudyID, encounter$StudyID)],
    age_at_index = round(age, 2),
    n_prior_rows = prior$patients$n_prior_rows[match(index$StudyID, prior$patients$StudyID)],
    n_prior_dates = prior$patients$n_prior_dates[match(index$StudyID, prior$patients$StudyID)],
    stringsAsFactors = FALSE
  )
}

summarise_index_encounters <- function(encounter) {
  linked <- encounter[encounter$encounter_category != "no linked encounter", , drop = FALSE]
  rbind(
    tabulate_utmb_levels(linked$encounter_type, "encounter_type_linked"),
    tabulate_utmb_levels(linked$patient_class, "patient_class_linked"),
    tabulate_utmb_levels(linked$admission_type, "admission_type_linked"),
    burn_level_table(factor(encounter$encounter_category, levels = burn_encounter_categories),
                     "encounter_category")
  )
}

# Plain bar, line and histogram figures in <run_dir>/figures. Uses theme_burn() and
# save_figure() from R/profile_figures.R.
write_burn_index_figures <- function(run_dir, groups, years, months, ages) {
  fig_dir <- file.path(run_dir, "figures")
  dir.create(fig_dir, showWarnings = FALSE)
  fill <- fig_sequential[3]
  g <- groups[groups$code_system == "all", ]
  g$axis <- factor(paste0(ifelse(is.na(g$group), "?", g$group), "\n", g$label),
                   levels = paste0(ifelse(is.na(g$group), "?", g$group), "\n", g$label))
  p1 <- ggplot2::ggplot(g, ggplot2::aes(axis, n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::labs(title = "Burn group at the index event",
                  subtitle = "Group = tenth of body surface area burned", x = NULL,
                  y = "Patients") +
    theme_burn()
  save_figure(p1, file.path(fig_dir, "index_group_distribution.png"), width = 9, height = 5)

  p2 <- ggplot2::ggplot(years, ggplot2::aes(factor(year), n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4, size = 3) +
    ggplot2::labs(title = "Index events per year", x = "Year", y = "Patients") +
    theme_burn()
  save_figure(p2, file.path(fig_dir, "index_events_per_year.png"))

  months$date <- as.Date(sprintf("%d-%02d-01", months$year, months$month))
  p3 <- ggplot2::ggplot(months, ggplot2::aes(date, n)) +
    ggplot2::geom_col(fill = fill, width = 25) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(title = "Index events per month", x = "Month", y = "Patients") +
    theme_burn()
  save_figure(p3, file.path(fig_dir, "index_events_per_month.png"), width = 10, height = 5)

  p4 <- ggplot2::ggplot(data.frame(age = ages[!is.na(ages)]), ggplot2::aes(age)) +
    ggplot2::geom_histogram(binwidth = 5, boundary = 0, fill = fill, colour = "white") +
    ggplot2::labs(title = "Age at the index event", x = "Age in years", y = "Patients") +
    theme_burn()
  save_figure(p4, file.path(fig_dir, "age_at_index.png"))
  invisible(fig_dir)
}

require_utmb_columns <- function(dt, table, columns) {
  absent <- setdiff(columns, names(dt))
  if (length(absent) > 0) {
    stop("Table ", table, " lacks the column(s): ", paste(absent, collapse = ", "), call. = FALSE)
  }
}

# Runs the analysis and writes <root>/output/utmb/burn_index_<YYYYmmdd_HHMMSS>[_<label>]/.
# Nothing is written unless `root` is confirmed by PROJECT_VERSION. Returns the run folder.
run_utmb_burn_index <- function(data_dir, root, label = NULL,
                                progress = function(...) cat(..., "\n", sep = "")) {
  files <- utmb_table_files(data_dir)
  needed <- c("crosswalk", "demographics", "diagnoses", "encounters")
  absent <- setdiff(needed, names(files))
  if (length(absent) > 0) {
    stop("Required table(s) not found in ", data_dir, ": ", paste(absent, collapse = ", "),
         call. = FALSE)
  }
  run_dir <- utmb_run_dir(root, "burn_index", label)
  started <- Sys.time()
  step <- function(text) {
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    progress(sprintf("[%.0f s] %s", elapsed, text))
  }

  dx <- read_utmb_table(files[["diagnoses"]])
  require_utmb_columns(dx, "diagnoses", c("StudyID", "CodeType", "Code", "StartDate", "Type",
                                          "EncounterKey"))
  dx <- dx[, c("StudyID", "CodeType", "Code", "StartDate", "Type", "EncounterKey"),
           with = FALSE]
  burn <- extract_burn_records(dx)
  lookalikes <- sum(grepl("^(T31|948)", toupper(trimws(dx[["Code"]]))), na.rm = TRUE) -
    nrow(burn)
  n_dx_rows <- nrow(dx)
  rm(dx)
  invisible(gc())
  step(sprintf("diagnoses read: %s rows, %s burn records", format(n_dx_rows, big.mark = ","),
               format(nrow(burn), big.mark = ",")))

  encounters <- read_utmb_table(files[["encounters"]])
  require_utmb_columns(encounters, "encounters", c("StudyID", "EncounterKey"))
  keep <- intersect(c("StudyID", "EncounterKey", "Type", "PatientClass", "AdmissionType",
                      "IsHospitalAdmission", "IsEdVisit"), names(encounters))
  encounters <- encounters[, keep, with = FALSE]
  demographics <- read_utmb_table(files[["demographics"]])
  require_utmb_columns(demographics, "demographics", "StudyID")
  crosswalk <- read_utmb_table(files[["crosswalk"]])
  require_utmb_columns(crosswalk, "crosswalk", c("StudyID", "BurnCode", "BurnCodeDate"))
  step("other tables read")

  index <- select_burn_index(burn)
  encounter <- select_index_encounter(burn, index, encounters)
  prior <- count_prior_burn_records(burn, index)
  demo <- summarise_index_demographics(index, demographics)
  comparison <- compare_burn_crosswalk(index, crosswalk)
  events <- build_index_events(index, encounter, prior, demo$age)

  groups <- burn_group_table(index)
  years <- burn_year_table(index$index_date)
  months <- burn_month_table(index$index_date)
  matrix_table <- burn_year_month_matrix(months)
  encounter_table <- summarise_index_encounters(encounter)
  prior_table <- summarise_prior_burn_records(prior)

  write_csv <- function(x, name) data.table::fwrite(x, file.path(run_dir, name), na = "")
  write_csv(groups, "index_group_distribution.csv")
  write_csv(years, "index_year_distribution.csv")
  write_csv(months, "index_by_month.csv")
  write_csv(matrix_table, "index_year_month_matrix.csv")
  write_csv(demo$categorical, "index_demographics_categorical.csv")
  write_csv(demo$numeric, "index_demographics_numeric.csv")
  write_csv(encounter_table, "index_encounter_type.csv")
  write_csv(prior_table, "prior_burn_records.csv")
  write_csv(comparison$table, "crosswalk_comparison.csv")
  write_csv(events, "index_events.csv")
  write_burn_index_figures(run_dir, groups, years, months, demo$age)

  issues <- list(
    diagnoses_rows = n_dx_rows, lookalikes = lookalikes,
    crosswalk_patients = length(unique(crosswalk[["StudyID"]])),
    crosswalk_duplicates = comparison$n_duplicate_crosswalk_rows,
    patients_without_demographics = sum(!index$StudyID %in% demographics[["StudyID"]]),
    burn_patients_outside_crosswalk = length(setdiff(unique(burn$StudyID),
                                                     crosswalk[["StudyID"]])),
    negative_ages = demo$n_negative_age,
    crosswalk_without_burn = length(setdiff(crosswalk[["StudyID"]], burn$StudyID))
  )
  writeLines(burn_index_summary_lines(burn, index, encounter, prior, demo, comparison, groups,
                                      years, months, encounter_table, prior_table, issues,
                                      as.numeric(difftime(Sys.time(), started, units = "secs"))),
             file.path(run_dir, "summary.txt"))
  progress("Output folder: ", run_dir)
  invisible(run_dir)
}

burn_index_summary_lines <- function(burn, index, encounter, prior, demo, comparison, groups,
                                     years, months, encounter_table, prior_table, issues,
                                     total_seconds) {
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  line_for <- function(df, variable) {
    d <- df[df$variable == variable, ]
    paste0(d$level, " ", d$n, " (", d$pct, "%)", collapse = "; ")
  }
  n_index <- nrow(index)
  patients_burn <- length(unique(burn$StudyID))
  dated <- burn[!is.na(burn$date), ]
  in_window <- dated[dated$date >= burn_index_window[1] & dated$date <= burn_index_window[2], ]
  patients_dated <- length(unique(dated$StudyID))
  g_all <- groups[groups$code_system == "all", ]
  num <- demo$numeric
  p <- prior$patients
  cmp <- comparison$table
  cat_df <- demo$categorical

  c(
    "UTMB burn index event summary", "",
    "WARNING: index_events.csv in this folder is PATIENT-LEVEL (one row per StudyID). It must",
    "not be shared or committed. All other files hold aggregate counts only.", "",
    sprintf("Run time: %.0f s.", total_seconds), "",
    "Definitions",
    "  Burn record: diagnoses row with ICD-10-CM T31.* or ICD-9-CM 948.* (all record types).",
    "  Group: first digit after the dot (percent of body surface area burned, tenths).",
    "  Index event: earliest burn record dated 2015-01-01 to 2023-12-31; highest group on that",
    "  date. Index encounter: hospital admission, then ED visit, then other, then lowest key.",
    "  Smoking status and race are the extract's current values, not values at the index date.",
    "",
    "Patient flow",
    sprintf("  Patients in crosswalk: %s", fmt(issues$crosswalk_patients)),
    sprintf("  Patients with any burn record: %s", fmt(patients_burn)),
    sprintf("  Patients with a burn record that has a valid date: %s", fmt(patients_dated)),
    sprintf("  Patients with an index event: %s", fmt(n_index)),
    sprintf("  Burn records: %s rows (%s in the index window)", fmt(nrow(burn)),
            fmt(nrow(in_window))), "",
    "Index group (n, % of index events)",
    paste0("  ", ifelse(is.na(g_all$group), "", paste0(g_all$group, " ")), g_all$label, ": ",
           g_all$n, " (", g_all$pct, "%)"),
    sprintf("  Code system at index: ICD-10 %d, ICD-9 %d",
            sum(index$code_system == "ICD-10"), sum(index$code_system == "ICD-9")),
    sprintf("  Patients with more than one distinct group on the index date: %d",
            sum(index$n_groups_on_date > 1)),
    sprintf("  Patients with both ICD-9 and ICD-10 records on the index date: %d",
            sum(index$both_code_systems)), "",
    "Index events per year (n)",
    paste0("  ", years$year, ": ", years$n, " (", years$pct, "%, cumulative ",
           years$cumulative_pct, "%)"),
    sprintf("  Months with no index event: %d of %d", sum(months$n == 0), nrow(months)), "",
    "Demographics at index",
    sprintf("  Age in years: n=%d, median %s (%s to %s), range %s to %s, missing %d",
            num$n, format(round(num$median, 1)), format(round(num$q1, 1)),
            format(round(num$q3, 1)), format(round(num$min, 1)), format(round(num$max, 1)),
            num$n_missing),
    sprintf("  Age band: %s", line_for(cat_df, "age_band")),
    sprintf("  Sex: %s", line_for(cat_df, "sex")),
    sprintf("  Race (current value): %s", line_for(cat_df, "race")),
    sprintf("  Multiracial (current value): %s", line_for(cat_df, "multiracial")),
    sprintf("  Ethnicity: %s", line_for(cat_df, "ethnicity")),
    sprintf("  Smoking status (current value): %s", line_for(cat_df, "smoking_status")),
    sprintf("  Deceased: %s", line_for(cat_df, "deceased")),
    sprintf("  Death within 30 days: %s", line_for(cat_df, "death_within_30_days")),
    sprintf("  Death within 365 days: %s", line_for(cat_df, "death_within_365_days")),
    sprintf("  Death date check: %s", line_for(cat_df, "death_date_check")), "",
    "Index encounter",
    sprintf("  Category: %s", line_for(encounter_table, "encounter_category")),
    sprintf("  Patients with more than one candidate encounter: %d",
            sum(encounter$n_candidate_encounters > 1)),
    "  Type, patient class and admission type counts are in index_encounter_type.csv.", "",
    "Burn records before the index event",
    sprintf("  Prior rows per patient: %s", line_for(prior_table, "patients_by_prior_rows")),
    sprintf("  Prior distinct dates per patient: %s",
            line_for(prior_table, "patients_by_prior_dates")),
    sprintf("  Patients with prior records on a placeholder date (1900-01-01 or earlier): %d",
            sum(p$n_prior_placeholder_rows > 0)),
    sprintf("  Prior rows by record type: %s", line_for(prior_table, "prior_rows_by_record_type")),
    sprintf("  Prior rows by code system: %s", line_for(prior_table, "prior_rows_by_code_system")),
    "  Statistics (mean, median, range) are in prior_burn_records.csv.", "",
    "Crosswalk comparison",
    sprintf("  Index date against crosswalk date: %s",
            line_for(cmp, "index_date_vs_crosswalk_date")),
    sprintf("  Index group against crosswalk group: %s",
            line_for(cmp, "index_group_vs_crosswalk_group")),
    sprintf("  Same date only: %s", line_for(cmp, "index_group_vs_crosswalk_group_same_date")),
    sprintf("  Crosswalk patients without an index event: %d",
            cmp$n[cmp$variable == "crosswalk_patients_without_index_event"]), "",
    "Data issues",
    sprintf("  Burn records without a StartDate: %d rows, %d patients",
            sum(is.na(burn$date)), length(unique(burn$StudyID[is.na(burn$date)]))),
    sprintf("  Burn records on placeholder dates (1900-01-01 or earlier): %d rows",
            sum(!is.na(burn$date) & burn$date <= burn_placeholder_cutoff)),
    sprintf("  Burn records dated before 2015 (placeholders excluded): %d rows",
            sum(!is.na(burn$date) & burn$date > burn_placeholder_cutoff &
                  burn$date < burn_index_window[1])),
    sprintf("  Burn records dated after 2023-12-31: %d rows",
            sum(!is.na(burn$date) & burn$date > burn_index_window[2])),
    sprintf("  Patients with burn records but none dated in the window: %d",
            patients_burn - n_index),
    sprintf("  Crosswalk patients with no burn record at all: %d", issues$crosswalk_without_burn),
    sprintf("  Burn records without a valid EncounterKey: %d rows",
            sum(is.na(burn$EncounterKey))),
    sprintf("  Index patients with no valid EncounterKey on the index date: %d",
            sum(encounter$n_valid_keys_on_date == 0)),
    sprintf("  Index patients whose valid keys are not in the encounters table: %d",
            sum(encounter$n_valid_keys_on_date > 0 & encounter$n_candidate_encounters == 0)),
    sprintf("  Index patients with no linked encounter: %d",
            sum(encounter$encounter_category == "no linked encounter")),
    sprintf("  Rows starting T31 or 948 but excluded by CodeType or format: %d",
            issues$lookalikes),
    sprintf("  Index patients without a demographics row: %d",
            issues$patients_without_demographics),
    sprintf("  Index patients with a negative age at index (set to missing): %d",
            issues$negative_ages),
    sprintf("  Patients with burn records that are not in the crosswalk: %d",
            issues$burn_patients_outside_crosswalk),
    sprintf("  Extra crosswalk rows for the same patient: %d", issues$crosswalk_duplicates)
  )
}
# nolint end
