for (f in list.files(testthat::test_path("..", "..", "R", "utmb"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

has_dash <- function(x) {
  any(grepl("\u2013", x, fixed = TRUE) | grepl("\u2014", x, fixed = TRUE))
}

burn_rows <- function(...) {
  d <- data.frame(..., stringsAsFactors = FALSE)
  for (col in c("Type", "EncounterKey")) {
    if (!col %in% names(d)) d[[col]] <- NA_character_
  }
  d
}

test_that("burn_code_system matches T31 and 948 under the right code type only", {
  icd10 <- c("T31.21", "T31.0", "T31", "t31.9", "T30.1", "T32.11", "T310", "948.21")
  expect_equal(burn_code_system(rep("ICD-10-CM", length(icd10)), icd10),
               c("ICD-10", "ICD-10", "ICD-10", "ICD-10", NA, NA, NA, NA))
  icd9 <- c("948.21", "948.0", "948", "949.1", "T31.21", "9480")
  expect_equal(burn_code_system(rep("ICD-9-CM", length(icd9)), icd9),
               c("ICD-9", "ICD-9", "ICD-9", NA, NA, NA))
  expect_equal(burn_code_system(c(NA, "Other"), c("T31.21", "T31.21")), c(NA_character_, NA))
  expect_true(is.na(burn_code_system("ICD-10-CM", NA)))
})

test_that("burn_group takes the first digit after the dot", {
  expect_equal(burn_group(c("T31.21", "T31.0", "948.0", "948.00", "948.31", "T31.99", "T31",
                            "948", "T30.1", NA)),
               c(2L, 0L, 0L, 0L, 3L, 9L, NA, NA, NA, NA))
  expect_equal(burn_group_label(c(0, 1, 9, NA)), c("under 10%", "10-19%", "90% or more",
                                                   "unknown"))
  expect_false(has_dash(burn_group_labels))
})

test_that("extract_burn_records keeps burn rows and turns missing and negative keys into NA", {
  dx <- data.frame(
    StudyID = c("a", "a", "a", "b"), CodeType = c("ICD-10-CM", "ICD-10-CM", "ICD-9-CM", "ICD-9-CM"),
    Code = c("T31.21", "T30.0", "948.21", "T31.21"),
    StartDate = c("2020-01-02", "2020-01-02", "20200103", NA), Type = c("x", "y", "z", "w"),
    EncounterKey = c("12", "13", "-1", NA), stringsAsFactors = FALSE
  )
  b <- extract_burn_records(dx)
  expect_equal(nrow(b), 2)
  expect_equal(b$EncounterKey, c("12", NA))
  expect_equal(b$group, c(2L, 2L))
  expect_equal(b$date, as.Date(c("2020-01-02", "2020-01-03")))
  expect_equal(b$code_system, c("ICD-10", "ICD-9"))
})

test_that("the index window includes its boundaries and excludes the days outside", {
  burn <- burn_rows(
    StudyID = c("a", "a", "b", "c", "d", "e"),
    code_system = "ICD-10", Code = "T31.21", group = 2L,
    date = as.Date(c("2014-12-31", "2015-01-01", "2023-12-31", "2024-01-01", "2014-12-31", NA))
  )
  idx <- select_burn_index(burn)
  expect_equal(idx$StudyID, c("a", "b"))
  expect_equal(idx$index_date, as.Date(c("2015-01-01", "2023-12-31")))
  expect_equal(nrow(select_burn_index(burn[burn$StudyID %in% c("c", "d", "e"), ])), 0)
})

test_that("the index is the earliest dated record, not the earliest row", {
  burn <- burn_rows(
    StudyID = "a", code_system = "ICD-10", Code = c("T31.9", "T31.1", "T31.5"),
    group = c(9L, 1L, 5L), date = as.Date(c("2018-06-01", "2016-03-01", "2016-03-01"))
  )
  idx <- select_burn_index(burn)
  expect_equal(idx$index_date, as.Date("2016-03-01"))
  expect_equal(c(idx$index_group, idx$index_code, idx$n_records_on_date), c(5, "T31.5", 2))
})

test_that("on the index date the highest group wins and ties prefer ICD-10", {
  burn <- burn_rows(
    StudyID = c("a", "a", "a", "b", "b", "c", "c"),
    code_system = c("ICD-10", "ICD-10", "ICD-9", "ICD-9", "ICD-10", "ICD-10", "ICD-10"),
    Code = c("T31.21", "T31.40", "948.21", "948.21", "T31.21", "T31.22", "T31.21"),
    group = c(2L, 4L, 2L, 2L, 2L, 2L, 2L), date = as.Date("2019-05-05")
  )
  idx <- select_burn_index(burn)
  a <- idx[idx$StudyID == "a", ]
  expect_equal(c(a$index_group, a$index_code, a$n_groups_on_date, a$both_code_systems),
               c(4, "T31.40", 2, TRUE))
  b <- idx[idx$StudyID == "b", ]
  expect_equal(c(b$code_system, b$n_groups_on_date, b$both_code_systems),
               c("ICD-10", 1, TRUE))
  cc <- idx[idx$StudyID == "c", ]
  expect_equal(c(cc$index_code, cc$both_code_systems), c("T31.21", FALSE))
  expect_equal(sum(idx$n_groups_on_date > 1), 1)
  expect_equal(sum(idx$both_code_systems), 2)
})

test_that("a group-less index code gives an unknown group", {
  burn <- burn_rows(StudyID = "a", code_system = "ICD-10", Code = "T31", group = NA_integer_,
                    date = as.Date("2019-05-05"))
  idx <- select_burn_index(burn)
  expect_true(is.na(idx$index_group))
  expect_equal(idx$n_groups_on_date, 0)
})

make_encounters <- function() {
  data.frame(
    StudyID = c("a", "a", "a", "b", "c"), EncounterKey = c("30", "10", "20", "40", "50"),
    Type = c("Hospital Encounter", "Office Visit", "Emergency", "Hospital Encounter", "Office"),
    PatientClass = c("Inpt", "Outpt", "Emerg", "Inpt", "Outpt"),
    AdmissionType = c("Emergency", NA, "Emergency", "Elective", NA),
    IsHospitalAdmission = c("1", "0", "0", "1", "0"), IsEdVisit = c("0", "0", "1", "0", "0"),
    stringsAsFactors = FALSE
  )
}

test_that("the index encounter prefers admission, then ED, then other, then lowest key", {
  index <- data.frame(StudyID = c("a", "b", "c", "d", "e"),
                      index_date = as.Date("2020-01-01"), stringsAsFactors = FALSE)
  burn <- burn_rows(
    StudyID = c("a", "a", "a", "b", "c", "d", "d", "e", "a"),
    EncounterKey = c("10", "20", "30", "40", "50", NA, NA, "99", "20"),
    date = as.Date(c(rep("2020-01-01", 8), "2019-01-01"))
  )
  enc <- select_index_encounter(burn, index, make_encounters())
  expect_equal(enc$index_encounter_key, c("30", "40", "50", NA, NA))
  expect_equal(enc$encounter_category,
               c("hospital admission", "hospital admission", "other", "no linked encounter",
                 "no linked encounter"))
  expect_equal(enc$n_candidate_encounters, c(3, 1, 1, 0, 0))
  expect_equal(enc$n_valid_keys_on_date, c(3, 1, 1, 0, 1))
  expect_equal(enc$encounter_type[1:2], c("Hospital Encounter", "Hospital Encounter"))
  expect_true(is.na(enc$patient_class[4]))

  no_admission <- make_encounters()
  no_admission$IsHospitalAdmission <- "0"
  expect_equal(select_index_encounter(burn, index, no_admission)$index_encounter_key[1], "20")
  no_ed <- no_admission
  no_ed$IsEdVisit <- "0"
  expect_equal(select_index_encounter(burn, index, no_ed)$index_encounter_key[1], "10")
})

test_that("an encounter of another patient is not linked", {
  index <- data.frame(StudyID = "z", index_date = as.Date("2020-01-01"), stringsAsFactors = FALSE)
  burn <- burn_rows(StudyID = "z", EncounterKey = "30", date = as.Date("2020-01-01"))
  enc <- select_index_encounter(burn, index, make_encounters())
  expect_equal(enc$encounter_category, "no linked encounter")
})

test_that("prior counts use strictly earlier dates and flag placeholder dates", {
  index <- data.frame(StudyID = c("a", "b", "c"),
                      index_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
                      stringsAsFactors = FALSE)
  burn <- burn_rows(
    StudyID = c("a", "a", "a", "a", "a", "b", "b", "b"),
    code_system = c("ICD-10", "ICD-9", "ICD-10", "ICD-10", "ICD-10", "ICD-10", "ICD-10", "ICD-10"),
    Type = c("t1", "t2", "t1", "t1", "t1", "t1", "t1", "t1"),
    date = as.Date(c("2018-01-01", "2018-01-01", "2019-06-01", "2020-01-01", "1900-01-01",
                     "2020-01-02", NA, "2020-01-01"))
  )
  prior <- count_prior_burn_records(burn, index)
  p <- prior$patients
  expect_equal(p$n_prior_rows, c(4, 0, 0))
  expect_equal(p$n_prior_dates, c(3, 0, 0))
  expect_equal(p$n_prior_placeholder_rows, c(1, 0, 0))
  expect_equal(p$earliest_prior_date[1], as.Date("2018-01-01"))
  expect_equal(p$years_prior_to_index[1], 730 / 365.25)
  expect_true(all(is.na(p$earliest_prior_date[2:3])))
  expect_equal(nrow(prior$rows), 4)

  tab <- summarise_prior_burn_records(prior)
  level <- function(variable, lvl) tab$n[tab$variable == variable & tab$level == lvl]
  expect_equal(level("patients_by_prior_rows", "2-5"), 1)
  expect_equal(level("patients_by_prior_rows", "0"), 2)
  expect_equal(level("patients_by_prior_dates", "2-5"), 1)
  expect_equal(level("patients_with_placeholder_date_prior", "yes"), 1)
  expect_equal(level("prior_rows_by_code_system", "ICD-9"), 1)
})

test_that("prior bands cover 0, 1, 2 to 5, 6 to 10 and more than 10", {
  expect_equal(as.character(burn_prior_band(c(0, 1, 2, 5, 6, 10, 11))),
               c("0", "1", "2-5", "2-5", "6-10", "6-10", "more than 10"))
})

test_that("the month table has 108 rows with zeros and the matrix matches", {
  dates <- as.Date(c("2015-01-01", "2015-01-31", "2023-12-31", "2020-02-29"))
  m <- burn_month_table(dates)
  expect_equal(nrow(m), 108)
  expect_equal(c(m$year[1], m$month[1], m$year[108], m$month[108]), c(2015, 1, 2023, 12))
  expect_equal(sum(m$n), 4)
  expect_equal(sum(m$n == 0), 105)
  expect_equal(m$n[m$year == 2015 & m$month == 1], 2)
  expect_equal(sum(m$pct), 100)
  mat <- burn_year_month_matrix(m)
  expect_equal(dim(mat), c(9, 14))
  expect_equal(mat$Jan[1], 2)
  expect_equal(mat$Dec[9], 1)
  expect_equal(sum(mat$total), 4)
  empty <- burn_month_table(as.Date(character()))
  expect_equal(c(nrow(empty), sum(empty$n), sum(empty$pct)), c(108, 0, 0))
})

test_that("the year table has 2015 to 2023 and a cumulative percentage", {
  y <- burn_year_table(as.Date(c("2015-05-01", "2015-06-01", "2023-01-01", "2019-01-01")))
  expect_equal(y$year, 2015:2023)
  expect_equal(y$n[c(1, 5, 9)], c(2, 1, 1))
  expect_equal(y$cumulative_pct[9], 100)
})

test_that("burn_group_table splits by code system and keeps all groups", {
  idx <- data.frame(index_group = c(2L, 2L, 0L, NA), code_system = c("ICD-10", "ICD-9", "ICD-10",
                                                                     "ICD-10"),
                    stringsAsFactors = FALSE)
  g <- burn_group_table(idx)
  expect_equal(nrow(g), 33)
  expect_equal(g$n[g$code_system == "all" & g$group %in% 2], 2)
  expect_equal(sum(g$n[g$code_system == "ICD-9"]), 1)
  expect_equal(g$n[g$code_system == "all" & is.na(g$group)], 1)
})

test_that("index demographics count age bands and deaths within 30 and 365 days", {
  events <- data.frame(StudyID = c("a", "b", "c", "d"),
                       index_date = as.Date("2020-01-01"), stringsAsFactors = FALSE)
  demo <- data.frame(
    StudyID = c("a", "b", "c"), BirthDate = c("2000-01-01", "1940-01-01", "2021-01-01"),
    Sex = c("Male", "Female", NA), DeathDate = c("2020-01-31", "2020-12-31", "2019-01-01"),
    stringsAsFactors = FALSE
  )
  res <- summarise_index_demographics(events, demo)
  cat <- res$categorical
  get <- function(v, l) cat$n[cat$variable == v & cat$level == l]
  expect_equal(get("age_band", "18-39"), 1)
  expect_equal(get("age_band", "65+"), 1)
  expect_equal(get("age_band", "missing"), 2)
  expect_equal(res$n_negative_age, 1)
  expect_equal(get("death_within_30_days", "yes"), 1)
  expect_equal(get("death_within_365_days", "yes"), 2)
  expect_equal(get("deceased", "yes"), 3)
  expect_equal(get("death_date_check", "death before index"), 1)
  expect_equal(get("sex", "(missing)"), 2)
  expect_equal(res$numeric$n, 2)
})

test_that("the crosswalk comparison classifies dates and groups", {
  index <- data.frame(StudyID = c("a", "b", "c", "d", "e"),
                      index_date = as.Date("2020-01-10"), index_group = c(2L, 2L, 3L, 1L, 4L),
                      stringsAsFactors = FALSE)
  xw <- data.frame(
    StudyID = c("a", "b", "c", "d", "z", "a"),
    BurnCode = c("T31.21", "T31.30", "T31.31", "T31.1", "T31.0", "T31.50"),
    BurnCodeDate = c("2020-01-10", "2020-01-05", "2020-01-20", NA, "2020-01-01", "2021-01-01"),
    stringsAsFactors = FALSE
  )
  res <- compare_burn_crosswalk(index, xw)
  tab <- res$table
  get <- function(v, l) tab$n[tab$variable == v & tab$level == l]
  expect_equal(get("index_date_vs_crosswalk_date", "same date"), 1)
  expect_equal(get("index_date_vs_crosswalk_date", "crosswalk earlier"), 1)
  expect_equal(get("index_date_vs_crosswalk_date", "crosswalk later"), 1)
  expect_equal(get("index_date_vs_crosswalk_date", "crosswalk date missing"), 1)
  expect_equal(get("index_date_vs_crosswalk_date", "no crosswalk row"), 1)
  expect_equal(get("index_group_vs_crosswalk_group", "agree"), 3)
  expect_equal(get("index_group_vs_crosswalk_group", "disagree"), 1)
  expect_equal(get("index_group_vs_crosswalk_group", "no crosswalk row"), 1)
  expect_equal(get("index_group_vs_crosswalk_group_same_date", "agree"), 1)
  expect_equal(get("crosswalk_patients_without_index_event", "count"), 1)
  expect_equal(res$n_duplicate_crosswalk_rows, 1)
  diff_row <- tab[tab$variable == "crosswalk_minus_index_days", ]
  expect_equal(c(diff_row$n, diff_row$min, diff_row$max), c(3, -5, 10))
})

write_burn_csv <- function(df, dir, name) {
  utils::write.csv(df, file.path(dir, name), row.names = FALSE, na = "NULL")
}

make_burn_data <- function(dir) {
  data_dir <- file.path(dir, "data")
  dir.create(data_dir)
  ids <- paste0("S", 1:5)
  write_burn_csv(data.frame(
    StudyID = ids, BurnCode = c("T31.21", "T31.0", "T31.2", NA, "T31.40"),
    BurnCodeDate = c("2015-01-01", "2023-12-31", "2020-01-01", NA, "2018-03-10")
  ), data_dir, "crosswalk")
  write_burn_csv(data.frame(
    StudyID = ids, BirthDate = c("1980-01-01", "1950-06-01", "2000-01-01", NA, "2010-01-01"),
    Sex = c("Male", "Female", "Male", "Male", "Female"), FirstRace = "Asian", MultiRacial = 0,
    Ethnicity = "Unknown", SmokingStatus = "Never",
    DeathDate = c(NA, "2024-01-10", NA, NA, NA)
  ), data_dir, "demographics")
  write_burn_csv(data.frame(
    StudyID = c("S1", "S2", "S2", "S5"), EncounterKey = c(101, 202, 203, 501),
    Type = "Hospital Encounter", PatientClass = c("Inpt", "Emerg", "Inpt", "Outpt"),
    AdmissionType = NA, IsHospitalAdmission = c(0, 0, 1, 0), IsEdVisit = c(0, 1, 0, 0)
  ), data_dir, "encounters")
  write_burn_csv(data.frame(
    StudyID = c("S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2", "S3", "S3", "S4", "S4", "S4",
                "S5", "S5"),
    CodeType = c("ICD-10-CM", "ICD-10-CM", "ICD-9-CM", "ICD-10-CM", "ICD-10-CM", "ICD-10-CM",
                 "ICD-10-CM", "ICD-10-CM", "ICD-10-CM", "ICD-10-CM", "ICD-10-CM", "ICD-10-CM",
                 "ICD-9-CM", "ICD-10-CM", "ICD-10-CM"),
    Code = c("T31.5", "T31.21", "948.21", "T31.0", "T31.0", "T31.30", "T31.1", "T31.1", "T31.2",
             "T31.2", "T30.1", "948.21", "T31.21", "T31.40", "T31.40"),
    StartDate = c("2014-12-31", "2015-01-01", "2015-01-01", "2020-05-05", "2023-12-31",
                  "2023-12-31", "1900-01-01", "2014-02-02", "2024-01-01", "2014-12-31",
                  "2020-01-01", "2020-01-01", "2020-01-01", NA, "2018-03-03"),
    Type = "Billing Diagnosis",
    EncounterKey = c(-1, 101, 101, NA, 202, 203, -1, -1, -1, -1, -1, -1, -1, 501, -2)
  ), data_dir, "diagnoses")
  data_dir
}

make_burn_root <- function(dir) {
  root <- file.path(dir, "fake_root")
  dir.create(root)
  file.copy(file.path(testthat::test_path("..", ".."), "PROJECT_VERSION"), root)
  root
}

test_that("run_utmb_burn_index writes the expected files from synthetic tables", {
  dir <- local_output_dir()
  data_dir <- make_burn_data(dir)
  root <- make_burn_root(dir)
  run_dir <- run_utmb_burn_index(data_dir, root, label = "unit test", progress = function(...) NULL)

  expect_true(startsWith(normalizePath(run_dir), normalizePath(file.path(root, "output"))))
  expect_match(basename(run_dir), "^burn_index_[0-9]{8}_[0-9]{6}_unit_test$")
  expected_files <- c(
    "crosswalk_comparison.csv", "figures", "index_by_month.csv",
    "index_demographics_categorical.csv", "index_demographics_numeric.csv",
    "index_encounter_type.csv", "index_events.csv", "index_group_distribution.csv",
    "index_year_distribution.csv", "index_year_month_matrix.csv", "prior_burn_records.csv",
    "summary.txt"
  )
  expect_setequal(list.files(run_dir), expected_files)
  expect_setequal(list.files(file.path(run_dir, "figures")),
                  c("age_at_index.png", "index_events_per_month.png", "index_events_per_year.png",
                    "index_group_distribution.png"))
  expect_setequal(list.files(root, all.files = TRUE, no.. = TRUE), c("PROJECT_VERSION", "output"))

  read <- function(name) {
    utils::read.csv(file.path(run_dir, name), stringsAsFactors = FALSE, na.strings = "")
  }
  ev <- read("index_events.csv")
  expect_equal(ev$StudyID, c("S1", "S2", "S5"))
  expect_equal(ev$index_date, c("2015-01-01", "2023-12-31", "2018-03-03"))
  expect_equal(ev$index_group, c(2, 3, 4))
  expect_equal(ev$code_system, c("ICD-10", "ICD-10", "ICD-10"))
  expect_equal(ev$index_encounter_key, c(101, 203, NA))
  expect_equal(ev$encounter_category, c("other", "hospital admission", "no linked encounter"))
  expect_equal(ev$n_prior_rows, c(1, 2, 0))
  expect_equal(ev$n_prior_dates, c(1, 2, 0))

  expect_equal(nrow(read("index_by_month.csv")), 108)
  groups <- read("index_group_distribution.csv")
  expect_equal(groups$n[groups$code_system == "all" & groups$group %in% 2:4], c(1, 1, 1))
  prior <- read("prior_burn_records.csv")
  expect_equal(prior$n[prior$variable == "patients_with_placeholder_date_prior" &
                         prior$level == "yes"], 1)
  cmp <- read("crosswalk_comparison.csv")
  expect_equal(cmp$n[cmp$variable == "index_date_vs_crosswalk_date" & cmp$level == "same date"], 2)
  expect_equal(cmp$n[cmp$variable == "index_group_vs_crosswalk_group" &
                       cmp$level == "disagree"], 1)
  demo <- read("index_demographics_categorical.csv")
  expect_equal(demo$n[demo$variable == "death_within_30_days" & demo$level == "yes"], 1)

  summary <- readLines(file.path(run_dir, "summary.txt"))
  expect_true(any(grepl("PATIENT-LEVEL", summary)))
  expect_true(any(grepl("current values", summary)))
  expect_true(any(grepl("Patients with an index event: 3", summary)))
  expect_false(has_dash(summary))
})

test_that("run_utmb_burn_index needs a confirmed root and the required tables", {
  dir <- local_output_dir()
  data_dir <- make_burn_data(dir)
  root <- make_burn_root(dir)
  bare <- file.path(dir, "bare")
  dir.create(bare)
  expect_error(run_utmb_burn_index(data_dir, bare, progress = function(...) NULL),
               "not confirmed")
  expect_false(dir.exists(file.path(bare, "output")))
  partial <- file.path(dir, "partial")
  dir.create(partial)
  file.copy(file.path(data_dir, "diagnoses"), partial)
  expect_error(run_utmb_burn_index(partial, root, progress = function(...) NULL),
               "crosswalk, demographics, encounters")
})
