for (f in list.files(testthat::test_path("..", "..", "R", "utmb"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

write_utmb_csv <- function(df, dir, name) {
  utils::write.csv(df, file.path(dir, name), row.names = FALSE, na = "NULL")
}

# Small synthetic extract with fake values, written without a file extension.
make_utmb_dir <- function(dir) {
  data_dir <- file.path(dir, "data")
  dir.create(data_dir)
  ids <- c("S1", "S2", "S3")
  write_utmb_csv(data.frame(
    StudyID = ids, PatientDurableKey = 1:3, PatientEpicId = c("E1", "E2", "E3"),
    MRN = c("M1", "M2", "M3"), PatientName = c("Fake One", "Fake Two", "Fake Three"),
    DOB = c("2000-01-01", "1950-05-05", "2010-10-10"), BurnCode = c("A", "A", "B"),
    BurnCodeDate = c("2020-01-01", "2020-02-01", "2020-03-01")
  ), data_dir, "crosswalk")
  write_utmb_csv(data.frame(
    StudyID = ids, PatientDurableKey = 1:3,
    BirthDate = c("2000-01-01", "1950-05-05", "2010-10-10"),
    Sex = c("Male", "Female", "Male"), DeathDate = c(NA, "2021-01-01", NA),
    Status = c("Alive", "Deceased", "Alive"), IsValid = 1, Test = c(0, 0, 1)
  ), data_dir, "demographics")
  write_utmb_csv(data.frame(
    StudyID = c("S1", "S1", "S2", "S9"), HospitalAdmissionKey = 1:4,
    AdmissionDateKey = c("20200101", "20200601", "20210101", "20210101"),
    AdmissionTimeOfDayKey = c(900, 1000, 800, 800), LengthOfStayInDays = c(5, 3, 10, 1),
    Count = 1
  ), data_dir, "hospital_admissions")
  write_utmb_csv(data.frame(
    StudyID = c("S1", "S1", "S2", "S2", "S2"), DiagnosisKey = c(-1, 5, 6, 7, 7),
    StartDate = c("2020-01-01", "2020-01-02", "2020-02-03", "2020-02-03", "2020-02-03"),
    Comment = c("free text a", "free text b", "free text c", NA, NA),
    IsPrimary = c(1, 0, 1, 0, 0), Count = 1,
    `_CreationInstant` = "2024-01-01 00:00:00.000", check.names = FALSE
  ), data_dir, "diagnoses")
  data_dir
}

# The root is a copy of the project marker inside output/, so nothing outside output/ is written.
make_fake_root <- function(dir) {
  root <- file.path(dir, "fake_root")
  dir.create(root)
  file.copy(file.path(testthat::test_path("..", ".."), "PROJECT_VERSION"), root)
  root
}

test_that("read_utmb_table reads character columns, NULL as missing, header only mode", {
  dir <- local_output_dir()
  data_dir <- make_utmb_dir(dir)
  dt <- read_utmb_table(file.path(data_dir, "demographics"))
  expect_equal(nrow(dt), 3)
  expect_true(all(vapply(dt, is.character, logical(1))))
  expect_equal(sum(is.na(dt$DeathDate)), 2)
  hdr <- read_utmb_table(file.path(data_dir, "demographics"), header_only = TRUE)
  expect_equal(nrow(hdr), 0)
  expect_equal(names(hdr), names(dt))
  expect_error(read_utmb_table(file.path(data_dir, "nope")), "not found")
})

test_that("read_utmb_table makes repeated column names unique", {
  dir <- local_output_dir()
  writeLines(c("StudyID,A,A", "S1,1,2"), file.path(dir, "dup"))
  expect_equal(names(read_utmb_table(file.path(dir, "dup"))), c("StudyID", "A", "A_1"))
})

test_that("run_utmb_profile writes the expected files from synthetic tables", {
  dir <- local_output_dir()
  data_dir <- make_utmb_dir(dir)
  root <- make_fake_root(dir)
  run_dir <- run_utmb_profile(data_dir, root, label = "unit test", progress = function(...) NULL)

  expect_true(startsWith(normalizePath(run_dir), normalizePath(file.path(root, "output"))))
  expect_match(basename(run_dir), "^profile_[0-9]{8}_[0-9]{6}_unit_test$")
  expect_setequal(list.files(run_dir),
                  c("column_profile.csv", "table_stats.csv", "demographics_categorical.csv",
                    "demographics_numeric.csv", "summary.txt"))

  ts <- utils::read.csv(file.path(run_dir, "table_stats.csv"), stringsAsFactors = FALSE)
  expect_equal(ts$table, c("crosswalk", "demographics", "diagnoses", "hospital_admissions"))
  adm <- ts[ts$table == "hospital_admissions", ]
  expect_equal(c(adm$n_rows, adm$n_patients, adm$n_patients_not_in_crosswalk,
                 adm$n_crosswalk_patients_absent), c(4, 3, 1, 1))
  expect_equal(c(adm$date_earliest, adm$date_latest), c("2020-01-01", "2021-01-01"))
  dx <- ts[ts$table == "diagnoses", ]
  expect_equal(dx$n_duplicate_rows, 1)
  expect_equal(dx$date_column, "StartDate")
  expect_true(all(c("read_seconds", "profile_seconds") %in% names(ts)))

  cp <- utils::read.csv(file.path(run_dir, "column_profile.csv"), stringsAsFactors = FALSE,
                        na.strings = character())
  key <- cp[cp$table == "diagnoses" & cp$column == "DiagnosisKey", ]
  expect_equal(key$n_sentinel, 1)
  expect_equal(key$role, "surrogate_key")
  comment <- cp[cp$table == "diagnoses" & cp$column == "Comment", ]
  expect_equal(c(comment$top_values, comment$min), c("", ""))
  expect_equal(comment$n_missing, 2)
  mrn <- cp[cp$table == "crosswalk" & cp$column == "MRN", ]
  expect_equal(c(mrn$top_values, mrn$min), c("", ""))
  prim <- cp[cp$table == "diagnoses" & cp$column == "IsPrimary", ]
  expect_equal(c(prim$inferred_type, prim$top_values), c("boolean", "0 (3) | 1 (2)"))

  written <- paste(unlist(lapply(list.files(run_dir, full.names = TRUE), readLines)),
                   collapse = "\n")
  expect_false(grepl("Fake One|M2|E3|free text", written))

  dict <- utils::read.csv(file.path(root, "metadata", "utmb_dictionary.csv"),
                          stringsAsFactors = FALSE)
  expect_equal(nrow(dict), nrow(cp))
  expect_equal(dict$table, sort(dict$table, method = "radix"))
  expect_true(dict$identifier[dict$column == "PatientName"])
  expect_false(any(grepl("Fake|M2|free text", unlist(dict))))

  cat_df <- utils::read.csv(file.path(run_dir, "demographics_categorical.csv"),
                            stringsAsFactors = FALSE)
  expect_equal(cat_df$n[cat_df$variable == "deceased" & cat_df$level == "yes"], 1)
  expect_equal(cat_df$n[cat_df$variable == "patients" & cat_df$level == "both"], 3)
  num_df <- utils::read.csv(file.path(run_dir, "demographics_numeric.csv"))
  expect_equal(num_df$max[num_df$variable == "hospital_admissions_per_patient"], 2)
  expect_true(any(grepl("UTMB profile summary", readLines(file.path(run_dir, "summary.txt")))))
})

test_that("run_utmb_profile can skip the dictionary and needs a confirmed root and a crosswalk", {
  dir <- local_output_dir()
  data_dir <- make_utmb_dir(dir)
  root <- make_fake_root(dir)
  run_utmb_profile(data_dir, root, write_dictionary = FALSE, progress = function(...) NULL)
  expect_false(file.exists(file.path(root, "metadata", "utmb_dictionary.csv")))

  bare <- file.path(dir, "bare")
  dir.create(bare)
  expect_error(run_utmb_profile(data_dir, bare, progress = function(...) NULL), "not confirmed")
  expect_false(dir.exists(file.path(bare, "output")))

  no_xw <- file.path(dir, "no_xw")
  dir.create(no_xw)
  file.copy(file.path(data_dir, "demographics"), no_xw)
  expect_error(run_utmb_profile(no_xw, root, progress = function(...) NULL), "crosswalk")
  expect_error(run_utmb_profile(file.path(dir, "empty"), root), "No UTMB tables")
})
