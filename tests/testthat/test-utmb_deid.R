for (f in list.files(testthat::test_path("..", "..", "R", "utmb"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

deid_policy_file <- normalizePath(testthat::test_path("..", "..", "metadata",
                                                      "utmb_deid_policy.csv"))
deid_secret <- paste(rep("ab12", 16), collapse = "")
deid_secret_2 <- paste(rep("cd34", 16), collapse = "")

deid_root <- function(dir) {
  root <- file.path(dir, "fake_root")
  dir.create(root)
  file.copy(file.path(testthat::test_path("..", ".."), "PROJECT_VERSION"), root)
  root
}

write_deid_csv <- function(x, data_dir, name) {
  data.table::fwrite(x, file.path(data_dir, name), na = "")
}

# Synthetic extract using real column names. All values are made up.
make_deid_data <- function(dir, with_flowsheets = TRUE) {
  data_dir <- file.path(dir, "raw")
  dir.create(data_dir)
  write_deid_csv(data.frame(
    StudyID = c("1", "2", "3"), PatientDurableKey = c(11, 12, 13),
    PatientEpicId = c("Z100", "Z200", "Z300"), MRN = c("MRN001", "MRN002", "MRN003"),
    PatientName = c("Doe, Jane", "Roe, Rick", "Poe, Pat"),
    DOB = c("1980-05-17", "1990-12-31", "2001-02-28"), BurnCode = c("T31.2", "T31.1", "948.3"),
    BurnCodeDate = c("2019-06-15", "2020-02-29", "2021-01-02")
  ), data_dir, "crosswalk")
  write_deid_csv(data.frame(
    StudyID = c("1", "2", "3"), PatientDurableKey = c(11, 12, 13),
    BirthDate = c("1980-05-17", "1990-12-31", "2001-02-28"), Sex = c("F", "M", "M"),
    DeathDate = c(NA, NA, "2022-03-01"), DeathInstant = c(NA, NA, "2022-03-01 14:30:00")
  ), data_dir, "demographics")
  write_deid_csv(data.frame(
    StudyID = c("1", "1", "2", "3", NA), DiagnosisEventKey = c(501, 502, 503, 504, 505),
    EncounterKey = c(101, 101, 202, -1, 303), StartDate = c("2019-06-15", "2014-06-16",
                                                            "2020-02-29", "2021-01-02",
                                                            "2020-01-01"),
    CodeType = "ICD-10-CM", Code = c("T31.21", "T31.21", "T31.11", "T31.30", "T31.0"),
    DiagnosisKey = c(7, 7, 8, 9, 10), DepartmentKey = c(5, 5, 6, 6, 6), Type = "Billing Diagnosis"
  ), data_dir, "diagnoses")
  write_deid_csv(data.frame(
    StudyID = c("1", "2", "3"), EncounterKey = c(101, 202, 404),
    DateKey = c("20190615", "20200229", "-1"), AdmissionDateKey = c("20191231", "0", "abc"),
    Date = c("2019-06-15 08:15:00", "2020-02-29T23:59:59", NA),
    EncounterEpicCsn = c(9001, 9002, 9003),
    AttendingProviderKey = c(31, 32, -2), DepartmentKey = c(5, 6, 6),
    PatientClass = c("Inpt", "Emerg", "Outpt"), IsHospitalAdmission = c(1, 0, 0),
    IsEdVisit = c(0, 1, 0), Type = c("Hospital Encounter", "Hospital Encounter", "Appointment")
  ), data_dir, "encounters")
  if (with_flowsheets) {
    write_deid_csv(data.frame(
      StudyID = c("1", "1", "1", "2", "2"), FlowsheetValueKey = c(1, 2, 3, 4, 5),
      EncounterKey = c(101, 101, 101, 202, 202), FlowsheetRowKey = c(60, 60, 61, 62, 62),
      TakenByEmployeeKey = c(31, 31, 32, 32, 31),
      RowValueType = c("Numeric Type", "String Type", "Custom List", NA, "Temperature"),
      Value = c("98", "free text here", "Yes", "unknown type", "37.1"),
      Comment = c("note 1", "note 2", NA, "note 3", NA),
      TakenInstant = c("2019-06-15 09:00:00", "2019-06-15 10:00:00", "2019-06-16 11:30:00",
                       "2020-02-29 12:00:00", "2020-03-01 00:00:00")
    ), data_dir, "flowsheets")
  }
  data_dir
}

read_out <- function(run_dir, table) {
  read_utmb_table(file.path(run_dir, "data", table)) # nolint: object_usage_linter.
}

quiet <- function(...) NULL

test_that("offsets are never 0, stay within 30 days, and are stable per StudyID and secret", {
  ids <- as.character(1:5000)
  off <- utmb_deid_offsets(ids, deid_secret)
  expect_false(anyNA(off))
  expect_true(all(off != 0L))
  expect_true(all(abs(off) <= 30L))
  expect_setequal(unique(off), c(-30:-1, 1:30))
  expect_equal(utmb_deid_offsets(rev(ids), deid_secret), rev(off))
  expect_equal(utmb_deid_offsets(c("7", "7", NA), deid_secret)[1:2],
               rep(utmb_deid_offsets("7", deid_secret), 2))
  expect_true(is.na(utmb_deid_offsets(NA, deid_secret)))
  expect_false(identical(off, utmb_deid_offsets(ids, deid_secret_2)))
})

test_that("a new secret is 64 hex characters and a secret file is validated", {
  s <- new_utmb_secret()
  expect_match(s, "^[0-9a-f]{64}$")
  expect_false(identical(s, new_utmb_secret()))
  dir <- local_output_dir()
  good <- file.path(dir, "secret.txt")
  writeLines(toupper(deid_secret), good)
  expect_equal(read_utmb_secret(good), deid_secret)
  bad <- file.path(dir, "bad.txt")
  writeLines("short", bad)
  expect_error(read_utmb_secret(bad), "64 hexadecimal")
  expect_error(read_utmb_secret(file.path(dir, "none.txt")), "not found")
})

test_that("date shifting keeps intervals across month, year and leap day boundaries", {
  x <- c("2019-12-31", "2020-02-28", "2020-03-01", "2021-01-01", NA)
  moved <- utmb_shift_iso(x, rep(1L, 5))$value
  expect_equal(moved, c("2020-01-01", "2020-02-29", "2020-03-02", "2021-01-02", NA))
  back <- utmb_shift_iso(c("2020-03-01", "2020-01-01"), c(-1L, -1L))$value
  expect_equal(back, c("2020-02-29", "2019-12-31"))
  shifted <- utmb_shift_iso(c("2019-01-31", "2020-02-29"), c(-30L, -30L))$value
  expect_equal(as.numeric(diff(as.Date(shifted))),
               as.numeric(diff(as.Date(c("2019-01-31", "2020-02-29")))))
})

test_that("datetime shifting keeps the time of day and the text format", {
  x <- c("2020-02-29 23:59:59", "2020-02-29T08:00:00.123", "2020-02-29 08:00", "2020-02-29")
  res <- utmb_shift_iso(x, rep(2L, 4))
  expect_equal(res$value, c("2020-03-02 23:59:59", "2020-03-02T08:00:00.123",
                            "2020-03-02 08:00", "2020-03-02"))
  expect_equal(res$changed, 4L)
})

test_that("text that is not an ISO date is blanked and placeholder dates stay as they were", {
  res <- utmb_shift_iso(c("garbage", "2020-02-30", "9999-12-31", "0001-01-01", ""),
                        rep(5L, 5))
  expect_equal(res$value, c(NA, NA, "9999-12-31", "0001-01-01", NA))
  expect_equal(res$blanked, 3L)
  expect_equal(res$unchanged, 2L)
})

test_that("date keys shift as YYYYMMDD and sentinels stay unchanged", {
  x <- c("20191231", "20200229", "20240229", "-1", "0", "abc", "20231301", "2020022", NA)
  res <- utmb_shift_datekey(x, rep(1L, length(x)))
  expect_equal(res$value, c("20200101", "20200301", "20240301", "-1", "0", "abc", "20231301",
                            "2020022", NA))
  expect_equal(utmb_shift_datekey("20200301", -1L)$value, "20200229")
  expect_equal(res$changed, 3L)
})

test_that("remap is consistent within a domain, differs between domains, and spares negatives", {
  a <- utmb_deid_remap(c("101", "202", "101", "-1", NA, "-2"), "encounter", deid_secret)
  expect_equal(a[1], a[3])
  expect_false(a[1] == a[2])
  expect_match(a[1:2], "^[0-9a-f]{16}$")
  expect_equal(a[4], "-1")
  expect_equal(a[6], "-2")
  expect_true(is.na(a[5]))
  expect_false(identical(a[1], utmb_deid_remap("101", "ed_visit", deid_secret)))
  expect_false(identical(a[1], utmb_deid_remap("101", "encounter", deid_secret_2)))
  expect_equal(utmb_deid_remap("101", "encounter", deid_secret), a[1])
})

test_that("the policy generator assigns the expected actions", {
  dict <- data.table::fread(testthat::test_path("..", "..", "metadata", "utmb_dictionary.csv"),
                            colClasses = "character", data.table = FALSE)
  policy <- build_utmb_deid_policy(dict)
  expect_equal(nrow(policy), nrow(dict))
  expect_true(all(policy$action %in% utmb_deid_actions))
  expect_equal(policy$table, dict$table)
  get <- function(table, column) policy[policy$table == table & policy$column == column, ]
  expect_equal(get("demographics", "BirthDate")$action, "shift_date")
  expect_equal(get("demographics", "DeathInstant")$action, "shift_instant")
  expect_equal(get("crosswalk", "DOB")$action, "drop")
  expect_equal(get("crosswalk", "MRN")$action, "drop")
  expect_equal(get("crosswalk", "BurnCodeDate")$action, "shift_date")
  expect_equal(get("crosswalk", "StudyID")$action, "keep")
  expect_equal(get("flowsheets", "Comment")$action, "drop")
  expect_equal(get("flowsheets", "Value")$action, "keep_typed_or_common_values")
  expect_equal(get("flowsheets", "TakenInstant")$action, "shift_instant")
  expect_equal(get("flowsheets", "_CreationInstant")$action, "drop")
  expect_equal(get("flowsheets", "PatientKey")$action, "drop")
  expect_equal(get("ed_visits", "ArrivalDateKey")$action, "shift_datekey")
  expect_equal(get("ed_visits", "ArrivalDateKey")$domain, NA_character_)
  expect_equal(get("encounters", "DepartmentKey")$action, "keep")
  expect_equal(get("encounters", "EncounterKey")$action, "remap")
  expect_equal(get("encounters", "EncounterKey")$domain, "encounter")
  expect_equal(get("diagnoses", "EncounterKey")$domain, "encounter")
  expect_equal(get("surgical_cases", "SurgeryEncounterKey")$domain, "encounter")
  expect_equal(get("encounters", "AttendingProviderKey")$domain, "person_staff")
  expect_equal(get("flowsheets", "TakenByEmployeeKey")$domain, "person_staff")
  expect_equal(get("surgical_cases", "PrimarySurgeonDurableKey")$domain, "person_staff")
  expect_equal(get("surgical_cases", "PrecedingCaseBySurgeonKey")$domain, "surgical_case")
  expect_equal(get("surgical_cases", "PrimaryPreferenceCardDurableKey")$reason,
               "unclassified key, review")
  expect_equal(get("labs", "Value")$action, "keep_common_values")
  expect_equal(get("flowsheets", "TimeValue")$action, "keep_common_values")
  expect_equal(get("medication_orders", "Sig")$action, "keep_common_values")
  expect_equal(get("medication_orders", "SecondPrnReason")$action, "keep_common_values")
  expect_equal(get("labs", "LoincName")$reason, "text, review")
  remap <- policy[policy$action == "remap", ]
  expect_false(anyNA(remap$domain))
  expect_true(all(policy$reason[policy$action == "drop" &
                                  dict$role == "surrogate_key"] %in%
                    c("patient key, StudyID replaces it", "unclassified key, review")))
})

test_that("the committed policy matches what the generator produces", {
  dict <- data.table::fread(testthat::test_path("..", "..", "metadata", "utmb_dictionary.csv"),
                            colClasses = "character", data.table = FALSE)
  fresh <- build_utmb_deid_policy(dict)
  committed <- read_utmb_deid_policy(deid_policy_file)
  expect_equal(committed, fresh, ignore_attr = TRUE)
})

test_that("run_utmb_deid_policy writes only under output and needs a confirmed root", {
  dir <- local_output_dir()
  root <- deid_root(dir)
  dir.create(file.path(root, "metadata"))
  file.copy(testthat::test_path("..", "..", "metadata", "utmb_dictionary.csv"),
            file.path(root, "metadata"))
  path <- run_utmb_deid_policy(root, label = "t")
  expect_true(file.exists(path))
  expect_true(startsWith(normalizePath(path), normalizePath(file.path(root, "output"))))
  bare <- file.path(dir, "bare")
  dir.create(bare)
  expect_error(run_utmb_deid_policy(bare, dictionary_path = file.path(root, "metadata",
                                                                      "utmb_dictionary.csv")),
               "not confirmed")
  expect_false(dir.exists(file.path(bare, "output")))
})

test_that("a policy file with an unknown action or a remap without domain is refused", {
  dir <- local_output_dir()
  p <- file.path(dir, "p.csv")
  writeLines(c("table,column,action,domain,reason", "t,a,explode,,"), p)
  expect_error(read_utmb_deid_policy(p), "Unknown action")
  writeLines(c("table,column,action,domain,reason", "t,a,remap,,"), p)
  expect_error(read_utmb_deid_policy(p), "domain")
})

test_that("deid_utmb_table blanks flowsheet values by type and drops unlisted columns", {
  policy <- read_utmb_deid_policy(deid_policy_file)
  dt <- data.table::data.table(
    StudyID = c("1", "1", "1", "1", "1", NA),
    RowValueType = c("Numeric Type", "String Type", "Custom List", NA, "Blood Pressure", "x"),
    Value = c("98", "text", "Yes", "other", "120/80", "z"),
    Comment = "c", NewColumn = "n", TakenInstant = "2020-01-01 10:00:00"
  )
  res <- deid_utmb_table(dt, "flowsheets", policy, deid_secret)
  expect_equal(res$data$Value, c("98", NA, "Yes", NA, "120/80"))
  expect_equal(res$rows_in, 6L)
  expect_equal(res$rows_out, 5L)
  expect_equal(res$rows_no_id, 1L)
  expect_false(any(c("Comment", "NewColumn") %in% names(res$data)))
  expect_equal(res$unlisted, "NewColumn")
  expect_equal(res$stats$n_blanked[res$stats$column == "Value"], 2L)
  expect_equal(res$stats$status[res$stats$column == "NewColumn"], "unlisted_dropped")
  expect_true(all(c("FlowsheetValueKey", "Occurrence") %in%
                    res$stats$column[res$stats$status == "absent_in_data"]))
})

test_that("the pipeline writes de-identified tables, keys and reports, and passes the leak check", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir)
  root <- deid_root(dir)
  res <- run_utmb_deid(data_dir, root, label = "a", secret_file = NULL,
                       policy_path = deid_policy_file, progress = quiet)
  expect_true(res$ok)
  expect_null(res$leaks)
  output <- normalizePath(file.path(root, "output"))
  expect_true(startsWith(normalizePath(res$run_dir), output))
  expect_true(startsWith(normalizePath(res$keys_dir), output))
  expect_match(basename(res$run_dir), "^deid_[0-9]{8}_[0-9]{6}_a$")
  expect_match(basename(res$keys_dir), "^deid_keys_[0-9]{8}_[0-9]{6}_a$")
  expect_setequal(list.files(res$keys_dir), c("secret.txt", "offsets.csv", "README.txt"))
  expect_setequal(list.files(res$run_dir), c("data", "summary.txt", "column_actions.csv"))
  expect_setequal(list.files(file.path(res$run_dir, "data")),
                  c("crosswalk", "demographics", "diagnoses", "encounters", "flowsheets"))
  expect_match(readLines(file.path(res$keys_dir, "secret.txt")), "^[0-9a-f]{64}$")

  offsets <- data.table::fread(file.path(res$keys_dir, "offsets.csv"), colClasses = "character")
  expect_setequal(offsets$StudyID, c("1", "2", "3"))
  off <- stats::setNames(as.integer(offsets$offset_days), offsets$StudyID)
  expect_true(all(off != 0 & abs(off) <= 30))

  cw <- read_out(res$run_dir, "crosswalk")
  expect_equal(names(cw), c("StudyID", "BurnCode", "BurnCodeDate"))
  expect_equal(as.Date(cw$BurnCodeDate),
               as.Date(c("2019-06-15", "2020-02-29", "2021-01-02")) + unname(off[cw$StudyID]))

  dx <- read_out(res$run_dir, "diagnoses")
  expect_equal(nrow(dx), 4L)
  expect_false(any(c("PatientDurableKey", "_CreationInstant") %in% names(dx)))
  expect_equal(as.Date(dx$StartDate),
               as.Date(c("2019-06-15", "2014-06-16", "2020-02-29", "2021-01-02")) +
                 unname(off[dx$StudyID]))
  enc <- read_out(res$run_dir, "encounters")
  expect_false(any(c("EncounterEpicCsn", "PatientDurableKey") %in% names(enc)))
  expect_equal(enc$DepartmentKey, c("5", "6", "6"))
  run_secret <- read_utmb_secret(file.path(res$keys_dir, "secret.txt"))
  expect_equal(enc$EncounterKey[3], utmb_deid_remap("404", "encounter", run_secret))
  expect_equal(enc$AttendingProviderKey[3], "-2")
  expect_equal(enc$DateKey[3], "-1")
  expect_equal(enc$AdmissionDateKey[2:3], c("0", "abc"))
  expect_equal(enc$DateKey[1], format(as.Date("2019-06-15") + off[["1"]], "%Y%m%d"))
  expect_equal(substr(enc$Date[1], 12, 19), "08:15:00")
  expect_equal(substr(enc$Date[2], 11, 11), "T")
  expect_true(is.na(enc$Date[3]))

  # Same remapped value in two tables and the same offset across tables.
  expect_equal(dx$EncounterKey[1:2], rep(enc$EncounterKey[1], 2))
  fl <- read_out(res$run_dir, "flowsheets")
  expect_equal(fl$EncounterKey[1], enc$EncounterKey[1])
  expect_equal(fl$TakenByEmployeeKey[1], enc$AttendingProviderKey[1])
  expect_equal(fl$Value, c("98", NA, "Yes", NA, "37.1"))
  expect_false("Comment" %in% names(fl))
  expect_equal(as.Date(fl$TakenInstant[1:3]) - as.Date(dx$StartDate[1]),
               as.Date(c("2019-06-15", "2019-06-15", "2019-06-16")) - as.Date("2019-06-15"),
               ignore_attr = TRUE)

  demo <- read_out(res$run_dir, "demographics")
  expect_equal(as.Date(demo$BirthDate), as.Date(c("1980-05-17", "1990-12-31", "2001-02-28")) +
                 unname(off[demo$StudyID]))
  expect_equal(substr(demo$DeathInstant[3], 12, 19), "14:30:00")
  expect_true(is.na(demo$DeathDate[1]))

  summary <- readLines(file.path(res$run_dir, "summary.txt"))
  expect_true(any(grepl("rows dropped for missing StudyID: 1", summary)))
  expect_true(any(grepl("flowsheet Value blanked: 2", summary)))
  expect_true(any(grepl("Result: OK", summary)))
  expect_false(any(grepl("MRN00|Doe|1980-05", summary)))
  expect_false(any(grepl(intToUtf8(0x2013), summary, fixed = TRUE)))
  expect_false(any(grepl(intToUtf8(0x2014), summary, fixed = TRUE)))
  actions <- data.table::fread(file.path(res$run_dir, "column_actions.csv"))
  expect_true(all(c("table", "column", "action", "status") %in% names(actions)))
})

test_that("the same secret file gives the same offsets and pseudonyms in a later run", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir)
  root <- deid_root(dir)
  first <- run_utmb_deid(data_dir, root, label = "one", policy_path = deid_policy_file,
                         progress = quiet)
  second <- run_utmb_deid(data_dir, root, label = "two", policy_path = deid_policy_file,
                          secret_file = file.path(first$keys_dir, "secret.txt"), progress = quiet)
  third <- run_utmb_deid(data_dir, root, label = "three", policy_path = deid_policy_file,
                         progress = quiet)
  read_off <- function(r) data.table::fread(file.path(r$keys_dir, "offsets.csv"))
  expect_equal(read_off(first), read_off(second))
  expect_equal(read_out(first$run_dir, "encounters"), read_out(second$run_dir, "encounters"))
  expect_false(identical(read_out(first$run_dir, "encounters")$EncounterKey,
                         read_out(third$run_dir, "encounters")$EncounterKey))
})

test_that("an unlisted column is dropped, warned about and reported", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir, with_flowsheets = FALSE)
  extra <- data.table::fread(file.path(data_dir, "diagnoses"), colClasses = "character")
  extra$ZzNewColumn <- "secret-ish"
  write_deid_csv(extra, data_dir, "diagnoses")
  root <- deid_root(dir)
  run <- function() {
    run_utmb_deid(data_dir, root, policy_path = deid_policy_file, progress = quiet)
  }
  expect_warning(res <- run(), "diagnoses.ZzNewColumn")
  expect_true(res$ok)
  expect_false("ZzNewColumn" %in% names(read_out(res$run_dir, "diagnoses")))
  summary <- readLines(file.path(res$run_dir, "summary.txt"))
  expect_true(any(grepl("diagnoses.ZzNewColumn", summary, fixed = TRUE)))
})

test_that("the leak check finds a planted MRN, a patient name in other case and own birth date", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir, with_flowsheets = FALSE)
  dx <- data.table::fread(file.path(data_dir, "diagnoses"), colClasses = "character")
  dx$Code[2] <- "MRN002"
  dx$Type[3] <- "DOE, JANE"
  dx$CodeType[1] <- "1980-05-17"
  write_deid_csv(dx, data_dir, "diagnoses")
  root <- deid_root(dir)
  res <- run_utmb_deid(data_dir, root, policy_path = deid_policy_file, progress = quiet)
  expect_false(res$ok)
  hit <- function(check) res$leaks[res$leaks$check == check, ]
  expect_equal(hit("exact")$column, "Code")
  expect_equal(hit("exact")$n, 1L)
  expect_equal(hit("name")$column, "Type")
  expect_equal(hit("own_birth_date")$column, "CodeType")
  summary <- readLines(file.path(res$run_dir, "summary.txt"))
  expect_true(any(grepl("FAILED", summary)))
  expect_false(any(grepl("MRN002|DOE, JANE|1980-05-17", summary)))
})

test_that("the leak check rejects an output column the policy drops or does not list", {
  dir <- local_output_dir()
  policy <- read_utmb_deid_policy(deid_policy_file)
  out <- file.path(dir, "out")
  dir.create(out)
  write_deid_csv(data.frame(StudyID = "1", PatientDurableKey = "5", Nope = "x"), out,
                 "demographics")
  check <- utmb_deid_leak_check(out, policy, list(exact = character(0), names = character(0)))
  expect_setequal(check$violations$column, c("PatientDurableKey", "Nope"))
  expect_null(check$leaks)
})

test_that("the leak check ignores identifier values that equal a shifted date by chance", {
  dir <- local_output_dir()
  policy <- read_utmb_deid_policy(deid_policy_file)
  out <- file.path(dir, "out")
  dir.create(out)
  write_deid_csv(data.frame(StudyID = c("1", "2"), StartDate = c("20190314", "2019-03-14"),
                            Code = c("T31.0", "20190314")), out, "diagnoses")
  check <- utmb_deid_leak_check(out, policy, list(exact = c("20190314", "2019-03-14"),
                                                  names = character(0)))
  expect_equal(check$leaks$column, "Code")
  expect_equal(check$leaks$n, 1L)
})

test_that("nothing is written without a confirmed project root", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir, with_flowsheets = FALSE)
  bare <- file.path(dir, "bare")
  dir.create(bare)
  expect_error(run_utmb_deid(data_dir, bare, policy_path = deid_policy_file, progress = quiet),
               "not confirmed")
  expect_equal(list.files(bare, all.files = TRUE, no.. = TRUE), character(0))
})

test_that("burn_index runs on the de-identified fixture output", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir, with_flowsheets = FALSE)
  root <- deid_root(dir)
  res <- run_utmb_deid(data_dir, root, label = "bi", policy_path = deid_policy_file,
                       progress = quiet)
  burn_dir <- run_utmb_burn_index(file.path(res$run_dir, "data"), root, label = "bi",
                                  progress = quiet)
  events <- data.table::fread(file.path(burn_dir, "index_events.csv"), colClasses = "character")
  expect_setequal(events$StudyID, c("1", "2", "3"))
  expect_true(file.exists(file.path(burn_dir, "summary.txt")))
})

test_that("placeholder date bounds stay unchanged in both shift functions", {
  expect_equal(format(utmb_placeholder_low), "1900-01-01")
  expect_equal(format(utmb_placeholder_high), "9999-01-01")
  days <- c(5L, -5L, 5L, -5L, 5L, -5L, 5L, -5L)
  x <- c("1900-01-01", "1900-01-01 10:00:00", "9999-12-31", "9999-12-31", "9999-01-01",
         "1900-01-02", "9998-12-31", "1900-01-02")
  res <- utmb_shift_iso(x, days)
  expect_equal(res$value, c("1900-01-01", "1900-01-01 10:00:00", "9999-12-31", "9999-12-31",
                            "9999-01-01", "1899-12-28", "9999-01-05", "1899-12-28"))
  expect_equal(res$unchanged, 5L)
  keys <- c("19000101", "99991231", "99990101", "19000102", "99981231", "18991231")
  kres <- utmb_shift_datekey(keys, c(5L, -5L, 5L, 5L, 5L, 5L))
  expect_equal(kres$value, c("19000101", "99991231", "99990101", "19000107", "99990105",
                             "18991231"))
  expect_equal(kres$changed, 2L)
})

test_that("1900-01-01 stays a placeholder for burn_index after de-identification", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir, with_flowsheets = FALSE)
  dx <- data.table::fread(file.path(data_dir, "diagnoses"), colClasses = "character")
  dx$StartDate[1] <- "1900-01-01"
  write_deid_csv(dx, data_dir, "diagnoses")
  root <- deid_root(dir)
  res <- run_utmb_deid(data_dir, root, label = "ph", policy_path = deid_policy_file,
                       progress = quiet)
  out <- read_out(res$run_dir, "diagnoses")
  expect_equal(out$StartDate[1], "1900-01-01")
  expect_true(as.Date(out$StartDate[1]) <= burn_placeholder_cutoff)
})

test_that("keep_common_values keeps numeric-like values and values shared by enough patients", {
  expect_equal(utmb_deid_min_patients, 20L)
  ids <- c(as.character(1:20), as.character(1:19), "1", "1")
  x <- c(rep("Common text", 20), rep("Almost common", 19), "Rare text", "Rare text")
  res <- utmb_keep_common_values(x, ids)
  expect_equal(res$value[1:20], rep("Common text", 20))
  expect_true(all(is.na(res$value[21:41])))
  expect_equal(res$blanked, 21L)
  expect_equal(res$unchanged, 20L)
  forms <- c("5", "-5", "5.25", ".5", "5.", "<5", ">= 5.5", "<=-3", " 7 ", "5 mg", "1e5", "<", "",
             NA)
  kept <- utmb_keep_common_values(forms, rep("1", length(forms)))$value
  expect_equal(is.na(kept), c(rep(FALSE, 9), rep(TRUE, 5)))
  ex <- utmb_keep_common_values(rep("words", 25), as.character(1:25),
                                exempt = c(rep(TRUE, 6), rep(FALSE, 19)))
  expect_equal(sum(!is.na(ex$value)), 6L)
})

test_that("keep_common_values columns are filtered in the pipeline and counted", {
  dir <- local_output_dir()
  data_dir <- make_deid_data(dir, with_flowsheets = FALSE)
  n <- 22
  write_deid_csv(data.frame(
    StudyID = as.character(1:n), Value = c(rep("Positive", n - 1), "Jane Doe called"),
    ComponentName = "x"
  ), data_dir, "labs")
  root <- deid_root(dir)
  res <- run_utmb_deid(data_dir, root, label = "cv", policy_path = deid_policy_file,
                       progress = quiet)
  labs <- read_out(res$run_dir, "labs")
  expect_equal(sum(is.na(labs$Value)), 1L)
  actions <- data.table::fread(file.path(res$run_dir, "column_actions.csv"))
  expect_equal(actions$n_blanked[actions$table == "labs" & actions$column == "Value"], 1L)
})
