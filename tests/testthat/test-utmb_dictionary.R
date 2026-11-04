for (f in list.files(testthat::test_path("..", "..", "R", "utmb"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

test_that("infer_utmb_type recognises each type", {
  expect_equal(infer_utmb_type("X", c(NA, "")), "empty")
  expect_equal(infer_utmb_type("X", character()), "empty")
  expect_equal(infer_utmb_type("AdmissionDateKey", c("20240101", "-1")), "date_key")
  expect_equal(infer_utmb_type("ArrivalTimeOfDayKey", c("930", "-1", "0")), "time_key")
  expect_equal(infer_utmb_type("AdmissionDateKey", c("abc")), "text")
  expect_equal(infer_utmb_type("StartDate", c("2024-01-02", "2023-12-31")), "date")
  expect_equal(infer_utmb_type("TakenInstant", c("2024-01-02 03:04:05.123")), "datetime")
  expect_equal(infer_utmb_type("X", c("2024-01-02", "2024-01-02 10:00:00")), "datetime")
  expect_equal(infer_utmb_type("X", c("12", "-3", "0")), "integer")
  expect_equal(infer_utmb_type("X", c("1.5", "-2", ".25", "1e3")), "numeric")
  expect_equal(infer_utmb_type("X", c("1.5", "abc")), "text")
  expect_equal(infer_utmb_type("IsValid", "1"), "boolean")
  expect_equal(infer_utmb_type("Test", c("0", "1")), "boolean")
  expect_equal(infer_utmb_type("Quantity", "1"), "integer")
  expect_equal(infer_utmb_type("Count", "1"), "integer")
  expect_equal(infer_utmb_type("X", c("True", "false")), "boolean")
  expect_equal(infer_utmb_type("Code", c("007", "12")), "text")
})

test_that("is_utmb_identifier flags direct identifiers and free text", {
  for (n in c("PatientName", "MRN", "PatientEpicId", "DOB", "BirthDate", "DeathDate",
              "EncounterEpicCsn", "HospitalAccountEpicId", "Comment", "ResultComment")) {
    expect_true(is_utmb_identifier(n), info = n)
  }
  for (n in c("StudyID", "PatientDurableKey", "Sex", "FlowsheetRowEpicId", "Count",
              "LengthOfStayInDays", "Value")) {
    expect_false(is_utmb_identifier(n), info = n)
  }
})

test_that("assign_utmb_role follows Caboodle conventions", {
  expect_equal(assign_utmb_role("StudyID", "text"), "study_id")
  expect_equal(assign_utmb_role("_CreationInstant", "datetime"), "audit")
  expect_equal(assign_utmb_role("_IsDeleted", "boolean"), "audit")
  expect_equal(assign_utmb_role("PatientKey", "integer"), "surrogate_key")
  expect_equal(assign_utmb_role("PatientDurableKey", "integer"), "surrogate_key")
  expect_equal(assign_utmb_role("AdmissionDateKey", "date_key"), "date_key")
  expect_equal(assign_utmb_role("AdmissionTimeOfDayKey", "time_key"), "time_key")
  expect_equal(assign_utmb_role("TakenInstant", "datetime"), "instant")
  expect_equal(assign_utmb_role("StartDate", "date"), "instant")
  expect_equal(assign_utmb_role("MRN", "text"), "identifier")
  expect_equal(assign_utmb_role("DeathDate", "date"), "identifier")
  expect_equal(assign_utmb_role("Comment", "text"), "free_text")
  expect_equal(assign_utmb_role("IsPrimary", "boolean"), "flag")
  expect_equal(assign_utmb_role("IsPrn", "text", 3), "flag")
  expect_equal(assign_utmb_role("LengthOfStayInDays", "numeric", 40), "measure")
  expect_equal(assign_utmb_role("Count", "integer", 1), "other")
  expect_equal(assign_utmb_role("Sex", "text", 3), "category")
  expect_equal(assign_utmb_role("DiagnosisName", "text", 500), "other")
  expect_equal(assign_utmb_role("Empty", "empty", 0), "other")
  expect_equal(assign_utmb_role("FlowsheetRowEpicId_1", "integer", 9), "other")
  expect_equal(assign_utmb_role("MRN_1", "text", 9), "identifier")
})

test_that("describe_utmb_column describes by convention and stays blank otherwise", {
  expect_match(describe_utmb_column("AdmissionDateKey"), "^Date of admission, as a YYYYMMDD key")
  expect_match(describe_utmb_column("PrimarySurgeonDurableKey"), "^Durable key for primary")
  expect_match(describe_utmb_column("TakenInstant"), "^Timestamp of taken")
  expect_match(describe_utmb_column("_LastUpdatedInstant"), "^ETL audit field: last updated")
  expect_equal(describe_utmb_column("StudyID"), "Study patient identifier")
  expect_equal(describe_utmb_column("BurnCode"), "")
  long_dashes <- paste0("[", intToUtf8(c(0x2013, 0x2014)), "]")
  expect_false(grepl(long_dashes, describe_utmb_column("AgeKey")))
})

test_that("build_utmb_dictionary has the documented columns and no data values", {
  d <- build_utmb_dictionary("t", c("StudyID", "MRN", "Sex"), c("text", "text", "text"),
                             c(10, 10, 2))
  expect_equal(names(d), c("table", "position", "column", "inferred_type", "role", "identifier",
                           "description"))
  expect_equal(d$role, c("study_id", "identifier", "category"))
  expect_equal(d$identifier, c(FALSE, TRUE, FALSE))
  expect_equal(d$position, 1:3)
})

test_that("write_utmb_dictionary refuses an unconfirmed root and orders rows", {
  dir <- local_output_dir()
  d <- rbind(build_utmb_dictionary("b", c("StudyID", "A"), c("text", "integer")),
             build_utmb_dictionary("a", c("StudyID"), c("text")))
  expect_error(write_utmb_dictionary(d, file.path(dir, "no_marker")), "not confirmed")
  expect_false(dir.exists(file.path(dir, "no_marker", "metadata")))

  root <- file.path(dir, "fake_root")
  dir.create(root)
  file.copy(file.path(testthat::test_path("..", ".."), "PROJECT_VERSION"), root)
  path <- write_utmb_dictionary(d, root)
  back <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(back$table, c("a", "b", "b"))
  expect_equal(back$position, c(1, 1, 2))
})
