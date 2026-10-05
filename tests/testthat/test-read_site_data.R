small_codebook <- function() {
  data.frame(
    position = 1:7,
    source_name = c("Patient ID", "Age (years)", "Sex", "Date ICU admission", "Time ICU admission",
                    "Date ICU admission", "Count"),
    section = "Demographics",
    name = c("patient_id", "age_years", "sex", "icu_admission_date", "icu_admission_time",
             "outcome_icu_admission_date", "count"),
    type = c("text", "numeric", "binary", "date", "time", "date", "integer"),
    unit = NA_character_,
    allowed_values = c(NA, NA, "Yes|No", NA, NA, NA, NA),
    time_window = NA_character_,
    dictionary_name = NA_character_,
    key_variable = FALSE,
    notes = NA_character_,
    stringsAsFactors = FALSE
  )
}

# Writes a template-like workbook: a section row, a header row, then the given data rows.
write_site_xlsx <- function(dir, headers, rows = list(), file = "site.xlsx") {
  n <- length(headers)
  sections <- c("Section", rep(NA_character_, n - 1))
  all_rows <- c(list(sections, headers), rows)
  df <- as.data.frame(do.call(rbind, all_rows), stringsAsFactors = FALSE)
  path <- file.path(dir, file)
  writexl::write_xlsx(df, path, col_names = FALSE)
  path
}

row7 <- function(...) {
  vals <- list(...)
  out <- rep(NA_character_, 7)
  out[seq_along(vals)] <- unlist(vals)
  out
}

test_that("headers are checked by position and every problem is listed", {
  dir <- local_output_dir()
  cb <- small_codebook()
  headers <- cb$source_name
  headers[2] <- "Age"
  headers[3] <- "Gender"
  path <- write_site_xlsx(dir, headers, list(row7("a", "1")))
  err <- tryCatch(read_site_data(path, cb, "NBC"), error = conditionMessage)
  expect_match(err, "position 2: found 'Age', expected 'Age (years)'", fixed = TRUE)
  expect_match(err, "position 3: found 'Gender', expected 'Sex'", fixed = TRUE)
})

test_that("extra and missing columns are reported with positions", {
  dir <- local_output_dir()
  cb <- small_codebook()
  path <- write_site_xlsx(dir, c(cb$source_name, "Surprise"), list(c(row7("a"), "x")))
  expect_error(read_site_data(path, cb, "NBC"), "extra column at position 8: 'Surprise'")
  path <- write_site_xlsx(dir, cb$source_name[1:5], list(row7("a")[1:5]), file = "short.xlsx")
  expect_error(
    read_site_data(path, cb, "NBC"),
    "missing column at position 6: expected 'Date ICU admission'"
  )
  expect_error(read_site_data(path, cb, "NBC"), "missing column at position 7: expected 'Count'")
})

test_that("header whitespace is ignored but case is not", {
  dir <- local_output_dir()
  cb <- small_codebook()
  headers <- cb$source_name
  headers[2] <- "  Age (years) "
  path <- write_site_xlsx(dir, headers, list(row7("a", "1")))
  expect_no_error(read_site_data(path, cb, "NBC"))
  headers[2] <- "age (years)"
  path <- write_site_xlsx(dir, headers, list(row7("a", "1")), file = "case.xlsx")
  expect_error(read_site_data(path, cb, "NBC"), "position 2")
})

test_that("duplicate header text is resolved by position", {
  dir <- local_output_dir()
  cb <- small_codebook()
  path <- write_site_xlsx(dir, cb$source_name, list(row7("a", "1", "Yes", "44045", "0.5", "44046")))
  res <- read_site_data(path, cb, "NBC")
  expect_equal(res$icu_admission_date, as.Date("2020-08-02"))
  expect_equal(res$outcome_icu_admission_date, as.Date("2020-08-03"))
})

test_that("empty rows are dropped and row numbers refer to file data rows", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(row7("a", "1"), row7(), row7("-", "N/A", "", "NA"), row7("b", "x"))
  path <- write_site_xlsx(dir, cb$source_name, rows)
  expect_warning(res <- read_site_data(path, cb, "NBC"), "could not be parsed")
  expect_equal(nrow(res), 2)
  expect_equal(res$patient_id, c("a", "b"))
  expect_equal(attr(res, "parse_problems")$row, 4)
})

test_that("numbers accept a dot or a comma as decimal mark", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(row7("a", "60.5"), row7("b", "60,5"), row7("c", " 7 "), row7("d", "2.7E1"))
  res <- read_site_data(write_site_xlsx(dir, cb$source_name, rows), cb, "NBC")
  expect_equal(res$age_years, c(60.5, 60.5, 7, 27))
  expect_type(res$age_years, "double")
})

test_that("integers must be whole numbers", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(row7("a", NA, NA, NA, NA, NA, "3"), row7("b", NA, NA, NA, NA, NA, "2.5"))
  path <- write_site_xlsx(dir, cb$source_name, rows)
  expect_warning(res <- read_site_data(path, cb, "NBC"), "1 value")
  expect_equal(res$count, c(3L, NA))
  expect_equal(attr(res, "parse_problems")$raw_value, "2.5")
})

test_that("Excel serials and ISO strings become Date", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(row7("a", NA, NA, "44045"), row7("b", NA, NA, "2021-03-04"),
               row7("c", NA, NA, "44045.5"))
  res <- read_site_data(write_site_xlsx(dir, cb$source_name, rows), cb, "NBC")
  expect_s3_class(res$icu_admission_date, "Date")
  expect_equal(res$icu_admission_date, as.Date(c("2020-08-02", "2021-03-04", "2020-08-02")))
})

test_that("time fractions become HH:MM and HH:MM strings are kept", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(
    row7("a", NA, NA, NA, "0.479166666666667"), row7("b", NA, NA, NA, "2.7777777777777776E-2"),
    row7("c", NA, NA, NA, "7:05"), row7("d", NA, NA, NA, "0")
  )
  res <- read_site_data(write_site_xlsx(dir, cb$source_name, rows), cb, "NBC")
  expect_equal(res$icu_admission_time, c("11:30", "00:40", "07:05", "00:00"))
})

test_that("missing-value strings become NA in every column type", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(row7("a", "NA", "N/A", "-", "", "-", "NA"), row7("b", "1"))
  res <- read_site_data(write_site_xlsx(dir, cb$source_name, rows), cb, "NBC")
  expect_true(all(is.na(res[1, -(1:2)])))
  expect_equal(nrow(attr(res, "parse_problems")), 0)
})

test_that("text columns are trimmed but not recoded", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(row7(" p1 ", "1", " N0 "))
  res <- read_site_data(write_site_xlsx(dir, cb$source_name, rows), cb, "NBC")
  expect_equal(res$patient_id, "p1")
  expect_equal(res$sex, "N0")
})

test_that("failed parses are recorded with row and column and warned about once", {
  dir <- local_output_dir()
  cb <- small_codebook()
  rows <- list(
    row7("a", "10"),
    row7("b", "abc", "Yes", "yesterday", "25:00", "2021-02-30", "1.5"),
    row7("c", "1,234.5", NA, "44045", "7:5")
  )
  path <- write_site_xlsx(dir, cb$source_name, rows)
  warnings <- character()
  res <- withCallingHandlers(
    read_site_data(path, cb, "NBC"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1)
  expect_match(warnings, "7 value(s) could not be parsed", fixed = TRUE)
  pp <- attr(res, "parse_problems")
  expect_named(pp, c("row", "column", "raw_value", "expected_type"))
  expect_equal(nrow(pp), 7)
  hit <- function(row, col) pp[pp$row == row & pp$column == col, ]
  expect_equal(hit(2, "age_years")$raw_value, "abc")
  expect_equal(hit(2, "age_years")$expected_type, "numeric")
  expect_equal(hit(2, "icu_admission_date")$raw_value, "yesterday")
  expect_equal(hit(2, "icu_admission_time")$raw_value, "25:00")
  expect_equal(hit(2, "outcome_icu_admission_date")$raw_value, "2021-02-30")
  expect_equal(hit(2, "count")$expected_type, "integer")
  expect_equal(hit(3, "age_years")$raw_value, "1,234.5")
  expect_true(is.na(res$age_years[2]))
  expect_equal(res$icu_admission_date[3], as.Date("2020-08-02"))
})

test_that("site column and source_file attribute are set", {
  dir <- local_output_dir()
  cb <- small_codebook()
  path <- write_site_xlsx(dir, cb$source_name, list(row7("a", "1"), row7("b", "2")))
  res <- read_site_data(path, cb, "UTMB")
  expect_equal(names(res)[1:2], c("site", "patient_id"))
  expect_equal(res$site, c("UTMB", "UTMB"))
  expect_equal(attr(res, "source_file"), normalizePath(path))
  expect_equal(nrow(attr(res, "parse_problems")), 0)
})

test_that("a sheet can be chosen by name", {
  dir <- local_output_dir()
  cb <- small_codebook()
  df <- as.data.frame(rbind(c("S", rep(NA, 6)), cb$source_name, row7("z", "3")))
  path <- file.path(dir, "sheets.xlsx")
  writexl::write_xlsx(list(Other = data.frame(x = 1), Ark1 = df), path, col_names = FALSE)
  res <- read_site_data(path, cb, "NBC", sheet = "Ark1")
  expect_equal(res$patient_id, "z")
})

test_that("a missing file or an empty site is an error", {
  cb <- small_codebook()
  expect_error(read_site_data(file.path(local_output_dir(), "nope.xlsx"), cb, "NBC"), "not found")
  dir <- local_output_dir()
  path <- write_site_xlsx(dir, cb$source_name, list(row7("a")))
  expect_error(read_site_data(path, cb, ""), "'site'")
})

test_that("the real codebook headers load a made-up row", {
  cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
  dir <- local_output_dir()
  made_up <- rep(NA_character_, nrow(cb))
  made_up[cb$name == "patient_id"] <- "T1"
  made_up[cb$name == "age_years"] <- "41,5"
  made_up[cb$name == "gender"] <- "Female"
  made_up[cb$name == "hypertension"] <- "No"
  made_up[cb$name == "burn_date"] <- "44100"
  made_up[cb$name == "burn_time"] <- "0.25"
  made_up[cb$name == "outcome_icu_admission_date"] <- "2020-10-01"
  made_up[cb$name == "ne_24h"] <- "Yes"
  made_up[cb$name == "rbc_48h_units"] <- "2"
  path <- write_site_xlsx(dir, cb$source_name, list(made_up))
  res <- read_site_data(path, cb, "NBC")
  expect_equal(ncol(res), 197)
  expect_equal(nrow(res), 1)
  expect_equal(res$age_years, 41.5)
  expect_equal(res$burn_date, as.Date("2020-09-26"))
  expect_equal(res$burn_time, "06:00")
  expect_equal(res$outcome_icu_admission_date, as.Date("2020-10-01"))
  expect_true(is.na(res$icu_admission_date))
  expect_equal(res$rbc_48h_units, 2L)
  expect_equal(nrow(attr(res, "parse_problems")), 0)
})
