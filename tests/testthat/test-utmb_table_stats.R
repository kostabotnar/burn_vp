for (f in list.files(testthat::test_path("..", "..", "R", "utmb"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

test_that("utmb_value_counts drops missing values and orders by count", {
  cnt <- utmb_value_counts(c("b", "a", "a", NA, "", "c", "b", "a"))
  expect_equal(cnt$v, c("a", "b", "c"))
  expect_equal(cnt$N, c(3L, 2L, 1L))
})

test_that("count_utmb_sentinels counts negative values in Key columns only", {
  cnt <- utmb_value_counts(c("-1", "-1", "-2", "5", "7", "-3"))
  s <- count_utmb_sentinels("PatientKey", cnt)
  expect_equal(s$n, 4L)
  expect_equal(s$values, "-1 (2) | -2 (1) | -3 (1)")
  expect_equal(count_utmb_sentinels("AdmissionDateKey", cnt)$n, 4L)
  expect_equal(count_utmb_sentinels("Occurrence", cnt)$n, 0L)
  expect_equal(count_utmb_sentinels("PatientKey", utmb_value_counts(c("1", "2")))$n, 0L)
})

test_that("top values are suppressed for identifiers, free text and many levels", {
  cnt <- utmb_value_counts(c(rep("x", 3), rep("y", 2), "z", "w", "v", "u"))
  expect_equal(format_utmb_top_values(cnt, FALSE, "category"),
               "x (3) | y (2) | u (1) | v (1) | w (1)")
  expect_equal(format_utmb_top_values(cnt, TRUE, "identifier"), "")
  expect_equal(format_utmb_top_values(cnt, FALSE, "free_text"), "")
  many <- utmb_value_counts(as.character(1:51))
  expect_equal(format_utmb_top_values(many, FALSE, "measure"), "")
  fifty <- utmb_value_counts(as.character(1:50))
  expect_match(format_utmb_top_values(fifty, FALSE, "measure"), "^1 \\(1\\)")
})

test_that("utmb_min_max handles numeric, date and date_key types", {
  expect_equal(utmb_min_max(utmb_value_counts(c("10", "9", "-2")), "integer"),
               c(min = "-2", max = "10"))
  expect_equal(utmb_min_max(utmb_value_counts(c("2024-03-01", "2023-01-05")), "date"),
               c(min = "2023-01-05", max = "2024-03-01"))
  expect_equal(utmb_min_max(utmb_value_counts(c("-1", "20240105", "20230101")), "date_key"),
               c(min = "20230101", max = "20240105"))
  expect_equal(utmb_min_max(utmb_value_counts(c("a", "b")), "text"), c(min = "", max = ""))
})

test_that("profile_utmb_column reports missing, distinct, placeholder and sentinel counts", {
  x <- c("1", "2", "2", NA, "-1", "*Unspecified")
  p <- profile_utmb_column(x, "PatientKey", "t")
  expect_equal(p$n_rows, 6)
  expect_equal(p$n_missing, 1)
  expect_equal(p$pct_missing, 16.67)
  expect_equal(p$n_sentinel, 1)
  expect_equal(p$n_placeholder, 1)
  expect_equal(p$n_distinct, 4)
  expect_equal(p$role, "surrogate_key")
  expect_equal(p$min, "")

  m <- profile_utmb_column(c("3.5", "1", NA), "Value", "t")
  expect_equal(c(m$inferred_type, m$min, m$max), c("numeric", "1", "3.5"))

  id <- profile_utmb_column(c("a", "b"), "MRN", "t")
  expect_true(id$identifier)
  expect_equal(id$top_values, "")
})

test_that("compute_utmb_table_stats summarises patients, duplicates and dates", {
  dt <- data.table::data.table(
    StudyID = c("p1", "p1", "p2", "p2", "p9", "p9"),
    Day = c("20240101", "20240101", "20240301", "-1", "20230601", "20230601"),
    Const = "k", Empty = NA_character_
  )
  prof <- profile_utmb_columns(dt, "t")
  s <- compute_utmb_table_stats(dt, "t", prof, c("p1", "p2", "p3"), "Day", file_bytes = 10)
  expect_equal(s$n_rows, 6)
  expect_equal(s$n_cols, 4)
  expect_equal(s$n_patients, 3)
  expect_equal(s$n_patients_not_in_crosswalk, 1)
  expect_equal(s$n_crosswalk_patients_absent, 1)
  expect_equal(c(s$rows_per_patient_min, s$rows_per_patient_median, s$rows_per_patient_max),
               c(2, 2, 2))
  expect_equal(s$n_duplicate_rows, 2)
  expect_equal(s$n_empty_columns, 1)
  expect_equal(s$n_constant_columns, 1)
  expect_equal(s$date_column, "Day")
  expect_equal(c(s$date_earliest, s$date_latest), c("2023-06-01", "2024-03-01"))
  expect_equal(s$date_n_unparsed, 1)

  none <- compute_utmb_table_stats(dt, "t", prof, "p1", NA_character_)
  expect_true(is.na(none$date_column))
})
