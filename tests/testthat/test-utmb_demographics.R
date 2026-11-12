for (f in list.files(testthat::test_path("..", "..", "R", "utmb"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

test_that("parse_utmb_dates reads dates, timestamps and keys", {
  d <- parse_utmb_dates(c("2024-02-03", "2024-02-03 10:11:12.123", "20240203", "-1", NA, "x"))
  expect_equal(d[1:3], rep(as.Date("2024-02-03"), 3))
  expect_true(all(is.na(d[4:6])))
})

test_that("utmb_age_years uses 365.25-day years", {
  expect_equal(utmb_age_years("2000-01-01", "2020-01-01"), 20)
  expect_equal(utmb_age_years(as.Date("1990-06-15"), as.Date("2020-06-15")), 30, tolerance = 0.01)
  expect_true(is.na(utmb_age_years(NA, "20200101")))
  expect_lt(utmb_age_years("2020-01-02", "2020-01-01"), 0)
})

test_that("utmb_age_band applies the boundaries", {
  age <- c(0, 17.99, 18, 39.99, 40, 64.99, 65, 90, NA, -1)
  expect_equal(as.character(utmb_age_band(age)),
               c("<18", "<18", "18-39", "18-39", "40-64", "40-64", "65+", "65+", "missing",
                 "missing"))
  expect_equal(levels(utmb_age_band(1)), c("<18", "18-39", "40-64", "65+", "missing"))
})

test_that("summarise_utmb_demographics gives aggregate tables", {
  demo <- data.table::data.table(
    StudyID = c("a", "b", "c", "d"),
    BirthDate = c("2000-01-01", "1990-01-01", "1940-01-01", NA),
    Sex = c("Male", "Female", "Male", NA),
    DeathDate = c(NA, NA, "2021-01-01", NA),
    Test = c("0", "0", "0", "1")
  )
  xw <- data.table::data.table(StudyID = c("a", "b", "c", "z"), BurnCode = c("1", "1", "2", "2"))
  adm <- data.table::data.table(
    StudyID = c("a", "a", "b", "c", "q"),
    AdmissionDateKey = c("20200101", "20190101", "20200101", "20100101", "20200101"),
    AdmissionTimeOfDayKey = c("100", "100", "100", "100", "100"),
    LengthOfStayInDays = c("4", "2", "10", "6", "1")
  )
  res <- summarise_utmb_demographics(demo, xw, adm)
  cat <- res$categorical
  get <- function(v, l) cat$n[cat$variable == v & cat$level == l]
  expect_equal(get("patients", "demographics"), 4)
  expect_equal(get("patients", "crosswalk"), 4)
  expect_equal(get("patients", "both"), 3)
  expect_equal(get("patients", "either"), 5)
  expect_equal(get("Sex", "(missing)"), 1)
  expect_equal(get("deceased", "yes"), 1)
  expect_equal(get("Test", "1"), 1)
  expect_equal(get("BurnCode", "2"), 2)
  expect_equal(get("age_band_at_first_admission", "18-39"), 2)
  expect_equal(get("age_band_at_first_admission", "65+"), 1)
  expect_equal(get("age_band_at_first_admission", "40-64"), 0)
  expect_equal(get("age_band_at_first_admission", "missing"), 1)
  expect_equal(get("age_at_first_admission_check", "valid"), 3)

  num <- res$numeric
  row <- function(v) num[num$variable == v, ]
  expect_equal(row("hospital_admissions_per_patient")$max, 2)
  expect_equal(row("hospital_admissions_per_patient")$n, 4)
  expect_equal(row("length_of_stay_first_admission_days")$median, 6)
  expect_equal(row("age_at_first_admission_years")$n, 3)
})

test_that("summarise_utmb_demographics counts an admission before birth separately", {
  demo <- data.table::data.table(StudyID = "a", BirthDate = "2021-01-01")
  adm <- data.table::data.table(StudyID = "a", AdmissionDateKey = "20200101")
  res <- summarise_utmb_demographics(demo, NULL, adm)
  cat <- res$categorical
  expect_equal(cat$n[cat$variable == "age_at_first_admission_check" &
                       cat$level == "admission before birth"], 1)
  expect_equal(cat$n[cat$variable == "age_band_at_first_admission" & cat$level == "missing"], 1)
})
