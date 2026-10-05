std_codebook <- function() {
  data.frame(
    position = 1:8,
    source_name = paste("Column", 1:8),
    section = "S",
    name = c("flag", "gender", "comorbidities_list", "other_vp_24h", "note", "age", "when",
             "clock"),
    type = c("binary", "categorical", "categorical", "categorical", "text", "numeric", "date",
             "time"),
    unit = NA_character_,
    allowed_values = c("Yes|No", "Male|Female|Others",
                       "Hypertension|Diabetes|Cirrhosis|Cancer",
                       "Epinephrine|Phenylephrine|Vasopressin|Dobutamine|Dopamine|Others",
                       NA, NA, NA, NA),
    time_window = NA_character_,
    dictionary_name = NA_character_,
    key_variable = FALSE,
    notes = NA_character_,
    stringsAsFactors = FALSE
  )
}

std_data <- function(...) {
  d <- data.frame(
    site = "NBC", flag = NA_character_, gender = NA_character_, comorbidities_list = NA_character_,
    other_vp_24h = NA_character_, note = NA_character_, age = 1, when = as.Date("2020-01-01"),
    clock = "07:05", stringsAsFactors = FALSE
  )
  args <- list(...)
  d <- d[rep(1, if (length(args)) max(lengths(args)) else 1), ]
  for (nm in names(args)) d[[nm]] <- args[[nm]]
  rownames(d) <- NULL
  d
}

test_that("every binary mapping is applied, ignoring case and whitespace", {
  yes <- c("yes", "YES", "Y", "ja", "J", "true", "TRUE", "1", " yes ", "Yes")
  no <- c("no", "N", "nei", "Nei", "false", "FALSE", "0", "N0", "n0", " No  ", "No")
  res <- standardise_values(std_data(flag = c(yes, no)), std_codebook())
  expect_equal(res$flag, c(rep("Yes", length(yes)), rep("No", length(no))))
})

test_that("unmapped binary values are kept and listed, never set to NA", {
  d <- std_data(flag = c("maybe", "Yes", "2", NA))
  res <- standardise_values(d, std_codebook())
  expect_equal(res$flag, c("maybe", "Yes", "2", NA))
  unm <- attr(res, "unmapped_values")
  expect_equal(unm$row, c(1, 3))
  expect_equal(unm$column, c("flag", "flag"))
  expect_equal(unm$raw_value, c("maybe", "2"))
  expect_equal(nrow(attr(res, "recode_log")), 0)
})

test_that("categorical values match case-insensitively and by synonym", {
  d <- std_data(
    gender = c("male", " FEMALE ", "m", "Mann", "man", "F", "k", "Kvinne", "woman", "Male", "x"),
    other_vp_24h = c("epinephrine", "adrenaline", "Noradrenaline", "phenylephrine",
                     "Phenylepinephrine", "dobutamin", "Dopamine", "VASOPRESSIN", "foo", NA, NA)
  )
  res <- standardise_values(d, std_codebook())
  expect_equal(res$gender, c("Male", "Female", "Male", "Male", "Male", "Female", "Female",
                             "Female", "Female", "Male", "x"))
  # Norepinephrine is not a level of other_vp_24h, so noradrenaline stays unchanged.
  expect_equal(res$other_vp_24h, c("Epinephrine", "Epinephrine", "Noradrenaline", "Phenylephrine",
                                   "Phenylephrine", "Dobutamine", "Dopamine", "Vasopressin",
                                   "foo", NA, NA))
  unm <- attr(res, "unmapped_values")
  expect_equal(unm$column, c("other_vp_24h", "other_vp_24h", "gender"))
  expect_equal(unm$raw_value, c("Noradrenaline", "foo", "x"))
})

test_that("a synonym is used when its target level exists", {
  cb <- std_codebook()
  cb$allowed_values[4] <- "Norepinephrine|Epinephrine"
  res <- standardise_values(std_data(other_vp_24h = c("noradrenaline", "Adrenaline")), cb)
  expect_equal(res$other_vp_24h, c("Norepinephrine", "Epinephrine"))
})

test_that("comorbidity lists are split, de-duplicated and ordered by allowed values", {
  d <- std_data(comorbidities_list = c(
    "cancer, hypertension", "Diabetes;Hypertension ;diabetes", "Cirrhosis",
    "Cancer, Asthma, Hypertension, asthma", "Hypertension, Diabetes", NA
  ))
  res <- standardise_values(d, std_codebook())
  expect_equal(res$comorbidities_list, c(
    "Hypertension, Cancer", "Hypertension, Diabetes", "Cirrhosis",
    "Hypertension, Cancer, Asthma", "Hypertension, Diabetes", NA
  ))
  log <- attr(res, "recode_log")
  expect_equal(log$row, c(1, 2, 4))
  expect_true(all(log$rule == "list"))
  unm <- attr(res, "unmapped_values")
  expect_equal(unm$row, 4)
  expect_equal(unm$raw_value, "Asthma")
})

test_that("text columns are trimmed and whitespace collapsed only", {
  res <- standardise_values(std_data(note = c("  a   b ", "Yes", "x\t\ny", NA)), std_codebook())
  expect_equal(res$note, c("a b", "Yes", "x y", NA))
  log <- attr(res, "recode_log")
  expect_equal(log$row, c(1, 3))
  expect_equal(log$rule, c("text_whitespace", "text_whitespace"))
  expect_equal(nrow(attr(res, "unmapped_values")), 0)
})

test_that("numeric, date and time columns are untouched", {
  d <- std_data(flag = c("y", "n"), age = c(1.5, NA), when = as.Date(c("2020-02-02", NA)),
                clock = c(" 7:05 ", "yes"))
  res <- standardise_values(d, std_codebook())
  expect_identical(res$age, d$age)
  expect_identical(res$when, d$when)
  expect_identical(res$clock, d$clock)
  expect_identical(res$site, d$site)
})

test_that("recode_log has the documented columns and contents", {
  d <- std_data(flag = c("Yes", " n0", "ja"), gender = c("Male", "m", "FEMALE"))
  res <- standardise_values(d, std_codebook())
  log <- attr(res, "recode_log")
  expect_named(log, c("row", "column", "raw_value", "new_value", "rule"))
  expect_equal(log$row, c(2, 2, 3, 3))
  expect_equal(log$column, c("flag", "gender", "flag", "gender"))
  expect_equal(log$raw_value, c(" n0", "m", "ja", "FEMALE"))
  expect_equal(log$new_value, c("No", "Male", "Yes", "Female"))
  expect_equal(log$rule, c("binary", "synonym", "binary", "categorical_case"))
  expect_type(log$row, "integer")
  expect_named(attr(res, "unmapped_values"), c("row", "column", "raw_value"))
})

test_that("an empty log has the right columns and existing attributes are kept", {
  d <- std_data(flag = "Yes")
  attr(d, "parse_problems") <- data.frame(row = 4, column = "age")
  attr(d, "source_file") <- "some/file.xlsx"
  res <- standardise_values(d, std_codebook())
  expect_equal(attr(res, "parse_problems"), data.frame(row = 4, column = "age"))
  expect_equal(attr(res, "source_file"), "some/file.xlsx")
  expect_equal(dim(attr(res, "recode_log")), c(0L, 5L))
  expect_equal(dim(attr(res, "unmapped_values")), c(0L, 3L))
  expect_identical(names(res), names(d))
})

test_that("the project codebook has the Phenylephrine level", {
  cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
  levels <- strsplit(cb$allowed_values[cb$name == "other_vp_24h"], "|", fixed = TRUE)[[1]]
  expect_true("Phenylephrine" %in% levels)
  expect_false("Phenylepinephrine" %in% levels)
})

test_that("clean synthetic data passes through unchanged", {
  cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
  syn <- generate_synthetic_nbc(cb, n = 50)
  res <- standardise_values(syn, cb)
  expect_equal(nrow(attr(res, "recode_log")), 0)
  expect_equal(nrow(attr(res, "unmapped_values")), 0)
  for (nm in names(syn)) expect_identical(res[[nm]], syn[[nm]], info = nm)
})
