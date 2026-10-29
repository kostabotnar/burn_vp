cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
rng <- read_qc_ranges(testthat::test_path("..", "..", "metadata", "qc_ranges.csv"))
root <- normalizePath(testthat::test_path("..", ".."))

# nolint start: object_usage_linter. Helpers call functions sourced from R/.
write_site_file <- function(dir, name, n, seed, corrupt = FALSE) {
  d <- generate_synthetic_nbc(cb, n = n, seed = seed)
  if (corrupt) d <- corrupt_synthetic(d, cb, n_per_type = 3, seed = seed)
  path <- file.path(dir, name)
  write_nbc_xlsx(d, cb, path)
  list(path = path, data = d)
}

# nolint end

test_that("run_pipeline writes every expected file and keeps patient data in output/", {
  dir <- local_output_dir()
  a <- write_site_file(dir, "a.xlsx", 50, 1)
  b <- write_site_file(dir, "b.xlsx", 50, 2)
  sub <- file.path("test-tmp", basename(dir), "runs")
  run <- suppressMessages(run_pipeline(c(NBC = a$path, UTMB = b$path), root, "unit", sub))
  expect_true(dir.exists(run))
  expect_match(basename(run), "^[0-9]{8}_[0-9]{6}_unit$")
  expect_true(startsWith(run, normalizePath(dir, winslash = "/")))
  expected <- c(
    "report.html",
    file.path("qc", paste0(c("findings", "rule_summary", "missingness", "parse_problems",
                             "unmapped_values", "rule_catalogue", "recode_log"), ".csv")),
    file.path("profile", paste0(c("variables_numeric", "variables_categorical",
                                  "site_comparison", "exposure_outcome", "correlation",
                                  "masked_counts"), ".csv")),
    file.path("figures", paste0(figure_catalogue()$id, ".png"))
  )
  expect_true(all(file.exists(file.path(run, expected))))
  html <- paste(readLines(file.path(run, "report.html"), warn = FALSE), collapse = "\n")
  expect_false(grepl("SYN-0", html, fixed = TRUE))
})

test_that("run_pipeline refuses an unconfirmed root and bad inputs", {
  expect_error(run_pipeline(c(NBC = "x.xlsx"), root = dirname(root)), "not confirmed")
  expect_error(run_pipeline(c("x.xlsx"), root), "unique site name")
  expect_error(run_pipeline(c(NBC = "x.xlsx", NBC = "y.xlsx"), root), "unique site name")
})

test_that("a second run in the same second gets its own folder", {
  dir <- local_output_dir()
  base <- file.path(dir, "r")
  dir.create(base)
  first <- new_run_dir(base, "x")
  second <- new_run_dir(base, "x")
  expect_false(identical(first, second))
})

test_that("a corrupted file is caught after the xlsx round trip", {
  dir <- local_output_dir()
  made <- write_site_file(dir, "c.xlsx", 60, 3, corrupt = TRUE)
  injected <- attr(made$data, "injected")
  raw <- suppressWarnings(read_site_data(made$path, cb, "NBC"))
  std <- standardise_values(raw, cb)
  qc <- run_qc(std, cb, rng)
  # No injection puts text into a numeric column, so none becomes a parse problem here.
  expect_equal(nrow(qc$parse_problems), 0)
  caught <- vapply(seq_len(nrow(injected)), function(i) {
    any(qc$findings$rule_id == injected$expected_rule_id[i] &
          qc$findings$row %in% injected$row[i])
  }, logical(1))
  expect_true(all(caught), info = paste(injected$error_type[!caught], collapse = ", "))
})

test_that("text in numeric, date and time columns survives the file as a parse problem", {
  dir <- local_output_dir()
  d <- generate_synthetic_nbc(cb, n = 20, seed = 4)
  d$age_years <- as.character(d$age_years)
  d$age_years[3] <- "abc"
  d$icu_admission_time[5] <- "late"
  d$burn_date <- as.character(as.numeric(d$burn_date) + 25569)
  d$burn_date[7] <- "yesterday"
  path <- file.path(dir, "t.xlsx")
  expect_no_error(write_nbc_xlsx(d, cb, path))
  raw <- suppressWarnings(read_site_data(path, cb, "NBC"))
  pp <- attr(raw, "parse_problems")
  expect_setequal(paste(pp$column, pp$row), c("age_years 3", "icu_admission_time 5", "burn_date 7"))
  expect_equal(raw$age_years[1], as.numeric(d$age_years[1]))
  expect_true(is.na(raw$age_years[3]))
})
