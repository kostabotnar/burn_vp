cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
rng <- read_qc_ranges(testthat::test_path("..", "..", "metadata", "qc_ranges.csv"))
root <- normalizePath(testthat::test_path("..", ".."))

# nolint start: object_usage_linter. Helpers call functions sourced from R/.
build_report <- function(dir) {
  a <- cbind(site = "NBC", generate_synthetic_nbc(cb, n = 60, seed = 1), stringsAsFactors = FALSE)
  b <- cbind(site = "UTMB", generate_synthetic_nbc(cb, n = 60, seed = 2), stringsAsFactors = FALSE)
  d <- rbind(a, b)
  d$age_years[3] <- 150
  d$diabetes[70] <- "Maybe"
  qc <- run_qc(d, cb, rng)
  qc$findings <- localise_finding_rows(qc$findings, d)
  masked <- mask_flagged_values(d, qc$findings)
  analysis <- derive_analysis_variables(masked)
  corr <- correlation_matrix(analysis)
  figs <- save_profile_figures(
    build_profile_figures(analysis, qc$missingness, corr), dir
  )
  path <- file.path(dir, "report.html")
  inputs <- c(NBC = "a.xlsx", UTMB = "b.xlsx")
  run <- report_run_info(inputs, c(NBC = 60, UTMB = 60), root, cb,
                         file.path(root, "metadata", "codebook.csv"),
                         file.path(root, "metadata", "qc_ranges.csv"), qc)
  recodes <- data.frame(site = "NBC", row = 1L, column = "x", raw_value = "a", new_value = "b",
                        rule = "r")
  write_html_report(path, run, qc, recodes, attr(masked, "masked_counts"),
                    site_comparison(analysis, cb, pipeline_comparison_vars),
                    exposure_outcome_summary(analysis), figs, cb)
  path
}

# nolint end

test_that("the report has every section, figures with alt text, and no patient data", {
  dir <- local_output_dir()
  path <- build_report(dir)
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (h in report_sections) expect_true(grepl(h, html, fixed = TRUE), info = h)
  expect_equal(lengths(regmatches(html, gregexpr("data:image/png;base64,", html))), 6)
  expect_equal(lengths(regmatches(html, gregexpr("<img [^>]*alt=\"[^\"]+\"", html))), 6)
  expect_false(grepl("SYN-0", html, fixed = TRUE))
  expect_false(grepl("[\u2013\u2014]", html))
  expect_false(grepl("<script|https?://", html))
  expect_true(grepl("findings in total", html, fixed = TRUE))
  expect_false(grepl("age_years=150", html, fixed = TRUE))
})

test_that("write_html_report only writes inside output/", {
  expect_error(write_html_report("/tmp/r.html", NULL, NULL, NULL, NULL, NULL, NULL, NULL, cb),
               "output")
})
