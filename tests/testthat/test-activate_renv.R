root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
rscript <- file.path(R.home("bin"), "Rscript")

# Runs Rscript with `wd` as the child's working directory and returns output plus exit status.
run_in <- function(wd, args) {
  old <- setwd(wd)
  on.exit(setwd(old))
  out <- suppressWarnings(system2(rscript, args, stdout = TRUE, stderr = TRUE))
  list(out = out, status = attr(out, "status") %||% 0L)
}

test_that("run_qc_pipeline.R works from a folder without .Rprofile", {
  skip_on_os("windows")
  work <- local_output_dir()
  expect_false(file.exists(file.path(work, ".Rprofile")))
  gen <- run_in(work, c(file.path(root, "scripts", "generate_synthetic_nbc.R"), "30", "3"))
  expect_equal(gen$status, 0L)
  xlsx <- file.path(root, "output", "generated", "NBC_synthetic_n30_seed3.xlsx")
  withr::defer(unlink(xlsx))
  expect_true(file.exists(xlsx))
  res <- run_in(work, c(file.path(root, "scripts", "run_qc_pipeline.R"),
                        paste0("NBC=", xlsx), "--label", "renvtest"))
  expect_equal(res$status, 0L)
  report <- sub("^Report: ", "", grep("^Report: ", res$out, value = TRUE))
  report <- trimws(sub(" $", "", report))
  withr::defer(unlink(dirname(report), recursive = TRUE))
  expect_true(file.exists(report))
  expect_true(startsWith(normalizePath(report, winslash = "/"), file.path(root, "output", "runs")))
})

test_that("activate_project_renv targets the given root and keeps the working directory", {
  skip_on_os("windows")
  work <- local_output_dir()
  script <- file.path(work, "probe.R")
  writeLines(c(
    sprintf('source("%s")', file.path(root, "R", "project_paths.R")),
    sprintf('activate_project_renv("%s")', root),
    'cat("WD=", normalizePath(getwd(), winslash = "/"), "\\n", sep = "")',
    'cat("LIB=", normalizePath(.libPaths()[1], winslash = "/"), "\\n", sep = "")'
  ), script)
  res <- run_in(work, script)
  expect_equal(res$status, 0L)
  wd_line <- grep("^WD=", res$out, value = TRUE)
  expect_equal(sub("^WD=", "", wd_line), normalizePath(work, winslash = "/"))
  expect_true(startsWith(sub("^LIB=", "", grep("^LIB=", res$out, value = TRUE)),
                         paste0(root, "/renv/library")))
})

test_that("activate_project_renv is harmless when renv is already active", {
  skip_if_not(startsWith(normalizePath(.libPaths()[1], winslash = "/"), paste0(root, "/renv/")))
  before <- list(wd = getwd(), libs = .libPaths())
  expect_no_error(activate_project_renv(root))
  expect_identical(getwd(), before$wd)
  expect_identical(.libPaths(), before$libs)
})

test_that("activate_project_renv stops with restore advice for missing packages", {
  expect_error(activate_project_renv(root, packages = "notapackage.burnvp"),
               "renv::restore\\(\\).*notapackage.burnvp|notapackage.burnvp.*renv::restore\\(\\)")
})

test_that("activate_project_renv refuses an unconfirmed folder", {
  expect_error(activate_project_renv(dirname(root)), "not confirmed")
})
