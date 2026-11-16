for (f in list.files(testthat::test_path("..", "..", "R", "utmb"), pattern = "deid_data\\.R$",
                     full.names = TRUE)) {
  source(f)
}

fake_root <- function(dir) {
  root <- file.path(dir, "fake_root")
  dir.create(file.path(root, "output", "utmb"), recursive = TRUE)
  dir.create(file.path(root, "data"))
  root
}

make_run <- function(root, name, result = "OK", summary = TRUE, actions = TRUE) {
  run <- file.path(root, "output", "utmb", name)
  dir.create(file.path(run, "data"), recursive = TRUE)
  if (summary) writeLines(c("UTMB de-identification", "", paste0("Result: ", result)),
                          file.path(run, "summary.txt"))
  if (actions) writeLines("table,column", file.path(run, "column_actions.csv"))
  normalizePath(file.path(run, "data"), winslash = "/")
}

test_that("no de-identified run stops and names deid_utmb.R", {
  root <- fake_root(local_output_dir())
  expect_error(resolve_utmb_deid_data(root), "deid_utmb.R")
  unlink(file.path(root, "output"), recursive = TRUE)
  expect_error(resolve_utmb_deid_data(root), "deid_utmb.R")
})

test_that("the newest valid run is picked", {
  root <- fake_root(local_output_dir())
  make_run(root, "deid_20240101_100000")
  newest <- make_run(root, "deid_20240301_090000_study")
  make_run(root, "deid_20240201_120000")
  expect_equal(resolve_utmb_deid_data(root), newest)
})

test_that("keys folders and incomplete or failed runs are ignored", {
  root <- fake_root(local_output_dir())
  good <- make_run(root, "deid_20240101_100000")
  make_run(root, "deid_keys_20240901_100000")
  make_run(root, "deid_20240301_100000", summary = FALSE)
  make_run(root, "deid_20240302_100000", actions = FALSE)
  make_run(root, "deid_20240303_100000", result = "FAILED, do not use these files")
  expect_equal(resolve_utmb_deid_data(root), good)
})

test_that("only keys or incomplete runs counts as no data", {
  root <- fake_root(local_output_dir())
  make_run(root, "deid_keys_20240901_100000")
  make_run(root, "deid_20240301_100000", summary = FALSE)
  expect_error(resolve_utmb_deid_data(root), "deid_utmb.R")
})

test_that("an explicit raw data folder is refused", {
  root <- fake_root(local_output_dir())
  make_run(root, "deid_20240101_100000")
  expect_error(resolve_utmb_deid_data(root, file.path(root, "data")), "de-identified")
  expect_error(resolve_utmb_deid_data(root, "data"), "deid_utmb.R")
})

test_that("an explicit valid de-identified data folder is accepted", {
  root <- fake_root(local_output_dir())
  old <- make_run(root, "deid_20240101_100000")
  make_run(root, "deid_20240301_100000")
  expect_equal(resolve_utmb_deid_data(root, old), old)
})

test_that("a relative path is resolved against the project root", {
  root <- fake_root(local_output_dir())
  run <- make_run(root, "deid_20240101_100000")
  expect_equal(resolve_utmb_deid_data(root, "output/utmb/deid_20240101_100000/data"), run)
})

test_that("paths outside output/utmb or with .. tricks are refused", {
  root <- fake_root(local_output_dir())
  make_run(root, "deid_20240101_100000")
  elsewhere <- make_run(file.path(root, "other"), "deid_20240101_100000")
  expect_error(resolve_utmb_deid_data(root, elsewhere), "de-identified")
  tricky <- file.path(root, "output", "utmb", "deid_20240101_100000", "data", "..", "..", "..",
                      "..", "data")
  expect_error(resolve_utmb_deid_data(root, tricky), "de-identified")
  keys <- make_run(root, "deid_keys_20240101_100000")
  expect_error(resolve_utmb_deid_data(root, keys), "de-identified")
  expect_error(resolve_utmb_deid_data(root, file.path(root, "output", "utmb", "nothing")),
               "de-identified")
})

test_that("an explicit run whose leak check failed or is incomplete is refused", {
  root <- fake_root(local_output_dir())
  failed <- make_run(root, "deid_20240101_100000", result = "FAILED, do not use these files")
  partial <- make_run(root, "deid_20240102_100000", summary = FALSE)
  expect_error(resolve_utmb_deid_data(root, failed), "de-identified")
  expect_error(resolve_utmb_deid_data(root, partial), "de-identified")
})
