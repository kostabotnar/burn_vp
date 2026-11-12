# nolint start: object_usage_linter. Functions come from R/, sourced by the test runner.

project_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")

# A fake project folder under output/test-tmp with the entry file in its root and a scripts/
# folder. `marker = NULL` leaves out PROJECT_VERSION.
make_start_project <- function(marker = "Project: burn_vp", env = parent.frame()) {
  dir <- local_output_dir(env)
  root <- file.path(dir, "proj")
  dir.create(file.path(root, "scripts"), recursive = TRUE)
  if (!is.null(marker)) writeLines(c(marker, "Version: 9.9.9"), file.path(root, "PROJECT_VERSION"))
  writeLines(c("{", '  "R": {', '    "Version": "4.5.2"', "  },", '  "Packages": {', "  }", "}"),
             file.path(root, "renv.lock"))
  normalizePath(root, winslash = "/")
}

file_listing <- function(dir) {
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
  info <- file.info(file.path(dir, files))
  data.frame(files, size = info$size, mtime = info$mtime)
}

# Fakes that record what was called, in order. `tk = FALSE` makes the dialog unavailable, so
# the winDialog question (`ask`) is used. `after` is the button pressed after a completed setup.
make_fakes <- function(ask = TRUE, outcome = "complete", status = "ok", needed = TRUE,
                       tk = TRUE, cancel = FALSE, after = "open") {
  calls <- new.env()
  calls$log <- character()
  calls$lines <- character()
  calls$option_during <- "unset"
  note <- function(x) calls$log <- c(calls$log, x)
  list(
    calls = calls,
    args = list(
      ask = function(question) {
        note("ask")
        calls$question <- question
        ask
      },
      run_setup = function(root) {
        note("setup")
        calls$option_during <- getOption("burn_vp.setup_progress")
        if (is.function(calls$option_during)) calls$option_during("Step 1 of 5: Finding it")
        outcome
      },
      dialog = function(root, run_setup) {
        note("dialog")
        if (cancel) return("cancel")
        result <- run_setup(root, function(line) calls$lines <- c(calls$lines, line))
        calls$option_after <- getOption("burn_vp.setup_progress")
        if (identical(result, "complete")) after else "failed"
      },
      dialog_available = function() tk,
      open_window = function(root) {
        note("window")
        invisible(NULL)
      },
      check_r_version = function(locked) {
        note("version")
        list(status = status, message = "R version message.")
      },
      setup_needed = function(root) needed,
      say = function(...) invisible(NULL)
    )
  )
}

run_start <- function(script_dir, fakes, interactive = TRUE, windows = TRUE) {
  do.call(start_burn_vp, c(list(script_dir, is_interactive = interactive, is_windows = windows),
                           fakes$args))
}

test_that("setup_find_root accepts the project folder itself and the scripts folder under it", {
  root <- make_start_project()
  expect_equal(setup_find_root(root, is_interactive = FALSE), root)
  expect_equal(setup_find_root(file.path(root, "scripts"), is_interactive = FALSE), root)
})

test_that("setup_find_root checks the folder itself before its parent", {
  outer <- make_start_project()
  inner <- file.path(outer, "inner")
  dir.create(inner)
  writeLines(c("Project: burn_vp", "Version: 1"), file.path(inner, "PROJECT_VERSION"))
  expect_equal(setup_find_root(inner, is_interactive = FALSE), normalizePath(inner, "/"))
})

test_that("setup_find_root still rejects a folder that is not a confirmed project", {
  root <- make_start_project(marker = "Project: other")
  expect_error(setup_find_root(root, is_interactive = FALSE), "project folder")
  expect_error(setup_find_root(file.path(root, "scripts"), is_interactive = FALSE),
               "project folder")
})

test_that("start_burn_vp stops and writes nothing when the folder is not confirmed", {
  root <- make_start_project(marker = "Project: other")
  before <- file_listing(dirname(root))
  fakes <- make_fakes()
  expect_error(run_start(root, fakes, interactive = FALSE), "project folder")
  expect_equal(file_listing(dirname(root)), before)
  expect_length(fakes$calls$log, 0)
})

test_that("start_burn_vp finds the root from the root folder and from scripts/", {
  root <- make_start_project()
  for (dir in c(root, file.path(root, "scripts"))) {
    fakes <- make_fakes(needed = FALSE)
    expect_equal(run_start(dir, fakes), "window")
    expect_equal(fakes$calls$log, c("version", "window"))
  }
})

test_that("start_burn_vp stops on a failing R version check before anything else", {
  root <- make_start_project()
  fakes <- make_fakes(status = "fail")
  expect_error(run_start(root, fakes), "contact the project team")
  expect_false(any(c("ask", "dialog", "setup", "window") %in% fakes$calls$log))
})

test_that("the R version error names the version needed and the version running", {
  root <- make_start_project()
  fakes <- make_fakes(status = "fail")
  expect_error(
    do.call(start_burn_vp, c(list(script_dir = root, is_interactive = TRUE, is_windows = TRUE,
                                  r_version = numeric_version("4.6.1")),
                             fakes$args)),
    "needs R 4\\.5, but this R is version 4\\.6\\.1\\. Close R and open R 4\\.5 from the Start menu"
  )
})

test_that("start_burn_vp runs setup in the dialog and opens the window when asked", {
  root <- make_start_project()
  fakes <- make_fakes(after = "open")
  expect_equal(run_start(root, fakes), "setup_then_window")
  expect_equal(fakes$calls$log, c("version", "dialog", "setup", "window"))
})

test_that("start_burn_vp does not open the window when the dialog is closed after setup", {
  root <- make_start_project()
  fakes <- make_fakes(after = "close")
  expect_equal(run_start(root, fakes), "setup_closed")
  expect_equal(fakes$calls$log, c("version", "dialog", "setup"))
})

test_that("start_burn_vp stops after Cancel in the dialog without running setup", {
  root <- make_start_project()
  fakes <- make_fakes(cancel = TRUE)
  expect_error(run_start(root, fakes), "source start_burn_vp.R again.*click Start setup")
  expect_equal(fakes$calls$log, c("version", "dialog"))
})

test_that("start_burn_vp does not open the window when setup failed in the dialog", {
  root <- make_start_project()
  fakes <- make_fakes(outcome = "failed")
  expect_equal(run_start(root, fakes), "setup_failed")
  expect_equal(fakes$calls$log, c("version", "dialog", "setup"))
})

test_that("the progress option is set during setup, receives the lines, and is restored", {
  root <- make_start_project()
  before <- function(line) "previous"
  withr::local_options(burn_vp.setup_progress = before)
  fakes <- make_fakes()
  run_start(root, fakes)
  expect_true(is.function(fakes$calls$option_during))
  expect_false(identical(fakes$calls$option_during, before))
  expect_equal(fakes$calls$lines, "Step 1 of 5: Finding it")
  expect_identical(fakes$calls$option_after, before)
  expect_identical(getOption("burn_vp.setup_progress"), before)
})

test_that("the progress option is unset again after setup and after a setup error", {
  root <- make_start_project()
  withr::local_options(burn_vp.setup_progress = NULL)
  run_start(root, make_fakes())
  expect_null(getOption("burn_vp.setup_progress"))
  fakes <- make_fakes()
  fakes$args$run_setup <- function(root) stop("boom")
  expect_error(run_start(root, fakes), "boom")
  expect_null(getOption("burn_vp.setup_progress"))
})

test_that("without Tk start_burn_vp asks with a yes/no question and runs setup in the console", {
  root <- make_start_project()
  fakes <- make_fakes(ask = TRUE, tk = FALSE)
  expect_equal(run_start(root, fakes), "setup_then_window")
  expect_equal(fakes$calls$log, c("version", "ask", "setup", "window"))
  expect_match(fakes$calls$question, "several minutes")
  expect_match(fakes$calls$question, "internet")
  expect_false(is.function(fakes$calls$option_during))
})

test_that("without Tk a no stops the start and a failed setup shows no window", {
  root <- make_start_project()
  fakes <- make_fakes(ask = FALSE, tk = FALSE)
  expect_error(run_start(root, fakes), "source start_burn_vp.R again.*answer Yes")
  expect_equal(fakes$calls$log, c("version", "ask"))
  fakes <- make_fakes(outcome = "failed", tk = FALSE)
  expect_equal(run_start(root, fakes), "setup_failed")
  expect_equal(fakes$calls$log, c("version", "ask", "setup"))
})

test_that("start_burn_vp stops without asking when the session is not interactive", {
  root <- make_start_project()
  fakes <- make_fakes()
  expect_error(run_start(root, fakes, interactive = FALSE), "must run first")
  expect_error(run_start(root, fakes, windows = FALSE), "must run first")
  expect_false(any(c("ask", "dialog", "setup", "window") %in% fakes$calls$log))
})

test_that("start_burn_vp goes straight to the window when the packages are present", {
  root <- make_start_project()
  fakes <- make_fakes(needed = FALSE)
  expect_equal(run_start(root, fakes), "window")
  expect_equal(fakes$calls$log, c("version", "window"))
})

test_that("start_burn_vp writes nothing itself", {
  root <- make_start_project()
  before <- file_listing(dirname(root))
  for (fakes in list(make_fakes(), make_fakes(needed = FALSE), make_fakes(cancel = TRUE),
                     make_fakes(outcome = "failed"), make_fakes(after = "close"),
                     make_fakes(tk = FALSE), make_fakes(ask = FALSE, tk = FALSE))) {
    try(run_start(root, fakes), silent = TRUE)
  }
  expect_equal(file_listing(dirname(root)), before)
})

test_that("start_burn_vp shows a version warning but continues", {
  root <- make_start_project()
  fakes <- make_fakes(status = "warning", needed = FALSE)
  shown <- character()
  fakes$args$say <- function(...) shown <<- c(shown, paste0(...))
  expect_equal(run_start(root, fakes), "window")
  expect_true(any(grepl("R version message.", shown, fixed = TRUE)))
})

test_that("start_library_path follows the renv layout without a patch version", {
  expect_equal(start_library_path("C:/p", getRversion(), "x86_64-w64-mingw32"),
               paste0("C:/p/renv/library/windows/R-", format(getRversion()[1, 1:2]),
                      "/x86_64-w64-mingw32"))
  expect_match(start_library_path("C:/p", package_version("4.5.2"), "x"), "/R-4.5/x$")
})

test_that("start_read_lock_packages reads versions and dependencies from the real lockfile", {
  lock <- start_read_lock_packages(project_root)
  expect_true(all(project_packages %in% names(lock)))
  expect_true(all(nzchar(vapply(lock[project_packages], function(p) p$Version, character(1)))))
  closure <- setup_dependency_closure(lock, project_packages)
  expect_true(all(project_packages %in% closure))
  expect_gt(length(closure), length(project_packages))
})

# A fake library: one folder per package with a DESCRIPTION that has a Version.
make_fake_library <- function(root, versions) {
  library <- start_library_path(root)
  for (p in names(versions)) {
    dir.create(file.path(library, p), recursive = TRUE)
    writeLines(c(paste("Package:", p), paste("Version:", versions[[p]])),
               file.path(library, p, "DESCRIPTION"))
  }
  library
}

write_fake_lock <- function(root, versions) {
  body <- unlist(lapply(seq_along(versions), function(i) {
    c(sprintf('    "%s": {', names(versions)[i]),
      sprintf('      "Package": "%s",', names(versions)[i]),
      sprintf('      "Version": "%s",', versions[[i]]),
      '      "Source": "Repository"',
      if (i < length(versions)) "    }," else "    }")
  }))
  writeLines(c("{", '  "R": {', '    "Version": "4.5.2"', "  },", '  "Packages": {', body,
               "  }", "}"), file.path(root, "renv.lock"))
}

test_that("start_setup_needed compares the project library with renv.lock", {
  root <- make_start_project()
  versions <- as.list(setNames(rep("1.0.0", length(project_packages) + 1),
                               c(project_packages, "renv")))
  write_fake_lock(root, versions)
  expect_true(start_setup_needed(root))
  make_fake_library(root, versions)
  expect_false(start_setup_needed(root))
  writeLines(c("Package: ggplot2", "Version: 0.9.0"),
             file.path(start_library_path(root), "ggplot2", "DESCRIPTION"))
  expect_true(start_setup_needed(root))
})

test_that("start_setup_needed is TRUE when renv is missing from the library", {
  root <- make_start_project()
  versions <- as.list(setNames(rep("1.0.0", length(project_packages) + 1),
                               c(project_packages, "renv")))
  write_fake_lock(root, versions)
  make_fake_library(root, versions[project_packages])
  expect_true(start_setup_needed(root))
})

test_that("the entry file runs inside local() and never calls quit", {
  entry <- file.path(project_root, "start_burn_vp.R")
  expect_true(file.exists(entry))
  text <- readLines(entry)
  expect_true(any(grepl("^local\\(\\{", text)))
  expect_false(any(grepl("quit\\(|q\\(", text)))
})
# nolint end
