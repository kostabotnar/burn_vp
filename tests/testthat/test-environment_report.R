script_src <- normalizePath(
  testthat::test_path("..", "..", "scripts", "environment_report.R"),
  mustWork = TRUE
)

# Build a fake project (scripts/, PROJECT_VERSION, and optionally output/) and a fake
# home folder inside a fresh temporary folder. marker is "valid", "missing", "wrong"
# (another project name) or "bad" (not parseable). Returns the paths.
make_fake <- function(with_output = TRUE, marker = "valid", script_dir = "scripts") {
  root <- normalizePath(tempfile("fake"), winslash = "/", mustWork = FALSE)
  project <- file.path(root, "project")
  home <- file.path(root, "home", "JaneDoe")
  dir.create(file.path(project, script_dir), recursive = TRUE)
  dir.create(home, recursive = TRUE)
  if (with_output) dir.create(file.path(project, "output"))
  marker_file <- file.path(project, "PROJECT_VERSION")
  marker_texts <- list(
    valid = c("Project: burn_vp", "Version: 0.1.0"),
    wrong = c("Project: other_project", "Version: 0.1.0"),
    bad = c("this is not", "a control file", "Project burn_vp")
  )
  marker_text <- marker_texts[[marker]]
  if (!is.null(marker_text)) writeLines(marker_text, marker_file)
  file.copy(script_src, file.path(project, script_dir, "environment_report.R"))
  list(root = root, project = project, home = home,
       script = file.path(project, script_dir, "environment_report.R"))
}

# Run an R command line in a subprocess with a fake home folder and username.
run_r <- function(fake, args, extra_env = character()) {
  env <- c(paste0("HOME=", fake$home), paste0("USERPROFILE=", fake$home),
           "USER=JaneDoe", "USERNAME=JaneDoe", "LOGNAME=JaneDoe", extra_env)
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), args,
                           stdout = TRUE, stderr = TRUE, env = env))
}

all_files <- function(root) {
  sort(list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE))
}

skip_on_os("windows")

test_that("report is written only to the project output folder and masks names", {
  fake <- make_fake()
  # Library folders spelled with different capitalisation than the home path.
  lib_in_home <- file.path(dirname(fake$home), "JANEDOE", "Rlib")
  lib_elsewhere <- file.path(fake$root, "other", "JANEDOE", "Rlib")
  dir.create(lib_in_home, recursive = TRUE)
  dir.create(lib_elsewhere, recursive = TRUE)
  before <- all_files(fake$root)

  res <- run_r(fake, fake$script,
               paste0("R_LIBS_USER=", lib_in_home, ":", lib_elsewhere))
  expect_null(attr(res, "status"))

  report_file <- file.path(fake$project, "output", "environment_report.txt")
  expect_true(file.exists(report_file))
  expect_setequal(setdiff(all_files(fake$root), before),
                  c("project/output/environment_report.txt"))

  report <- paste(readLines(report_file), collapse = "\n")
  expect_false(grepl("JaneDoe", report, ignore.case = TRUE))
  expect_false(grepl(tolower(fake$home), tolower(report), fixed = TRUE))
  expect_match(report, "<home>", fixed = TRUE)
  expect_match(report, "<user>", fixed = TRUE)
  expect_match(report, "Can write to output folder: +yes")
  expect_match(report, "Project folder confirmed: +yes")
  expect_match(report, "Project version: +0.1.0")
  expect_match(paste(res, collapse = "\n"), "Report saved to:", fixed = TRUE)
})

test_that("a username inside another word is not masked", {
  fake <- make_fake()
  lib <- file.path(fake$root, "other", "xJaneDoex", "Rlib")
  dir.create(lib, recursive = TRUE)
  run_r(fake, fake$script, paste0("R_LIBS_USER=", lib))
  report <- readLines(file.path(fake$project, "output", "environment_report.txt"))
  expect_true(any(grepl("xJaneDoex", report, fixed = TRUE)))
})

# confirmed is FALSE when the project folder is expected to be not recognised.
expect_not_saved <- function(fake, before, confirmed = FALSE) {
  res <- run_r(fake, fake$script)
  testthat::expect_null(attr(res, "status"))
  testthat::expect_identical(all_files(fake$root), before)

  text <- paste(res, collapse = "\n")
  testthat::expect_match(text, "could not be saved", fixed = TRUE)
  testthat::expect_match(text, "copy the report text", fixed = TRUE)
  testthat::expect_match(text, "Output folder created: +no")
  testthat::expect_false(grepl("Report saved to", text, fixed = TRUE))
  testthat::expect_false(grepl("send this file back", text, fixed = TRUE))
  testthat::expect_false(grepl("JaneDoe", text, ignore.case = TRUE))
  testthat::expect_match(text, "Environment report, version", fixed = TRUE)
  if (!confirmed) {
    testthat::expect_match(text, "Project folder confirmed: +no")
    testthat::expect_match(text, "not checked (project folder not confirmed)", fixed = TRUE)
    testthat::expect_match(text, "complete project folder", fixed = TRUE)
    testthat::expect_match(text, "PROJECT_VERSION", fixed = TRUE)
  }
}

test_that("the output folder is created inside the confirmed project folder", {
  fake <- make_fake(with_output = FALSE)
  before <- all_files(fake$root)

  res <- run_r(fake, fake$script)
  expect_null(attr(res, "status"))
  expect_true(dir.exists(file.path(fake$project, "output")))
  expect_setequal(setdiff(all_files(fake$root), before),
                  c("project/output", "project/output/environment_report.txt"))
  expect_setequal(setdiff(before, all_files(fake$root)), character())

  text <- paste(res, collapse = "\n")
  expect_match(text, "Output folder created: +yes")
  expect_match(text, "Can write to output folder: +yes")
  expect_match(text, "Report saved to:", fixed = TRUE)
})

test_that("nothing is written when PROJECT_VERSION is missing, wrong, or unparseable", {
  for (marker in c("missing", "wrong", "bad")) {
    for (with_output in c(FALSE, TRUE)) {
      fake <- make_fake(with_output = with_output, marker = marker)
      expect_not_saved(fake, all_files(fake$root))
      res <- run_r(fake, fake$script)
      expect_match(paste(res, collapse = "\n"), "Project folder confirmed: +no")
      expect_match(paste(res, collapse = "\n"), "Project version: +unknown")
    }
  }
})

test_that("nothing is created when the script folder is not named scripts", {
  for (with_output in c(FALSE, TRUE)) {
    fake <- make_fake(with_output = with_output, script_dir = "tools")
    expect_not_saved(fake, all_files(fake$root))
  }
})

test_that("a script alone in Downloads is not saved next to an unrelated output folder", {
  root <- normalizePath(tempfile("fake"), winslash = "/", mustWork = FALSE)
  home <- file.path(root, "home", "JaneDoe")
  downloads <- file.path(home, "Downloads")
  dir.create(downloads, recursive = TRUE)
  dir.create(file.path(home, "output"))
  file.copy(script_src, file.path(downloads, "environment_report.R"))
  fake <- list(root = root, home = home, script = file.path(downloads, "environment_report.R"))
  before <- all_files(root)

  res <- run_r(fake, fake$script)
  expect_null(attr(res, "status"))
  expect_identical(all_files(root), before)
  text <- paste(res, collapse = "\n")
  expect_match(text, "could not be saved", fixed = TRUE)
  expect_false(grepl("Report saved to", text, fixed = TRUE))
  expect_match(text, "Project folder confirmed: +no")
})

test_that("nothing is written when the script path is unknown", {
  fake <- make_fake()
  workdir <- file.path(fake$root, "work")
  dir.create(file.path(workdir, "output"), recursive = TRUE)
  env <- c(paste0("HOME=", fake$home), "USER=JaneDoe", "USERNAME=JaneDoe", "LOGNAME=JaneDoe")
  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)
  driver <- file.path(fake$root, "driver.R")
  writeLines(sprintf("source(textConnection(readLines(\"%s\")))", fake$script), driver)
  before <- all_files(fake$root)
  expr <- shQuote(sprintf("eval(parse(file = \"%s\"))", fake$script))
  res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", expr),
                                  stdout = TRUE, stderr = TRUE, env = env))
  expect_null(attr(res, "status"))
  expect_identical(all_files(fake$root), before)
  text <- paste(res, collapse = "\n")
  expect_match(text, "Project folder confirmed: +no")
  expect_false(grepl("Report saved to", text, fixed = TRUE))

  res2 <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), driver,
                                   stdout = TRUE, stderr = TRUE, env = env))
  expect_null(attr(res2, "status"))
  expect_identical(all_files(fake$root), before)
  expect_match(paste(res2, collapse = "\n"), "Project folder confirmed: +no")
})

test_that("a failed folder creation gives the not-saved message, not an error", {
  fake <- make_fake(with_output = FALSE)
  Sys.chmod(fake$project, "555")
  on.exit(Sys.chmod(fake$project, "755"), add = TRUE)
  skip_if(dir.create(file.path(fake$project, "probe"), showWarnings = FALSE),
          "running as a user that ignores folder permissions")
  expect_not_saved(fake, all_files(fake$root), confirmed = TRUE)
})

test_that("sourcing the script leaves the global environment unchanged", {
  fake <- make_fake()
  driver <- file.path(fake$root, "driver.R")
  writeLines(c(
    "before <- ls(globalenv(), all.names = TRUE)",
    sprintf("source(\"%s\", local = FALSE)", fake$script),
    "after <- ls(globalenv(), all.names = TRUE)",
    "cat('NEW_OBJECTS:', setdiff(after, c(before, 'before')), '\\n')"
  ), driver)
  res <- run_r(fake, driver)
  expect_null(attr(res, "status"))
  expect_true(file.exists(file.path(fake$project, "output", "environment_report.txt")))
  expect_true(any(grepl("^NEW_OBJECTS: *$", res)))
})
