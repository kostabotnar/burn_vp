script_src <- normalizePath(
  testthat::test_path("..", "..", "scripts", "environment_report.R"),
  mustWork = TRUE
)

# Build a fake project (scripts/ and optionally output/) and a fake home folder
# inside a fresh temporary folder. Returns the paths.
make_fake <- function(with_output = TRUE) {
  root <- normalizePath(tempfile("fake"), winslash = "/", mustWork = FALSE)
  project <- file.path(root, "project")
  home <- file.path(root, "home", "JaneDoe")
  dir.create(file.path(project, "scripts"), recursive = TRUE)
  dir.create(home, recursive = TRUE)
  if (with_output) dir.create(file.path(project, "output"))
  file.copy(script_src, file.path(project, "scripts", "environment_report.R"))
  list(root = root, project = project, home = home,
       script = file.path(project, "scripts", "environment_report.R"))
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

test_that("nothing is written and a clear message is shown without output folder", {
  fake <- make_fake(with_output = FALSE)
  before <- all_files(fake$root)

  res <- run_r(fake, fake$script)
  expect_null(attr(res, "status"))
  expect_identical(all_files(fake$root), before)

  text <- paste(res, collapse = "\n")
  expect_match(text, "could not be saved", fixed = TRUE)
  expect_match(text, "copy the report text", fixed = TRUE)
  expect_false(grepl("Report saved to", text, fixed = TRUE))
  expect_false(grepl("send this file back", text, fixed = TRUE))
  expect_false(grepl("JaneDoe", text, ignore.case = TRUE))
  expect_match(text, "Environment report, version", fixed = TRUE)
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
