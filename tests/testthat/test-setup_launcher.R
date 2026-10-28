launcher_source <- normalizePath(testthat::test_path("..", "..", "setup.bat"), winslash = "/")

# Runs a copy of setup.bat in `dir` without pausing. Returns the exit status.
run_launcher <- function(bat, args = character(), env = parent.frame()) {
  withr::local_envvar(BURN_VP_NO_PAUSE = "1", .local_envir = env)
  suppressWarnings(system2("cmd", c("/c", shQuote(normalizePath(bat), "cmd"), args),
                           stdout = TRUE, stderr = TRUE))
}

launcher_status <- function(out) {
  status <- attr(out, "status")
  if (is.null(status)) 0L else status
}

test_that("the launcher stops and writes nothing when PROJECT_VERSION is missing", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  bat <- file.path(dir, "setup.bat")
  file.copy(launcher_source, bat)
  out <- run_launcher(bat)
  expect_gt(launcher_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
  expect_true(any(grepl("unzipped project folder", out, fixed = TRUE)))
})

test_that("the launcher stops and writes nothing when PROJECT_VERSION names another project", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  bat <- file.path(dir, "setup.bat")
  file.copy(launcher_source, bat)
  writeLines(c("Project: other_project", "Version: 1.0.0"), file.path(dir, "PROJECT_VERSION"))
  out <- run_launcher(bat)
  expect_gt(launcher_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
})

test_that("the launcher stops when the project name only starts with burn_vp", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  bat <- file.path(dir, "setup.bat")
  file.copy(launcher_source, bat)
  writeLines(c("Project: burn_vp_other", "Version: 1.0.0"), file.path(dir, "PROJECT_VERSION"))
  out <- run_launcher(bat)
  expect_gt(launcher_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
})

test_that("--find-r returns an existing Rscript.exe of R 4.5", {
  skip_on_os(c("mac", "linux", "solaris"))
  out <- run_launcher(launcher_source, "--find-r")
  expect_equal(launcher_status(out), 0)
  rscript <- trimws(out[length(out)])
  expect_true(file.exists(rscript))
  expect_equal(tolower(basename(rscript)), "rscript.exe")
  version <- system2(rscript, "--version", stdout = TRUE, stderr = TRUE)
  expect_match(paste(version, collapse = " "), "version 4\\.5\\.")
})

test_that("--find-r writes nothing in the project folder", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  bat <- file.path(dir, "setup.bat")
  file.copy(launcher_source, bat)
  writeLines(c("Project: burn_vp", "Version: 1.0.0"), file.path(dir, "PROJECT_VERSION"))
  run_launcher(bat, "--find-r")
  expect_false(dir.exists(file.path(dir, "output")))
})

# A fake project folder with a copy of setup.bat and a stub in place of the real setup script.
# The stub records where R's temporary folder is and stops with `status`.
make_stub_project <- function(status = 0, env = parent.frame()) {
  dir <- local_output_dir(env) # nolint: object_usage_linter.
  testthat::skip_if(grepl(" ", dir, fixed = TRUE), "the test folder path has a space")
  dir.create(file.path(dir, "scripts"))
  file.copy(launcher_source, file.path(dir, "setup.bat"))
  writeLines(c("Project: burn_vp", "Version: 1.0.0"), file.path(dir, "PROJECT_VERSION"))
  writeLines(c(
    'root <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(),',
    '  value = TRUE)), winslash = "/")))',
    'dir.create(file.path(root, "output", "setup"), recursive = TRUE)',
    'writeLines(normalizePath(tempdir(), winslash = "/"),',
    '  file.path(root, "output", "setup", "tmp.txt"))',
    sprintf("quit(status = %d)", status)
  ), file.path(dir, "scripts", "setup_environment.R"))
  dir
}

test_that("the launcher removes output/tmp after R exits and keeps what R wrote", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_stub_project()
  out <- run_launcher(file.path(dir, "setup.bat"))
  expect_equal(launcher_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output", "tmp")))
  expect_true(file.exists(file.path(dir, "output", "setup", "tmp.txt")))
  used <- readLines(file.path(dir, "output", "setup", "tmp.txt"))
  expect_true(startsWith(used, paste0(normalizePath(dir, winslash = "/"), "/output/tmp/")))
})

test_that("the launcher removes output/tmp when setup fails and passes the exit status on", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_stub_project(status = 1)
  out <- run_launcher(file.path(dir, "setup.bat"))
  expect_equal(launcher_status(out), 1)
  expect_false(dir.exists(file.path(dir, "output", "tmp")))
  expect_true(file.exists(file.path(dir, "output", "setup", "tmp.txt")))
})

test_that("the launcher does not remove an output/tmp folder that existed before", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_stub_project()
  dir.create(file.path(dir, "output", "tmp"), recursive = TRUE)
  writeLines("x", file.path(dir, "output", "tmp", "mine.txt"))
  run_launcher(file.path(dir, "setup.bat"))
  expect_true(file.exists(file.path(dir, "output", "tmp", "mine.txt")))
})

test_that("the launcher does not tell the user to install R", {
  text <- paste(readLines(launcher_source, warn = FALSE), collapse = "\n")
  expect_no_match(text, "install R", ignore.case = TRUE)
  expect_match(text, "Please contact the project team", fixed = TRUE)
})

test_that("git keeps CRLF line endings for setup.bat", {
  skip_on_os(c("mac", "linux", "solaris"))
  skip_if(Sys.which("git") == "", "git is not available")
  root <- dirname(launcher_source)
  skip_if(!dir.exists(file.path(root, ".git")) && !file.exists(file.path(root, ".git")),
          "not a git checkout")
  out <- suppressWarnings(system2("git", c("-C", shQuote(root), "ls-files", "--eol", "setup.bat"),
                                  stdout = TRUE, stderr = TRUE))
  skip_if(length(out) == 0 || !is.null(attr(out, "status")), "setup.bat is not tracked by git")
  expect_match(out[1], "attr/text eol=crlf", fixed = TRUE)
})
