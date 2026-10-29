gui_launcher_source <- normalizePath(testthat::test_path("..", "..", "burn_vp.bat"), winslash = "/")
gui_finder_source <- normalizePath(testthat::test_path("..", "..", "scripts", "find_r45.bat"),
                                   winslash = "/")
gui_setup_source <- normalizePath(testthat::test_path("..", "..", "setup.bat"), winslash = "/")

# Runs a batch file without pausing. Returns its output with the exit status as an attribute.
run_gui_bat <- function(bat, args = character(), env = parent.frame()) {
  withr::local_envvar(BURN_VP_NO_PAUSE = "1", .local_envir = env)
  suppressWarnings(system2("cmd", c("/c", shQuote(normalizePath(bat), "cmd"), args),
                           stdout = TRUE, stderr = TRUE))
}

gui_status <- function(out) {
  status <- attr(out, "status")
  if (is.null(status)) 0L else status
}

mark_project <- function(dir, name = "burn_vp") {
  writeLines(c(paste("Project:", name), "Version: 1.0.0"), file.path(dir, "PROJECT_VERSION"))
}

# A fake project folder with a copy of burn_vp.bat and, optionally, the shared finder and a stub
# in place of the real desktop script. The stub writes nothing and stops with `status`.
make_gui_project <- function(status = 0, finder = TRUE, gui = TRUE, env = parent.frame()) {
  dir <- local_output_dir(env) # nolint: object_usage_linter.
  testthat::skip_if(grepl(" ", dir, fixed = TRUE), "the test folder path has a space")
  dir.create(file.path(dir, "scripts"))
  file.copy(gui_launcher_source, file.path(dir, "burn_vp.bat"))
  if (finder) file.copy(gui_finder_source, file.path(dir, "scripts", "find_r45.bat"))
  mark_project(dir)
  if (gui) {
    writeLines(sprintf("quit(status = %d)", status), file.path(dir, "scripts", "run_gui.R"))
  }
  dir
}

test_that("the GUI launcher stops and writes nothing when PROJECT_VERSION is missing", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  bat <- file.path(dir, "burn_vp.bat")
  file.copy(gui_launcher_source, bat)
  out <- run_gui_bat(bat)
  expect_gt(gui_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
  expect_true(any(grepl("unzipped project folder", out, fixed = TRUE)))
})

test_that("the GUI launcher stops and writes nothing when PROJECT_VERSION names another project", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  bat <- file.path(dir, "burn_vp.bat")
  file.copy(gui_launcher_source, bat)
  mark_project(dir, "other_project")
  out <- run_gui_bat(bat)
  expect_gt(gui_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
})

test_that("the GUI launcher stops when the project name only starts with burn_vp", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  bat <- file.path(dir, "burn_vp.bat")
  file.copy(gui_launcher_source, bat)
  mark_project(dir, "burn_vp_other")
  out <- run_gui_bat(bat)
  expect_gt(gui_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
})

test_that("the GUI launcher stops and writes nothing when scripts/find_r45.bat is missing", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_gui_project(finder = FALSE)
  out <- run_gui_bat(file.path(dir, "burn_vp.bat"))
  expect_gt(gui_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
  expect_true(any(grepl("project folder is incomplete", out, fixed = TRUE)))
})

test_that("the GUI launcher --find-r prints the same Rscript.exe as setup.bat", {
  skip_on_os(c("mac", "linux", "solaris"))
  out <- run_gui_bat(gui_launcher_source, "--find-r")
  expect_equal(gui_status(out), 0)
  expect_length(out, 1)
  rscript <- trimws(out)
  expect_true(file.exists(rscript))
  expect_equal(tolower(basename(rscript)), "rscript.exe")
  setup_out <- run_gui_bat(gui_setup_source, "--find-r")
  expect_equal(rscript, trimws(setup_out[length(setup_out)]))
})

test_that("the GUI launcher --find-r writes nothing in the project folder", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_gui_project(gui = FALSE)
  run_gui_bat(file.path(dir, "burn_vp.bat"), "--find-r")
  expect_false(dir.exists(file.path(dir, "output")))
})

test_that("the GUI launcher stops and writes nothing when scripts/run_gui.R is missing", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_gui_project(gui = FALSE)
  out <- run_gui_bat(file.path(dir, "burn_vp.bat"))
  expect_gt(gui_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output")))
  expect_true(any(grepl("run_gui.R is missing", out, fixed = TRUE)))
})

test_that("the GUI launcher runs the script, then removes output/tmp and output", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_gui_project(status = 0)
  out <- run_gui_bat(file.path(dir, "burn_vp.bat"))
  expect_equal(gui_status(out), 0)
  expect_false(dir.exists(file.path(dir, "output", "tmp")))
  expect_false(dir.exists(file.path(dir, "output")))
  expect_true(any(grepl("keep this black window open", out, fixed = TRUE)))
})

test_that("the GUI launcher passes a failing exit status on and asks for a screenshot", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_gui_project(status = 3)
  out <- run_gui_bat(file.path(dir, "burn_vp.bat"))
  expect_equal(gui_status(out), 3)
  expect_false(dir.exists(file.path(dir, "output", "tmp")))
  expect_false(dir.exists(file.path(dir, "output")))
  expect_true(any(grepl("stopped with an error", out, fixed = TRUE)))
  expect_true(any(grepl("screenshot", out, fixed = TRUE)))
})

test_that("the GUI launcher does not remove an output/tmp folder that existed before", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_gui_project()
  dir.create(file.path(dir, "output", "tmp"), recursive = TRUE)
  writeLines("x", file.path(dir, "output", "tmp", "mine.txt"))
  run_gui_bat(file.path(dir, "burn_vp.bat"))
  expect_true(file.exists(file.path(dir, "output", "tmp", "mine.txt")))
})

test_that("the GUI launcher points R's temporary folder into output/tmp", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- make_gui_project()
  # Keep output/ in place (a pre-existing tmp folder is not removed), then inspect the used path.
  dir.create(file.path(dir, "output"))
  writeLines(c(
    'root <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(),',
    '  value = TRUE)), winslash = "/")))',
    'writeLines(normalizePath(tempdir(), winslash = "/"), file.path(root, "output", "tmp.txt"))'
  ), file.path(dir, "scripts", "run_gui.R"))
  out <- run_gui_bat(file.path(dir, "burn_vp.bat"))
  expect_equal(gui_status(out), 0)
  used <- readLines(file.path(dir, "output", "tmp.txt"))
  expect_true(startsWith(used, paste0(normalizePath(dir, winslash = "/"), "/output/tmp/")))
  expect_false(dir.exists(file.path(dir, "output", "tmp")))
})

test_that("the shared finder returns only RSCRIPT and prints nothing", {
  skip_on_os(c("mac", "linux", "solaris"))
  dir <- local_output_dir()
  wrapper <- file.path(dir, "wrapper.bat")
  writeLines(c(
    "@echo off",
    "setlocal EnableExtensions EnableDelayedExpansion",
    'set "KEEP=mine"',
    sprintf('call "%s"', normalizePath(gui_finder_source)),
    "echo RSCRIPT=[!RSCRIPT!]",
    "echo KEEP=[!KEEP!]",
    "for %%V in (BESTPATCH KEY LINE V P N H) do if defined %%V echo LEAK %%V"
  ), wrapper, sep = "\r\n")
  out <- run_gui_bat(wrapper)
  expect_equal(gui_status(out), 0)
  expect_length(out, 2)
  expect_equal(out[2], "KEEP=[mine]")
  rscript <- sub("^RSCRIPT=\\[(.*)\\]$", "\\1", out[1])
  expect_equal(tolower(basename(rscript)), "rscript.exe")
  expect_true(file.exists(rscript))
  expect_equal(sort(list.files(dir)), "wrapper.bat")
})

test_that("the GUI launcher does not tell the user to install R", {
  text <- paste(readLines(gui_launcher_source, warn = FALSE), collapse = "\n")
  expect_no_match(text, "install R", ignore.case = TRUE)
  expect_match(text, "Please contact the project team", fixed = TRUE)
})

test_that("git keeps CRLF line endings for the batch files", {
  skip_on_os(c("mac", "linux", "solaris"))
  skip_if(Sys.which("git") == "", "git is not available")
  root <- dirname(gui_launcher_source)
  skip_if(!dir.exists(file.path(root, ".git")) && !file.exists(file.path(root, ".git")),
          "not a git checkout")
  for (file in c("burn_vp.bat", "scripts/find_r45.bat")) {
    out <- suppressWarnings(system2("git", c("-C", shQuote(root), "ls-files", "--eol", file),
                                    stdout = TRUE, stderr = TRUE))
    skip_if(length(out) == 0 || !is.null(attr(out, "status")), paste(file, "is not tracked by git"))
    expect_match(out[1], "attr/text eol=crlf", fixed = TRUE)
  }
})
