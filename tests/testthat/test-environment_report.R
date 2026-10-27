script_src <- normalizePath(
  testthat::test_path("..", "..", "scripts", "environment_report.R"),
  mustWork = TRUE
)

# Build a fake project (scripts/, PROJECT_VERSION, and optionally output/) and a fake
# home folder inside a fresh folder under the real project's output/test-tmp. marker is
# "valid", "missing", "wrong" (another project name) or "bad" (not parseable). Returns the paths.
make_fake <- function(with_output = TRUE, marker = "valid", script_dir = "scripts",
                      env = parent.frame()) {
  root <- local_output_dir(env) # nolint: object_usage_linter.
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
  root <- local_output_dir()
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

# ---- Unit tests of helper functions inside the script ----------------------------------------

# Evaluate one helper defined directly inside the script's local({ ... }) block, so it can be
# tested without running the whole report. `...` are objects the helper reads from the block.
script_function <- function(name, ...) {
  block <- as.list(parse(script_src)[[1]][[2]])[-1]
  env <- list2env(list(...), parent = baseenv())
  defs <- Filter(function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]])
  }, block)
  get_def <- function(n) {
    for (e in defs) if (identical(as.character(e[[2]]), n)) return(eval(e[[3]], env))
    stop("helper not found: ", n)
  }
  env$as_utf8 <- get_def("as_utf8")
  get_def(name)
}

running_mm <- function() {
  paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = ".")
}

lock_text <- c(
  "{",
  '  "R": {',
  '    "Version": "4.5.2",',
  '    "Repositories": [ { "Name": "CRAN", "URL": "https://example.org" } ]',
  "  },",
  '  "Packages": {',
  '    "aaa": {',
  '      "Package": "aaa",',
  '      "Version": "1.0.0",',
  '      "Description": "Says \\"Package\\": \\"fake\\", \\"Version\\": \\"9.9\\" in text.",',
  '      "Requirements": ["bbb"]',
  "    },",
  '    "bbb": { "Package": "bbb", "Version": "2.1-3", "Source": "Repository" },',
  '    "ccc": {',
  '      "Source": "GitHub",',
  '      "Package"   :   "ccc"',
  "    },",
  '    "ddd": {"Package":"ddd","Version":"0.0.1"}',
  "  }",
  "}"
)

test_that("read_lockfile reads package versions and the R version with base R only", {
  read_lockfile <- script_function("read_lockfile")
  dir <- local_output_dir()
  writeLines(lock_text, file.path(dir, "renv.lock"))
  lock <- read_lockfile(dir)
  expect_identical(lock$r_version, "4.5.2")
  expect_identical(lock$packages$package, c("aaa", "bbb", "ccc", "ddd"))
  expect_identical(lock$packages$version, c("1.0.0", "2.1-3", NA, "0.0.1"))
})

test_that("read_lockfile handles minified files and returns NULL for unusable input", {
  read_lockfile <- script_function("read_lockfile")
  dir <- local_output_dir()
  writeLines(paste(trimws(lock_text), collapse = ""), file.path(dir, "renv.lock"))
  lock <- read_lockfile(dir)
  expect_identical(lock$packages$package, c("aaa", "bbb", "ccc", "ddd"))
  expect_identical(lock$r_version, "4.5.2")

  expect_null(read_lockfile(file.path(dir, "missing")))
  expect_null(read_lockfile(NA_character_))
  writeLines("not json at all", file.path(dir, "renv.lock"))
  expect_null(read_lockfile(dir))
  writeLines('{ "Packages": {} }', file.path(dir, "renv.lock"))
  expect_null(read_lockfile(dir))
})

test_that("read_lockfile agrees with the real renv.lock of the project", {
  skip_if_not_installed("jsonlite")
  read_lockfile <- script_function("read_lockfile")
  root <- normalizePath(testthat::test_path("..", ".."))
  skip_if_not(file.exists(file.path(root, "renv.lock")))
  parsed <- jsonlite::fromJSON(file.path(root, "renv.lock"), simplifyVector = FALSE)
  lock <- read_lockfile(root)
  expect_identical(lock$r_version, parsed$R$Version)
  expect_identical(lock$packages$package,
                   unname(vapply(parsed$Packages, `[[`, "", "Package")))
  expect_identical(lock$packages$version,
                   unname(vapply(parsed$Packages, `[[`, "", "Version")))
})

test_that("find_project_library finds the renv layout of recent and older renv versions", {
  find_lib <- script_function("find_project_library", r_major_minor = running_mm())
  expect_true(is.na(find_lib(NA_character_)))

  dir <- local_output_dir()
  expect_true(is.na(find_lib(dir)))

  other <- file.path(dir, "renv", "library", "linux-x", "R-3.0", R.version$platform)
  dir.create(other, recursive = TRUE)
  expect_true(is.na(find_lib(dir)))

  recent <- file.path(dir, "renv", "library", "linux-x", paste0("R-", running_mm()),
                      R.version$platform)
  dir.create(recent, recursive = TRUE)
  expect_identical(find_lib(dir), recent)

  dir2 <- local_output_dir()
  older <- file.path(dir2, "renv", "library", paste0("R-", running_mm()), R.version$platform)
  dir.create(older, recursive = TRUE)
  expect_identical(find_lib(dir2), older)
})

test_that("as_utf8 marks unmarked UTF-8 bytes and leaves ASCII and marked text alone", {
  as_utf8 <- script_function("as_utf8")
  bytes <- rawToChar(as.raw(c(0x42, 0x6f, 0x6b, 0x6d, 0xc3, 0xa5, 0x6c)))
  expect_identical(Encoding(bytes), "unknown")
  res <- as_utf8(bytes)
  expect_identical(Encoding(res), "UTF-8")
  expect_identical(res, "Bokmål")
  expect_identical(as_utf8(c("plain", NA)), c("plain", NA))
  expect_identical(as_utf8("Bokmål"), "Bokmål")
})

test_that("repair_mojibake restores UTF-8 text that was read as a single-byte encoding", {
  repair <- script_function("repair_mojibake")
  expect_identical(repair("Norwegian BokmÃ¥l_Norway.utf8"),
                   "Norwegian Bokmål_Norway.utf8")
  expect_identical(repair("Norwegian Bokmål_Norway.utf8"),
                   "Norwegian Bokmål_Norway.utf8")
  expect_identical(repair("en_US.utf8"), "en_US.utf8")
})

# ---- Report content --------------------------------------------------------------------------

platform_lib <- function(project) {
  lib <- file.path(project, "renv", "library", "testos", paste0("R-", running_mm()),
                   R.version$platform)
  dir.create(lib, recursive = TRUE)
  lib
}

fake_package <- function(lib, name, version) {
  dir.create(file.path(lib, name, "Meta"), recursive = TRUE)
  desc <- c(Package = name, Version = version, Title = "Fake",
            Built = "R 4.5.0; ; 2025-01-01 00:00:00 UTC; unix")
  writeLines(paste0(names(desc), ": ", desc), file.path(lib, name, "DESCRIPTION"))
  saveRDS(list(DESCRIPTION = desc, Built = list(R = package_version("4.5.0"))),
          file.path(lib, name, "Meta", "package.rds"))
}

# Expect a "key: value" line of the report with exactly this value.
expect_line <- function(text, key, value) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  hit <- lines[startsWith(lines, paste0(key, ":"))]
  values <- trimws(substring(hit, nchar(key) + 2))
  testthat::expect_true(value %in% values, info = paste(key, "->", paste(values, collapse = "; ")))
}

report_text <- function(fake) {
  run_r(fake, fake$script)
  report <- file.path(fake$project, "output", "environment_report.txt")
  lines <- readLines(report, encoding = "UTF-8")
  paste(lines, collapse = "\n")
}

test_that("the report lists the project packages and not the old key packages", {
  fake <- make_fake()
  text <- report_text(fake)
  expect_match(text, "Environment report, version 1.1", fixed = TRUE)
  for (p in c("readxl (runtime)", "openxlsx (runtime)", "ggplot2 (runtime)",
              "htmltools (runtime)", "base64enc (runtime)", "withr (runtime)",
              "writexl (development)", "testthat (development)", "lintr (development)")) {
    expect_match(text, p, fixed = TRUE)
  }
  for (p in c("shiny:", "jsonlite:", "data.table:", "rstudioapi:")) {
    expect_false(grepl(p, text, fixed = TRUE))
  }
  expect_match(text, "== renv and Rtools ==", fixed = TRUE)
  expect_match(text, "renv: +")
  expect_match(text, "Rtools: +not applicable")
})

test_that("a missing project library gives a clear line and skips the lockfile check", {
  fake <- make_fake()
  writeLines(lock_text, file.path(fake$project, "renv.lock"))
  text <- report_text(fake)
  expect_line(text, "Project library", "not found (run renv::restore())")
  expect_line(text, "renv.lock packages", "not checked (project library not found)")
})

test_that("the lockfile check counts installed, missing and different packages", {
  fake <- make_fake()
  writeLines(lock_text, file.path(fake$project, "renv.lock"))
  lib <- platform_lib(fake$project)
  fake_package(lib, "aaa", "1.0.0")
  fake_package(lib, "bbb", "2.0")
  fake_package(lib, "ddd", "0.0.1")
  text <- report_text(fake)
  # The report masks the user name, so only the part below the project folder is compared.
  shown <- sub("^.*/project/", "", lib)
  library_line <- grep("^Project library:", strsplit(text, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_true(endsWith(library_line, shown))
  expect_line(text, "renv.lock packages", "2 of 4 installed at the locked version")
  expect_line(text, "renv.lock missing", "ccc")
  expect_line(text, "renv.lock other version", "bbb (installed 2.0, locked 2.1-3)")
  expect_match(text, "aaa 1.0.0 (project library)", fixed = TRUE)
})

test_that("the running R version is compared with the version in renv.lock", {
  fake <- make_fake()
  writeLines(sub('"4.5.2"', '"3.6.3"', lock_text, fixed = TRUE),
             file.path(fake$project, "renv.lock"))
  expect_match(report_text(fake),
               "R version check: +MISMATCH \\(running [0-9.]+, expected 3.6 from renv.lock\\)")

  writeLines(sub('"4.5.2"', sprintf('"%s.9"', running_mm()), lock_text, fixed = TRUE),
             file.path(fake$project, "renv.lock"))
  expect_match(report_text(fake),
               sprintf("R version check: +match \\(running %s, expected %s from renv.lock\\)",
                       running_mm(), running_mm()))
})

test_that("an unconfirmed project is checked against the built-in R version", {
  fake <- make_fake(marker = "wrong")
  text <- paste(run_r(fake, fake$script), collapse = "\n")
  expect_match(text, "expected 4.5 from built-in default", fixed = TRUE)
  expect_line(text, "Project library", "not checked (project folder not confirmed)")
  expect_line(text, "renv.lock packages", "not checked (project folder not confirmed)")
})

# ---- Text encoding ---------------------------------------------------------------------------

has_bytes <- function(bytes, needle) {
  n <- length(needle)
  any(vapply(seq_len(length(bytes) - n + 1), function(i) {
    identical(bytes[i:(i + n - 1)], needle)
  }, logical(1)))
}

# A library folder name with a-ring ends up in the "Library paths" line. Under LC_ALL=C R treats
# the folder name as unmarked native text, the same situation as the Windows locale name:
# non-ASCII bytes with no encoding mark. The saved file must hold the single UTF-8 sequence for
# the letter (C3 A5), not a re-encoded one (C3 83 C2 A5).
test_that("non-ASCII text is saved as UTF-8 in a UTF-8 and in a non-UTF-8 session", {
  skip_if_not(l10n_info()[["UTF-8"]], "test session is not UTF-8")
  for (locale_env in list(character(), "LC_ALL=C")) {
    fake <- make_fake()
    lib <- file.path(fake$root, "other", "bokmål", "Rlib")
    dir.create(lib, recursive = TRUE)
    res <- run_r(fake, fake$script, c(paste0("R_LIBS_USER=", lib), locale_env))
    expect_null(attr(res, "status"))
    report_file <- file.path(fake$project, "output", "environment_report.txt")
    expect_true(file.exists(report_file))

    bytes <- readBin(report_file, "raw", file.size(report_file))
    expect_true(has_bytes(bytes, c(charToRaw("bokm"), as.raw(c(0xc3, 0xa5)), charToRaw("l"))))
    expect_false(has_bytes(bytes, c(charToRaw("bokm"), as.raw(c(0xc3, 0x83, 0xc2, 0xa5)))))
    expect_true(validUTF8(rawToChar(bytes)))
  }
})
