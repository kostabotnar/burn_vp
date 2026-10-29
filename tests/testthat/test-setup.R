project_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")

# A fake project folder (scripts/, PROJECT_VERSION and renv.lock) under output/test-tmp.
make_project <- function(marker = "Project: burn_vp", env = parent.frame()) {
  dir <- local_output_dir(env) # nolint: object_usage_linter.
  root <- file.path(dir, "proj")
  dir.create(file.path(root, "scripts"), recursive = TRUE)
  if (!is.null(marker)) writeLines(c(marker, "Version: 9.9.9"), file.path(root, "PROJECT_VERSION"))
  writeLines(c("{", '  "R": {', '    "Version": "4.5.2",', '    "Repositories": []', "  },",
               '  "Packages": {', '    "R6": {', '      "Version": "2.6.1"', "    }", "  }", "}"),
             file.path(root, "renv.lock"))
  normalizePath(root, winslash = "/")
}

# Returns a function that gives the next value each time it is called.
sequence_of <- function(...) {
  state <- new.env()
  state$values <- list(...)
  function(...) {
    value <- state$values[[1]]
    state$values <- state$values[-1]
    value
  }
}

test_that("setup_script_dir reads Rscript's --file argument", {
  root <- make_project()
  script <- file.path(root, "scripts", "setup_environment.R")
  writeLines("1", script)
  expect_equal(setup_script_dir(c("R", "--no-echo", paste0("--file=", script)), list()),
               file.path(root, "scripts"))
})

test_that("setup_script_dir restores spaces written as ~+~ in --file", {
  root <- make_project()
  dir.create(file.path(root, "my scripts"))
  script <- file.path(root, "my scripts", "setup_environment.R")
  writeLines("1", script)
  arg <- paste0("--file=", gsub(" ", "~+~", script, fixed = TRUE))
  expect_equal(setup_script_dir(arg, list()), file.path(root, "my scripts"))
})

test_that("setup_script_dir reads the ofile of a source() call when there is no --file", {
  root <- make_project()
  script <- file.path(root, "scripts", "setup_environment.R")
  writeLines("1", script)
  frames <- list(list2env(list(x = 1)), list2env(list(ofile = script)))
  expect_equal(setup_script_dir(c("Rgui", "--no-save"), frames), file.path(root, "scripts"))
})

test_that("setup_script_dir prefers the sourced script over the wrapper named by --file", {
  root <- make_project()
  script <- file.path(root, "scripts", "setup_environment.R")
  writeLines("1", script)
  frames <- list(list2env(list(ofile = script)))
  args <- paste0("--file=", file.path(project_root, "tests", "testthat.R"))
  expect_equal(setup_script_dir(args, frames), file.path(root, "scripts"))
})

test_that("setup_script_dir ignores an ofile that does not exist and returns NA", {
  frames <- list(list2env(list(ofile = file.path(project_root, "no_such_script.R"))))
  expect_true(is.na(setup_script_dir(c("Rgui", "--no-save"), frames)))
  expect_true(is.na(setup_script_dir(character(), list())))
})

test_that("setup_find_root confirms the folder above the script folder", {
  root <- make_project()
  expect_equal(setup_find_root(file.path(root, "scripts"), is_interactive = FALSE), root)
})

test_that("setup_find_root rejects an unconfirmed folder without asking when not interactive", {
  root <- make_project(marker = "Project: other")
  expect_error(setup_find_root(file.path(root, "scripts"), is_interactive = FALSE),
               "project folder was not found")
  expect_error(setup_find_root(NA_character_, is_interactive = FALSE), "project folder")
  chooser <- function(...) stop("must not be called")
  expect_error(setup_find_root(NA_character_, is_interactive = TRUE, is_windows = FALSE,
                               chooser = chooser), "project folder was not found")
})

test_that("setup_find_root asks the user when the folder is unknown on interactive Windows", {
  root <- make_project()
  expect_message(
    found <- setup_find_root(NA_character_, TRUE, TRUE, chooser = function(...) root),
    "pick it"
  )
  expect_equal(found, root)
  # The scripts folder is accepted too.
  found <- suppressMessages(setup_find_root(NA_character_, TRUE, TRUE,
                                            chooser = function(...) file.path(root, "scripts")))
  expect_equal(found, root)
})

test_that("setup_find_root asks again after a wrong folder and gives up after cancel or retries", {
  root <- make_project()
  other <- make_project(marker = "Project: other")
  found <- suppressMessages(setup_find_root(NA_character_, TRUE, TRUE,
                                            chooser = sequence_of(other, root)))
  expect_equal(found, root)
  expect_error(suppressMessages(setup_find_root(NA_character_, TRUE, TRUE,
                                                chooser = sequence_of(other, NA_character_))),
               "No folder was picked")
  always_other <- function(...) other
  expect_error(suppressMessages(setup_find_root(NA_character_, TRUE, TRUE, always_other,
                                                attempts = 2)),
               "not found")
  # An unconfirmed script folder falls back to the chooser as well.
  found <- suppressMessages(setup_find_root(file.path(other, "scripts"), TRUE, TRUE,
                                            chooser = function(...) root))
  expect_equal(found, root)
})

test_that("setup_lock_r_version reads the R version from renv.lock", {
  root <- make_project()
  expect_equal(setup_lock_r_version(root), "4.5.2")
  expect_match(setup_lock_r_version(project_root), "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  unlink(file.path(root, "renv.lock"))
  expect_true(is.na(setup_lock_r_version(root)))
  writeLines('{"Packages": {}}', file.path(root, "renv.lock"))
  expect_true(is.na(setup_lock_r_version(root)))
})

test_that("setup_check_r_version separates match, patch difference and minor difference", {
  ok <- setup_check_r_version("4.5.2", "4.5.2")
  expect_equal(ok$status, "ok")
  patch <- setup_check_r_version("4.5.2", "4.5.1")
  expect_equal(patch$status, "warning")
  expect_match(patch$message, "4.5.1")
  minor <- setup_check_r_version("4.5.2", "4.4.3")
  expect_equal(minor$status, "fail")
  expect_match(minor$message, "needs R 4.5. This computer has R 4.4.3", fixed = TRUE)
  expect_match(minor$message, "Please contact the project team", fixed = TRUE)
  expect_no_match(minor$message, "install", ignore.case = TRUE)
  expect_equal(setup_check_r_version("4.5.2", "4.6.0")$status, "fail")
  expect_equal(setup_check_r_version("4.5.2", "5.5.2")$status, "fail")
  expect_equal(setup_check_r_version(NA_character_, "4.5.2")$status, "warning")
})

test_that("setup_check_repository reports reachable and unreachable servers", {
  seen <- NULL
  reachable <- function(address, timeout) {
    seen <<- list(address = address, timeout = timeout)
    TRUE
  }
  res <- setup_check_repository("https://example.test/cran/x", "4.5.2", timeout = 7,
                                open_url = reachable)
  expect_equal(res$status, "ok")
  expect_equal(seen$address, "https://example.test/cran/x/bin/windows/contrib/4.5/PACKAGES")
  expect_equal(seen$timeout, 7)

  for (open_url in list(function(...) stop("cannot open"), function(...) warning("HTTP 403"),
                        function(...) FALSE)) {
    res <- setup_check_repository("https://example.test/cran/x", "4.5.2", open_url = open_url)
    expect_equal(res$status, "fail")
    expect_match(res$message, "hospital network or a proxy")
    expect_match(res$message, "ask IT", ignore.case = TRUE)
    expect_match(res$message, "https://example.test/cran/x", fixed = TRUE)
  }
})

test_that("setup_path_warnings flags OneDrive, special letters and long paths", {
  expect_length(setup_path_warnings("C:/burn_vp"), 0)
  expect_match(setup_path_warnings("C:/Users/x/OneDrive - Sykehus/burn_vp"), "OneDrive")
  expect_match(setup_path_warnings("C:/Users/x/onedrive/burn_vp"), "OneDrive")
  special <- setup_path_warnings("C:/Users/J\u00f8rn/Dokumenter/burn_vp")
  expect_length(special, 1)
  expect_match(special, "special letters")
  long <- setup_path_warnings(paste0("C:/", strrep("a", 100), "/burn_vp"))
  expect_length(long, 1)
  expect_match(long, "long")
  expect_length(setup_path_warnings(paste0("C:/", strrep("a", 90))), 0)
  all_three <- setup_path_warnings(paste0("C:/OneDrive/\u00e6/", strrep("a", 100)))
  expect_length(all_three, 3)
  expect_length(setup_path_warnings("C:/burn_vp-main with spaces/burn_vp-main"), 0)
})

test_that("setup_free_disk_mb reads the last line of the listing in any language", {
  english <- function(command, args) {
    expect_equal(command, "cmd")
    expect_equal(args, c("/c", "dir", "/-c", "D:\\"))
    c(" Volume in drive D is Data", "", " Directory of D:\\",
      "          2 File(s)  1234 bytes", "         10 Dir(s)  536870912000 bytes free", "")
  }
  expect_equal(setup_free_disk_mb("D:/projects/burn_vp", TRUE, english), 512000)
  norwegian <- function(...) c("   1 fil(er)  12 byte", "   3 mappe(r)  1048576 byte ledig")
  expect_equal(setup_free_disk_mb("c:/burn_vp", TRUE, norwegian), 1)
})

test_that("setup_free_disk_mb returns NA when it cannot tell", {
  never <- function(...) stop("must not be called")
  expect_true(is.na(setup_free_disk_mb("C:/burn_vp", is_windows = FALSE, run = never)))
  expect_true(is.na(setup_free_disk_mb("//server/share/burn_vp", TRUE, never)))
  expect_true(is.na(setup_free_disk_mb("C:/burn_vp", TRUE, function(...) stop("no cmd"))))
  expect_true(is.na(setup_free_disk_mb("C:/burn_vp", TRUE, function(...) character())))
  expect_true(is.na(setup_free_disk_mb("C:/burn_vp", TRUE, function(...) "no numbers here")))
})

test_that("setup_check_disk warns below the minimum and does not fail", {
  expect_equal(setup_check_disk(2000, 500)$status, "ok")
  expect_equal(setup_check_disk(500, 500)$status, "ok")
  low <- setup_check_disk(120.4, 500)
  expect_equal(low$status, "warning")
  expect_match(low$message, "120 MB")
  expect_equal(setup_check_disk(NA_real_)$status, "note")
})

fake_lock <- list(
  top = list(Version = "1.0", Depends = list("R (>= 4.1)", "mid"), Imports = list("leaf (>= 2.0)")),
  mid = list(Version = "2.0", Imports = list("leaf", "stats", "cycle")),
  leaf = list(Version = "3.0-1"),
  cycle = list(Version = "4.0", Imports = list("mid"), LinkingTo = list("linked")),
  linked = list(Version = "5.0"),
  unused = list(Version = "6.0", Imports = list("leaf"))
)

test_that("setup_dependency_closure follows Depends, Imports and LinkingTo", {
  expect_setequal(setup_dependency_closure(fake_lock, "top"),
                  c("top", "mid", "leaf", "cycle", "linked"))
  expect_setequal(setup_dependency_closure(fake_lock, "leaf"), "leaf")
  expect_setequal(setup_dependency_closure(fake_lock, c("leaf", "cycle")),
                  c("leaf", "cycle", "mid", "linked"))
  expect_equal(setup_dependency_closure(fake_lock, "notinlock"), character())
})

test_that("setup_lock_versions keeps the version text exactly as written", {
  expect_equal(setup_lock_versions(fake_lock, c("leaf", "top")), c(leaf = "3.0-1", top = "1.0"))
})

test_that("setup_installed_versions reads DESCRIPTION text and gives NA for missing packages", {
  lib <- file.path(local_output_dir(), "lib")
  dir.create(file.path(lib, "fakepkg"), recursive = TRUE)
  writeLines(c("Package: fakepkg", "Version: 0.1-6"), file.path(lib, "fakepkg", "DESCRIPTION"))
  found <- setup_installed_versions(c("fakepkg", "notinstalledpkg"), lib)
  expect_equal(found, c(fakepkg = "0.1-6", notinstalledpkg = NA_character_))
})

test_that("setup_packages_to_restore is empty only when all are at the locked version", {
  locked <- c(a = "1.0", b = "0.1-6", c = "2.0.1")
  expect_equal(setup_packages_to_restore(locked, locked), character())
  expect_equal(setup_packages_to_restore(locked, c(a = "1.0", b = "0.1-6", c = NA)), "c")
  expect_equal(setup_packages_to_restore(locked, c(c = "2.0.1", b = "0.1.6", a = "1.0")), "b")
  expect_equal(setup_packages_to_restore(locked, c(a = "0.9", b = NA, c = "2.0.1")), c("a", "b"))
  expect_equal(setup_packages_to_restore(locked[0], locked[0]), character())
})

test_that("setup_verify_packages checks that each package loads at the locked version", {
  locked <- c(a = "1.0", b = "2.0", c = "3.0", d = "4.0")
  installed <- c(a = "1.0", b = "2.1", c = NA, d = "4.0")
  loader <- function(p) p != "d"
  res <- setup_verify_packages(c("a", "b", "c", "d"), locked, installed, loader)
  expect_equal(res$package, c("a", "b", "c", "d"))
  expect_equal(res$loads, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(res$ok, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(res$installed, c("1.0", "2.1", NA, "4.0"))
  expect_true(all(setup_verify_packages("a", locked, installed, function(p) TRUE)$ok))
})

test_that("setup_can_load tells installed from missing packages", {
  expect_true(setup_can_load("stats"))
  expect_false(setup_can_load("notinstalledpkg.burnvp"))
})

test_that("setup_mask hides home folders and whole-segment user names only", {
  private <- list(homes = c("C:/Users/JaneDoe", "C:\\Users\\JaneDoe"), users = "JaneDoe")
  expect_equal(setup_mask("C:/users/janedoe/Downloads/burn_vp", private),
               "<home>/Downloads/burn_vp")
  expect_equal(setup_mask("C:\\Users\\JaneDoe\\x", private), "<home>\\x")
  expect_equal(setup_mask("D:/data/JaneDoe/file", private), "D:/data/<user>/file")
  expect_equal(setup_mask("D:/data/JaneDoeX/file", private), "D:/data/JaneDoeX/file")
  expect_equal(setup_mask("no names here", list(homes = character(), users = character())),
               "no names here")
})

test_that("setup_write_log writes only inside output/setup as masked UTF-8 bytes", {
  root <- make_project()
  before <- list.files(root, recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  private <- list(homes = "C:/Users/JaneDoe", users = "JaneDoe")
  lines <- c("Locale: Bokm\u00e5l", "Folder: C:/Users/JaneDoe/Downloads/burn_vp")
  path <- setup_write_log(root, lines, "20260101_101010", private)

  expect_equal(path, file.path(root, "output", "setup", "setup_20260101_101010.txt"))
  expect_true(file.exists(path))
  after <- list.files(root, recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  expect_setequal(setdiff(after, before),
                  c("output", "output/setup", "output/setup/setup_20260101_101010.txt"))
  text <- rawToChar(readBin(path, "raw", file.size(path)))
  utf8_word <- rawToChar(as.raw(c(0x42, 0x6f, 0x6b, 0x6d, 0xc3, 0xa5, 0x6c)))
  expect_true(grepl(utf8_word, text, fixed = TRUE, useBytes = TRUE))
  expect_match(text, "<home>/Downloads/burn_vp", fixed = TRUE)
  expect_no_match(text, "JaneDoe")
})

test_that("setup_write_log writes nothing when the folder is not confirmed", {
  dir <- local_output_dir()
  stranger <- file.path(dir, "stranger")
  dir.create(stranger)
  expect_error(setup_write_log(stranger, "text", "20260101_101010"), "not confirmed")
  expect_length(list.files(stranger, recursive = TRUE, include.dirs = TRUE, all.files = TRUE), 0)
  wrong <- make_project(marker = "Project: other")
  expect_error(setup_write_log(wrong, "text", "20260101_101010"), "not confirmed")
  expect_false(dir.exists(file.path(wrong, "output")))
  expect_error(setup_write_log(NA_character_, "text", "20260101_101010"), "not confirmed")
})

test_that("project_renv_env keeps renv's files inside output/ and turns cache and sandbox off", {
  root <- make_project()
  expect_equal(project_renv_env(root),
               c(RENV_PATHS_ROOT = file.path(root, "output", "renv"),
                 RENV_CONFIG_CACHE_ENABLED = "FALSE",
                 RENV_CONFIG_SANDBOX_ENABLED = "FALSE",
                 RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"))
})

test_that("activate_project_renv sets the renv environment variables for the project root", {
  skip_if_not(startsWith(normalizePath(.libPaths()[1], winslash = "/"),
                         paste0(project_root, "/renv/")))
  withr::local_envvar(c(RENV_PATHS_ROOT = "elsewhere", RENV_CONFIG_CACHE_ENABLED = "TRUE",
                        RENV_CONFIG_SANDBOX_ENABLED = NA,
                        RENV_CONFIG_SYNCHRONIZED_CHECK = "TRUE"))
  activate_project_renv(project_root, packages = character())
  expect_equal(Sys.getenv("RENV_PATHS_ROOT"), file.path(project_root, "output", "renv"))
  expect_equal(Sys.getenv("RENV_CONFIG_CACHE_ENABLED"), "FALSE")
  expect_equal(Sys.getenv("RENV_CONFIG_SANDBOX_ENABLED"), "FALSE")
  expect_equal(Sys.getenv("RENV_CONFIG_SYNCHRONIZED_CHECK"), "FALSE")
})

test_that("activate_project_renv does not change the environment for an unconfirmed folder", {
  withr::local_envvar(c(RENV_PATHS_ROOT = "elsewhere"))
  expect_error(activate_project_renv(dirname(project_root)), "not confirmed")
  expect_equal(Sys.getenv("RENV_PATHS_ROOT"), "elsewhere")
})

test_that("the project settings turn the renv cache off", {
  settings <- paste(readLines(file.path(project_root, "renv", "settings.json")), collapse = "")
  expect_match(settings, '"use.cache"\\s*:\\s*false')
})

# A fake project with the folders a setup run leaves behind, plus a log and an older log.
make_setup_leftovers <- function(env = parent.frame()) {
  root <- make_project(env = env)
  paths <- file.path(root, c("output/setup", "output/runs/20260101_101010_setup_check/qc",
                             "output/renv/binary", "renv/staging/1"))
  for (p in paths) dir.create(p, recursive = TRUE)
  writeLines("x", file.path(root, "output/setup/setup_check_synthetic.xlsx"))
  writeLines("x", file.path(root, "output/setup/setup_20260101_101010.txt"))
  writeLines("x", file.path(root, "output/setup/setup_20250101_101010.txt"))
  writeLines("x", file.path(root, "output/runs/20260101_101010_setup_check/report.html"))
  writeLines("x", file.path(root, "output/renv/binary/pkg.zip"))
  dir.create(file.path(root, "renv/library"), recursive = TRUE)
  writeLines("x", file.path(root, "renv/library/pkg"))
  root
}

cleanup_created <- function(root) {
  file.path(root, c("output/setup/setup_check_synthetic.xlsx",
                    "output/runs/20260101_101010_setup_check", "output/renv", "renv/staging"))
}

test_that("setup_cleanup removes the listed paths and keeps the log and the library", {
  root <- make_setup_leftovers()
  result <- setup_cleanup(root, cleanup_created(root), file.path(root, "output/setup"))
  expect_setequal(result$removed,
                  c("output/setup/setup_check_synthetic.xlsx",
                    "output/runs/20260101_101010_setup_check", "output/renv", "renv/staging",
                    "output/runs"))
  expect_length(result$warnings, 0)
  expect_false(file.exists(file.path(root, "output/setup/setup_check_synthetic.xlsx")))
  expect_false(dir.exists(file.path(root, "output/runs")))
  expect_false(dir.exists(file.path(root, "output/renv")))
  expect_false(dir.exists(file.path(root, "renv/staging")))
  expect_true(file.exists(file.path(root, "output/setup/setup_20260101_101010.txt")))
  expect_true(file.exists(file.path(root, "output/setup/setup_20250101_101010.txt")))
  expect_true(file.exists(file.path(root, "renv/library/pkg")))
  expect_true(file.exists(file.path(root, "renv.lock")))
})

test_that("setup_cleanup keeps folders that existed before the run", {
  root <- make_setup_leftovers()
  before <- file.path(root, c("output", "output/setup", "output/runs", "output/renv"))
  result <- setup_cleanup(root, cleanup_created(root), before)
  expect_setequal(result$removed,
                  c("output/setup/setup_check_synthetic.xlsx",
                    "output/runs/20260101_101010_setup_check", "renv/staging"))
  expect_true(dir.exists(file.path(root, "output/runs")))
  expect_true(file.exists(file.path(root, "output/renv/binary/pkg.zip")))
  expect_true(dir.exists(file.path(root, "output")))
})

test_that("setup_cleanup removes only empty parent folders", {
  root <- make_setup_leftovers()
  writeLines("x", file.path(root, "output/runs/20250101_101010_setup_check.txt"))
  result <- setup_cleanup(root, cleanup_created(root), file.path(root, "output/setup"))
  expect_false("output/runs" %in% result$removed)
  expect_true(file.exists(file.path(root, "output/runs/20250101_101010_setup_check.txt")))
  expect_true(dir.exists(file.path(root, "output/setup")))
})

test_that("setup_cleanup ignores paths that do not exist", {
  root <- make_setup_leftovers()
  result <- setup_cleanup(root, file.path(root, "output/not_there"))
  expect_length(result$removed, 0)
  expect_length(result$warnings, 0)
})

test_that("setup_cleanup refuses paths outside output and renv/staging", {
  root <- make_setup_leftovers()
  outside <- file.path(root, c("renv.lock", "renv/library", "renv/library/pkg", "scripts",
                               "renv", "output", "PROJECT_VERSION"))
  tricky <- c(file.path(root, "output", "..", "renv.lock"),
              file.path(root, "output/setup/../../scripts"),
              file.path(root, "output/setup/..", "..", "renv.lock"),
              "output/../renv.lock", "", NA_character_,
              file.path(dirname(root), "elsewhere"))
  dir.create(file.path(dirname(root), "elsewhere"))
  result <- setup_cleanup(root, c(outside, tricky))
  expect_length(result$removed, 0)
  expect_length(result$warnings, length(outside) + length(tricky))
  expect_true(all(file.exists(file.path(root, c("renv.lock", "PROJECT_VERSION", "scripts",
                                                "renv/library/pkg")))))
  expect_true(dir.exists(file.path(root, "output")))
  expect_true(dir.exists(file.path(dirname(root), "elsewhere")))
})

test_that("setup_cleanup resolves relative paths against the project folder", {
  root <- make_setup_leftovers()
  result <- setup_cleanup(root, c("output/renv", "renv/library"))
  expect_equal(result$removed, "output/renv")
  expect_length(result$warnings, 1)
  expect_true(file.exists(file.path(root, "renv/library/pkg")))
})

test_that("setup_cleanup reports a path it cannot remove as a warning and goes on", {
  root <- make_setup_leftovers()
  # Windows refuses to delete a file that is open.
  skip_if_not(.Platform$OS.type == "windows")
  locked <- file.path(root, "output/renv/binary/pkg.zip")
  con <- file(locked, open = "r")
  on.exit(close(con))
  result <- setup_cleanup(root, cleanup_created(root))
  expect_match(result$warnings, "Could not remove output/renv", all = FALSE)
  expect_false(file.exists(file.path(root, "output/setup/setup_check_synthetic.xlsx")))
  expect_false(dir.exists(file.path(root, "renv/staging")))
  expect_true(file.exists(locked))
})
