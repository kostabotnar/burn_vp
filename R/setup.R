# Helpers for scripts/setup_environment.R. Base R only: they run before any project package is
# installed. They read and check; they write the setup log (setup_write_log) inside output/ of a
# confirmed project folder, and setup_cleanup removes files setup created, only inside output/
# and renv/staging.
# nolint start: object_usage_linter. is_project_root() and project_output_dir() come from
# project_paths.R, which is sourced first.

# Folder that holds the running script: from the `ofile` of a source() call (Rgui File > Source
# R code, RStudio Source), else from Rscript's --file= argument, else NA. The source() call comes
# first because it names the script itself when Rscript runs a wrapper script that sources it.
setup_script_dir <- function(args = commandArgs(trailingOnly = FALSE), frames = sys.frames()) {
  script <- NA_character_
  for (frame in rev(frames)) {
    candidate <- get0("ofile", envir = frame, inherits = FALSE)
    if (is.character(candidate) && length(candidate) == 1 && !is.na(candidate) &&
          file.exists(candidate)) {
      script <- candidate
      break
    }
  }
  file_arg <- grep("^--file=", args, value = TRUE)
  if (is.na(script) && length(file_arg) > 0) {
    script <- sub("^--file=", "", file_arg[1])
    # Some R front ends write a space in the path as "~+~".
    if (!file.exists(script)) script <- gsub("~+~", " ", script, fixed = TRUE)
  }
  if (is.na(script)) return(NA_character_)
  dirname(normalizePath(script, winslash = "/", mustWork = FALSE))
}

# The confirmed project folder (normalised path). `script_dir` is the folder of the script, which
# sits in scripts/ directly under the project folder. When it is unknown or does not lead to a
# confirmed folder, an interactive Windows session asks the user to pick the project folder.
# `chooser` takes a `caption` and returns a path or NA, like utils::choose.dir().
setup_find_root <- function(script_dir, is_interactive = interactive(),
                            is_windows = .Platform$OS.type == "windows", chooser = NULL,
                            attempts = 3) {
  if (!is.na(script_dir) && is_project_root(dirname(script_dir))) {
    return(normalizePath(dirname(script_dir), winslash = "/"))
  }
  if (!(is_interactive && is_windows)) {
    stop("The project folder was not found. Run setup.bat in the project folder, or run this ",
         "file from the scripts folder of the complete project folder (the folder that contains ",
         "the file PROJECT_VERSION).", call. = FALSE)
  }
  if (is.null(chooser)) chooser <- utils::choose.dir
  message("The project folder was not found automatically. A window will ask you to pick it.")
  for (i in seq_len(attempts)) {
    picked <- chooser(caption = paste("Pick the project folder (the folder that contains the",
                                      "file PROJECT_VERSION)"))
    if (length(picked) != 1 || is.na(picked) || !nzchar(picked)) {
      stop("No folder was picked, so setup could not start. Nothing was changed.", call. = FALSE)
    }
    # A user who picks the scripts folder inside the project is accepted too.
    for (candidate in c(picked, dirname(picked))) {
      if (is_project_root(candidate)) return(normalizePath(candidate, winslash = "/"))
    }
    message("That is not the project folder. It must contain the file PROJECT_VERSION.")
  }
  stop("The project folder was not found. Nothing was changed.", call. = FALSE)
}

# R version recorded in renv.lock, read as text so no package is needed. NA when not found.
setup_lock_r_version <- function(root) {
  path <- file.path(root, "renv.lock")
  if (!file.exists(path)) return(NA_character_)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  hit <- regmatches(text, regexec('"R"\\s*:\\s*\\{\\s*"Version"\\s*:\\s*"([0-9][^"]*)"', text))[[1]]
  if (length(hit) == 2) hit[2] else NA_character_
}

# Each check returns list(status, message). status is "ok", "warning", "fail" or "note".
setup_result <- function(status, ...) list(status = status, message = paste0(...))

# Same major.minor is fine, a different patch is a warning, anything else fails. Windows binary
# packages are built per major.minor, so every patch release of that R shares them.
setup_check_r_version <- function(locked, running = getRversion()) {
  running <- as.character(running)
  if (is.na(locked)) {
    return(setup_result("warning", "R version: could not read the R version the project needs. ",
                        "You have R ", running, "."))
  }
  major_minor <- function(v) sub("^([0-9]+\\.[0-9]+).*$", "\\1", v)
  if (major_minor(locked) != major_minor(running)) {
    return(setup_result(
      "fail", "R version: this tool needs R ", major_minor(locked), ". This computer has R ",
      running, ". Setup cannot continue. Please contact the project team."
    ))
  }
  if (locked != running) {
    return(setup_result(
      "warning", "R version: you have R ", running, " and the project was built with R ", locked,
      ". This small difference should not cause problems."
    ))
  }
  setup_result("ok", "R version: R ", running, " matches the project.")
}

# Opens the address and reads a few bytes. Errors and warnings mean it failed.
setup_open_url <- function(address, timeout) {
  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)
  con <- url(address, open = "rb")
  on.exit(close(con), add = TRUE)
  length(readBin(con, "raw", n = 64)) > 0
}

# Checks that the package server answers, using the list of Windows binary packages for this
# R version. `open_url` can be replaced in tests.
setup_check_repository <- function(repository, r_version = getRversion(), timeout = 20,
                                   open_url = setup_open_url) {
  major_minor <- sub("^([0-9]+\\.[0-9]+).*$", "\\1", as.character(r_version))
  address <- paste0(repository, "/bin/windows/contrib/", major_minor, "/PACKAGES")
  reached <- tryCatch(isTRUE(open_url(address, timeout)),
                      error = function(e) FALSE, warning = function(w) FALSE)
  if (reached) {
    return(setup_result("ok", "Package server: reachable."))
  }
  setup_result(
    "fail", "Package server: could not be reached. The hospital network or a proxy may block it. ",
    "Please ask IT to allow this address: ", repository
  )
}

# Warnings (not failures) about where the project folder sits.
setup_path_warnings <- function(root, max_chars = 100) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  out <- character()
  if (grepl("onedrive", root, ignore.case = TRUE)) {
    out <- c(out, paste("The project folder is inside OneDrive. OneDrive can lock files while it",
                        "syncs and slow the installation down. Moving the folder to a local",
                        "folder such as C:/burn_vp is safer."))
  }
  if (grepl("[^\\x01-\\x7f]", root, perl = TRUE, useBytes = TRUE)) {
    out <- c(out, paste("The project folder path contains special letters (for example the",
                        "Norwegian letters with a stroke or a ring). Some tools fail on such",
                        "paths. Moving the folder to a path like C:/burn_vp is safer."))
  }
  if (nchar(root) > max_chars) {
    out <- c(out, paste0("The project folder path is long (", nchar(root), " characters). ",
                         "Windows stops working with paths over 260 characters and the package ",
                         "library adds about 100. Moving the folder closer to the top of the ",
                         "drive, for example C:/burn_vp, is safer."))
  }
  out
}

setup_run_command <- function(command, args) system2(command, args, stdout = TRUE, stderr = FALSE)

# Free space in MB on the drive of `path`, or NA when it cannot be found. Windows only. It reads
# the last line of the drive's directory listing ("... N bytes free"); the number is the last one
# on that line in every language. Nothing is written.
setup_free_disk_mb <- function(path, is_windows = .Platform$OS.type == "windows",
                               run = setup_run_command) {
  drive <- regmatches(path, regexpr("^[A-Za-z]:", path))
  if (!is_windows || length(drive) == 0) return(NA_real_)
  tryCatch({
    lines <- run("cmd", c("/c", "dir", "/-c", paste0(drive, "\\")))
    lines <- iconv(lines, from = "", to = "ASCII", sub = " ")
    lines <- lines[nzchar(trimws(lines))]
    numbers <- regmatches(lines[length(lines)], gregexpr("[0-9]+", lines[length(lines)]))[[1]]
    if (length(numbers) == 0) return(NA_real_)
    as.numeric(numbers[length(numbers)]) / 1024^2
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
}

setup_check_disk <- function(free_mb, min_mb = 500) {
  if (is.na(free_mb)) {
    return(setup_result("note", "Disk space: not checked."))
  }
  if (free_mb < min_mb) {
    return(setup_result("warning", sprintf(
      "Disk space: only %.0f MB free. About %d MB are needed. Free some space first.",
      free_mb, min_mb
    )))
  }
  setup_result("ok", sprintf("Disk space: %.0f MB free.", free_mb))
}

# `roots` and every package they need, following Depends, Imports and LinkingTo in the lockfile
# records (`lock_packages`, a named list). Base packages are not in the lockfile and are skipped.
setup_dependency_closure <- function(lock_packages, roots) {
  seen <- character()
  queue <- roots
  while (length(queue) > 0) {
    package <- queue[1]
    queue <- queue[-1]
    if (package %in% seen || !package %in% names(lock_packages)) next
    seen <- c(seen, package)
    deps <- unlist(lock_packages[[package]][c("Depends", "Imports", "LinkingTo")],
                   use.names = FALSE)
    deps <- unique(trimws(sub("\\(.*$", "", deps)))
    queue <- c(queue, setdiff(deps, c("R", seen)))
  }
  seen
}

# Named character vector of lockfile versions.
setup_lock_versions <- function(lock_packages, packages) {
  vapply(packages, function(p) as.character(lock_packages[[p]]$Version), character(1))
}

# Named character vector of installed versions in `lib` (NA when not installed).
setup_installed_versions <- function(packages, lib) {
  # The DESCRIPTION text is used as is: packageVersion() would turn "0.1-6" into "0.1.6".
  version_of <- function(p) {
    as.character(suppressWarnings(utils::packageDescription(p, lib.loc = lib, fields = "Version")))
  }
  vapply(packages, function(p) tryCatch(version_of(p), error = function(e) NA_character_),
         character(1))
}

# Packages that are missing or not at the locked version. Empty means restoring can be skipped.
setup_packages_to_restore <- function(locked, installed) {
  installed <- installed[names(locked)]
  names(locked)[is.na(installed) | installed != locked]
}

setup_can_load <- function(package) {
  suppressWarnings(suppressMessages(requireNamespace(package, quietly = TRUE)))
}

# One row per package: locked and installed version, whether it loads, and whether all is well.
setup_verify_packages <- function(packages, locked, installed, loader = setup_can_load) {
  loads <- vapply(packages, function(p) !is.na(installed[[p]]) && isTRUE(loader(p)), logical(1))
  same <- vapply(packages, function(p) {
    !is.na(installed[[p]]) && identical(unname(installed[[p]]), unname(locked[[p]]))
  }, logical(1))
  data.frame(package = packages, locked = unname(locked[packages]),
             installed = unname(installed[packages]), loads = unname(loads),
             ok = unname(loads & same), stringsAsFactors = FALSE)
}

# Return text as UTF-8 marked strings, so useBytes writes it unchanged (see environment_report.R).
setup_as_utf8 <- function(x) {
  x <- as.character(x)
  todo <- !is.na(x) & Encoding(x) == "unknown" &
    grepl("[^\\x01-\\x7f]", x, perl = TRUE, useBytes = TRUE)
  for (i in which(todo)) {
    if (validUTF8(x[i])) {
      Encoding(x[i]) <- "UTF-8"
    } else {
      x[i] <- iconv(x[i], from = "", to = "UTF-8", sub = "byte")
    }
  }
  x
}

# Every spelling of the home folder (both slash styles, and the short 8.3 form on Windows),
# longest first, and the user names as R and the system report them.
setup_private_names <- function() {
  homes <- c(Sys.getenv("USERPROFILE"), Sys.getenv("HOME"), path.expand("~"))
  homes <- setup_as_utf8(homes[nzchar(homes)])
  homes <- unique(c(homes, normalizePath(homes, winslash = "/", mustWork = FALSE),
                    normalizePath(homes, winslash = "\\", mustWork = FALSE)))
  if (.Platform$OS.type == "windows") {
    short <- vapply(homes, function(h) tryCatch(utils::shortPathName(h), error = function(e) h),
                    character(1), USE.NAMES = FALSE)
    homes <- unique(c(homes, short, gsub("\\", "/", short, fixed = TRUE)))
  }
  homes <- homes[nchar(homes) > 1]
  users <- unique(c(Sys.info()[["user"]], Sys.info()[["login"]], Sys.getenv("USERNAME"),
                    Sys.getenv("USER")))
  list(homes = homes[order(nchar(homes), decreasing = TRUE)],
       users = setup_as_utf8(users[nzchar(users) & users != "unknown"]))
}

# Replaces home folder paths with <home> and user names with <user> (whole path segments only).
setup_mask <- function(x, private = setup_private_names()) {
  escape <- function(s) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", s)
  x <- setup_as_utf8(x)
  for (h in private$homes) x <- gsub(escape(h), "<home>", x, ignore.case = TRUE)
  for (u in private$users) {
    x <- gsub(paste0("(?<=[/\\\\])", escape(u), "(?=[/\\\\]|$)"), "<user>", x,
              ignore.case = TRUE, perl = TRUE)
  }
  x
}

# Writes the log as UTF-8 bytes to <root>/output/setup/setup_<stamp>.txt after masking. Stops
# without writing when the project folder is not confirmed. Returns the file path.
setup_write_log <- function(root, lines, stamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
                            private = setup_private_names()) {
  dir <- project_output_dir(root, "setup")
  path <- file.path(dir, sprintf("setup_%s.txt", stamp))
  con <- file(path, open = "w")
  on.exit(close(con))
  writeLines(setup_mask(lines, private), con, useBytes = TRUE)
  path
}

# Removes the files and folders in `created` after a setup run, then any parent folder that is
# left empty. Only paths inside <root>/output or <root>/renv/staging are touched; others are
# refused. A folder listed in `existed_before` is never removed, and neither is a parent folder
# that is not empty. Nothing here stops setup: problems come back as warnings.
# Returns list(removed = paths relative to root, warnings = text).
setup_cleanup <- function(root, created, existed_before = character()) {
  windows <- .Platform$OS.type == "windows"
  norm <- function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    sub("/+$", "", p)
  }
  key <- function(p) if (windows) tolower(p) else p
  root <- norm(root)
  output <- key(file.path(root, "output"))
  staging <- key(file.path(root, "renv", "staging"))
  inside <- function(p) {
    k <- key(p)
    startsWith(k, paste0(output, "/")) || k == staging || startsWith(k, paste0(staging, "/"))
  }
  relative <- function(p) substring(p, nchar(root) + 2)
  kept <- key(vapply(existed_before, norm, character(1), USE.NAMES = FALSE))
  removed <- character()
  warnings <- character()
  parents <- character()

  for (path in unique(as.character(created))) {
    if (is.na(path) || !nzchar(path)) {
      warnings <- c(warnings, "Not removed, empty path.")
      next
    }
    parts <- strsplit(path, "[/\\\\]")[[1]]
    absolute <- grepl("^([A-Za-z]:|[/\\\\])", path)
    target <- norm(if (absolute) path else file.path(root, path))
    if (".." %in% parts || !inside(target)) {
      warnings <- c(warnings, paste0("Not removed, outside the allowed folders: ", path))
      next
    }
    if (key(target) %in% kept || !file.exists(target)) next
    unlink(target, recursive = TRUE, force = TRUE)
    if (file.exists(target)) {
      warnings <- c(warnings, paste0("Could not remove ", relative(target),
                                     ". It may be open in another program."))
    } else {
      removed <- c(removed, relative(target))
      parents <- c(parents, dirname(target))
    }
  }

  # Deepest folders first, so a chain of new empty folders goes in one pass.
  parents <- unique(parents)
  for (parent in parents[order(nchar(parents), decreasing = TRUE)]) {
    while (inside(parent) && !key(parent) %in% kept && dir.exists(parent) &&
             length(list.files(parent, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(parent, recursive = TRUE)
      if (dir.exists(parent)) {
        warnings <- c(warnings, paste0("Could not remove ", relative(parent), "."))
        break
      }
      removed <- c(removed, relative(parent))
      parent <- dirname(parent)
    }
  }
  list(removed = removed, warnings = warnings)
}
# nolint end
