# =============================================================================
# Environment report for the cohort comparison tool
# =============================================================================
#
# PURPOSE
#   Collects technical information about this computer's R setup, so we can
#   confirm that the cohort comparison tool will run here before we send it.
#   The result is a plain text file that you send back to us.
#
# WHAT THIS SCRIPT READS
#   - Operating system name, release, version, processor type, CPU core count.
#   - R version, R installation folder, R package library folders.
#   - Names and versions of installed R packages, and whether the packages the
#     project needs (readxl, openxlsx, ggplot2, htmltools, base64enc, withr,
#     writexl, testthat, lintr, renv) are installed and load.
#   - Whether the project's own package library (renv/library in the project
#     folder) exists, and how many packages in the file "renv.lock" it holds.
#   - Whether the running R version matches the R version in "renv.lock".
#   - Whether renv is installed and, on Windows, whether Rtools (needed to
#     build packages from source) is found.
#   - Whether graphics and dialog support (Tcl/Tk, XQuartz, display) is present.
#   - Language, decimal mark, and time zone settings.
#   - These environment variables: USERPROFILE, HOME, USERNAME, USER,
#     RSTUDIO, DISPLAY, WAYLAND_DISPLAY, RTOOLS43_HOME, RTOOLS44_HOME,
#     RTOOLS45_HOME. The first four are used only to mask the home folder and
#     username in the report.
#   - The location of this script file, to find the project folder.
#   - The file "PROJECT_VERSION" in the project folder (the folder above this
#     script's folder). It is a small text file with the project name and
#     version. Only its "Project" and "Version" fields are used.
#   - The file "renv.lock" in the project folder (when the project folder is
#     confirmed). Only the R version and the package names and versions in it
#     are used.
#   - The names of the folders inside "renv/library" in the project folder, to
#     find the project package library. Nothing in it is changed.
#
# WHAT THIS SCRIPT WRITES
#   - Only inside the "output" folder of this project (the folder next to the
#     "scripts" folder that contains this file), and only when the project
#     folder is confirmed (see below). Nothing is written to the Desktop, the
#     home folder, the temporary folder, or any other location.
#   - The project folder is confirmed only when all of these hold: this file
#     is in a folder named "scripts", the folder above it contains the file
#     "PROJECT_VERSION", and that file can be read and its "Project" field is
#     exactly the expected project name set at the top of the script.
#   - If the project folder is not confirmed, nothing at all is created or
#     written: no folder, no test file, no report. The report is only printed
#     to the console. This also applies when the location of this script
#     cannot be determined (for example when its text is pasted into R). The
#     current working folder is never used.
#   - One small test file ("test" text only) in that output folder, to check
#     that it is writable. It is deleted immediately after the check.
#   - One report file, output/environment_report.txt. An existing file with
#     that name in the output folder is replaced.
#   - If the project folder is confirmed and the "output" folder does not
#     exist, this script creates it (that one folder only, nothing above it).
#   - If the output folder cannot be created, or is not writable, nothing is
#     written, and the report is only printed to the console.
#
# WHAT THIS SCRIPT DOES NOT DO
#   - It does not install, update, or remove any software or R package.
#   - It does not connect to the internet or any network location.
#   - It does not read patient data or any document or data file.
#   - It does not change system or R settings and does not need administrator
#     rights.
#   - It does not collect the computer name, IP address, or email address.
#
# SIDE EFFECTS IN THE R SESSION
#   - Installed packages from the "Project packages" list below, and tcltk, are
#     loaded into the current R session to test that they work. Loading runs
#     the package's own startup code but installs nothing.
#
# PRIVACY
#   The username and home folder path (on Windows also the short 8.3 form of
#   the path) are replaced with <user> and <home> before the report is saved.
#   Matching ignores upper and lower case. A username is replaced only where
#   it forms a whole folder name in a path. The report is also printed to the
#   console, so you can read it in full before sending it.
#
# HOW TO RUN
#   Keep this file in the "scripts" folder inside the complete project folder
#   you received (the folder that contains the file "PROJECT_VERSION"). The
#   "output" folder is created next to it if it is missing. If the file is run
#   from another location, nothing is written and the report is only printed
#   to the console.
#
#   Step 1. Start RStudio.
#     Windows: open the Start menu, type "RStudio", and press Enter.
#     macOS:   open the Applications folder and double-click RStudio, or
#              press Cmd+Space, type "RStudio", and press Return.
#     Linux:   open RStudio from the applications menu, or type "rstudio"
#              in a terminal and press Enter.
#
#   Step 2. Open this file.
#     In RStudio choose File > Open File..., go to the project folder, open
#     the "scripts" folder, and select "environment_report.R".
#
#   Step 3. Run it.
#     Click the "Source" button at the top right of the editor panel, or
#     press Ctrl+Shift+S (Windows and Linux) or Cmd+Shift+S (macOS).
#     The report appears in the Console panel and is saved as
#     output/environment_report.txt in the project folder.
#
#   Without RStudio:
#     Windows: start R from the Start menu, choose File > Source R code...,
#              and select this file.
#     macOS:   start R from the Applications folder, choose
#              File > Source File..., and select this file.
#     Linux:   in a terminal, go to the project folder and run:
#              Rscript scripts/environment_report.R
#     In any R console you can also run: source(file.choose()) and select
#     this file.
#
# TEXT ENCODING
#   The report is saved as UTF-8 text, so letters such as a-ring or o-slash in
#   system names (for example the language setting) are kept intact.
#
# REQUIREMENTS
#   Base R only. No additional packages are needed.
# =============================================================================

# The whole script runs inside local() so that no helper objects are left in
# the R workspace.
local({
  report_version <- "1.1"

  # The project name that the "Project" field of the PROJECT_VERSION file must
  # match exactly for the project folder to be confirmed.
  expected_project <- "burn_vp"

  # The R major.minor version the project is built for. It is used when the
  # project folder is not confirmed; otherwise the version in renv.lock is used.
  fallback_r_version <- "4.5"

  # ---- Helper functions ------------------------------------------------------

  # Return text as UTF-8 marked strings. System functions (locale name, paths)
  # may return non-ASCII text with no encoding mark, and R then assumes the
  # native encoding, which can turn UTF-8 bytes into two wrong characters. Text
  # that is valid UTF-8 is marked as UTF-8; anything else is converted from the
  # native encoding.
  as_utf8 <- function(x) {
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

  # Undo text that was UTF-8 but was read as a single-byte encoding, such as
  # "Bokm" followed by A-tilde and a yen sign instead of "Bokm" and a-ring. Each
  # character is turned back into its byte and the result is kept only if it is
  # valid UTF-8. Only used for the locale name, where such text cannot be real.
  repair_mojibake <- function(x) {
    x <- as_utf8(x)
    for (i in which(!is.na(x) & grepl("[^\\x01-\\x7f]", x, perl = TRUE))) {
      for (enc in c("CP1252", "latin1")) {
        bytes <- suppressWarnings(iconv(x[i], from = "UTF-8", to = enc, toRaw = TRUE))[[1]]
        if (is.null(bytes)) next
        fixed <- rawToChar(bytes)
        Encoding(fixed) <- "UTF-8"
        if (validUTF8(fixed) && fixed != x[i]) {
          x[i] <- fixed
          break
        }
      }
    }
    x
  }

  # Report lines are collected in memory and written to disk once at the end.
  # "<<-" updates the "out" defined just below, inside this local() block.
  out <- character()
  add <- function(...) out <<- c(out, as_utf8(c(...)))

  # Add a section heading to the report.
  section <- function(title) add("", paste("==", title, "=="))

  # Add one "key: value" line. Missing values become "unknown", line breaks are
  # flattened, and multiple values are joined with " | ".
  kv <- function(key, value) {
    if (is.null(value) || length(value) == 0) value <- "unknown"
    value <- gsub("\\s+", " ", trimws(as_utf8(value)))
    add(sprintf("%-30s %s", paste0(key, ":"), paste(value, collapse = " | ")))
  }

  # Run a check and record its error message instead of stopping the script.
  safe <- function(expr) tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))

  # Detect the operating system, because some checks differ between systems.
  is_windows <- .Platform$OS.type == "windows"
  is_mac <- Sys.info()[["sysname"]] == "Darwin"

  # ---- Locate the project folder ---------------------------------------------

  # The report may only be saved in the "output" folder of the project, which is
  # next to the "scripts" folder that holds this file. The script location comes
  # from source() (or RStudio's Source button) or from the Rscript command line.
  # The project folder is confirmed only if the script is in a folder named
  # "scripts" and the parent folder has a PROJECT_VERSION file that can be read
  # and whose Project field equals expected_project. If the script location is
  # unknown, the project is not confirmed. Nothing is created or written here.
  # Returns a list with confirmed (TRUE or FALSE), the project folder and output
  # folder paths (NA if not confirmed) and the project version (NA if not
  # confirmed).
  find_project <- function() {
    none <- list(confirmed = FALSE, project_dir = NA_character_,
                 output_dir = NA_character_, version = NA_character_)
    script <- NA_character_
    for (fr in rev(sys.frames())) {
      f <- get0("ofile", envir = fr, inherits = FALSE)
      if (is.null(f)) f <- get0("fileName", envir = fr, inherits = FALSE)
      if (is.character(f) && length(f) == 1 && !is.na(f) && file.exists(f)) {
        script <- f
        break
      }
    }
    if (is.na(script)) {
      args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
      if (length(args) > 0) script <- sub("^--file=", "", args[1])
    }
    if (is.na(script)) return(none)
    script_dir <- dirname(normalizePath(script, winslash = "/", mustWork = FALSE))
    project_dir <- dirname(script_dir)
    if (basename(script_dir) != "scripts") return(none)
    marker <- file.path(project_dir, "PROJECT_VERSION")
    if (!file.exists(marker)) return(none)
    info <- tryCatch(suppressWarnings(read.dcf(marker, fields = c("Project", "Version"))),
                     error = function(e) NULL)
    if (is.null(info) || nrow(info) < 1) return(none)
    if (!isTRUE(info[1, "Project"] == expected_project)) return(none)
    list(confirmed = TRUE, project_dir = project_dir,
         output_dir = file.path(project_dir, "output"), version = info[1, "Version"])
  }
  project <- find_project()
  output_dir <- project$output_dir

  # Create the output folder (only that one folder, not its parents) if it is
  # missing and the project folder is confirmed. A failure is not an error: the
  # folder is then treated as not found and the report is only printed.
  output_created <- "no"
  if (project$confirmed && !dir.exists(output_dir)) {
    if (suppressWarnings(dir.create(output_dir, showWarnings = FALSE))) output_created <- "yes"
  }

  # ---- Project library and lockfile ------------------------------------------

  # R version as "major.minor", for example "4.5".
  r_major_minor <- paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = ".")

  # Find the project's renv package library. renv puts it in
  # renv/library/<system>/R-<major.minor>/<platform> (older renv versions omit
  # the <system> folder). It is not active when this script is run with
  # source() from Rgui, so it is located by folder name. Returns the folder
  # path, or NA if it does not exist for the running R version and platform.
  find_project_library <- function(project_dir) {
    if (is.na(project_dir)) return(NA_character_)
    base <- file.path(project_dir, "renv", "library")
    if (!dir.exists(base)) return(NA_character_)
    wanted <- paste0("R-", r_major_minor)
    search <- function(dir, depth) {
      for (sub in sort(list.dirs(dir, full.names = TRUE, recursive = FALSE))) {
        if (basename(dir) == wanted && basename(sub) == R.version$platform) return(sub)
        if (depth < 3) {
          found <- search(sub, depth + 1)
          if (!is.na(found)) return(found)
        }
      }
      NA_character_
    }
    search(base, 1)
  }

  # Read the R version and the package names and versions from renv.lock with
  # base R only. The file is JSON, but it is read as text: every "Package" entry
  # is paired with the first "Version" entry after it and before the next
  # "Package" entry. A quote inside a text value is escaped in JSON (backslash
  # before it) and is skipped. Returns NULL if the file is missing, unreadable,
  # or has no package entries; otherwise a list with r_version (NA if absent) and
  # packages (a data frame with columns package and version).
  read_lockfile <- function(project_dir) {
    if (is.na(project_dir)) return(NULL)
    path <- file.path(project_dir, "renv.lock")
    if (!file.exists(path)) return(NULL)
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(lines)) return(NULL)
    text <- paste(lines, collapse = "\n")
    pkg_start <- regexpr('(?<!\\\\)"Packages"\\s*:\\s*\\{', text, perl = TRUE)
    if (pkg_start == -1) return(NULL)

    # Values of a string entry: positions and contents.
    entries <- function(txt, key) {
      m <- gregexpr(paste0('(?<!\\\\)"', key, '"\\s*:\\s*"([^"]*)"'), txt, perl = TRUE)[[1]]
      if (m[1] == -1) return(data.frame(pos = integer(), value = character()))
      starts <- attr(m, "capture.start")[, 1]
      lens <- attr(m, "capture.length")[, 1]
      data.frame(pos = as.integer(m), value = substring(txt, starts, starts + lens - 1),
                 stringsAsFactors = FALSE)
    }

    r_version <- NA_character_
    before <- substr(text, 1, pkg_start - 1)
    r_block <- regexpr('(?<!\\\\)"R"\\s*:\\s*\\{', before, perl = TRUE)
    if (r_block != -1) {
      r_text <- substring(before, r_block)
      hit <- regmatches(r_text, regexec('"Version"\\s*:\\s*"([^"]*)"', r_text))[[1]]
      if (length(hit) == 2) r_version <- hit[2]
    }

    packages_text <- substring(text, pkg_start)
    pkgs <- entries(packages_text, "Package")
    if (nrow(pkgs) == 0) return(NULL)
    vers <- entries(packages_text, "Version")
    next_pos <- c(pkgs$pos[-1], .Machine$integer.max)
    version <- vapply(seq_len(nrow(pkgs)), function(i) {
      v <- vers$value[vers$pos > pkgs$pos[i] & vers$pos < next_pos[i]]
      if (length(v) > 0) v[1] else NA_character_
    }, character(1))
    list(r_version = r_version,
         packages = data.frame(package = pkgs$value, version = version,
                               stringsAsFactors = FALSE))
  }

  lockfile <- read_lockfile(project$project_dir)
  project_lib <- find_project_library(project$project_dir)

  # Folders searched for the packages the project needs: the project library
  # (plus R's own library, which holds the base and recommended packages) when
  # it exists, otherwise the library folders of this R session.
  search_libs <- if (is.na(project_lib)) .libPaths() else unique(c(project_lib, .Library))

  # List installed packages in the given folders. If a package is installed in
  # more than one folder, keep the copy from the first folder in the list.
  list_packages <- function(libs) {
    ip <- installed.packages(lib.loc = libs)
    ip[!duplicated(ip[, "Package"]), , drop = FALSE]
  }

  # ---- Prepare masking of username and home folder ---------------------------

  # Collect every spelling of the home folder path (both slash styles, and on
  # Windows also the short 8.3 form). Longest paths first, so a parent folder
  # does not partially mask a child path.
  homes <- c(Sys.getenv("USERPROFILE"), Sys.getenv("HOME"), path.expand("~"))
  homes <- as_utf8(homes[nzchar(homes)])
  homes <- unique(c(homes, normalizePath(homes, winslash = "/", mustWork = FALSE),
                    normalizePath(homes, winslash = "\\", mustWork = FALSE)))
  if (is_windows) {
    short <- vapply(homes, function(h) tryCatch(utils::shortPathName(h), error = function(e) h),
                    character(1), USE.NAMES = FALSE)
    homes <- unique(c(homes, short, gsub("\\", "/", short, fixed = TRUE)))
  }
  # A one-character path such as "/" would mask every separator, so skip it.
  homes <- homes[nchar(homes) > 1]
  homes <- homes[order(nchar(homes), decreasing = TRUE)]

  # Collect the login name as reported by R and by the operating system.
  users <- unique(c(Sys.info()[["user"]], Sys.info()[["login"]],
                    Sys.getenv("USERNAME"), Sys.getenv("USER")))
  users <- as_utf8(users[nzchar(users) & users != "unknown"])

  # Put a backslash before characters that have a special meaning in a regular
  # expression, so that text such as a path is matched literally.
  escape_regex <- function(x) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)

  # Replace home folder paths with <home> and usernames with <user>. Matching
  # ignores upper and lower case. A username is replaced only when it is a whole
  # path segment: after "/" or "\", and followed by "/", "\", or the end of the
  # text.
  mask <- function(x) {
    x <- as_utf8(x)
    for (h in homes) x <- gsub(escape_regex(h), "<home>", x, ignore.case = TRUE)
    for (u in users) {
      pattern <- paste0("(?<=[/\\\\])", escape_regex(u), "(?=[/\\\\]|$)")
      x <- gsub(pattern, "<user>", x, ignore.case = TRUE, perl = TRUE)
    }
    x
  }

  # Check whether a folder is writable by creating a small test file in it and
  # deleting it straight away. Only called for a confirmed project folder.
  # Returns "yes", "no", or "folder not found".
  can_write <- function(dir) {
    if (!dir.exists(dir)) return("folder not found")
    f <- tempfile(tmpdir = dir, fileext = ".txt")
    ok <- tryCatch({
      writeLines("test", f)
      file.exists(f)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (file.exists(f)) unlink(f)
    if (ok) "yes" else "no"
  }

  # ---- Report header ---------------------------------------------------------

  add(paste("Environment report, version", report_version), paste("Date:", Sys.Date()))

  # ---- Operating system ------------------------------------------------------

  # Operating system name, release, and processor type. The computer name is not
  # included.
  section("Operating system")
  si <- Sys.info()
  kv("System", si[["sysname"]])
  kv("Release", si[["release"]])
  kv("Version", si[["version"]])
  kv("Machine", si[["machine"]])
  kv("Description", safe(utils::sessionInfo()$running))
  kv("CPU cores", safe(parallel::detectCores()))

  # ---- R installation --------------------------------------------------------

  # R version, where R is installed, and where packages are installed. The tool
  # needs Rscript, and needs a writable library folder if packages must be added.
  section("R installation")
  kv("R version", R.version.string)
  kv("Platform", R.version$platform)
  kv("R home", R.home())
  rscript <- file.path(R.home("bin"), if (is_windows) "Rscript.exe" else "Rscript")
  kv("Rscript path", rscript)
  kv("Rscript exists", file.exists(rscript))
  kv("Library paths (active)", .libPaths())
  # Checks folder permissions only. Nothing is written to the library folders.
  kv("Library paths writable", file.access(.libPaths(), 2) == 0)
  # The project library is not active when this script is run with source() from
  # Rgui, so it is reported separately. Package checks below use it when it exists.
  kv("Project library", if (!project$confirmed) {
    "not checked (project folder not confirmed)"
  } else if (is.na(project_lib)) {
    "not found (run renv::restore())"
  } else {
    project_lib
  })
  kv("Package lookup folders", search_libs)
  r_from_lock <- !is.null(lockfile) && !is.na(lockfile$r_version)
  expected_r <- if (r_from_lock) lockfile$r_version else fallback_r_version
  expected_r_source <- if (r_from_lock) "renv.lock" else "built-in default"
  expected_mm <- sub("^([0-9]+\\.[0-9]+).*$", "\\1", expected_r)
  kv("R version check", if (identical(expected_mm, r_major_minor)) {
    sprintf("match (running %s, expected %s from %s)", r_major_minor, expected_mm,
            expected_r_source)
  } else {
    sprintf("MISMATCH (running %s, expected %s from %s)", r_major_minor, expected_mm,
            expected_r_source)
  })
  kv("Running in RStudio", nzchar(Sys.getenv("RSTUDIO")))
  kv("GUI", .Platform$GUI)

  # ---- Graphics and interface support ----------------------------------------

  # The tool uses file dialogs and windows, so check which graphics and dialog
  # features this R build supports.
  section("Graphics and interface support")
  caps <- capabilities()
  kv("Capabilities", paste(names(caps), caps, sep = "="))

  # Load the tcltk package (part of base R) and check whether it can show
  # windows. No window is opened.
  tk_status <- if (!caps[["tcltk"]]) "no (R built without Tcl/Tk)" else tryCatch({
    if (!suppressWarnings(requireNamespace("tcltk", quietly = TRUE))) stop("package failed to load")
    tcl_ver <- tcltk::tclvalue(tcltk::tcl("info", "patchlevel"))
    # winfo is a Tk command, so it fails when Tcl loads but windows cannot be shown.
    tk_ok <- tryCatch({
      tcltk::tcl("winfo", "exists", ".")
      "yes"
    }, error = function(e) paste("no:", conditionMessage(e)))
    paste0("Tcl ", tcl_ver, ", windows available: ", tk_ok)
  }, error = function(e) paste("no:", conditionMessage(e)))
  kv("Tcl/Tk", tk_status)

  # Windows: check that the built-in R file dialog functions are present.
  if (is_windows) {
    utils_ns <- asNamespace("utils")
    kv("Windows dialogs available", c(
      paste0("choose.files=", exists("choose.files", envir = utils_ns)),
      paste0("winDialog=", exists("winDialog", envir = utils_ns))
    ))
  }
  # macOS: Tcl/Tk windows need XQuartz, so check whether its folder exists.
  if (is_mac) kv("XQuartz present", dir.exists("/opt/X11"))
  # Linux: windows need a display server, so record whether one is set.
  if (!is_windows && !is_mac) {
    kv("DISPLAY", Sys.getenv("DISPLAY", "not set"))
    kv("WAYLAND_DISPLAY", Sys.getenv("WAYLAND_DISPLAY", "not set"))
  }

  # ---- Project packages ------------------------------------------------------

  # The packages the project needs. For each one, report its version and
  # whether it loads. They are looked up in the folders listed above as
  # "Package lookup folders". Packages that are not installed are only
  # reported, never installed.
  section("Project packages")
  ip <- list_packages(search_libs)
  needed <- c(readxl = "runtime", openxlsx = "runtime", ggplot2 = "runtime",
              htmltools = "runtime", base64enc = "runtime", withr = "runtime",
              writexl = "development", testthat = "development", lintr = "development")
  for (p in names(needed)) {
    status <- if (p %in% rownames(ip)) {
      load_error <- tryCatch({
        suppressMessages(suppressWarnings(loadNamespace(p, lib.loc = search_libs)))
        NULL
      }, error = function(e) conditionMessage(e))
      paste0(ip[p, "Version"],
             if (is.null(load_error)) ", loads" else paste0(", FAILS TO LOAD: ", load_error))
    } else {
      "not installed"
    }
    kv(paste0(p, " (", needed[[p]], ")"), status)
  }

  # Compare the packages in renv.lock with the packages found in the lookup
  # folders. Only checked when the project folder is confirmed and the project
  # library exists.
  lock_check <- function() {
    skipped <- function(why) {
      list(count = why, missing = why, different = why)
    }
    if (!project$confirmed) return(skipped("not checked (project folder not confirmed)"))
    if (is.null(lockfile)) return(skipped("not checked (renv.lock not found or not readable)"))
    if (is.na(project_lib)) return(skipped("not checked (project library not found)"))
    lp <- lockfile$packages
    installed <- ip[match(lp$package, rownames(ip)), "Version"]
    missing <- is.na(installed)
    different <- !missing & !is.na(lp$version) & installed != lp$version
    list(
      count = sprintf("%d of %d installed at the locked version",
                      sum(!missing & !different), nrow(lp)),
      missing = if (any(missing)) lp$package[missing] else "none",
      different = if (any(different)) {
        sprintf("%s (installed %s, locked %s)", lp$package[different], installed[different],
                lp$version[different])
      } else {
        "none"
      }
    )
  }
  lock <- lock_check()
  kv("renv.lock packages", lock$count)
  kv("renv.lock missing", lock$missing)
  kv("renv.lock other version", lock$different)

  # ---- renv and Rtools -------------------------------------------------------

  # renv restores the project packages. Rtools is a set of build tools that
  # Windows needs only when a package must be built from source.
  section("renv and Rtools")
  renv_ip <- list_packages(unique(c(project_lib[!is.na(project_lib)], .libPaths())))
  renv_where <- if ("renv" %in% rownames(renv_ip)) {
    if (identical(unname(renv_ip["renv", "LibPath"]), project_lib)) {
      "project library"
    } else {
      "other library"
    }
  }
  kv("renv", if (is.null(renv_where)) {
    "not installed"
  } else {
    paste0(renv_ip["renv", "Version"], " (", renv_where, ")")
  })
  if (is_windows) {
    rtools_vars <- c("RTOOLS45_HOME", "RTOOLS44_HOME", "RTOOLS43_HOME")
    rtools_values <- Sys.getenv(rtools_vars)
    kv("Rtools variables set", if (any(nzchar(rtools_values))) {
      paste0(rtools_vars[nzchar(rtools_values)], "=", rtools_values[nzchar(rtools_values)])
    } else {
      "none"
    })
    make_path <- unname(Sys.which("make"))
    kv("make program", if (nzchar(make_path)) make_path else "not found")
    own_var <- paste0("RTOOLS", sub(".", "", r_major_minor, fixed = TRUE), "_HOME")
    own_home <- Sys.getenv(own_var)
    kv("Rtools", if (nzchar(own_home) && dir.exists(own_home)) {
      paste0("found for this R (", own_var, ")")
    } else if (nzchar(make_path)) {
      paste0("a make program was found, but ", own_var, " is not set to an existing folder")
    } else {
      "not found (only needed to build packages from source)"
    })
  } else {
    kv("Rtools", "not applicable")
  }

  # ---- Locale and file handling ----------------------------------------------

  # Language, decimal mark, and time zone affect how data files and dates are
  # read. The tool saves its output in the project output folder, so, if the
  # project folder is confirmed, check that this folder exists and can be
  # written to (using a temporary test file that is deleted at once), and record
  # whether this run had to create it. If it is not confirmed, nothing is
  # checked or written.
  section("Locale and file handling")
  kv("Locale", repair_mojibake(Sys.getlocale()))
  kv("UTF-8 locale", l10n_info()[["UTF-8"]])
  kv("Decimal mark", Sys.localeconv()[["decimal_point"]])
  kv("Time zone", safe(Sys.timezone()))
  output_status <- if (project$confirmed) can_write(output_dir) else "not checked"
  kv("Project folder confirmed", if (project$confirmed) "yes" else "no")
  kv("Project version", if (project$confirmed) project$version else "unknown")
  kv("Output folder created", output_created)
  kv("Can write to output folder",
     if (project$confirmed) output_status else "not checked (project folder not confirmed)")

  # ---- All installed packages ------------------------------------------------

  # Full list of installed packages, sorted by name, to help diagnose version
  # conflicts. Packages in the project library are marked.
  section("All installed packages (name, version, priority)")
  all_ip <- list_packages(unique(c(project_lib[!is.na(project_lib)], .libPaths())))
  pkg_order <- order(tolower(all_ip[, "Package"]))
  in_project <- !is.na(project_lib) & all_ip[, "LibPath"] == project_lib
  add(trimws(sprintf("%s %s %s%s", all_ip[, "Package"], all_ip[, "Version"],
                     ifelse(is.na(all_ip[, "Priority"]), "", all_ip[, "Priority"]),
                     ifelse(in_project, " (project library)", "")))[pkg_order])

  # ---- Save and display the report -------------------------------------------

  # Remove the username and home folder path from every line.
  out <- mask(out)

  # Save the report in the project output folder, only if the project folder is
  # confirmed and the output folder exists and is writable (it was created above
  # if missing). An existing report is replaced.
  report_path <- NA_character_
  if (output_status == "yes") {
    target <- file.path(output_dir, "environment_report.txt")
    # The file is opened with an explicit UTF-8 encoding. writeLines() on a plain
    # file name would use the native encoding, which is not UTF-8 on every system.
    con <- NULL
    saved <- tryCatch({
      con <- file(target, open = "w", encoding = "UTF-8")
      writeLines(out, con)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!is.null(con)) close(con)
    if (saved) report_path <- target else output_status <- "no"
  }

  # Print the report to the console in all cases, so it can be read before
  # sending.
  cat(out, sep = "\n")
  if (!is.na(report_path)) {
    cat("\n\nReport saved to:", mask(report_path), "\n")
    cat("Please send this file back. It contains no patient data.\n")
  } else {
    reason <- if (!project$confirmed) {
      paste("the project folder could not be confirmed, so nothing was written.",
            "The script must be run from the scripts folder of the complete",
            "project folder (the folder that contains the file PROJECT_VERSION).")
    } else if (output_status == "no") {
      paste("the project output folder is not writable:", mask(output_dir))
    } else {
      paste("the project output folder was not found and could not be created:",
            mask(output_dir))
    }
    cat("\n\nThe report could not be saved because", reason, "\n")
    cat("Please copy the report text printed above in the console and send that instead.\n")
    cat("It contains no patient data.\n")
  }
})
