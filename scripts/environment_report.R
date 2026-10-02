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
#   - Names and versions of installed R packages.
#   - Whether graphics and dialog support (Tcl/Tk, XQuartz, display) is present.
#   - Language, decimal mark, and time zone settings.
#   - These environment variables: USERPROFILE, HOME, USERNAME, USER,
#     RSTUDIO, DISPLAY, WAYLAND_DISPLAY. The first four are used only to mask
#     the home folder and username in the report.
#   - The location of this script file, to find the project's output folder.
#     If that location cannot be determined, the current working folder is
#     used instead, but only if it already contains a folder named "output".
#
# WHAT THIS SCRIPT WRITES
#   - Only inside the "output" folder of this project (the folder next to the
#     "scripts" folder that contains this file). Nothing is written to the
#     Desktop, the home folder, the temporary folder, or any other location.
#   - One small test file ("test" text only) in that output folder, to check
#     that it is writable. It is deleted immediately after the check.
#   - One report file, output/environment_report.txt. An existing file with
#     that name in the output folder is replaced.
#   - The output folder is never created. If it is missing or not writable,
#     nothing is written, and the report is only printed to the console.
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
#   - Installed packages from the "Key packages" list below, and tcltk, are
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
#   Keep this file in the "scripts" folder inside the project folder you
#   received, with the "output" folder next to it. Open it there in RStudio
#   and click "Source", or in the R console run: source(file.choose()) and
#   select this file. If the file is run from another location, the report
#   is not saved, only printed to the console.
#
# REQUIREMENTS
#   Base R only. No additional packages are needed.
# =============================================================================

# The whole script runs inside local() so that no helper objects are left in
# the R workspace.
local({
  report_version <- "1.0"

  # ---- Helper functions ------------------------------------------------------

  # Report lines are collected in memory and written to disk once at the end.
  # "<<-" updates the "out" defined just below, inside this local() block.
  out <- character()
  add <- function(...) out <<- c(out, ...)

  # Add a section heading to the report.
  section <- function(title) add("", paste("==", title, "=="))

  # Add one "key: value" line. Missing values become "unknown", line breaks are
  # flattened, and multiple values are joined with " | ".
  kv <- function(key, value) {
    if (is.null(value) || length(value) == 0) value <- "unknown"
    value <- gsub("\\s+", " ", trimws(as.character(value)))
    add(sprintf("%-30s %s", paste0(key, ":"), paste(value, collapse = " | ")))
  }

  # Run a check and record its error message instead of stopping the script.
  safe <- function(expr) tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))

  # Detect the operating system, because some checks differ between systems.
  is_windows <- .Platform$OS.type == "windows"
  is_mac <- Sys.info()[["sysname"]] == "Darwin"

  # ---- Locate the project output folder --------------------------------------

  # The report may only be saved in the "output" folder of the project, which is
  # next to the "scripts" folder that holds this file. The script location comes
  # from source() (or RStudio's Source button) or from the Rscript command line.
  # If it cannot be found, the current working folder is used, but only if it
  # already contains an "output" folder. Nothing is created here. Returns NA if
  # no folder is known.
  find_output_dir <- function() {
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
    if (!is.na(script)) {
      script_dir <- dirname(normalizePath(script, winslash = "/", mustWork = FALSE))
      return(file.path(dirname(script_dir), "output"))
    }
    wd_output <- file.path(getwd(), "output")
    if (dir.exists(wd_output)) wd_output else NA_character_
  }
  output_dir <- find_output_dir()

  # ---- Prepare masking of username and home folder ---------------------------

  # Collect every spelling of the home folder path (both slash styles, and on
  # Windows also the short 8.3 form). Longest paths first, so a parent folder
  # does not partially mask a child path.
  homes <- c(Sys.getenv("USERPROFILE"), Sys.getenv("HOME"), path.expand("~"))
  homes <- homes[nzchar(homes)]
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
  users <- users[nzchar(users) & users != "unknown"]

  # Put a backslash before characters that have a special meaning in a regular
  # expression, so that text such as a path is matched literally.
  escape_regex <- function(x) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)

  # Replace home folder paths with <home> and usernames with <user>. Matching
  # ignores upper and lower case. A username is replaced only when it is a whole
  # path segment: after "/" or "\", and followed by "/", "\", or the end of the
  # text.
  mask <- function(x) {
    for (h in homes) x <- gsub(escape_regex(h), "<home>", x, ignore.case = TRUE)
    for (u in users) {
      pattern <- paste0("(?<=[/\\\\])", escape_regex(u), "(?=[/\\\\]|$)")
      x <- gsub(pattern, "<user>", x, ignore.case = TRUE, perl = TRUE)
    }
    x
  }

  # Check whether a folder is writable by creating a small test file in it and
  # deleting it straight away. Returns "yes", "no", or "folder not found".
  can_write <- function(dir) {
    if (!dir.exists(dir)) return("folder not found")
    f <- tempfile(tmpdir = dir, fileext = ".txt")
    ok <- tryCatch({ writeLines("test", f); file.exists(f) },
                   error = function(e) FALSE, warning = function(w) FALSE)
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
  kv("Library paths", .libPaths())
  # Checks folder permissions only. Nothing is written to the library folders.
  kv("Library paths writable", file.access(.libPaths(), 2) == 0)
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
    tk_ok <- tryCatch({ tcltk::tcl("winfo", "exists", "."); "yes" },
                      error = function(e) paste("no:", conditionMessage(e)))
    paste0("Tcl ", tcl_ver, ", windows available: ", tk_ok)
  }, error = function(e) paste("no:", conditionMessage(e)))
  kv("Tcl/Tk", tk_status)

  # Windows: check that the built-in R file dialog functions are present.
  if (is_windows) {
    kv("Windows dialogs available", c(
      paste0("choose.files=", exists("choose.files", envir = asNamespace("utils"))),
      paste0("winDialog=", exists("winDialog", envir = asNamespace("utils")))))
  }
  # macOS: Tcl/Tk windows need XQuartz, so check whether its folder exists.
  if (is_mac) kv("XQuartz present", dir.exists("/opt/X11"))
  # Linux: windows need a display server, so record whether one is set.
  if (!is_windows && !is_mac) {
    kv("DISPLAY", Sys.getenv("DISPLAY", "not set"))
    kv("WAYLAND_DISPLAY", Sys.getenv("WAYLAND_DISPLAY", "not set"))
  }

  # ---- Key packages ----------------------------------------------------------

  # List installed packages. If a package is installed in more than one library,
  # keep the copy R would use (the first library in the search order).
  section("Key packages")
  ip <- installed.packages()
  ip <- ip[!duplicated(ip[, "Package"]), , drop = FALSE]

  # Packages the tool may use. For each one, report its version and whether it
  # loads. Packages that are not installed are only reported, never installed.
  key <- c("survival", "tcltk", "shiny", "jsonlite", "readxl", "openxlsx",
           "writexl", "ggplot2", "data.table", "rstudioapi")
  for (p in key) {
    status <- if (p %in% rownames(ip)) {
      loads <- suppressWarnings(requireNamespace(p, quietly = TRUE))
      paste0(ip[p, "Version"], if (loads) ", loads" else ", FAILS TO LOAD")
    } else "not installed"
    kv(p, status)
  }

  # ---- Locale and file handling ----------------------------------------------

  # Language, decimal mark, and time zone affect how data files and dates are
  # read. The tool saves its output in the project output folder, so check that
  # this folder exists and can be written to (using a temporary test file that
  # is deleted at once).
  section("Locale and file handling")
  kv("Locale", Sys.getlocale())
  kv("UTF-8 locale", l10n_info()[["UTF-8"]])
  kv("Decimal mark", Sys.localeconv()[["decimal_point"]])
  kv("Time zone", safe(Sys.timezone()))
  output_status <- if (is.na(output_dir)) "folder not found" else can_write(output_dir)
  kv("Can write to output folder", output_status)

  # ---- All installed packages ------------------------------------------------

  # Full list of installed packages, sorted by name, to help diagnose version
  # conflicts.
  section("All installed packages (name, version, priority)")
  pkg_order <- order(tolower(ip[, "Package"]))
  add(sprintf("%s %s %s", ip[, "Package"], ip[, "Version"],
              ifelse(is.na(ip[, "Priority"]), "", ip[, "Priority"]))[pkg_order])

  # ---- Save and display the report -------------------------------------------

  # Remove the username and home folder path from every line.
  out <- mask(out)

  # Save the report in the project output folder, only if it exists and is
  # writable. The folder is never created. An existing report is replaced.
  report_path <- NA_character_
  if (output_status == "yes") {
    target <- file.path(output_dir, "environment_report.txt")
    result <- suppressWarnings(try(writeLines(out, target), silent = TRUE))
    if (inherits(result, "try-error")) output_status <- "no" else report_path <- target
  }

  # Print the report to the console in all cases, so it can be read before
  # sending.
  cat(out, sep = "\n")
  if (!is.na(report_path)) {
    cat("\n\nReport saved to:", mask(report_path), "\n")
    cat("Please send this file back. It contains no patient data.\n")
  } else {
    reason <- if (output_status == "no") {
      paste("the project output folder is not writable:", mask(output_dir))
    } else if (is.na(output_dir)) {
      paste("the project output folder could not be located. Run this script",
            "from the scripts folder inside the project folder.")
    } else {
      paste("the project output folder was not found. Looked for:", mask(output_dir))
    }
    cat("\n\nThe report could not be saved because", reason, "\n")
    cat("Please copy the report text printed above in the console and send that instead.\n")
    cat("It contains no patient data.\n")
  }
})
