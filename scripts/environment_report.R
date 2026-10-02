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
#     OneDrive, RSTUDIO, DISPLAY, WAYLAND_DISPLAY. They are used only to find
#     standard folders and to mask the username in the report.
#
# WHAT THIS SCRIPT WRITES
#   - Small test files ("test" text only) in the temporary folder, the home
#     folder, and the Desktop, to check that they are writable. Each test file
#     is deleted immediately after the check.
#   - One report file, environment_report.txt, in the first writable folder of:
#     Desktop, home folder, temporary folder. An existing file with that name
#     in that folder is replaced.
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
#   - The script creates helper objects (for example "out", "ip", "key") in
#     the R workspace. Restart R afterwards if you want a clean session.
#
# PRIVACY
#   The username and home folder path are replaced with <user> and <home>
#   before the report is saved. The report is also printed to the console,
#   so you can read it in full before sending it.
#
# HOW TO RUN
#   Open this file in RStudio and click "Source",
#   or in the R console run: source(file.choose()) and select this file.
#
# REQUIREMENTS
#   Base R only. No additional packages are needed.
# =============================================================================

report_version <- "1.0"

# ---- Helper functions --------------------------------------------------------

# Report lines are collected in memory and written to disk once at the end.
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

# ---- Prepare masking of username and home folder ------------------------------

# Collect every spelling of the home folder path (both slash styles).
# Longest paths first, so a parent folder does not partially mask a child path.
homes <- c(Sys.getenv("USERPROFILE"), Sys.getenv("HOME"), path.expand("~"))
homes <- homes[nzchar(homes)]
homes <- unique(c(homes, normalizePath(homes, winslash = "/", mustWork = FALSE),
                  normalizePath(homes, winslash = "\\", mustWork = FALSE)))
homes <- homes[order(nchar(homes), decreasing = TRUE)]

# Collect the login name as reported by R and by the operating system.
users <- unique(c(Sys.info()[["user"]], Sys.info()[["login"]],
                  Sys.getenv("USERNAME"), Sys.getenv("USER")))
users <- users[nzchar(users) & users != "unknown"]

# Replace home folder paths with <home> and usernames with <user>.
mask <- function(x) {
  for (h in homes) x <- gsub(h, "<home>", x, fixed = TRUE)
  # Usernames are masked only between path separators, to avoid altering other text.
  for (u in users) for (s in c("/", "\\")) {
    x <- gsub(paste0(s, u, s), paste0(s, "<user>", s), x, fixed = TRUE)
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

# ---- Report header -----------------------------------------------------------

add(paste("Environment report, version", report_version), paste("Date:", Sys.Date()))

# ---- Operating system --------------------------------------------------------

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

# ---- R installation ----------------------------------------------------------

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

# ---- Graphics and interface support ------------------------------------------

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

# ---- Key packages ------------------------------------------------------------

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

# ---- Locale and file handling ------------------------------------------------

# Language, decimal mark, and time zone affect how data files and dates are
# read. Writable folders determine where the tool can save its output.
section("Locale and file handling")
kv("Locale", Sys.getlocale())
kv("UTF-8 locale", l10n_info()[["UTF-8"]])
kv("Decimal mark", Sys.localeconv()[["decimal_point"]])
kv("Time zone", safe(Sys.timezone()))
kv("Can write to temp folder", can_write(tempdir()))
kv("Can write to home folder", can_write(path.expand("~")))

# Find the Desktop folder. It can be in the user profile, in OneDrive, or in
# the home folder, depending on the system. The first one that exists is used.
desktops <- c(file.path(Sys.getenv("USERPROFILE"), "Desktop"),
              file.path(Sys.getenv("OneDrive"), "Desktop"),
              file.path(Sys.getenv("HOME"), "Desktop"))
desktops <- unique(desktops[nzchar(dirname(desktops)) & dirname(desktops) != "."])
desktop <- desktops[dir.exists(desktops)][1]
kv("Can write to Desktop", if (is.na(desktop)) "folder not found" else can_write(desktop))

# ---- All installed packages --------------------------------------------------

# Full list of installed packages, sorted by name, to help diagnose version
# conflicts.
section("All installed packages (name, version, priority)")
add(sprintf("%s %s %s", ip[, "Package"], ip[, "Version"],
            ifelse(is.na(ip[, "Priority"]), "", ip[, "Priority"]))[order(tolower(ip[, "Package"]))])

# ---- Save and display the report ---------------------------------------------

# Remove the username and home folder path from every line.
out <- mask(out)

# Save the report to the first writable folder: Desktop, then home, then temp.
targets <- c(if (!is.na(desktop)) desktop, path.expand("~"), tempdir())
report_path <- NA
for (d in targets) {
  if (can_write(d) == "yes") {
    report_path <- file.path(d, "environment_report.txt")
    writeLines(out, report_path)
    break
  }
}

# Print the report to the console so it can be read before sending.
cat(out, sep = "\n")
cat("\n\nReport saved to:", report_path, "\n")
cat("Please send this file back. It contains no patient data.\n")
