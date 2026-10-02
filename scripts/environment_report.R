# Environment report for the cohort comparison tool.
# Uses base R only. Installs nothing, reads no patient data, makes no network calls.
# Usernames and home folder paths are masked before the report is written.
#
# How to run: open this file in RStudio and click "Source",
# or in the R console run: source(file.choose()) and select this file.

report_version <- "1.0"
out <- character()
add <- function(...) out <<- c(out, ...)
section <- function(title) add("", paste("==", title, "=="))
kv <- function(key, value) {
  if (is.null(value) || length(value) == 0) value <- "unknown"
  value <- gsub("\\s+", " ", trimws(as.character(value)))
  add(sprintf("%-30s %s", paste0(key, ":"), paste(value, collapse = " | ")))
}
safe <- function(expr) tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))

is_windows <- .Platform$OS.type == "windows"
is_mac <- Sys.info()[["sysname"]] == "Darwin"

# Longest paths first, so a parent folder does not partially mask a child path.
homes <- c(Sys.getenv("USERPROFILE"), Sys.getenv("HOME"), path.expand("~"))
homes <- homes[nzchar(homes)]
homes <- unique(c(homes, normalizePath(homes, winslash = "/", mustWork = FALSE),
                  normalizePath(homes, winslash = "\\", mustWork = FALSE)))
homes <- homes[order(nchar(homes), decreasing = TRUE)]
users <- unique(c(Sys.info()[["user"]], Sys.info()[["login"]],
                  Sys.getenv("USERNAME"), Sys.getenv("USER")))
users <- users[nzchar(users) & users != "unknown"]

mask <- function(x) {
  for (h in homes) x <- gsub(h, "<home>", x, fixed = TRUE)
  # Usernames are masked only between path separators, to avoid altering other text.
  for (u in users) for (s in c("/", "\\")) {
    x <- gsub(paste0(s, u, s), paste0(s, "<user>", s), x, fixed = TRUE)
  }
  x
}

can_write <- function(dir) {
  if (!dir.exists(dir)) return("folder not found")
  f <- tempfile(tmpdir = dir, fileext = ".txt")
  ok <- tryCatch({ writeLines("test", f); file.exists(f) },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (file.exists(f)) unlink(f)
  if (ok) "yes" else "no"
}

add(paste("Environment report, version", report_version), paste("Date:", Sys.Date()))

section("Operating system")
si <- Sys.info()
kv("System", si[["sysname"]])
kv("Release", si[["release"]])
kv("Version", si[["version"]])
kv("Machine", si[["machine"]])
kv("Description", safe(utils::sessionInfo()$running))
kv("CPU cores", safe(parallel::detectCores()))

section("R installation")
kv("R version", R.version.string)
kv("Platform", R.version$platform)
kv("R home", R.home())
rscript <- file.path(R.home("bin"), if (is_windows) "Rscript.exe" else "Rscript")
kv("Rscript path", rscript)
kv("Rscript exists", file.exists(rscript))
kv("Library paths", .libPaths())
kv("Library paths writable", file.access(.libPaths(), 2) == 0)
kv("Running in RStudio", nzchar(Sys.getenv("RSTUDIO")))
kv("GUI", .Platform$GUI)

section("Graphics and interface support")
caps <- capabilities()
kv("Capabilities", paste(names(caps), caps, sep = "="))
tk_status <- if (!caps[["tcltk"]]) "no (R built without Tcl/Tk)" else tryCatch({
  if (!suppressWarnings(requireNamespace("tcltk", quietly = TRUE))) stop("package failed to load")
  tcl_ver <- tcltk::tclvalue(tcltk::tcl("info", "patchlevel"))
  # winfo is a Tk command, so it fails when Tcl loads but windows cannot be shown.
  tk_ok <- tryCatch({ tcltk::tcl("winfo", "exists", "."); "yes" },
                    error = function(e) paste("no:", conditionMessage(e)))
  paste0("Tcl ", tcl_ver, ", windows available: ", tk_ok)
}, error = function(e) paste("no:", conditionMessage(e)))
kv("Tcl/Tk", tk_status)
if (is_windows) {
  kv("Windows dialogs available", c(
    paste0("choose.files=", exists("choose.files", envir = asNamespace("utils"))),
    paste0("winDialog=", exists("winDialog", envir = asNamespace("utils")))))
}
if (is_mac) kv("XQuartz present", dir.exists("/opt/X11"))
if (!is_windows && !is_mac) {
  kv("DISPLAY", Sys.getenv("DISPLAY", "not set"))
  kv("WAYLAND_DISPLAY", Sys.getenv("WAYLAND_DISPLAY", "not set"))
}

section("Key packages")
ip <- installed.packages()
ip <- ip[!duplicated(ip[, "Package"]), , drop = FALSE]
key <- c("survival", "tcltk", "shiny", "jsonlite", "readxl", "openxlsx",
         "writexl", "ggplot2", "data.table", "rstudioapi")
for (p in key) {
  status <- if (p %in% rownames(ip)) {
    loads <- suppressWarnings(requireNamespace(p, quietly = TRUE))
    paste0(ip[p, "Version"], if (loads) ", loads" else ", FAILS TO LOAD")
  } else "not installed"
  kv(p, status)
}

section("Locale and file handling")
kv("Locale", Sys.getlocale())
kv("UTF-8 locale", l10n_info()[["UTF-8"]])
kv("Decimal mark", Sys.localeconv()[["decimal_point"]])
kv("Time zone", safe(Sys.timezone()))
kv("Can write to temp folder", can_write(tempdir()))
kv("Can write to home folder", can_write(path.expand("~")))
desktops <- c(file.path(Sys.getenv("USERPROFILE"), "Desktop"),
              file.path(Sys.getenv("OneDrive"), "Desktop"),
              file.path(Sys.getenv("HOME"), "Desktop"))
desktops <- unique(desktops[nzchar(dirname(desktops)) & dirname(desktops) != "."])
desktop <- desktops[dir.exists(desktops)][1]
kv("Can write to Desktop", if (is.na(desktop)) "folder not found" else can_write(desktop))

section("All installed packages (name, version, priority)")
add(sprintf("%s %s %s", ip[, "Package"], ip[, "Version"],
            ifelse(is.na(ip[, "Priority"]), "", ip[, "Priority"]))[order(tolower(ip[, "Package"]))])

out <- mask(out)
targets <- c(if (!is.na(desktop)) desktop, path.expand("~"), tempdir())
report_path <- NA
for (d in targets) {
  if (can_write(d) == "yes") {
    report_path <- file.path(d, "environment_report.txt")
    writeLines(out, report_path)
    break
  }
}
cat(out, sep = "\n")
cat("\n\nReport saved to:", report_path, "\n")
cat("Please send this file back. It contains no patient data.\n")
