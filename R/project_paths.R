expected_project_name <- "burn_vp"

# TRUE only when `root` holds a readable PROJECT_VERSION whose Project field is burn_vp.
# `root` is the project folder itself; parent folders are never searched.
is_project_root <- function(root) {
  if (!is.character(root) || length(root) != 1 || is.na(root)) return(FALSE)
  marker <- file.path(root, "PROJECT_VERSION")
  if (!file.exists(marker)) return(FALSE)
  info <- tryCatch(suppressWarnings(read.dcf(marker, fields = "Project")),
                   error = function(e) NULL)
  !is.null(info) && nrow(info) >= 1 && isTRUE(info[1, "Project"] == expected_project_name)
}

# Returns <root>/output/<subfolder>, creating it when missing. Stops without writing anything
# when the root is not confirmed or the subfolder would point outside output/.
project_output_dir <- function(root, subfolder = NULL) {
  if (!is_project_root(root)) {
    stop("Project folder not confirmed: no PROJECT_VERSION with 'Project: ", expected_project_name,
         "' in the given folder. Nothing was written.", call. = FALSE)
  }
  parts <- character()
  if (!is.null(subfolder)) {
    parts <- strsplit(subfolder, "[/\\\\]")[[1]]
    parts <- parts[nzchar(parts)]
    outside <- grepl("^([A-Za-z]:|[/\\\\~])", subfolder) || any(parts %in% c(".", ".."))
    if (length(parts) == 0 || outside) {
      stop("'subfolder' must be a relative path inside output/.", call. = FALSE)
    }
  }
  target <- do.call(file.path, as.list(c(root, "output", parts)))
  if (!dir.exists(target)) dir.create(target, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(target)) stop("Could not create output folder: ", target, call. = FALSE)
  normalizePath(target, winslash = "/")
}

# Packages the scripts need from the project's renv library.
project_packages <- c("readxl", "openxlsx", "ggplot2", "htmltools", "base64enc", "withr")

# Environment variables that keep renv's own files inside the project's output/ folder (the only
# place project code may write, apart from renv/library and renv/staging): no user-level cache and
# no sandbox copy of the system library. They must be set before renv loads.
project_renv_env <- function(root) {
  root <- normalizePath(root, winslash = "/")
  c(RENV_PATHS_ROOT = file.path(root, "output", "renv"),
    RENV_CONFIG_CACHE_ENABLED = "FALSE",
    RENV_CONFIG_SANDBOX_ENABLED = "FALSE",
    RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE")
}

# Makes the project's renv library the active library, whatever the working directory is.
# renv's activate.R takes the project from RENV_PROJECT, else from the working directory, so
# both are pointed at `root` for the call and the working directory is restored afterwards.
# Does nothing when renv is already active for this root (script started from the project folder).
# Stops with a plain message when a package in `packages` is still missing; it never installs.
activate_project_renv <- function(root, packages = project_packages) {
  if (!is_project_root(root)) {
    stop("Project folder not confirmed: no PROJECT_VERSION with 'Project: ", expected_project_name,
         "' in the given folder.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/")
  activate <- file.path(root, "renv", "activate.R")
  if (!file.exists(activate)) {
    stop("renv/activate.R not found in the project folder: ", root, call. = FALSE)
  }
  do.call(Sys.setenv, as.list(project_renv_env(root)))
  active_project <- normalizePath(Sys.getenv("RENV_PROJECT", unset = ""), winslash = "/",
                                  mustWork = FALSE)
  already <- identical(active_project, root) &&
    startsWith(normalizePath(.libPaths()[1], winslash = "/"), paste0(root, "/renv/"))
  if (!already) {
    old_wd <- setwd(root)
    on.exit(setwd(old_wd), add = TRUE)
    Sys.setenv(RENV_PROJECT = root)
    source(activate, local = new.env())
  }
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The project's R packages are not installed (missing: ", paste(missing, collapse = ", "),
         "). Open R in the project folder ", root, " and run renv::restore(), then run this ",
         "script again. Nothing was done.", call. = FALSE)
  }
  invisible(root)
}
