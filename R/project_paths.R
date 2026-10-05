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
