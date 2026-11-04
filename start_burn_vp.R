# Starts the burn_vp tool. In R (Rgui): File > Source R code..., select this file.
#
# What it does: finds the project folder (the folder this file is in), checks that R matches the
# project, offers to run the one-time setup when the packages are not installed yet, and opens the
# program window. It leaves nothing in the R workspace and works from any working directory.
#
# Reads:  PROJECT_VERSION, renv.lock, the R/ files, and the project package library (renv/library).
# Writes: nothing itself. When you accept the setup it writes renv/library and output/setup (see
#         scripts/setup_environment.R). The program window writes only inside output/.
#
# R's own temporary folder stays in the Windows temporary folder when started from Rgui.

local({
  # The project has to be found before any helper can be loaded, so this part is inline.
  start <- NA_character_
  for (frame in rev(sys.frames())) {
    candidate <- get0("ofile", envir = frame, inherits = FALSE)
    if (is.character(candidate) && length(candidate) == 1 && !is.na(candidate) &&
          file.exists(candidate)) {
      start <- candidate
      break
    }
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (is.na(start) && length(file_arg) > 0) {
    start <- sub("^--file=", "", file_arg[1])
    if (!file.exists(start)) start <- gsub("~+~", " ", start, fixed = TRUE)
  }
  folder <- if (is.na(start)) NA_character_ else dirname(normalizePath(start, winslash = "/"))
  has_helpers <- function(dir) !is.na(dir) && file.exists(file.path(dir, "R", "start.R"))
  if (!has_helpers(folder) && interactive() && .Platform$OS.type == "windows") {
    picked <- utils::choose.dir(caption = paste("Pick the project folder (the folder that",
                                                "contains the file PROJECT_VERSION)"))
    if (length(picked) == 1 && !is.na(picked) && has_helpers(picked)) {
      folder <- normalizePath(picked, winslash = "/")
    }
  }
  if (!has_helpers(folder)) {
    stop("The project files were not found. Source start_burn_vp.R from the complete project ",
         "folder. Nothing was changed.", call. = FALSE)
  }

  app <- new.env(parent = globalenv())
  for (f in list.files(file.path(folder, "R"), pattern = "\\.R$", full.names = TRUE)) {
    source(f, local = app)
  }
  invisible(app$start_burn_vp(folder))
})
