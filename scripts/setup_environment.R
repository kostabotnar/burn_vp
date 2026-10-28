# =============================================================================
# Setup for the cohort comparison tool
# =============================================================================
#
# PURPOSE
#   Installs the R packages the project needs into the project's own package
#   library, then checks that everything works by running the whole analysis
#   once on fake (synthetic) data. You only need to run it once on each
#   computer. Running it again is safe: it finds that everything is already
#   installed and only repeats the checks.
#
# WHAT THIS SCRIPT READS
#   - The files "PROJECT_VERSION", "renv.lock" and "metadata/codebook.csv" and
#     the R scripts in the "R" folder of the project. From "renv.lock" it uses
#     the R version and the names and versions of the packages.
#   - Free disk space on the drive of the project folder (it asks Windows for
#     the folder listing of the drive and reads only the last line).
#   - Operating system and R version, to write them in the log.
#
# WHAT THIS SCRIPT WRITES
#   - Only inside the "output" folder of this project, with one exception
#     approved for the package library: the folders "renv/library" and
#     "renv/staging" in the project folder. Nothing is written to the home
#     folder, AppData, the Desktop or any other location. (R itself keeps a
#     temporary folder of its own. setup.bat moves it to "output/tmp"; when
#     this file is run from Rgui, it stays in the Windows temporary folder.)
#   - The packages: downloaded packages are kept in "output/renv" and
#     installed into "renv/library".
#   - A log file, output/setup/setup_<date>_<time>.txt.
#   - Fake data and results from the test run: output/setup/ and
#     output/runs/<date>_<time>_setup_check/.
#   - Nothing is written unless the project folder is confirmed: the folder
#     above this script's folder must contain a readable file "PROJECT_VERSION"
#     with the line "Project: burn_vp".
#
# WHAT THIS SCRIPT NEEDS
#   - Internet access to the package server packagemanager.posit.co. If your
#     hospital network blocks it, the script says so and shows the address
#     to give to IT.
#   - About 500 MB of free disk space.
#   - No administrator rights and no Rtools (only ready-made packages are used).
#
# WHAT THIS SCRIPT DOES NOT DO
#   - It does not read or use patient data. The test run uses fake data only.
#   - It does not change system or R settings and does not install anything
#     outside the project folder.
#
# PRIVACY
#   The log file replaces your user name and home folder path with <user> and
#   <home>. It contains no patient data. If setup fails, send us this file.
#
# HOW TO RUN
#   Easiest: double-click "setup.bat" in the project folder.
#
#   Or in R (Rgui):
#     Start R from the Start menu, choose File > Source R code..., go to the
#     project folder, open the "scripts" folder, and select this file
#     ("setup_environment.R"). Wait until the final message says
#     "SETUP COMPLETE" or "SETUP FAILED". The first run can take several
#     minutes. If R asks you to pick the project folder, pick the folder that
#     contains the file "PROJECT_VERSION".
#
# REQUIREMENTS
#   R 4.5 for Windows. Base R only is needed to start; the script installs the
#   rest.
# =============================================================================

local({
  # The package server snapshot. Every package version in renv.lock is
  # available there as a Windows binary, which is why no Rtools is needed.
  package_repository <- "https://packagemanager.posit.co/cran/2026-10-27"
  min_free_mb <- 500
  is_windows <- .Platform$OS.type == "windows"
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # ---- Load the helper functions ---------------------------------------------

  # They sit in R/ next to the scripts folder. They are loaded into their own
  # environment so nothing is left in the R workspace. This part has to find the
  # project before the helpers exist, so it is small and separate.
  app <- new.env(parent = globalenv())
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
  start <- if (is.na(start)) NA_character_ else dirname(normalizePath(start, winslash = "/"))
  has_helpers <- function(scripts_dir) {
    !is.na(scripts_dir) && file.exists(file.path(dirname(scripts_dir), "R", "setup.R"))
  }
  if (!has_helpers(start) && interactive() && is_windows) {
    picked <- utils::choose.dir(caption = paste("Pick the project folder (the folder that",
                                                "contains the file PROJECT_VERSION)"))
    if (length(picked) == 1 && !is.na(picked)) {
      for (candidate in c(picked, dirname(picked))) {
        if (has_helpers(file.path(candidate, "scripts"))) {
          start <- normalizePath(file.path(candidate, "scripts"), winslash = "/")
          break
        }
      }
    }
  }
  if (!has_helpers(start)) {
    stop("The project files were not found. Run setup.bat in the project folder, or run this ",
         "file from the scripts folder of the complete project folder. Nothing was changed.",
         call. = FALSE)
  }
  for (f in c("project_paths.R", "setup.R")) {
    source(file.path(dirname(start), "R", f), local = app)
  }

  # ---- Main part --------------------------------------------------------------

  local({
    log_lines <- character()
    say <- function(...) {
      text <- paste0(...)
      cat(text, "\n", sep = "")
      log_lines <<- c(log_lines, text)
    }
    show_check <- function(check) {
      label <- c(ok = "OK", warning = "WARNING", fail = "PROBLEM", note = "NOTE")[[check$status]]
      say("[", label, "] ", check$message)
      invisible(check$status)
    }
    # A planned stop: `message` says what went wrong, `action` the one thing to do.
    fail <- function(message, action = paste("Send the log file named below to the project",
                                             "team. Do not change anything first.")) {
      stop(structure(class = c("setup_failure", "error", "condition"),
                     list(message = message, action = action, call = NULL)))
    }

    root <- NA_character_
    project_version <- "unknown"
    failure <- NULL
    package_table <- NULL

    run <- function() {
      # Step 1
      say("Step 1 of 5: Finding the project folder")
      root <<- setup_find_root(start)
      project_version <<- read.dcf(file.path(root, "PROJECT_VERSION"),
                                   fields = "Version")[1, "Version"]
      say("Project folder: ", root)
      say("Project version: ", project_version)

      # Step 2
      say("")
      say("Step 2 of 5: Checking this computer and the network")
      checks <- list(
        r_version = setup_check_r_version(setup_lock_r_version(root)),
        repository = setup_check_repository(package_repository),
        disk = setup_check_disk(setup_free_disk_mb(root), min_free_mb)
      )
      for (check in checks) show_check(check)
      path_warnings <- setup_path_warnings(root)
      if (length(path_warnings) == 0) {
        say("[OK] Project folder path: no problems found.")
      }
      for (w in path_warnings) say("[WARNING] ", w)
      if (checks$r_version$status == "fail") {
        fail("This version of R does not match the project.",
             "Install the R version named above, then run setup again.")
      }

      # Step 3
      say("")
      say("Step 3 of 5: Preparing the package library")
      options(repos = c(CRAN = package_repository), pkgType = "binary",
              install.packages.compile.from.source = "never")
      repository_action <- paste("Ask IT to allow the package server address shown above,",
                                 "then run setup again.")
      # renv downloads itself on the first run. The lockfile names the "latest"
      # server, so the snapshot is given here to keep to one address.
      Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = package_repository)
      activation_error <- ""
      tryCatch(suppressWarnings(activate_project_renv(root, packages = character())),
               error = function(e) activation_error <<- conditionMessage(e))
      library_path <- normalizePath(.libPaths()[1], winslash = "/")
      if (!startsWith(library_path, paste0(root, "/renv/"))) {
        if (checks$repository$status == "fail") {
          fail("The package tool could not be downloaded.", repository_action)
        }
        fail(paste("The project package library could not be activated.", activation_error))
      }
      say("renv (package tool) version: ", as.character(utils::packageVersion("renv")))

      lock <- renv::lockfile_read(file.path(root, "renv.lock"))
      missing_in_lock <- setdiff(project_packages, names(lock$Packages))
      if (length(missing_in_lock) > 0) {
        fail(paste("The file renv.lock does not list:", paste(missing_in_lock, collapse = ", ")),
             "Ask the project team for a complete copy of the project.")
      }
      needed <- setup_dependency_closure(lock$Packages, project_packages)
      locked <- setup_lock_versions(lock$Packages, needed)
      installed <- setup_installed_versions(needed, library_path)
      to_restore <- setup_packages_to_restore(locked, installed)

      # Step 4
      say("")
      if (length(to_restore) == 0) {
        say("Step 4 of 5: Installing packages (skipped, all ", length(needed),
            " are already installed)")
      } else {
        say("Step 4 of 5: Installing ", length(to_restore), " of ", length(needed),
            " packages (this can take several minutes)")
        if (checks$repository$status == "fail") {
          fail("The packages cannot be downloaded because the package server is not reachable.",
               repository_action)
        }
        renv::restore(project = root, packages = project_packages,
                      repos = c(CRAN = package_repository), prompt = FALSE)
      }
      runtime_locked <- setup_lock_versions(lock$Packages, project_packages)
      runtime_installed <- setup_installed_versions(project_packages, library_path)
      package_table <<- setup_verify_packages(project_packages, runtime_locked, runtime_installed)
      for (i in seq_len(nrow(package_table))) {
        row <- package_table[i, ]
        say(if (row$ok) "[OK] " else "[PROBLEM] ", row$package, ": ",
            if (row$ok) {
              paste0("version ", row$installed, " installed and loads.")
            } else if (is.na(row$installed)) {
              "not installed."
            } else {
              paste0("version ", row$installed, ", the project needs ", row$locked,
                     if (row$loads) "." else " and it does not load.")
            })
      }
      if (!all(package_table$ok)) fail("Some packages are missing or do not work.")

      # Step 5
      say("")
      say("Step 5 of 5: Testing the whole analysis with fake data")
      for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
        source(f, local = app)
      }
      report <- tryCatch({
        codebook <- read_codebook(file.path(root, "metadata", "codebook.csv"))
        data <- generate_synthetic_nbc(codebook, n = 60, seed = 1)
        xlsx <- file.path(project_output_dir(root, "setup"), "setup_check_synthetic.xlsx")
        write_nbc_xlsx(data, codebook, xlsx)
        run_dir <- suppressMessages(run_pipeline(c(NBC = xlsx), root, run_label = "setup_check"))
        file.path(run_dir, "report.html")
      }, error = function(e) {
        fail(paste("The test run stopped with this message:", conditionMessage(e)))
      })
      output_dir <- normalizePath(file.path(root, "output"), winslash = "/")
      if (!file.exists(report) ||
            !startsWith(normalizePath(report, winslash = "/"), paste0(output_dir, "/"))) {
        fail("The test run did not produce its report.")
      }
      say("[OK] The test run produced its report: ",
          substring(normalizePath(report, winslash = "/"), nchar(root) + 2))
    }

    outcome <- tryCatch({
      run()
      "complete"
    }, setup_failure = function(e) {
      failure <<- list(message = conditionMessage(e), action = e$action)
      "failed"
    }, error = function(e) {
      failure <<- list(message = paste("An unexpected problem occurred:", conditionMessage(e)),
                       action = paste("Send the log file named below to the project team. Do",
                                      "not change anything first."))
      "failed"
    })

    # Log (only when the project folder is known and confirmed).
    header <- c(
      paste("Setup log, script version 1.0, date", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("Outcome:", toupper(outcome)),
      paste("R version:", R.version.string),
      paste("Platform:", R.version$platform),
      paste("System:", utils::sessionInfo()$running),
      paste("Project version:", project_version),
      paste("Package repository:", package_repository),
      paste("Reason:", if (is.null(failure)) "none" else failure$message),
      ""
    )
    packages <- if (is.null(package_table)) {
      "Packages: not checked"
    } else {
      c("Packages (name, locked version, installed version, loads):",
        paste(package_table$package, package_table$locked, package_table$installed,
              package_table$loads))
    }
    log_file <- NA_character_
    if (!is.na(root)) {
      log_file <- tryCatch(setup_write_log(root, c(header, log_lines, "", packages), stamp),
                           error = function(e) NA_character_)
    }
    log_name <- if (is.na(log_file)) {
      NULL
    } else {
      paste0("output/setup/", basename(log_file), " (in the project folder)")
    }

    say("")
    say("=====================================================")
    if (outcome == "complete") {
      cat("SETUP COMPLETE\n")
      cat("Everything is installed and the test run worked.\n")
      cat("You can now use the tool. You do not need to run setup again.\n")
      if (!is.null(log_name)) cat("Log file: ", log_name, "\n", sep = "")
    } else {
      cat("SETUP FAILED\n")
      cat("What went wrong: ", failure$message, "\n", sep = "")
      cat("What to do: ", failure$action, "\n", sep = "")
      if (is.null(log_name)) {
        cat("No log file could be written. Copy the text above and send it to us.\n")
      } else {
        cat("Log file: ", log_name, "\n", sep = "")
      }
    }
    cat("=====================================================\n")

    if (outcome != "complete" && !interactive()) quit(status = 1)
    invisible(outcome)
  }, envir = list2env(list(package_repository = package_repository, min_free_mb = min_free_mb,
                           stamp = stamp, start = start, app = app), parent = app))
})
