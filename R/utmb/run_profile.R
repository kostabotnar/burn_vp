# nolint start: object_usage_linter. Functions from the other files are not visible here.
# Profiles every UTMB table found in `data_dir`. Results go to
# <root>/output/utmb/profile_<YYYYmmdd_HHMMSS>[_<label>]/ and the structure-only dictionary to
# <root>/metadata/utmb_dictionary.csv (when write_dictionary is TRUE). Nothing is written unless
# `root` is confirmed by PROJECT_VERSION. Returns the run folder.
run_utmb_profile <- function(data_dir, root, label = NULL, write_dictionary = TRUE,
                             progress = function(...) cat(..., "\n", sep = "")) {
  files <- utmb_table_files(data_dir)
  if (length(files) == 0) stop("No UTMB tables found in ", data_dir, call. = FALSE)
  if (!"crosswalk" %in% names(files)) {
    stop("The crosswalk table is required but was not found in ", data_dir, call. = FALSE)
  }
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  suffix <- if (is.null(label) || !nzchar(label)) {
    ""
  } else {
    paste0("_", gsub("[^A-Za-z0-9_-]", "_", label))
  }
  run_dir <- project_output_dir(root, file.path("utmb", paste0("profile_", stamp, suffix)))

  started <- Sys.time()
  crosswalk_ids <- unique(read_utmb_table(files[["crosswalk"]])[["StudyID"]])
  kept <- list()
  keep_columns <- list(
    crosswalk = c("StudyID", "BurnCode"),
    demographics = c("StudyID", "BirthDate", "Sex", "SexAssignedAtBirth", "FirstRace",
                     "MultiRacial", "Ethnicity", "SmokingStatus", "DeathDate", "Status",
                     "IsValid", "Test"),
    hospital_admissions = c("StudyID", "AdmissionDateKey", "AdmissionTimeOfDayKey",
                            "LengthOfStayInDays")
  )
  stats_parts <- list()
  profile_parts <- list()
  for (i in seq_along(files)) {
    table <- names(files)[i]
    t0 <- Sys.time()
    dt <- read_utmb_table(files[[i]])
    t1 <- Sys.time()
    profile <- profile_utmb_columns(dt, table)
    stats <- compute_utmb_table_stats(dt, table, profile, crosswalk_ids,
                                      unname(utmb_event_date_columns[table]),
                                      file_bytes = file.size(files[[i]]))
    t2 <- Sys.time()
    stats$read_seconds <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
    stats$profile_seconds <- round(as.numeric(difftime(t2, t1, units = "secs")), 1)
    if (table %in% names(keep_columns)) {
      cols <- intersect(keep_columns[[table]], names(dt))
      kept[[table]] <- dt[, cols, with = FALSE]
    }
    stats_parts[[table]] <- stats
    profile_parts[[table]] <- profile
    progress(sprintf("[%d/%d] %s: %s rows, %d columns, read %.1f s, profile %.1f s",
                     i, length(files), table, format(nrow(dt), big.mark = ","), ncol(dt),
                     stats$read_seconds, stats$profile_seconds))
    rm(dt)
    invisible(gc())
  }

  table_stats <- do.call(rbind, stats_parts)
  column_profile <- do.call(rbind, profile_parts)
  col_order <- c("table", "position", "column")
  column_profile <- column_profile[, c(col_order, setdiff(names(column_profile), col_order))]
  rownames(column_profile) <- NULL
  rownames(table_stats) <- NULL

  demo <- summarise_utmb_demographics(kept[["demographics"]], kept[["crosswalk"]],
                                      kept[["hospital_admissions"]])

  write_csv <- function(x, name) {
    data.table::fwrite(x, file.path(run_dir, name), na = "")
  }
  write_csv(column_profile, "column_profile.csv")
  write_csv(table_stats, "table_stats.csv")
  write_csv(demo$categorical, "demographics_categorical.csv")
  if (!is.null(demo$numeric)) write_csv(demo$numeric, "demographics_numeric.csv")
  total_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  writeLines(utmb_summary_lines(table_stats, column_profile, demo, total_seconds),
             file.path(run_dir, "summary.txt"))

  if (write_dictionary) {
    dictionary <- do.call(rbind, lapply(split(column_profile, column_profile$table), function(p) {
      p <- p[order(p$position), ]
      build_utmb_dictionary(p$table[1], p$column, p$inferred_type, p$n_distinct)
    }))
    write_utmb_dictionary(dictionary, root)
  }
  progress("Output folder: ", run_dir)
  invisible(run_dir)
}

utmb_summary_lines <- function(table_stats, column_profile, demo, total_seconds) {
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  lines <- c("UTMB profile summary", "",
             sprintf("Tables: %d. Total run time: %.0f s.", nrow(table_stats), total_seconds),
             "", "Per table (rows, columns, patients, read and profile seconds):")
  for (i in seq_len(nrow(table_stats))) {
    s <- table_stats[i, ]
    lines <- c(lines, sprintf("  %s: %s rows, %d columns, %s patients, %.1f s read, %.1f s profile",
                              s$table, fmt(s$n_rows), s$n_cols, fmt(s$n_patients),
                              s$read_seconds, s$profile_seconds))
  }
  roles <- table(column_profile$role)
  lines <- c(lines, "", "Columns by role:",
             sprintf("  %s: %d", names(roles), as.integer(roles)))
  types <- table(column_profile$inferred_type)
  lines <- c(lines, "", "Columns by inferred type:",
             sprintf("  %s: %d", names(types), as.integer(types)))
  lines <- c(lines, "",
             sprintf("Patients absent from the crosswalk, summed over tables: %s",
                     fmt(sum(table_stats$n_patients_not_in_crosswalk))),
             sprintf("Fully duplicated rows, summed over tables: %s",
                     fmt(sum(table_stats$n_duplicate_rows))))
  cat_df <- demo$categorical
  lines <- c(lines, "", "Demographics (counts and percentages):")
  for (v in unique(cat_df$variable)) {
    d <- cat_df[cat_df$variable == v, ]
    lines <- c(lines, sprintf("  %s: %s", v,
                              paste0(d$level, " ", d$n, " (", d$pct, "%)", collapse = "; ")))
  }
  if (!is.null(demo$numeric)) {
    lines <- c(lines, "", "Numeric summaries (n, median, q1-q3):")
    for (i in seq_len(nrow(demo$numeric))) {
      d <- demo$numeric[i, ]
      lines <- c(lines, sprintf("  %s: n=%d, median %s (%s to %s)", d$variable, d$n,
                                format(d$median), format(d$q1), format(d$q3)))
    }
  }
  lines
}
# nolint end
