# nolint start: object_usage_linter. Functions from the other R/ files are not visible here.
pipeline_comparison_vars <- c(
  "age_years", "gender", "admission_weight_kg", "tbsa_burned_pct", "inhalation_injury",
  "diabetes", "hypertension", "burn_to_icu_h", "any_ne_48h", "vasopressor_type", "peak_ne_48h",
  "ne_delay_h", "any_other_vp_48h", "any_albumin_48h", "crystalloids_24h_ml_per_kg",
  "crystalloids_48h_ml_per_kg", "crystalloids_24h_ml_per_kg_tbsa",
  "crystalloids_24h_parkland_ratio", "total_fluids_48h_ml_per_kg", "hospital_mortality",
  "hospital_los_days", "rrt", "sepsis", "diuresis_0_48h_ml_kg_h", "peak_lactate_0_24h_mmol_l",
  "peak_creatinine_0_72h_umol_l", "lowest_map_48h_mmhg", "highest_pulse_48h_bpm"
)

with_site <- function(df, site) {
  if (nrow(df) == 0) {
    df$site <- character()
    return(df[c("site", setdiff(names(df), "site"))])
  }
  cbind(site = site, df, stringsAsFactors = FALSE)
}

drop_attrs <- function(x, names) {
  for (nm in names) attr(x, nm) <- NULL
  x
}

bind_tables <- function(tables, empty) {
  tables <- Filter(function(t) nrow(t) > 0, tables)
  if (length(tables) == 0) return(empty)
  out <- do.call(rbind, unname(tables))
  rownames(out) <- NULL
  out
}

run_folder_name <- function(label) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  if (is.null(label) || !nzchar(label)) return(stamp)
  paste0(stamp, "_", gsub("[^A-Za-z0-9_-]", "_", label))
}

# A run folder that does not exist yet; a counter is added when two runs start in the same second.
new_run_dir <- function(base, label) {
  name <- run_folder_name(label)
  dir <- file.path(base, name)
  k <- 1
  while (dir.exists(dir)) {
    k <- k + 1
    dir <- file.path(base, sprintf("%s_%d", name, k))
  }
  dir.create(dir)
  for (sub in c("qc", "profile", "figures")) dir.create(file.path(dir, sub))
  dir
}

# Runs the whole chain: read and standardise each site, QC, masking, profiling, figures and
# the HTML report. `inputs` is a named character vector, site = path to the xlsx file.
# Everything is written to <root>/output/<out_subfolder>/<YYYYmmdd_HHMMSS>[_<label>]/.
# Rows in the QC outputs count data rows within each site file.
run_pipeline <- function(inputs, root, run_label = NULL, out_subfolder = "runs") {
  if (!is.character(inputs) || length(inputs) == 0 || is.null(names(inputs)) ||
        any(!nzchar(names(inputs))) || anyDuplicated(names(inputs)) > 0) {
    stop("'inputs' must be a character vector with a unique site name for each file.",
         call. = FALSE)
  }
  out_base <- project_output_dir(root, out_subfolder) # confirms the project root first
  codebook_path <- file.path(root, "metadata", "codebook.csv")
  ranges_path <- file.path(root, "metadata", "qc_ranges.csv")
  codebook <- read_codebook(codebook_path)
  ranges <- read_qc_ranges(ranges_path)

  site_data <- list()
  parse_problems <- list()
  unmapped <- list()
  recodes <- list()
  for (site in names(inputs)) {
    message("Reading ", site, ": ", basename(inputs[[site]]))
    raw <- suppressWarnings(read_site_data(inputs[[site]], codebook, site))
    std <- standardise_values(raw, codebook)
    parse_problems[[site]] <- with_site(attr(raw, "parse_problems"), site)
    unmapped[[site]] <- with_site(attr(std, "unmapped_values"), site)
    recodes[[site]] <- with_site(attr(std, "recode_log"), site)
    site_data[[site]] <- drop_attrs(std, c("parse_problems", "unmapped_values", "recode_log"))
  }
  data <- do.call(rbind, unname(site_data))
  rownames(data) <- NULL
  parse_problems <- bind_tables(parse_problems, with_site(data.frame(
    row = integer(), column = character(), raw_value = character(),
    expected_type = character()
  ), ""))
  unmapped <- bind_tables(unmapped, with_site(data.frame(
    row = integer(), column = character(), raw_value = character()
  ), ""))
  recode_log <- bind_tables(recodes, with_site(data.frame(
    row = integer(), column = character(), raw_value = character(), new_value = character(),
    rule = character()
  ), ""))
  attr(data, "parse_problems") <- parse_problems
  attr(data, "unmapped_values") <- unmapped

  message("Running QC rules")
  qc <- run_qc(data, codebook, ranges, qc_rules(codebook, ranges))
  qc$findings <- localise_finding_rows(qc$findings, data)
  data <- drop_attrs(data, c("parse_problems", "unmapped_values"))

  message("Profiling")
  masked <- mask_flagged_values(data, qc$findings)
  masked_counts <- attr(masked, "masked_counts")
  analysis <- derive_analysis_variables(masked)
  profile <- profile_variables(analysis, codebook)
  comparison <- site_comparison(analysis, codebook, pipeline_comparison_vars)
  exposure <- exposure_outcome_summary(analysis)
  correlation <- correlation_matrix(analysis)

  run_dir <- new_run_dir(out_base, run_label)
  write_qc_outputs(qc, file.path(run_dir, "qc"))
  utils::write.csv(recode_log, file.path(run_dir, "qc", "recode_log.csv"), row.names = FALSE,
                   na = "", fileEncoding = "UTF-8")
  write_profile_outputs(list(
    variables_numeric = profile$numeric, variables_categorical = profile$categorical,
    site_comparison = comparison, exposure_outcome = exposure, correlation = correlation,
    masked_counts = masked_counts
  ), file.path(run_dir, "profile"))

  message("Drawing figures")
  figures <- save_profile_figures(
    build_profile_figures(analysis, qc$missingness, correlation), file.path(run_dir, "figures")
  )
  rows <- vapply(site_data, nrow, integer(1))
  run <- report_run_info(inputs, rows, root, codebook, codebook_path, ranges_path, qc)
  write_html_report(file.path(run_dir, "report.html"), run, qc, recode_log, masked_counts,
                    comparison, exposure, figures, codebook)
  message("Run folder: ", run_dir)
  invisible(run_dir)
}
# nolint end
