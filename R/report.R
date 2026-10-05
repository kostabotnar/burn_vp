# nolint start: object_usage_linter. Functions from the other R/ files are not visible here.
report_sections <- c(
  "Run information", "QC summary", "Site comparison", "Exposures and outcomes", "Figures",
  "Assumptions and open questions"
)

report_css <- "
body { font-family: system-ui, -apple-system, 'Segoe UI', sans-serif; color: #0b0b0b;
  background: #f9f9f7; margin: 0; line-height: 1.45; }
main { max-width: 1100px; margin: 0 auto; padding: 24px 28px 60px; background: #fcfcfb; }
h1 { font-size: 1.7rem; font-weight: 500; margin: 0 0 4px; }
h2 { font-size: 1.25rem; font-weight: 500; margin: 36px 0 8px; padding-top: 12px;
  border-top: 1px solid #e1e0d9; }
h3 { font-size: 1rem; font-weight: 600; margin: 20px 0 6px; color: #52514e; }
p, li { max-width: 80ch; }
.muted { color: #52514e; }
table { border-collapse: collapse; margin: 6px 0 14px; font-size: 0.88rem; }
caption { text-align: left; color: #52514e; padding-bottom: 4px; }
th, td { padding: 4px 10px; border-bottom: 1px solid #e1e0d9; text-align: left;
  vertical-align: top; }
th { color: #52514e; font-weight: 600; border-bottom: 1px solid #c3c2b7; }
td.num { text-align: right; font-variant-numeric: tabular-nums; }
figure { margin: 18px 0 28px; }
figure img { max-width: 100%; height: auto; border: 1px solid #e1e0d9; }
figcaption { color: #52514e; font-size: 0.9rem; margin-top: 6px; max-width: 80ch; }
"

# Plain text for HTML: no en or em dashes, whatever the source text holds.
report_plain <- function(x) gsub("[\u2013\u2014]", "-", x)

fmt_cell <- function(x) {
  if (is.numeric(x)) {
    ifelse(is.na(x), "",
           ifelse(x == round(x) & abs(x) < 1e9, format(x, scientific = FALSE, trim = TRUE),
                  formatC(x, format = "f", digits = 2)))
  } else {
    ifelse(is.na(x), "", as.character(x))
  }
}

html_table <- function(df, caption = NULL) {
  if (nrow(df) == 0) return(htmltools::tags$p(class = "muted", "None."))
  head <- htmltools::tags$tr(lapply(names(df), htmltools::tags$th))
  body <- lapply(seq_len(nrow(df)), function(i) {
    htmltools::tags$tr(lapply(names(df), function(col) {
      htmltools::tags$td(class = if (is.numeric(df[[col]])) "num" else NULL,
                         fmt_cell(df[[col]][i]))
    }))
  })
  htmltools::tags$table(
    if (!is.null(caption)) htmltools::tags$caption(caption),
    htmltools::tags$thead(head), htmltools::tags$tbody(body)
  )
}

count_by <- function(df, col, sites) {
  vapply(sites, function(s) sum(df$site == s), integer(1))
}

# Run details: input files (names only), row counts, and versions of the metadata files.
report_run_info <- function(inputs, rows, root, codebook, codebook_path, ranges_path, qc) {
  marker <- tryCatch(read.dcf(file.path(root, "PROJECT_VERSION")), error = function(e) NULL)
  list(
    date = format(Sys.time(), "%Y-%m-%d %H:%M"),
    inputs = data.frame(site = names(inputs), file = basename(unname(inputs)),
                        rows = as.integer(rows[names(inputs)]), stringsAsFactors = FALSE),
    project = if (is.null(marker)) "unknown" else paste(marker[1, "Project"], marker[1, "Version"]),
    codebook = sprintf("%s (%d variables, checksum %s)", basename(codebook_path),
                       nrow(codebook), substr(unname(tools::md5sum(codebook_path)), 1, 8)),
    rules = sprintf("%d rules, plausible ranges %s (checksum %s)", nrow(qc$rule_catalogue),
                    basename(ranges_path), substr(unname(tools::md5sum(ranges_path)), 1, 8))
  )
}

section_run_info <- function(run) {
  items <- list(
    c("Report date", run$date), c("Project", run$project), c("Codebook", run$codebook),
    c("QC rules", run$rules)
  )
  htmltools::tagList(
    htmltools::tags$h2("1. Run information"),
    htmltools::tags$table(htmltools::tags$tbody(lapply(items, function(it) {
      htmltools::tags$tr(htmltools::tags$th(it[1]), htmltools::tags$td(it[2]))
    }))),
    htmltools::tags$h3("Input files"),
    html_table(setNames(run$inputs, c("Site", "File", "Patients")))
  )
}

severity_levels <- c("error", "warning", "info")

findings_by_category <- function(findings, sites) {
  if (nrow(findings) == 0) return(data.frame())
  tab <- table(factor(findings$site, sites), findings$category,
               factor(findings$severity, severity_levels))
  rows <- list()
  for (s in sites) {
    for (cat in colnames(tab)) {
      counts <- tab[s, cat, ]
      if (sum(counts) > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          Site = s, Category = cat, Errors = counts[["error"]], Warnings = counts[["warning"]],
          Info = counts[["info"]], Total = sum(counts), stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}

top_rules <- function(qc, sites, n = 10) {
  f <- qc$findings
  if (nrow(f) == 0) return(data.frame())
  totals <- sort(table(f$rule_id), decreasing = TRUE)
  ids <- names(utils::head(totals, n))
  cat_df <- qc$rule_catalogue
  out <- data.frame(Rule = ids, Category = cat_df$category[match(ids, cat_df$id)],
                    Severity = cat_df$severity[match(ids, cat_df$id)],
                    Description = cat_df$description[match(ids, cat_df$id)],
                    stringsAsFactors = FALSE)
  for (s in sites) out[[s]] <- vapply(ids, function(i) sum(f$rule_id == i & f$site == s), 0L)
  out$Total <- as.integer(totals[ids])
  out
}

problem_counts <- function(qc, recode_log, sites) {
  data.frame(
    Site = sites, `Parse problems` = count_by(qc$parse_problems, "site", sites),
    `Unmapped values` = count_by(qc$unmapped_values, "site", sites),
    Recodes = count_by(recode_log, "site", sites), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

section_qc <- function(qc, recode_log, masked_counts, sites) {
  high <- qc$missingness[qc$missingness$flag_high_missing %in% TRUE, , drop = FALSE]
  high <- high[order(-high$pct_unexpected_missing), , drop = FALSE]
  high_df <- data.frame(Variable = high$variable, Site = high$site,
                        `Unexpected missing (%)` = round(high$pct_unexpected_missing, 1),
                        check.names = FALSE, stringsAsFactors = FALSE)
  masked_df <- if (is.null(masked_counts) || nrow(masked_counts) == 0) {
    data.frame()
  } else {
    agg <- stats::aggregate(n_masked ~ variable + site, masked_counts, sum)
    agg <- agg[order(-agg$n_masked), , drop = FALSE]
    data.frame(Variable = agg$variable, Site = agg$site, `Values masked` = agg$n_masked,
               check.names = FALSE, stringsAsFactors = FALSE)
  }
  htmltools::tagList(
    htmltools::tags$h2("2. QC summary"),
    htmltools::tags$p(
      sprintf("%d findings in total. Patient identifiers and individual values are not listed",
              nrow(qc$findings)),
      "here; they are in qc/findings.csv in the run folder."
    ),
    htmltools::tags$h3("Findings by severity and category"),
    html_table(findings_by_category(qc$findings, sites)),
    htmltools::tags$h3("Rules with the most findings"),
    html_table(utils::head(top_rules(qc, sites), 10)),
    htmltools::tags$h3("Parse problems, unmapped values and recodes"),
    html_table(problem_counts(qc, recode_log, sites)),
    htmltools::tags$h3("Key variables with high unexpected missingness"),
    htmltools::tags$p(class = "muted",
                      "Flagged when more than 20% of values are missing without a reason."),
    html_table(high_df),
    htmltools::tags$h3("Values masked before profiling"),
    htmltools::tags$p(class = "muted", paste(
      "Values that failed a range, type or category rule were set to missing before the",
      "tables and figures below were made."
    )),
    html_table(utils::head(masked_df, 15))
  )
}

section_comparison <- function(comparison) {
  n_by_site <- attr(comparison, "n_by_site")
  shown <- comparison
  names(shown)[names(shown) == "smd"] <- "SMD"
  names(shown)[names(shown) == "variable"] <- "Variable"
  names(shown)[names(shown) == "level"] <- "Level"
  names(shown)[names(shown) == "statistic"] <- "Summary"
  names(shown)[names(shown) == "missing"] <- "Missing (by site)"
  site_cols <- names(n_by_site)
  names(shown)[match(site_cols, names(shown))] <- sprintf("%s (n = %d)", site_cols, n_by_site)
  shown$Variable[duplicated(shown$Variable)] <- ""
  shown$SMD <- round(shown$SMD, 2)
  htmltools::tagList(
    htmltools::tags$h2("3. Site comparison"),
    htmltools::tags$p(paste(
      "Continuous variables show the median with the first and third quartiles. Categorical",
      "variables show counts and percentages of patients with a recorded value.",
      "SMD is the absolute standardized mean difference; values above 0.1 are often read as a",
      "meaningful imbalance. It is shown on the first row of each variable."
    )),
    html_table(shown)
  )
}

section_exposures <- function(exposure) {
  sites <- unique(exposure$site)
  tables <- lapply(c("Exposure", "Outcome"), function(g) {
    d <- exposure[exposure$group == g, , drop = FALSE]
    wide <- data.frame(Measure = unique(d$measure), stringsAsFactors = FALSE)
    for (s in sites) {
      ds <- d[d$site == s, , drop = FALSE]
      wide[[paste0(s, ": n")]] <- ds$n[match(wide$Measure, ds$measure)]
      wide[[paste0(s, ": summary")]] <- ds$summary[match(wide$Measure, ds$measure)]
    }
    htmltools::tagList(htmltools::tags$h3(paste0(g, "s")), html_table(wide))
  })
  htmltools::tagList(
    htmltools::tags$h2("4. Exposures and outcomes"),
    htmltools::tags$p(paste(
      "Binary measures show n (%). Continuous measures show median [first quartile, third",
      "quartile]. Rates and delays are summarised among patients who received the drug.",
      "Hospital stay is given for all patients, for survivors and for non-survivors. These",
      "are descriptive summaries; no tests or models were run."
    )),
    tables
  )
}

section_figures <- function(figures) {
  items <- lapply(seq_len(nrow(figures)), function(i) {
    uri <- paste0("data:image/png;base64,", base64enc::base64encode(figures$path[i]))
    htmltools::tags$figure(
      htmltools::tags$img(src = uri, alt = figures$alt[i]),
      htmltools::tags$figcaption(
        htmltools::tags$strong(sprintf("Figure %d. %s. ", i, figures$title[i])), figures$caption[i]
      )
    )
  })
  htmltools::tagList(htmltools::tags$h2("5. Figures"), items)
}

# Short list of variables, for example "a, b, c and 2 more".
short_list <- function(x, n = 4) {
  if (length(x) <= n) return(paste(x, collapse = ", "))
  sprintf("%s and %d more", paste(x[seq_len(n)], collapse = ", "), length(x) - n)
}

report_assumptions <- c(
  paste("Fluid volumes for 24h, 48h and 72h are treated as cumulative from the burn (0-24 h,",
        "0-48 h, 0-72 h), not as separate intervals. The template does not say which it is."),
  paste("Hematocrit is read as a percentage. The template example is a fraction (0.43), and",
        "fractions are flagged by QC."),
  paste("Day counts for hospital stay, ICU stay and ventilation are unresolved: the",
        "dictionary counts days inclusively, the template example does not. Values are used as",
        "entered, and the QC duration rules accept both conventions."),
  paste("Diuresis is taken as the first 48 h, as in the template. The dictionary gives 0-24 h."),
  paste("RBC and FFP are recorded as numbers of units in the template, while the dictionary",
        "gives mL. They are not added to fluid totals."),
  paste("The delay to the first NE and the time to first surgery are taken as measured from the",
        "burn. First surgery is given in days and shown in hours (days times 24)."),
  paste("Peak NE rate over 0-48 h is the larger of the two window rates. A blank rate is read",
        "as zero only when the matching NE flag is No."),
  paste("A blank entry for other vasopressors is read as none given."),
  paste("The Parkland volume is 4 mL x weight (kg) x TBSA (%). Fluid per kg and per %TBSA uses",
        "admission weight and the TBSA burned. Crystalloids exclude albumin."),
  paste("Values that fail a range, type or category rule are set to missing before profiling,",
        "so known errors do not distort the summaries. Other rule failures do not mask values."),
  paste("Associations between exposures and outcomes are not tested here. Modelling is a",
        "separate step."),
  "With more than two sites, the SMD shown is the largest pairwise value."
)

section_assumptions <- function(codebook) {
  notes <- codebook[codebook$key_variable %in% TRUE & !is.na(codebook$notes) &
                      nzchar(codebook$notes), c("name", "notes")]
  groups <- split(notes$name, notes$notes)
  note_df <- data.frame(Note = names(groups), Variables = vapply(groups, short_list, ""),
                        stringsAsFactors = FALSE, row.names = NULL)
  htmltools::tagList(
    htmltools::tags$h2("6. Assumptions and open questions"),
    htmltools::tags$h3("Conventions used in this report"),
    htmltools::tags$ul(lapply(report_assumptions, htmltools::tags$li)),
    htmltools::tags$h3("Open questions from the codebook notes (key variables)"),
    html_table(note_df)
  )
}

# Writes one self-contained HTML file with aggregated results only: no patient identifiers and
# no individual values. `figures` is the catalogue returned by save_profile_figures().
write_html_report <- function(path, run, qc, recode_log, masked_counts, comparison, exposure,
                              figures, codebook) {
  # qc_inside_output() lives in R/qc_rules.R, which lintr does not see from this file.
  if (!qc_inside_output(dirname(path))) { # nolint: object_usage_linter.
    stop("The report can only be written in an existing folder inside <project root>/output/.",
         call. = FALSE)
  }
  sites <- run$inputs$site
  body <- htmltools::tags$main(
    htmltools::tags$h1("Vasopressor and fluid resuscitation after severe burn: data profile"),
    htmltools::tags$p(class = "muted", paste(
      "Data quality and descriptive comparison of the participating sites. Only aggregated",
      "results appear in this report."
    )),
    section_run_info(run), section_qc(qc, recode_log, masked_counts, sites),
    section_comparison(comparison), section_exposures(exposure), section_figures(figures),
    section_assumptions(codebook)
  )
  doc <- htmltools::tags$html(
    lang = "en",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title("Burn vasopressor data profile"),
      htmltools::tags$style(htmltools::HTML(report_css))
    ),
    htmltools::tags$body(body)
  )
  html <- paste0("<!DOCTYPE html>\n", as.character(doc))
  writeLines(report_plain(html), path, useBytes = TRUE)
  invisible(path)
}
# nolint end
