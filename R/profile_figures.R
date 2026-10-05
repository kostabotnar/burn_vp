fig_colours <- list(
  surface = "#fcfcfb", ink = "#0b0b0b", ink_secondary = "#52514e", muted = "#898781",
  grid = "#e1e0d9", axis = "#c3c2b7"
)
# Sequential (blue, light to dark) and diverging (blue to red through a neutral grey) scales.
fig_sequential <- c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")
fig_diverging <- c(low = "#2a78d6", mid = "#f0efec", high = "#e34948")
fig_line_width <- 0.7
fig_point_size <- 2.5

site_identity <- c(NBC = "#2a78d6", UTMB = "#eb6834")
site_extra <- c("#1baf7a", "#eda100")

# Colours follow the site, never its rank or position in the data.
site_colours <- function(sites) {
  sites <- unique(as.character(sites))
  out <- stats::setNames(rep(NA_character_, length(sites)), sites)
  known <- sites %in% names(site_identity)
  out[known] <- site_identity[sites[known]]
  others <- sites[!known]
  if (length(others) > length(site_extra)) {
    stop("At most four sites are supported (NBC, UTMB and two more).", call. = FALSE)
  }
  out[others] <- site_extra[seq_along(others)]
  out
}

theme_burn <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = fig_colours$ink),
      plot.background = ggplot2::element_rect(fill = fig_colours$surface, colour = NA),
      panel.background = ggplot2::element_rect(fill = fig_colours$surface, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = fig_colours$grid, linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = fig_colours$axis, linewidth = 0.3),
      axis.ticks = ggplot2::element_line(colour = fig_colours$axis, linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = fig_colours$muted),
      axis.title = ggplot2::element_text(colour = fig_colours$ink_secondary),
      strip.text = ggplot2::element_text(colour = fig_colours$ink, hjust = 0, face = "plain"),
      plot.title = ggplot2::element_text(colour = fig_colours$ink, face = "plain",
                                         size = base_size + 3, hjust = 0),
      plot.subtitle = ggplot2::element_text(colour = fig_colours$ink_secondary, hjust = 0),
      plot.caption = ggplot2::element_text(colour = fig_colours$ink_secondary, hjust = 0),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(colour = fig_colours$ink_secondary),
      legend.text = ggplot2::element_text(colour = fig_colours$ink_secondary),
      legend.key = ggplot2::element_rect(fill = fig_colours$surface, colour = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

site_scale <- function(sites, aesthetic = "colour") {
  cols <- site_colours(sites)
  if (aesthetic == "fill") {
    ggplot2::scale_fill_manual(values = cols, name = "Site", drop = FALSE)
  } else {
    ggplot2::scale_colour_manual(values = cols, name = "Site", drop = FALSE)
  }
}

# Sites keep the order of first appearance so the legend matches the data.
site_factor <- function(site) factor(as.character(site), levels = unique(as.character(site)))

long_variables <- function(data, vars, labels) {
  parts <- lapply(vars, function(v) {
    data.frame(site = site_factor(data$site), variable = labels[[v]],
               value = as.numeric(data[[v]]), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, parts)
  out$variable <- factor(out$variable, levels = unname(unlist(labels[vars])))
  out[!is.na(out$value), , drop = FALSE]
}

box_with_points <- function(df, x, y) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = site)) +
    ggplot2::geom_boxplot(
      fill = fig_colours$surface, outlier.shape = NA, linewidth = fig_line_width, width = 0.5,
      show.legend = TRUE, key_glyph = "point"
    ) +
    ggplot2::geom_jitter(
      width = 0.14, height = 0, size = fig_point_size, alpha = 0.55, stroke = 0
    )
}

# Small multiples, one panel per continuous variable, one box per site. Peak NE rate and NE
# delay are shown for patients who received NE (others have no value by design).
fig_distributions <- function(data) {
  d <- if ("peak_ne_48h" %in% names(data)) data else derive_analysis_variables(data)
  d$peak_ne_48h[!d$any_ne_48h %in% "Yes"] <- NA
  vars <- c("tbsa_burned_pct", "age_years", "crystalloids_24h_ml_per_kg",
            "crystalloids_48h_ml_per_kg", "peak_ne_48h", "ne_delay_h", "lowest_map_48h_mmhg",
            "highest_pulse_48h_bpm")
  vars <- vars[vars %in% names(d)]
  labels <- as.list(vapply(vars, variable_label, character(1)))
  names(labels) <- vars
  labels$peak_ne_48h <- "Peak NE rate 0-48 h (ug/kg/min), NE-treated"
  df <- long_variables(d, vars, labels)
  box_with_points(df, "site", "value") +
    ggplot2::facet_wrap(~variable, scales = "free_y", nrow = 2,
                        labeller = ggplot2::label_wrap_gen(30)) +
    site_scale(df$site) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3.5, alpha = 1))) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = "Distributions of key variables by site",
                  subtitle = "Box: median and quartiles. Points: individual patients.") +
    theme_burn() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = fig_colours$ink_secondary))
}

# Crystalloids in the first 24 h per kg against burn size, with the Parkland formula as a
# reference (4 mL per kg per %TBSA is a line through the origin with slope 4).
fig_fluid_vs_tbsa <- function(data) {
  d <- if ("crystalloids_24h_ml_per_kg" %in% names(data)) data else {
    derive_analysis_variables(data)
  }
  df <- data.frame(site = site_factor(d$site), tbsa = d$tbsa_burned_pct,
                   fluid = d$crystalloids_24h_ml_per_kg)
  df <- df[stats::complete.cases(df), , drop = FALSE]
  x_max <- max(df$tbsa, 1)
  y_max <- max(df$fluid, 4 * x_max)
  ggplot2::ggplot(df, ggplot2::aes(x = tbsa, y = fluid, colour = site)) +
    ggplot2::geom_abline(slope = 4, intercept = 0, colour = fig_colours$ink_secondary,
                         linewidth = fig_line_width) +
    ggplot2::geom_point(size = fig_point_size + 0.5, alpha = 0.7, stroke = 0) +
    ggplot2::annotate("text", x = min(df$tbsa), y = y_max * 1.03, hjust = 0, vjust = 1,
                      label = "Line: Parkland volume, 4 mL/kg/%TBSA",
                      colour = fig_colours$ink_secondary, size = 3.4) +
    site_scale(df$site) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.04))) +
    ggplot2::coord_cartesian(ylim = c(0, y_max * 1.05)) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3.5, alpha = 1))) +
    ggplot2::labs(x = "TBSA burned (%)", y = "Crystalloids 0-24 h (mL/kg)",
                  title = "Crystalloids in the first 24 h against burn size",
                  subtitle = "Points above the line received more than the Parkland volume.") +
    theme_burn()
}

# Heatmap of the percentage of unexpected missing values for key variables. Variables that
# are blank by design (for example NE dose when no NE was given) do not count.
fig_missingness <- function(missingness, max_variables = 40) {
  m <- missingness[missingness$key_variable %in% TRUE, , drop = FALSE]
  if (nrow(m) == 0) stop("No key variables in 'missingness'.", call. = FALSE)
  worst <- tapply(m$pct_unexpected_missing, m$variable, max, na.rm = TRUE)
  ordered <- names(sort(worst, decreasing = TRUE))
  shown <- utils::head(ordered, max_variables)
  m <- m[m$variable %in% shown, , drop = FALSE]
  m$variable <- factor(m$variable, levels = rev(shown))
  m$site <- site_factor(m$site)
  m$label <- ifelse(round(m$pct_unexpected_missing) >= 1,
                    sprintf("%.0f", m$pct_unexpected_missing), "")
  m$dark <- m$pct_unexpected_missing >= 45
  note <- if (all(worst == 0)) "No unexpected missing values in any key variable." else {
    "Key variables ordered by the highest unexpected missing percentage."
  }
  if (length(ordered) > max_variables) {
    note <- sprintf("%s Showing %d of %d.", note, max_variables, length(ordered))
  }
  note <- paste(strwrap(note, 62), collapse = "\n")
  ggplot2::ggplot(m, ggplot2::aes(x = site, y = variable, fill = pct_unexpected_missing)) +
    ggplot2::geom_tile(colour = fig_colours$surface, linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = label, colour = dark), size = 2.8,
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = fig_colours$surface,
                                            `FALSE` = fig_colours$ink)) +
    ggplot2::scale_fill_gradientn(colours = fig_sequential, limits = c(0, 100),
                                  name = "Unexpected missing (%)") +
    ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 12, barheight = 0.6)) +
    ggplot2::labs(x = NULL, y = NULL, title = "Missing values in key variables",
                  subtitle = note) +
    theme_burn() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), legend.title.position = "top",
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = fig_colours$ink_secondary)
    )
}

# Hours from the burn to ICU admission, first NE and first surgery.
fig_timeline <- function(data) {
  d <- if ("burn_to_icu_h" %in% names(data)) data else derive_analysis_variables(data)
  vars <- c("burn_to_icu_h", "ne_delay_h", "first_surgery_h")
  labels <- list(burn_to_icu_h = "Burn to ICU admission (hours)",
                 ne_delay_h = "Burn to first NE (hours, NE-treated)",
                 first_surgery_h = "Burn to first surgery (hours)")
  df <- long_variables(d, vars, labels)
  box_with_points(df, "site", "value") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
    site_scale(df$site) +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3.5, alpha = 1))) +
    ggplot2::labs(x = NULL, y = "Hours", title = "Timing of care after the burn",
                  subtitle = "Box: median and quartiles. Points: individual patients.") +
    theme_burn() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = fig_colours$ink_secondary))
}

# Wilson score interval for a proportion.
wilson_ci <- function(x, n, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- ifelse(n > 0, x / n, NA_real_)
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  data.frame(p = p, lower = pmax(0, centre - half), upper = pmin(1, centre + half))
}

binary_figure_vars <- list(
  Exposure = c("any_ne_48h", "any_other_vp_48h", "any_albumin_48h"),
  Outcome = c("hospital_mortality", "rrt", "sepsis")
)

# Proportions with 95 % Wilson intervals for the binary exposures and outcomes.
fig_binary_proportions <- function(data) {
  d <- if ("any_ne_48h" %in% names(data)) data else derive_analysis_variables(data)
  rows <- list()
  for (grp in names(binary_figure_vars)) {
    for (v in binary_figure_vars[[grp]]) {
      if (!v %in% names(d)) next
      for (s in unique(as.character(d$site))) {
        x <- d[[v]][d$site == s]
        x <- x[!is.na(x)]
        ci <- wilson_ci(sum(x == "Yes"), length(x))
        rows[[length(rows) + 1]] <- cbind(
          data.frame(group = grp, variable = variable_label(v), site = s, n = length(x)), ci
        )
      }
    }
  }
  df <- do.call(rbind, rows)
  df$site <- site_factor(df$site)
  df$group <- factor(df$group, levels = names(binary_figure_vars))
  df$variable <- factor(df$variable, levels = rev(unique(df$variable)))
  df <- df[df$n > 0, , drop = FALSE]
  dodge <- ggplot2::position_dodge(width = 0.5)
  ggplot2::ggplot(df, ggplot2::aes(y = variable, x = 100 * p, colour = site)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = 100 * lower, xmax = 100 * upper),
                            linewidth = fig_line_width, position = dodge) +
    ggplot2::geom_point(size = 3, position = dodge) +
    ggplot2::facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    site_scale(df$site) +
    ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3.5))) +
    ggplot2::labs(x = "Patients (%), with 95% Wilson interval", y = NULL,
                  title = "Binary exposures and outcomes by site") +
    theme_burn() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = fig_colours$ink_secondary),
                   strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
}

# Spearman correlation heatmap (lower triangle). Coefficients with |rho| of at least 0.3 are
# written in the cells; every coefficient is in profile/correlation.csv.
fig_correlation <- function(r) {
  vars <- rownames(r)
  labels <- vapply(vars, variable_label, character(1))
  idx <- which(row(r) > col(r), arr.ind = TRUE)
  df <- data.frame(row = factor(labels[idx[, 1]], levels = rev(labels)),
                   col = factor(labels[idx[, 2]], levels = labels), rho = r[idx])
  df$label <- ifelse(!is.na(df$rho) & abs(df$rho) >= 0.3, sprintf("%.2f", df$rho), "")
  df$dark <- !is.na(df$rho) & abs(df$rho) >= 0.7
  ggplot2::ggplot(df, ggplot2::aes(x = col, y = row, fill = rho)) +
    ggplot2::geom_tile(colour = fig_colours$surface, linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = label, colour = dark), size = 2.8,
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = fig_colours$surface,
                                            `FALSE` = fig_colours$ink)) +
    ggplot2::scale_fill_gradient2(
      low = fig_diverging[["low"]], mid = fig_diverging[["mid"]], high = fig_diverging[["high"]],
      midpoint = 0, limits = c(-1, 1), name = "Spearman rho", na.value = fig_colours$grid
    ) +
    ggplot2::scale_x_discrete(labels = function(x) vapply(strwrap(x, 22, simplify = FALSE),
                                                          paste, character(1), collapse = "\n"),
                              expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 12, barheight = 0.6)) +
    ggplot2::labs(x = NULL, y = NULL, title = "Spearman correlations of key variables",
                  subtitle = "Pairwise complete observations, both sites together.") +
    theme_burn() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), legend.title.position = "top",
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = fig_colours$ink_secondary),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                          colour = fig_colours$ink_secondary)
    )
}

# Saves a plot as a 150 dpi PNG inside <project root>/output.
save_figure <- function(plot, path, width = 8, height = 5) {
  # qc_inside_output() lives in R/qc_rules.R, which lintr does not see from this file.
  if (!qc_inside_output(dirname(path))) { # nolint: object_usage_linter.
    stop("Figures can only be saved in an existing folder inside <project root>/output/.",
         call. = FALSE)
  }
  ggplot2::ggsave(path, plot, device = "png", width = width, height = height, dpi = 150,
                  bg = fig_colours$surface, limitsize = FALSE)
  invisible(path)
}

figure_catalogue <- function() {
  data.frame(
    id = c("distributions", "fluid_vs_tbsa", "missingness", "timeline", "binary", "correlation"),
    title = c("Distributions by site", "Crystalloids against burn size",
              "Missing values in key variables", "Timing after the burn",
              "Binary exposures and outcomes", "Correlations"),
    caption = c(
      paste("Key continuous variables by site. Each box shows the median and quartiles and",
            "each point is one patient. Peak NE rate and NE delay include only patients who",
            "received NE."),
      paste("Crystalloids given in the first 24 h per kg, against TBSA. The line marks the",
            "Parkland volume of 4 mL/kg/%TBSA."),
      paste("Percentage of unexpected missing values in key variables, by site. Values that",
            "are blank by design are not counted."),
      paste("Hours from the burn to ICU admission, to the first NE and to the first surgery,",
            "by site."),
      paste("Proportion of patients with each binary exposure or outcome, by site, with 95%",
            "Wilson confidence intervals."),
      paste("Spearman correlations between key continuous exposures and covariates, using",
            "pairwise complete observations.")
    ),
    alt = c(
      "Small multiples of box plots with points comparing key variables between sites.",
      "Scatter plot of crystalloid volume per kg against TBSA, coloured by site, with a line for the Parkland formula.",
      "Heatmap of the percentage of unexpected missing values for each key variable and site.",
      "Box plots with points of hours from burn to ICU admission, first NE and first surgery by site.",
      "Dot and interval chart of proportions with confidence intervals for binary exposures and outcomes by site.",
      "Heatmap of Spearman correlation coefficients between key variables."
    ),
    width = c(11, 8, 6.5, 8, 8, 9), height = c(6, 5.5, 8.5, 6.5, 5, 7.5),
    stringsAsFactors = FALSE
  )
}

# Builds every figure. Returns a named list of ggplot objects.
build_profile_figures <- function(data, missingness, correlation) {
  list(
    distributions = fig_distributions(data),
    fluid_vs_tbsa = fig_fluid_vs_tbsa(data),
    missingness = fig_missingness(missingness),
    timeline = fig_timeline(data),
    binary = fig_binary_proportions(data),
    correlation = fig_correlation(correlation)
  )
}

# Saves the figures as figures/<id>.png and returns the catalogue with a `path` column.
save_profile_figures <- function(figures, dir) {
  cat_df <- figure_catalogue()
  cat_df$path <- file.path(dir, paste0(cat_df$id, ".png"))
  for (i in seq_len(nrow(cat_df))) {
    save_figure(figures[[cat_df$id[i]]], cat_df$path[i], cat_df$width[i], cat_df$height[i])
  }
  cat_df
}
