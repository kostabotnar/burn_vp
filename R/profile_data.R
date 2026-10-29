profile_masked_categories <- c("range", "type", "category")

# Position of each row within its own site, so findings stay meaningful per site file.
site_row_index <- function(site) {
  idx <- integer(length(site))
  for (s in unique(site)) idx[site == s] <- seq_len(sum(site == s))
  idx
}

# run_qc() numbers rows across the bound data. This converts finding rows to per-site rows.
localise_finding_rows <- function(findings, data) {
  idx <- site_row_index(as.character(data$site))
  findings$row <- ifelse(is.na(findings$row), NA_integer_, idx[findings$row])
  findings
}

# Sets to NA every value that failed a range, type or category rule. `findings` must carry
# per-site rows (see localise_finding_rows). The attribute "masked_counts" lists the number of
# masked values per site and variable.
mask_flagged_values <- function(data, findings) {
  flagged <- findings[findings$category %in% profile_masked_categories & !is.na(findings$row),
                      , drop = FALSE]
  counts <- data.frame(site = character(), variable = character(), n_masked = integer(),
                       stringsAsFactors = FALSE)
  if (nrow(flagged) > 0) {
    site_of_row <- as.character(data$site)
    key <- paste(site_of_row, site_row_index(site_of_row), sep = "\r")
    cells <- list()
    for (i in seq_len(nrow(flagged))) {
      cols <- strsplit(flagged$columns[i], "; ", fixed = TRUE)[[1]]
      for (col in intersect(cols, names(data))) {
        cells[[length(cells) + 1]] <- data.frame(
          key = paste(flagged$site[i], flagged$row[i], sep = "\r"), column = col,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(cells) > 0) {
      cells <- unique(do.call(rbind, cells))
      for (col in unique(cells$column)) {
        hit <- key %in% cells$key[cells$column == col] & !is.na(data[[col]])
        if (any(hit)) {
          data[[col]][hit] <- NA
          counts <- rbind(counts, data.frame(
            site = names(table(site_of_row[hit])), variable = col,
            n_masked = as.integer(table(site_of_row[hit])), stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  rownames(counts) <- NULL
  attr(data, "masked_counts") <- counts
  data
}

masked_n <- function(counts, site, variable) {
  if (is.null(counts)) return(0L)
  hit <- counts$site == site & counts$variable == variable
  if (any(hit)) sum(counts$n_masked[hit]) else 0L
}

numeric_stats <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(c(n = 0, mean = NA, sd = NA, median = NA, q1 = NA, q3 = NA, min = NA, max = NA))
  }
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
  c(n = length(x), mean = mean(x), sd = if (length(x) > 1) stats::sd(x) else NA,
    median = q[2], q1 = q[1], q3 = q[3], min = min(x), max = max(x))
}

# Counts per level, with a "(missing)" row. A comorbidity list cell can hold several levels, so
# its levels are counted per patient.
level_counts <- function(x, levels, split_list) {
  total <- length(x)
  if (split_list) {
    items <- strsplit(ifelse(is.na(x), "", x), ", ", fixed = TRUE)
    counts <- vapply(levels, function(l) sum(vapply(items, function(i) l %in% i, logical(1))),
                     numeric(1))
    extra <- setdiff(unique(unlist(items)), c(levels, ""))
    for (e in extra) counts[e] <- sum(vapply(items, function(i) e %in% i, logical(1)))
  } else {
    x <- as.character(x)
    seen <- unique(x[!is.na(x)])
    all_levels <- c(levels, setdiff(seen, levels))
    counts <- vapply(all_levels, function(l) sum(x %in% l), numeric(1))
  }
  counts <- c(counts, "(missing)" = sum(is.na(x)))
  data.frame(level = names(counts), n = as.integer(counts), pct = 100 * counts / total,
             stringsAsFactors = FALSE, row.names = NULL)
}

# Returns list(numeric, categorical). Numeric: one row per variable and site. Categorical:
# one row per variable, site and level (binary and categorical), including missing.
profile_variables <- function(data, codebook) {
  if (!"site" %in% names(data)) stop("'data' needs a 'site' column.", call. = FALSE)
  counts <- attr(data, "masked_counts")
  site <- as.character(data$site)
  sites <- unique(site)
  num_rows <- list()
  cat_rows <- list()
  for (i in seq_len(nrow(codebook))) {
    nm <- codebook$name[i]
    type <- codebook$type[i]
    if (!nm %in% names(data) || !type %in% c("numeric", "integer", "binary", "categorical")) next
    for (s in sites) {
      x <- data[[nm]][site == s]
      if (type %in% c("numeric", "integer")) {
        st <- numeric_stats(as.numeric(x))
        num_rows[[length(num_rows) + 1]] <- data.frame(
          variable = nm, site = s, n = st[["n"]], missing = sum(is.na(x)),
          n_masked = masked_n(counts, s, nm), mean = st[["mean"]], sd = st[["sd"]],
          median = st[["median"]], q1 = st[["q1"]], q3 = st[["q3"]], min = st[["min"]],
          max = st[["max"]], stringsAsFactors = FALSE
        )
      } else {
        levels <- strsplit(codebook$allowed_values[i], "|", fixed = TRUE)[[1]]
        lc <- level_counts(x, levels, split_list = nm == "comorbidities_list")
        cat_rows[[length(cat_rows) + 1]] <- cbind(variable = nm, site = s, lc,
                                                  stringsAsFactors = FALSE)
      }
    }
  }
  bind <- function(rows, empty) if (length(rows)) do.call(rbind, rows) else empty
  numeric_df <- bind(num_rows, data.frame(
    variable = character(), site = character(), n = numeric(), missing = integer(),
    n_masked = integer(), mean = numeric(), sd = numeric(), median = numeric(), q1 = numeric(),
    q3 = numeric(), min = numeric(), max = numeric()
  ))
  cat_df <- bind(cat_rows, data.frame(variable = character(), site = character(),
                                      level = character(), n = integer(), pct = numeric()))
  rownames(numeric_df) <- NULL
  rownames(cat_df) <- NULL
  list(numeric = numeric_df, categorical = cat_df)
}

is_yes <- function(x) {
  out <- rep(NA, length(x))
  out[x %in% "Yes"] <- TRUE
  out[x %in% "No"] <- FALSE
  out
}

yes_no <- function(x) ifelse(is.na(x), NA_character_, ifelse(x, "Yes", "No"))

col_or_na <- function(data, col, na = NA_real_) {
  if (col %in% names(data)) data[[col]] else rep(na, nrow(data))
}

clock_minutes <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(sub(":.*", "", x[ok])) * 60 + as.numeric(sub(".*:", "", x[ok]))
  out
}

# Divides, returning NA when the denominator is missing or not positive.
safe_ratio <- function(num, den) ifelse(!is.na(den) & den > 0, num / den, NA_real_)

# NA is read as 0 only where the matching "Yes/No" flag says No (the dose is blank by design).
rate_if_flag <- function(rate, flag) {
  ifelse(is.na(rate) & flag %in% "No", 0, rate)
}

# Fluid columns for 24h and 48h are treated as cumulative from the burn. Albumin is not part of
# crystalloids or total fluids. Blank "other vasopressor" columns mean none was given.
# Adds columns and leaves every existing column unchanged.
derive_analysis_variables <- function(data) {
  weight <- as.numeric(col_or_na(data, "admission_weight_kg"))
  tbsa <- as.numeric(col_or_na(data, "tbsa_burned_pct"))
  for (w in c("24h", "48h")) {
    for (kind in c("crystalloids", "total_fluids")) {
      ml <- as.numeric(col_or_na(data, sprintf("%s_%s_ml", kind, w)))
      per_kg <- safe_ratio(ml, weight)
      data[[sprintf("%s_%s_ml_per_kg", kind, w)]] <- per_kg
      data[[sprintf("%s_%s_ml_per_kg_tbsa", kind, w)]] <- safe_ratio(per_kg, tbsa)
    }
  }
  data$parkland_24h_ml <- ifelse(!is.na(weight) & !is.na(tbsa) & weight > 0 & tbsa > 0,
                                 4 * weight * tbsa, NA_real_)
  data$crystalloids_24h_parkland_ratio <-
    safe_ratio(as.numeric(col_or_na(data, "crystalloids_24h_ml")), data$parkland_24h_ml)

  ne24 <- col_or_na(data, "ne_24h", NA_character_)
  ne2448 <- col_or_na(data, "ne_24_48h", NA_character_)
  ne48 <- col_or_na(data, "ne_48h", NA_character_)
  any_yes <- is_yes(ne24) %in% TRUE | is_yes(ne2448) %in% TRUE | is_yes(ne48) %in% TRUE
  all_no <- ne48 %in% "No" | (ne24 %in% "No" & ne2448 %in% "No")
  data$any_ne_48h <- yes_no(ifelse(any_yes, TRUE, ifelse(all_no, FALSE, NA)))

  rate24 <- rate_if_flag(as.numeric(col_or_na(data, "peak_ne_24h_ug_kg_min")), ne24)
  rate2448 <- rate_if_flag(as.numeric(col_or_na(data, "peak_ne_24_48h_ug_kg_min")), ne2448)
  data$peak_ne_48h <- pmax(rate24, rate2448, na.rm = TRUE)

  other24 <- col_or_na(data, "other_vp_24h", NA_character_)
  other2448 <- col_or_na(data, "other_vp_24_48h", NA_character_)
  data$any_other_vp_48h <- ifelse(!is.na(other24) | !is.na(other2448), "Yes", "No")
  data$peak_other_vp_48h <- pmax(
    as.numeric(col_or_na(data, "peak_other_vp_24h_ug_kg_min")),
    as.numeric(col_or_na(data, "peak_other_vp_24_48h_ug_kg_min")), na.rm = TRUE
  )
  data$vasopressor_type <- ifelse(
    is.na(data$any_ne_48h), NA_character_,
    ifelse(data$any_ne_48h == "Yes",
           ifelse(data$any_other_vp_48h == "Yes", "NE and other", "NE only"),
           ifelse(data$any_other_vp_48h == "Yes", "Other only", "None"))
  )

  alb24 <- is_yes(col_or_na(data, "albumin_24h", NA_character_))
  alb48 <- is_yes(col_or_na(data, "albumin_48h", NA_character_))
  data$any_albumin_48h <- yes_no(ifelse(alb24 %in% TRUE | alb48 %in% TRUE, TRUE,
                                        ifelse(alb48 %in% FALSE, FALSE, NA)))

  data$lowest_map_48h_mmhg <- pmin(as.numeric(col_or_na(data, "lowest_map_24h_mmhg")),
                                   as.numeric(col_or_na(data, "lowest_map_24_48h_mmhg")),
                                   na.rm = TRUE)
  data$highest_pulse_48h_bpm <- pmax(as.numeric(col_or_na(data, "highest_pulse_24h_bpm")),
                                     as.numeric(col_or_na(data, "highest_pulse_24_48h_bpm")),
                                     na.rm = TRUE)

  icu_date <- col_or_na(data, "icu_admission_date", as.Date(NA))
  burn_date <- col_or_na(data, "burn_date", as.Date(NA))
  day_diff <- as.numeric(icu_date) - as.numeric(burn_date)
  data$burn_to_icu_h <- day_diff * 24 +
    (clock_minutes(col_or_na(data, "icu_admission_time", NA_character_)) -
       clock_minutes(col_or_na(data, "burn_time", NA_character_))) / 60
  data$first_surgery_h <- as.numeric(col_or_na(data, "first_surgery_days")) * 24
  data
}

# Pooled-SD standardized mean difference of two numeric vectors (absolute value).
smd_continuous <- function(x1, x2) {
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  if (length(x1) < 2 || length(x2) < 2) return(NA_real_)
  pooled <- sqrt((stats::var(x1) + stats::var(x2)) / 2)
  if (pooled == 0) return(if (mean(x1) == mean(x2)) 0 else NA_real_)
  abs(mean(x1) - mean(x2)) / pooled
}

# Proportion SMD for a binary variable given the two proportions.
smd_binary <- function(p1, p2) {
  if (is.na(p1) || is.na(p2)) return(NA_real_)
  denom <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
  if (denom == 0) return(if (p1 == p2) 0 else NA_real_)
  abs(p1 - p2) / denom
}

# Multi-level SMD of Yang and Dalton (2012): sqrt(t' S^-1 t), where t holds the differences in
# proportion for all but one level and S is the average of the two multinomial covariance
# matrices. `p1` and `p2` are full proportion vectors over the same levels.
smd_categorical <- function(p1, p2) {
  keep <- p1 > 0 | p2 > 0
  p1 <- p1[keep]
  p2 <- p2[keep]
  if (length(p1) < 2) return(0)
  if (length(p1) == 2) return(smd_binary(p1[1], p2[1]))
  p1 <- p1[-length(p1)]
  p2 <- p2[-length(p2)]
  cov_matrix <- function(p) diag(p, length(p)) - outer(p, p)
  s <- (cov_matrix(p1) + cov_matrix(p2)) / 2
  d <- p1 - p2
  val <- tryCatch(drop(t(d) %*% solve(s, d)), error = function(e) NA_real_)
  if (is.na(val) || val < 0) NA_real_ else sqrt(val)
}

# With more than two sites the largest pairwise SMD is reported.
max_pairwise <- function(sites, fun) {
  if (length(sites) < 2) return(NA_real_)
  vals <- utils::combn(length(sites), 2, function(ix) fun(sites[[ix[1]]], sites[[ix[2]]]))
  if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
}

variable_kind <- function(x) {
  if (is.numeric(x)) return("continuous")
  vals <- unique(x[!is.na(x)])
  if (length(vals) > 0 && all(vals %in% c("Yes", "No"))) "binary" else "categorical"
}

fmt_num <- function(x, digits = 1) formatC(x, format = "f", digits = digits)

fmt_median_iqr <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("no data")
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  sprintf("%s [%s, %s]", fmt_num(q[2], digits), fmt_num(q[1], digits), fmt_num(q[3], digits))
}

fmt_n_pct <- function(n, total) {
  if (total == 0) return("no data")
  sprintf("%d (%s%%)", as.integer(n), fmt_num(100 * n / total, 1))
}

# Table 1 by site. `vars` are names of columns in `data`, from the codebook or derived. Rows:
# one per continuous variable (median [IQR]), one per level for categorical variables and one
# row ("Yes") for binary variables (n (%) of non-missing). `smd` appears on the first row of
# each variable and is NA when there is only one site. Percentages exclude missing values;
# the `missing` column gives the number of missing values per site.
site_comparison <- function(data, codebook, vars) {
  site <- as.character(data$site)
  sites <- unique(site)
  vars <- vars[vars %in% names(data)]
  rows <- list()
  for (v in vars) {
    x <- data[[v]]
    kind <- variable_kind(x)
    label <- variable_label(v, codebook)
    by_site <- lapply(sites, function(s) x[site == s])
    names(by_site) <- sites
    miss <- paste(vapply(sites, function(s) sum(is.na(by_site[[s]])), numeric(1)),
                  collapse = " / ")
    cell <- function(fun) vapply(sites, fun, character(1))
    if (kind == "continuous") {
      smd <- max_pairwise(unname(by_site), smd_continuous)
      row <- data.frame(variable = label, level = "", statistic = "median [IQR]",
                        t(cell(function(s) fmt_median_iqr(by_site[[s]]))), missing = miss,
                        smd = smd, stringsAsFactors = FALSE)
      rows[[length(rows) + 1]] <- row
    } else {
      levels <- if (kind == "binary") {
        "Yes"
      } else {
        lv <- unique(as.character(x[!is.na(x)]))
        if (v %in% codebook$name) {
          allowed <- strsplit(codebook$allowed_values[codebook$name == v], "|", fixed = TRUE)[[1]]
          lv <- c(intersect(allowed, lv), setdiff(lv, allowed))
        }
        lv
      }
      prop_levels <- if (kind == "binary") c("Yes", "No") else levels
      prop <- function(vals) {
        vals <- vals[!is.na(vals)]
        vapply(prop_levels, function(l) if (length(vals)) mean(vals == l) else NA_real_,
               numeric(1))
      }
      props <- lapply(by_site, prop)
      smd <- if (kind == "binary") {
        max_pairwise(unname(props), function(a, b) smd_binary(a[["Yes"]], b[["Yes"]]))
      } else {
        max_pairwise(unname(props), smd_categorical)
      }
      for (j in seq_along(levels)) {
        l <- levels[j]
        cells <- cell(function(s) {
          vals <- by_site[[s]][!is.na(by_site[[s]])]
          fmt_n_pct(sum(vals == l), length(vals))
        })
        rows[[length(rows) + 1]] <- data.frame(
          variable = label, level = if (kind == "binary") "" else l, statistic = "n (%)",
          t(cells), missing = if (j == 1) miss else "", smd = if (j == 1) smd else NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else NULL
  if (is.null(out)) stop("None of 'vars' are columns of 'data'.", call. = FALSE)
  names(out)[4:(3 + length(sites))] <- sites
  rownames(out) <- NULL
  attr(out, "n_by_site") <- stats::setNames(as.integer(table(factor(site, sites))), sites)
  out
}

variable_labels <- c(
  age_years = "Age (years)", tbsa_burned_pct = "TBSA burned (%)",
  admission_weight_kg = "Weight (kg)", crystalloids_24h_ml_per_kg = "Crystalloids 0-24 h (mL/kg)",
  crystalloids_48h_ml_per_kg = "Crystalloids 0-48 h (mL/kg)",
  crystalloids_24h_ml_per_kg_tbsa = "Crystalloids 0-24 h (mL/kg/%TBSA)",
  crystalloids_48h_ml_per_kg_tbsa = "Crystalloids 0-48 h (mL/kg/%TBSA)",
  total_fluids_24h_ml_per_kg = "Total fluids 0-24 h (mL/kg)",
  total_fluids_48h_ml_per_kg = "Total fluids 0-48 h (mL/kg)",
  total_fluids_24h_ml_per_kg_tbsa = "Total fluids 0-24 h (mL/kg/%TBSA)",
  total_fluids_48h_ml_per_kg_tbsa = "Total fluids 0-48 h (mL/kg/%TBSA)",
  crystalloids_24h_parkland_ratio = "Crystalloids 0-24 h / Parkland volume",
  peak_ne_48h = "Peak NE rate 0-48 h (ug/kg/min)", ne_delay_h = "NE delay (h)",
  peak_other_vp_48h = "Peak other vasopressor rate 0-48 h (ug/kg/min)",
  lowest_map_48h_mmhg = "Lowest MAP 0-48 h (mmHg)",
  highest_pulse_48h_bpm = "Highest pulse 0-48 h (bpm)",
  burn_to_icu_h = "Burn to ICU admission (h)", first_surgery_h = "Burn to first surgery (h)",
  any_ne_48h = "Any NE 0-48 h", any_other_vp_48h = "Any other vasopressor 0-48 h",
  any_albumin_48h = "Any albumin 0-48 h", vasopressor_type = "Vasopressor type 0-48 h",
  hospital_mortality = "In-hospital mortality", hospital_los_days = "Hospital stay (days)",
  rrt = "Renal replacement therapy", sepsis = "Septicemia",
  diuresis_0_48h_ml_kg_h = "Diuresis 0-48 h (mL/kg/h)",
  peak_lactate_0_24h_mmol_l = "Highest lactate 0-24 h (mmol/L)",
  peak_creatinine_0_72h_umol_l = "Highest creatinine 0-72 h (umol/L)",
  gender = "Sex", inhalation_injury = "Inhalation injury", diabetes = "Diabetes",
  hypertension = "Hypertension", mech_vent = "Mechanical ventilation"
)

# Short readable label: a fixed label when known, else the codebook source name, else the name.
variable_label <- function(name, codebook = NULL) {
  if (name %in% names(variable_labels)) return(unname(variable_labels[name]))
  if (!is.null(codebook) && name %in% codebook$name) {
    return(codebook$source_name[codebook$name == name])
  }
  name
}

# Summary of one measure by site. Binary measures give n (%) of Yes; continuous give
# median [IQR]. Rows with `keep` FALSE are left out (for example survivors only).
measure_rows <- function(group, label, x, site, kind, keep = rep(TRUE, length(x)),
                         level = NULL) {
  sites <- unique(site)
  rows <- lapply(sites, function(s) {
    v <- x[site == s & keep]
    if (kind == "binary") {
      v <- v[!is.na(v)]
      n_yes <- if (is.null(level)) sum(v == "Yes") else sum(v == level)
      data.frame(
        group = group, measure = label, site = s, type = kind, n = length(v),
        summary = fmt_n_pct(n_yes, length(v)), count = n_yes,
        pct = if (length(v)) 100 * n_yes / length(v) else NA_real_, median = NA_real_,
        q1 = NA_real_, q3 = NA_real_, mean = NA_real_, stringsAsFactors = FALSE
      )
    } else {
      st <- numeric_stats(as.numeric(v))
      data.frame(
        group = group, measure = label, site = s, type = kind, n = st[["n"]],
        summary = fmt_median_iqr(as.numeric(v), 2), count = NA_integer_, pct = NA_real_,
        median = st[["median"]], q1 = st[["q1"]], q3 = st[["q3"]], mean = st[["mean"]],
        stringsAsFactors = FALSE
      )
    }
  })
  do.call(rbind, rows)
}

# The study exposures and outcomes by site. Rates and delays are summarised among patients
# who received the drug; hospital stay is given for all, survivors and non-survivors.
exposure_outcome_summary <- function(data) {
  d <- if ("any_ne_48h" %in% names(data)) data else derive_analysis_variables(data)
  site <- as.character(d$site)
  lab <- function(v) variable_label(v)
  cont <- function(group, v, keep = rep(TRUE, nrow(d)), suffix = "") {
    measure_rows(group, paste0(lab(v), suffix), as.numeric(d[[v]]), site, "continuous", keep)
  }
  bin <- function(group, v, level = NULL, label = lab(v)) {
    measure_rows(group, label, d[[v]], site, "binary", level = level)
  }
  on_ne <- d$any_ne_48h %in% "Yes"
  on_other <- d$any_other_vp_48h %in% "Yes"
  alive <- d$hospital_mortality %in% "No"
  dead <- d$hospital_mortality %in% "Yes"
  types <- c("None", "NE only", "NE and other", "Other only")
  rows <- c(
    list(bin("Exposure", "any_ne_48h")),
    lapply(types, function(t) {
      bin("Exposure", "vasopressor_type", t, paste0(lab("vasopressor_type"), ": ", t))
    }),
    list(
      cont("Exposure", "peak_ne_48h", on_ne, ", NE-treated"),
      cont("Exposure", "peak_other_vp_48h", on_other, ", treated"),
      cont("Exposure", "ne_delay_h", on_ne, ", NE-treated"),
      cont("Exposure", "crystalloids_24h_ml_per_kg"),
      cont("Exposure", "crystalloids_48h_ml_per_kg"),
      cont("Exposure", "crystalloids_24h_ml_per_kg_tbsa"),
      cont("Exposure", "crystalloids_24h_parkland_ratio"),
      cont("Exposure", "total_fluids_48h_ml_per_kg"),
      bin("Exposure", "any_albumin_48h"),
      bin("Outcome", "hospital_mortality"),
      cont("Outcome", "hospital_los_days", suffix = ", all patients"),
      cont("Outcome", "hospital_los_days", alive, ", survivors"),
      cont("Outcome", "hospital_los_days", dead, ", non-survivors"),
      bin("Outcome", "rrt"), bin("Outcome", "sepsis"),
      cont("Outcome", "diuresis_0_48h_ml_kg_h"),
      cont("Outcome", "peak_lactate_0_24h_mmol_l"),
      cont("Outcome", "peak_creatinine_0_72h_umol_l"),
      cont("Outcome", "lowest_map_48h_mmhg"),
      cont("Outcome", "highest_pulse_48h_bpm")
    )
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

correlation_vars <- c(
  "age_years", "tbsa_burned_pct", "admission_weight_kg", "crystalloids_24h_ml_per_kg",
  "crystalloids_48h_ml_per_kg", "peak_ne_48h", "ne_delay_h", "lowest_map_48h_mmhg",
  "highest_pulse_48h_bpm", "peak_lactate_0_24h_mmol_l", "peak_creatinine_0_72h_umol_l",
  "diuresis_0_48h_ml_kg_h", "hospital_los_days"
)

# Spearman correlations with pairwise complete observations. The attribute "n_pairs" holds the
# number of observations behind each coefficient.
correlation_matrix <- function(data, vars = correlation_vars) {
  vars <- vars[vars %in% names(data)]
  if (length(vars) < 2) stop("At least two numeric variables are needed.", call. = FALSE)
  m <- as.matrix(as.data.frame(lapply(data[vars], as.numeric)))
  r <- suppressWarnings(stats::cor(m, use = "pairwise.complete.obs", method = "spearman"))
  present <- !is.na(m)
  attr(r, "n_pairs") <- crossprod(present)
  r
}

# Writes the profile tables as CSV into a folder under <project root>/output.
write_profile_outputs <- function(tables, dir) {
  # qc_inside_output() lives in R/qc_rules.R, which lintr does not see from this file.
  if (!qc_inside_output(dir)) { # nolint: object_usage_linter.
    stop("'dir' must be an existing folder inside <project root>/output/. Nothing was written.",
         call. = FALSE)
  }
  paths <- file.path(normalizePath(dir, winslash = "/"), paste0(names(tables), ".csv"))
  for (i in seq_along(tables)) {
    tbl <- tables[[i]]
    if (is.matrix(tbl)) tbl <- data.frame(variable = rownames(tbl), tbl, check.names = FALSE)
    utils::write.csv(tbl, paths[i], row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  invisible(paths)
}
