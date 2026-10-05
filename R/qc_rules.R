qc_categories <- c(
  "structure", "type", "category", "range", "unit", "derived", "fluid", "hemodynamics",
  "vasopressor", "gating", "date", "duration", "outcome", "duplicate", "identifier"
)
qc_severities <- c("error", "warning", "info")

qc_fluid_windows <- c("pre_icu", "24h", "48h", "72h")
qc_bp_times <- c("admission", "before_ne", "24h", "48h")
qc_picco_columns <- c(
  "ci_admission_l_min_m2", "cvp_admission_mmhg", "ci_before_ne_l_min_m2", "cvp_before_ne_mmhg",
  "ci_24h_l_min_m2", "cvp_24h_mmhg", "ci_48h_l_min_m2", "cvp_48h_mmhg"
)

qc_comorbidity_flags <- c(
  hypertension = "Hypertension", diabetes = "Diabetes",
  coronary_artery_disease = "Coronary artery disease",
  chronic_kidney_disease = "Chronic kidney disease",
  dialysis_dependent_ckd = "Dialysis dependent CKD",
  congestive_heart_failure = "Congestive heart failure",
  peripheral_vascular_disease = "Peripheral vascular disease", stroke = "Stroke",
  copd = "Chronic obstructive pulmonary disease", cirrhosis = "Cirrhosis", cancer = "Cancer",
  immunosuppression = "Immunosuppression", psychiatry = "Psychiatry",
  alcohol_opioid_addiction = "Alcohol or opioid addiction"
)

# Each flag controls the columns that should be filled only when the flag is "Yes".
qc_flag_dependents <- list(
  aki_kdigo_1plus = "aki_kdigo_1plus_date", aki_kdigo_3 = "aki_kdigo_3_date",
  ards = "ards_date", mesenteric_ischemia = "mesenteric_ischemia_date",
  abdominal_compartment_syndrome = "acs_date",
  sepsis = c("septic_events_n", "sepsis_dates"), bacteremia = "bacteremia_dates",
  candidemia = "candidemia_dates",
  rrt = c("rrt_start_days", "rrt_start_date", "rrt_stop_date"),
  mech_vent = c("mech_vent_days", "intubation_date", "vent_stop_date"),
  crrt = "crrt_days", icu_readmission = "icu_readmission_date"
)

qc_event_dates <- c(
  "aki_kdigo_1plus_date", "aki_kdigo_3_date", "ards_date", "mesenteric_ischemia_date",
  "acs_date", "sepsis_dates", "bacteremia_dates", "candidemia_dates", "rrt_start_date",
  "rrt_stop_date", "intubation_date", "vent_stop_date", "last_graft_surgery_date",
  "icu_readmission_date"
)

qc_inclusion_start <- as.Date("2015-01-01")
qc_inclusion_end <- as.Date("2023-12-31")

# Column access. An absent column reads as all NA, so every rule can run on partial data and
# reports "not evaluable" instead of failing.
qc_col <- function(data, col) {
  if (col %in% names(data)) data[[col]] else rep(NA, nrow(data))
}

qc_num <- function(data, col) {
  x <- qc_col(data, col)
  if (is.numeric(x)) return(as.numeric(x))
  if (inherits(x, "Date")) return(rep(NA_real_, length(x)))
  suppressWarnings(as.numeric(as.character(x)))
}

qc_chr <- function(data, col) {
  x <- qc_col(data, col)
  x <- if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

# "Yes" is TRUE, "No" is FALSE, anything else (including unmapped values) is NA.
qc_flag <- function(data, col) {
  x <- qc_chr(data, col)
  out <- rep(NA, length(x))
  out[x %in% "Yes"] <- TRUE
  out[x %in% "No"] <- FALSE
  out
}

qc_date <- function(data, col) {
  x <- qc_col(data, col)
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  out <- as.Date(rep(NA_character_, length(x)))
  iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  out[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  out
}

qc_minutes <- function(data, col) {
  x <- qc_chr(data, col)
  ok <- !is.na(x) & grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(sub(":.*", "", x[ok])) * 60 + as.numeric(sub(".*:", "", x[ok]))
  out
}

qc_day_diff <- function(later, earlier) as.numeric(later) - as.numeric(earlier)

qc_burn_to_icu_hours <- function(data) {
  days <- qc_day_diff(qc_date(data, "icu_admission_date"), qc_date(data, "burn_date"))
  days * 24 + (qc_minutes(data, "icu_admission_time") - qc_minutes(data, "burn_time")) / 60
}

qc_nonzero <- function(x) !is.na(x) & x != 0

# TRUE when any part failed, NA when no part could be evaluated, FALSE otherwise.
qc_any_fail <- function(...) {
  parts <- list(...)
  any_true <- Reduce(`|`, lapply(parts, function(p) p %in% TRUE))
  all_na <- Reduce(`&`, lapply(parts, is.na))
  out <- any_true
  out[!any_true & all_na] <- NA
  out
}

# Applies `fail` only where `gate` is TRUE; an unknown gate makes the row not evaluable.
qc_gate <- function(gate, fail) {
  out <- gate & fail
  out[is.na(gate)] <- NA
  out
}

# Inclusive day counting is unresolved, so both the plain difference and the plain difference
# plus 1 are accepted, each within 1 day.
qc_duration_off <- function(observed, plain) observed < plain - 1 | observed > plain + 2

qc_duration_note <- paste(
  "Accepted when the value is within 1 day of the plain date difference or of the difference",
  "plus 1, because the dictionary counts days inclusively but the template example does not."
)

qc_rule <- function(id, category, severity, description, columns, check, detail = NULL,
                    level = "row") {
  if (!category %in% qc_categories) stop("Unknown rule category: ", category, call. = FALSE)
  if (!severity %in% qc_severities) stop("Unknown rule severity: ", severity, call. = FALSE)
  list(id = id, category = category, severity = severity, description = description,
       columns = columns, check = check, detail = detail, level = level)
}

qc_position_id <- function(prefix, codebook, name) {
  sprintf("%s_%03d", prefix, as.integer(codebook$position[match(name, codebook$name)]))
}

qc_levels <- function(codebook, name) {
  strsplit(codebook$allowed_values[codebook$name == name], "|", fixed = TRUE)[[1]]
}

# Reads metadata/qc_ranges.csv: one row per numeric or integer column with plausible limits.
read_qc_ranges <- function(path = file.path("metadata", "qc_ranges.csv")) {
  if (!file.exists(path)) stop("QC ranges file not found: ", path, call. = FALSE)
  r <- utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8")
  needed <- c("name", "min", "max", "source_note")
  missing_cols <- setdiff(needed, names(r))
  if (length(missing_cols) > 0) {
    stop("QC ranges file is missing columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  r$min <- suppressWarnings(as.numeric(r$min))
  r$max <- suppressWarnings(as.numeric(r$max))
  if (anyNA(r$min) || anyNA(r$max) || any(r$min > r$max)) {
    stop("QC ranges need numeric min <= max in every row.", call. = FALSE)
  }
  if (anyDuplicated(r$name) > 0) {
    stop("QC ranges list a column twice: ", paste(unique(r$name[duplicated(r$name)]),
                                                  collapse = ", "), call. = FALSE)
  }
  r[needed]
}

qc_structure_rules <- function(codebook) {
  missing_cols <- function(d) setdiff(codebook$name, names(d))
  extra_cols <- function(d) setdiff(names(d), c("site", codebook$name))
  list(
    qc_rule("STR_001", "structure", "error",
            "One or more codebook columns are absent from the data.", character(),
            function(d) rep(length(missing_cols(d)) > 0, nrow(d)),
            detail = function(d) {
              rep(paste0("absent=", paste(missing_cols(d), collapse = ", ")), nrow(d))
            },
            level = "dataset"),
    qc_rule("STR_002", "structure", "info",
            "The data has columns that are not in the codebook.", character(),
            function(d) rep(length(extra_cols(d)) > 0, nrow(d)),
            detail = function(d) {
              rep(paste0("extra=", paste(extra_cols(d), collapse = ", ")), nrow(d))
            },
            level = "dataset")
  )
}

qc_type_check <- function(name, type) {
  force(name)
  force(type)
  function(d) {
    x <- qc_col(d, name)
    present <- !is.na(x)
    if (is.character(x)) present <- present & nzchar(trimws(x))
    bad <- switch(type,
      numeric = ,
      integer = {
        v <- qc_num(d, name)
        out <- !is.finite(v)
        if (type == "integer") out <- out | (is.finite(v) & v != round(v))
        out
      },
      date = is.na(qc_date(d, name)),
      time = is.na(qc_minutes(d, name))
    )
    out <- bad
    out[!present] <- NA
    out
  }
}

qc_type_rules <- function(codebook) {
  rows <- which(codebook$type %in% c("numeric", "integer", "date", "time"))
  lapply(rows, function(i) {
    nm <- codebook$name[i]
    tp <- codebook$type[i]
    expected <- switch(tp, numeric = "a number", integer = "a whole number",
                       date = "a date", time = "a clock time HH:MM")
    qc_rule(qc_position_id("TYP", codebook, nm), "type", "error",
            sprintf("%s is not %s.", nm, expected), nm, qc_type_check(nm, tp))
  })
}

qc_category_rules <- function(codebook) {
  rows <- which(codebook$type %in% c("binary", "categorical"))
  lapply(rows, function(i) {
    nm <- codebook$name[i]
    allowed <- qc_levels(codebook, nm)
    split_list <- nm == "comorbidities_list"
    qc_rule(qc_position_id("CAT", codebook, nm), "category", "error",
            sprintf("%s holds a value outside the allowed values (%s)%s.", nm,
                    paste(allowed, collapse = ", "),
                    if (split_list) ", after splitting the list on \", \"" else ""),
            nm, function(d) {
              x <- qc_chr(d, nm)
              out <- rep(NA, length(x))
              ok <- !is.na(x)
              out[ok] <- if (split_list) {
                vapply(strsplit(x[ok], ", ", fixed = TRUE),
                       function(items) !all(items %in% allowed), logical(1))
              } else {
                !x[ok] %in% allowed
              }
              out
            })
  })
}

qc_range_rules <- function(codebook, ranges) {
  unknown <- setdiff(ranges$name, codebook$name)
  if (length(unknown) > 0) {
    warning("QC ranges name columns that are not in the codebook: ",
            paste(unknown, collapse = ", "), call. = FALSE)
  }
  rows <- which(ranges$name %in% codebook$name)
  lapply(rows, function(i) {
    nm <- ranges$name[i]
    lo <- ranges$min[i]
    hi <- ranges$max[i]
    qc_rule(qc_position_id("RNG", codebook, nm), "range", "error",
            sprintf("%s is outside the plausible range %s to %s.", nm, format(lo), format(hi)),
            nm, function(d) {
              x <- qc_num(d, nm)
              x < lo | x > hi
            })
  })
}

qc_unit_rules <- function() {
  list(
    qc_rule("UNI_001", "unit", "warning",
            "Hematocrit is 1 or lower, so it was probably entered as a fraction, not percent.",
            "hematocrit_0_24h_pct", function(d) qc_num(d, "hematocrit_0_24h_pct") <= 1),
    qc_rule("UNI_002", "unit", "warning",
            "Peak creatinine is below 15, so it was probably entered in mg/dL, not umol/L.",
            "peak_creatinine_0_72h_umol_l",
            function(d) qc_num(d, "peak_creatinine_0_72h_umol_l") < 15),
    qc_rule("UNI_003", "unit", "warning",
            paste("Weight or height looks like the wrong unit: height below 3 suggests metres,",
                  "and weight above 150 that gives a BMI of 15 to 40 when read as pounds",
                  "suggests pounds."),
            c("admission_weight_kg", "height_cm"), function(d) {
              w <- qc_num(d, "admission_weight_kg")
              h <- qc_num(d, "height_cm")
              bmi_lb <- (w * 0.4536) / (h / 100)^2
              h < 3 | (w > 150 & bmi_lb >= 15 & bmi_lb <= 40)
            }),
    qc_rule("UNI_004", "unit", "warning",
            "A peak norepinephrine rate is above 5 ug/kg/min, which suggests a wrong unit.",
            c("peak_ne_24h_ug_kg_min", "peak_ne_24_48h_ug_kg_min"),
            function(d) {
              qc_any_fail(qc_num(d, "peak_ne_24h_ug_kg_min") > 5,
                          qc_num(d, "peak_ne_24_48h_ug_kg_min") > 5)
            })
  )
}

qc_baux_parts <- function(d) {
  plain <- qc_num(d, "age_years") + qc_num(d, "tbsa_burned_pct")
  baux <- qc_num(d, "baux_admission")
  list(off_plain = abs(baux - plain) > 1, off_revised = abs(baux - (plain + 17)) > 1,
       inhalation = qc_flag(d, "inhalation_injury"))
}

qc_comorbidity_mismatch <- function(d) {
  listed <- qc_chr(d, "comorbidities_list")
  items <- strsplit(ifelse(is.na(listed), "", listed), ", ", fixed = TRUE)
  parts <- lapply(names(qc_comorbidity_flags), function(f) {
    in_list <- vapply(items, function(l) qc_comorbidity_flags[[f]] %in% l, logical(1))
    qc_flag(d, f) != in_list
  })
  if (nrow(d) == 0) return(logical())
  do.call(qc_any_fail, parts)
}

qc_derived_rules <- function() {
  list(
    qc_rule("DER_001", "derived", "error",
            "BMI differs from weight / (height / 100)^2 by more than 0.5.",
            c("bmi_kg_m2", "admission_weight_kg", "height_cm"), function(d) {
              expected <- qc_num(d, "admission_weight_kg") / (qc_num(d, "height_cm") / 100)^2
              abs(qc_num(d, "bmi_kg_m2") - expected) > 0.5
            }),
    qc_rule("DER_002", "derived", "error",
            paste("Predicted prehospital 24h fluid differs from 4 x weight x prehospital TBSA",
                  "by more than 1 %."),
            c("predicted_fluid_24h_prehospital_ml", "admission_weight_kg",
              "tbsa_prehospital_pct"), function(d) {
              expected <- 4 * qc_num(d, "admission_weight_kg") *
                qc_num(d, "tbsa_prehospital_pct")
              abs(qc_num(d, "predicted_fluid_24h_prehospital_ml") - expected) >
                0.01 * pmax(abs(expected), 1)
            }),
    qc_rule("DER_003", "derived", "error",
            paste("Baux differs from age + TBSA by more than 1, and is not the revised Baux",
                  "(+17 with inhalation injury) either."),
            c("baux_admission", "age_years", "tbsa_burned_pct", "inhalation_injury"),
            function(d) {
              p <- qc_baux_parts(d)
              p$off_plain & !(p$inhalation & !p$off_revised)
            }),
    qc_rule("DER_004", "derived", "info",
            "Baux equals the revised Baux (age + TBSA + 17 for inhalation injury), not age + TBSA.",
            c("baux_admission", "age_years", "tbsa_burned_pct", "inhalation_injury"),
            function(d) {
              p <- qc_baux_parts(d)
              p$inhalation & p$off_plain & !p$off_revised
            }),
    qc_rule("DER_005", "derived", "error",
            "The comorbidity list does not agree with the comorbidity Yes/No columns.",
            c("comorbidities_list", names(qc_comorbidity_flags)), qc_comorbidity_mismatch),
    qc_rule("DER_006", "derived", "error",
            "Dialysis dependent CKD is Yes but chronic kidney disease is not Yes.",
            c("dialysis_dependent_ckd", "chronic_kidney_disease"),
            function(d) {
              qc_flag(d, "dialysis_dependent_ckd") & !qc_flag(d, "chronic_kidney_disease")
            })
  )
}

qc_fluid_rules <- function() {
  ml <- function(f, w) paste0(f, "_", w, "_ml")
  rules <- list()
  for (k in seq_along(qc_fluid_windows)) {
    w <- qc_fluid_windows[k]
    parts <- ml(c("ra", "nacl", "plasmalyte"), w)
    rules[[length(rules) + 1]] <- local({
      parts <- parts
      total <- ml("crystalloids", w)
      qc_rule(sprintf("FLU_%03d", k), "fluid", "error",
              sprintf("Crystalloids %s differ from RA + NaCl + Plasmalyte by more than 1 mL.", w),
              c(total, parts), function(d) {
                abs(qc_num(d, total) - Reduce(`+`, lapply(parts, qc_num, data = d))) > 1
              })
    })
  }
  for (k in seq_along(qc_fluid_windows)) {
    w <- qc_fluid_windows[k]
    parts <- ml(c("crystalloids", "glucose5", "glucose10", "glucose20"), w)
    if (w == "pre_icu") parts <- c(parts, "glucose_unknown_pre_icu_ml")
    rules[[length(rules) + 1]] <- local({
      parts <- parts
      total <- ml("total_fluids", w)
      qc_rule(sprintf("FLU_%03d", 4 + k), "fluid", "error",
              sprintf("Total fluids %s differ from crystalloids + glucose by more than 1 mL.", w),
              c(total, parts), function(d) {
                abs(qc_num(d, total) - Reduce(`+`, lapply(parts, qc_num, data = d))) > 1
              })
    })
  }
  rules[[length(rules) + 1]] <- qc_rule(
    "FLU_009", "fluid", "error",
    "Albumin (any) 24h differs from albumin 20% + albumin 5% 24h by more than 1 mL.",
    c("albumin_any_24h_ml", "albumin20_24h_ml", "albumin5_24h_ml"), function(d) {
      abs(qc_num(d, "albumin_any_24h_ml") -
            (qc_num(d, "albumin20_24h_ml") + qc_num(d, "albumin5_24h_ml"))) > 1
    }
  )
  series <- list(
    ra = qc_fluid_windows, nacl = qc_fluid_windows, plasmalyte = qc_fluid_windows,
    glucose5 = qc_fluid_windows, glucose10 = qc_fluid_windows, glucose20 = qc_fluid_windows,
    crystalloids = qc_fluid_windows, total_fluids = qc_fluid_windows,
    albumin20 = c("24h", "48h"), albumin5 = c("24h", "48h")
  )
  for (k in seq_along(series)) {
    rules[[length(rules) + 1]] <- local({
      cols <- ml(names(series)[k], series[[k]])
      qc_rule(sprintf("FLU_%03d", 9 + k), "fluid", "error",
              sprintf("Cumulative %s volumes decrease over time (%s), by more than 1 mL.",
                      names(series)[k], paste(series[[k]], collapse = " <= ")),
              cols, function(d) {
                v <- lapply(cols, qc_num, data = d)
                pairs <- utils::combn(length(v), 2, simplify = FALSE)
                do.call(qc_any_fail, lapply(pairs, function(p) v[[p[2]]] < v[[p[1]]] - 1))
              })
    })
  }
  rules[[length(rules) + 1]] <- qc_rule(
    "FLU_020", "fluid", "warning",
    "Crystalloids 24h per kg per % TBSA is outside 1 to 10 mL.",
    c("crystalloids_24h_ml", "admission_weight_kg", "tbsa_burned_pct"), function(d) {
      ratio <- qc_num(d, "crystalloids_24h_ml") / qc_num(d, "admission_weight_kg") /
        qc_num(d, "tbsa_burned_pct")
      ratio < 1 | ratio > 10
    }
  )
  rules
}

# Values recorded at admission or before NE count for the 0-24h window only when they were
# taken within 24 h of the burn; when the timing is unknown they are kept.
qc_window_24h_parts <- function(d, extreme, prefix, suffix, cmp) {
  icu_in_24h <- !((qc_burn_to_icu_hours(d) > 24) %in% TRUE)
  ne_in_24h <- !((qc_num(d, "ne_delay_h") >= 24) %in% TRUE)
  keep <- list(icu_in_24h, ne_in_24h, rep(TRUE, nrow(d)))
  times <- c("admission", "before_ne", "24h")
  parts <- lapply(seq_along(times), function(i) {
    out <- cmp(extreme, qc_num(d, paste0(prefix, times[i], suffix)))
    out[!keep[[i]]] <- NA
    out
  })
  do.call(qc_any_fail, parts)
}

qc_hemodynamic_rules <- function() {
  rules <- list()
  for (k in seq_along(qc_bp_times)) {
    rules[[length(rules) + 1]] <- local({
      t <- qc_bp_times[k]
      cols <- paste0(c("sap_", "map_", "dap_"), t, "_mmhg")
      qc_rule(sprintf("HEM_%03d", k), "hemodynamics", "error",
              sprintf("Blood pressure %s is not ordered SAP > MAP > DAP.", t), cols,
              function(d) {
                sap <- qc_num(d, cols[1])
                map <- qc_num(d, cols[2])
                dap <- qc_num(d, cols[3])
                !(sap > map & map > dap)
              })
    })
  }
  for (k in seq_along(qc_bp_times)) {
    rules[[length(rules) + 1]] <- local({
      t <- qc_bp_times[k]
      cols <- paste0(c("sap_", "map_", "dap_"), t, "_mmhg")
      qc_rule(sprintf("HEM_%03d", 4 + k), "hemodynamics", "warning",
              sprintf("MAP %s differs from (SAP + 2 x DAP) / 3 by more than 10 mmHg.", t), cols,
              function(d) {
                expected <- (qc_num(d, cols[1]) + 2 * qc_num(d, cols[3])) / 3
                abs(qc_num(d, cols[2]) - expected) > 10
              })
    })
  }
  above <- function(a, b) a > b
  below <- function(a, b) a < b
  c(rules, list(
    qc_rule("HEM_009", "hemodynamics", "error",
            paste("Lowest MAP 0-24h is above a MAP recorded in that window (admission and",
                  "before NE when within 24 h of the burn, and MAP 24h)."),
            c("lowest_map_24h_mmhg", "map_admission_mmhg", "map_before_ne_mmhg",
              "map_24h_mmhg"),
            function(d) {
              qc_window_24h_parts(d, qc_num(d, "lowest_map_24h_mmhg"), "map_", "_mmhg", above)
            }),
    qc_rule("HEM_010", "hemodynamics", "error",
            "Lowest MAP 24-48h is above MAP 48h.",
            c("lowest_map_24_48h_mmhg", "map_48h_mmhg"),
            function(d) qc_num(d, "lowest_map_24_48h_mmhg") > qc_num(d, "map_48h_mmhg")),
    qc_rule("HEM_011", "hemodynamics", "error",
            paste("Highest pulse 0-24h is below a heart rate recorded in that window (admission",
                  "and before NE when within 24 h of the burn, and HR 24h)."),
            c("highest_pulse_24h_bpm", "hr_admission_bpm", "hr_before_ne_bpm", "hr_24h_bpm"),
            function(d) {
              qc_window_24h_parts(d, qc_num(d, "highest_pulse_24h_bpm"), "hr_", "_bpm", below)
            }),
    qc_rule("HEM_012", "hemodynamics", "error",
            "Highest pulse 24-48h is below HR 48h.",
            c("highest_pulse_24_48h_bpm", "hr_48h_bpm"),
            function(d) qc_num(d, "highest_pulse_24_48h_bpm") < qc_num(d, "hr_48h_bpm")),
    qc_rule("HEM_013", "hemodynamics", "warning",
            "CI or CVP is recorded although PiCCO use 0-48h is No.",
            c("picco_use_0_48h", qc_picco_columns), function(d) {
              recorded <- Reduce(`|`, lapply(qc_picco_columns, function(col) {
                !is.na(qc_num(d, col))
              }))
              qc_gate(!qc_flag(d, "picco_use_0_48h"), recorded)
            })
  ))
}

qc_no_ne <- function(d) {
  ne48 <- qc_flag(d, "ne_48h")
  ifelse(is.na(ne48), !qc_flag(d, "ne_24h") & !qc_flag(d, "ne_24_48h"), !ne48)
}

qc_vasopressor_rules <- function() {
  peak_rules <- function(id_missing, id_extra, window) {
    flag <- paste0("ne_", window)
    peak <- paste0("peak_ne_", window, "_ug_kg_min")
    list(
      qc_rule(id_missing, "vasopressor", "warning",
              sprintf("NE %s is Yes but peak NE %s is missing.", window, window), c(flag, peak),
              function(d) qc_gate(qc_flag(d, flag), is.na(qc_num(d, peak)))),
      qc_rule(id_extra, "vasopressor", "error",
              sprintf("NE %s is No but a peak NE %s dose above 0 is recorded.", window, window),
              c(flag, peak),
              function(d) qc_gate(!qc_flag(d, flag), qc_nonzero(qc_num(d, peak))))
    )
  }
  other_rule <- function(id, window) {
    drug <- paste0("other_vp_", window)
    dose <- paste0("peak_other_vp_", window, "_ug_kg_min")
    qc_rule(id, "vasopressor", "error",
            sprintf("Other vasopressor %s and its peak dose are not both recorded or both blank.",
                    window),
            c(drug, dose), function(d) is.na(qc_chr(d, drug)) != is.na(qc_num(d, dose)))
  }
  c(
    list(
      qc_rule("VAS_001", "vasopressor", "error",
              "NE 48h is not Yes exactly when NE 24h or NE 24-48h is Yes.",
              c("ne_48h", "ne_24h", "ne_24_48h"), function(d) {
                qc_flag(d, "ne_48h") != (qc_flag(d, "ne_24h") | qc_flag(d, "ne_24_48h"))
              }),
      qc_rule("VAS_002", "vasopressor", "error",
              "No NE was given in 48 h, but an NE delay, peak dose or total dose is recorded.",
              c("ne_48h", "ne_24h", "ne_24_48h", "ne_delay_h", "peak_ne_24h_ug_kg_min",
                "peak_ne_24_48h_ug_kg_min", "total_ne_48h_ug_kg"),
              function(d) {
                given <- !is.na(qc_num(d, "ne_delay_h")) |
                  qc_nonzero(qc_num(d, "peak_ne_24h_ug_kg_min")) |
                  qc_nonzero(qc_num(d, "peak_ne_24_48h_ug_kg_min")) |
                  qc_nonzero(qc_num(d, "total_ne_48h_ug_kg"))
                qc_gate(qc_no_ne(d), given)
              })
    ),
    peak_rules("VAS_003", "VAS_004", "24h"),
    peak_rules("VAS_005", "VAS_006", "24_48h"),
    list(
      qc_rule("VAS_007", "vasopressor", "error",
              "NE delay below 24 h does not match NE 24h = Yes.",
              c("ne_delay_h", "ne_24h"),
              function(d) (qc_num(d, "ne_delay_h") < 24) != qc_flag(d, "ne_24h")),
      qc_rule("VAS_008", "vasopressor", "error",
              "NE delay is between 24 and 48 h but NE 24-48h is not Yes.",
              c("ne_delay_h", "ne_24_48h"), function(d) {
                delay <- qc_num(d, "ne_delay_h")
                (delay >= 24 & delay < 48) & !qc_flag(d, "ne_24_48h")
              }),
      other_rule("VAS_009", "24h"),
      other_rule("VAS_010", "24_48h")
    )
  )
}

# Two rules per flag: a dependent value recorded with flag No is a contradiction (error), and a
# dependent value missing with flag Yes is incomplete data (warning).
qc_dependent_rules <- function(first_number, flag, dependents, recorded, label) {
  id <- function(k) sprintf("GAT_%03d", first_number + k)
  list(
    qc_rule(id(0), "gating", "error",
            sprintf("%s is No but %s is recorded.", flag, label), c(flag, dependents),
            function(d) {
              qc_gate(!qc_flag(d, flag), Reduce(`|`, lapply(dependents, recorded, d = d)))
            }),
    qc_rule(id(1), "gating", "warning",
            sprintf("%s is Yes but %s is missing.", flag, label), c(flag, dependents),
            function(d) {
              qc_gate(qc_flag(d, flag),
                      Reduce(`|`, lapply(dependents, function(col) is.na(qc_col(d, col)))))
            })
  )
}

qc_recorded <- function(col, d) {
  if (grepl("_dates?$", col)) !is.na(qc_date(d, col)) else qc_nonzero(qc_num(d, col))
}

qc_units_rules <- function(first_number, product) {
  units <- paste0(product, "_48h_units")
  pre <- paste0(product, "_pre_icu")
  delay <- paste0(product, "_delay_h")
  label <- toupper(product)
  id <- function(k) sprintf("GAT_%03d", first_number + k)
  list(
    qc_rule(id(0), "gating", "error",
            sprintf("%s before ICU is Yes but %s 48h units are 0.", label, label), c(pre, units),
            function(d) qc_flag(d, pre) & !(qc_num(d, units) > 0)),
    qc_rule(id(1), "gating", "error",
            sprintf("%s delay is recorded but %s 48h units are 0.", label, label),
            c(units, delay),
            function(d) qc_gate(qc_num(d, units) == 0, !is.na(qc_num(d, delay)))),
    qc_rule(id(2), "gating", "warning",
            sprintf("%s 48h units are above 0 but the %s delay is missing.", label, label),
            c(units, delay),
            function(d) qc_gate(qc_num(d, units) > 0, is.na(qc_num(d, delay))))
  )
}

qc_gating_rules <- function() {
  albumin48 <- function(d) qc_num(d, "albumin20_48h_ml") + qc_num(d, "albumin5_48h_ml")
  rules <- list(
    qc_rule("GAT_001", "gating", "error",
            "Albumin 24h = Yes does not match albumin (any) 24h volume above 0.",
            c("albumin_24h", "albumin_any_24h_ml"),
            function(d) qc_flag(d, "albumin_24h") != (qc_num(d, "albumin_any_24h_ml") > 0)),
    qc_rule("GAT_002", "gating", "error",
            "Albumin 48h = Yes does not match albumin 20% + 5% 48h volume above 0.",
            c("albumin_48h", "albumin20_48h_ml", "albumin5_48h_ml"),
            function(d) qc_flag(d, "albumin_48h") != (albumin48(d) > 0)),
    qc_rule("GAT_003", "gating", "error",
            "Albumin before ICU is Yes but albumin 48h is not Yes.",
            c("albumin_pre_icu", "albumin_48h"),
            function(d) qc_flag(d, "albumin_pre_icu") & !qc_flag(d, "albumin_48h"))
  )
  rules <- c(
    rules,
    qc_dependent_rules(4, "albumin_48h", "albumin_delay_h", qc_recorded, "an albumin delay"),
    list(qc_rule("GAT_006", "gating", "error",
                 "Venofundin 48h = Yes does not match a Venofundin volume above 0.",
                 c("venofundin_48h", "venofundin_48h_ml"), function(d) {
                   qc_flag(d, "venofundin_48h") != (qc_num(d, "venofundin_48h_ml") > 0)
                 })),
    qc_units_rules(7, "rbc"),
    qc_units_rules(10, "ffp"),
    list(
      qc_rule("GAT_013", "gating", "error",
              "Full thickness is No but a full thickness BSA above 0 is recorded.",
              c("full_thickness", "full_thickness_bsa_pct"), function(d) {
                qc_gate(!qc_flag(d, "full_thickness"),
                        qc_nonzero(qc_num(d, "full_thickness_bsa_pct")))
              }),
      qc_rule("GAT_014", "gating", "error",
              "Full thickness BSA is larger than TBSA burned.",
              c("full_thickness_bsa_pct", "tbsa_burned_pct"),
              function(d) qc_num(d, "full_thickness_bsa_pct") > qc_num(d, "tbsa_burned_pct")),
      qc_rule("GAT_015", "gating", "error",
              "Inhalation injury grade 3 is Yes but inhalation injury is not Yes.",
              c("inhalation_injury_grade3", "inhalation_injury"), function(d) {
                qc_flag(d, "inhalation_injury_grade3") & !qc_flag(d, "inhalation_injury")
              }),
      qc_rule("GAT_016", "gating", "error",
              "CRRT is Yes but RRT is not Yes.", c("crrt", "rrt"),
              function(d) qc_flag(d, "crrt") & !qc_flag(d, "rrt")),
      qc_rule("GAT_017", "gating", "info",
              "RRT is Yes but CRRT is No, so only intermittent RRT was used or CRRT is wrong.",
              c("rrt", "crrt"), function(d) qc_flag(d, "rrt") & !qc_flag(d, "crrt"))
    )
  )
  number <- 18
  for (flag in names(qc_flag_dependents)) {
    deps <- qc_flag_dependents[[flag]]
    rules <- c(rules, qc_dependent_rules(number, flag, deps, qc_recorded,
                                         paste(deps, collapse = " or ")))
    number <- number + 2
  }
  rules
}

qc_date_rules <- function() {
  before <- function(id, later, earlier, text) {
    qc_rule(id, "date", "error", text, c(later, earlier),
            function(d) qc_date(d, later) < qc_date(d, earlier))
  }
  rules <- list(
    qc_rule("DAT_001", "date", "error",
            "Burn date is outside the inclusion period 2015-01-01 to 2023-12-31.", "burn_date",
            function(d) {
              b <- qc_date(d, "burn_date")
              b < qc_inclusion_start | b > qc_inclusion_end
            }),
    qc_rule("DAT_002", "date", "error",
            "Burn is after ICU admission (date and time when both times are recorded).",
            c("burn_date", "burn_time", "icu_admission_date", "icu_admission_time"),
            function(d) {
              hours <- qc_burn_to_icu_hours(d)
              days <- qc_day_diff(qc_date(d, "icu_admission_date"), qc_date(d, "burn_date"))
              ifelse(is.na(hours), days < 0, hours < 0)
            }),
    qc_rule("DAT_003", "date", "warning",
            "ICU admission is over 48 h after the burn (over 2 days when a time is blank).",
            c("burn_date", "burn_time", "icu_admission_date", "icu_admission_time"),
            function(d) {
              hours <- qc_burn_to_icu_hours(d)
              days <- qc_day_diff(qc_date(d, "icu_admission_date"), qc_date(d, "burn_date"))
              ifelse(is.na(hours), days > 2, hours > 48)
            }),
    qc_rule("DAT_004", "date", "error",
            "The two ICU admission date columns differ.",
            c("icu_admission_date", "outcome_icu_admission_date"), function(d) {
              qc_date(d, "icu_admission_date") != qc_date(d, "outcome_icu_admission_date")
            }),
    before("DAT_005", "icu_discharge_date", "icu_admission_date",
           "ICU discharge is before ICU admission."),
    qc_rule("DAT_006", "date", "error",
            "ICU readmission is not after ICU discharge.",
            c("icu_readmission_date", "icu_discharge_date"),
            function(d) qc_date(d, "icu_readmission_date") <= qc_date(d, "icu_discharge_date")),
    before("DAT_007", "follow_up_end_date", "icu_discharge_date",
           "End of follow-up is before ICU discharge."),
    before("DAT_008", "vent_stop_date", "intubation_date",
           "Ventilation stop is before intubation."),
    before("DAT_009", "rrt_stop_date", "rrt_start_date", "RRT stop is before RRT start.")
  )
  for (k in seq_along(qc_event_dates)) {
    rules[[length(rules) + 1]] <- local({
      col <- qc_event_dates[k]
      qc_rule(sprintf("DAT_%03d", 9 + k), "date", "error",
              sprintf("%s is before the burn date or after the end of follow-up.", col),
              c(col, "burn_date", "follow_up_end_date"), function(d) {
                x <- qc_date(d, col)
                qc_any_fail(x < qc_date(d, "burn_date"), x > qc_date(d, "follow_up_end_date"))
              })
    })
  }
  rules
}

qc_duration_rules <- function() {
  span <- function(id, value, stop_col, start_col, text) {
    qc_rule(id, "duration", "error", paste(text, qc_duration_note),
            c(value, start_col, stop_col), function(d) {
              qc_duration_off(qc_num(d, value),
                              qc_day_diff(qc_date(d, stop_col), qc_date(d, start_col)))
            })
  }
  list(
    span("DUR_001", "icu_los_days", "icu_discharge_date", "icu_admission_date",
         "ICU length of stay does not match ICU discharge minus ICU admission."),
    span("DUR_002", "mech_vent_days", "vent_stop_date", "intubation_date",
         "Ventilation days do not match ventilation stop minus intubation."),
    span("DUR_003", "crrt_days", "rrt_stop_date", "rrt_start_date",
         "CRRT days do not match RRT stop minus RRT start."),
    qc_rule("DUR_004", "duration", "error",
            paste("Time to RRT does not match the first RRT date minus the burn date and time",
                  "(midnight when the burn time is blank).", qc_duration_note),
            c("rrt_start_days", "rrt_start_date", "burn_date", "burn_time"), function(d) {
              burn_min <- qc_minutes(d, "burn_time")
              burn_min[is.na(burn_min)] <- 0
              plain <- qc_day_diff(qc_date(d, "rrt_start_date"), qc_date(d, "burn_date")) -
                burn_min / 1440
              qc_duration_off(qc_num(d, "rrt_start_days"), plain)
            }),
    qc_rule("DUR_005", "duration", "error",
            "Hospital length of stay is shorter than ICU length of stay.",
            c("hospital_los_days", "icu_los_days"),
            function(d) qc_num(d, "hospital_los_days") < qc_num(d, "icu_los_days"))
  )
}

qc_outcome_rules <- function() {
  status <- function(d) tolower(qc_chr(d, "vital_status_follow_up"))
  list(
    qc_rule("OUT_001", "outcome", "error",
            "In-ICU mortality is Yes but in-hospital mortality is not Yes.",
            c("icu_mortality", "hospital_mortality"),
            function(d) qc_flag(d, "icu_mortality") & !qc_flag(d, "hospital_mortality")),
    qc_rule("OUT_002", "outcome", "error",
            "In-hospital mortality is Yes but vital status at end of follow-up is Alive.",
            c("hospital_mortality", "vital_status_follow_up"),
            function(d) qc_flag(d, "hospital_mortality") & status(d) == "alive"),
    qc_rule("OUT_003", "outcome", "info",
            "In-hospital mortality is No and vital status is Dead, so death came after discharge.",
            c("hospital_mortality", "vital_status_follow_up"),
            function(d) !qc_flag(d, "hospital_mortality") & status(d) == "dead")
  )
}

qc_site_key <- function(d, ...) {
  site <- qc_chr(d, "site")
  site[is.na(site)] <- ""
  parts <- list(...)
  key <- do.call(paste, c(list(site), parts, sep = "\r"))
  key[Reduce(`|`, lapply(parts, is.na))] <- NA
  key
}

qc_duplicated_key <- function(key) {
  out <- rep(NA, length(key))
  ok <- !is.na(key)
  out[ok] <- duplicated(key[ok]) | duplicated(key[ok], fromLast = TRUE)
  out
}

qc_duplicate_rules <- function() {
  list(
    qc_rule("DUP_001", "duplicate", "error",
            "Patient ID is missing or occurs more than once within the site.",
            c("site", "patient_id"), function(d) {
              id <- trimws(qc_chr(d, "patient_id"))
              dup <- qc_duplicated_key(qc_site_key(d, id))
              is.na(id) | dup %in% TRUE
            }),
    qc_rule("DUP_002", "duplicate", "warning",
            "Possible duplicate patient: same site, burn date, age (rounded), gender and TBSA.",
            c("site", "patient_id", "burn_date", "age_years", "gender", "tbsa_burned_pct"),
            function(d) {
              qc_duplicated_key(qc_site_key(
                d, qc_chr(d, "burn_date"), round(qc_num(d, "age_years")), qc_chr(d, "gender"),
                qc_num(d, "tbsa_burned_pct")
              ))
            })
  )
}

qc_phone_like <- function(x) {
  out <- rep(FALSE, length(x))
  ok <- !is.na(x)
  # Dates have digit groups and separators like phone numbers, so they are removed first.
  y <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}|[0-9]{1,2}[./][0-9]{1,2}[./][0-9]{2,4}", " ", x[ok])
  # A run glued to letters or a hyphen (for example "NBC-2019-00123") is a code, not a number.
  found <- regmatches(y, gregexpr("(?<![A-Za-z0-9-])\\+?\\(?[0-9][0-9 ().-]{5,}[0-9]", y,
                                  perl = TRUE))
  out[ok] <- vapply(found, function(cands) {
    digits <- nchar(gsub("[^0-9]", "", cands))
    any(digits >= 8 & digits <= 15 & grepl("^\\+|[ ().-]", cands))
  }, logical(1))
  out
}

qc_identifier_patterns <- list(
  list(id = "IDN_001", label = "an 11-digit Norwegian personal number",
       match = function(x) grepl("(^|[^0-9])[0-9]{6} ?[0-9]{5}([^0-9]|$)", x)),
  list(id = "IDN_002", label = "a US social security number",
       match = function(x) grepl("(^|[^0-9])[0-9]{3}-[0-9]{2}-[0-9]{4}([^0-9]|$)", x)),
  list(id = "IDN_003", label = "an email address",
       match = function(x) grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", x)),
  list(id = "IDN_004", label = "a phone number", match = qc_phone_like)
)

qc_identifier_rules <- function(codebook) {
  cols <- codebook$name[codebook$type %in% c("text", "categorical")]
  lapply(qc_identifier_patterns, function(p) {
    hits <- function(d) {
      lapply(cols, function(col) {
        x <- qc_chr(d, col)
        !is.na(x) & p$match(x)
      })
    }
    qc_rule(p$id, "identifier", "error",
            sprintf("A text or categorical cell looks like %s.", p$label), cols,
            function(d) {
              if (nrow(d) == 0) return(logical())
              Reduce(`|`, hits(d))
            },
            # The matching values are identifiers, so findings show the column only.
            detail = function(d) {
              h <- hits(d)
              vapply(seq_len(nrow(d)), function(i) {
                paste0(cols[vapply(h, `[`, logical(1), i)], "=[redacted]", collapse = "; ")
              }, character(1))
            })
  })
}

# Returns every QC rule. Column rules (type, category, range) take their id number from the
# codebook position, so ids stay stable when rules are added.
qc_rules <- function(codebook, ranges = read_qc_ranges()) {
  rules <- c(
    qc_structure_rules(codebook), qc_type_rules(codebook), qc_category_rules(codebook),
    qc_range_rules(codebook, ranges), qc_unit_rules(), qc_derived_rules(), qc_fluid_rules(),
    qc_hemodynamic_rules(), qc_vasopressor_rules(), qc_gating_rules(), qc_date_rules(),
    qc_duration_rules(), qc_outcome_rules(), qc_duplicate_rules(), qc_identifier_rules(codebook)
  )
  ids <- vapply(rules, `[[`, character(1), "id")
  if (anyDuplicated(ids) > 0) {
    stop("Duplicate QC rule ids: ", paste(unique(ids[duplicated(ids)]), collapse = ", "),
         call. = FALSE)
  }
  names(rules) <- ids
  rules
}

qc_rule_catalogue <- function(rules) {
  data.frame(
    id = vapply(rules, `[[`, character(1), "id"),
    category = vapply(rules, `[[`, character(1), "category"),
    severity = vapply(rules, `[[`, character(1), "severity"),
    description = vapply(rules, `[[`, character(1), "description"),
    columns = vapply(rules, function(r) paste(r$columns, collapse = "; "), character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# Runs one rule and returns a logical vector with one element per row. A rule that errors or
# returns the wrong shape is reported with a warning and treated as not evaluable.
qc_evaluate <- function(rule, data) {
  n <- nrow(data)
  res <- tryCatch(rule$check(data), error = function(e) {
    warning(sprintf("QC rule %s could not be evaluated: %s", rule$id, conditionMessage(e)),
            call. = FALSE)
    rep(NA, n)
  })
  if (!is.logical(res) || length(res) != n) {
    warning(sprintf("QC rule %s returned %d values for %d rows; treated as not evaluable.",
                    rule$id, length(res), n), call. = FALSE)
    res <- rep(NA, n)
  }
  as.vector(res)
}

qc_format_values <- function(data, cols, rows) {
  if (length(cols) == 0) return(rep("", length(rows)))
  parts <- lapply(cols, function(col) {
    v <- qc_chr(data, col)[rows]
    paste0(col, "=", ifelse(is.na(v), "NA", v))
  })
  do.call(paste, c(parts, sep = "; "))
}

qc_empty_findings <- function() {
  data.frame(site = character(), row = integer(), patient_id = character(),
             rule_id = character(), category = character(), severity = character(),
             columns = character(), values = character(), message = character(),
             stringsAsFactors = FALSE)
}

qc_rule_findings <- function(rule, data, res, site, patient_id) {
  failed <- which(res %in% TRUE)
  if (length(failed) == 0) return(NULL)
  values <- if (is.null(rule$detail)) {
    qc_format_values(data, rule$columns, failed)
  } else {
    rule$detail(data)[failed]
  }
  out <- data.frame(
    site = site[failed], row = failed, patient_id = patient_id[failed], rule_id = rule$id,
    category = rule$category, severity = rule$severity,
    columns = paste(rule$columns, collapse = "; "), values = values,
    message = rule$description, stringsAsFactors = FALSE
  )
  if (identical(rule$level, "dataset")) {
    out <- out[!duplicated(out$site), , drop = FALSE]
    out$row <- NA_integer_
    out$patient_id <- NA_character_
  }
  out
}

qc_rule_summary <- function(rule, res, site) {
  sites <- unique(site)
  data.frame(
    site = sites, rule_id = rule$id, category = rule$category, severity = rule$severity,
    n_rows = vapply(sites, function(s) sum(site == s), integer(1)),
    n_evaluated = vapply(sites, function(s) sum(!is.na(res[site == s])), integer(1)),
    n_failed = vapply(sites, function(s) sum(res[site == s] %in% TRUE), integer(1)),
    n_not_evaluable = vapply(sites, function(s) sum(is.na(res[site == s])), integer(1)),
    n_passed = vapply(sites, function(s) sum(res[site == s] %in% FALSE), integer(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# For each variable, TRUE where a blank cell is expected from another column, for example an
# NE dose when no NE was given. Variables without an entry are never structurally missing.
qc_structural_gates <- function() {
  # force() binds the column now; the factories are called inside loops.
  is_no <- function(col) {
    force(col)
    function(d) qc_flag(d, col) %in% FALSE
  }
  zero_units <- function(col) {
    force(col)
    function(d) qc_num(d, col) %in% 0
  }
  no_ne <- function(d) qc_no_ne(d) %in% TRUE
  no_picco <- is_no("picco_use_0_48h")
  gates <- list()
  for (col in c("ne_delay_h", "total_ne_48h_ug_kg", "days_on_vp", "sap_before_ne_mmhg",
                "map_before_ne_mmhg", "dap_before_ne_mmhg", "hr_before_ne_bpm")) {
    gates[[col]] <- no_ne
  }
  gates$peak_ne_24h_ug_kg_min <- is_no("ne_24h")
  gates$peak_ne_24_48h_ug_kg_min <- is_no("ne_24_48h")
  gates$other_vp_24h <- function(d) rep(TRUE, nrow(d))
  gates$other_vp_24_48h <- function(d) rep(TRUE, nrow(d))
  gates$peak_other_vp_24h_ug_kg_min <- function(d) is.na(qc_chr(d, "other_vp_24h"))
  gates$peak_other_vp_24_48h_ug_kg_min <- function(d) is.na(qc_chr(d, "other_vp_24_48h"))
  for (col in qc_picco_columns) gates[[col]] <- no_picco
  gates$ci_before_ne_l_min_m2 <- function(d) no_picco(d) | no_ne(d)
  gates$cvp_before_ne_mmhg <- function(d) no_picco(d) | no_ne(d)
  gates$full_thickness_bsa_pct <- is_no("full_thickness")
  gates$albumin_delay_h <- is_no("albumin_48h")
  gates$rbc_delay_h <- zero_units("rbc_48h_units")
  gates$ffp_delay_h <- zero_units("ffp_48h_units")
  gates$first_surgery_days <- zero_units("surgeries_total_n")
  gates$last_graft_surgery_date <- function(d) {
    zero_units("surgeries_total_n")(d) | is_no("graft_surgery_7d")(d)
  }
  gates$comorbidities_list <- function(d) {
    Reduce(`&`, lapply(names(qc_comorbidity_flags), function(f) is_no(f)(d)))
  }
  for (flag in names(qc_flag_dependents)) {
    for (col in qc_flag_dependents[[flag]]) gates[[col]] <- is_no(flag)
  }
  gates
}

qc_missingness <- function(data, codebook, site) {
  gates <- qc_structural_gates()
  sites <- unique(site)
  rows <- lapply(seq_len(nrow(codebook)), function(i) {
    nm <- codebook$name[i]
    x <- qc_col(data, nm)
    missing <- is.na(x)
    if (is.character(x)) missing <- missing | !nzchar(trimws(x))
    expected <- if (is.null(gates[[nm]])) rep(FALSE, nrow(data)) else gates[[nm]](data)
    structural <- missing & expected
    lapply(sites, function(s) {
      in_site <- site == s
      n <- sum(in_site)
      unexpected <- sum(missing[in_site] & !structural[in_site])
      pct_unexpected <- if (n > 0) 100 * unexpected / n else NA_real_
      data.frame(
        site = s, variable = nm, n = n, n_missing = sum(missing[in_site]),
        pct_missing = if (n > 0) 100 * sum(missing[in_site]) / n else NA_real_,
        structural_missing = sum(structural[in_site]), unexpected_missing = unexpected,
        pct_unexpected_missing = pct_unexpected, key_variable = isTRUE(codebook$key_variable[i]),
        flag_high_missing = isTRUE(codebook$key_variable[i]) && isTRUE(pct_unexpected > 20),
        stringsAsFactors = FALSE
      )
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

qc_attr_table <- function(data, name, empty) {
  x <- attr(data, name)
  if (is.data.frame(x)) x else empty
}

# Runs every rule on data (one or more sites, with a `site` column) and returns findings,
# per-rule counts, missingness and the loader and standardisation problem tables.
run_qc <- function(data, codebook, ranges = read_qc_ranges(),
                   rules = qc_rules(codebook, ranges)) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  if (!"site" %in% names(data)) stop("'data' needs a 'site' column.", call. = FALSE)
  site <- qc_chr(data, "site")
  site[is.na(site)] <- "(missing)"
  patient_id <- qc_chr(data, "patient_id")

  findings <- list()
  summary <- list()
  for (rule in rules) {
    res <- qc_evaluate(rule, data)
    findings[[rule$id]] <- qc_rule_findings(rule, data, res, site, patient_id)
    summary[[rule$id]] <- qc_rule_summary(rule, res, site)
  }
  findings <- do.call(rbind, c(list(qc_empty_findings()), unname(findings)))
  rownames(findings) <- NULL
  summary <- do.call(rbind, unname(summary))
  rownames(summary) <- NULL

  list(
    findings = findings,
    rule_summary = summary,
    missingness = qc_missingness(data, codebook, site),
    parse_problems = qc_attr_table(data, "parse_problems", data.frame(
      row = integer(), column = character(), raw_value = character(),
      expected_type = character(), stringsAsFactors = FALSE
    )),
    unmapped_values = qc_attr_table(data, "unmapped_values", data.frame(
      column = character(), value = character(), n = integer(), stringsAsFactors = FALSE
    )),
    rule_catalogue = qc_rule_catalogue(rules)
  )
}

# TRUE when `dir` is an existing folder at or below <project root>/output, where the project
# root is confirmed by PROJECT_VERSION.
qc_inside_output <- function(dir) {
  if (!is.character(dir) || length(dir) != 1 || is.na(dir) || !dir.exists(dir)) return(FALSE)
  p <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  repeat {
    at_output <- basename(p) == "output"
    # is_project_root() lives in R/project_paths.R, which lintr does not see from this file.
    if (at_output && is_project_root(dirname(p))) return(TRUE) # nolint: object_usage_linter.
    parent <- dirname(p)
    if (identical(parent, p)) return(FALSE)
    p <- parent
  }
}

write_qc_outputs <- function(qc, dir) {
  if (!qc_inside_output(dir)) {
    stop("'dir' must be an existing folder inside <project root>/output/ (use ",
         "project_output_dir()). Nothing was written.", call. = FALSE)
  }
  tables <- list(
    findings = qc$findings, rule_summary = qc$rule_summary, missingness = qc$missingness,
    parse_problems = qc$parse_problems, unmapped_values = qc$unmapped_values,
    rule_catalogue = qc$rule_catalogue
  )
  absent <- names(tables)[vapply(tables, is.null, logical(1))]
  if (length(absent) > 0) {
    stop("'qc' is missing: ", paste(absent, collapse = ", "), ". Nothing was written.",
         call. = FALSE)
  }
  paths <- file.path(normalizePath(dir, winslash = "/"), paste0(names(tables), ".csv"))
  for (i in seq_along(tables)) {
    utils::write.csv(tables[[i]], paths[i], row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  invisible(paths)
}
