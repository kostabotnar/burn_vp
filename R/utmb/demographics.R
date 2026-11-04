# nolint start: object_usage_linter. Functions from the other files are not visible here.
# Aggregate demographics for the UTMB extract. Nothing returned here is patient-level.

utmb_age_bands <- c("<18", "18-39", "40-64", "65+", "missing")

# Age in years (365.25-day years) at `event` for a person born on `birth`. Dates may be Date
# objects or text. NA when either date is missing or unreadable. Negative when event < birth.
utmb_age_years <- function(birth, event) {
  if (!inherits(birth, "Date")) birth <- parse_utmb_dates(birth)
  if (!inherits(event, "Date")) event <- parse_utmb_dates(event)
  as.numeric(event - birth) / 365.25
}

# Age band for ages in years. Missing and negative ages (event before birth) are "missing".
utmb_age_band <- function(age) {
  band <- rep("missing", length(age))
  ok <- !is.na(age) & age >= 0
  band[ok & age < 18] <- "<18"
  band[ok & age >= 18 & age < 40] <- "18-39"
  band[ok & age >= 40 & age < 65] <- "40-64"
  band[ok & age >= 65] <- "65+"
  factor(band, levels = utmb_age_bands)
}

# Counts and percentages of the levels of `x`; missing values appear as "(missing)".
tabulate_utmb_levels <- function(x, variable) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "(missing)"
  tab <- table(x)
  out <- data.frame(variable = variable, level = names(tab), n = as.integer(tab),
                    stringsAsFactors = FALSE)
  out$pct <- round(100 * out$n / sum(out$n), 2)
  out <- out[order(-out$n, out$level, method = "radix"), ]
  rownames(out) <- NULL
  out
}

tabulate_utmb_factor <- function(f, variable) {
  tab <- table(f)
  out <- data.frame(variable = variable, level = names(tab), n = as.integer(tab),
                    stringsAsFactors = FALSE)
  out$pct <- round(100 * out$n / sum(out$n), 2)
  out
}

summarise_utmb_numeric <- function(x, variable) {
  n_all <- length(x)
  x <- x[!is.na(x)]
  q <- if (length(x)) stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE) else rep(NA_real_, 3)
  data.frame(
    variable = variable, n = length(x), n_missing = n_all - length(x),
    mean = if (length(x)) round(mean(x), 3) else NA_real_,
    sd = if (length(x) > 1) round(stats::sd(x), 3) else NA_real_,
    median = q[2], q1 = q[1], q3 = q[3],
    min = if (length(x)) min(x) else NA_real_, max = if (length(x)) max(x) else NA_real_,
    stringsAsFactors = FALSE
  )
}

# Row of the first admission per StudyID: earliest AdmissionDateKey (positive keys only), ties
# broken by AdmissionTimeOfDayKey and then file order.
first_utmb_admissions <- function(admissions) {
  date <- parse_utmb_dates(admissions[["AdmissionDateKey"]])
  time <- suppressWarnings(as.numeric(admissions[["AdmissionTimeOfDayKey"]]))
  if (is.null(admissions[["AdmissionTimeOfDayKey"]])) time <- rep(0, length(date))
  time[is.na(time) | time < 0] <- 0
  keep <- !is.na(date) & !is.na(admissions[["StudyID"]])
  idx <- which(keep)
  idx <- idx[order(admissions[["StudyID"]][idx], date[idx], time[idx], idx, method = "radix")]
  idx[!duplicated(admissions[["StudyID"]][idx])]
}

# `demographics`, `crosswalk` and `admissions` are character data.tables from the reader; any of
# them may be NULL. Returns list(categorical, numeric) with the layout of the output CSV files.
summarise_utmb_demographics <- function(demographics = NULL, crosswalk = NULL,
                                        admissions = NULL) {
  cat_parts <- list()
  num_parts <- list()
  demo_ids <- if (is.null(demographics)) character() else unique(demographics[["StudyID"]])
  xw_ids <- if (is.null(crosswalk)) character() else unique(crosswalk[["StudyID"]])
  sets <- c(demographics = length(demo_ids), crosswalk = length(xw_ids),
            both = length(intersect(demo_ids, xw_ids)),
            either = length(union(demo_ids, xw_ids)))
  cat_parts[[1]] <- data.frame(variable = "patients", level = names(sets), n = as.integer(sets),
                               pct = round(100 * sets / max(sets["either"], 1), 2),
                               stringsAsFactors = FALSE)

  if (!is.null(demographics)) {
    vars <- c("Sex", "SexAssignedAtBirth", "FirstRace", "MultiRacial", "Ethnicity",
              "SmokingStatus", "Status", "IsValid", "Test")
    for (v in intersect(vars, names(demographics))) {
      cat_parts[[length(cat_parts) + 1]] <- tabulate_utmb_levels(demographics[[v]], v)
    }
    if ("DeathDate" %in% names(demographics)) {
      deceased <- ifelse(is.na(demographics[["DeathDate"]]), "no", "yes")
      cat_parts[[length(cat_parts) + 1]] <- tabulate_utmb_levels(deceased, "deceased")
    }
  }
  if (!is.null(crosswalk) && "BurnCode" %in% names(crosswalk)) {
    cat_parts[[length(cat_parts) + 1]] <-
      tabulate_utmb_levels(crosswalk[["BurnCode"]], "BurnCode")
  }

  if (!is.null(demographics) && !is.null(admissions) &&
        all(c("StudyID", "AdmissionDateKey") %in% names(admissions))) {
    first <- first_utmb_admissions(admissions)
    first <- first[admissions[["StudyID"]][first] %in% demo_ids]
    first_date <- parse_utmb_dates(admissions[["AdmissionDateKey"]][first])
    names(first_date) <- admissions[["StudyID"]][first]
    event <- first_date[demographics[["StudyID"]]]
    age <- if ("BirthDate" %in% names(demographics)) {
      utmb_age_years(demographics[["BirthDate"]], unname(event))
    } else {
      rep(NA_real_, nrow(demographics))
    }
    check <- ifelse(is.na(age), "missing", ifelse(age < 0, "admission before birth", "valid"))
    cat_parts[[length(cat_parts) + 1]] <-
      tabulate_utmb_levels(check, "age_at_first_admission_check")
    cat_parts[[length(cat_parts) + 1]] <-
      tabulate_utmb_factor(utmb_age_band(age), "age_band_at_first_admission")
    num_parts[[length(num_parts) + 1]] <-
      summarise_utmb_numeric(age[!is.na(age) & age >= 0], "age_at_first_admission_years")

    n_adm <- table(factor(admissions[["StudyID"]], levels = demo_ids))
    num_parts[[length(num_parts) + 1]] <-
      summarise_utmb_numeric(as.numeric(n_adm), "hospital_admissions_per_patient")
    if ("LengthOfStayInDays" %in% names(admissions)) {
      los <- suppressWarnings(as.numeric(admissions[["LengthOfStayInDays"]][first]))
      num_parts[[length(num_parts) + 1]] <-
        summarise_utmb_numeric(los, "length_of_stay_first_admission_days")
    }
  }
  list(categorical = do.call(rbind, cat_parts),
       numeric = if (length(num_parts)) do.call(rbind, num_parts) else NULL)
}
# nolint end
