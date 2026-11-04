# nolint start: object_usage_linter. data.table column names and functions from the other
# files are not visible here.
# Column profiles and table statistics for one UTMB table. Only aggregates are returned; values are
# listed only for columns that are neither identifiers nor free text.

utmb_placeholder_pattern <- "^\\*(Unspecified|Unknown|Not Applicable|Deleted)$"
utmb_sentinel_columns <- "Key$"

# Counts of the non-missing values of a character vector, as a data.table with columns v and N.
utmb_value_counts <- function(x) {
  counts <- data.table::data.table(v = x)
  counts <- counts[!is.na(v) & v != "", .N, by = v]
  counts[order(-N, v, method = "radix")]
}

# Negative values in Key columns are Caboodle sentinels for unknown or not applicable.
# Returns the number of rows holding one and the distinct sentinel values with their counts.
count_utmb_sentinels <- function(name, counts) {
  if (!grepl(utmb_sentinel_columns, name) || nrow(counts) == 0) {
    return(list(n = 0L, values = ""))
  }
  neg <- counts[grepl("^-[0-9]+$", v)]
  if (nrow(neg) == 0) return(list(n = 0L, values = ""))
  neg <- neg[order(as.numeric(v), decreasing = TRUE, method = "radix")]
  list(n = sum(neg$N), values = paste0(neg$v, " (", neg$N, ")", collapse = " | "))
}

# Up to five most frequent values with counts. Blank for identifier and free-text columns and for
# columns with more than `max_distinct` distinct values.
format_utmb_top_values <- function(counts, identifier, role, max_distinct = 50, n_top = 5) {
  if (isTRUE(identifier) || role == "free_text" || nrow(counts) == 0 ||
        nrow(counts) > max_distinct) {
    return("")
  }
  top <- utils::head(counts, n_top)
  paste0(top$v, " (", top$N, ")", collapse = " | ")
}

# Smallest and largest value for numeric, integer, date, datetime and date_key columns, as the
# original text. Date keys ignore negative sentinels. Returns c(min = "", max = "") otherwise.
utmb_min_max <- function(counts, type) {
  none <- c(min = "", max = "")
  if (nrow(counts) == 0) return(none)
  v <- counts$v
  if (type %in% c("integer", "numeric", "date_key")) {
    num <- suppressWarnings(as.numeric(v))
    if (type == "date_key") num[!is.na(num) & num < 0] <- NA_real_
    if (all(is.na(num))) return(none)
    return(c(min = v[which.min(num)], max = v[which.max(num)]))
  }
  if (type %in% c("date", "datetime")) {
    ord <- sort(v, method = "radix")
    return(c(min = ord[1], max = ord[length(ord)]))
  }
  none
}

# Profile of one column. `x` is a character vector of the whole column.
profile_utmb_column <- function(x, name, table = NA_character_) {
  n_rows <- length(x)
  counts <- utmb_value_counts(x)
  type <- infer_utmb_type(name, counts$v)
  n_distinct <- nrow(counts)
  role <- assign_utmb_role(name, type, n_distinct)
  identifier <- is_utmb_identifier(name)
  n_missing <- n_rows - sum(counts$N)
  sentinel <- count_utmb_sentinels(name, counts)
  hide_range <- identifier || role %in% c("free_text", "surrogate_key", "study_id")
  range <- if (hide_range) c(min = "", max = "") else utmb_min_max(counts, type)
  data.frame(
    table = table, column = name, inferred_type = type, role = role, identifier = identifier,
    n_rows = n_rows, n_missing = n_missing,
    pct_missing = if (n_rows > 0) round(100 * n_missing / n_rows, 2) else NA_real_,
    n_placeholder = sum(counts$N[grepl(utmb_placeholder_pattern, counts$v)]),
    n_sentinel = sentinel$n, sentinel_values = sentinel$values,
    n_distinct = n_distinct, min = range[["min"]], max = range[["max"]],
    top_values = format_utmb_top_values(counts, identifier, role),
    stringsAsFactors = FALSE
  )
}

profile_utmb_columns <- function(dt, table) {
  cols <- names(dt)
  res <- if (nrow(dt) < 1e5) {
    lapply(cols, function(n) profile_utmb_column(dt[[n]], n, table))
  } else {
    utmb_lapply(cols, function(n) profile_utmb_column(dt[[n]], n, table))
  }
  out <- do.call(rbind, res)
  out$position <- seq_along(cols)
  out
}

# Earliest and latest calendar date of a column plus the number of non-missing values that could
# not be read as a date. Values come from ISO dates, ISO timestamps or YYYYMMDD keys; negative
# keys count as unparsed.
summarise_utmb_dates <- function(x) {
  counts <- utmb_value_counts(x)
  dates <- parse_utmb_dates(counts$v)
  ok <- !is.na(dates)
  list(earliest = if (any(ok)) format(min(dates[ok])) else "",
       latest = if (any(ok)) format(max(dates[ok])) else "",
       n_unparsed = sum(counts$N[!ok]))
}

# One row of table_stats. `profile` is the output of profile_utmb_columns for the same table,
# `crosswalk_ids` the StudyIDs of the crosswalk table.
compute_utmb_table_stats <- function(dt, table, profile, crosswalk_ids, date_column,
                                     file_bytes = NA_real_) {
  ids <- dt[["StudyID"]]
  per_patient <- if (is.null(ids)) integer() else utmb_value_counts(ids)$N
  patients <- if (is.null(ids)) character() else unique(ids[!is.na(ids)])
  has_date <- !is.na(date_column) && date_column %in% names(dt)
  dates <- if (has_date) summarise_utmb_dates(dt[[date_column]]) else
    list(earliest = "", latest = "", n_unparsed = NA_integer_)
  n_dup <- if (nrow(dt) > 0 && ncol(dt) > 0) nrow(dt) - data.table::uniqueN(dt) else 0L
  data.frame(
    table = table, file_bytes = file_bytes, n_rows = nrow(dt), n_cols = ncol(dt),
    n_patients = length(patients),
    n_patients_not_in_crosswalk = sum(!patients %in% crosswalk_ids),
    n_crosswalk_patients_absent = sum(!crosswalk_ids %in% patients),
    rows_per_patient_min = if (length(per_patient)) min(per_patient) else NA_integer_,
    rows_per_patient_median = if (length(per_patient)) stats::median(per_patient) else NA_real_,
    rows_per_patient_max = if (length(per_patient)) max(per_patient) else NA_integer_,
    n_duplicate_rows = n_dup,
    n_empty_columns = sum(profile$n_missing == profile$n_rows),
    n_constant_columns = sum(profile$n_distinct == 1 & profile$n_missing == 0),
    date_column = if (has_date) date_column else NA_character_,
    date_earliest = dates$earliest, date_latest = dates$latest,
    date_n_unparsed = dates$n_unparsed,
    stringsAsFactors = FALSE
  )
}
# nolint end
