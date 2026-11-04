# Type inference, role assignment and the data dictionary for the UTMB extract. Everything here is
# a pure function of column names and (distinct) values; the dictionary holds structure only.

utmb_iso_date <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
utmb_iso_datetime <-
  "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}(:[0-9]{2}([.][0-9]+)?)?$"
utmb_flag_name <- "^_?(Is|Has)[A-Z]|^(Test|Abnormal|Accepted|Chronic|FromDevice|Significant)$"
utmb_max_category_levels <- 50

# `values` are the non-missing values of a column (distinct values are enough).
# Returns one of: integer, numeric, date, datetime, date_key, time_key, boolean, text, empty.
infer_utmb_type <- function(name, values) {
  v <- values[!is.na(values) & values != ""]
  if (length(v) == 0) return("empty")
  all_int <- all(grepl("^-?[0-9]{1,18}$", v))
  if (all_int && grepl("TimeOfDayKey$", name)) return("time_key")
  if (all_int && grepl("DateKey$", name)) return("date_key")
  if (all(tolower(v) %in% c("true", "false"))) return("boolean")
  if (all_int && all(v %in% c("0", "1")) &&
        (grepl(utmb_flag_name, name) || all(c("0", "1") %in% v))) {
    return("boolean")
  }
  is_date <- grepl(utmb_iso_date, v)
  is_datetime <- grepl(utmb_iso_datetime, v)
  if (all(is_date)) return("date")
  if (all(is_date | is_datetime)) return("datetime")
  if (all_int && !any(grepl("^-?0[0-9]", v))) return("integer")
  if (all(grepl("^-?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", v)) &&
        !any(grepl("^-?0[0-9]", v))) {
    return("numeric")
  }
  "text"
}

utmb_free_text_pattern <- "(Comment|Narrative|FreeText|Note)s?$"
utmb_identifier_names <- c("PatientName", "MRN", "PatientEpicId", "DOB", "BirthDate", "DeathDate",
                           "DeathInstant")

# The reader appends _1, _2 to repeated column names; rules apply to the original name.
utmb_base_name <- function(name) sub("(?<=[A-Za-z])_[0-9]+$", "", name, perl = TRUE)

# TRUE for direct identifiers: names, MRN, Epic ids, CSN and account ids, birth and death dates,
# and free-text comment fields. Epic ids of definitions (flowsheet rows, macros, order sets)
# are not patient records and are excluded.
is_utmb_identifier <- function(name) {
  name <- utmb_base_name(name)
  if (name %in% utmb_identifier_names) return(TRUE)
  if (grepl(utmb_free_text_pattern, name)) return(TRUE)
  if (grepl("Csn$", name)) return(TRUE)
  grepl("EpicId$", name) && !grepl("^(FlowsheetRow|Macro|OrderSet)", name)
}

# Role of a column: study_id, surrogate_key, date_key, time_key, instant, identifier, audit, flag,
# measure, category, free_text or other.
assign_utmb_role <- function(name, type, n_distinct = NA_integer_) {
  name <- utmb_base_name(name)
  if (name == "StudyID") return("study_id")
  if (startsWith(name, "_")) return("audit")
  if (grepl(utmb_free_text_pattern, name)) return("free_text")
  if (is_utmb_identifier(name)) return("identifier")
  if (grepl("DateKey$", name)) return("date_key")
  if (grepl("TimeOfDayKey$", name)) return("time_key")
  if (grepl("Key$", name)) return("surrogate_key")
  if (grepl("Instant$", name) || type %in% c("date", "datetime")) return("instant")
  if (type == "boolean" || grepl(utmb_flag_name, name)) return("flag")
  if (name == "Count" || grepl("Id$", name)) return("other")
  if (type %in% c("integer", "numeric")) return("measure")
  if (type == "text" && !is.na(n_distinct) && n_distinct <= utmb_max_category_levels) {
    return("category")
  }
  "other"
}

utmb_words <- function(s) {
  s <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", s)
  s <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", s)
  tolower(trimws(s))
}

utmb_exact_descriptions <- c(
  StudyID = "Study patient identifier",
  Count = "Row count, usually 1",
  PatientName = "Patient name (direct identifier)",
  MRN = "Medical record number (direct identifier)",
  PatientEpicId = "Epic patient identifier (direct identifier)",
  DOB = "Date of birth (direct identifier)",
  BirthDate = "Date of birth (direct identifier)",
  DeathDate = "Date of death (direct identifier)",
  DeathInstant = "Timestamp of death (direct identifier)",
  PatientDurableKey = "Durable key for the patient",
  Sex = "Sex",
  SexAssignedAtBirth = "Sex assigned at birth",
  Ethnicity = "Ethnicity",
  SmokingStatus = "Smoking status"
)

# Short plain-English description from the column name and Caboodle naming conventions.
# Returns "" when the name does not say enough.
describe_utmb_column <- function(name) {
  name <- utmb_base_name(name)
  if (name %in% names(utmb_exact_descriptions)) return(unname(utmb_exact_descriptions[name]))
  of <- function(suffix, fallback) {
    stem <- utmb_words(sub(paste0(suffix, "$"), "", name))
    if (nzchar(stem)) stem else fallback
  }
  if (startsWith(name, "_")) return(paste("ETL audit field:", utmb_words(sub("^_", "", name))))
  neg <- "; negative values mean unknown or not applicable"
  if (grepl("DateKey$", name)) {
    return(paste0("Date of ", of("DateKey", "the record"), ", as a YYYYMMDD key", neg))
  }
  if (grepl("TimeOfDayKey$", name)) {
    return(paste0("Time of day of ", of("TimeOfDayKey", "the record"), ", as a key", neg))
  }
  if (grepl("DurableKey$", name)) return(paste("Durable key for", of("DurableKey", "the record")))
  if (grepl("Key$", name)) return(paste0("Surrogate key for ", of("Key", "the record"), neg))
  if (grepl("Instant$", name)) return(paste("Timestamp of", of("Instant", "the record")))
  if (grepl("Date$", name)) return(paste("Date of", of("Date", "the record")))
  if (grepl(utmb_flag_name, name)) return(paste("Yes/no flag:", utmb_words(name)))
  ""
}

# One dictionary row per column. `names` and `types` are in file order; `n_distinct` is the number
# of distinct non-missing values per column (used only to tell categories from other text).
build_utmb_dictionary <- function(table, names, types,
                                  n_distinct = rep(NA_integer_, length(names))) {
  roles <- mapply(assign_utmb_role, names, types, n_distinct, USE.NAMES = FALSE)
  data.frame(
    table = table,
    position = seq_along(names),
    column = names,
    inferred_type = types,
    role = roles,
    identifier = vapply(names, is_utmb_identifier, logical(1), USE.NAMES = FALSE),
    description = vapply(names, describe_utmb_column, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

# Writes the dictionary as utmb_dictionary.csv into `dir` (a run folder inside output/), ordered by
# table, then position. Returns the file path.
write_utmb_dictionary <- function(dictionary, dir) {
  dictionary <- dictionary[order(dictionary$table, dictionary$position, method = "radix"), ]
  path <- file.path(dir, "utmb_dictionary.csv")
  utils::write.csv(dictionary, path, row.names = FALSE, na = "")
  invisible(path)
}
