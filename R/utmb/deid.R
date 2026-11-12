# nolint start: object_usage_linter. Functions from the other files are not visible here.
# De-identification of the UTMB extract. A policy file (metadata/utmb_deid_policy.csv) lists one
# action per table column. Patients get a new number (written as StudyID), dates move by one
# random offset per patient, linking keys are replaced by pseudonyms, and identifiers and free
# text are dropped. Offsets and pseudonyms are derived from one random run secret with SHA-256,
# so every table stays joinable. Only the patient number needs a lookup file (patients.csv).

utmb_deid_actions <- c("keep", "drop", "shift_date", "shift_instant", "shift_datekey", "remap",
                       "keep_common_values", "keep_typed_or_common_values")

# RowValueType values whose flowsheet Value is kept. Other types may hold free text or dates.
utmb_flowsheet_value_types <- c("Custom List", "Numeric Type", "Blood Pressure", "Temperature")

# Allowed offsets in days: 60 values, never 0.
utmb_deid_offset_days <- c(-30:-1, 1:30)

# A text value in a keep_common_values column survives only if it is numeric-like or occurs for
# at least this many distinct patients in that column of that table.
utmb_deid_min_patients <- 20L

# Actions whose output cells can equal original values (checked against the own birth date).
utmb_deid_unshifted_actions <- c("keep", "keep_common_values", "keep_typed_or_common_values")

# Dates on or before the first, or on or after the second, are Epic placeholders and are never
# shifted (burn_index.R also treats 1900-01-01 and earlier as a placeholder).
utmb_placeholder_low <- as.Date("1900-01-01")
utmb_placeholder_high <- as.Date("9999-01-01")

# Date columns dropped as identifiers are compared patient by patient in the leak check, because
# a date string also occurs legitimately in other patients' date columns.
utmb_deid_date_identifiers <- c("DOB", "BirthDate")

utmb_deid_policy_file <- "utmb_deid_policy.csv"

# Column that identifies a patient in every table of the extract (Epic's stable patient key).
utmb_default_patient_key <- "PatientDurableKey"

# Names the output uses for the new patient id and in patients.csv.
utmb_deid_id_column <- "StudyID"
utmb_deid_extract_id_column <- "extract_StudyID"

# ---- run secret, offsets and pseudonyms -------------------------------------------------------

# 32 random bytes as 64 hex characters from the operating system's random source.
new_utmb_secret <- function() {
  source <- "/dev/urandom"
  if (!file.exists(source)) {
    stop("No cryptographic random source: ", source, " is not available on this system. ",
         "Run on Linux, or pass an earlier keys folder with --keys.", call. = FALSE)
  }
  bytes <- tryCatch({
    con <- file(source, open = "rb", raw = TRUE)
    on.exit(close(con))
    readBin(con, what = "raw", n = 32L)
  }, error = function(e) raw(0))
  if (length(bytes) != 32L) stop("Could not read 32 random bytes from ", source, ".", call. = FALSE)
  paste(as.character(bytes), collapse = "")
}

read_utmb_secret <- function(path) {
  if (!file.exists(path)) stop("Secret file not found: ", path, call. = FALSE)
  secret <- tolower(trimws(readLines(path, n = 1L, warn = FALSE)))
  if (length(secret) != 1L || !grepl("^[0-9a-f]{64}$", secret)) {
    stop("The secret file must hold 64 hexadecimal characters: ", path, call. = FALSE)
  }
  secret
}

utmb_sha256 <- function(x) {
  if (length(x) == 0L) return(character(0))
  digest::getVDigest(algo = "sha256")(x, serialize = FALSE)
}

# One offset per patient key value: SHA-256 of secret, "offset" and the value, mapped onto the 60
# allowed days.
utmb_deid_offsets <- function(ids, secret) {
  ids <- as.character(ids)
  out <- rep(NA_integer_, length(ids))
  present <- !is.na(ids)
  u <- unique(ids[present])
  hash <- utmb_sha256(paste(secret, "offset", u, sep = "|"))
  slot <- strtoi(substr(hash, 1L, 7L), 16L) %% 60L
  out[present] <- utmb_deid_offset_days[slot + 1L][match(ids[present], u)]
  out
}

# Pseudonym of a key value: first 16 hex characters of SHA-256 of secret, domain and value.
# Missing values and negative numbers (Epic's "unknown" keys) are returned unchanged.
utmb_deid_remap <- function(x, domain, secret) {
  x <- as.character(x)
  todo <- !is.na(x) & !grepl("^-[0-9]", x)
  u <- unique(x[todo])
  x[todo] <- substr(utmb_sha256(paste(secret, domain, u, sep = "|")), 1L, 16L)[match(x[todo], u)]
  x
}

# ---- date shifting ----------------------------------------------------------------------------

utmb_shift_result <- function(value, changed, unchanged, blanked) {
  list(value = value, changed = changed, unchanged = unchanged, blanked = blanked)
}

utmb_is_placeholder <- function(d) {
  !is.na(d) & (d <= utmb_placeholder_low | d >= utmb_placeholder_high)
}

# Shifts ISO dates and timestamps by `days` (one per element), keeping everything after the
# first 10 characters (time of day, separator, fraction). Text that is not an ISO date becomes
# missing. Placeholder dates (see utmb_placeholder_low and utmb_placeholder_high) and results
# outside years 1000 to 9999 are left as they were.
utmb_shift_iso <- function(x, days) {
  x <- as.character(x)
  out <- x
  present <- !is.na(x)
  iso <- present & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}($|[ T])", x)
  out[present & !iso] <- NA_character_
  idx <- which(iso)
  d10 <- substr(x[idx], 1L, 10L)
  u <- unique(d10)
  parsed <- as.Date(u, format = "%Y-%m-%d")[match(d10, u)]
  moved <- format(parsed + days[idx], "%Y-%m-%d")
  valid <- !is.na(parsed) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", moved) &
    !utmb_is_placeholder(parsed)
  out[idx[valid]] <- paste0(moved[valid], substr(x[idx[valid]], 11L, nchar(x[idx[valid]])))
  out[idx[is.na(parsed)]] <- NA_character_
  blanked <- sum(present & is.na(out))
  unchanged <- sum(!is.na(parsed) & !valid)
  utmb_shift_result(out, sum(valid), unchanged, blanked)
}

# Shifts YYYYMMDD keys. Values that are not 8 digits, negative, or not a real date stay as they
# were (Epic's "unknown" sentinels).
utmb_shift_datekey <- function(x, days) {
  x <- as.character(x)
  out <- x
  idx <- which(!is.na(x) & grepl("^[0-9]{8}$", x))
  u <- unique(x[idx])
  parsed <- as.Date(u, format = "%Y%m%d")[match(x[idx], u)]
  moved <- format(parsed + days[idx], "%Y%m%d")
  valid <- !is.na(parsed) & grepl("^[0-9]{8}$", moved) & !utmb_is_placeholder(parsed)
  out[idx[valid]] <- moved[valid]
  utmb_shift_result(out, sum(valid), sum(!is.na(x)) - sum(valid), 0L)
}

# ---- policy -----------------------------------------------------------------------------------

utmb_staff_pattern <- paste0("(Provider|Provi|Employee|Surgeon|Anesthesiologist|Circulator|",
                             "Resident|Nurse|Attending|MidLevel|Midlevel|SurgicalTech|",
                             "CaseManager|User)")

# Key rules in order; the first matching pattern wins. Entries: pattern, action, domain, reason.
utmb_deid_key_rules <- list(
  list("^Patient(SourceDataDurable|Durable)?Key$", "drop", NA_character_,
       "patient key, StudyID replaces it"),
  list("SurgicalCaseKey$|^PrecedingCase(BySurgeon|InRoom)Key$", "remap", "surgical_case",
       "patient-level key"),
  list(utmb_staff_pattern, "remap", "person_staff", "provider or staff key"),
  list("^Guarantor(Durable)?Key$", "remap", "guarantor", "account key"),
  list("^PortalAccount(Durable)?Key$", "remap", "portal_account", "account key"),
  list("CoverageKey$", "remap", "coverage", "coverage key"),
  list("EncounterKey$", "remap", "encounter", "patient-level key"),
  list("^HospitalAdmissionKey$", "remap", "hospital_admission", "patient-level key"),
  list("^(Previous|Next)?EdVisitKey$", "remap", "ed_visit", "patient-level key"),
  list("^MedicationOrderKey$", "remap", "medication_order", "patient-level key"),
  list("^MedicationAdministrationKey$", "remap", "medication_administration",
       "patient-level key"),
  list("^LabComponentResultKey$", "remap", "lab_result", "patient-level key"),
  list("^FlowsheetValueKey$", "remap", "flowsheet_value", "patient-level key"),
  list("^DiagnosisEventKey$", "remap", "diagnosis_event", "patient-level key"),
  list("^ProcedureEventKey$", "remap", "procedure_event", "patient-level key"),
  list("^SurgicalProcedureEventKey$", "remap", "surgical_procedure_event", "patient-level key"),
  list("^PatientLocationEventKey$", "remap", "patient_location_event", "patient-level key"),
  list("^LdaKey$", "remap", "lda", "patient-level key"),
  list("^PumpDocumentationKey$", "remap", "pump_documentation", "patient-level key"),
  list("^AdmissionBedRequestKey$", "remap", "bed_request", "patient-level key"),
  list(paste0("Department|ProcedureCode|CategoryKey$|ComboKey$|^(Primary)?Procedure(Durable)?Key$|",
              "^(Primary|Second|Third|Fourth|Fifth)ComponentKey$|^MedicationKey$|Pharmacy|",
              "^LabComponentKey$|^LabOrganismKey$|^LabTestingSourceKey$|^ResultingLab|",
              "FlowsheetRowKey$|^FlowsheetTemplateKey$|DiagnosisKey$|^AgeKey$|^SourceKey$|",
              "Drg|^OperatingRoomKey$|ChiefComplaintKey$|^LoincKey$|^DurationKey$|",
              "^VisitTypeKey$|^PlaceOfServiceKey$|PrimaryCareAreaKey$|",
              "^LevelOfService(Durable)?Key$|",
              "^External(Diagnosis|Loinc|Procedure)ConceptKey$|Modifier(Category|Concept)Key$|",
              "^OrdersetKey$|^MedicationOrderTemplateKey$|^PrimaryCoded(Procedure|Diagnosis)Key$"),
       "keep", NA_character_, "reference dimension key")
)

# Classifies a surrogate key column: list(action, domain, reason).
classify_utmb_key <- function(name) {
  name <- utmb_base_name(name)
  for (rule in utmb_deid_key_rules) {
    if (grepl(rule[[1]], name)) {
      return(list(action = rule[[2]], domain = rule[[3]], reason = rule[[4]]))
    }
  }
  list(action = "drop", domain = NA_character_, reason = "unclassified key, review")
}

# Action for one dictionary column: list(action, domain, reason).
utmb_deid_rule <- function(table, column, type, role) {
  name <- utmb_base_name(column)
  act <- function(action, reason = "", domain = NA_character_) {
    list(action = action, domain = domain, reason = reason)
  }
  if (role == "study_id") {
    return(act("drop", "extract study id, replaced by a new patient id"))
  }
  if (grepl("NumericId$", name)) return(act("drop", "identifier: Epic record id (NumericId)"))
  if (name %in% c("BirthDate", "DeathDate")) return(act("shift_date", "date shifted"))
  if (name == "DeathInstant") return(act("shift_instant", "date shifted"))
  if (name == "DOB") return(act("drop", "identifier, BirthDate in demographics is shifted"))
  if (role == "identifier") return(act("drop", "identifier"))
  if (role == "free_text") return(act("drop", "free text"))
  if (role == "audit") return(act("drop", "audit field"))
  if (grepl("Identifier$|SerialNumber$", name)) return(act("drop", "identifier-like name, review"))
  if (role == "date_key") return(act("shift_datekey", "date shifted"))
  if (role == "time_key") return(act("keep", "time of day"))
  if (role == "instant" || type %in% c("date", "datetime")) {
    return(act(if (type == "date") "shift_date" else "shift_instant", "date shifted"))
  }
  if (role == "surrogate_key") {
    r <- classify_utmb_key(name)
    return(act(r$action, r$reason, r$domain))
  }
  if (grepl("User[Ii][Dd]", name)) return(act("remap", "staff user id", "person_staff"))
  if (table == "flowsheets" && name == "Value") {
    return(act("keep_typed_or_common_values",
               "free text possible: listed RowValueType kept, others common values only"))
  }
  if ((table == "labs" && name == "Value") || (table == "flowsheets" && name == "TimeValue") ||
        (table == "medication_orders" && (name == "Sig" || grepl("PrnReason$", name)))) {
    return(act("keep_common_values", "free text possible: numeric-like or common values only"))
  }
  if (role == "other" && type == "text") return(act("keep", "text, review"))
  if (role == "other" && type == "empty") return(act("keep", "empty in the extract"))
  act("keep")
}

# Policy data frame (table, column, action, domain, reason) from the data dictionary.
build_utmb_deid_policy <- function(dictionary) {
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  rules <- lapply(seq_len(nrow(dictionary)), function(i) {
    utmb_deid_rule(dictionary$table[i], dictionary$column[i], dictionary$inferred_type[i],
                   dictionary$role[i])
  })
  data.frame(
    table = dictionary$table, column = dictionary$column,
    action = vapply(rules, `[[`, "", "action"),
    domain = vapply(rules, `[[`, "", "domain"),
    reason = vapply(rules, `[[`, "", "reason"),
    stringsAsFactors = FALSE
  )
}

read_utmb_deid_policy <- function(path) {
  if (!file.exists(path)) stop("Policy file not found: ", path, call. = FALSE)
  policy <- as.data.frame(data.table::fread(path, colClasses = "character", na.strings = "",
                                            data.table = FALSE))
  needed <- c("table", "column", "action", "domain", "reason")
  if (!all(needed %in% names(policy))) {
    stop("The policy file needs the columns: ", paste(needed, collapse = ", "), call. = FALSE)
  }
  bad <- setdiff(unique(policy$action), utmb_deid_actions)
  if (length(bad) > 0) {
    stop("Unknown action in the policy file: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(policy[c("table", "column")]) > 0) {
    stop("The policy file lists a table and column more than once.", call. = FALSE)
  }
  if (any(policy$action == "remap" & is.na(policy$domain))) {
    stop("Every remap row in the policy file needs a domain.", call. = FALSE)
  }
  policy
}

# Writes a fresh policy to <root>/output/utmb/deid_policy_<date>_<time>[_<label>]/. The committed
# copy in metadata/ is updated by hand, as with the data dictionary. Returns the file path.
run_utmb_deid_policy <- function(root, dictionary_path = NULL, label = NULL) {
  if (is.null(dictionary_path)) {
    dictionary_path <- file.path(root, "metadata", "utmb_dictionary.csv")
  }
  if (!file.exists(dictionary_path)) stop("Dictionary not found: ", dictionary_path, call. = FALSE)
  dictionary <- data.table::fread(dictionary_path, colClasses = "character", na.strings = "",
                                  data.table = FALSE)
  run_dir <- utmb_run_dir(root, "deid_policy", label)
  path <- file.path(run_dir, utmb_deid_policy_file)
  data.table::fwrite(build_utmb_deid_policy(dictionary), path, na = "")
  path
}

# Keeps `exempt` values and numeric-like values (optional comparison sign, optional minus, plain
# or decimal number, after trimming) and values found for at least `min_patients` distinct ids
# among the other values. Everything else becomes missing.
utmb_keep_common_values <- function(x, ids, exempt = rep(FALSE, length(x)),
                                    min_patients = utmb_deid_min_patients) {
  x <- as.character(x)
  present <- !is.na(x)
  numeric_like <- present &
    grepl("^(<=|>=|<|>)?[[:space:]]*-?([0-9]+[.]?[0-9]*|[.][0-9]+)$", trimws(x))
  candidate <- present & !exempt & !numeric_like
  common <- rep(FALSE, length(x))
  if (any(candidate)) {
    pairs <- unique(data.frame(v = x[candidate], id = ids[candidate], stringsAsFactors = FALSE))
    n_ids <- table(pairs$v)
    common[candidate] <- x[candidate] %in% names(n_ids)[n_ids >= min_patients]
  }
  keep <- !present | exempt | numeric_like | common
  out <- x
  out[!keep] <- NA_character_
  utmb_shift_result(out, 0L, sum(present & keep), sum(present & !keep))
}

# ---- one table --------------------------------------------------------------------------------

# Applies the policy to one table (a data.table of character columns). `id_map` is a character
# vector of new patient ids named by patient key value. The patient key column and the extract's
# StudyID column are always dropped; the new id is written first as StudyID. Returns the
# de-identified table, per column counts, row counts and what the leak check needs.
deid_utmb_table <- function(dt, table, policy, secret, id_map,
                            patient_key = utmb_default_patient_key) {
  if (!patient_key %in% names(dt)) {
    stop("Table ", table, " has no ", patient_key, " column.", call. = FALSE)
  }
  pol <- policy[policy$table == table, , drop = FALSE]
  cols <- names(dt)
  listed <- cols %in% pol$column
  rows_in <- nrow(dt)
  key <- dt[[patient_key]]
  new_id <- unname(id_map[match(key, names(id_map))])

  leak <- list(exact = NULL, names = NULL, dob = NULL)
  pair <- function(values) {
    has <- !is.na(new_id) & !is.na(values)
    unique(data.table::data.table(StudyID = new_id[has], value = values[has]))
  }
  # The patient key is dropped whatever the policy says, so it is added to the exact pairs here.
  leak$exact <- pair(key)
  for (col in cols[listed]) {
    row <- pol[match(col, pol$column), ]
    base <- utmb_base_name(col)
    if (row$action == "drop" && isTRUE(startsWith(row$reason, "identifier")) &&
          !base %in% utmb_deid_date_identifiers) {
      pairs <- pair(dt[[col]])
      leak$exact <- unique(rbind(leak$exact, pairs))
      if (base == "PatientName") {
        pairs$value <- tolower(pairs$value)
        leak$names <- unique(rbind(leak$names, pairs))
      }
    }
    if (base %in% utmb_deid_date_identifiers) {
      has <- !is.na(new_id) & !is.na(dt[[col]])
      leak$dob <- rbind(leak$dob, data.frame(StudyID = new_id[has], dob = dt[[col]][has],
                                             stringsAsFactors = FALSE))
    }
  }

  keep_rows <- !is.na(key) & !is.na(new_id)
  if (!all(keep_rows)) dt <- dt[keep_rows, ]
  days <- utmb_deid_offsets(key[keep_rows], secret)
  new_id <- new_id[keep_rows]

  always_dropped <- c(patient_key, utmb_deid_id_column)
  stats <- vector("list", length(cols))
  for (i in seq_along(cols)) {
    col <- cols[i]
    s <- data.frame(table = table, column = col, action = "drop", domain = NA_character_,
                    status = "unlisted_dropped", n_changed = 0L, n_unchanged = 0L,
                    n_blanked = 0L, stringsAsFactors = FALSE)
    if (col %in% always_dropped) {
      s$status <- "dropped"
    } else if (listed[i]) {
      row <- pol[match(col, pol$column), ]
      s$action <- row$action
      s$domain <- row$domain
      s$status <- if (row$action == "drop") "dropped" else "written"
      res <- NULL
      if (row$action %in% c("shift_date", "shift_instant")) {
        res <- utmb_shift_iso(dt[[col]], days)
      } else if (row$action == "shift_datekey") {
        res <- utmb_shift_datekey(dt[[col]], days)
      } else if (row$action == "remap") {
        old <- dt[[col]]
        new <- utmb_deid_remap(old, row$domain, secret)
        res <- utmb_shift_result(new, sum(!is.na(new) & new != old), sum(!is.na(new) & new == old),
                                 0L)
      } else if (row$action %in% c("keep_common_values", "keep_typed_or_common_values")) {
        exempt <- rep(FALSE, nrow(dt))
        if (row$action == "keep_typed_or_common_values" && "RowValueType" %in% cols) {
          exempt <- !is.na(dt[["RowValueType"]]) &
            dt[["RowValueType"]] %in% utmb_flowsheet_value_types
        }
        res <- utmb_keep_common_values(dt[[col]], new_id, exempt)
      }
      if (!is.null(res)) {
        data.table::set(dt, j = col, value = res$value)
        s$n_changed <- res$changed
        s$n_unchanged <- res$unchanged
        s$n_blanked <- res$blanked
      }
    }
    stats[[i]] <- s
  }
  stats <- do.call(rbind, stats)
  absent <- pol[!pol$column %in% cols, , drop = FALSE]
  if (nrow(absent) > 0) {
    missing_rows <- data.frame(table = table, column = absent$column, action = absent$action,
                               domain = absent$domain, status = "absent_in_data",
                               n_changed = 0L, n_unchanged = 0L, n_blanked = 0L,
                               stringsAsFactors = FALSE)
    stats <- rbind(stats, missing_rows)
  }
  out_cols <- stats$column[stats$status == "written"]
  dt <- dt[, out_cols, with = FALSE]
  data.table::set(dt, j = utmb_deid_id_column, value = new_id)
  data.table::setcolorder(dt, utmb_deid_id_column)
  list(data = dt, stats = stats, rows_in = rows_in, rows_out = nrow(dt),
       rows_no_key = rows_in - sum(keep_rows), leak = leak, unlisted = cols[!listed])
}

# ---- leak check -------------------------------------------------------------------------------

# Exact matches in these columns are reported for review but do not fail the run: a catalog code
# (reference dimension key, flowsheet row id) shared by many patients can equal one patient's own
# case number or CSN by chance, because Epic id spaces overlap.
utmb_leak_review_reasons <- "reference dimension key"
utmb_leak_review_columns <- list(flowsheets = "FlowsheetRowEpicId")

utmb_leak_review_only <- function(table, column, reason) {
  isTRUE(reason %in% utmb_leak_review_reasons) ||
    utmb_base_name(column) %in% utmb_leak_review_columns[[table]]
}

# Scans the written tables. Counts are cells equal to an original identifier value of the same
# patient (the row's own new StudyID, the first column of every output table, which is exempt
# from the checks): a dropped identifier value or the original patient key ("exact"), the patient
# name in any case ("name"), and the original birth date in an unshifted column
# ("own_birth_date"). Matching is per patient because Epic id spaces overlap, so another patient's
# id can equal a reference key by chance. leak$exact and leak$names are tables of StudyID and
# value (names lowercased). Also lists every written column that the policy does not allow
# ("not_allowed"). Exact matches in catalog code columns (see utmb_leak_review_only) go to
# `review` instead of `leaks`: cells, distinct colliding values and the fewest distinct patients
# that have any one colliding value in the column. Counts only.
utmb_deid_leak_check <- function(out_data_dir, policy, leak) {
  leaks <- list()
  review <- list()
  violations <- list()
  exact <- leak$exact
  lower_names <- leak$names
  matched_cells <- function(pairs, sid, x) {
    probe <- data.table::data.table(StudyID = sid, value = x, cell = seq_along(x))
    probe <- probe[!is.na(x) & !is.na(sid)]
    probe[pairs, on = c("StudyID", "value"), nomatch = 0L, cell]
  }
  match_count <- function(pairs, sid, x) length(matched_cells(pairs, sid, x))
  dob <- leak$dob
  files <- utmb_table_files(out_data_dir)
  for (table in names(files)) {
    dt <- read_utmb_table(files[[table]])
    for (col in setdiff(names(dt), utmb_deid_id_column)) {
      row <- policy[policy$table == table & policy$column == col, , drop = FALSE]
      if (nrow(row) != 1L || row$action == "drop") {
        violations[[length(violations) + 1L]] <- data.frame(table = table, column = col,
                                                            check = "not_allowed", n = 1L)
        next
      }
      x <- dt[[col]]
      present <- !is.na(x)
      # A shifted date can equal an 8-digit Epic ID by chance; it cannot carry the original ID.
      shifted <- row$action %in% c("shift_date", "shift_instant", "shift_datekey")
      if (!is.null(exact) && !shifted) {
        cells <- matched_cells(exact, dt[["StudyID"]], x)
        if (length(cells) > 0 && utmb_leak_review_only(table, col, row$reason)) {
          values <- unique(x[cells])
          all_cells <- data.table::data.table(sid = dt[["StudyID"]], value = x)
          holders <- unique(all_cells[value %in% values])
          item <- data.frame(table = table, column = col, n_cells = length(cells),
                             n_values = length(values), min_patients = min(table(holders$value)))
          review[[length(review) + 1L]] <- item
        } else if (length(cells) > 0) {
          leaks[[length(leaks) + 1L]] <- data.frame(table = table, column = col,
                                                    check = "exact", n = length(cells))
        }
      }
      if (!is.null(lower_names)) {
        n <- match_count(lower_names, dt[["StudyID"]], tolower(x))
        if (n > 0) leaks[[length(leaks) + 1L]] <- data.frame(table = table, column = col,
                                                             check = "name", n = n)
      }
      if (!is.null(dob) && row$action %in% utmb_deid_unshifted_actions) {
        own <- dob$dob[match(dt[["StudyID"]], dob$StudyID)]
        n <- sum(present & !is.na(own) & x == own)
        if (n > 0) {
          leaks[[length(leaks) + 1L]] <- data.frame(table = table, column = col,
                                                    check = "own_birth_date", n = n)
        }
      }
    }
    rm(dt)
  }
  list(leaks = do.call(rbind, leaks), violations = do.call(rbind, violations),
       review = do.call(rbind, review))
}

# ---- run --------------------------------------------------------------------------------------

utmb_deid_keys_readme <- c(
  "DATE KEYS: this folder can re-identify patients and dates.",
  "",
  "patients.csv links each new StudyID to the Epic patient (the patient key column and, if the",
  "extract had one, extract_StudyID) and lists the date shift in days of that patient.",
  "secret.txt holds the run secret. With either file, de-identified dates can be moved back to",
  "the real dates, and keys replaced by pseudonyms can be matched to the real keys.",
  "",
  "Store this folder separately from the de-identified data and never share it.",
  "To give a later extract the same patient ids, offsets and pseudonyms, run the script with",
  "--keys <path to this folder>."
)

utmb_read_key_column <- function(path, columns) {
  data.table::fread(path, select = columns, colClasses = "character",
                    na.strings = utmb_missing_strings, showProgress = FALSE, data.table = TRUE)
}

# Reads patients.csv of an earlier run. Stops if a patient key has two ids or an id is used twice.
read_utmb_patients <- function(path, patient_key) {
  if (!file.exists(path)) stop("Patient file not found: ", path, call. = FALSE)
  old <- as.data.frame(data.table::fread(path, colClasses = "character", na.strings = "",
                                         data.table = FALSE))
  needed <- c(utmb_deid_id_column, patient_key)
  if (!all(needed %in% names(old))) {
    stop("The patient file ", path, " needs the columns: ", paste(needed, collapse = ", "),
         call. = FALSE)
  }
  id <- old[[utmb_deid_id_column]]
  key <- old[[patient_key]]
  problems <- character(0)
  if (anyNA(id) || !all(grepl("^[1-9][0-9]*$", id[!is.na(id)]))) {
    problems <- c(problems, "StudyID is missing or not a positive whole number in some rows")
  }
  if (anyNA(key)) problems <- c(problems, "the patient key is missing in some rows")
  if (anyDuplicated(key[!is.na(key)]) > 0) {
    problems <- c(problems, paste0(sum(duplicated(key[!is.na(key)])),
                                   " row(s) repeat a patient key that has another StudyID"))
  }
  if (anyDuplicated(id[!is.na(id)]) > 0) {
    problems <- c(problems, paste0(sum(duplicated(id[!is.na(id)])),
                                   " row(s) repeat a StudyID used for another patient"))
  }
  if (length(problems) > 0) {
    stop("The patient file ", path, " is inconsistent: ", paste(problems, collapse = "; "), ".",
         call. = FALSE)
  }
  old
}

# First pass over the extract, before anything is written. Reads only the patient key column of
# each table (plus StudyID in demographics) and stops, naming tables and counts, unless
# demographics has exactly one row per patient and every other table's keys are in demographics.
# Numbers the patients 1..N in the order of SHA-256(secret, "patient_id", key); with `previous`
# (patients.csv of an earlier run) known patients keep their id and new ones continue from the
# largest id. Returns list(patients, id_map, n_patients, n_new).
utmb_deid_prepare_patients <- function(files, patient_key, secret, previous = NULL) {
  problems <- character(0)
  if (patient_key %in% c(utmb_deid_id_column, utmb_deid_extract_id_column, "offset_days")) {
    stop("The patient key column cannot be called ", patient_key, ".", call. = FALSE)
  }
  if (!"demographics" %in% names(files)) {
    stop("The data folder has no demographics table, which defines the patients.", call. = FALSE)
  }
  header <- lapply(files, function(f) names(read_utmb_table(f, header_only = TRUE)))
  no_key <- names(files)[!vapply(header, function(h) patient_key %in% h, logical(1))]
  if (length(no_key) > 0) {
    problems <- c(problems, paste0("table ", no_key, " has no ", patient_key, " column"))
  }
  demo <- NULL
  if (!"demographics" %in% no_key) {
    has_extract_id <- utmb_deid_id_column %in% header$demographics
    demo <- utmb_read_key_column(files[["demographics"]],
                                 c(patient_key, if (has_extract_id) utmb_deid_id_column))
    keys <- demo[[patient_key]]
    n_missing <- sum(is.na(keys))
    n_repeated <- sum(duplicated(keys[!is.na(keys)]))
    if (n_missing > 0) {
      problems <- c(problems, paste0("demographics: ", n_missing, " row(s) have no ", patient_key))
    }
    if (n_repeated > 0) {
      problems <- c(problems, paste0("demographics: ", n_repeated, " row(s) repeat a ", patient_key,
                                     " (each patient needs exactly one row)"))
    }
    if (n_missing == 0 && n_repeated == 0) {
      for (table in setdiff(names(files), c("demographics", no_key))) {
        other <- utmb_read_key_column(files[[table]], patient_key)[[1]]
        n_unmatched <- sum(!is.na(other) & !other %in% keys)
        rm(other)
        if (n_unmatched > 0) {
          problems <- c(problems, paste0("table ", table, ": ", n_unmatched,
                                         " row(s) have a ", patient_key,
                                         " that is not in demographics"))
        }
      }
    }
  }
  if (length(problems) > 0) {
    stop("The patient key checks failed, nothing was written:\n  ",
         paste(problems, collapse = "\n  "), call. = FALSE)
  }

  keys <- demo[[patient_key]]
  extract_id <- if (utmb_deid_id_column %in% names(demo)) demo[[utmb_deid_id_column]] else NULL
  by_hash <- order(utmb_sha256(paste(secret, "patient_id", keys, sep = "|")), method = "radix")
  keys_sorted <- keys[by_hash]
  extract_sorted <- if (is.null(extract_id)) NULL else extract_id[by_hash]
  if (is.null(previous)) {
    ids <- seq_along(keys_sorted)
    n_new <- length(keys_sorted)
  } else {
    known <- match(keys_sorted, previous[[patient_key]])
    ids <- rep(NA_integer_, length(keys_sorted))
    ids[!is.na(known)] <- as.integer(previous[[utmb_deid_id_column]][known[!is.na(known)]])
    n_new <- sum(is.na(known))
    ids[is.na(known)] <- max(c(0L, as.integer(previous[[utmb_deid_id_column]]))) + seq_len(n_new)
  }
  current <- data.frame(StudyID = as.character(ids), key = keys_sorted, stringsAsFactors = FALSE)
  names(current)[2] <- patient_key
  if (!is.null(extract_sorted)) current[[utmb_deid_extract_id_column]] <- extract_sorted

  patients <- current
  if (!is.null(previous)) {
    kept_old <- previous[!previous[[patient_key]] %in% keys, , drop = FALSE]
    if (utmb_deid_extract_id_column %in% names(previous)) {
      known <- match(current[[patient_key]], previous[[patient_key]])
      if (is.null(extract_sorted)) {
        current[[utmb_deid_extract_id_column]] <- previous[[utmb_deid_extract_id_column]][known]
      } else {
        old_value <- previous[[utmb_deid_extract_id_column]][known]
        current[[utmb_deid_extract_id_column]] <- ifelse(is.na(extract_sorted), old_value,
                                                         extract_sorted)
      }
    }
    if (utmb_deid_extract_id_column %in% names(current) &&
          !utmb_deid_extract_id_column %in% names(kept_old)) {
      kept_old[[utmb_deid_extract_id_column]] <- rep(NA_character_, nrow(kept_old))
    }
    cols <- names(current)
    patients <- rbind(current[cols], kept_old[cols])
  }
  patients <- patients[order(as.integer(patients$StudyID)), , drop = FALSE]
  patients$offset_days <- utmb_deid_offsets(patients[[patient_key]], secret)
  rownames(patients) <- NULL
  id_map <- stats::setNames(current$StudyID, current[[patient_key]])
  list(patients = patients, id_map = id_map, n_patients = length(keys), n_new = n_new)
}

# De-identifies every UTMB table in `data_dir`. `patient_key` names the column that identifies a
# patient in every table; demographics must hold exactly one row per key value and all other
# tables only keys found there, else nothing is written. Patients get a new number, written as
# the first column StudyID. Writes the tables, summary.txt and column_actions.csv to
# <root>/output/utmb/deid_<date>_<time>[_<label>]/ and secret.txt, patients.csv and README.txt
# to the separate output/utmb/deid_keys_<date>_<time>[_<label>]/. `keys_folder` is the keys
# folder of an earlier run whose secret and patient numbers are reused. Nothing is written unless
# `root` is confirmed by PROJECT_VERSION. Returns a list with run_dir, keys_dir, ok, leaks,
# violations, review and unlisted; ok is FALSE when the leak check found anything other than
# review items (leak_review.csv).
run_utmb_deid <- function(data_dir, root, label = NULL, keys_folder = NULL,
                          patient_key = utmb_default_patient_key, policy_path = NULL,
                          progress = function(...) cat(..., "\n", sep = "")) {
  if (!is_project_root(root)) {
    stop("Project folder not confirmed: no PROJECT_VERSION with 'Project: ", expected_project_name,
         "' in the given folder. Nothing was written.", call. = FALSE)
  }
  if (is.null(policy_path)) {
    policy_path <- file.path(root, "metadata", utmb_deid_policy_file)
  }
  policy <- read_utmb_deid_policy(policy_path)
  files <- utmb_table_files(data_dir)
  if (length(files) == 0) stop("No UTMB tables found in ", data_dir, call. = FALSE)
  previous <- NULL
  if (is.null(keys_folder)) {
    secret <- new_utmb_secret()
  } else {
    secret <- read_utmb_secret(file.path(keys_folder, "secret.txt"))
    previous <- read_utmb_patients(file.path(keys_folder, "patients.csv"), patient_key)
  }
  prep <- utmb_deid_prepare_patients(files, patient_key, secret, previous)

  keys_dir <- utmb_run_dir(root, "deid_keys", label)
  run_dir <- utmb_run_dir(root, "deid", label)
  out_data_dir <- file.path(run_dir, "data")
  dir.create(out_data_dir, showWarnings = FALSE)
  writeLines(secret, file.path(keys_dir, "secret.txt"))
  writeLines(utmb_deid_keys_readme, file.path(keys_dir, "README.txt"))
  data.table::fwrite(prep$patients, file.path(keys_dir, "patients.csv"), na = "")

  started <- Sys.time()
  step <- function(text) {
    progress(sprintf("[%.0f s] %s", as.numeric(difftime(Sys.time(), started, units = "secs")),
                     text))
  }
  stats <- list()
  counts <- list()
  leak <- list(exact = NULL, names = NULL, dob = NULL)
  for (table in names(files)) {
    res <- deid_utmb_table(read_utmb_table(files[[table]]), table, policy, secret, prep$id_map,
                           patient_key)
    data.table::fwrite(res$data, file.path(out_data_dir, table), na = "")
    stats[[table]] <- res$stats
    counts[[table]] <- data.frame(table = table, rows_in = res$rows_in, rows_out = res$rows_out,
                                  rows_no_key = res$rows_no_key, stringsAsFactors = FALSE)
    leak$exact <- unique(rbind(leak$exact, res$leak$exact))
    leak$names <- unique(rbind(leak$names, res$leak$names))
    leak$dob <- rbind(leak$dob, res$leak$dob)
    step(sprintf("%s: %s rows in, %s rows out", table, format(res$rows_in, big.mark = ","),
                 format(res$rows_out, big.mark = ",")))
    rm(res)
    invisible(gc())
  }
  stats <- do.call(rbind, stats)
  counts <- do.call(rbind, counts)
  rownames(stats) <- rownames(counts) <- NULL
  absent_tables <- setdiff(unique(policy$table), names(files))

  data.table::fwrite(stats, file.path(run_dir, "column_actions.csv"), na = "")

  step("leak check")
  check <- utmb_deid_leak_check(out_data_dir, policy, leak)
  rm(leak)
  ok <- is.null(check$leaks) && is.null(check$violations)
  if (!is.null(check$review)) {
    data.table::fwrite(check$review, file.path(run_dir, "leak_review.csv"), na = "")
  }
  unlisted <- stats[stats$status == "unlisted_dropped", c("table", "column"), drop = FALSE]

  info <- list(patient_key = patient_key, n_patients = prep$n_patients,
               n_new = if (is.null(previous)) NULL else prep$n_new,
               n_in_file = nrow(prep$patients), keys_name = basename(keys_dir),
               reused = !is.null(keys_folder))
  writeLines(utmb_deid_summary(files, counts, stats, unlisted, absent_tables, check, ok, info),
             file.path(run_dir, "summary.txt"))
  if (nrow(unlisted) > 0) {
    warning(nrow(unlisted), " column(s) not listed in the policy were dropped: ",
            paste(paste0(unlisted$table, ".", unlisted$column), collapse = ", "), call. = FALSE)
  }
  list(run_dir = run_dir, keys_dir = keys_dir, ok = ok, leaks = check$leaks,
       violations = check$violations, review = check$review, unlisted = unlisted)
}

utmb_deid_summary <- function(files, counts, stats, unlisted, absent_tables, check, ok, info) {
  lines <- c(
    "UTMB de-identification",
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Secret: ", if (info$reused) "reused from a given keys folder" else "newly generated",
           "; keys folder output/utmb/", info$keys_name, " (store apart, never share)"),
    paste0("Patient key: ", info$patient_key),
    paste0("Patients: ", info$n_patients, " in this extract, ", info$n_in_file, " in patients.csv"),
    if (!is.null(info$n_new)) paste0("New patients not in the earlier keys folder: ", info$n_new),
    "Patient key checks passed: demographics has one row per patient (no missing or repeated key),",
    "  every table has the key column, every key in another table is in demographics",
    "This file holds counts only.", ""
  )
  for (table in names(files)) {
    s <- stats[stats$table == table, ]
    n <- function(status) sum(s$status == status)
    n_action <- function(a) sum(s$status == "written" & s$action == a)
    cnt <- counts[counts$table == table, ]
    lines <- c(lines,
      paste0("Table ", table),
      paste0("  rows in: ", cnt$rows_in, ", rows out: ", cnt$rows_out,
             ", rows dropped for missing patient key: ", cnt$rows_no_key),
      paste0("  columns kept unchanged: ", n_action("keep"),
             ", dropped: ", n("dropped") + n("unlisted_dropped"),
             ", dates shifted: ", n_action("shift_date") + n_action("shift_instant") +
               n_action("shift_datekey"),
             ", keys remapped: ", n_action("remap"),
             ", text columns filtered: ", n_action("keep_common_values") +
               n_action("keep_typed_or_common_values")),
      paste0("  shifted or remapped values: ", sum(s$n_changed),
             ", date values left unchanged (sentinels, out of range): ",
             sum(s$n_unchanged[s$action %in% c("shift_date", "shift_instant",
                                               "shift_datekey")]),
             ", date values blanked (not ISO dates): ",
             sum(s$n_blanked[s$action %in% c("shift_date", "shift_instant")])),
      paste0("  text values blanked (not numeric-like, fewer than ", utmb_deid_min_patients,
             " patients): ",
             sum(s$n_blanked[s$action %in% c("keep_common_values",
                                             "keep_typed_or_common_values")])),
      if (table == "flowsheets") {
        paste0("  flowsheet Value blanked: ",
               sum(s$n_blanked[s$action == "keep_typed_or_common_values"]))
      },
      paste0("  columns in the data but not in the policy (dropped): ", n("unlisted_dropped")),
      ""
    )
  }
  lines <- c(lines, "Columns not listed in the policy (dropped):")
  unlisted_lines <- if (nrow(unlisted) == 0) {
    "  none"
  } else {
    paste0("  ", unlisted$table, ".", unlisted$column)
  }
  table_lines <- if (length(absent_tables) == 0) "  none" else paste0("  ", absent_tables)
  n_absent <- sum(stats$status == "absent_in_data")
  lines <- c(lines, unlisted_lines, "", "Policy rows for tables not found in the data folder:",
             table_lines, "",
             paste0("Policy rows for columns absent from the data: ", n_absent,
                    " (see column_actions.csv)"),
             "", "Leak check")
  if (is.null(check$leaks) && is.null(check$violations)) {
    lines <- c(lines, "  passed: no original identifier value found in any output file",
               "  passed: every output column is in the policy with an action other than drop")
  } else {
    if (!is.null(check$leaks)) {
      lines <- c(lines, "  FAILED: matches with original identifier values (cell counts):",
                 paste0("    ", check$leaks$table, ".", check$leaks$column, " [",
                        check$leaks$check, "]: ", check$leaks$n))
    }
    if (!is.null(check$violations)) {
      lines <- c(lines, "  FAILED: output columns the policy does not allow:",
                 paste0("    ", check$violations$table, ".", check$violations$column))
    }
  }
  if (!is.null(check$review)) {
    r <- check$review
    lines <- c(lines, "",
               "Leak check: review items (catalog code columns, do not fail the run)",
               paste0("  ", r$table, ".", r$column, ": cells ", r$n_cells, ", distinct values ",
                      r$n_values, ", fewest patients per value ", r$min_patients),
               "  details in leak_review.csv")
  }
  c(lines, "", paste0("Result: ", if (ok) "OK" else "FAILED, do not use these files"))
}
# nolint end
