# nolint start: object_usage_linter. Functions from the other files are not visible here.
# De-identification of the UTMB extract. A policy file (metadata/utmb_deid_policy.csv) lists one
# action per table column. Dates move by one random offset per patient, linking keys are replaced
# by pseudonyms, and identifiers and free text are dropped. Offsets and pseudonyms are derived
# from one random run secret with SHA-256, so every table stays joinable and nothing has to be
# held in a lookup table.

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

# ---- run secret, offsets and pseudonyms -------------------------------------------------------

# 32 random bytes as 64 hex characters from the operating system's random source.
new_utmb_secret <- function() {
  source <- "/dev/urandom"
  if (!file.exists(source)) {
    stop("No cryptographic random source: ", source, " is not available on this system. ",
         "Run on Linux, or pass an existing secret with --secret.", call. = FALSE)
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

# One offset per StudyID: SHA-256 of secret, "offset" and the id, mapped onto the 60 allowed days.
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
  if (role == "study_id") return(act("keep", "study identifier"))
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

# Applies the policy to one table (a data.table of character columns). Returns the de-identified
# table, per column counts, row counts, the StudyIDs seen and what the leak check needs.
deid_utmb_table <- function(dt, table, policy, secret) {
  if (!"StudyID" %in% names(dt)) stop("Table ", table, " has no StudyID column.", call. = FALSE)
  pol <- policy[policy$table == table, , drop = FALSE]
  cols <- names(dt)
  listed <- cols %in% pol$column
  rows_in <- nrow(dt)

  leak <- list(exact = character(0), names = character(0), dob = NULL)
  for (col in cols[listed]) {
    row <- pol[match(col, pol$column), ]
    base <- utmb_base_name(col)
    if (row$action == "drop" && isTRUE(startsWith(row$reason, "identifier")) &&
          !base %in% utmb_deid_date_identifiers) {
      values <- unique(dt[[col]][!is.na(dt[[col]])])
      leak$exact <- c(leak$exact, values)
      if (base == "PatientName") leak$names <- c(leak$names, values)
    }
    if (base %in% utmb_deid_date_identifiers) {
      has <- !is.na(dt[["StudyID"]]) & !is.na(dt[[col]])
      leak$dob <- rbind(leak$dob, data.frame(StudyID = dt[["StudyID"]][has], dob = dt[[col]][has],
                                             stringsAsFactors = FALSE))
    }
  }

  keep_rows <- !is.na(dt[["StudyID"]])
  if (!all(keep_rows)) dt <- dt[keep_rows, ]
  ids <- unique(dt[["StudyID"]])
  days <- utmb_deid_offsets(dt[["StudyID"]], secret)

  stats <- vector("list", length(cols))
  for (i in seq_along(cols)) {
    col <- cols[i]
    s <- data.frame(table = table, column = col, action = "drop", domain = NA_character_,
                    status = "unlisted_dropped", n_changed = 0L, n_unchanged = 0L,
                    n_blanked = 0L, stringsAsFactors = FALSE)
    if (listed[i]) {
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
        res <- utmb_keep_common_values(dt[[col]], dt[["StudyID"]], exempt)
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
  list(data = dt, stats = stats, rows_in = rows_in, rows_out = nrow(dt),
       rows_no_id = rows_in - sum(keep_rows), ids = ids, leak = leak,
       unlisted = cols[!listed])
}

# ---- leak check -------------------------------------------------------------------------------

# Scans the written tables. Counts are cells equal to an original identifier value: any dropped
# identifier value anywhere ("exact"), a patient name in any case ("name"), and a patient's own
# original birth date in an unshifted column of that patient ("own_birth_date"). Also lists every
# written column that the policy does not allow ("not_allowed"). Counts only, never values.
utmb_deid_leak_check <- function(out_data_dir, policy, leak) {
  leaks <- list()
  violations <- list()
  exact <- unique(leak$exact)
  lower_names <- unique(tolower(leak$names))
  dob <- leak$dob
  files <- utmb_table_files(out_data_dir)
  for (table in names(files)) {
    dt <- read_utmb_table(files[[table]])
    for (col in names(dt)) {
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
      if (length(exact) > 0 && !shifted) {
        n <- sum(x[present] %in% exact)
        if (n > 0) leaks[[length(leaks) + 1L]] <- data.frame(table = table, column = col,
                                                             check = "exact", n = n)
      }
      if (length(lower_names) > 0) {
        u <- unique(x[present])
        hit <- u[tolower(u) %in% lower_names]
        n <- sum(x %in% hit)
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
  list(leaks = do.call(rbind, leaks), violations = do.call(rbind, violations))
}

# ---- run --------------------------------------------------------------------------------------

utmb_deid_keys_readme <- c(
  "DATE KEYS: this folder can re-identify dates.",
  "",
  "secret.txt holds the run secret. offsets.csv lists the shift in days for each StudyID.",
  "With either file, de-identified dates can be moved back to the real dates, and keys",
  "replaced by pseudonyms can be matched to the real keys.",
  "",
  "Store this folder separately from the de-identified data and never share it.",
  "To give a later extract the same offsets and pseudonyms, run the script with",
  "--secret <path to secret.txt>."
)

# De-identifies every UTMB table in `data_dir`. Writes the tables, summary.txt and
# column_actions.csv to <root>/output/utmb/deid_<date>_<time>[_<label>]/ and the secret and
# offsets to the separate output/utmb/deid_keys_<date>_<time>[_<label>]/. Nothing is written
# unless `root` is confirmed by PROJECT_VERSION. Returns a list with run_dir, keys_dir, ok,
# leaks, violations and unlisted; ok is FALSE when the leak check found anything.
run_utmb_deid <- function(data_dir, root, label = NULL, secret_file = NULL, policy_path = NULL,
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
  secret <- if (is.null(secret_file)) new_utmb_secret() else read_utmb_secret(secret_file)

  keys_dir <- utmb_run_dir(root, "deid_keys", label)
  run_dir <- utmb_run_dir(root, "deid", label)
  out_data_dir <- file.path(run_dir, "data")
  dir.create(out_data_dir, showWarnings = FALSE)
  writeLines(secret, file.path(keys_dir, "secret.txt"))
  writeLines(utmb_deid_keys_readme, file.path(keys_dir, "README.txt"))

  started <- Sys.time()
  step <- function(text) {
    progress(sprintf("[%.0f s] %s", as.numeric(difftime(Sys.time(), started, units = "secs")),
                     text))
  }
  stats <- list()
  counts <- list()
  all_ids <- character(0)
  leak <- list(exact = character(0), names = character(0), dob = NULL)
  for (table in names(files)) {
    res <- deid_utmb_table(read_utmb_table(files[[table]]), table, policy, secret)
    data.table::fwrite(res$data, file.path(out_data_dir, table), na = "")
    stats[[table]] <- res$stats
    counts[[table]] <- data.frame(table = table, rows_in = res$rows_in, rows_out = res$rows_out,
                                  rows_no_id = res$rows_no_id, stringsAsFactors = FALSE)
    all_ids <- unique(c(all_ids, res$ids))
    leak$exact <- unique(c(leak$exact, res$leak$exact))
    leak$names <- unique(c(leak$names, res$leak$names))
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

  offsets <- data.frame(StudyID = all_ids, offset_days = utmb_deid_offsets(all_ids, secret))
  data.table::fwrite(offsets, file.path(keys_dir, "offsets.csv"), na = "")
  data.table::fwrite(stats, file.path(run_dir, "column_actions.csv"), na = "")

  step("leak check")
  check <- utmb_deid_leak_check(out_data_dir, policy, leak)
  rm(leak)
  ok <- is.null(check$leaks) && is.null(check$violations)
  unlisted <- stats[stats$status == "unlisted_dropped", c("table", "column"), drop = FALSE]

  writeLines(utmb_deid_summary(files, counts, stats, unlisted, absent_tables, check, ok,
                               length(all_ids), basename(keys_dir), !is.null(secret_file)),
             file.path(run_dir, "summary.txt"))
  if (nrow(unlisted) > 0) {
    warning(nrow(unlisted), " column(s) not listed in the policy were dropped: ",
            paste(paste0(unlisted$table, ".", unlisted$column), collapse = ", "), call. = FALSE)
  }
  list(run_dir = run_dir, keys_dir = keys_dir, ok = ok, leaks = check$leaks,
       violations = check$violations, unlisted = unlisted)
}

utmb_deid_summary <- function(files, counts, stats, unlisted, absent_tables, check, ok, n_ids,
                              keys_name, reused_secret) {
  lines <- c(
    "UTMB de-identification",
    paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Secret: ", if (reused_secret) "reused from a given file" else "newly generated",
           "; keys folder output/utmb/", keys_name, " (store apart, never share)"),
    paste0("Patients (StudyID) found: ", n_ids),
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
             ", rows dropped for missing StudyID: ", cnt$rows_no_id),
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
  c(lines, "", paste0("Result: ", if (ok) "OK" else "FAILED, do not use these files"))
}
# nolint end
