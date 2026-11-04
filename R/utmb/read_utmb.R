# Reading and basic helpers for the UTMB Epic Caboodle extract: 13 comma-separated files without
# an extension, each with StudyID as the first column.

# Lets data.table's [ syntax work when this file is sourced rather than loaded as a package.
.datatable.aware <- TRUE # nolint: object_name_linter.

utmb_table_names <- c("crosswalk", "demographics", "diagnoses", "ed_visits", "encounters",
                      "flowsheets", "hospital_admissions", "labs", "medication_administrations",
                      "medication_orders", "procedures_coded", "surgical_cases",
                      "surgical_procedures")

# One event date column per table, used for the earliest/latest date in table_stats.
utmb_event_date_columns <- c(
  crosswalk = "BurnCodeDate",
  demographics = NA_character_,
  diagnoses = "StartDate",
  ed_visits = "ArrivalDateKey",
  encounters = "Date",
  flowsheets = "TakenInstant",
  hospital_admissions = "AdmissionDateKey",
  labs = "CollectionInstant",
  medication_administrations = "AdministrationInstant",
  medication_orders = "OrderedInstant",
  procedures_coded = "ProcedureStartInstant",
  surgical_cases = "SurgeryDateKey",
  surgical_procedures = "SurgeryDateKey"
)

# The extract writes missing values as the text NULL, or leaves them empty. "NA" is not treated
# as missing because it is a real value in some lab columns.
utmb_missing_strings <- c("", "NULL")

# Reads one table as a data.table of character columns. With header_only = TRUE no rows are read.
# Repeated column names (flowsheets has one) get a numeric suffix so every name is unique.
read_utmb_table <- function(path, header_only = FALSE) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  dt <- data.table::fread(path, colClasses = "character", na.strings = utmb_missing_strings,
                          nrows = if (header_only) 0L else Inf, showProgress = FALSE,
                          data.table = TRUE)
  data.table::setnames(dt, make.unique(names(dt), sep = "_"))
  dt
}

# Paths of the UTMB tables found in `data_dir`, named by table, in alphabetical order.
utmb_table_files <- function(data_dir) {
  paths <- file.path(data_dir, utmb_table_names)
  names(paths) <- utmb_table_names
  paths[file.exists(paths)]
}

# Calendar dates from ISO dates, ISO timestamps and YYYYMMDD keys. Anything else gives NA.
parse_utmb_dates <- function(x) {
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))
  key <- !is.na(x) & grepl("^[0-9]{8}$", x)
  out[key] <- as.Date(x[key], format = "%Y%m%d")
  iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  out[iso] <- as.Date(substr(x[iso], 1, 10), format = "%Y-%m-%d")
  out
}

# Number of worker processes for per-column work. Forking is not available on Windows.
utmb_cores <- function(n_tasks) {
  cores <- getOption("utmb.cores", NULL)
  if (is.null(cores)) cores <- min(32L, parallel::detectCores(), na.rm = TRUE)
  if (.Platform$OS.type == "windows") cores <- 1L
  max(1L, min(as.integer(cores), n_tasks))
}

utmb_lapply <- function(x, fun, ...) {
  cores <- utmb_cores(length(x))
  if (cores == 1L) return(lapply(x, fun, ...))
  res <- parallel::mclapply(x, fun, ..., mc.cores = cores)
  failed <- vapply(res, inherits, logical(1), what = "try-error")
  if (any(failed)) stop(as.character(res[[which(failed)[1]]]), call. = FALSE)
  res
}
