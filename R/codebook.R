codebook_required_columns <- c(
  "position", "source_name", "section", "name", "type", "unit", "allowed_values",
  "time_window", "dictionary_name", "key_variable", "notes"
)

codebook_types <- c("numeric", "integer", "binary", "categorical", "date", "time", "text")

# Reads metadata/codebook.csv and checks it is usable by the loader and later QC steps.
read_codebook <- function(path) {
  if (!file.exists(path)) stop("Codebook file not found: ", path, call. = FALSE)
  cb <- utils::read.csv(
    path, colClasses = "character", na.strings = "NA", stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )

  missing_cols <- setdiff(codebook_required_columns, names(cb))
  if (length(missing_cols) > 0) {
    stop("Codebook is missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  position <- suppressWarnings(as.integer(cb$position))
  if (anyNA(position)) {
    stop("Codebook column 'position' must hold whole numbers in every row.", call. = FALSE)
  }
  if (anyDuplicated(position) > 0) {
    stop("Codebook 'position' is not unique: ",
         paste(unique(position[duplicated(position)]), collapse = ", "), call. = FALSE)
  }
  if (anyNA(cb$name) || any(!nzchar(cb$name))) {
    stop("Codebook 'name' must be filled in every row.", call. = FALSE)
  }
  if (anyDuplicated(cb$name) > 0) {
    stop("Codebook 'name' is not unique: ",
         paste(unique(cb$name[duplicated(cb$name)]), collapse = ", "), call. = FALSE)
  }

  bad_type <- is.na(cb$type) | !(cb$type %in% codebook_types)
  if (any(bad_type)) {
    stop("Codebook has unknown 'type' values: ", paste(unique(cb$type[bad_type]), collapse = ", "),
         ". Allowed: ", paste(codebook_types, collapse = ", "), call. = FALSE)
  }

  needs_levels <- cb$type %in% c("binary", "categorical")
  no_levels <- needs_levels & (is.na(cb$allowed_values) | !nzchar(trimws(cb$allowed_values)))
  if (any(no_levels)) {
    stop("Binary and categorical rows need 'allowed_values': ",
         paste(cb$name[no_levels], collapse = ", "), call. = FALSE)
  }

  cb$position <- position
  cb$key_variable <- as.logical(cb$key_variable)
  cb[order(cb$position), , drop = FALSE]
}
