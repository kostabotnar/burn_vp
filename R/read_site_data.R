missing_strings <- c("", "NA", "N/A", "-")
excel_origin <- "1899-12-30"

# Compares file headers with the codebook by position and stops with every difference.
check_headers <- function(file_headers, codebook) {
  expected <- trimws(codebook$source_name)
  n_file <- length(file_headers)
  n_book <- length(expected)
  common <- seq_len(min(n_file, n_book))

  lines <- character()
  for (i in common[file_headers[common] != expected[common]]) {
    lines <- c(lines, sprintf("  position %d: found '%s', expected '%s'", i, file_headers[i],
                              expected[i]))
  }
  if (n_file > n_book) {
    extra <- (n_book + 1):n_file
    lines <- c(lines, sprintf("  extra column at position %d: '%s'", extra, file_headers[extra]))
  }
  if (n_book > n_file) {
    absent <- (n_file + 1):n_book
    lines <- c(lines, sprintf("  missing column at position %d: expected '%s'", absent,
                              expected[absent]))
  }
  if (length(lines) > 0) {
    stop("Column headers do not match the codebook:\n", paste(lines, collapse = "\n"),
         call. = FALSE)
  }
}

clean_text <- function(x) {
  x <- trimws(x)
  x[x %in% missing_strings] <- NA_character_
  x
}

# A comma is a decimal mark only when no dot is present, so "1,234.5" is rejected.
parse_numeric <- function(x) {
  x <- ifelse(grepl(".", x, fixed = TRUE), x, sub(",", ".", x, fixed = TRUE))
  ok <- grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(x[ok])
  out
}

parse_integer <- function(x) {
  out <- parse_numeric(x)
  out[!is.na(out) & (out != round(out) | abs(out) > .Machine$integer.max)] <- NA_real_
  as.integer(out)
}

parse_date <- function(x) {
  out <- as.Date(rep(NA_character_, length(x)))
  serial <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
  out[serial] <- as.Date(floor(as.numeric(x[serial])), origin = excel_origin)
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  out[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  out
}

parse_time <- function(x) {
  fraction <- parse_numeric(x)
  fraction[!is.na(fraction) & (fraction < 0 | fraction >= 1)] <- NA_real_
  minutes <- round(fraction * 1440) %% 1440
  out <- sprintf("%02d:%02d", as.integer(minutes %/% 60), as.integer(minutes %% 60))
  out[is.na(fraction)] <- NA_character_
  clock <- grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", x)
  out[clock] <- sprintf("%02d:%s", as.integer(sub(":.*", "", x[clock])), sub(".*:", "", x[clock]))
  out
}

parse_column <- function(x, type) {
  switch(type,
    numeric = parse_numeric(x),
    integer = parse_integer(x),
    date = parse_date(x),
    time = parse_time(x),
    x
  )
}

read_site_data <- function(path, codebook, site, sheet = 1) {
  if (!is.character(site) || length(site) != 1 || is.na(site) || !nzchar(site)) {
    stop("'site' must be a single non-empty string.", call. = FALSE)
  }
  if (!file.exists(path)) stop("Site data file not found: ", path, call. = FALSE)
  codebook <- codebook[order(codebook$position), , drop = FALSE]
  if (!identical(as.integer(codebook$position), seq_len(nrow(codebook)))) {
    stop("Codebook positions must run from 1 without gaps.", call. = FALSE)
  }

  raw <- suppressMessages(
    readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
  )
  if (nrow(raw) < 2) stop("The file needs two header rows before the data.", call. = FALSE)

  headers <- trimws(as.character(unlist(raw[2, ], use.names = FALSE)))
  headers[is.na(headers)] <- ""
  check_headers(headers, codebook)

  body <- raw[-(1:2), , drop = FALSE]
  cells <- lapply(body, clean_text)
  empty <- Reduce(`&`, lapply(cells, is.na), rep(TRUE, nrow(body)))
  # Row numbers refer to data rows as found in the file, before empty rows are dropped.
  row_id <- seq_len(nrow(body))[!empty]

  problems <- list()
  out <- list(site = rep(site, sum(!empty)))
  for (i in seq_len(nrow(codebook))) {
    nm <- codebook$name[i]
    type <- codebook$type[i]
    x <- cells[[i]][!empty]
    value <- parse_column(x, type)
    failed <- !is.na(x) & is.na(value)
    if (any(failed)) {
      problems[[length(problems) + 1]] <- data.frame(
        row = row_id[failed], column = nm, raw_value = x[failed], expected_type = type,
        stringsAsFactors = FALSE
      )
    }
    out[[nm]] <- value
  }
  result <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)

  parse_problems <- if (length(problems) > 0) {
    do.call(rbind, problems)
  } else {
    data.frame(row = integer(), column = character(), raw_value = character(),
               expected_type = character(), stringsAsFactors = FALSE)
  }
  attr(result, "parse_problems") <- parse_problems
  attr(result, "source_file") <- normalizePath(path, mustWork = TRUE)

  if (nrow(parse_problems) > 0) {
    warning(sprintf(
      "%d value(s) could not be parsed in %d column(s): %s. See attr(, \"parse_problems\").",
      nrow(parse_problems), length(unique(parse_problems$column)),
      paste(unique(parse_problems$column), collapse = ", ")
    ), call. = FALSE)
  }
  result
}
