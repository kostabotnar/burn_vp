# Synonyms for categorical levels. `target` is only used when it is a level of the column
# being standardised (case-insensitive), so the table is safe to apply to every column.
categorical_synonyms <- data.frame(
  synonym = c("m", "mann", "man", "f", "k", "kvinne", "woman",
              "noradrenaline", "adrenaline", "phenylephrine", "phenylepinephrine", "dobutamin"),
  target = c("Male", "Male", "Male", "Female", "Female", "Female", "Female",
             "Norepinephrine", "Epinephrine", "Phenylephrine", "Phenylephrine", "Dobutamine"),
  stringsAsFactors = FALSE
)

binary_map <- c(
  yes = "Yes", y = "Yes", ja = "Yes", j = "Yes", true = "Yes", "1" = "Yes",
  no = "No", n = "No", nei = "No", false = "No", "0" = "No",
  n0 = "No" # a zero typed for the letter o
)

collapse_space <- function(x) trimws(gsub("[[:space:] ]+", " ", x))

normalise_key <- function(x) tolower(collapse_space(x))

# Standardises unique non-missing strings against `levels`. Returns new value, rule and whether
# the value is now a level. Values that match nothing are returned unchanged.
match_levels <- function(values, levels) {
  key <- normalise_key(values)
  new <- values
  rule <- rep(NA_character_, length(values))
  mapped <- rep(FALSE, length(values))
  level_key <- tolower(levels)

  idx <- match(key, level_key)
  hit <- !is.na(idx)
  new[hit] <- levels[idx[hit]]
  rule[hit] <- "categorical_case"
  mapped[hit] <- TRUE

  target <- categorical_synonyms$target[match(key, categorical_synonyms$synonym)]
  tidx <- match(tolower(target), level_key)
  syn <- !hit & !is.na(tidx)
  new[syn] <- levels[tidx[syn]]
  rule[syn] <- "synonym"
  mapped[syn] <- TRUE

  list(new = new, rule = rule, mapped = mapped)
}

match_binary <- function(values) {
  new <- unname(binary_map[normalise_key(values)])
  mapped <- !is.na(new)
  new[!mapped] <- values[!mapped]
  list(new = new, rule = ifelse(mapped, "binary", NA_character_), mapped = mapped)
}

split_list_cell <- function(cell, levels) {
  items <- trimws(strsplit(cell, "[,;]")[[1]])
  items <- items[nzchar(items)]
  m <- match_levels(items, levels)
  known <- levels[levels %in% m$new[m$mapped]]
  keep <- !m$mapped & !duplicated(normalise_key(items))
  other <- items[keep]
  list(value = paste(c(known, other), collapse = ", "), unmapped = other)
}

empty_recode_log <- function() {
  data.frame(row = integer(), column = character(), raw_value = character(),
             new_value = character(), rule = character(), stringsAsFactors = FALSE)
}

empty_unmapped <- function() {
  data.frame(row = integer(), column = character(), raw_value = character(),
             stringsAsFactors = FALSE)
}

# Recodes values in `data` according to the codebook types and returns it with attributes
# `recode_log` (row, column, raw_value, new_value, rule) and `unmapped_values` (row, column,
# raw_value). Row is the row index in `data`. For comorbidities_list, one unmapped row is
# recorded per unmatched item, with the item as raw_value.
standardise_values <- function(data, codebook) {
  acc <- new.env()
  acc$logs <- list()
  acc$unmapped <- list()
  add_log <- function(rows, col, raw, new, rule) {
    if (length(rows) == 0) return(invisible())
    acc$logs[[length(acc$logs) + 1]] <- data.frame(
      row = rows, column = col, raw_value = raw, new_value = new, rule = rule,
      stringsAsFactors = FALSE
    )
  }
  add_unmapped <- function(rows, col, raw) {
    if (length(rows) == 0) return(invisible())
    acc$unmapped[[length(acc$unmapped) + 1]] <- data.frame(
      row = rows, column = col, raw_value = raw, stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(codebook))) {
    col <- codebook$name[i]
    type <- codebook$type[i]
    if (!col %in% names(data) || !is.character(data[[col]])) next
    x <- data[[col]]
    present <- which(!is.na(x))
    if (length(present) == 0) next
    levels <- if (is.na(codebook$allowed_values[i])) {
      character()
    } else {
      strsplit(codebook$allowed_values[i], "|", fixed = TRUE)[[1]]
    }
    is_list <- col == "comorbidities_list"

    if (type == "text") {
      new <- collapse_space(x[present])
      rule <- rep("text_whitespace", length(present))
    } else if (is_list && type == "categorical") {
      cells <- lapply(x[present], split_list_cell, levels = levels)
      new <- vapply(cells, function(z) z$value, character(1))
      rule <- rep("list", length(present))
      for (k in seq_along(present)) {
        add_unmapped(rep(present[k], length(cells[[k]]$unmapped)), col, cells[[k]]$unmapped)
      }
    } else if (type %in% c("binary", "categorical")) {
      u <- unique(x[present])
      m <- if (type == "binary") match_binary(u) else match_levels(u, levels)
      at <- match(x[present], u)
      new <- m$new[at]
      rule <- m$rule[at]
      add_unmapped(present[!m$mapped[at]], col, x[present][!m$mapped[at]])
    } else {
      next
    }

    changed <- new != x[present]
    add_log(present[changed], col, x[present][changed], new[changed], rule[changed])
    x[present] <- new
    data[[col]] <- x
  }

  log_df <- if (length(acc$logs)) do.call(rbind, acc$logs) else empty_recode_log()
  unm_df <- if (length(acc$unmapped)) do.call(rbind, acc$unmapped) else empty_unmapped()
  log_df <- log_df[order(log_df$row), , drop = FALSE]
  unm_df <- unm_df[order(unm_df$row), , drop = FALSE]
  rownames(log_df) <- NULL
  rownames(unm_df) <- NULL
  attr(data, "recode_log") <- log_df
  attr(data, "unmapped_values") <- unm_df
  data
}
