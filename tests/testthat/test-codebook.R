write_codebook_csv <- function(rows, dir = NULL) {
  if (is.null(dir)) dir <- local_output_dir(parent.frame()) # nolint: object_usage.
  path <- file.path(dir, "codebook.csv")
  utils::write.csv(rows, path, row.names = FALSE, na = "NA")
  path
}

valid_rows <- function() {
  data.frame(
    position = 1:4,
    source_name = c("Age (years)", "Gender", "Date ICU admission", "Comment"),
    section = "Demographics",
    name = c("age_years", "gender", "icu_admission_date", "comment"),
    type = c("numeric", "categorical", "date", "text"),
    unit = c("years", NA, NA, NA),
    allowed_values = c(NA, "Male|Female|Others", NA, NA),
    time_window = NA_character_,
    dictionary_name = NA_character_,
    key_variable = c(TRUE, TRUE, FALSE, FALSE),
    notes = NA_character_,
    stringsAsFactors = FALSE
  )
}

test_that("a valid codebook is read with typed columns", {
  cb <- read_codebook(write_codebook_csv(valid_rows()))
  expect_equal(nrow(cb), 4)
  expect_type(cb$position, "integer")
  expect_type(cb$key_variable, "logical")
  expect_true(is.na(cb$allowed_values[1]))
})

test_that("missing required columns are reported", {
  rows <- valid_rows()
  rows$unit <- NULL
  rows$notes <- NULL
  expect_error(read_codebook(write_codebook_csv(rows)), "missing required columns: unit, notes")
})

test_that("duplicated name or position is an error", {
  rows <- valid_rows()
  rows$name[2] <- "age_years"
  expect_error(read_codebook(write_codebook_csv(rows)), "'name' is not unique: age_years")
  rows <- valid_rows()
  rows$position[2] <- 1
  expect_error(read_codebook(write_codebook_csv(rows)), "'position' is not unique: 1")
})

test_that("an unknown type is an error", {
  rows <- valid_rows()
  rows$type[4] <- "string"
  expect_error(read_codebook(write_codebook_csv(rows)), "unknown 'type' values: string")
})

test_that("binary and categorical rows need allowed values", {
  rows <- valid_rows()
  rows$allowed_values[2] <- NA
  expect_error(read_codebook(write_codebook_csv(rows)), "need 'allowed_values': gender")
  rows <- valid_rows()
  rows$type[4] <- "binary"
  expect_error(read_codebook(write_codebook_csv(rows)), "need 'allowed_values': comment")
})

test_that("a missing file is an error", {
  expect_error(read_codebook(file.path(local_output_dir(), "no_such_codebook.csv")), "not found")
})

codebook_path <- normalizePath(
  testthat::test_path("..", "..", "metadata", "codebook.csv"),
  mustWork = FALSE
)

test_that("the project codebook loads and has 196 unique rows", {
  cb <- read_codebook(codebook_path)
  expect_equal(nrow(cb), 196)
  expect_equal(cb$position, 1:196)
  expect_false(anyDuplicated(cb$name) > 0)
  expect_true(all(cb$name != ""))
  expect_true(all(c("icu_admission_date", "outcome_icu_admission_date") %in% cb$name))
  expect_false(any(is.na(cb$key_variable)))
})

test_that("codebook source_name matches the real template header", {
  template <- "/home/kobotnar/data/src/burn_vp/NBC_dataset_template.xlsx"
  skip_if_not(file.exists(template))
  # Only the header rows are read, never the example patient.
  hdr <- suppressMessages(readxl::read_excel(
    template, sheet = "Ark1", col_names = FALSE, col_types = "text", n_max = 2
  ))
  cb <- read_codebook(codebook_path)
  expect_equal(ncol(hdr), nrow(cb))
  expect_equal(cb$source_name, trimws(as.character(unlist(hdr[2, ], use.names = FALSE))))
  section <- as.character(unlist(hdr[1, ], use.names = FALSE))
  section <- section[!is.na(section)][cumsum(!is.na(section))]
  expect_equal(cb$section, section)
})
