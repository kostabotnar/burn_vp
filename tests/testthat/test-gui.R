root <- normalizePath(testthat::test_path("..", ".."))

# nolint start: object_usage_linter. Helpers call functions sourced from R/.
test_that("site_name_from_file keeps the text before the first underscore", {
  expect_equal(site_name_from_file("NBC_2026.xlsx"), "NBC")
  expect_equal(site_name_from_file("UTMB.xlsx"), "UTMB")
  expect_equal(site_name_from_file("NBC_2026_v2.final.xlsx"), "NBC")
  expect_equal(site_name_from_file("C:/data_files/raw/UTMB_burns.xlsx"), "UTMB")
  expect_equal(site_name_from_file(c("A_1.xlsx", "B.xlsx", "D:/x_y/C_3.xlsx")), c("A", "B", "C"))
})

test_that("the progress tracker numbers the steps of a run", {
  one <- progress_tracker("NBC")
  steps <- list(one("Reading NBC: a.xlsx\n"), one("Running QC rules"), one("Profiling"),
                one("Drawing figures"), one("Run folder: x/y"))
  expect_equal(vapply(steps, function(s) s$step, numeric(1)), 1:5)
  expect_true(all(vapply(steps, function(s) s$total, numeric(1)) == 5))
  expect_true(all(vapply(steps, function(s) nzchar(s$label), logical(1))))

  three <- progress_tracker(c("A", "B", "C"))
  msgs <- c("Reading A: a.xlsx", "Reading B: b.xlsx", "Reading C: c.xlsx", "Running QC rules",
            "Profiling", "Drawing figures", "Run folder: z")
  got <- lapply(msgs, three)
  expect_equal(vapply(got, function(s) s$step, numeric(1)), 1:7)
  expect_true(all(vapply(got, function(s) s$total, numeric(1)) == 7))
})

test_that("the progress tracker ignores messages it does not know", {
  tracker <- progress_tracker(c("A", "B"))
  expect_null(tracker("Something else"))
  expect_null(tracker("Reading Z: z.xlsx"))
  expect_null(tracker(""))
})

test_that("the action registry has a QC action and an analysis action that is not available", {
  expect_true(is.function(gui_actions$qc$run))
  expect_null(gui_actions$analysis$run)
  expect_true(all(vapply(gui_actions, function(a) nzchar(a$label), logical(1))))
})

test_that("run_action refuses bad inputs with plain messages", {
  dir <- local_output_dir()
  file.create(a <- file.path(dir, "a.xlsx"))
  file.create(b <- file.path(dir, "b.xlsx"))
  qc <- gui_actions$qc
  expect_error(run_action(qc, character(0), root), "at least one")
  expect_error(run_action(qc, c(NBC = file.path(dir, "gone.xlsx")), root), "cannot be found")
  expect_error(run_action(qc, c(NBC = a, "  " = b), root), "site name")
  expect_error(run_action(qc, stats::setNames(c(a, b), c("X", "X")), root), "different")
  expect_error(run_action(qc, stats::setNames(c(a, b), c("X", " X ")), root), "different")
  expect_error(run_action(gui_actions$analysis, c(NBC = a), root), "not available yet")
})

test_that("run_action reports every step in order and returns the run folder", {
  dir <- local_output_dir()
  cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
  d <- corrupt_synthetic(generate_synthetic_nbc(cb, n = 40, seed = 5), cb, n_per_type = 2, seed = 5)
  a <- file.path(dir, "a.xlsx")
  b <- file.path(dir, "b.xlsx")
  write_nbc_xlsx(d, cb, a)
  write_nbc_xlsx(generate_synthetic_nbc(cb, n = 40, seed = 6), cb, b)
  seen <- list()
  on_progress <- function(step, total, label) seen[[length(seen) + 1]] <<- list(step, total, label)
  sub <- file.path("test-tmp", basename(dir), "runs")
  console <- character()
  run <- withCallingHandlers(
    run_action(gui_actions$qc, c(" NBC " = a, UTMB = b), root, on_progress, out_subfolder = sub),
    message = function(m) console <<- c(console, conditionMessage(m))
  )
  expect_length(console, 0)
  expect_equal(vapply(seen, `[[`, numeric(1), 1), 1:6)
  expect_true(all(vapply(seen, `[[`, numeric(1), 2) == 6))
  expect_equal(seen[[1]][[3]], "Reading NBC")
  expect_true(startsWith(run, normalizePath(dir, winslash = "/")))
  expect_true(file.exists(file.path(run, "report.html")))
})

test_that("a pipeline error reaches the caller with its own message", {
  dir <- local_output_dir()
  bad <- file.path(dir, "bad.xlsx")
  writeLines("not a workbook", bad)
  sub <- file.path("test-tmp", basename(dir), "runs")
  expect_error(run_action(gui_actions$qc, c(NBC = bad), root, out_subfolder = sub))
})

test_that("gui_build_window builds the expected widgets", {
  skip_if_not(isTRUE(unname(capabilities("tcltk"))), "Tcl/Tk is not available in this R")
  win <- tryCatch(gui_build_window(root), error = function(e) NULL)
  skip_if(is.null(win), "A Tk window cannot be created here (no display)")
  withr::defer(tcltk::tkdestroy(win$top))

  expect_setequal(names(win$buttons), names(gui_actions))
  state_of <- function(w) as.character(tcltk::tkcget(w, "-state"))
  expect_equal(state_of(win$buttons$qc), "normal")
  expect_equal(state_of(win$buttons$analysis), "disabled")
  expect_match(tcltk::tclvalue(tcltk::tkcget(win$buttons$analysis, "-text")), "not available yet")
  expect_equal(state_of(win$open_button), "disabled")
  expect_equal(length(win$get_inputs()), 0)

  dir <- local_output_dir()
  file.create(a <- file.path(dir, "NBC_2026.xlsx"))
  file.create(b <- file.path(dir, "UTMB.xlsx"))
  expect_equal(win$add_files(c(a, b)), 2)
  expect_equal(win$add_files(a), 0)
  inputs <- win$get_inputs()
  expect_equal(names(inputs), c("NBC", "UTMB"))
  expect_equal(unname(inputs), normalizePath(c(a, b), winslash = "/"))
  expect_equal(win$remove_checked(), 0)
  expect_equal(length(win$get_inputs()), 2)
})
# nolint end
