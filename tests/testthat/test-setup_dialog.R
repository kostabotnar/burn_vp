# nolint start: object_usage_linter. Functions come from R/, sourced by the test runner.

test_that("setup_progress_step reads step lines", {
  expect_equal(setup_progress_step("Step 3 of 5: Preparing the package library"),
               list(step = 3L, total = 5L, label = "Preparing the package library", long = FALSE))
  step <- setup_progress_step(
    "Step 4 of 5: Installing 37 of 40 packages (this can take several minutes)"
  )
  expect_equal(step$step, 4L)
  expect_equal(step$label, "Installing 37 of 40 packages (this can take several minutes)")
  expect_true(step$long)
})

test_that("a skipped installation step is not a long step", {
  step <- setup_progress_step(
    "Step 4 of 5: Installing packages (skipped, all 40 are already installed)"
  )
  expect_equal(c(step$step, step$total), c(4L, 5L))
  expect_match(step$label, "^Installing packages \\(skipped")
  expect_false(step$long)
})

test_that("setup_progress_step returns NULL for lines that are not steps", {
  for (line in c("", "Project folder: C:/x", "[OK] Disk space: 100 MB free.", "SETUP COMPLETE",
                 "Step 4 of 5:", "step 4 of 5: lower case", " Step 1 of 5: indented",
                 "Step x of 5: bad")) {
    expect_null(setup_progress_step(line))
  }
})

test_that("setup_result_view shows the log file after a complete setup", {
  lines <- c("SETUP COMPLETE", "Log file: output/setup/setup_1.txt (in the project folder)")
  view <- setup_result_view("complete", lines)
  expect_equal(view$headline, "Setup complete.")
  expect_true("Log file: output/setup/setup_1.txt (in the project folder)" %in% view$details)
})

test_that("setup_result_view shows the reason, the action and the log file after a failure", {
  lines <- c("SETUP FAILED", "What went wrong: Some packages are missing.",
             "What to do: Send the log file.", "Log file: output/setup/setup_2.txt")
  view <- setup_result_view("failed", lines)
  expect_equal(view$headline, "Setup failed.")
  expect_equal(view$details, c("What went wrong: Some packages are missing.",
                               "What to do: Send the log file.",
                               "Log file: output/setup/setup_2.txt"))
})

test_that("setup_result_view has a fallback when setup printed no reason", {
  view <- setup_result_view("failed", character())
  expect_match(view$details[1], "^What went wrong: ")
  expect_match(view$details[2], "^What to do: ")
  expect_length(view$details, 2)
})

test_that("the dialog texts are plain and mention the key facts", {
  txt <- setup_dialog_text()
  expect_match(txt$intro, "one-time setup")
  expect_match(txt$intro, "several minutes")
  expect_match(txt$intro, "internet")
  expect_equal(c(txt$start, txt$cancel, txt$open, txt$close),
               c("Start setup", "Cancel", "Open the program", "Close"))
  expect_false(any(grepl("\u2013|\u2014", unlist(txt))))
})

test_that("setup_environment.R passes its lines to the progress option and not otherwise", {
  text <- readLines(testthat::test_path("..", "..", "scripts", "setup_environment.R"))
  expect_true(any(grepl('getOption("burn_vp.setup_progress")', text, fixed = TRUE)))
  expect_true(any(grepl("say_final(\"SETUP COMPLETE\")", text, fixed = TRUE)))
  expect_true(any(grepl("say_final(\"SETUP FAILED\")", text, fixed = TRUE)))
})

# Tk smoke test: runs only where a Tk window can be created. Nothing blocks: the buttons are
# invoked from R, so the wait variable is set before it is waited on.
build_test_dialog <- function() {
  skip_if_not(isTRUE(unname(capabilities("tcltk"))), "Tcl/Tk is not available in this R")
  win <- tryCatch(setup_dialog_build(), error = function(e) NULL)
  skip_if(is.null(win), "A Tk window cannot be created here (no display)")
  win
}
text_of <- function(widget) tcltk::tclvalue(tcltk::tkcget(widget, "-text"))
state_of <- function(widget) as.character(tcltk::tkcget(widget, "-state"))

test_that("the dialog moves from the explanation to the progress and the result", {
  win <- build_test_dialog()
  withr::defer(tcltk::tkdestroy(win$top))
  expect_equal(text_of(win$primary), "Start setup")
  expect_equal(text_of(win$secondary), "Cancel")
  expect_match(tcltk::tclvalue(win$body_var), "one-time setup")

  win$show_progress()
  expect_equal(state_of(win$primary), "disabled")
  expect_equal(state_of(win$secondary), "disabled")

  win$on_line("Step 1 of 5: Finding the project folder")
  expect_equal(tcltk::tclvalue(win$status_var), "Step 1 of 5: Finding the project folder")
  expect_equal(as.numeric(tcltk::tkcget(win$progress, "-value")), 0)
  win$on_line("Step 3 of 5: Preparing the package library")
  expect_equal(as.numeric(tcltk::tkcget(win$progress, "-value")), 2)
  expect_equal(as.numeric(tcltk::tkcget(win$progress, "-maximum")), 5)
  expect_false(as.logical(as.integer(tcltk::tcl("winfo", "ismapped", win$wait))))

  win$on_line("Step 4 of 5: Installing 3 of 4 packages (this can take several minutes)")
  expect_equal(as.numeric(tcltk::tkcget(win$progress, "-value")), 3)
  expect_true(as.logical(as.integer(tcltk::tcl("winfo", "ismapped", win$wait))))
  expect_match(text_of(win$wait), "Please wait")
  win$on_line("Logged line")
  logged <- tcltk::tclvalue(tcltk::tcl(win$log_text, "get", "1.0", "end"))
  expect_match(logged, "Step 3 of 5: Preparing the package library\nStep 4 of 5", fixed = TRUE)
  expect_match(logged, "Logged line", fixed = TRUE)
  expect_equal(state_of(win$log_text), "disabled")

  win$on_line("Log file: output/setup/setup_1.txt")
  win$show_result("complete")
  expect_equal(tcltk::tclvalue(win$heading_var), "Setup complete.")
  expect_match(tcltk::tclvalue(win$body_var), "setup_1.txt", fixed = TRUE)
  expect_equal(c(text_of(win$primary), text_of(win$secondary)), c("Open the program", "Close"))
  expect_equal(as.numeric(tcltk::tkcget(win$progress, "-value")), 5)
  tcltk::tcl(win$primary, "invoke")
  expect_equal(tcltk::tclvalue(win$choice), "open")
  tcltk::tcl(win$secondary, "invoke")
  expect_equal(tcltk::tclvalue(win$choice), "close")
})

test_that("the dialog shows a failure with a single Close button", {
  win <- build_test_dialog()
  withr::defer(tcltk::tkdestroy(win$top))
  win$show_progress()
  win$on_line("What went wrong: The test run did not produce its report.")
  win$on_line("What to do: Send the log file.")
  win$show_result("failed")
  expect_equal(tcltk::tclvalue(win$heading_var), "Setup failed.")
  expect_match(tcltk::tclvalue(win$body_var), "did not produce its report", fixed = TRUE)
  expect_equal(text_of(win$primary), "Close")
  expect_false(as.logical(as.integer(tcltk::tcl("winfo", "ismapped", win$secondary))))
  tcltk::tcl(win$primary, "invoke")
  expect_equal(tcltk::tclvalue(win$choice), "close")
})

test_that("setup_dialog_run follows the buttons, with a wait that presses them", {
  skip_if_not(isTRUE(unname(capabilities("tcltk"))), "Tcl/Tk is not available in this R")
  skip_if(!setup_dialog_available(), "A Tk window cannot be created here (no display)")
  # The wait never blocks: it invokes the named buttons in turn.
  presser <- function(...) {
    pending <- list(...)
    function(win) {
      button <- pending[[1]]
      pending <<- pending[-1]
      tcltk::tcl(win[[button]], "invoke")
    }
  }
  never <- function(root, on_line) stop("must not run")
  expect_equal(setup_dialog_run("root", never, presser("secondary")), "cancel")

  seen <- character()
  fake_setup <- function(root, on_line) {
    on_line("Step 1 of 5: Finding the project folder")
    seen <<- c(seen, "ran")
    "complete"
  }
  expect_equal(setup_dialog_run("root", fake_setup, presser("primary", "secondary")), "close")
  expect_equal(setup_dialog_run("root", fake_setup, presser("primary", "primary")), "open")
  expect_equal(seen, c("ran", "ran"))
  failing <- function(root, on_line) "failed"
  expect_equal(setup_dialog_run("root", failing, presser("primary", "primary")), "failed")
  erroring <- function(root, on_line) stop("boom")
  expect_equal(setup_dialog_run("root", erroring, presser("primary", "primary")), "failed")
})
# nolint end
