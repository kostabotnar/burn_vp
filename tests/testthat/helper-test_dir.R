# Creates a unique folder under <project root>/output/test-tmp and removes it when `env` ends.
local_output_dir <- function(env = parent.frame()) {
  root <- normalizePath(testthat::test_path("..", ".."))
  sub <- file.path("test-tmp", basename(tempfile("t")))
  dir <- project_output_dir(root, sub) # nolint: object_usage_linter.
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}
