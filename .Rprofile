# Posit Package Manager serves prebuilt Linux binaries; renv picks them automatically.
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
# Keep renv's own files inside this project's output/ folder: no user-level cache, no sandbox
# copy of the system library. R/project_paths.R (project_renv_env) sets the same values.
local({
  root <- normalizePath(getwd(), winslash = "/")
  Sys.setenv(RENV_PATHS_ROOT = file.path(root, "output", "renv"),
             RENV_CONFIG_CACHE_ENABLED = "FALSE",
             RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
})
source("renv/activate.R")
