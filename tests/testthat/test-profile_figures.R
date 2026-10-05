cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
rng <- read_qc_ranges(testthat::test_path("..", "..", "metadata", "qc_ranges.csv"))

fig_data <- local({
  a <- cbind(site = "NBC", generate_synthetic_nbc(cb, n = 60, seed = 1), stringsAsFactors = FALSE)
  b <- cbind(site = "UTMB", generate_synthetic_nbc(cb, n = 60, seed = 2), stringsAsFactors = FALSE)
  derive_analysis_variables(rbind(a, b))
})
fig_qc <- run_qc(fig_data, cb, rng)
fig_corr <- correlation_matrix(fig_data)

expect_plot <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
}

test_that("sites keep their colour by identity", {
  expect_equal(site_colours(c("NBC", "UTMB")), c(NBC = "#2a78d6", UTMB = "#eb6834"))
  expect_equal(site_colours(c("UTMB", "NBC")), c(UTMB = "#eb6834", NBC = "#2a78d6"))
  expect_equal(site_colours("UTMB"), c(UTMB = "#eb6834"))
  expect_equal(unname(site_colours(c("NBC", "UTMB", "X", "Y"))[3:4]), c("#1baf7a", "#eda100"))
  expect_error(site_colours(c("NBC", "UTMB", "X", "Y", "Z")), "At most four")
})

test_that("wilson_ci matches known values", {
  ci <- wilson_ci(5, 10)
  expect_equal(ci$p, 0.5)
  expect_equal(ci$lower, 0.2366, tolerance = 1e-3)
  expect_equal(ci$upper, 0.7634, tolerance = 1e-3)
  zero <- wilson_ci(0, 20)
  expect_equal(zero$lower, 0)
  expect_gt(zero$upper, 0.1)
  expect_true(is.na(wilson_ci(0, 0)$p))
})

test_that("every figure function returns a ggplot on two-site data", {
  expect_plot(fig_distributions(fig_data))
  expect_plot(fig_fluid_vs_tbsa(fig_data))
  expect_plot(fig_missingness(fig_qc$missingness))
  expect_plot(fig_timeline(fig_data))
  expect_plot(fig_binary_proportions(fig_data))
  expect_plot(fig_correlation(fig_corr))
})

test_that("figures also work with a single site and with underived data", {
  one <- fig_data[fig_data$site == "NBC", ]
  expect_plot(fig_distributions(one))
  expect_plot(fig_binary_proportions(one))
  raw <- fig_data[, !names(fig_data) %in% c("peak_ne_48h", "any_ne_48h", "burn_to_icu_h")]
  expect_plot(fig_timeline(raw))
})

test_that("build_profile_figures matches the catalogue", {
  figs <- build_profile_figures(fig_data, fig_qc$missingness, fig_corr)
  expect_setequal(names(figs), figure_catalogue()$id)
  cat_df <- figure_catalogue()
  expect_true(all(nzchar(cat_df$caption)) && all(nzchar(cat_df$alt)))
})

test_that("save_figure writes a 150 dpi PNG inside output/ only", {
  dir <- local_output_dir()
  path <- file.path(dir, "p.png")
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()
  save_figure(p, path, width = 4, height = 3)
  expect_true(file.exists(path))
  header <- readBin(path, "raw", 24)
  dims <- c(sum(as.integer(header[17:20]) * 256^(3:0)), sum(as.integer(header[21:24]) * 256^(3:0)))
  expect_equal(dims, c(600, 450))
  expect_error(save_figure(p, "/tmp/p.png"), "output")
})
