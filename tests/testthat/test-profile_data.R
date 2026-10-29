cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
rng <- read_qc_ranges(testthat::test_path("..", "..", "metadata", "qc_ranges.csv"))

# nolint start: object_usage_linter. Helpers call functions sourced from R/.
two_sites <- function(n = 80) {
  a <- cbind(site = "NBC", generate_synthetic_nbc(cb, n = n, seed = 1), stringsAsFactors = FALSE)
  b <- cbind(site = "UTMB", generate_synthetic_nbc(cb, n = n, seed = 2), stringsAsFactors = FALSE)
  rbind(a, b)
}
syn <- two_sites()

# nolint end

test_that("SMD matches hand-computed values", {
  expect_equal(smd_continuous(c(1, 2, 3), c(3, 4, 5)), 2)
  expect_equal(smd_continuous(c(1, 2, NA, 3), c(3, 4, 5)), 2)
  expect_equal(smd_binary(0.5, 0.25), 0.25 / sqrt(0.21875))
  expect_equal(smd_binary(0.3, 0.3), 0)
  expect_equal(smd_categorical(c(0.5, 0.5), c(0.25, 0.75)), smd_binary(0.5, 0.25))
  # t = (0.3, 0); S = [[0.205, -0.105], [-0.105, 0.21]]; t' S^-1 t = 0.09 * 0.21 / 0.032025
  expect_equal(smd_categorical(c(0.5, 0.3, 0.2), c(0.2, 0.3, 0.5)), sqrt(0.0189 / 0.032025))
  expect_equal(smd_categorical(c(1, 0, 0), c(1, 0, 0)), 0)
})

test_that("site_comparison reports SMD per variable and NA for one site", {
  d <- data.frame(
    site = rep(c("A", "B"), each = 3), x = c(1, 2, 3, 3, 4, 5),
    flag = c("Yes", "Yes", "No", "No", "No", "Yes"),
    g = c("u", "v", "w", "u", "u", "w"), stringsAsFactors = FALSE
  )
  tab <- site_comparison(d, cb, c("x", "flag", "g"))
  expect_equal(tab$smd[tab$variable == "x"], 2)
  expect_equal(tab$A[tab$variable == "x"], "2.0 [1.5, 2.5]")
  flag <- tab[tab$variable == "flag", ]
  expect_equal(nrow(flag), 1)
  expect_equal(flag$A, "2 (66.7%)")
  expect_equal(flag$smd, smd_binary(2 / 3, 1 / 3))
  g <- tab[tab$variable == "g", ]
  expect_equal(g$level, c("u", "v", "w"))
  expect_equal(g$B, c("2 (66.7%)", "0 (0.0%)", "1 (33.3%)"))
  expect_equal(g$smd, c(smd_categorical(c(1 / 3, 1 / 3, 1 / 3), c(2 / 3, 0, 1 / 3)), NA, NA))
  expect_equal(attr(tab, "n_by_site"), c(A = 3L, B = 3L))

  one <- site_comparison(d[d$site == "A", ], cb, c("x", "flag"))
  expect_true(all(is.na(one$smd)))
})

test_that("site_comparison works on synthetic two-site data", {
  d <- derive_analysis_variables(syn)
  tab <- site_comparison(d, cb, c("age_years", "gender", "hospital_mortality", "peak_ne_48h"))
  expect_true(all(c("NBC", "UTMB", "smd") %in% names(tab)))
  expect_true(all(tab$smd[!is.na(tab$smd)] >= 0))
})

test_that("masking removes only flagged values and counts them", {
  d <- syn
  d$age_years[c(2, 5)] <- c(150, 200)
  d$diabetes[3] <- "Maybe"
  d$tbsa_burned_pct[4] <- 500
  d$total_fluids_24h_ml[6] <- 1 # fluid rule: not a range, type or category rule
  qc <- run_qc(d, cb, rng)
  findings <- localise_finding_rows(qc$findings, d)
  masked <- mask_flagged_values(d, findings)

  expect_true(all(is.na(masked$age_years[c(2, 5)])))
  expect_true(is.na(masked$diabetes[3]))
  expect_true(is.na(masked$tbsa_burned_pct[4]))
  expect_equal(masked$total_fluids_24h_ml[6], 1)
  changed <- vapply(names(d), function(col) {
    !identical(is.na(d[[col]]), is.na(masked[[col]]))
  }, logical(1))
  expect_setequal(names(d)[changed], c("age_years", "diabetes", "tbsa_burned_pct"))
  counts <- attr(masked, "masked_counts")
  expect_equal(counts$n_masked[counts$variable == "age_years"], 2)
  expect_equal(sum(counts$n_masked), 4)
  expect_equal(masked$age_years[-c(2, 5)], d$age_years[-c(2, 5)])
})

test_that("masking uses per-site rows", {
  d <- syn
  site2_row <- 10
  idx <- 80 + site2_row
  d$age_years[idx] <- 150
  qc <- run_qc(d, cb, rng)
  findings <- localise_finding_rows(qc$findings, d)
  hit <- findings[findings$rule_id == "RNG_002", ]
  expect_equal(hit$site, "UTMB")
  expect_equal(hit$row, site2_row)
  masked <- mask_flagged_values(d, findings)
  expect_true(is.na(masked$age_years[idx]))
  expect_false(any(is.na(masked$age_years[-idx])))
})

test_that("derive_analysis_variables does the arithmetic", {
  d <- data.frame(
    site = "A", admission_weight_kg = c(80, 100, 0), tbsa_burned_pct = c(40, 20, 30),
    crystalloids_24h_ml = c(12800, 6000, 1000), total_fluids_24h_ml = c(14400, 8000, 1000),
    crystalloids_48h_ml = c(20000, 9000, 1000), total_fluids_48h_ml = c(24000, 10000, 1000),
    ne_24h = c("Yes", "No", "No"), ne_24_48h = c("Yes", "No", NA), ne_48h = c("Yes", "No", NA),
    peak_ne_24h_ug_kg_min = c(0.1, NA, NA), peak_ne_24_48h_ug_kg_min = c(0.3, NA, NA),
    other_vp_24h = c("Vasopressin", NA, NA), other_vp_24_48h = c(NA, NA, NA),
    albumin_24h = c("No", "No", NA), albumin_48h = c("Yes", "No", NA),
    lowest_map_24h_mmhg = c(60, 70, NA), lowest_map_24_48h_mmhg = c(55, 75, NA),
    highest_pulse_24h_bpm = c(100, 90, NA), highest_pulse_24_48h_bpm = c(110, 80, NA),
    icu_admission_date = as.Date(c("2020-01-02", "2020-01-01", NA)),
    icu_admission_time = c("03:30", "10:00", NA),
    burn_date = as.Date(c("2020-01-01", "2020-01-01", NA)),
    burn_time = c("22:00", "08:30", NA), first_surgery_days = c(1.5, NA, NA),
    stringsAsFactors = FALSE
  )
  out <- derive_analysis_variables(d)
  expect_equal(out$crystalloids_24h_ml_per_kg, c(160, 60, NA))
  expect_equal(out$crystalloids_24h_ml_per_kg_tbsa, c(4, 3, NA))
  expect_equal(out$total_fluids_24h_ml_per_kg, c(180, 80, NA))
  expect_equal(out$total_fluids_48h_ml_per_kg_tbsa, c(300 / 40, 100 / 20, NA))
  expect_equal(out$crystalloids_48h_ml_per_kg, c(250, 90, NA))
  expect_equal(out$parkland_24h_ml, c(12800, 8000, NA))
  expect_equal(out$crystalloids_24h_parkland_ratio, c(1, 0.75, NA))
  expect_equal(out$any_ne_48h, c("Yes", "No", NA))
  expect_equal(out$peak_ne_48h, c(0.3, 0, 0))
  expect_equal(out$any_other_vp_48h, c("Yes", "No", "No"))
  expect_equal(out$vasopressor_type, c("NE and other", "None", NA))
  expect_equal(out$any_albumin_48h, c("Yes", "No", NA))
  expect_equal(out$lowest_map_48h_mmhg, c(55, 70, NA))
  expect_equal(out$highest_pulse_48h_bpm, c(110, 90, NA))
  expect_equal(out$burn_to_icu_h, c(5.5, 1.5, NA))
  expect_equal(out$first_surgery_h, c(36, NA, NA))
  expect_identical(out[names(d)], d)
})

test_that("derive_analysis_variables keeps existing columns on synthetic data", {
  out <- derive_analysis_variables(syn)
  expect_identical(out[names(syn)], syn)
  expect_true(all(out$burn_to_icu_h > 0, na.rm = TRUE))
  expect_true(all(out$peak_ne_48h >= 0, na.rm = TRUE))
})

test_that("profile_variables counts values per site", {
  d <- data.frame(
    site = c("A", "A", "A", "B", "B"), age_years = c(20, 30, NA, 40, 60),
    gender = c("Male", "Female", "Male", NA, "Male"),
    diabetes = c("Yes", "No", NA, "Yes", "Yes"), stringsAsFactors = FALSE
  )
  prof <- profile_variables(d, cb)
  a <- prof$numeric[prof$numeric$variable == "age_years" & prof$numeric$site == "A", ]
  expect_equal(c(a$n, a$missing, a$mean, a$median, a$min, a$max), c(2, 1, 25, 25, 20, 30))
  b <- prof$numeric[prof$numeric$variable == "age_years" & prof$numeric$site == "B", ]
  expect_equal(c(b$q1, b$q3), c(45, 55))
  cat <- prof$categorical
  g <- cat[cat$variable == "gender" & cat$site == "A", ]
  expect_equal(g$n[g$level == "Male"], 2)
  expect_equal(g$n[g$level == "Others"], 0)
  expect_equal(g$n[g$level == "(missing)"], 0)
  expect_equal(cat$n[cat$variable == "gender" & cat$site == "B" & cat$level == "(missing)"], 1)
  db <- cat[cat$variable == "diabetes" & cat$site == "B", ]
  expect_equal(db$pct[db$level == "Yes"], 100)
  expect_equal(sum(cat$n[cat$variable == "diabetes" & cat$site == "A"]), 3)
  expect_false("patient_id" %in% c(prof$numeric$variable, cat$variable))
})

test_that("profile_variables reports masked counts and splits comorbidity lists", {
  d <- data.frame(site = c("A", "A"), age_years = c(NA, 30),
                  comorbidities_list = c("Diabetes, Stroke", NA), stringsAsFactors = FALSE)
  attr(d, "masked_counts") <- data.frame(site = "A", variable = "age_years", n_masked = 1L)
  prof <- profile_variables(d, cb)
  expect_equal(prof$numeric$n_masked, 1)
  cl <- prof$categorical[prof$categorical$variable == "comorbidities_list", ]
  expect_equal(cl$n[cl$level %in% c("Diabetes", "Stroke")], c(1, 1))
  expect_equal(cl$n[cl$level == "(missing)"], 1)
})

test_that("exposure_outcome_summary covers exposures, outcomes and survivor LOS", {
  out <- exposure_outcome_summary(derive_analysis_variables(syn))
  expect_setequal(unique(out$site), c("NBC", "UTMB"))
  expect_true(all(c("Exposure", "Outcome") %in% out$group))
  los <- out$measure[grepl("Hospital stay", out$measure)]
  expect_true(all(c("Hospital stay (days), survivors", "Hospital stay (days), non-survivors")
                  %in% los))
  mort <- out[out$measure == "In-hospital mortality" & out$site == "NBC", ]
  expect_equal(mort$count, sum(syn$hospital_mortality[syn$site == "NBC"] == "Yes"))
})

test_that("correlation_matrix is Spearman on pairwise complete data", {
  d <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1, 4, 9, 16, NA), c = c(5, 4, 3, 2, 1))
  r <- correlation_matrix(d, c("a", "b", "c"))
  expect_equal(r["a", "b"], 1)
  expect_equal(r["a", "c"], -1)
  expect_equal(attr(r, "n_pairs")["a", "b"], 4)
  derived <- derive_analysis_variables(syn)
  expect_equal(dim(correlation_matrix(derived)),
               rep(sum(correlation_vars %in% names(derived)), 2))
})

test_that("write_profile_outputs only writes inside output/", {
  dir <- local_output_dir()
  m <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("a", "b")))
  paths <- write_profile_outputs(list(a = data.frame(x = 1), m = m), dir)
  expect_true(all(file.exists(paths)))
  expect_error(write_profile_outputs(list(a = data.frame(x = 1)), "/tmp"), "output")
})
