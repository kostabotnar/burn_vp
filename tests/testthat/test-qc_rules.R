cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
rng <- read_qc_ranges(testthat::test_path("..", "..", "metadata", "qc_ranges.csv"))
rules <- qc_rules(cb, rng)
project_root <- normalizePath(testthat::test_path("..", ".."))

with_site <- function(d, site = "NBC") cbind(site = site, d, stringsAsFactors = FALSE)
clean <- lapply(1:3, function(s) with_site(generate_synthetic_nbc(cb, n = 100, seed = s)))

check <- function(id, df) rules[[id]]$check(df)

# A unique folder under output/test-tmp, removed (with test-tmp when left empty) after the test.
local_test_dir <- function(prefix, env = parent.frame()) {
  name <- sprintf("test-tmp/%s-%s-%d", prefix, format(Sys.time(), "%Y%m%d%H%M%OS3"),
                  Sys.getpid())
  dir <- project_output_dir(project_root, name) # nolint: object_usage_linter. Sourced by runner.
  withr::defer({
    unlink(dir, recursive = TRUE)
    parent <- dirname(dir)
    if (length(list.files(parent, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(parent, recursive = TRUE)
    }
  }, envir = env)
  dir
}
pass_fail_na <- c(FALSE, TRUE, NA)

test_that("qc_ranges.csv covers every numeric and integer column exactly once", {
  numeric_cols <- cb$name[cb$type %in% c("numeric", "integer")]
  expect_setequal(rng$name, numeric_cols)
  expect_false(anyDuplicated(rng$name) > 0)
  expect_true(all(rng$min <= rng$max))
  expect_true(all(nzchar(rng$source_note)))
  expect_equal(c(rng$min[rng$name == "age_years"], rng$max[rng$name == "age_years"]), c(18, 110))
})

test_that("read_qc_ranges rejects malformed files", {
  dir <- local_test_dir("qc-ranges")
  bad <- file.path(dir, "bad.csv")
  writeLines(c("name,min,max,source_note", "age_years,110,18,x"), bad)
  expect_error(read_qc_ranges(bad), "min <= max")
  writeLines(c("name,min,max", "age_years,18,110"), bad)
  expect_error(read_qc_ranges(bad), "source_note")
  writeLines(c("name,min,max,source_note", "a,1,2,x", "a,1,2,y"), bad)
  expect_error(read_qc_ranges(bad), "twice")
  expect_error(read_qc_ranges(file.path(dir, "absent.csv")), "not found")
})

test_that("the registry is well formed", {
  ids <- vapply(rules, `[[`, character(1), "id")
  expect_false(anyDuplicated(ids) > 0)
  expect_true(all(grepl("^[A-Z]{3}_[0-9]{3}$", ids)))
  expect_true(all(vapply(rules, `[[`, character(1), "category") %in% qc_categories))
  expect_true(all(vapply(rules, `[[`, character(1), "severity") %in% qc_severities))
  expect_true(all(nzchar(vapply(rules, `[[`, character(1), "description"))))
  expect_true(all(vapply(rules, function(r) is.function(r$check), logical(1))))
  expect_setequal(unique(vapply(rules, `[[`, character(1), "category")), qc_categories)
  all_cols <- unique(unlist(lapply(rules, `[[`, "columns")))
  expect_true(all(all_cols %in% c("site", cb$name)))
  expect_equal(sum(grepl("^RNG_", ids)), nrow(rng))
  expect_equal(sum(grepl("^CAT_", ids)), sum(cb$type %in% c("binary", "categorical")))
  expect_identical(rules$RNG_002$columns, "age_years")
  for (d in clean[1]) {
    for (r in rules) expect_length(r$check(d), nrow(d))
  }
})

test_that("clean synthetic data gives no error findings and no other findings either", {
  # Clean data passes every rule, so the expected warning and info findings are none.
  expected_non_error <- character()
  for (d in clean) {
    qc <- run_qc(d, cb, rng, rules)
    expect_equal(sum(qc$findings$severity == "error"), 0)
    expect_setequal(unique(qc$findings$rule_id[qc$findings$severity != "error"]),
                    expected_non_error)
    expect_equal(sum(qc$missingness$unexpected_missing), 0)
  }
})

test_that("every injected error is found by its expected rule on its row", {
  runs <- list(
    list(data = clean[[1]], n = 3, seed = 1),
    list(data = clean[[2]], n = 4, seed = 2),
    list(data = clean[[3]], n = 2, seed = 3)
  )
  for (run in runs) {
    bad <- corrupt_synthetic(run$data, cb, n_per_type = run$n, seed = run$seed)
    inj <- attr(bad, "injected")
    expect_setequal(unique(inj$error_type), corrupt_error_types)
    expect_false(anyDuplicated(inj$row) > 0)
    expect_true(all(inj$original != inj$corrupted | is.na(inj$original)))
    qc <- run_qc(bad, cb, rng, rules)
    found <- paste(inj$row, inj$expected_rule_id) %in%
      paste(qc$findings$row, qc$findings$rule_id)
    expect_true(all(found), info = paste(inj$error_type[!found], collapse = ", "))
    expect_equal(mean(found), 1)
  }
  inj <- attr(corrupt_synthetic(clean[[2]], cb, n_per_type = 4, seed = 2), "injected")
  expect_true(all(c("IDN_001", "IDN_002", "IDN_003", "IDN_004") %in% inj$expected_rule_id))
})

test_that("corrupt_synthetic is reproducible and checks its arguments", {
  a <- corrupt_synthetic(clean[[1]], cb, 2, seed = 5)
  expect_identical(a, corrupt_synthetic(clean[[1]], cb, 2, seed = 5))
  expect_identical(names(attr(a, "injected")),
                   c("row", "column", "original", "corrupted", "error_type",
                     "expected_rule_id"))
  changed <- attr(a, "injected")
  for (i in seq_len(nrow(changed))) {
    expect_equal(corrupt_text(a[[changed$column[i]]][changed$row[i]]), changed$corrupted[i])
  }
  untouched <- setdiff(seq_len(nrow(a)), changed$row)
  expect_identical(lapply(a, `[`, untouched), lapply(clean[[1]], `[`, untouched))
  expect_error(corrupt_synthetic(clean[[1]], cb, 0), "positive whole number")
  expect_error(corrupt_synthetic(clean[[1]][1:10, ], cb, 3), "at least")
})

test_that("structure rules report absent and extra columns once per site", {
  d <- clean[[1]][1:3, ]
  expect_identical(check("STR_001", d), rep(FALSE, 3))
  expect_identical(check("STR_002", d), rep(FALSE, 3))
  d$age_years <- NULL
  d$surprise <- 1
  expect_identical(check("STR_001", d), rep(TRUE, 3))
  expect_identical(check("STR_002", d), rep(TRUE, 3))
  qc <- run_qc(d, cb, rng, rules)
  str <- qc$findings[qc$findings$rule_id == "STR_001", ]
  expect_equal(nrow(str), 1)
  expect_true(is.na(str$row))
  expect_identical(str$values, "absent=age_years")
})

test_that("type rules flag values that are not of the codebook type", {
  expect_identical(check("TYP_002", data.frame(age_years = c("45", "abc", NA))), pass_fail_na)
  expect_identical(check("TYP_063", data.frame(initial_gcs = c(14, 7.5, NA))), pass_fail_na)
  expect_identical(check("TYP_072", data.frame(burn_date = c("2020-01-31", "31/01/2020", NA))),
                   pass_fail_na)
  expect_identical(check("TYP_073", data.frame(burn_time = c("08:30", "8.30 pm", NA))),
                   pass_fail_na)
  expect_identical(check("TYP_002", data.frame(x = 1:3)), rep(NA, 3))
})

test_that("category rules flag values outside the allowed values", {
  expect_identical(check("CAT_005", data.frame(hypertension = c("Yes", "Maybe", NA))),
                   pass_fail_na)
  comorb <- c("Hypertension, Diabetes", "Hypertension, Gout", NA)
  expect_identical(check("CAT_004", data.frame(comorbidities_list = comorb)), pass_fail_na)
})

test_that("range rules use qc_ranges.csv", {
  expect_identical(check("RNG_002", data.frame(age_years = c(40, 150, NA))), pass_fail_na)
  expect_identical(check("RNG_026", data.frame(tbsa_burned_pct = c(100, 100.5, NA))),
                   pass_fail_na)
  custom <- qc_rules(cb, data.frame(name = "age_years", min = 0, max = 200, source_note = "x"))
  expect_identical(custom$RNG_002$check(data.frame(age_years = 150)), FALSE)
  expect_warning(qc_rules(cb, data.frame(name = "nope", min = 0, max = 1, source_note = "x")),
                 "not in the codebook")
})

test_that("unit rules flag probable unit mistakes", {
  expect_identical(check("UNI_001", data.frame(hematocrit_0_24h_pct = c(42, 0.42, NA))),
                   pass_fail_na)
  expect_identical(check("UNI_002", data.frame(peak_creatinine_0_72h_umol_l = c(80, 1.1, NA))),
                   pass_fail_na)
  wh <- data.frame(admission_weight_kg = c(80, 80, NA, 176), height_cm = c(180, 1.8, 180, 176))
  expect_identical(check("UNI_003", wh), c(FALSE, TRUE, NA, TRUE))
  ne <- data.frame(peak_ne_24h_ug_kg_min = c(0.2, 0.1, NA),
                   peak_ne_24_48h_ug_kg_min = c(NA, 12, NA))
  expect_identical(check("UNI_004", ne), pass_fail_na)
})

test_that("derived rules recompute BMI, predicted fluid, Baux and comorbidity agreement", {
  bmi <- data.frame(admission_weight_kg = c(80, 80, 80), height_cm = c(180, 180, NA),
                    bmi_kg_m2 = c(24.7, 26, 24.7))
  expect_identical(check("DER_001", bmi), pass_fail_na)
  fluid <- data.frame(admission_weight_kg = c(80, 80, NA), tbsa_prehospital_pct = c(30, 30, 30),
                      predicted_fluid_24h_prehospital_ml = c(9650, 9800, 9600))
  expect_identical(check("DER_002", fluid), pass_fail_na)
  baux <- data.frame(age_years = c(40, 40, 40, 40), tbsa_burned_pct = c(30, 30, 30, NA),
                     inhalation_injury = c("No", "No", "Yes", "Yes"),
                     baux_admission = c(70.5, 87, 87, 70))
  expect_identical(check("DER_003", baux), c(FALSE, TRUE, FALSE, NA))
  expect_identical(check("DER_004", baux), c(FALSE, FALSE, TRUE, NA))
  flags <- as.data.frame(lapply(qc_comorbidity_flags, function(x) rep("No", 3)))
  flags$hypertension <- c("Yes", "No", "Yes")
  flags$diabetes <- c("No", "No", NA)
  flags[3, setdiff(names(qc_comorbidity_flags), "hypertension")] <- NA
  flags$comorbidities_list <- c("Hypertension", "Hypertension", "Hypertension")
  expect_identical(check("DER_005", flags), c(FALSE, TRUE, FALSE))
  flags[3, "hypertension"] <- NA
  expect_identical(check("DER_005", flags)[3], NA)
  ckd <- data.frame(dialysis_dependent_ckd = c("Yes", "Yes", "Yes"),
                    chronic_kidney_disease = c("Yes", "No", NA))
  expect_identical(check("DER_006", ckd), pass_fail_na)
})

test_that("fluid rules check sums, albumin and cumulative order", {
  pre <- data.frame(ra_pre_icu_ml = c(100, 100, NA), nacl_pre_icu_ml = c(50, 50, 50),
                    plasmalyte_pre_icu_ml = c(0, 0, 0),
                    crystalloids_pre_icu_ml = c(150.5, 160, 150))
  expect_identical(check("FLU_001", pre), pass_fail_na)
  tot <- data.frame(crystalloids_24h_ml = c(1000, 1000, 1000), glucose5_24h_ml = c(100, 100, 100),
                    glucose10_24h_ml = c(0, 0, NA), glucose20_24h_ml = c(0, 0, 0),
                    total_fluids_24h_ml = c(1100, 1000, 1100))
  expect_identical(check("FLU_006", tot), pass_fail_na)
  unknown <- data.frame(crystalloids_pre_icu_ml = 100, glucose5_pre_icu_ml = 0,
                        glucose10_pre_icu_ml = 0, glucose20_pre_icu_ml = 0,
                        glucose_unknown_pre_icu_ml = 50, total_fluids_pre_icu_ml = 150)
  expect_identical(check("FLU_005", unknown), FALSE)
  alb <- data.frame(albumin20_24h_ml = c(100, 100, NA), albumin5_24h_ml = c(250, 250, 250),
                    albumin_any_24h_ml = c(350, 300, 350))
  expect_identical(check("FLU_009", alb), pass_fail_na)
  ra <- data.frame(ra_pre_icu_ml = c(100, 100, NA, NA), ra_24h_ml = c(200, 200, NA, 500),
                   ra_48h_ml = c(300, 150, NA, NA), ra_72h_ml = c(400, 400, NA, 499.5))
  expect_identical(check("FLU_010", ra), c(pass_fail_na, FALSE))
  ratio <- data.frame(crystalloids_24h_ml = c(8000, 20000, 8000),
                      admission_weight_kg = c(80, 80, NA), tbsa_burned_pct = c(25, 20, 25))
  expect_identical(check("FLU_020", ratio), pass_fail_na)
})

test_that("hemodynamic rules check pressure order, the MAP formula and window extremes", {
  bp <- data.frame(sap_admission_mmhg = c(120, 70, 120), map_admission_mmhg = c(90, 75, NA),
                   dap_admission_mmhg = c(75, 80, 75))
  expect_identical(check("HEM_001", bp), pass_fail_na)
  bp$map_admission_mmhg <- c(90, 110, NA)
  bp$sap_admission_mmhg <- c(120, 120, 120)
  expect_identical(check("HEM_005", bp), pass_fail_na)
  low <- data.frame(lowest_map_24h_mmhg = c(60, 80, NA, 80),
                    map_admission_mmhg = c(70, 70, 70, 70), map_before_ne_mmhg = NA,
                    map_24h_mmhg = c(65, 90, 65, 90),
                    burn_date = as.Date("2020-01-01"), burn_time = "00:00",
                    icu_admission_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01",
                                                   "2020-01-02")),
                    icu_admission_time = c("06:00", "06:00", "06:00", "06:00"))
  # Row 4: admission 30 h after the burn falls outside 0-24h, so its MAP is not compared.
  expect_identical(check("HEM_009", low), c(pass_fail_na, FALSE))
  high <- data.frame(highest_pulse_24_48h_bpm = c(130, 100, NA), hr_48h_bpm = c(120, 110, 90))
  expect_identical(check("HEM_012", high), pass_fail_na)
  picco <- data.frame(picco_use_0_48h = c("No", "No", NA), ci_24h_l_min_m2 = c(NA, 3.1, 3.1))
  expect_identical(check("HEM_013", picco), pass_fail_na)
})

test_that("vasopressor rules check NE flags, doses, delay and other vasopressors", {
  ne <- data.frame(ne_24h = c("No", "No", NA), ne_24_48h = c("Yes", "No", "No"),
                   ne_48h = c("Yes", "Yes", "Yes"))
  expect_identical(check("VAS_001", ne), pass_fail_na)
  none <- data.frame(ne_48h = c("No", "No", NA), ne_delay_h = c(NA, 5, 5),
                     peak_ne_24h_ug_kg_min = c(0, NA, NA))
  expect_identical(check("VAS_002", none), pass_fail_na)
  peak <- data.frame(ne_24h = c("No", "No", NA), peak_ne_24h_ug_kg_min = c(NA, 0.15, 0.15))
  expect_identical(check("VAS_004", peak), pass_fail_na)
  peak$ne_24h <- c("Yes", "Yes", NA)
  peak$peak_ne_24h_ug_kg_min <- c(0.1, NA, NA)
  expect_identical(check("VAS_003", peak), pass_fail_na)
  delay <- data.frame(ne_delay_h = c(5, 30, NA), ne_24h = c("Yes", "Yes", "Yes"))
  expect_identical(check("VAS_007", delay), pass_fail_na)
  late <- data.frame(ne_delay_h = c(30, 30, NA), ne_24_48h = c("Yes", "No", "No"))
  expect_identical(check("VAS_008", late), pass_fail_na)
  other <- data.frame(other_vp_24h = c("Vasopressin", "Vasopressin", NA),
                      peak_other_vp_24h_ug_kg_min = c(0.03, NA, NA))
  expect_identical(check("VAS_009", other), c(FALSE, TRUE, FALSE))
})

test_that("gating rules check flags against volumes, dates and counts", {
  alb <- data.frame(albumin_24h = c("Yes", "Yes", NA), albumin_any_24h_ml = c(100, 0, 100))
  expect_identical(check("GAT_001", alb), pass_fail_na)
  rbc <- data.frame(rbc_pre_icu = c("Yes", "Yes", NA), rbc_48h_units = c(2L, 0L, 0L))
  expect_identical(check("GAT_007", rbc), pass_fail_na)
  ft <- data.frame(full_thickness = c("No", "No", NA), full_thickness_bsa_pct = c(0, 12, 12))
  expect_identical(check("GAT_013", ft), pass_fail_na)
  ft2 <- data.frame(full_thickness_bsa_pct = c(10, 40, NA), tbsa_burned_pct = c(30, 30, 30))
  expect_identical(check("GAT_014", ft2), pass_fail_na)
  inh <- data.frame(inhalation_injury_grade3 = c("Yes", "Yes", "Yes"),
                    inhalation_injury = c("Yes", "No", NA))
  expect_identical(check("GAT_015", inh), pass_fail_na)
  rrt <- data.frame(crrt = c("Yes", "Yes", NA), rrt = c("Yes", "No", "No"))
  expect_identical(check("GAT_016", rrt), pass_fail_na)
  aki <- data.frame(aki_kdigo_1plus = c("No", "No", NA),
                    aki_kdigo_1plus_date = as.Date(c(NA, "2020-01-05", "2020-01-05")))
  expect_identical(rules$GAT_018$columns, c("aki_kdigo_1plus", "aki_kdigo_1plus_date"))
  expect_identical(check("GAT_018", aki), pass_fail_na)
  aki$aki_kdigo_1plus <- c("Yes", "Yes", NA)
  aki$aki_kdigo_1plus_date <- as.Date(c("2020-01-05", NA, NA))
  expect_identical(check("GAT_019", aki), pass_fail_na)
  sepsis <- data.frame(sepsis = c("No", "No", "Yes"), septic_events_n = c(0L, 2L, 1L),
                       sepsis_dates = as.Date(c(NA, NA, NA)))
  gating <- rules[grepl("^GAT_", names(rules))]
  ids <- names(gating)[vapply(gating, function(r) identical(r$columns[1], "sepsis"), logical(1))]
  expect_identical(check(ids[1], sepsis), c(FALSE, TRUE, FALSE))
  expect_identical(check(ids[2], sepsis), c(FALSE, FALSE, TRUE))
})

test_that("date rules check order, period and event dates", {
  burn <- data.frame(burn_date = as.Date(c("2015-01-01", "2024-01-01", NA)))
  expect_identical(check("DAT_001", burn), pass_fail_na)
  stamp <- data.frame(burn_date = as.Date(c("2020-01-01", "2020-01-01", NA, "2020-01-02")),
                      burn_time = c("06:00", "10:00", "06:00", NA),
                      icu_admission_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01",
                                                     "2020-01-01")),
                      icu_admission_time = c("08:00", "08:00", "08:00", "08:00"))
  expect_identical(check("DAT_002", stamp), c(pass_fail_na, TRUE))
  late <- data.frame(burn_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
                     burn_time = c("06:00", "06:00", NA),
                     icu_admission_date = as.Date(c("2020-01-03", "2020-01-03", NA)),
                     icu_admission_time = c("05:00", "07:00", "07:00"))
  expect_identical(check("DAT_003", late), pass_fail_na)
  two <- data.frame(icu_admission_date = as.Date(c("2020-01-01", "2020-01-01", NA)),
                    outcome_icu_admission_date = as.Date(c("2020-01-01", "2020-01-02", NA)))
  expect_identical(check("DAT_004", two), pass_fail_na)
  readmit <- data.frame(icu_discharge_date = as.Date(rep("2020-02-01", 3)),
                        icu_readmission_date = as.Date(c("2020-02-03", "2020-02-01", NA)))
  expect_identical(check("DAT_006", readmit), pass_fail_na)
  event <- data.frame(burn_date = as.Date(rep("2020-01-01", 4)),
                      follow_up_end_date = as.Date(c("2020-06-01", "2020-06-01", NA, NA)),
                      ards_date = as.Date(c("2020-01-10", "2020-07-01", NA, "2019-12-31")))
  dates <- rules[grepl("^DAT_", names(rules))]
  id <- names(dates)[vapply(dates, function(r) identical(r$columns[1], "ards_date"), logical(1))]
  expect_identical(check(id, event), c(pass_fail_na, TRUE))
})

test_that("duration rules accept plain and inclusive day counts within 1 day", {
  base <- as.Date("2020-01-01")
  los <- data.frame(icu_admission_date = base, icu_discharge_date = base + 10,
                    icu_los_days = c(10, 11, 12, 9, 13, 8, NA))
  expect_identical(check("DUR_001", los), c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, NA))
  rrt <- data.frame(burn_date = base, burn_time = c("12:00", "12:00", NA),
                    rrt_start_date = base + 5, rrt_start_days = c(4.5, 9, 5))
  expect_identical(check("DUR_004", rrt), c(FALSE, TRUE, FALSE))
  hosp <- data.frame(hospital_los_days = c(20, 5, NA), icu_los_days = c(10, 10, 10))
  expect_identical(check("DUR_005", hosp), pass_fail_na)
})

test_that("outcome rules check mortality and vital status", {
  icu <- data.frame(icu_mortality = c("Yes", "Yes", NA), hospital_mortality = c("Yes", "No", "No"))
  expect_identical(check("OUT_001", icu), pass_fail_na)
  vital <- data.frame(hospital_mortality = c("Yes", "Yes", "Yes"),
                      vital_status_follow_up = c("Dead", "alive", NA))
  expect_identical(check("OUT_002", vital), pass_fail_na)
  later <- data.frame(hospital_mortality = c("No", "No", NA),
                      vital_status_follow_up = c("Alive", "Dead", "Dead"))
  expect_identical(check("OUT_003", later), pass_fail_na)
})

test_that("duplicate rules work within each site", {
  ids <- data.frame(site = c("NBC", "UTMB", "NBC", "NBC", "NBC"),
                    patient_id = c("A1", "A1", "B2", "B2", NA))
  expect_identical(check("DUP_001", ids), c(FALSE, FALSE, TRUE, TRUE, TRUE))
  pat <- data.frame(site = c("NBC", "NBC", "NBC", "UTMB", "NBC"),
                    burn_date = as.Date("2020-01-01"), age_years = c(40.2, 39.8, 40, 40, NA),
                    gender = "Male", tbsa_burned_pct = c(30, 30, 31, 30, 30))
  expect_identical(check("DUP_002", pat), c(TRUE, TRUE, FALSE, FALSE, NA))
})

test_that("identifier rules flag personal numbers, SSNs, emails and phone numbers", {
  cells <- c("SYN-0001", "Flame", "Hypertension, Diabetes", "2019-01-15", "12.03.2019",
             "01019012345", "010190 12345", "123-45-6789", "jane.doe@example.com",
             "+47 912 34 567", "(409) 555-1234", "912 34 567", "NBC-2019-00123", NA)
  d <- data.frame(burn_mechanism = cells)
  flagged <- lapply(c("IDN_001", "IDN_002", "IDN_003", "IDN_004"), function(id) {
    which(check(id, d))
  })
  expect_identical(flagged[[1]], 6:7)
  expect_identical(flagged[[2]], 8L)
  expect_identical(flagged[[3]], 9L)
  expect_true(all(c(10L, 11L, 12L) %in% flagged[[4]]))
  expect_false(any(c(1:6, 13:14) %in% flagged[[4]]))
  # A blank cell cannot hold an identifier, so identifier rules are never "not evaluable".
  expect_false(any(is.na(check("IDN_001", d))))
  qc <- run_qc(with_site(data.frame(patient_id = "P1", covid19_status = "01019012345")), cb,
               rng, rules)
  idn <- qc$findings[qc$findings$rule_id == "IDN_001", ]
  expect_identical(idn$values, "covid19_status=[redacted]")
  expect_false(any(grepl("01019012345", unlist(qc$findings))))
})

test_that("run_qc returns findings in the documented format", {
  d <- clean[[1]][1:5, ]
  d$age_years[2] <- 150
  d$burn_date[3] <- as.Date("2014-06-01")
  qc <- run_qc(d, cb, rng, rules)
  expect_named(qc, c("findings", "rule_summary", "missingness", "parse_problems",
                     "unmapped_values", "rule_catalogue"))
  expect_named(qc$findings, c("site", "row", "patient_id", "rule_id", "category", "severity",
                              "columns", "values", "message"))
  age <- qc$findings[qc$findings$rule_id == "RNG_002", ]
  expect_equal(age$row, 2)
  expect_identical(age$patient_id, d$patient_id[2])
  expect_identical(age$values, "age_years=150")
  burn <- qc$findings[qc$findings$rule_id == "DAT_001", ]
  expect_identical(burn$values, "burn_date=2014-06-01")
  expect_identical(nrow(qc$rule_catalogue), length(rules))
  expect_named(qc$rule_catalogue, c("id", "category", "severity", "description", "columns"))
  expect_equal(nrow(qc$parse_problems), 0)
  expect_equal(nrow(qc$unmapped_values), 0)
  expect_error(run_qc(d[, -1], cb, rng, rules), "site")
})

test_that("run_qc carries over the loader and standardisation tables", {
  d <- clean[[1]][1:2, ]
  pp <- data.frame(row = 1L, column = "age_years", raw_value = "forty", expected_type = "numeric")
  um <- data.frame(column = "gender", value = "M?", n = 1L)
  attr(d, "parse_problems") <- pp
  attr(d, "unmapped_values") <- um
  qc <- run_qc(d, cb, rng, rules)
  expect_identical(qc$parse_problems, pp)
  expect_identical(qc$unmapped_values, um)
})

test_that("unexpected values do not crash QC and are flagged", {
  d <- clean[[1]][1:4, ]
  binary <- cb$name[cb$type == "binary"]
  d[binary] <- lapply(d[binary], function(x) rep("Maybe", length(x)))
  d$age_years <- as.character(d$age_years)
  d$age_years[1] <- "forty"
  d$icu_admission_time[2] <- "25:99"
  qc <- expect_silent(run_qc(d, cb, rng, rules))
  ids <- unique(qc$findings$rule_id)
  expect_true(all(qc_position_id("CAT", cb, binary) %in% ids))
  expect_true(all(c("TYP_002", "TYP_071") %in% ids))
  broken <- c(rules["RNG_002"], list(qc_rule("ZZZ_001", "range", "error", "Broken rule.",
                                             "age_years", function(d) stop("boom"))))
  expect_warning(qc2 <- run_qc(d, cb, rng, broken), "ZZZ_001 could not be evaluated")
  expect_equal(qc2$rule_summary$n_not_evaluable[qc2$rule_summary$rule_id == "ZZZ_001"], 4)
})

test_that("rule_summary counts add up to the rows of each site", {
  d <- rbind(clean[[1]][1:30, ], with_site(generate_synthetic_nbc(cb, n = 20, seed = 7), "UTMB"))
  d <- corrupt_synthetic(d, cb, n_per_type = 2, seed = 4)
  qc <- run_qc(d, cb, rng, rules)
  s <- qc$rule_summary
  expect_named(s, c("site", "rule_id", "category", "severity", "n_rows", "n_evaluated",
                    "n_failed", "n_not_evaluable", "n_passed"))
  expect_equal(nrow(s), 2 * length(rules))
  site_rows <- c(NBC = 30, UTMB = 20)
  expect_equal(s$n_rows, unname(site_rows[s$site]))
  expect_equal(s$n_failed + s$n_not_evaluable + s$n_passed, s$n_rows)
  expect_equal(s$n_evaluated, s$n_failed + s$n_passed)
  row_findings <- qc$findings[!is.na(qc$findings$row), ]
  failed <- stats::aggregate(row ~ site + rule_id, data = row_findings, FUN = length)
  merged <- merge(failed, s, by = c("site", "rule_id"))
  expect_equal(merged$row, merged$n_failed)
})

test_that("missingness separates structural from unexpected blanks for NE-gated columns", {
  d <- with_site(data.frame(
    patient_id = paste0("P", 1:5),
    ne_24h = c("Yes", "No", "No", "Yes", NA),
    ne_24_48h = c("No", "No", "No", "No", NA),
    ne_48h = c("Yes", "No", "No", "Yes", NA),
    peak_ne_24h_ug_kg_min = c(0.2, NA, NA, NA, NA),
    ne_delay_h = c(3, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  ))
  qc <- run_qc(d, cb, rng, rules)
  m <- qc$missingness
  expect_named(m, c("site", "variable", "n", "n_missing", "pct_missing", "structural_missing",
                    "unexpected_missing", "pct_unexpected_missing", "key_variable",
                    "flag_high_missing"))
  expect_equal(nrow(m), nrow(cb))
  peak <- m[m$variable == "peak_ne_24h_ug_kg_min", ]
  expect_equal(c(peak$n, peak$n_missing, peak$structural_missing, peak$unexpected_missing),
               c(5, 4, 2, 2))
  expect_equal(peak$pct_missing, 80)
  expect_true(peak$key_variable)
  expect_true(peak$flag_high_missing)
  delay <- m[m$variable == "ne_delay_h", ]
  expect_equal(c(delay$structural_missing, delay$unexpected_missing), c(2, 2))
  other <- m[m$variable == "other_vp_24h", ]
  expect_equal(c(other$structural_missing, other$unexpected_missing), c(5, 0))
  expect_false(m$flag_high_missing[m$variable == "other_vp_24h"])
  ages <- m[m$variable == "age_years", ]
  expect_equal(c(ages$n_missing, ages$unexpected_missing), c(5, 5))
})

test_that("write_qc_outputs writes six files inside output and refuses other folders", {
  dir <- local_test_dir("qc-outputs")
  qc <- run_qc(corrupt_synthetic(clean[[1]], cb, 1, 1), cb, rng, rules)
  paths <- write_qc_outputs(qc, dir)
  expected <- c("findings.csv", "rule_summary.csv", "missingness.csv", "parse_problems.csv",
                "unmapped_values.csv", "rule_catalogue.csv")
  expect_setequal(basename(paths), expected)
  expect_setequal(list.files(dir), expected)
  back <- utils::read.csv(file.path(dir, "findings.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(back), nrow(qc$findings))
  catalogue <- utils::read.csv(file.path(dir, "rule_catalogue.csv"), stringsAsFactors = FALSE)
  expect_named(catalogue, c("id", "category", "severity", "description", "columns"))

  not_output <- file.path(project_root, "R")
  before <- list.files(not_output)
  expect_error(write_qc_outputs(qc, not_output), "inside <project root>/output/")
  expect_identical(list.files(not_output), before)
  expect_error(write_qc_outputs(qc, file.path(dir, "does-not-exist")), "Nothing was written")
  expect_error(write_qc_outputs(qc, file.path(dir, "..", "..", "..", "R")), "inside")
  expect_error(write_qc_outputs(qc[1:3], dir), "missing")
})
