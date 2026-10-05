cb <- read_codebook(testthat::test_path("..", "..", "metadata", "codebook.csv"))
project_root <- normalizePath(testthat::test_path("..", ".."))

syn <- generate_synthetic_nbc(cb, n = 100, seed = 1)
datasets <- list(syn, generate_synthetic_nbc(cb, n = 200, seed = 2),
                 generate_synthetic_nbc(cb, n = 37, seed = 3))

yes <- function(x) x == "Yes"
icu_delay_hours <- function(d) {
  icu <- as.POSIXct(paste(d$icu_admission_date, d$icu_admission_time), tz = "UTC")
  burn <- as.POSIXct(paste(d$burn_date, d$burn_time), tz = "UTC")
  as.numeric(difftime(icu, burn, units = "hours"))
}
day_diff <- function(a, b) as.numeric(a) - as.numeric(b)
all_true <- function(x) all(x, na.rm = TRUE)

test_that("the data frame has the codebook layout", {
  expect_s3_class(syn, "data.frame")
  expect_equal(dim(syn), c(100L, nrow(cb)))
  expect_identical(names(syn), cb$name[order(cb$position)])
  expect_equal(nrow(generate_synthetic_nbc(cb, n = 7, seed = 4)), 7L)
  for (i in seq_len(nrow(cb))) {
    x <- syn[[cb$name[i]]]
    ok <- switch(cb$type[i], numeric = is.numeric(x), integer = is.integer(x),
                 date = inherits(x, "Date"), is.character(x))
    expect_true(ok, info = cb$name[i])
  }
  expect_true(all(grepl("^[0-9]{2}:[0-9]{2}$", syn$burn_time)))
  expect_true(all(grepl("^[0-9]{2}:[0-9]{2}$", syn$icu_admission_time)))
})

test_that("the same seed gives identical output and the caller's random state is kept", {
  expect_identical(generate_synthetic_nbc(cb, 30, seed = 9), generate_synthetic_nbc(cb, 30, 9))
  expect_false(identical(generate_synthetic_nbc(cb, 30, seed = 9),
                         generate_synthetic_nbc(cb, 30, seed = 10)))
  set.seed(123)
  before <- .Random.seed
  generate_synthetic_nbc(cb, 20, seed = 5)
  expect_identical(.Random.seed, before)
})

test_that("binary, categorical and text values are allowed", {
  for (d in datasets) {
    for (i in which(cb$type %in% c("binary", "categorical"))) {
      allowed <- strsplit(cb$allowed_values[i], "|", fixed = TRUE)[[1]]
      x <- d[[cb$name[i]]]
      if (cb$name[i] == "comorbidities_list") {
        x <- unlist(strsplit(x[!is.na(x)], ", ", fixed = TRUE))
      }
      expect_true(all(x[!is.na(x)] %in% allowed), info = cb$name[i])
    }
    expect_true(all(d$burn_mechanism %in% c("Thermal", "Electrical", "Chemical")))
    expect_true(all(d$covid19_status %in% c("Negative", "Positive")))
    expect_true(all(d$vital_status_follow_up %in% c("Alive", "Dead")))
    expect_false(anyDuplicated(d$patient_id) > 0)
  }
  binary_cols <- cb$name[cb$type == "binary"]
  for (d in datasets) expect_true(all(unlist(d[binary_cols]) %in% c("Yes", "No")))
})

test_that("the cohort is plausible", {
  for (d in datasets) {
    expect_true(all(d$age_years >= 18 & d$age_years <= 90))
    expect_true(all(d$gender %in% c("Male", "Female")))
    expect_true(all(d$burn_date >= as.Date("2015-01-01") & d$burn_date <= as.Date("2023-12-31")))
    expect_true(all(d$tbsa_burned_pct >= 15 & d$tbsa_burned_pct <= 85))
    expect_true(all(d$admission_weight_kg >= 45 & d$admission_weight_kg <= 140))
    expect_true(all(d$height_cm >= 150 & d$height_cm <= 200))
    expect_equal(d$bmi_kg_m2, round(d$admission_weight_kg / (d$height_cm / 100)^2, 2))
  }
  expect_true(mean(syn$tbsa_burned_pct >= 20 & syn$tbsa_burned_pct <= 40) > 0.5)
  expect_equal(names(which.max(table(syn$burn_sub_mechanism))), "Flame")
  expect_true(length(unique(syn$burn_date)) > 80)
})

test_that("comorbidity flags agree with the list", {
  flags <- c(hypertension = "Hypertension", diabetes = "Diabetes",
             coronary_artery_disease = "Coronary artery disease",
             chronic_kidney_disease = "Chronic kidney disease",
             dialysis_dependent_ckd = "Dialysis dependent CKD",
             congestive_heart_failure = "Congestive heart failure",
             peripheral_vascular_disease = "Peripheral vascular disease", stroke = "Stroke",
             copd = "Chronic obstructive pulmonary disease", cirrhosis = "Cirrhosis",
             cancer = "Cancer", immunosuppression = "Immunosuppression",
             psychiatry = "Psychiatry", alcohol_opioid_addiction = "Alcohol or opioid addiction")
  for (d in datasets) {
    listed <- strsplit(ifelse(is.na(d$comorbidities_list), "", d$comorbidities_list), ", ",
                       fixed = TRUE)
    for (f in names(flags)) {
      expect_identical(vapply(listed, function(l) flags[[f]] %in% l, logical(1)), yes(d[[f]]),
                       info = f)
    }
    expect_true(all(!yes(d$dialysis_dependent_ckd) | yes(d$chronic_kidney_disease)))
    prevalence <- vapply(names(flags), function(f) mean(yes(d[[f]])), numeric(1))
    expect_true(all(prevalence < 0.4))
  }
})

test_that("derived values and scores are consistent", {
  for (d in datasets) {
    expect_equal(d$predicted_fluid_24h_prehospital_ml,
                 4 * d$admission_weight_kg * d$tbsa_prehospital_pct)
    expect_equal(d$baux_admission, d$age_years + d$tbsa_burned_pct)
    expect_true(all(d$charlson_index >= 0 & d$charlson_index <= 15))
    expect_true(all(d$absi_admission >= 2 & d$absi_admission <= 18))
    expect_true(all(d$sofa_admission >= 0 & d$sofa_admission <= 24))
    expect_true(all(d$saps2_admission >= 0 & d$saps2_admission <= 163))
    expect_true(all(d$saps3_admission >= 0 & d$saps3_admission <= 217))
    expect_true(all(d$initial_gcs >= 3 & d$initial_gcs <= 15))
    expect_true(all(yes(d$inhalation_injury) | !yes(d$inhalation_injury_grade3)))
    expect_true(all(is.na(d$full_thickness_bsa_pct) == !yes(d$full_thickness)))
    expect_true(all_true(d$full_thickness_bsa_pct <= d$tbsa_burned_pct))
  }
})

test_that("fluid totals and cumulative order hold", {
  fluids <- c("ra", "nacl", "plasmalyte", "glucose5", "glucose10", "glucose20")
  windows <- c("pre_icu", "24h", "48h", "72h")
  for (d in datasets) {
    for (w in windows) {
      g <- function(f) d[[paste0(f, "_", w, "_ml")]]
      cryst <- g("ra") + g("nacl") + g("plasmalyte")
      expect_equal(g("crystalloids"), cryst)
      glucose <- g("glucose5") + g("glucose10") + g("glucose20")
      if (w == "pre_icu") glucose <- glucose + d$glucose_unknown_pre_icu_ml
      expect_equal(g("total_fluids"), cryst + glucose)
    }
    for (f in c(fluids, "crystalloids", "total_fluids")) {
      for (k in 1:3) {
        expect_true(all(d[[paste0(f, "_", windows[k + 1], "_ml")]] >=
                          d[[paste0(f, "_", windows[k], "_ml")]]), info = f)
      }
    }
    ratio <- d$crystalloids_24h_ml / d$admission_weight_kg / d$tbsa_burned_pct
    expect_true(all(ratio >= 2 & ratio <= 6))
    expect_true(all(unlist(d[grep("_ml$", names(d))]) >= 0))
  }
})

test_that("albumin, red cell and plasma flags follow volumes, counts and delays", {
  for (d in datasets) {
    expect_equal(d$albumin_any_24h_ml, d$albumin20_24h_ml + d$albumin5_24h_ml)
    expect_identical(yes(d$albumin_24h), d$albumin_any_24h_ml > 0)
    expect_identical(yes(d$albumin_48h), d$albumin20_48h_ml + d$albumin5_48h_ml > 0)
    expect_true(all(d$albumin20_48h_ml >= d$albumin20_24h_ml))
    expect_true(all(d$albumin5_48h_ml >= d$albumin5_24h_ml))
    expect_true(all(!yes(d$albumin_pre_icu) | yes(d$albumin_24h)))
    expect_identical(is.na(d$albumin_delay_h), !yes(d$albumin_48h))
    expect_true(all_true(!yes(d$albumin_24h) | d$albumin_delay_h < 24))
    expect_identical(yes(d$venofundin_48h), d$venofundin_48h_ml > 0)
    for (p in c("rbc", "ffp")) {
      units <- d[[paste0(p, "_48h_units")]]
      delay <- d[[paste0(p, "_delay_h")]]
      pre <- yes(d[[paste0(p, "_pre_icu")]])
      expect_identical(is.na(delay), units == 0L)
      expect_true(all(!pre | units > 0))
      expect_true(all_true(pre == (delay < icu_delay_hours(d))))
    }
  }
})

test_that("blood pressure and pulse values are ordered", {
  for (d in datasets) {
    for (w in c("admission", "before_ne", "24h", "48h")) {
      sap <- d[[paste0("sap_", w, "_mmhg")]]
      map <- d[[paste0("map_", w, "_mmhg")]]
      dap <- d[[paste0("dap_", w, "_mmhg")]]
      expect_true(all_true(sap > map & map > dap), info = w)
      expect_true(all_true(abs(map - (sap + 2 * dap) / 3) <= 3), info = w)
    }
    expect_true(all_true(d$lowest_map_24h_mmhg <= d$map_admission_mmhg))
    expect_true(all_true(d$lowest_map_24h_mmhg <= d$map_before_ne_mmhg))
    expect_true(all_true(d$lowest_map_24h_mmhg <= d$map_24h_mmhg))
    expect_true(all_true(d$lowest_map_24_48h_mmhg <= d$map_48h_mmhg))
    expect_true(all_true(d$highest_pulse_24h_bpm >= d$hr_admission_bpm))
    expect_true(all_true(d$highest_pulse_24h_bpm >= d$hr_before_ne_bpm))
    expect_true(all_true(d$highest_pulse_24h_bpm >= d$hr_24h_bpm))
    expect_true(all_true(d$highest_pulse_24_48h_bpm >= d$hr_48h_bpm))
  }
})

test_that("CI and CVP exist only with PiCCO use", {
  for (d in datasets) {
    picco <- yes(d$picco_use_0_48h)
    for (col in c("ci_admission_l_min_m2", "cvp_admission_mmhg", "ci_24h_l_min_m2",
                  "cvp_24h_mmhg", "ci_48h_l_min_m2", "cvp_48h_mmhg")) {
      expect_identical(!is.na(d[[col]]), picco, info = col)
    }
    for (col in c("ci_before_ne_l_min_m2", "cvp_before_ne_mmhg")) {
      expect_true(all(is.na(d[[col]][!picco])), info = col)
    }
    expect_identical(!is.na(d$map_before_ne_mmhg), yes(d$ne_48h))
  }
})

test_that("norepinephrine fields are gated and consistent", {
  for (d in datasets) {
    ne24 <- yes(d$ne_24h)
    ne2448 <- yes(d$ne_24_48h)
    none <- !ne24 & !ne2448
    expect_identical(yes(d$ne_48h), ne24 | ne2448)
    for (col in c("ne_delay_h", "peak_ne_24h_ug_kg_min", "peak_ne_24_48h_ug_kg_min",
                  "total_ne_48h_ug_kg", "days_on_vp", "other_vp_24h", "other_vp_24_48h")) {
      expect_true(all(is.na(d[[col]][none])), info = col)
    }
    expect_identical(!is.na(d$ne_delay_h), !none)
    expect_true(all_true((d$ne_delay_h < 24) == ne24))
    expect_true(all_true(d$ne_delay_h >= 0 & d$ne_delay_h < 48))
    expect_identical(!is.na(d$peak_ne_24h_ug_kg_min), ne24)
    expect_identical(!is.na(d$peak_ne_24_48h_ug_kg_min), ne2448)
    peaks <- c(d$peak_ne_24h_ug_kg_min, d$peak_ne_24_48h_ug_kg_min)
    expect_true(all(peaks[!is.na(peaks)] >= 0.01 & peaks[!is.na(peaks)] <= 1.5))
    bound <- 1440 * (ifelse(ne24, d$peak_ne_24h_ug_kg_min, 0) +
                       ifelse(ne2448, d$peak_ne_24_48h_ug_kg_min, 0))
    expect_true(all_true(d$total_ne_48h_ug_kg > 0 & d$total_ne_48h_ug_kg <= bound))
    expect_true(all(is.na(d$other_vp_24h) | ne24))
    expect_true(all(is.na(d$other_vp_24_48h) | ne2448))
    expect_identical(!is.na(d$peak_other_vp_24h_ug_kg_min), !is.na(d$other_vp_24h))
    expect_identical(!is.na(d$peak_other_vp_24_48h_ug_kg_min), !is.na(d$other_vp_24_48h))
    expect_true(all(!yes(d$ne_pre_burn_icu) | yes(d$ne_48h)))
  }
  expect_true(mean(yes(syn$ne_48h)) >= 0.4 && mean(yes(syn$ne_48h)) <= 0.6)
  with_other <- !is.na(syn$other_vp_24h) | !is.na(syn$other_vp_24_48h)
  expect_true(any(with_other) && sum(with_other) < sum(yes(syn$ne_48h)))
})

test_that("laboratory values are in range", {
  for (d in datasets) {
    expect_true(all(d$peak_lactate_0_24h_mmol_l >= 0.5 & d$peak_lactate_0_24h_mmol_l <= 15))
    expect_true(all(d$peak_hemoglobin_0_24h_g_dl >= 8 & d$peak_hemoglobin_0_24h_g_dl <= 20))
    expect_true(all(d$hematocrit_0_24h_pct >= 25 & d$hematocrit_0_24h_pct <= 60))
    expect_true(all(d$peak_creatinine_0_72h_umol_l >= 40 & d$peak_creatinine_0_72h_umol_l <= 600))
    expect_true(all(d$diuresis_0_48h_ml_kg_h >= 0.1 & d$diuresis_0_48h_ml_kg_h <= 3))
  }
})

test_that("dates and durations are consistent", {
  for (d in datasets) {
    expect_identical(d$icu_admission_date, d$outcome_icu_admission_date)
    burn_stamp <- as.POSIXct(paste(d$burn_date, d$burn_time), tz = "UTC")
    icu_stamp <- as.POSIXct(paste(d$icu_admission_date, d$icu_admission_time), tz = "UTC")
    hours <- as.numeric(difftime(icu_stamp, burn_stamp, units = "hours"))
    expect_true(all(hours >= 1 & hours <= 24))

    event_dates <- c("aki_kdigo_1plus_date", "aki_kdigo_3_date", "ards_date",
                     "mesenteric_ischemia_date", "acs_date", "sepsis_dates", "bacteremia_dates",
                     "candidemia_dates", "rrt_start_date", "rrt_stop_date", "intubation_date",
                     "vent_stop_date", "last_graft_surgery_date")
    for (col in event_dates) {
      expect_true(all_true(d[[col]] >= d$burn_date), info = col)
      expect_true(all_true(d[[col]] <= d$icu_discharge_date), info = col)
    }
    first_surgery <- as.POSIXct(paste(d$burn_date, d$burn_time), tz = "UTC") +
      d$first_surgery_days * 86400
    expect_true(all_true(as.Date(first_surgery) <= d$icu_discharge_date))
    expect_true(all_true(as.Date(first_surgery) <= d$last_graft_surgery_date |
                           is.na(d$last_graft_surgery_date)))
    expect_true(all(d$follow_up_end_date >= d$icu_admission_date + d$hospital_los_days))
    expect_equal(day_diff(d$icu_discharge_date, d$icu_admission_date), d$icu_los_days)
    expect_true(all(d$hospital_los_days >= d$icu_los_days))
    expect_true(all(d$icu_los_days >= 1))
    expect_equal(d$mech_vent_days, day_diff(d$vent_stop_date, d$intubation_date))
    expect_true(all_true(d$vent_stop_date >= d$intubation_date))
    expect_equal(d$crrt_days, day_diff(d$rrt_stop_date, d$rrt_start_date))
    expect_true(all_true(d$rrt_stop_date >= d$rrt_start_date))
    expect_equal(d$rrt_start_days,
                 round(as.numeric(difftime(as.POSIXct(d$rrt_start_date), burn_stamp,
                                           units = "days")), 2), tolerance = 1e-6)
    expect_true(all_true(d$rrt_start_days > 0))
    expect_true(all_true(d$aki_kdigo_1plus_date <= d$aki_kdigo_3_date))
    expect_true(all_true(d$icu_readmission_date > d$icu_discharge_date))
    expect_true(all_true(d$icu_readmission_date <= d$icu_admission_date + d$hospital_los_days))
  }
})

test_that("complication flags control their date and count columns", {
  pairs <- list(
    aki_kdigo_1plus = "aki_kdigo_1plus_date", aki_kdigo_3 = "aki_kdigo_3_date",
    ards = "ards_date", mesenteric_ischemia = "mesenteric_ischemia_date",
    abdominal_compartment_syndrome = "acs_date",
    sepsis = c("septic_events_n", "sepsis_dates"), bacteremia = "bacteremia_dates",
    candidemia = "candidemia_dates",
    rrt = c("rrt_start_days", "rrt_start_date", "rrt_stop_date"),
    crrt = "crrt_days", mech_vent = c("mech_vent_days", "intubation_date", "vent_stop_date"),
    icu_readmission = "icu_readmission_date"
  )
  for (d in datasets) {
    for (flag in names(pairs)) {
      for (col in pairs[[flag]]) {
        expect_identical(!is.na(d[[col]]), yes(d[[flag]]), info = paste(flag, col))
      }
    }
    expect_identical(d$crrt, d$rrt)
    expect_true(all(!yes(d$rrt) | yes(d$aki_kdigo_3)))
    expect_true(all(!yes(d$aki_kdigo_3) | yes(d$aki_kdigo_1plus)))
    expect_true(all(!yes(d$mech_vent_admission) | yes(d$mech_vent)))
    expect_true(all(!yes(d$mech_vent_24h) | yes(d$mech_vent)))
    expect_true(all(!yes(d$mech_vent_admission) | yes(d$mech_vent_24h)))
    expect_true(all(!yes(d$graft_surgery_48h) | yes(d$graft_surgery_7d)))
    expect_true(all(!yes(d$graft_surgery_7d) | d$graft_surgeries_7d_n >= 1))
    expect_true(all(d$surgeries_total_n >= d$graft_surgeries_7d_n))
    expect_true(all(is.na(d$last_graft_surgery_date) | d$surgeries_total_n >= 1))
  }
})

test_that("mortality fields agree and outcome rates are plausible", {
  for (d in datasets) {
    expect_true(all(!yes(d$icu_mortality) | yes(d$hospital_mortality)))
    expect_identical(d$vital_status_follow_up == "Dead", yes(d$hospital_mortality))
    expect_true(all(d$hospital_los_days[yes(d$icu_mortality)] ==
                      d$icu_los_days[yes(d$icu_mortality)]))
    expect_true(all(!yes(d$icu_readmission) | !yes(d$hospital_mortality)))
    dead <- yes(d$hospital_mortality)
    expect_true(all(d$follow_up_end_date[dead] ==
                      (d$icu_admission_date + d$hospital_los_days)[dead]))
  }
  rate <- function(x) mean(yes(x))
  expect_true(rate(syn$hospital_mortality) >= 0.15 && rate(syn$hospital_mortality) <= 0.30)
  expect_true(rate(syn$rrt) >= 0.10 && rate(syn$rrt) <= 0.15)
  expect_true(rate(syn$sepsis) >= 0.20 && rate(syn$sepsis) <= 0.35)
  dead <- yes(syn$hospital_mortality)
  expect_true(mean(syn$age_years[dead]) > mean(syn$age_years[!dead]))
  expect_true(mean(syn$tbsa_burned_pct[dead]) > mean(syn$tbsa_burned_pct[!dead]))
})

test_that("only structural missingness is present", {
  always_present <- c("patient_id", "age_years", "gender", "admission_weight_kg", "height_cm",
                      "tbsa_burned_pct", "burn_date", "burn_time", "icu_admission_date",
                      "icu_discharge_date", "hospital_los_days", "follow_up_end_date",
                      "peak_lactate_0_24h_mmol_l", "crystalloids_72h_ml", "map_24h_mmhg",
                      "map_48h_mmhg", "saps2_admission", "charlson_index")
  expect_false(anyNA(syn[always_present]))
})

test_that("a written workbook reads back with the loader and matches the data", {
  out_dir <- project_output_dir(project_root, "test-tmp")
  withr::defer(unlink(out_dir, recursive = TRUE))
  path <- file.path(out_dir, "roundtrip.xlsx")
  small <- generate_synthetic_nbc(cb, n = 10, seed = 11)
  write_nbc_xlsx(small, cb, path)

  expect_identical(readxl::excel_sheets(path), "Ark1")
  raw <- suppressMessages(readxl::read_excel(path, col_names = FALSE, col_types = "text"))
  expect_equal(as.character(unlist(raw[2, ])), cb$source_name)
  sections <- unlist(raw[1, ])
  expect_equal(unname(sections[!is.na(sections)]), unique(cb$section))
  expect_equal(nrow(raw), 12L)

  back <- read_site_data(path, cb, "NBC")
  expect_equal(nrow(attr(back, "parse_problems")), 0L)
  expect_equal(nrow(back), 10L)
  expect_true(all(back$site == "NBC"))
  for (nm in names(small)) {
    expect_equal(back[[nm]], small[[nm]], tolerance = 1e-8, info = nm)
  }
})

test_that("write_nbc_xlsx rejects data that does not match the codebook", {
  out_dir <- project_output_dir(project_root, "test-tmp")
  withr::defer(unlink(out_dir, recursive = TRUE))
  expect_error(write_nbc_xlsx(syn[, -1], cb, file.path(out_dir, "bad.xlsx")),
               "must match codebook names")
  expect_false(file.exists(file.path(out_dir, "bad.xlsx")))
})

test_that("project_output_dir refuses an unconfirmed project folder and writes nothing", {
  not_root <- file.path(project_root, "R")
  before <- list.files(not_root, all.files = TRUE, recursive = TRUE)
  expect_error(project_output_dir(not_root, "generated"), "not confirmed")
  expect_identical(list.files(not_root, all.files = TRUE, recursive = TRUE), before)
  expect_false(dir.exists(file.path(not_root, "output")))
  expect_error(project_output_dir(file.path(project_root, "does-not-exist"), "x"), "not confirmed")
  expect_false(is_project_root(NA_character_))
})

test_that("project_output_dir keeps the subfolder inside output", {
  expect_error(project_output_dir(project_root, "../escape"), "inside output")
  expect_error(project_output_dir(project_root, "/tmp/escape"), "inside output")
  expect_error(project_output_dir(project_root, ""), "inside output")
  dir <- project_output_dir(project_root, "test-tmp/nested")
  withr::defer(unlink(file.path(project_root, "output", "test-tmp"), recursive = TRUE))
  expect_true(dir.exists(dir))
  expect_identical(basename(dir), "nested")
  expect_true(startsWith(dir, normalizePath(file.path(project_root, "output"), winslash = "/")))
})

test_that("is_project_root checks the Project field exactly", {
  expect_true(is_project_root(project_root))
  base <- project_output_dir(project_root, "test-tmp/roots")
  withr::defer(unlink(file.path(project_root, "output", "test-tmp"), recursive = TRUE))
  make_root <- function(name, text) {
    dir <- file.path(base, name)
    dir.create(dir)
    writeLines(text, file.path(dir, "PROJECT_VERSION"))
    dir
  }
  expect_false(is_project_root(make_root("other", c("Project: other", "Version: 1"))))
  expect_false(is_project_root(make_root("prefix", c("Project: burn_vp_copy", "Version: 1"))))
  expect_false(is_project_root(make_root("empty", "")))
  expect_true(is_project_root(make_root("good", c("Project: burn_vp", "Version: 1"))))
})
