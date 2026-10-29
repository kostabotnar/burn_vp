syn_clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# Sets entries outside `flag` to NA, keeping the type of x.
syn_mask <- function(x, flag) {
  x[!flag] <- NA
  x
}

syn_yn <- function(flag) ifelse(flag, "Yes", "No")

syn_r10 <- function(x) round(x / 10) * 10

# Whole-day numbers drawn uniformly from lo..hi. Rows with hi < lo are masked by the caller.
syn_rand_day <- function(lo, hi) lo + floor(stats::runif(length(lo)) * (pmax(hi, lo) - lo + 1))

syn_as_date <- function(day) structure(as.numeric(day), class = "Date")

syn_clock <- function(minutes) {
  sprintf("%02d:%02d", as.integer(minutes %/% 60), as.integer(minutes %% 60))
}

# Picks round(n * frac) rows without replacement, favouring rows with a high weight. A fixed
# count keeps the prevalence of key outcomes inside the requested range for any seed.
syn_pick <- function(frac, w) {
  n <- length(w)
  k <- min(n, round(n * frac))
  out <- logical(n)
  if (k > 0) out[sample.int(n, k, prob = w)] <- TRUE
  out
}

syn_levels <- function(codebook, name) {
  strsplit(codebook$allowed_values[codebook$name == name], "|", fixed = TRUE)[[1]]
}

syn_comorbidity_levels <- c(
  hypertension = "Hypertension", diabetes = "Diabetes",
  coronary_artery_disease = "Coronary artery disease",
  chronic_kidney_disease = "Chronic kidney disease",
  dialysis_dependent_ckd = "Dialysis dependent CKD",
  congestive_heart_failure = "Congestive heart failure",
  peripheral_vascular_disease = "Peripheral vascular disease", stroke = "Stroke",
  copd = "Chronic obstructive pulmonary disease", cirrhosis = "Cirrhosis", cancer = "Cancer",
  immunosuppression = "Immunosuppression", psychiatry = "Psychiatry",
  alcohol_opioid_addiction = "Alcohol or opioid addiction"
)

syn_comorbidity_base <- c(
  hypertension = 0.18, diabetes = 0.10, coronary_artery_disease = 0.05,
  chronic_kidney_disease = 0.03, congestive_heart_failure = 0.03,
  peripheral_vascular_disease = 0.02, stroke = 0.02, copd = 0.05, cirrhosis = 0.015,
  cancer = 0.03, immunosuppression = 0.02, psychiatry = 0.08, alcohol_opioid_addiction = 0.08
)

syn_cohort <- function(n, codebook) {
  male <- stats::runif(n) < 0.7
  age <- syn_clip(round(stats::rnorm(n, 44, 17)), 18, 90)
  weight <- round(syn_clip(ifelse(male, stats::rnorm(n, 82, 14), stats::rnorm(n, 68, 13)),
                           45, 140), 1)
  height <- round(syn_clip(ifelse(male, stats::rnorm(n, 176, 7), stats::rnorm(n, 163, 6)),
                           150, 200))
  tbsa <- round(syn_clip(15 + 70 * stats::rbeta(n, 1.6, 4.5), 15, 85), 1)

  age_factor <- syn_clip(age / 45, 0.4, 2)
  flags <- lapply(syn_comorbidity_base, function(p) stats::runif(n) < p * age_factor)
  flags$dialysis_dependent_ckd <- flags$chronic_kidney_disease & stats::runif(n) < 0.15
  flags <- flags[names(syn_comorbidity_levels)]
  comorb_levels <- syn_levels(codebook, "comorbidities_list")
  unknown <- setdiff(syn_comorbidity_levels, comorb_levels)
  if (length(unknown) > 0) {
    stop("Codebook comorbidity levels do not match the generator: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  ordered_names <- names(syn_comorbidity_levels)[order(match(syn_comorbidity_levels,
                                                             comorb_levels))]
  comorb_list <- vapply(seq_len(n), function(i) {
    has <- vapply(ordered_names, function(f) flags[[f]][i], logical(1))
    paste(syn_comorbidity_levels[ordered_names][has], collapse = ", ")
  }, character(1))
  comorb_list[comorb_list == ""] <- NA_character_

  sub_levels <- syn_levels(codebook, "burn_sub_mechanism")
  sub_weights <- c(Flame = 55, Scald = 20, "High voltage electrical" = 7,
                   "Low voltage electrical" = 4, Chemical = 5, Contact = 7, Sauna = 2)
  w <- unname(sub_weights[sub_levels])
  w[is.na(w)] <- 1
  sub_mech <- sub_levels[sample.int(length(sub_levels), n, replace = TRUE, prob = w)]
  mechanism <- ifelse(grepl("electrical", sub_mech), "Electrical",
                      ifelse(sub_mech == "Chemical", "Chemical", "Thermal"))

  inhal <- stats::runif(n) < ifelse(sub_mech == "Flame", 0.28, 0.06)
  full_thick <- stats::runif(n) < 0.6

  c(list(
    male = male, age = age, weight = weight, height = height, tbsa = tbsa,
    comorb_list = comorb_list, sub_mech = sub_mech, mechanism = mechanism, inhal = inhal,
    full_thick = full_thick,
    full_thick_pct = round(tbsa * stats::runif(n, 0.1, 0.6), 1),
    inhal3 = inhal & stats::runif(n) < 0.15,
    cyanokit = inhal & stats::runif(n) < 0.15,
    cardiac_arrest = stats::runif(n) < 0.02,
    trauma = stats::runif(n) < 0.08,
    antihtn = (flags$hypertension | flags$congestive_heart_failure |
                 flags$coronary_artery_disease) & stats::runif(n) < 0.8
  ), flags)
}

syn_timing <- function(n) {
  first_day <- as.numeric(as.Date("2015-01-01"))
  last_day <- as.numeric(as.Date("2023-12-31"))
  burn_day <- sample(first_day:last_day, n, replace = TRUE)
  burn_min <- sample(0:1439, n, replace = TRUE)
  delay_min <- sample(60:1440, n, replace = TRUE)
  burn_stamp <- burn_day * 1440 + burn_min
  icu_stamp <- burn_stamp + delay_min
  list(burn_day = burn_day, burn_min = burn_min, burn_stamp = burn_stamp,
       delay_h = delay_min / 60, admit_day = icu_stamp %/% 1440,
       admit_min = icu_stamp %% 1440)
}

# Blood pressure triplet with SAP > MAP > DAP and MAP from the standard formula.
syn_bp <- function(mean_sap, sd = 12) {
  n <- length(mean_sap)
  sap <- round(syn_clip(stats::rnorm(n, mean_sap, sd), 80, 190))
  pulse_pressure <- round(syn_clip(stats::rnorm(n, 45, 8), 28, 80))
  dap <- sap - pulse_pressure
  list(sap = sap, dap = dap, map = round((sap + 2 * dap) / 3))
}

syn_hemodynamics <- function(n, ne_any, picco) {
  hr <- function(mean) round(syn_clip(stats::rnorm(n, mean, 14), 55, 150))
  ci <- function() round(syn_clip(stats::rnorm(n, 3.4, 0.5), 2, 5), 1)
  cvp <- function() round(syn_clip(stats::rnorm(n, 8, 2.5), 2, 18))
  adm <- syn_bp(ifelse(ne_any, 112, 122))
  pre <- syn_bp(rep(98, n), 8)
  h24 <- syn_bp(ifelse(ne_any, 108, 120))
  h48 <- syn_bp(ifelse(ne_any, 110, 120))
  hr_adm <- hr(98)
  hr_pre <- syn_mask(hr(112), ne_any)
  hr_24 <- hr(100)
  hr_48 <- hr(94)
  map_pre <- syn_mask(pre$map, ne_any)
  low_24 <- pmin(adm$map, map_pre, h24$map, na.rm = TRUE) - round(stats::runif(n, 0, 8))
  low_48 <- h48$map - round(stats::runif(n, 0, 8))
  high_24 <- pmax(hr_adm, hr_pre, hr_24, na.rm = TRUE) + round(stats::runif(n, 0, 25))
  high_48 <- hr_48 + round(stats::runif(n, 0, 25))
  list(
    sap_admission_mmhg = adm$sap, map_admission_mmhg = adm$map, dap_admission_mmhg = adm$dap,
    hr_admission_bpm = hr_adm,
    ci_admission_l_min_m2 = syn_mask(ci(), picco), cvp_admission_mmhg = syn_mask(cvp(), picco),
    sap_before_ne_mmhg = syn_mask(pre$sap, ne_any), map_before_ne_mmhg = map_pre,
    dap_before_ne_mmhg = syn_mask(pre$dap, ne_any), hr_before_ne_bpm = hr_pre,
    ci_before_ne_l_min_m2 = syn_mask(ci(), picco & ne_any),
    cvp_before_ne_mmhg = syn_mask(cvp(), picco & ne_any),
    sap_24h_mmhg = h24$sap, map_24h_mmhg = h24$map, dap_24h_mmhg = h24$dap, hr_24h_bpm = hr_24,
    ci_24h_l_min_m2 = syn_mask(ci(), picco), cvp_24h_mmhg = syn_mask(cvp(), picco),
    sap_48h_mmhg = h48$sap, map_48h_mmhg = h48$map, dap_48h_mmhg = h48$dap, hr_48h_bpm = hr_48,
    ci_48h_l_min_m2 = syn_mask(ci(), picco),
    lowest_map_24h_mmhg = low_24, lowest_map_24_48h_mmhg = low_48,
    highest_pulse_24h_bpm = high_24, highest_pulse_24_48h_bpm = high_48,
    cvp_48h_mmhg = syn_mask(cvp(), picco)
  )
}

# Fluids are cumulative from the burn. Every fluid is generated at 24h and scaled up for 48h
# and 72h, and pre-ICU is a fraction of the 24h value, so the order pre-ICU <= 24h <= 48h <= 72h
# holds for each fluid and for the sums.
syn_fluids <- function(weight, tbsa, delay_h) {
  n <- length(weight)
  ratio <- syn_clip(stats::rlnorm(n, log(3.5), 0.22), 2.3, 5.7)
  crystalloid <- ratio * weight * tbsa
  s_ra <- stats::runif(n, 0.4, 0.95)
  s_nacl <- (1 - s_ra) * stats::runif(n)
  base24 <- list(
    ra = crystalloid * s_ra, nacl = crystalloid * s_nacl,
    plasmalyte = crystalloid * (1 - s_ra - s_nacl),
    glucose5 = ifelse(stats::runif(n) < 0.5, stats::runif(n, 250, 1500), 0),
    glucose10 = ifelse(stats::runif(n) < 0.1, stats::runif(n, 200, 1000), 0),
    glucose20 = ifelse(stats::runif(n) < 0.05, stats::runif(n, 100, 500), 0)
  )
  pre_share <- 0.04 + 0.5 * (delay_h / 24) * stats::runif(n, 0.3, 1)
  g48 <- stats::runif(n, 1.3, 1.9)
  g72 <- stats::runif(n, 1.15, 1.5)
  fluids <- c("ra", "nacl", "plasmalyte", "glucose5", "glucose10", "glucose20")
  at <- lapply(base24, function(x) {
    pre_factor <- pre_share * stats::runif(n, 0.8, 1)
    list(pre = syn_r10(x * pre_factor), h24 = syn_r10(x), h48 = syn_r10(x * g48),
         h72 = syn_r10(x * g48 * g72))
  })
  out <- list()
  windows <- c(pre = "pre_icu", h24 = "24h", h48 = "48h", h72 = "72h")
  for (key in names(windows)) {
    suffix <- windows[[key]]
    for (f in fluids) {
      out[[paste0(f, "_", suffix, "_ml")]] <- at[[f]][[key]]
    }
    cryst <- at$ra[[key]] + at$nacl[[key]] + at$plasmalyte[[key]]
    out[[paste0("crystalloids_", suffix, "_ml")]] <- cryst
    out[[paste0("total_fluids_", suffix, "_ml")]] <- cryst + at$glucose5[[key]] +
      at$glucose10[[key]] + at$glucose20[[key]]
  }
  out$glucose_unknown_pre_icu_ml <- rep(0, n)
  out
}

# Albumin volumes are 0 when none was given. The delay is hours from the burn to the first dose,
# so the pre-ICU, 24h and 48h flags follow from it.
syn_albumin <- function(n, delay_h) {
  use <- stats::runif(n) < 0.4
  delay <- round(stats::runif(n, 2, 46), 1)
  active24 <- use & delay < 24
  t20 <- stats::runif(n) < 0.5
  t5 <- !t20 | stats::runif(n) < 0.2
  base20 <- ifelse(use & t20, 50 * (sample.int(9, n, replace = TRUE) + 1), 0)
  base5 <- ifelse(use & t5, 250 * sample.int(4, n, replace = TRUE), 0)
  at24 <- function(base) {
    ifelse(active24 & base > 0, ceiling(base * stats::runif(n, 0.4, 1) / 50) * 50, 0)
  }
  a20_24 <- at24(base20)
  a5_24 <- at24(base5)
  list(
    albumin_pre_icu = syn_yn(use & delay < delay_h), albumin_24h = syn_yn(active24),
    albumin_48h = syn_yn(use), albumin_any_24h_ml = a20_24 + a5_24,
    albumin20_24h_ml = a20_24, albumin5_24h_ml = a5_24, albumin20_48h_ml = base20,
    albumin5_48h_ml = base5, albumin_delay_h = syn_mask(delay, use)
  )
}

syn_blood <- function(n, delay_h, p, max_units) {
  use <- stats::runif(n) < p
  delay <- round(stats::runif(n, 1, 47), 1)
  units <- ifelse(use, sample.int(max_units, n, replace = TRUE, prob = max_units:1), 0L)
  list(pre = syn_yn(use & delay < delay_h), units = as.integer(units),
       delay = syn_mask(delay, use))
}

# Norepinephrine (NE) pattern per patient: started before 24h (delay < 24) or only in 24-48h.
syn_vasopressors <- function(n, ne_any, delay_h, other_levels) {
  pattern <- sample(c("both", "first", "second"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  ne24 <- ne_any & pattern != "second"
  ne2448 <- ne_any & pattern != "first"
  delay <- ifelse(pattern == "second", round(stats::runif(n, 24.1, 47.5), 1),
                  round(stats::runif(n, 1, 23.5), 1))
  dose <- function() round(syn_clip(stats::rlnorm(n, log(0.12), 0.7), 0.01, 1.5), 2)
  peak24 <- syn_mask(dose(), ne24)
  peak2448 <- syn_mask(dose(), ne2448)
  hours1 <- ifelse(ne24, stats::runif(n, 0.25, 1) * (24 - delay), 0)
  hours2 <- ifelse(ne2448, stats::runif(n, 0.3, 1) * (24 - pmax(delay - 24, 0)), 0)
  total <- round(stats::runif(n, 0.3, 0.7) * ifelse(ne24, peak24, 0) * 60 * hours1 +
                   stats::runif(n, 0.3, 0.7) * ifelse(ne2448, peak2448, 0) * 60 * hours2, 2)
  other <- ne_any & stats::runif(n) < 0.35
  drug <- other_levels[sample.int(length(other_levels), n, replace = TRUE)]
  other24 <- other & ne24
  other2448 <- other & ne2448
  list(
    ne_24h = ne24, ne_24_48h = ne2448, ne_delay_h = syn_mask(delay, ne_any),
    peak24 = peak24, peak2448 = peak2448, total = syn_mask(total, ne_any),
    hours_on = hours1 + hours2, ne_pre = ne_any & delay < delay_h,
    other_24h = syn_mask(drug, other24), other_24_48h = syn_mask(drug, other2448),
    peak_other_24h = syn_mask(dose(), other24), peak_other_24_48h = syn_mask(dose(), other2448)
  )
}

# Abbreviated Burn Severity Index (ABSI) from the published point table.
syn_absi <- function(co) {
  age_points <- findInterval(co$age, c(21, 41, 61, 81)) + 1
  tbsa_points <- findInterval(co$tbsa, seq(10, 90, by = 10), left.open = TRUE) + 1
  as.integer(age_points + tbsa_points + !co$male + co$inhal + co$full_thick)
}

syn_charlson <- function(co) {
  as.integer(findInterval(co$age, c(50, 60, 70, 80)) + co$coronary_artery_disease +
               co$congestive_heart_failure + co$peripheral_vascular_disease + co$stroke +
               co$copd + co$diabetes + 2 * co$chronic_kidney_disease + 3 * co$cirrhosis +
               2 * co$cancer)
}

# Outcomes, lengths of stay and every event date. All dates are whole-day numbers until the
# end; events fall between the ICU admission date (never before the burn date) and the ICU
# discharge date. Lengths and durations are plain date differences.
syn_course <- function(n, co, tm) {
  baux <- co$age + co$tbsa
  hosp_dead <- syn_pick(0.22, stats::plogis((baux - 95) / 12 + 0.7 * co$inhal))
  icu_dead <- hosp_dead & stats::runif(n) < 0.85
  rrt <- syn_pick(0.12, exp(0.04 * co$tbsa + 0.02 * co$age + hosp_dead))
  aki3 <- rrt | stats::runif(n) < 0.05
  aki1 <- aki3 | stats::runif(n) < 0.3

  icu_los <- ifelse(icu_dead, pmax(1, ceiling(stats::rexp(n, 1 / 7))),
                    pmax(2, round(stats::rlnorm(n, log(0.55 * co$tbsa), 0.45))))
  icu_los <- pmin(ifelse(rrt, pmax(icu_los, 3), icu_los), 150)
  extra <- ifelse(hosp_dead, sample.int(15, n, replace = TRUE),
                  1 + round(stats::rexp(n, 1 / 12)))
  hosp_los <- ifelse(icu_dead, icu_los, icu_los + extra)

  burn <- tm$burn_day
  admit <- tm$admit_day
  discharge <- admit + icu_los
  hosp_discharge <- admit + hosp_los
  follow_end <- hosp_discharge + ifelse(hosp_dead, 0, sample(30:365, n, replace = TRUE))
  ev <- function(lo = admit, hi = discharge) syn_rand_day(lo, hi)

  any_surg <- stats::runif(n) < 0.9
  first_days <- pmin(discharge - burn, round(0.5 + stats::rexp(n, 1 / 3.5), 1))
  first_surg_day <- floor(tm$burn_stamp / 1440 + first_days)
  graft7 <- any_surg & first_days <= 7 & stats::runif(n) < 0.85
  graft48 <- graft7 & first_days <= 2 & stats::runif(n) < 0.5
  total_surg <- ifelse(any_surg, pmin(8, 1 + stats::rpois(n, 2)), 0)
  graft7_n <- ifelse(graft7, pmin(total_surg, sample.int(3, n, replace = TRUE)), 0)
  has_graft <- graft7 | (any_surg & stats::runif(n) < 0.3)

  rrt_start <- syn_rand_day(pmax(admit, burn + 1), discharge)
  rrt_stop <- pmin(discharge, rrt_start + 1 + stats::rpois(n, 5))
  aki3_day <- ifelse(rrt, pmax(admit, rrt_start - sample(0:1, n, replace = TRUE)), ev())
  aki1_day <- syn_rand_day(admit, ifelse(aki3, aki3_day, discharge))

  mv <- syn_pick(0.6, exp(0.03 * co$tbsa + 1.2 * co$inhal))
  mv_adm <- mv & stats::runif(n) < 0.25
  mv_24 <- mv & (mv_adm | discharge == admit | stats::runif(n) < 0.5)
  intubation <- ifelse(mv_adm, burn, ifelse(mv_24, admit, syn_rand_day(admit + 1, discharge)))
  vent_stop <- pmin(discharge, intubation + round(stats::rexp(n, 1 / 6)))

  sepsis <- syn_pick(0.27, exp(0.03 * co$tbsa + 0.05 * icu_los))
  bacteremia <- sepsis & stats::runif(n) < 0.4
  candidemia <- sepsis & stats::runif(n) < 0.12
  ards <- stats::runif(n) < 0.12 + 0.25 * co$inhal
  mesenteric <- stats::runif(n) < 0.03
  acs <- stats::runif(n) < 0.06

  gap <- hosp_los - icu_los
  readmit <- !hosp_dead & gap >= 2 & stats::runif(n) < 0.06

  list(
    hosp_dead = hosp_dead, icu_dead = icu_dead, rrt = rrt, aki3 = aki3, aki1 = aki1,
    icu_los = icu_los, hosp_los = hosp_los, discharge = discharge, admit = admit,
    first_days = first_days, any_surg = any_surg, graft7 = graft7, graft48 = graft48,
    total_surg = total_surg, graft7_n = graft7_n, has_graft = has_graft,
    last_graft = ev(first_surg_day, discharge), rrt_start = rrt_start, rrt_stop = rrt_stop,
    aki3_day = aki3_day, aki1_day = aki1_day, mv = mv, mv_adm = mv_adm, mv_24 = mv_24,
    intubation = intubation, vent_stop = vent_stop, sepsis = sepsis,
    septic_n = pmin(4, 1 + stats::rpois(n, 0.7)), sepsis_day = ev(pmin(admit + 2, discharge)),
    bacteremia = bacteremia, bacteremia_day = ev(), candidemia = candidemia,
    candidemia_day = ev(), ards = ards, ards_day = ev(), mesenteric = mesenteric,
    mesenteric_day = ev(), acs = acs, acs_day = ev(),
    rhabdo = stats::runif(n) < 0.10, lv_dysfunction = stats::runif(n) < 0.08,
    cor_pulmonale = stats::runif(n) < 0.05,
    follow_end = follow_end, readmit = readmit,
    readmit_day = syn_rand_day(discharge + 1, hosp_discharge)
  )
}

syn_labs_scores <- function(n, co, cs, ne_any) {
  hb <- round(syn_clip(ifelse(co$male, stats::rnorm(n, 14.8, 1.8), stats::rnorm(n, 13, 1.6)),
                       8, 20), 1)
  creat <- ifelse(cs$aki3, stats::rlnorm(n, log(300), 0.4),
                  ifelse(cs$aki1, stats::rlnorm(n, log(140), 0.3), stats::rlnorm(n, log(75), 0.25)))
  creat <- round(syn_clip(creat, 40, 600))
  diuresis <- ifelse(cs$aki3, stats::rnorm(n, 0.5, 0.25),
                     ifelse(cs$aki1, stats::rnorm(n, 0.9, 0.3), stats::rnorm(n, 1.3, 0.4)))
  gcs <- ifelse(stats::runif(n) < 0.85, 15, sample(3:14, n, replace = TRUE, prob = (3:14)^2))
  shock <- as.numeric(ne_any)
  list(
    lactate = round(syn_clip(stats::rlnorm(n, log(2) + 0.012 * (co$tbsa - 30) + 0.4 * shock,
                                           0.5), 0.5, 15), 1),
    hb = hb, hct = round(syn_clip(3 * hb + stats::rnorm(n, 0, 1.5), 25, 60), 1),
    creat = creat, diuresis = round(syn_clip(diuresis, 0.1, 3), 2), gcs = as.integer(gcs),
    saps2 = as.integer(syn_clip(round(15 + 0.3 * co$age + 0.3 * co$tbsa + 5 * shock +
                                        stats::rnorm(n, 0, 6)), 0, 163)),
    saps3 = as.integer(syn_clip(round(25 + 0.35 * co$age + 0.35 * co$tbsa + 6 * shock +
                                        stats::rnorm(n, 0, 6)), 0, 217)),
    sofa = as.integer(syn_clip(round(1 + 0.04 * co$tbsa + 2 * shock + co$inhal +
                                       stats::rnorm(n, 0, 1)), 0, 24))
  )
}

syn_build <- function(n, codebook) {
  co <- syn_cohort(n, codebook)
  tm <- syn_timing(n)
  ne_any <- syn_pick(0.5, exp(0.03 * co$tbsa + 0.01 * co$age + 0.5 * co$inhal))
  picco <- stats::runif(n) < 0.3
  fl <- syn_fluids(co$weight, co$tbsa, tm$delay_h)
  alb <- syn_albumin(n, tm$delay_h)
  rbc <- syn_blood(n, tm$delay_h, 0.2, 8)
  ffp <- syn_blood(n, tm$delay_h, 0.15, 6)
  other_levels <- syn_levels(codebook, "other_vp_24h")
  vp <- syn_vasopressors(n, ne_any, tm$delay_h, other_levels)
  hemo <- syn_hemodynamics(n, ne_any, picco)
  cs <- syn_course(n, co, tm)
  ls <- syn_labs_scores(n, co, cs, ne_any)
  venofundin <- stats::runif(n) < 0.08
  tbsa_pre <- round(syn_clip(co$tbsa * stats::runif(n, 0.8, 1.25), 5, 95), 1)
  covid <- ifelse(stats::runif(n) < 0.05, "Positive", "Negative")
  days_on_vp <- round(pmax(vp$hours_on / 24, 0.5) + stats::rexp(n, 1 / 2.5), 1)
  days_on_vp <- syn_mask(pmin(days_on_vp, cs$icu_los + 1), ne_any)
  day_date <- function(day, flag) syn_as_date(syn_mask(day, flag))
  count <- function(x, flag) as.integer(syn_mask(x, flag))

  d <- list(
    patient_id = sprintf("SYN-%04d", seq_len(n)), age_years = co$age,
    gender = ifelse(co$male, "Male", "Female"), comorbidities_list = co$comorb_list,
    comorbidity_other = rep(FALSE, n), preadmission_antihypertensives = co$antihtn,
    admission_weight_kg = co$weight, height_cm = co$height,
    bmi_kg_m2 = round(co$weight / (co$height / 100)^2, 2),
    tbsa_prehospital_pct = tbsa_pre,
    predicted_fluid_24h_prehospital_ml = 4 * co$weight * tbsa_pre,
    tbsa_burned_pct = co$tbsa, full_thickness = co$full_thick,
    full_thickness_bsa_pct = syn_mask(co$full_thick_pct, co$full_thick),
    inhalation_injury = co$inhal, inhalation_injury_grade3 = co$inhal3,
    burn_mechanism = co$mechanism, burn_sub_mechanism = co$sub_mech,
    prehospital_cardiac_arrest = co$cardiac_arrest, associated_trauma = co$trauma,
    initial_gcs = ls$gcs, covid19_status = covid,
    peak_lactate_0_24h_mmol_l = ls$lactate, peak_hemoglobin_0_24h_g_dl = ls$hb,
    hematocrit_0_24h_pct = ls$hct, peak_creatinine_0_72h_umol_l = ls$creat,
    diuresis_0_48h_ml_kg_h = ls$diuresis,
    icu_admission_date = syn_as_date(tm$admit_day), icu_admission_time = syn_clock(tm$admit_min),
    burn_date = syn_as_date(tm$burn_day), burn_time = syn_clock(tm$burn_min),
    albumin_delay_h = alb$albumin_delay_h,
    venofundin_48h = venofundin,
    venofundin_48h_ml = ifelse(venofundin, 250 * sample.int(4, n, replace = TRUE), 0),
    rbc_pre_icu = rbc$pre, rbc_48h_units = rbc$units, rbc_delay_h = rbc$delay,
    ffp_pre_icu = ffp$pre, ffp_48h_units = ffp$units, ffp_delay_h = ffp$delay,
    ne_pre_burn_icu = vp$ne_pre, cont_vasopressor_pre_burn_icu = vp$ne_pre,
    ne_24h = vp$ne_24h, ne_48h = vp$ne_24h | vp$ne_24_48h, ne_24_48h = vp$ne_24_48h,
    ne_delay_h = vp$ne_delay_h, peak_ne_24h_ug_kg_min = vp$peak24,
    peak_ne_24_48h_ug_kg_min = vp$peak2448, total_ne_48h_ug_kg = vp$total,
    other_vp_24h = vp$other_24h, other_vp_24_48h = vp$other_24_48h,
    peak_other_vp_24h_ug_kg_min = vp$peak_other_24h,
    peak_other_vp_24_48h_ug_kg_min = vp$peak_other_24_48h, days_on_vp = days_on_vp,
    mech_vent_admission = cs$mv_adm, cyanokit = co$cyanokit,
    escharotomy_fasciotomy_48h = stats::runif(n) < 0.1 + 0.3 * (co$tbsa > 40),
    enzymatic_escharotomy_48h = stats::runif(n) < 0.08,
    graft_surgery_48h = cs$graft48, graft_surgery_7d = cs$graft7,
    graft_surgeries_7d_n = as.integer(cs$graft7_n), surgeries_total_n = as.integer(cs$total_surg),
    first_surgery_days = syn_mask(cs$first_days, cs$any_surg), picco_use_0_48h = picco,
    nephrotoxic_drugs_day1_7 = stats::runif(n) < 0.25,
    saps2_admission = ls$saps2, saps3_admission = ls$saps3,
    absi_admission = syn_absi(co), charlson_index = syn_charlson(co), sofa_admission = ls$sofa,
    baux_admission = co$age + co$tbsa, lv_systolic_dysfunction_48h = cs$lv_dysfunction,
    acute_cor_pulmonale = cs$cor_pulmonale,
    aki_kdigo_1plus = cs$aki1, aki_kdigo_1plus_date = day_date(cs$aki1_day, cs$aki1),
    aki_kdigo_3 = cs$aki3, aki_kdigo_3_date = day_date(cs$aki3_day, cs$aki3),
    crrt = cs$rrt, crrt_days = syn_mask(cs$rrt_stop - cs$rrt_start, cs$rrt),
    ards = cs$ards, ards_date = day_date(cs$ards_day, cs$ards),
    mesenteric_ischemia = cs$mesenteric,
    mesenteric_ischemia_date = day_date(cs$mesenteric_day, cs$mesenteric),
    abdominal_compartment_syndrome = cs$acs, rhabdomyolysis = cs$rhabdo,
    acs_date = day_date(cs$acs_day, cs$acs),
    sepsis = cs$sepsis, septic_events_n = count(cs$septic_n, cs$sepsis),
    sepsis_dates = day_date(cs$sepsis_day, cs$sepsis),
    bacteremia = cs$bacteremia,
    bacteremia_dates = day_date(cs$bacteremia_day, cs$bacteremia),
    candidemia = cs$candidemia, candidemia_dates = day_date(cs$candidemia_day, cs$candidemia),
    rrt = cs$rrt,
    rrt_start_days = syn_mask(round((cs$rrt_start * 1440 - tm$burn_stamp) / 1440, 2), cs$rrt),
    rrt_start_date = day_date(cs$rrt_start, cs$rrt), rrt_stop_date = day_date(cs$rrt_stop, cs$rrt),
    mech_vent = cs$mv, mech_vent_24h = cs$mv_24,
    mech_vent_days = syn_mask(cs$vent_stop - cs$intubation, cs$mv),
    intubation_date = day_date(cs$intubation, cs$mv),
    vent_stop_date = day_date(cs$vent_stop, cs$mv),
    last_graft_surgery_date = day_date(cs$last_graft, cs$has_graft),
    hospital_mortality = cs$hosp_dead, icu_mortality = cs$icu_dead,
    outcome_icu_admission_date = syn_as_date(tm$admit_day),
    icu_discharge_date = syn_as_date(cs$discharge),
    hospital_los_days = cs$hosp_los, icu_los_days = cs$icu_los,
    follow_up_end_date = syn_as_date(cs$follow_end),
    vital_status_follow_up = ifelse(cs$hosp_dead, "Dead", "Alive"),
    icu_readmission = cs$readmit, icu_readmission_date = day_date(cs$readmit_day, cs$readmit)
  )
  c(d, fl, alb[setdiff(names(alb), "albumin_delay_h")], hemo, co[names(syn_comorbidity_levels)])
}

# Returns n synthetic patients in the NBC template layout (one column per codebook row, in
# position order). The caller's random number state is left unchanged.
generate_synthetic_nbc <- function(codebook, n = 100, seed = 1) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n != round(n)) {
    stop("'n' must be a single positive whole number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("'seed' must be a single number.", call. = FALSE)
  }
  codebook <- codebook[order(codebook$position), , drop = FALSE]
  built <- withr::with_seed(
    seed, syn_build(n, codebook),
    .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection"
  )
  absent <- setdiff(codebook$name, names(built))
  extra <- setdiff(names(built), codebook$name)
  if (length(absent) > 0 || length(extra) > 0) {
    stop("Generator and codebook disagree. Not generated: ", paste(absent, collapse = ", "),
         ". Not in codebook: ", paste(extra, collapse = ", "), call. = FALSE)
  }
  out <- lapply(seq_len(nrow(codebook)), function(i) {
    x <- built[[codebook$name[i]]]
    switch(codebook$type[i],
      numeric = as.numeric(x),
      integer = as.integer(x),
      binary = if (is.logical(x)) syn_yn(x) else x,
      date = x,
      as.character(x)
    )
  })
  names(out) <- codebook$name
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

# Converts a column to what Excel stores: dates as day serials, times as day fractions. A value
# that cannot be converted (for example text injected into a numeric or date column) is written
# as text, which turns the whole column into text cells; the loader reports such values as
# parse problems.
xlsx_cells <- function(x, type) {
  excel_origin_offset <- 25569
  if (type == "date" && !is.character(x)) {
    return(as.numeric(x) + excel_origin_offset)
  }
  if (type == "time") {
    clock <- !is.na(x) & grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", x)
    fraction <- rep(NA_real_, length(x))
    parts <- strsplit(x[clock], ":", fixed = TRUE)
    fraction[clock] <- vapply(parts, function(p) (as.numeric(p[1]) * 60 + as.numeric(p[2])) / 1440,
                              numeric(1))
    if (all(clock | is.na(x))) return(fraction)
    out <- format(fraction, digits = 15, trim = TRUE)
    out[!clock] <- x[!clock]
    out[is.na(x)] <- NA_character_
    return(out)
  }
  x
}

# Writes data in the template layout: section names in row 1 (first column of each section
# only), source headers in row 2, data from row 3. Writes only to `path`.
write_nbc_xlsx <- function(data, codebook, path) {
  codebook <- codebook[order(codebook$position), , drop = FALSE]
  if (!identical(names(data), codebook$name)) {
    stop("Data columns must match codebook names in position order.", call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Ark1")
  n_cols <- nrow(codebook)
  first_of_section <- c(TRUE, codebook$section[-1] != codebook$section[-n_cols])
  for (i in which(first_of_section)) {
    openxlsx::writeData(wb, "Ark1", codebook$section[i], startCol = i, startRow = 1,
                        colNames = FALSE)
  }
  openxlsx::writeData(wb, "Ark1", as.data.frame(t(codebook$source_name)), startRow = 2,
                      colNames = FALSE)
  for (i in seq_len(nrow(codebook))) {
    x <- xlsx_cells(data[[i]], codebook$type[i])
    openxlsx::writeData(wb, "Ark1", x, startCol = i, startRow = 3, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
