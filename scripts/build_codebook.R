# =============================================================================
# One-off builder for the curated codebook (metadata/codebook.csv)
# =============================================================================
#
# Reads the two header rows of the NBC template and the data dictionary, and
# combines them with the hand-written specification below. Example patient
# values (row 3 of the template) are never read.
#
# After the first run, metadata/codebook.csv is the source of truth and is edited
# directly. Re-running this script is only needed to rebuild it from scratch.
#
# Output: output/codebook.csv inside the project folder (found by PROJECT_VERSION).
# Copy it to metadata/codebook.csv by hand; this script writes nowhere else.

library(readxl)

src_dir <- "/home/kobotnar/data/src/burn_vp"

find_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) != 1) stop("Cannot locate this script, so the project folder is unknown.")
  dir <- normalizePath(dirname(sub("^--file=", "", file_arg)), mustWork = TRUE)
  while (!file.exists(file.path(dir, "PROJECT_VERSION"))) {
    parent <- dirname(dir)
    if (parent == dir) stop("PROJECT_VERSION not found; refusing to write.")
    dir <- parent
  }
  dir
}

# One line per template column, in template order:
# name | type | dictionary prefix | allowed values (";" separated) | notes
# Types: n numeric, i integer, b binary, c categorical, d date, t time, x text.
fluid_note <- "It is unclear whether 48h fluid columns are cumulative 0-48h or 24-48h."
# nolint start: line_length_linter
spec_text <- paste0("
patient_id|x|.|.|.
age_years|n|Age|.|.
gender|c|Gender|Male;Female;Others|.
comorbidities_list|c|Co-morbidities|Hypertension;Diabetes;Coronary artery disease;Chronic kidney disease;Dialysis dependent CKD;Congestive heart failure;Peripheral vascular disease;Stroke;Chronic obstructive pulmonary disease;Cirrhosis;Cancer;Immunosuppression;Psychiatry;Alcohol or opioid addiction|Comma separated list, so a cell can hold several levels; QC must split it. Duplicates the comorbidity flag columns, so consistency should be checked.
hypertension|b|Co-morbidities|.|.
diabetes|b|Co-morbidities|.|.
coronary_artery_disease|b|Co-morbidities|.|.
chronic_kidney_disease|b|Co-morbidities|.|.
dialysis_dependent_ckd|b|Co-morbidities|.|Header reads Dialysis (dependent CKD before ICU admission); dictionary level is Dialysis dependent CKD.
congestive_heart_failure|b|Co-morbidities|.|.
peripheral_vascular_disease|b|Co-morbidities|.|.
stroke|b|Co-morbidities|.|.
copd|b|Co-morbidities|.|.
cirrhosis|b|Co-morbidities|.|.
cancer|b|Co-morbidities|.|.
immunosuppression|b|Co-morbidities|.|.
psychiatry|b|Co-morbidities|.|.
alcohol_opioid_addiction|b|Co-morbidities|.|.
comorbidity_other|b|.|.|Free-text comment is requested in the header, but only a Yes/No column exists.
preadmission_antihypertensives|b|.|.|.
admission_weight_kg|n|Admission weight|.|.
height_cm|n|Height|.|.
bmi_kg_m2|n|.|.|Derived from weight and height; QC can recompute it.
tbsa_prehospital_pct|n|.|.|.
predicted_fluid_24h_prehospital_ml|n|.|.|.
tbsa_burned_pct|n|% Total body|.|.
full_thickness|b|Full thickness burn|.|.
full_thickness_bsa_pct|n|.|.|.
inhalation_injury|b|Suspected inhalation|.|Dictionary says suspected inhalation injury.
inhalation_injury_grade3|b|.|.|.
burn_mechanism|x|Burn mechanism|.|The template splits mechanism into Mechanism of burn injury and Sub-mechanism, while the dictionary has one list. Levels of the main mechanism are not defined anywhere, so it is text.
burn_sub_mechanism|c|Burn mechanism|Flame;Scald;High voltage electrical;Low voltage electrical;Chemical;Contact;Sauna|The template splits mechanism into Mechanism of burn injury and Sub-mechanism, while the dictionary has one list. Levels are the dictionary list, assumed to apply to the sub-mechanism.
prehospital_cardiac_arrest|b|.|.|.
associated_trauma|b|.|.|.
sap_admission_mmhg|n|.|.|.
map_admission_mmhg|n|.|.|.
dap_admission_mmhg|n|.|.|.
hr_admission_bpm|n|.|.|.
ci_admission_l_min_m2|n|.|.|.
cvp_admission_mmhg|n|.|.|.
sap_before_ne_mmhg|n|.|.|.
map_before_ne_mmhg|n|.|.|.
dap_before_ne_mmhg|n|.|.|.
hr_before_ne_bpm|n|.|.|.
ci_before_ne_l_min_m2|n|.|.|The header gives the unit mmHg for CI before NE, but it should be L/min/m2. Unit column keeps the header text.
cvp_before_ne_mmhg|n|.|.|.
sap_24h_mmhg|n|.|.|.
map_24h_mmhg|n|.|.|Header typo mmgHg; the unit is mmHg.
dap_24h_mmhg|n|.|.|Header typo: unbalanced bracket, reads DAP 24h mmHg).
hr_24h_bpm|n|.|.|.
ci_24h_l_min_m2|n|.|.|.
cvp_24h_mmhg|n|.|.|.
sap_48h_mmhg|n|.|.|.
map_48h_mmhg|n|.|.|.
dap_48h_mmhg|n|.|.|.
hr_48h_bpm|n|.|.|.
ci_48h_l_min_m2|n|.|.|.
lowest_map_24h_mmhg|n|Lowest MAP|.|Header has no unit; mmHg per the dictionary.
lowest_map_24_48h_mmhg|n|Lowest MAP|.|Header has no unit; mmHg per the dictionary.
highest_pulse_24h_bpm|n|Highest pulse|.|Header has no unit; BPM per the dictionary.
highest_pulse_24_48h_bpm|n|Highest pulse|.|Header has no unit; BPM per the dictionary.
cvp_48h_mmhg|n|.|.|Sits after the in-hospital MAP and pulse columns, out of the order of the other 48h hemodynamic columns.
initial_gcs|i|.|.|.
covid19_status|x|.|.|Levels not defined.
peak_lactate_0_24h_mmol_l|n|Highest lactate|.|.
peak_hemoglobin_0_24h_g_dl|n|Highest hemoglobin|.|.
hematocrit_0_24h_pct|n|Highest hematocrit|.|Unit is % but the template example is a fraction (0.43). Header does not say peak, the dictionary says highest.
peak_creatinine_0_72h_umol_l|n|Highest creatinine|.|.
diuresis_0_48h_ml_kg_h|n|Urine output|.|The dictionary gives diuresis as 0-24h, but the template has 0-48h.
icu_admission_date|d|.|.|Header text Date ICU admission appears twice (positions 70 and 189).
icu_admission_time|t|.|.|.
burn_date|d|Time and date of burn|.|Dictionary has one combined date and time variable; the template splits it.
burn_time|t|Time and date of burn|.|Dictionary has one combined date and time variable; the template splits it.
ra_pre_icu_ml|n|.|.|.
nacl_pre_icu_ml|n|.|.|.
plasmalyte_pre_icu_ml|n|.|.|.
glucose5_pre_icu_ml|n|.|.|.
glucose10_pre_icu_ml|n|.|.|.
glucose20_pre_icu_ml|n|.|.|.
glucose_unknown_pre_icu_ml|n|.|.|.
crystalloids_pre_icu_ml|n|Specified amounts of crystalloids|.|.
total_fluids_pre_icu_ml|n|Total amount of resuscitation|.|.
ra_24h_ml|n|.|.|.
nacl_24h_ml|n|.|.|.
plasmalyte_24h_ml|n|.|.|.
glucose5_24h_ml|n|.|.|.
glucose10_24h_ml|n|.|.|.
glucose20_24h_ml|n|.|.|.
crystalloids_24h_ml|n|Specified amounts of crystalloids|.|.
total_fluids_24h_ml|n|Total amount of resuscitation|.|.
ra_48h_ml|n|.|.|", fluid_note, "
nacl_48h_ml|n|.|.|", fluid_note, "
plasmalyte_48h_ml|n|.|.|", fluid_note, "
glucose5_48h_ml|n|.|.|", fluid_note, "
glucose10_48h_ml|n|.|.|", fluid_note, "
glucose20_48h_ml|n|.|.|", fluid_note, "
crystalloids_48h_ml|n|Specified amounts of crystalloids|.|", fluid_note, "
total_fluids_48h_ml|n|Total amount of resuscitation|.|", fluid_note, "
ra_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
nacl_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
plasmalyte_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
glucose5_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
glucose10_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
glucose20_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
crystalloids_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
total_fluids_72h_ml|n|.|.|Same cumulative versus interval question as the 48h fluid columns.
albumin_pre_icu|b|Specified amounts of albumin|.|.
albumin_24h|b|Specified amounts of albumin|.|.
albumin_48h|b|Specified amounts of albumin|.|.
albumin_any_24h_ml|n|Specified amounts of albumin|.|.
albumin20_24h_ml|n|Specified amounts of albumin|.|.
albumin5_24h_ml|n|Specified amounts of albumin|.|.
albumin20_48h_ml|n|Specified amounts of albumin|.|", fluid_note, "
albumin5_48h_ml|n|Specified amounts of albumin|.|", fluid_note, "
albumin_delay_h|n|.|.|.
venofundin_48h|b|Specified amounts of colloids|.|.
venofundin_48h_ml|n|Specified amounts of colloids|.|", fluid_note, "
rbc_pre_icu|b|.|.|.
rbc_48h_units|i|Specified amounts of blood|.|The dictionary gives RBC in mL, but the template gives a number of units.
rbc_delay_h|n|.|.|.
ffp_pre_icu|b|.|.|.
ffp_48h_units|i|Specified amounts of plasma|.|The dictionary gives FFP in mL, but the template gives a number of units.
ffp_delay_h|n|.|.|.
ne_pre_burn_icu|b|.|.|Meaning of pre burn ICU is not defined.
cont_vasopressor_pre_burn_icu|b|.|.|Meaning of pre burn ICU is not defined. Header does not follow the style of the others.
ne_24h|b|Types of vasopressors|.|.
ne_48h|b|Types of vasopressors|.|Overlaps with ne_24h and ne_24_48h; consistency can be checked.
ne_24_48h|b|Types of vasopressors|.|.
ne_delay_h|n|Delay of NE|.|.
peak_ne_24h_ug_kg_min|n|Maximum infusion rate|.|.
peak_ne_24_48h_ug_kg_min|n|Maximum infusion rate|.|.
total_ne_48h_ug_kg|n|.|.|Not in the dictionary.
other_vp_24h|c|Types of vasopressors|Epinephrine;Phenylephrine;Vasopressin;Dobutamine;Dopamine;Others|Levels from the dictionary. The level is spelled Phenylephrine here; the dictionary has the typo Phenylepinephrine. Template example uses other names (for example Ephedrine), so later recoding is needed. May hold several drugs.
other_vp_24_48h|c|Types of vasopressors|Epinephrine;Phenylephrine;Vasopressin;Dobutamine;Dopamine;Others|Same notes as other_vp_24h. Level spelled Phenylephrine (dictionary typo Phenylepinephrine corrected).
peak_other_vp_24h_ug_kg_min|n|Maximum infusion rate|.|Unclear which drug the rate refers to when several are given.
peak_other_vp_24_48h_ug_kg_min|n|Maximum infusion rate|.|Unclear which drug the rate refers to when several are given.
days_on_vp|n|.|.|Header has no unit; days assumed. Not in the dictionary.
mech_vent_admission|b|Intubated prior|.|Probably corresponds to the dictionary's Intubated prior to arrival.
cyanokit|b|.|.|.
escharotomy_fasciotomy_48h|b|.|.|.
enzymatic_escharotomy_48h|b|.|.|.
graft_surgery_48h|b|.|.|.
graft_surgery_7d|b|.|.|.
graft_surgeries_7d_n|i|.|.|.
surgeries_total_n|i|Total number of burn surgeries|.|.
first_surgery_days|n|Time until first burn|.|The dictionary unit is days and hours; the template gives days (fractional).
picco_use_0_48h|b|PiCCO|.|.
nephrotoxic_drugs_day1_7|b|.|.|.
saps2_admission|i|.|.|.
saps3_admission|i|.|.|.
absi_admission|i|.|.|.
charlson_index|i|.|.|.
sofa_admission|i|.|.|.
baux_admission|n|.|.|.
lv_systolic_dysfunction_48h|b|.|.|.
acute_cor_pulmonale|b|.|.|.
aki_kdigo_1plus|b|.|.|.
aki_kdigo_1plus_date|d|.|.|.
aki_kdigo_3|b|.|.|.
aki_kdigo_3_date|d|.|.|.
crrt|b|.|.|.
crrt_days|n|.|.|Header has no unit; days assumed.
ards|b|.|.|.
ards_date|d|.|.|.
mesenteric_ischemia|b|.|.|.
mesenteric_ischemia_date|d|.|.|.
abdominal_compartment_syndrome|b|.|.|.
rhabdomyolysis|b|.|.|Rhabdomyolysis has no date column, and Date ACS follows it.
acs_date|d|.|.|Date ACS follows Rhabdomyolysis, so it is unclear whether it belongs to abdominal compartment syndrome (two columns earlier) or to rhabdomyolysis.
sepsis|b|Septicemia|.|Header is Sepsis/septic shock; the dictionary says septicemia.
septic_events_n|i|.|.|.
sepsis_dates|d|.|.|Plural header; may hold several dates in one cell.
bacteremia|b|.|.|.
bacteremia_dates|d|.|.|Plural header; may hold several dates in one cell.
candidemia|b|.|.|.
candidemia_dates|d|.|.|Plural header; may hold several dates in one cell.
rrt|b|Use of renal|.|.
rrt_start_days|n|Time until initiation of renal|.|.
rrt_start_date|d|.|.|.
rrt_stop_date|d|.|.|.
mech_vent|b|.|.|.
mech_vent_24h|b|Mechanically ventilated during|.|.
mech_vent_days|n|Number of days on mechanical|.|The dictionary counts ventilation days inclusively (start and stop day both counted).
intubation_date|d|.|.|.
vent_stop_date|d|.|.|.
last_graft_surgery_date|d|.|.|.
hospital_mortality|b|Mortality during|.|.
icu_mortality|b|.|.|.
outcome_icu_admission_date|d|.|.|Header text Date ICU admission appears twice (positions 70 and 189). Name is distinct from icu_admission_date; the two should agree.
icu_discharge_date|d|.|.|.
hospital_los_days|n|Length of stay in hospital|.|The dictionary counts length of stay inclusively (admission and discharge day both counted).
icu_los_days|n|Length of stay in ICU|.|The dictionary counts length of stay inclusively (admission and discharge day both counted).
follow_up_end_date|d|.|.|.
vital_status_follow_up|x|.|.|Levels not defined.
icu_readmission|b|.|.|.
icu_readmission_date|d|.|.|.
")
# nolint end

key_names <- c(
  "age_years", "gender", "admission_weight_kg", "height_cm", "tbsa_burned_pct", "full_thickness",
  "inhalation_injury", "burn_mechanism", "burn_sub_mechanism", "hypertension", "diabetes",
  "coronary_artery_disease", "chronic_kidney_disease", "dialysis_dependent_ckd",
  "congestive_heart_failure", "peripheral_vascular_disease", "stroke", "copd", "cirrhosis",
  "cancer", "immunosuppression", "psychiatry", "alcohol_opioid_addiction", "comorbidity_other",
  "crystalloids_pre_icu_ml", "total_fluids_pre_icu_ml", "crystalloids_24h_ml",
  "total_fluids_24h_ml", "crystalloids_48h_ml", "total_fluids_48h_ml", "crystalloids_72h_ml",
  "total_fluids_72h_ml", "albumin_pre_icu", "albumin_24h", "albumin_48h", "albumin_any_24h_ml",
  "albumin20_24h_ml", "albumin5_24h_ml", "albumin20_48h_ml", "albumin5_48h_ml",
  "albumin_delay_h", "rbc_pre_icu", "rbc_48h_units", "rbc_delay_h", "ffp_pre_icu",
  "ffp_48h_units", "ffp_delay_h", "ne_pre_burn_icu", "cont_vasopressor_pre_burn_icu", "ne_24h",
  "ne_48h", "ne_24_48h", "ne_delay_h", "peak_ne_24h_ug_kg_min", "peak_ne_24_48h_ug_kg_min",
  "total_ne_48h_ug_kg", "other_vp_24h", "other_vp_24_48h", "peak_other_vp_24h_ug_kg_min",
  "peak_other_vp_24_48h_ug_kg_min", "days_on_vp", "lowest_map_24h_mmhg",
  "lowest_map_24_48h_mmhg", "highest_pulse_24h_bpm", "highest_pulse_24_48h_bpm",
  "peak_lactate_0_24h_mmol_l", "peak_creatinine_0_72h_umol_l", "diuresis_0_48h_ml_kg_h",
  "rrt", "rrt_start_days", "rrt_start_date", "rrt_stop_date", "crrt", "crrt_days", "sepsis",
  "septic_events_n", "sepsis_dates", "hospital_mortality", "hospital_los_days",
  "burn_date", "burn_time", "icu_admission_date", "icu_admission_time"
)

pre_burn <- c(
  "comorbidities_list", "hypertension", "diabetes", "coronary_artery_disease",
  "chronic_kidney_disease", "dialysis_dependent_ckd", "congestive_heart_failure",
  "peripheral_vascular_disease", "stroke", "copd", "cirrhosis", "cancer", "immunosuppression",
  "psychiatry", "alcohol_opioid_addiction", "comorbidity_other", "preadmission_antihypertensives"
)
time_window_overrides <- c(
  setNames(rep("pre-burn", length(pre_burn)), pre_burn),
  tbsa_prehospital_pct = "pre-ICU", predicted_fluid_24h_prehospital_ml = "pre-ICU",
  ne_pre_burn_icu = "pre-ICU", cont_vasopressor_pre_burn_icu = "pre-ICU",
  mech_vent_admission = "pre-ICU", graft_surgery_7d = "0-7d", graft_surgeries_7d_n = "0-7d",
  nephrotoxic_drugs_day1_7 = "day 1-7", lowest_map_24h_mmhg = "0-24h",
  lowest_map_24_48h_mmhg = "24-48h", highest_pulse_24h_bpm = "0-24h",
  highest_pulse_24_48h_bpm = "24-48h", peak_ne_24h_ug_kg_min = "0-24h",
  peak_ne_24_48h_ug_kg_min = "24-48h", initial_gcs = "admission", age_years = "admission",
  admission_weight_kg = "admission", height_cm = "admission"
)

unit_overrides <- c(
  lowest_map_24h_mmhg = "mmHg", lowest_map_24_48h_mmhg = "mmHg",
  highest_pulse_24h_bpm = "bpm", highest_pulse_24_48h_bpm = "bpm", days_on_vp = "days",
  crrt_days = "days", mech_vent_days = "days", map_24h_mmhg = "mmHg", dap_24h_mmhg = "mmHg",
  rbc_48h_units = "number", ffp_48h_units = "number"
)

window_from_name <- function(name) {
  rules <- c(
    "pre_icu" = "pre-ICU", "before_ne" = "pre-NE", "_24_48h" = "24-48h", "_0_24h" = "0-24h",
    "_0_48h" = "0-48h", "_0_72h" = "0-72h", "_24h" = "0-24h", "_48h" = "0-48h",
    "_72h" = "0-72h", "_admission" = "admission"
  )
  for (i in seq_along(rules)) if (grepl(names(rules)[i], name, fixed = TRUE)) return(rules[[i]])
  NA_character_
}

unit_from_header <- function(header) {
  known <- c(
    "years", "kg", "cm", "kg/m2", "%", "ml", "mmHg", "bpm", "L/min/m2", "mmol/l",
    "g/dl", "umol/l", "ml/kg/h", "ug/kg/min", "ug/kg", "h", "days"
  )
  m <- regmatches(header, regexec("\\(([^()]+)\\)$", header))[[1]]
  if (length(m) == 2 && m[2] %in% known) m[2] else NA_character_
}

build_codebook <- function() {
  tmpl <- read_excel(
    file.path(src_dir, "NBC_dataset_template.xlsx"),
    sheet = "Ark1", col_names = FALSE, col_types = "text", n_max = 2
  )
  dict <- read_excel(
    file.path(src_dir, "variables.xlsx"),
    sheet = "Sheet1", col_names = FALSE, col_types = "text"
  )
  dict_names <- trimws(dict[[1]][-(1:3)])
  dict_names <- dict_names[!is.na(dict_names)]

  lines <- trimws(strsplit(spec_text, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  fields <- strsplit(lines, "|", fixed = TRUE)
  stopifnot(all(lengths(fields) == 5), length(fields) == ncol(tmpl))
  spec <- as.data.frame(do.call(rbind, fields), stringsAsFactors = FALSE)
  names(spec) <- c("name", "type", "dict", "allowed", "notes")
  spec[spec == "."] <- NA

  section <- as.character(unlist(tmpl[1, ]))
  for (i in seq_along(section)) if (is.na(section[i]) && i > 1) section[i] <- section[i - 1]
  source_name <- trimws(as.character(unlist(tmpl[2, ])))

  type_map <- c(n = "numeric", i = "integer", b = "binary", c = "categorical", d = "date",
                t = "time", x = "text")
  dictionary_name <- vapply(spec$dict, function(p) {
    if (is.na(p)) return(NA_character_)
    hit <- dict_names[startsWith(dict_names, p)]
    if (length(hit) == 0) stop("No dictionary variable starts with: ", p)
    hit[1]
  }, character(1), USE.NAMES = FALSE)

  type <- unname(type_map[spec$type])
  allowed <- ifelse(type == "binary", "Yes|No", gsub(";", "|", spec$allowed, fixed = TRUE))
  unit <- vapply(source_name, unit_from_header, character(1), USE.NAMES = FALSE)
  has_override <- spec$name %in% names(unit_overrides)
  unit[has_override] <- unit_overrides[spec$name[has_override]]
  window <- vapply(spec$name, window_from_name, character(1), USE.NAMES = FALSE)
  has_window <- spec$name %in% names(time_window_overrides)
  window[has_window] <- time_window_overrides[spec$name[has_window]]
  window[type %in% c("date", "time")] <- NA

  stopifnot(all(key_names %in% spec$name))
  data.frame(
    position = seq_len(nrow(spec)), source_name = source_name, section = section,
    name = spec$name, type = type, unit = unit, allowed_values = allowed,
    time_window = window, dictionary_name = dictionary_name,
    key_variable = spec$name %in% key_names, notes = spec$notes,
    stringsAsFactors = FALSE
  )
}

codebook <- build_codebook()
out_dir <- file.path(find_root(), "output")
if (!dir.exists(out_dir)) dir.create(out_dir)
write.csv(codebook, file.path(out_dir, "codebook.csv"), row.names = FALSE, na = "NA")
