#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

# ==============================================================================
# 0. Paths
# ==============================================================================
path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
path_processed_base <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_folder_name <- "Outcomes_word_complications_first_nonMAC_sepsis_A40_A41_3_30_2026"
path_output <- file.path(path_processed_base, output_folder_name)
path_preop_lab_imputed <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputed/preop_labs_attributable_90d_latest.csv"
path_postop_lab_imputed <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputed/postop_labs_attributable_7d_hierarchical_imputed_latest.csv"

if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
}

WINDOW_30D_MIN <- 30 * 24 * 60
WINDOW_90D_MIN <- 90 * 24 * 60
WINDOW_7D_MIN <- 7 * 24 * 60

# ==============================================================================
# 1. Load operations and select anchor surgery
# ==============================================================================
cat("Loading operations and selecting first non-MAC anchor surgery per admission...\n")

ops_all <- fread(
  file.path(path_raw, "operations.csv"),
  select = c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate", "antype",
    "orin_time", "orout_time", "opstart_time", "anstart_time",
    "admission_time", "discharge_time", "icuin_time", "icuout_time",
    "inhosp_death_time", "allcause_death_time"
  ),
  na.strings = c("", "NA")
)

ops_all[, `:=`(
  orin_time = as.numeric(orin_time),
  orout_time = as.numeric(orout_time),
  opstart_time = as.numeric(opstart_time),
  anstart_time = as.numeric(anstart_time),
  admission_time = as.numeric(admission_time),
  discharge_time = as.numeric(discharge_time),
  icuin_time = as.numeric(icuin_time),
  icuout_time = as.numeric(icuout_time),
  inhosp_death_time = as.numeric(inhosp_death_time),
  allcause_death_time = as.numeric(allcause_death_time),
  opdate_num = suppressWarnings(as.numeric(opdate)),
  antype_clean = toupper(trimws(as.character(antype)))
)]
ops_all[, anchor_sort_time := fcoalesce(opstart_time, anstart_time, orin_time, opdate_num, admission_time)]
ops_all[, hadm_group := fifelse(is.na(hadm_id), paste0("MISSING_HADM_", op_id), as.character(hadm_id))]

anchor_ops <- ops_all[
  !is.na(subject_id) & !is.na(op_id) & !is.na(orin_time) & !is.na(orout_time) &
    !is.na(antype_clean) & antype_clean != "MAC"
][order(subject_id, hadm_group, anchor_sort_time, op_id)][
  , .SD[1], by = .(subject_id, hadm_group)
]

setorderv(anchor_ops, c("subject_id", "hadm_id", "op_id"))

anchor_index <- anchor_ops[, .(
  subject_id, hadm_id, op_id, case_id,
  admission_time, orin_time, orout_time,
  discharge_time, icuin_time, icuout_time,
  inhosp_death_time, allcause_death_time,
  postop30_start = orout_time,
  postop30_end = orout_time + WINDOW_30D_MIN,
  postop7_end = orout_time + WINDOW_7D_MIN
)]

fwrite(anchor_index, file.path(path_output, "anchor_first_nonMAC_operations.csv"))
cat(sprintf("Anchor operations kept: %d\n", nrow(anchor_index)))

# ==============================================================================
# 2. Load diagnosis / meds / ward_vitals / labs
# ==============================================================================
cat("Loading diagnosis.csv ...\n")
diag <- fread(
  file.path(path_raw, "diagnosis.csv"),
  select = c("subject_id", "chart_time", "icd10_cm"),
  na.strings = c("", "NA")
)
diag <- diag[subject_id %in% anchor_index$subject_id]
diag[, `:=`(
  chart_time = as.numeric(chart_time),
  icd3 = substr(gsub("\\.", "", toupper(trimws(icd10_cm))), 1, 3)
)]
diag <- diag[!is.na(subject_id) & !is.na(chart_time) & !is.na(icd3) & nchar(icd3) == 3]

cat("Loading medications.csv ...\n")
meds <- fread(
  file.path(path_raw, "medications.csv"),
  select = c("subject_id", "chart_time", "atc_code", "atc_code2", "atc_code3"),
  na.strings = c("", "NA")
)
meds <- meds[subject_id %in% anchor_index$subject_id]
meds[, `:=`(
  chart_time = as.numeric(chart_time),
  atc_code1 = toupper(trimws(fifelse(is.na(atc_code), "", atc_code))),
  atc_code2 = toupper(trimws(fifelse(is.na(atc_code2), "", atc_code2))),
  atc_code3 = toupper(trimws(fifelse(is.na(atc_code3), "", atc_code3)))
)]
meds <- meds[!is.na(subject_id) & !is.na(chart_time)]

cat("Loading ward_vitals.csv subset ...\n")
ward_file <- file.path(path_raw, "ward_vitals.csv")
ward <- fread(
  cmd = sprintf("grep -iE ',(vent|iabp|ecmo|crrt),' %s", shQuote(ward_file)),
  header = FALSE,
  col.names = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
ward <- ward[subject_id %in% anchor_index$subject_id]
ward[, `:=`(
  chart_time = as.numeric(chart_time),
  item_name = tolower(trimws(item_name)),
  value_num = suppressWarnings(as.numeric(value))
)]
ward <- ward[!is.na(subject_id) & !is.na(chart_time) & item_name %chin% c("vent", "iabp", "ecmo", "crrt")]

cat("Loading imputed preop lab features for AKI baseline ...\n")
preop_lab_imputed <- fread(
  path_preop_lab_imputed,
  select = c("op_id", "preop_creatinine"),
  na.strings = c("", "NA")
)
preop_lab_imputed[, `:=`(
  preop_creatinine = suppressWarnings(as.numeric(preop_creatinine))
)]
preop_lab_imputed <- unique(preop_lab_imputed, by = "op_id")

cat("Loading imputed postop lab features for AKI peak creatinine ...\n")
postop_lab_imputed <- fread(
  path_postop_lab_imputed,
  select = c("op_id", "postop_creatinine_peak", "postop_creatinine_peak_imputed"),
  na.strings = c("", "NA")
)
postop_lab_imputed[, `:=`(
  postop_creatinine_peak = suppressWarnings(as.numeric(postop_creatinine_peak)),
  postop_creatinine_peak_imputed = as.integer(postop_creatinine_peak_imputed)
)]
postop_lab_imputed <- unique(postop_lab_imputed, by = "op_id")

# ==============================================================================
# 3. Helper windows and helper functions
# ==============================================================================
postop_window <- anchor_index[, .(
  subject_id, op_id, case_id, hadm_id, orin_time, orout_time,
  start = postop30_start,
  end = postop30_end
)]
setkey(postop_window, subject_id, start, end)

closest_diag_event <- function(diag_matched, codes, event_name) {
  interval_col <- paste0(event_name, "_interval_after_surgery_min")
  matched <- diag_matched[icd3 %chin% codes]
  if (nrow(matched) == 0L) {
    return(data.table(op_id = integer(), tmp_flag = integer(), tmp_interval = numeric())[
      , setnames(.SD, c("tmp_flag", "tmp_interval"), c(event_name, interval_col))
    ])
  }
  matched[, interval_min := chart_time - orout_time]
  matched <- matched[interval_min >= 0]
  if (nrow(matched) == 0L) {
    return(data.table(op_id = integer(), tmp_flag = integer(), tmp_interval = numeric())[
      , setnames(.SD, c("tmp_flag", "tmp_interval"), c(event_name, interval_col))
    ])
  }
  matched <- matched[order(op_id, interval_min, chart_time)]
  matched <- matched[, .SD[1], by = op_id]
  out <- matched[, .(op_id, flag = 1L, interval = interval_min)]
  setnames(out, c("flag", "interval"), c(event_name, interval_col))
  out
}

closest_ward_event <- function(ward_matched, item_name_target, event_name) {
  interval_col <- paste0(event_name, "_interval_after_surgery_min")
  matched <- ward_matched[item_name == item_name_target & value_num == 1]
  if (nrow(matched) == 0L) {
    return(data.table(op_id = integer(), tmp_flag = integer(), tmp_interval = numeric())[
      , setnames(.SD, c("tmp_flag", "tmp_interval"), c(event_name, interval_col))
    ])
  }
  matched[, interval_min := chart_time - orout_time]
  matched <- matched[interval_min >= 0]
  if (nrow(matched) == 0L) {
    return(data.table(op_id = integer(), tmp_flag = integer(), tmp_interval = numeric())[
      , setnames(.SD, c("tmp_flag", "tmp_interval"), c(event_name, interval_col))
    ])
  }
  matched <- matched[order(op_id, interval_min, chart_time)]
  matched <- matched[, .SD[1], by = op_id]
  out <- matched[, .(op_id, flag = 1L, interval = interval_min)]
  setnames(out, c("flag", "interval"), c(event_name, interval_col))
  out
}

atc_any <- function(dt, pattern) {
  grepl(pattern, dt$atc_code1) | grepl(pattern, dt$atc_code2) | grepl(pattern, dt$atc_code3)
}

# ==============================================================================
# 4. Postop diagnosis complications within 30 days
# ==============================================================================
cat("Matching postop diagnosis events ...\n")
diag_interest_codes <- unique(c(
  "I21", "I22", "I23", "I20", "I26", "I46", "I48", "I63", "I61", "I60", "I62",
  "G81", "G82", "G45", "G96", "J80", "J96", "J12", "J13", "J14", "J15", "J16",
  "J17", "J18", "J90", "K55", "K56", "K65", "K72", "K85", "K92", "N17", "R57",
  "D65", "J38", "A40", "A41"
))
diag_union <- diag[icd3 %chin% diag_interest_codes]
diag_union[, `:=`(start = chart_time, end = chart_time)]
setkey(diag_union, subject_id, start, end)
diag_matched <- foverlaps(
  diag_union, postop_window,
  by.x = c("subject_id", "start", "end"),
  by.y = c("subject_id", "start", "end"),
  type = "within",
  nomatch = 0L
)

diag_events <- list(
  closest_diag_event(diag_matched, c("I21", "I22", "I23"), "acute_myocardial_infarction"),
  closest_diag_event(diag_matched, c("I20"), "angina"),
  closest_diag_event(diag_matched, c("I26"), "pulmonary_embolism"),
  closest_diag_event(diag_matched, c("I46"), "cardiac_arrest"),
  closest_diag_event(diag_matched, c("I63"), "cerebral_infarction"),
  closest_diag_event(diag_matched, c("I61"), "intracerebral_hemorrhage"),
  closest_diag_event(diag_matched, c("I60"), "subarachnoid_hemorrhage"),
  closest_diag_event(diag_matched, c("I62"), "subdural_hemorrhage"),
  closest_diag_event(diag_matched, c("G81"), "hemiplegia"),
  closest_diag_event(diag_matched, c("G82"), "paraplegia"),
  closest_diag_event(diag_matched, c("G45"), "tia"),
  closest_diag_event(diag_matched, c("G96"), "cerebrospinal_fluid_leak"),
  closest_diag_event(diag_matched, c("J80", "J96"), "ards"),
  closest_diag_event(diag_matched, c("J12", "J13", "J14", "J15", "J16", "J17", "J18"), "pneumonia"),
  closest_diag_event(diag_matched, c("J90"), "pleural_effusion"),
  closest_diag_event(diag_matched, c("K55"), "intestinal_ischemia"),
  closest_diag_event(diag_matched, c("K56"), "ileus"),
  closest_diag_event(diag_matched, c("K65"), "peritonitis"),
  closest_diag_event(diag_matched, c("K72"), "hepatic_failure"),
  closest_diag_event(diag_matched, c("K85"), "acute_pancreatitis"),
  closest_diag_event(diag_matched, c("K92"), "gastrointestinal_hemorrhage"),
  closest_diag_event(diag_matched, c("N17"), "acute_kidney_failure_icd10"),
  closest_diag_event(diag_matched, c("R57"), "shock"),
  closest_diag_event(diag_matched, c("D65"), "dic"),
  closest_diag_event(diag_matched, c("J38"), "vocal_cord_larynx_paralysis"),
  closest_diag_event(diag_matched, c("A40", "A41"), "sepsis")
)

# Preop AF history for new-onset AF
diag_af_preop <- diag[icd3 == "I48"][
  anchor_index,
  on = .(subject_id, chart_time < orin_time),
  allow.cartesian = TRUE,
  nomatch = 0L
][, .(preop_af_history = 1L), by = op_id]

postop_af <- closest_diag_event(diag_matched, c("I48"), "postop_af_raw")
new_onset_af <- merge(anchor_index[, .(op_id)], postop_af, by = "op_id", all.x = TRUE)
new_onset_af <- merge(new_onset_af, diag_af_preop, by = "op_id", all.x = TRUE)
new_onset_af[is.na(postop_af_raw), postop_af_raw := 0L]
new_onset_af[is.na(preop_af_history), preop_af_history := 0L]
new_onset_af[, new_onset_af := as.integer(postop_af_raw == 1L & preop_af_history == 0L)]
new_onset_af[, new_onset_af_interval_after_surgery_min := fifelse(new_onset_af == 1L, postop_af_raw_interval_after_surgery_min, NA_real_)]
new_onset_af <- new_onset_af[, .(op_id, new_onset_af, new_onset_af_interval_after_surgery_min)]

# ==============================================================================
# 5. Ward-based postop support events within 30 days
# ==============================================================================
cat("Matching postop ward support events ...\n")
ward_union <- ward[item_name %chin% c("vent", "iabp", "ecmo", "crrt")]
ward_union[, `:=`(start = chart_time, end = chart_time)]
setkey(ward_union, subject_id, start, end)
ward_matched <- foverlaps(
  ward_union, postop_window,
  by.x = c("subject_id", "start", "end"),
  by.y = c("subject_id", "start", "end"),
  type = "within",
  nomatch = 0L
)

ward_events <- list(
  closest_ward_event(ward_matched, "iabp", "iabp_postop"),
  closest_ward_event(ward_matched, "ecmo", "ecmo_postop"),
  closest_ward_event(ward_matched, "vent", "ventilation_postop"),
  closest_ward_event(ward_matched, "crrt", "crrt_postop")
)

# ==============================================================================
# 6. Postop medications within 30 days
# ==============================================================================
cat("Matching postop medication events ...\n")
meds_union <- meds[, `:=`(start = chart_time, end = chart_time)]
setkey(meds_union, subject_id, start, end)
meds_matched <- foverlaps(
  meds_union, postop_window,
  by.x = c("subject_id", "start", "end"),
  by.y = c("subject_id", "start", "end"),
  type = "within",
  nomatch = 0L
)

if (nrow(meds_matched) > 0L) {
  meds_matched[, interval_min := chart_time - orout_time]
}

make_med_flag <- function(matched_dt, pattern, event_name) {
  interval_col <- paste0(event_name, "_interval_after_surgery_min")
  x <- matched_dt[atc_any(matched_dt, pattern)]
  if (nrow(x) == 0L) {
    return(data.table(op_id = integer(), tmp_flag = integer(), tmp_interval = numeric())[
      , setnames(.SD, c("tmp_flag", "tmp_interval"), c(event_name, interval_col))
    ])
  }
  x <- x[interval_min >= 0][order(op_id, interval_min, chart_time)]
  if (nrow(x) == 0L) {
    return(data.table(op_id = integer(), tmp_flag = integer(), tmp_interval = numeric())[
      , setnames(.SD, c("tmp_flag", "tmp_interval"), c(event_name, interval_col))
    ])
  }
  x <- x[, .SD[1], by = op_id]
  out <- x[, .(op_id, flag = 1L, interval = interval_min)]
  setnames(out, c("flag", "interval"), c(event_name, interval_col))
  out
}

antibiotic_escalation <- make_med_flag(meds_matched, "^J01DH|^A07AA09$", "antibiotic_escalation_vanc_or_carbapenem")
inotropes_postop <- make_med_flag(meds_matched, "^C01C", "inotropes_and_vasopressors_postop")
antihemorrhagics_postop <- make_med_flag(meds_matched, "^B02", "antihemorrhagics_postop")

# New antiepileptics after surgery with no preop antiepileptics
meds_preop_antiepileptics <- meds[
  anchor_index,
  on = .(subject_id, chart_time < orin_time),
  allow.cartesian = TRUE,
  nomatch = 0L
]
if (nrow(meds_preop_antiepileptics) > 0L) {
  meds_preop_antiepileptics <- meds_preop_antiepileptics[atc_any(meds_preop_antiepileptics, "^N03")][, .(preop_antiepileptics = 1L), by = op_id]
} else {
  meds_preop_antiepileptics <- data.table(op_id = integer(), preop_antiepileptics = integer())
}

antiepileptics_postop_raw <- make_med_flag(meds_matched, "^N03", "antiepileptics_postop_raw")
antiepileptics_new <- merge(anchor_index[, .(op_id)], antiepileptics_postop_raw, by = "op_id", all.x = TRUE)
antiepileptics_new <- merge(antiepileptics_new, meds_preop_antiepileptics, by = "op_id", all.x = TRUE)
antiepileptics_new[is.na(antiepileptics_postop_raw), antiepileptics_postop_raw := 0L]
antiepileptics_new[is.na(preop_antiepileptics), preop_antiepileptics := 0L]
antiepileptics_new[, antiepileptics_new_postop := as.integer(antiepileptics_postop_raw == 1L & preop_antiepileptics == 0L)]
antiepileptics_new[, antiepileptics_new_postop_interval_after_surgery_min := fifelse(
  antiepileptics_new_postop == 1L,
  antiepileptics_postop_raw_interval_after_surgery_min,
  NA_real_
)]
antiepileptics_new <- antiepileptics_new[, .(op_id, antiepileptics_new_postop, antiepileptics_new_postop_interval_after_surgery_min)]

# ==============================================================================
# 7. Reoperation / ICU / death / LOS
# ==============================================================================
cat("Building operation timeline outcomes ...\n")

reop_dt <- ops_all[
  anchor_index,
  on = .(subject_id, hadm_id),
  allow.cartesian = TRUE,
  nomatch = 0L
][op_id != i.op_id]

reop_dt <- reop_dt[orin_time > i.orout_time & orin_time <= i.orout_time + WINDOW_30D_MIN]
if (nrow(reop_dt) > 0L) {
  reop_dt[, reop_interval := orin_time - i.orout_time]
  reop_dt <- reop_dt[order(i.op_id, reop_interval, orin_time)][, .SD[1], by = i.op_id][
    , .(op_id = i.op_id, reoperation = 1L, reoperation_interval_after_surgery_min = reop_interval)
  ]
} else {
  reop_dt <- data.table(op_id = integer(), reoperation = integer(), reoperation_interval_after_surgery_min = numeric())
}

ops_timeline <- anchor_index[, .(
  op_id,
  death_within_hospital_stay = as.integer(!is.na(inhosp_death_time) & inhosp_death_time > 0),
  death_within_30_days = as.integer(
    (!is.na(inhosp_death_time) & inhosp_death_time >= orout_time & inhosp_death_time - orout_time <= 43200) |
      (!is.na(allcause_death_time) & allcause_death_time >= orout_time & allcause_death_time - orout_time <= 43200)
  ),
  death_within_90_days = as.integer(
    !is.na(allcause_death_time) & allcause_death_time >= orout_time & allcause_death_time - orout_time <= WINDOW_90D_MIN
  ),
  death_time_from_orout_min = fifelse(
    !is.na(allcause_death_time) & allcause_death_time >= orout_time,
    allcause_death_time - orout_time,
    fifelse(!is.na(inhosp_death_time) & inhosp_death_time >= orout_time, inhosp_death_time - orout_time, NA_real_)
  ),
  unexpected_icu_admission_from_general_ward = as.integer(!is.na(icuin_time) & !is.na(orout_time) & (icuin_time - orout_time) > 20),
  interval_between_icu_in_and_orout_min = fifelse(!is.na(icuin_time) & !is.na(orout_time), icuin_time - orout_time, NA_real_),
  icu_stay = as.integer(!is.na(icuout_time) & !is.na(orout_time) & icuout_time > orout_time),
  icu_duration_min = fifelse(
    is.na(icuout_time) | is.na(orout_time) | icuout_time <= orout_time,
    NA_real_,
    fifelse(!is.na(icuin_time) & icuin_time >= orout_time, icuout_time - icuin_time, icuout_time - orout_time)
  ),
  hospital_stay_duration_min = fifelse(!is.na(discharge_time) & !is.na(orout_time) & discharge_time >= orout_time, discharge_time - orout_time, NA_real_)
)]

# ==============================================================================
# 8. AKI by creatinine and CRRT
# ==============================================================================
cat("Calculating AKI by creatinine ...\n")

baseline_cr <- merge(
  anchor_index[, .(op_id)],
  preop_lab_imputed[, .(
    op_id,
    baseline_creatinine = preop_creatinine
  )],
  by = "op_id",
  all.x = TRUE
)

peak_cr <- merge(
  anchor_index[, .(op_id)],
  postop_lab_imputed[, .(
    op_id,
    peak_creatinine_postop = postop_creatinine_peak,
    peak_creatinine_postop_imputed = fifelse(is.na(postop_creatinine_peak_imputed), 0L, postop_creatinine_peak_imputed)
  )],
  by = "op_id",
  all.x = TRUE
)
peak_cr <- merge(peak_cr, baseline_cr, by = "op_id", all.x = TRUE)
peak_cr[, peak_creatinine_interval_after_surgery_min := NA_real_]
peak_cr[, `:=`(
  aki_cr_evaluable = as.integer(!is.na(baseline_creatinine) & !is.na(peak_creatinine_postop)),
  ratio = peak_creatinine_postop / baseline_creatinine,
  abs_delta = peak_creatinine_postop - baseline_creatinine
)]
# KDIGO SCr-only staging (no urine output):
# stage 1: ratio 1.5-1.9 OR abs delta >= 0.3 mg/dL
# stage 2: ratio 2.0-2.9
# stage 3: ratio >= 3 OR peak SCr >= 4.0 mg/dL OR RRT (handled below)
peak_cr[, aki_category := fifelse(
  aki_cr_evaluable == 0L,
  NA_integer_,
  fifelse(
    ratio >= 3 | peak_creatinine_postop >= 4,
    3L,
    fifelse(
      ratio >= 2,
      2L,
      fifelse(
        ratio >= 1.5 | abs_delta >= 0.3,
        1L,
        0L
      )
    )
  )
)]
peak_cr[, aki_creatinine := fifelse(is.na(aki_category), NA_integer_, as.integer(aki_category >= 1L))]
peak_cr <- peak_cr[, .(
  op_id,
  aki_cr_evaluable = aki_cr_evaluable,
  aki_creatinine = as.integer(aki_creatinine),
  aki_category = as.integer(aki_category),
  baseline_creatinine = baseline_creatinine,
  peak_creatinine_postop = peak_creatinine_postop,
  peak_creatinine_postop_imputed = peak_creatinine_postop_imputed,
  peak_creatinine_interval_after_surgery_min = peak_creatinine_interval_after_surgery_min
)]

# Upgrade AKI category to stage 3 if CRRT occurs
crrt_dt <- NULL
if (length(ward_events) >= 4L) {
  crrt_dt <- ward_events[[4]]
}
if (is.null(crrt_dt)) {
  crrt_dt <- data.table(op_id = integer(), crrt_postop = integer(), crrt_postop_interval_after_surgery_min = numeric())
}

aki_final <- merge(anchor_index[, .(op_id)], peak_cr, by = "op_id", all.x = TRUE)
aki_final <- merge(aki_final, crrt_dt, by = "op_id", all.x = TRUE)
if (!"crrt_postop" %in% names(aki_final)) aki_final[, crrt_postop := 0L]
set(aki_final, which(is.na(aki_final$crrt_postop)), "crrt_postop", 0L)
if (!"crrt_postop_interval_after_surgery_min" %in% names(aki_final)) {
  aki_final[, crrt_postop_interval_after_surgery_min := NA_real_]
}
if (!"aki_cr_evaluable" %in% names(aki_final)) {
  aki_final[, aki_cr_evaluable := as.integer(!is.na(baseline_creatinine) & !is.na(peak_creatinine_postop))]
}
aki_final[, aki_category := fifelse(
  crrt_postop == 1L &
    !is.na(crrt_postop_interval_after_surgery_min) &
    crrt_postop_interval_after_surgery_min <= WINDOW_7D_MIN &
    (is.na(aki_category) | aki_category < 3L),
  3L,
  aki_category
)]
aki_final[, aki_creatinine := fifelse(is.na(aki_category), NA_integer_, as.integer(aki_category >= 1L))]
aki_final[, aki_creatinine_unknown := as.integer(is.na(aki_category))]

# ==============================================================================
# 9. Merge final output
# ==============================================================================
cat("Merging final outcome table ...\n")

final_dt <- copy(anchor_index[, .(subject_id, hadm_id, op_id, admission_time, orin_time, orout_time, discharge_time)])

merge_many <- function(base_dt, pieces) {
  out <- copy(base_dt)
  for (p in pieces) {
    out <- merge(out, p, by = "op_id", all.x = TRUE)
  }
  out
}

all_pieces <- c(
  diag_events,
  list(new_onset_af),
  ward_events[1:3],
  list(reop_dt, ops_timeline, antibiotic_escalation, inotropes_postop, antiepileptics_new, antihemorrhagics_postop, aki_final)
)

final_dt <- merge_many(final_dt, all_pieces)

if ("icu_duration_min" %in% names(final_dt)) {
  final_dt[!is.na(icu_duration_min) & icu_duration_min < 0, icu_duration_min := 0]
}

flag_cols <- c(
  "acute_myocardial_infarction", "angina", "pulmonary_embolism", "cardiac_arrest",
  "new_onset_af", "cerebral_infarction", "intracerebral_hemorrhage", "subarachnoid_hemorrhage",
  "subdural_hemorrhage", "hemiplegia", "paraplegia", "tia", "cerebrospinal_fluid_leak",
  "ards", "pneumonia", "pleural_effusion", "intestinal_ischemia", "ileus", "peritonitis",
  "hepatic_failure", "acute_pancreatitis", "gastrointestinal_hemorrhage",
  "acute_kidney_failure_icd10", "shock", "dic", "vocal_cord_larynx_paralysis", "sepsis",
  "antibiotic_escalation_vanc_or_carbapenem", "inotropes_and_vasopressors_postop",
  "antiepileptics_new_postop", "antihemorrhagics_postop", "ventilation_postop",
  "iabp_postop", "ecmo_postop", "death_within_hospital_stay", "death_within_30_days",
  "death_within_90_days", "unexpected_icu_admission_from_general_ward", "reoperation",
  "icu_stay", "crrt_postop"
)

for (nm in flag_cols) {
  if (!nm %in% names(final_dt)) final_dt[, (nm) := 0L]
  set(final_dt, which(is.na(final_dt[[nm]])), nm, 0L)
}

drop_cols <- intersect(c("case_id", "flag", "interval"), names(final_dt))
if (length(drop_cols) > 0L) {
  final_dt[, (drop_cols) := NULL]
}

setorderv(final_dt, c("subject_id", "hadm_id", "op_id"))

final_out <- copy(final_dt)
duration_min_cols <- grep(
  "_interval_after_surgery_min$|_time_from_orout_min$|_between_icu_in_and_orout_min$|_duration_min$",
  names(final_out),
  value = TRUE
)
if (length(duration_min_cols) > 0L) {
  for (col_nm in duration_min_cols) {
    v <- suppressWarnings(as.numeric(final_out[[col_nm]]))
    col_day <- sub("_min$", "_day", col_nm)
    final_out[, (col_day) := v / 1440]
  }
  final_out[, (duration_min_cols) := NULL]
}
setorderv(final_out, c("subject_id", "hadm_id", "op_id"))
fwrite(final_out, file.path(path_output, "postop_complications_word_defined_first_nonMAC.csv"))

# ==============================================================================
# 10. Summary outputs
# ==============================================================================
cat("Building summary outputs ...\n")

summary_vars <- c(
  "death_within_hospital_stay", "death_within_30_days", "death_within_90_days",
  "unexpected_icu_admission_from_general_ward", "reoperation",
  "acute_myocardial_infarction", "angina", "pulmonary_embolism", "cardiac_arrest", "new_onset_af",
  "iabp_postop", "ecmo_postop", "cerebral_infarction", "intracerebral_hemorrhage",
  "subarachnoid_hemorrhage", "subdural_hemorrhage", "hemiplegia", "paraplegia", "tia",
  "cerebrospinal_fluid_leak", "ards", "pneumonia", "pleural_effusion", "intestinal_ischemia",
  "ileus", "peritonitis", "hepatic_failure", "acute_pancreatitis", "gastrointestinal_hemorrhage",
  "acute_kidney_failure_icd10", "aki_creatinine", "aki_creatinine_unknown", "crrt_postop", "shock", "dic",
  "vocal_cord_larynx_paralysis", "sepsis", "antibiotic_escalation_vanc_or_carbapenem",
  "inotropes_and_vasopressors_postop", "antiepileptics_new_postop", "antihemorrhagics_postop",
  "ventilation_postop", "icu_stay"
)

summary_out <- rbindlist(lapply(summary_vars, function(v) {
  interval_var <- switch(
    v,
    death_within_hospital_stay = "death_time_from_orout_min",
    death_within_30_days = "death_time_from_orout_min",
    death_within_90_days = "death_time_from_orout_min",
    unexpected_icu_admission_from_general_ward = "interval_between_icu_in_and_orout_min",
    reoperation = "reoperation_interval_after_surgery_min",
    acute_myocardial_infarction = "acute_myocardial_infarction_interval_after_surgery_min",
    angina = "angina_interval_after_surgery_min",
    pulmonary_embolism = "pulmonary_embolism_interval_after_surgery_min",
    cardiac_arrest = "cardiac_arrest_interval_after_surgery_min",
    new_onset_af = "new_onset_af_interval_after_surgery_min",
    iabp_postop = "iabp_postop_interval_after_surgery_min",
    ecmo_postop = "ecmo_postop_interval_after_surgery_min",
    cerebral_infarction = "cerebral_infarction_interval_after_surgery_min",
    intracerebral_hemorrhage = "intracerebral_hemorrhage_interval_after_surgery_min",
    subarachnoid_hemorrhage = "subarachnoid_hemorrhage_interval_after_surgery_min",
    subdural_hemorrhage = "subdural_hemorrhage_interval_after_surgery_min",
    hemiplegia = "hemiplegia_interval_after_surgery_min",
    paraplegia = "paraplegia_interval_after_surgery_min",
    tia = "tia_interval_after_surgery_min",
    cerebrospinal_fluid_leak = "cerebrospinal_fluid_leak_interval_after_surgery_min",
    ards = "ards_interval_after_surgery_min",
    pneumonia = "pneumonia_interval_after_surgery_min",
    pleural_effusion = "pleural_effusion_interval_after_surgery_min",
    intestinal_ischemia = "intestinal_ischemia_interval_after_surgery_min",
    ileus = "ileus_interval_after_surgery_min",
    peritonitis = "peritonitis_interval_after_surgery_min",
    hepatic_failure = "hepatic_failure_interval_after_surgery_min",
    acute_pancreatitis = "acute_pancreatitis_interval_after_surgery_min",
    gastrointestinal_hemorrhage = "gastrointestinal_hemorrhage_interval_after_surgery_min",
    acute_kidney_failure_icd10 = "acute_kidney_failure_icd10_interval_after_surgery_min",
    aki_creatinine = "peak_creatinine_interval_after_surgery_min",
    crrt_postop = "crrt_postop_interval_after_surgery_min",
    shock = "shock_interval_after_surgery_min",
    dic = "dic_interval_after_surgery_min",
    vocal_cord_larynx_paralysis = "vocal_cord_larynx_paralysis_interval_after_surgery_min",
    sepsis = "sepsis_interval_after_surgery_min",
    antibiotic_escalation_vanc_or_carbapenem = "antibiotic_escalation_vanc_or_carbapenem_interval_after_surgery_min",
    inotropes_and_vasopressors_postop = "inotropes_and_vasopressors_postop_interval_after_surgery_min",
    antiepileptics_new_postop = "antiepileptics_new_postop_interval_after_surgery_min",
    antihemorrhagics_postop = "antihemorrhagics_postop_interval_after_surgery_min",
    ventilation_postop = "ventilation_postop_interval_after_surgery_min",
    icu_stay = "icu_duration_min",
    NA_character_
  )
  interval_vec <- if (!is.na(interval_var) && interval_var %in% names(final_dt)) final_dt[[interval_var]] else rep(NA_real_, nrow(final_dt))
  flag_vec <- final_dt[[v]]
  data.table(
    outcome = v,
    n_cases = sum(flag_vec == 1L, na.rm = TRUE),
    total_ops = nrow(final_dt),
    prevalence_pct = round(100 * mean(flag_vec == 1L, na.rm = TRUE), 2),
    median_interval_min = round(median(interval_vec[flag_vec == 1L], na.rm = TRUE), 1),
    p25_interval_min = round(quantile(interval_vec[flag_vec == 1L], 0.25, na.rm = TRUE, names = FALSE), 1),
    p75_interval_min = round(quantile(interval_vec[flag_vec == 1L], 0.75, na.rm = TRUE, names = FALSE), 1)
  )
}), use.names = TRUE)

for (j in c("median_interval_min", "p25_interval_min", "p75_interval_min")) {
  set(summary_out, which(!is.finite(summary_out[[j]])), j, NA_real_)
}

extra_duration_summary <- data.table(
  metric = c("icu_duration_min", "hospital_stay_duration_min", "death_time_from_orout_min"),
  n_nonmissing = c(
    sum(!is.na(final_dt$icu_duration_min)),
    sum(!is.na(final_dt$hospital_stay_duration_min)),
    sum(!is.na(final_dt$death_time_from_orout_min))
  ),
  median = c(
    round(median(final_dt$icu_duration_min, na.rm = TRUE), 1),
    round(median(final_dt$hospital_stay_duration_min, na.rm = TRUE), 1),
    round(median(final_dt$death_time_from_orout_min, na.rm = TRUE), 1)
  ),
  p25 = c(
    round(quantile(final_dt$icu_duration_min, 0.25, na.rm = TRUE, names = FALSE), 1),
    round(quantile(final_dt$hospital_stay_duration_min, 0.25, na.rm = TRUE, names = FALSE), 1),
    round(quantile(final_dt$death_time_from_orout_min, 0.25, na.rm = TRUE, names = FALSE), 1)
  ),
  p75 = c(
    round(quantile(final_dt$icu_duration_min, 0.75, na.rm = TRUE, names = FALSE), 1),
    round(quantile(final_dt$hospital_stay_duration_min, 0.75, na.rm = TRUE, names = FALSE), 1),
    round(quantile(final_dt$death_time_from_orout_min, 0.75, na.rm = TRUE, names = FALSE), 1)
  )
)
for (j in c("median", "p25", "p75")) {
  set(extra_duration_summary, which(!is.finite(extra_duration_summary[[j]])), j, NA_real_)
}

fwrite(summary_out, file.path(path_output, "postop_complications_word_summary.csv"))
fwrite(extra_duration_summary, file.path(path_output, "postop_complications_duration_summary.csv"))

notes <- data.table(
  note_type = c("anchor_definition", "window_definition", "new_onset_af_rule", "sepsis_rule", "linkage_risk", "aki_rule", "duration_unit_rule"),
  note = c(
    "If an admission has multiple surgeries, anchor op is the first non-MAC surgery.",
    "Complication window = [orout_time, orout_time + 30 days] unless the metric itself specifies 7 days or 90 days.",
    "New-onset AF requires postop I48 and no preop I48 before orin_time.",
    "Sepsis in v3 uses the clinically common ICD-10 family A40-A41.",
    "diagnosis.csv / medications.csv / ward_vitals.csv only contain subject_id; linkage is time-window based and may still carry cross-admission risk within the same subject.",
    "AKI uses KDIGO SCr criteria without urine output: stage1 (ratio 1.5-1.9 or abs delta >=0.3 mg/dL), stage2 (ratio 2.0-2.9), stage3 (ratio >=3 or peak SCr >=4.0 mg/dL), and CRRT within 7 days upgrades to stage3. Missing baseline/peak SCr is retained as unknown (not auto-labeled non-AKI).",
    "All computed duration/interval fields are output as _day columns only; corresponding _min columns are removed from the final outcome table."
  )
)
fwrite(notes, file.path(path_output, "postop_complications_word_notes.csv"))

cat("\nTop 20 complication prevalence:\n")
print(summary_out[order(-prevalence_pct)][1:min(20L, .N)])
cat("\nDone.\n")
