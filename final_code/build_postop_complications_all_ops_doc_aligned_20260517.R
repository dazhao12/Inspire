#!/usr/bin/env Rscript
# Timestamp: 2026-05-17T14:05:00Z
# Purpose: Build all-operation postoperative outcomes/complications aligned to the
# MOVER variable definition document. Extraction only; AKI uses raw creatinine
# without imputation.

suppressPackageStartupMessages({
  library(data.table)
})

raw_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/01_raw"
out_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517"
out_defined <- file.path(out_dir, "postop_complications_defined_latest.csv")
out_summary <- file.path(out_dir, "postop_complications_summary_latest.csv")
out_dictionary <- file.path(out_dir, "postop_complications_definition_dictionary_20260517.csv")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

WINDOW_30D <- 30 * 24 * 60
WINDOW_90D <- 90 * 24 * 60
WINDOW_7D <- 7 * 24 * 60
WINDOW_48H <- 48 * 60
PREOP_90D <- 90 * 24 * 60

num <- function(x) suppressWarnings(as.numeric(x))
clean_code <- function(x) gsub("[^A-Z0-9]", "", toupper(trimws(as.character(x))))

code_hit_expr <- function(code_vec_name, prefixes) {
  paste0("startsWith(", code_vec_name, ", '", prefixes, "')", collapse = " | ")
}

cat("Reading operations.csv ...\n")
ops <- fread(
  file.path(raw_dir, "operations.csv"),
  select = c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate", "antype",
    "orin_time", "orout_time", "opstart_time", "opend_time", "anstart_time", "anend_time",
    "admission_time", "discharge_time", "icuin_time", "icuout_time",
    "inhosp_death_time", "allcause_death_time"
  ),
  na.strings = c("", "NA", "NULL", "(Null)", "null")
)
for (cc in c("opdate", "orin_time", "orout_time", "opstart_time", "opend_time", "anstart_time", "anend_time", "admission_time", "discharge_time", "icuin_time", "icuout_time", "inhosp_death_time", "allcause_death_time")) {
  ops[, (cc) := num(get(cc))]
}
ops <- ops[!is.na(op_id) & !is.na(subject_id) & !is.na(orout_time)]
setorderv(ops, c("subject_id", "hadm_id", "orout_time", "op_id"))

anchor <- ops[, .(
  op_id, subject_id, hadm_id, case_id, admission_time, orin_time, orout_time, discharge_time,
  icuin_time, icuout_time, inhosp_death_time, allcause_death_time,
  postop30_start = orout_time,
  postop30_end = orout_time + WINDOW_30D,
  postop7_start = orout_time,
  postop7_end = orout_time + WINDOW_7D,
  postop48h_end = orout_time + WINDOW_48H,
  preop90_start = orin_time - PREOP_90D,
  preop_end = orin_time
)]

cat(sprintf("All operations kept: %d\n", nrow(anchor)))

# ICD outcome definitions. Prefixes are dot-stripped, uppercase ICD-10-CM startsWith patterns.
diag_defs <- data.table::rbindlist(list(
  data.table(var = "acute_myocardial_infarction", group = "cardiac", prefixes = list(c("I21", "I22")), definition = "ICD-10-CM I21-I22 within 30 days after OR exit"),
  data.table(var = "angina", group = "cardiac", prefixes = list(c("I20")), definition = "ICD-10-CM I20 within 30 days after OR exit"),
  data.table(var = "cardiac_arrest", group = "cardiac", prefixes = list(c("I46")), definition = "ICD-10-CM I46 within 30 days after OR exit; nonfatal flag additionally excludes death at same event"),
  data.table(var = "heart_failure", group = "cardiac", prefixes = list(c("I50", "J81")), definition = "ICD-10-CM I50 or supportive pulmonary edema J81 within 30 days after OR exit"),
  data.table(var = "ventricular_arrhythmia_raw", group = "cardiac", prefixes = list(c("I472", "I490")), definition = "ICD-10-CM I47.2/I49.0 within 30 days after OR exit"),
  data.table(var = "postop_af_raw", group = "cardiac", prefixes = list(c("I48")), definition = "ICD-10-CM I48 within 30 days after OR exit; INSPIRE stores AF/atrial flutter at the 3-character I48 level"),

  data.table(var = "cerebral_infarction", group = "neurologic", prefixes = list(c("I63", "I64")), definition = "ICD-10-CM I63 or optional I64 within 30 days after OR exit"),
  data.table(var = "tia", group = "neurologic", prefixes = list(c("G45")), definition = "ICD-10-CM G45 within 30 days after OR exit"),
  data.table(var = "intracerebral_hemorrhage", group = "neurologic", prefixes = list(c("I61")), definition = "ICD-10-CM I61 within 30 days after OR exit"),
  data.table(var = "subarachnoid_hemorrhage", group = "neurologic", prefixes = list(c("I60")), definition = "ICD-10-CM I60 within 30 days after OR exit"),
  data.table(var = "subdural_hemorrhage", group = "neurologic", prefixes = list(c("I62")), definition = "ICD-10-CM I62 within 30 days after OR exit"),
  data.table(var = "hemiplegia", group = "neurologic", prefixes = list(c("G81")), definition = "ICD-10-CM G81 within 30 days after OR exit"),
  data.table(var = "paraplegia", group = "neurologic", prefixes = list(c("G82")), definition = "ICD-10-CM G82 within 30 days after OR exit"),
  data.table(var = "cerebrospinal_fluid_leak", group = "other", prefixes = list(c("G960", "G9782")), definition = "ICD-10-CM G96.0/G97.82 within 30 days after OR exit"),

  data.table(var = "acute_kidney_failure_icd10", group = "renal", prefixes = list(c("N17")), definition = "ICD-10-CM N17 within 30 days after OR exit"),

  data.table(var = "pneumonia", group = "pulmonary_infectious", prefixes = list(c("J12", "J13", "J14", "J15", "J16", "J17", "J18")), definition = "ICD-10-CM J12-J18 within 30 days after OR exit"),
  data.table(var = "respiratory_failure", group = "pulmonary", prefixes = list(c("J96", "J9582")), definition = "ICD-10-CM J96/J95.82 within 30 days after OR exit"),
  data.table(var = "ards", group = "pulmonary", prefixes = list(c("J80")), definition = "ICD-10-CM J80 within 30 days after OR exit"),
  data.table(var = "pleural_effusion", group = "pulmonary", prefixes = list(c("J90")), definition = "ICD-10-CM J90 within 30 days after OR exit"),
  data.table(var = "atelectasis", group = "pulmonary", prefixes = list(c("J9811")), definition = "ICD-10-CM J98.11 within 30 days after OR exit"),
  data.table(var = "pneumothorax", group = "pulmonary", prefixes = list(c("J93")), definition = "ICD-10-CM J93 within 30 days after OR exit"),
  data.table(var = "bronchospasm", group = "pulmonary", prefixes = list(c("J9801", "J9809")), definition = "ICD-10-CM J98.01/J98.09 within 30 days after OR exit"),
  data.table(var = "aspiration_pneumonitis", group = "pulmonary", prefixes = list(c("J69")), definition = "ICD-10-CM J69 within 30 days after OR exit; tracked separately from confirmed pneumonia"),
  data.table(var = "pulmonary_edema", group = "pulmonary", prefixes = list(c("J81")), definition = "ICD-10-CM J81 within 30 days after OR exit"),
  data.table(var = "tracheobronchitis", group = "pulmonary", prefixes = list(c("J20", "J40")), definition = "ICD-10-CM J20/J40 within 30 days after OR exit"),
  data.table(var = "exacerbation_chronic_lung_disease", group = "pulmonary", prefixes = list(c("J441", "J45901")), definition = "ICD-10-CM J44.1/J45.901 within 30 days after OR exit"),

  data.table(var = "surgical_site_infection", group = "infectious", prefixes = list(c("T8141", "T8142", "T8143", "T8149")), definition = "ICD-10-CM T81.41-T81.43/T81.49 within 30 days after OR exit"),
  data.table(var = "sepsis", group = "infectious", prefixes = list(c("A40", "A41")), definition = "ICD-10-CM A40/A41 within 30 days after OR exit; used as bloodstream infection proxy when culture data unavailable"),
  data.table(var = "peritonitis", group = "infectious_other", prefixes = list(c("K65")), definition = "ICD-10-CM K65 within 30 days after OR exit"),
  data.table(var = "urinary_tract_infection", group = "infectious", prefixes = list(c("N10", "N12", "N30", "N39")), definition = "ICD-10-CM N10/N12/N30/N39 within 30 days after OR exit"),

  data.table(var = "intestinal_ischemia", group = "other", prefixes = list(c("K550", "K559")), definition = "ICD-10-CM K55.0/K55.9 within 30 days after OR exit"),
  data.table(var = "ileus", group = "other", prefixes = list(c("K567")), definition = "ICD-10-CM K56.7 within 30 days after OR exit"),
  data.table(var = "hepatic_failure", group = "other", prefixes = list(c("K72")), definition = "ICD-10-CM K72 within 30 days after OR exit"),
  data.table(var = "acute_pancreatitis", group = "other", prefixes = list(c("K85")), definition = "ICD-10-CM K85 within 30 days after OR exit"),
  data.table(var = "gastrointestinal_hemorrhage", group = "other", prefixes = list(c("K920", "K921", "K922")), definition = "ICD-10-CM K92.0/K92.1/K92.2 within 30 days after OR exit"),
  data.table(var = "pulmonary_embolism", group = "other", prefixes = list(c("I26")), definition = "ICD-10-CM I26 within 30 days after OR exit"),
  data.table(var = "shock", group = "other", prefixes = list(c("R57")), definition = "ICD-10-CM R57 within 30 days after OR exit"),
  data.table(var = "dic", group = "other", prefixes = list(c("D65")), definition = "ICD-10-CM D65 within 30 days after OR exit")
), fill = TRUE)
all_prefixes <- unique(unlist(diag_defs$prefixes))

cat("Reading diagnosis.csv ...\n")
diag <- fread(file.path(raw_dir, "diagnosis.csv"), select = c("subject_id", "chart_time", "icd10_cm"), na.strings = c("", "NA", "NULL", "(Null)", "null"))
diag[, `:=`(chart_time = num(chart_time), icd_clean = clean_code(icd10_cm))]
diag <- diag[!is.na(subject_id) & !is.na(chart_time) & !is.na(icd_clean) & nchar(icd_clean) > 0]
# Keep only codes that can contribute to a documented postoperative outcome.
diag <- diag[Reduce(`|`, lapply(all_prefixes, function(pfx) startsWith(icd_clean, pfx)))]

cat("Matching postoperative diagnosis events ...\n")
postop_window <- anchor[, .(subject_id, op_id, orout_time, start = postop30_start, end = postop30_end)]
setkey(postop_window, subject_id, start, end)
diag[, `:=`(start = chart_time, end = chart_time)]
setkey(diag, subject_id, start, end)
diag_matched <- foverlaps(diag, postop_window, by.x = c("subject_id", "start", "end"), by.y = c("subject_id", "start", "end"), type = "within", nomatch = 0L)
if (nrow(diag_matched) > 0L) diag_matched[, interval_min := chart_time - orout_time]

make_diag_event <- function(var_name, prefixes) {
  interval_col <- paste0(var_name, "_interval_after_surgery_min")
  x <- diag_matched[Reduce(`|`, lapply(prefixes, function(pfx) startsWith(icd_clean, pfx)))]
  if (nrow(x) == 0L) return(data.table(op_id = integer(), flag = integer(), interval = numeric())[, setnames(.SD, c("flag", "interval"), c(var_name, interval_col))])
  x <- x[interval_min >= 0][order(op_id, interval_min, chart_time)]
  if (nrow(x) == 0L) return(data.table(op_id = integer(), flag = integer(), interval = numeric())[, setnames(.SD, c("flag", "interval"), c(var_name, interval_col))])
  out <- x[, .SD[1], by = op_id][, .(op_id, flag = 1L, interval = interval_min)]
  setnames(out, c("flag", "interval"), c(var_name, interval_col))
  out
}

diag_events <- lapply(seq_len(nrow(diag_defs)), function(i) make_diag_event(diag_defs$var[i], diag_defs$prefixes[[i]]))

# Preop history for new-onset events.
preop_hist_flag <- function(prefixes, out_name) {
  d <- diag[Reduce(`|`, lapply(prefixes, function(pfx) startsWith(icd_clean, pfx))), .(subject_id, chart_time)]
  if (nrow(d) == 0L) return(data.table(op_id = integer(), flag = integer())[, setnames(.SD, "flag", out_name)])
  x <- d[anchor, on = .(subject_id, chart_time < orin_time), allow.cartesian = TRUE, nomatch = 0L]
  if (nrow(x) == 0L) return(data.table(op_id = integer(), flag = integer())[, setnames(.SD, "flag", out_name)])
  x <- x[, .(flag = 1L), by = op_id]
  setnames(x, "flag", out_name)
  x
}
preop_af <- preop_hist_flag(c("I48"), "preop_af_history")
preop_va <- preop_hist_flag(c("I472", "I490"), "preop_ventricular_arrhythmia_history")

cat("Reading ward_vitals support subset ...\n")
ward_file <- file.path(raw_dir, "ward_vitals.csv")
ward <- fread(cmd = sprintf("grep -iE ',(vent|iabp|ecmo|crrt),' %s", shQuote(ward_file)), header = FALSE, col.names = c("subject_id", "chart_time", "item_name", "value"), na.strings = c("", "NA", "NULL", "(Null)", "null"))
ward[, `:=`(chart_time = num(chart_time), item_name = tolower(trimws(item_name)), value_num = num(value))]
ward <- ward[!is.na(subject_id) & !is.na(chart_time) & item_name %chin% c("vent", "iabp", "ecmo", "crrt")]
ward[, `:=`(start = chart_time, end = chart_time)]
setkey(ward, subject_id, start, end)
ward_matched <- foverlaps(ward, postop_window, by.x = c("subject_id", "start", "end"), by.y = c("subject_id", "start", "end"), type = "within", nomatch = 0L)
if (nrow(ward_matched) > 0L) ward_matched[, interval_min := chart_time - orout_time]

make_ward_event <- function(item, var_name, max_window = WINDOW_30D) {
  interval_col <- paste0(var_name, "_interval_after_surgery_min")
  x <- ward_matched[item_name == item & value_num == 1 & interval_min >= 0 & interval_min <= max_window][order(op_id, interval_min, chart_time)]
  if (nrow(x) == 0L) return(data.table(op_id = integer(), flag = integer(), interval = numeric())[, setnames(.SD, c("flag", "interval"), c(var_name, interval_col))])
  out <- x[, .SD[1], by = op_id][, .(op_id, flag = 1L, interval = interval_min)]
  setnames(out, c("flag", "interval"), c(var_name, interval_col))
  out
}
ward_events <- list(
  make_ward_event("vent", "ventilation_postop", WINDOW_30D),
  make_ward_event("iabp", "iabp_postop", WINDOW_30D),
  make_ward_event("ecmo", "ecmo_postop", WINDOW_30D),
  make_ward_event("crrt", "crrt_postop", WINDOW_7D)
)
crrt_dt <- ward_events[[4]]

cat("Reading medications.csv for legacy medication-based postoperative support flags ...\n")
meds <- fread(file.path(raw_dir, "medications.csv"), select = c("subject_id", "chart_time", "atc_code", "atc_code2", "atc_code3"), na.strings = c("", "NA", "NULL", "(Null)", "null"))
meds[, `:=`(
  chart_time = num(chart_time),
  atc_code1 = toupper(trimws(fifelse(is.na(atc_code), "", atc_code))),
  atc_code2 = toupper(trimws(fifelse(is.na(atc_code2), "", atc_code2))),
  atc_code3 = toupper(trimws(fifelse(is.na(atc_code3), "", atc_code3)))
)]
meds <- meds[!is.na(subject_id) & !is.na(chart_time)]
meds[, `:=`(start = chart_time, end = chart_time)]
setkey(meds, subject_id, start, end)
meds_matched <- foverlaps(meds, postop_window, by.x = c("subject_id", "start", "end"), by.y = c("subject_id", "start", "end"), type = "within", nomatch = 0L)
if (nrow(meds_matched) > 0L) meds_matched[, interval_min := chart_time - orout_time]
atc_any <- function(dt, pattern) grepl(pattern, dt$atc_code1) | grepl(pattern, dt$atc_code2) | grepl(pattern, dt$atc_code3)
make_med_event <- function(pattern, var_name) {
  interval_col <- paste0(var_name, "_interval_after_surgery_min")
  x <- meds_matched[atc_any(meds_matched, pattern) & interval_min >= 0][order(op_id, interval_min, chart_time)]
  if (nrow(x) == 0L) return(data.table(op_id = integer(), flag = integer(), interval = numeric())[, setnames(.SD, c("flag", "interval"), c(var_name, interval_col))])
  out <- x[, .SD[1], by = op_id][, .(op_id, flag = 1L, interval = interval_min)]
  setnames(out, c("flag", "interval"), c(var_name, interval_col))
  out
}
med_events <- list(
  make_med_event("^J01DH|^A07AA09$", "antibiotic_escalation_vanc_or_carbapenem"),
  make_med_event("^C01C", "inotropes_and_vasopressors_postop"),
  make_med_event("^B02", "antihemorrhagics_postop")
)

cat("Building reoperation, ICU, and mortality outcomes ...\n")
reop_dt <- ops[anchor, on = .(subject_id, hadm_id), allow.cartesian = TRUE, nomatch = 0L][op_id != i.op_id]
reop_dt <- reop_dt[orin_time > i.orout_time & orin_time <= i.orout_time + WINDOW_30D]
if (nrow(reop_dt) > 0L) {
  reop_dt[, reoperation_interval_after_surgery_min := orin_time - i.orout_time]
  reop_dt <- reop_dt[order(i.op_id, reoperation_interval_after_surgery_min, orin_time)][, .SD[1], by = i.op_id][, .(op_id = i.op_id, reoperation = 1L, reoperation_interval_after_surgery_min)]
} else {
  reop_dt <- data.table(op_id = integer(), reoperation = integer(), reoperation_interval_after_surgery_min = numeric())
}

timeline_events <- anchor[, .(
  op_id,
  death_within_hospital_stay = as.integer(!is.na(inhosp_death_time) & inhosp_death_time > 0 & (is.na(discharge_time) | inhosp_death_time <= discharge_time)),
  death_within_30_days = as.integer(
    (!is.na(inhosp_death_time) & inhosp_death_time >= orout_time & inhosp_death_time - orout_time <= WINDOW_30D) |
      (!is.na(allcause_death_time) & allcause_death_time >= orout_time & allcause_death_time - orout_time <= WINDOW_30D)
  ),
  death_within_90_days = as.integer(!is.na(allcause_death_time) & allcause_death_time >= orout_time & allcause_death_time - orout_time <= WINDOW_90D),
  death_time_from_orout_min = fifelse(!is.na(allcause_death_time) & allcause_death_time >= orout_time, allcause_death_time - orout_time, fifelse(!is.na(inhosp_death_time) & inhosp_death_time >= orout_time, inhosp_death_time - orout_time, NA_real_)),
  unexpected_icu_admission_from_general_ward = as.integer(!is.na(icuin_time) & !is.na(orout_time) & (icuin_time - orout_time) > 20),
  interval_between_icu_in_and_orout_min = fifelse(!is.na(icuin_time) & !is.na(orout_time), icuin_time - orout_time, NA_real_),
  icu_stay = as.integer(!is.na(icuout_time) & !is.na(orout_time) & icuout_time > orout_time),
  icu_duration_min = fifelse(is.na(icuout_time) | is.na(orout_time) | icuout_time <= orout_time, NA_real_, fifelse(!is.na(icuin_time) & icuin_time >= orout_time, icuout_time - icuin_time, icuout_time - orout_time)),
  hospital_stay_duration_min = fifelse(!is.na(discharge_time) & !is.na(orout_time) & discharge_time >= orout_time, discharge_time - orout_time, NA_real_)
)]

cat("Calculating AKI from raw creatinine labs without imputation ...\n")
labs <- fread(file.path(raw_dir, "labs.csv"), select = c("subject_id", "chart_time", "item_name", "value"), na.strings = c("", "NA", "NULL", "(Null)", "null"))
labs[, `:=`(chart_time = num(chart_time), item_name = tolower(trimws(item_name)), value_num = num(value))]
cr <- labs[item_name == "creatinine" & !is.na(subject_id) & !is.na(chart_time) & !is.na(value_num) & value_num > 0]
rm(labs); gc()

pre_win <- anchor[!is.na(preop90_start) & !is.na(preop_end), .(subject_id, op_id, orin_time, start = preop90_start, end = preop_end)]
setkey(pre_win, subject_id, start, end)
cr_pre <- copy(cr)[, `:=`(start = chart_time, end = chart_time)]
setkey(cr_pre, subject_id, start, end)
pre_match <- foverlaps(cr_pre, pre_win, by.x = c("subject_id", "start", "end"), by.y = c("subject_id", "start", "end"), type = "within", nomatch = 0L)
pre_match <- pre_match[chart_time < orin_time]
if (nrow(pre_match) > 0L) {
  pre_match[, preop_gap_min := orin_time - chart_time]
  baseline_cr <- pre_match[order(op_id, preop_gap_min, -chart_time)][, .SD[1], by = op_id][, .(op_id, baseline_creatinine = value_num, baseline_creatinine_time = chart_time, baseline_creatinine_interval_before_surgery_min = orin_time - chart_time)]
} else {
  baseline_cr <- data.table(op_id = integer(), baseline_creatinine = numeric(), baseline_creatinine_time = numeric(), baseline_creatinine_interval_before_surgery_min = numeric())
}

post_win <- anchor[, .(subject_id, op_id, orout_time, start = postop7_start, end = postop7_end, end48 = postop48h_end)]
setkey(post_win, subject_id, start, end)
cr_post <- copy(cr)[, `:=`(start = chart_time, end = chart_time)]
setkey(cr_post, subject_id, start, end)
post_match <- foverlaps(cr_post, post_win, by.x = c("subject_id", "start", "end"), by.y = c("subject_id", "start", "end"), type = "within", nomatch = 0L)
post_match <- post_match[chart_time >= orout_time & chart_time <= end]
if (nrow(post_match) > 0L) {
  post_peak <- post_match[order(op_id, -value_num, chart_time)][, .SD[1], by = op_id][, .(op_id, peak_creatinine_postop_7d = value_num, peak_creatinine_postop_time = chart_time, peak_creatinine_interval_after_surgery_min = chart_time - orout_time)]
  post_peak48 <- post_match[chart_time <= end48][order(op_id, -value_num, chart_time)][, .SD[1], by = op_id][, .(op_id, peak_creatinine_postop_48h = value_num, peak_creatinine_postop_48h_time = chart_time, peak_creatinine_48h_interval_after_surgery_min = chart_time - orout_time)]
} else {
  post_peak <- data.table(op_id = integer(), peak_creatinine_postop_7d = numeric(), peak_creatinine_postop_time = numeric(), peak_creatinine_interval_after_surgery_min = numeric())
  post_peak48 <- data.table(op_id = integer(), peak_creatinine_postop_48h = numeric(), peak_creatinine_postop_48h_time = numeric(), peak_creatinine_48h_interval_after_surgery_min = numeric())
}
rm(cr, cr_pre, cr_post, pre_match, post_match); gc()

aki <- merge(anchor[, .(op_id)], baseline_cr, by = "op_id", all.x = TRUE)
aki <- merge(aki, post_peak, by = "op_id", all.x = TRUE)
aki <- merge(aki, post_peak48, by = "op_id", all.x = TRUE)
aki <- merge(aki, crrt_dt, by = "op_id", all.x = TRUE)
aki[is.na(crrt_postop), crrt_postop := 0L]
aki[, `:=`(
  aki_cr_evaluable = as.integer(!is.na(baseline_creatinine) & !is.na(peak_creatinine_postop_7d)),
  creatinine_ratio_7d = peak_creatinine_postop_7d / baseline_creatinine,
  creatinine_delta_7d = peak_creatinine_postop_7d - baseline_creatinine,
  creatinine_delta_48h = peak_creatinine_postop_48h - baseline_creatinine
)]
aki[, aki_category := NA_integer_]
aki[aki_cr_evaluable == 1L, aki_category := 0L]
aki[aki_cr_evaluable == 1L & (creatinine_delta_48h >= 0.3 | creatinine_ratio_7d >= 1.5), aki_category := pmax(aki_category, 1L, na.rm = TRUE)]
aki[aki_cr_evaluable == 1L & creatinine_ratio_7d >= 2, aki_category := pmax(aki_category, 2L, na.rm = TRUE)]
aki[aki_cr_evaluable == 1L & (creatinine_ratio_7d >= 3 | peak_creatinine_postop_7d >= 4), aki_category := pmax(aki_category, 3L, na.rm = TRUE)]
aki[crrt_postop == 1L, aki_category := 3L]
aki[, aki_creatinine := fifelse(is.na(aki_category), NA_integer_, as.integer(aki_category >= 1L))]
aki[, aki_source := fifelse(
  crrt_postop == 1L, "crrt",
  fifelse(is.na(aki_category), "not_evaluable",
          fifelse(aki_category == 0L, "none",
                  fifelse(aki_category == 3L & !is.na(peak_creatinine_postop_7d) & peak_creatinine_postop_7d >= 4, "creatinine_peak_ge_4",
                          fifelse(!is.na(creatinine_ratio_7d) & creatinine_ratio_7d >= 1.5, "creatinine_ratio_7d", "creatinine_delta_48h"))))
)]

cat("Merging output table ...\n")
final <- anchor[, .(op_id, subject_id, hadm_id, admission_time, orin_time, orout_time, discharge_time)]
merge_many <- function(base_dt, pieces) {
  out <- copy(base_dt)
  for (p in pieces) out <- merge(out, p, by = "op_id", all.x = TRUE)
  out
}
final <- merge_many(final, c(diag_events, list(preop_af, preop_va), ward_events[1:3], med_events, list(reop_dt, timeline_events, aki)))

# New-onset definitions.
for (cc in c("postop_af_raw", "preop_af_history", "ventricular_arrhythmia_raw", "preop_ventricular_arrhythmia_history")) {
  if (!cc %in% names(final)) final[, (cc) := 0L]
  set(final, which(is.na(final[[cc]])), cc, 0L)
}
final[, new_onset_af := as.integer(postop_af_raw == 1L & preop_af_history == 0L)]
final[, new_onset_af_interval_after_surgery_min := fifelse(new_onset_af == 1L, postop_af_raw_interval_after_surgery_min, NA_real_)]
final[, new_onset_ventricular_arrhythmia := as.integer(ventricular_arrhythmia_raw == 1L & preop_ventricular_arrhythmia_history == 0L)]
final[, new_onset_ventricular_arrhythmia_interval_after_surgery_min := fifelse(new_onset_ventricular_arrhythmia == 1L, ventricular_arrhythmia_raw_interval_after_surgery_min, NA_real_)]

# Nonfatal cardiac arrest: code event and no death on/before event time.
if (!"cardiac_arrest" %in% names(final)) final[, cardiac_arrest := 0L]
final[, nonfatal_cardiac_arrest := as.integer(cardiac_arrest == 1L & (is.na(death_time_from_orout_min) | is.na(cardiac_arrest_interval_after_surgery_min) | death_time_from_orout_min > cardiac_arrest_interval_after_surgery_min))]
final[, nonfatal_cardiac_arrest_interval_after_surgery_min := fifelse(nonfatal_cardiac_arrest == 1L, cardiac_arrest_interval_after_surgery_min, NA_real_)]

# Fill binary flags.
binary_cols <- unique(c(
  diag_defs$var, "new_onset_af", "new_onset_ventricular_arrhythmia", "nonfatal_cardiac_arrest",
  "ventilation_postop", "iabp_postop", "ecmo_postop", "reoperation", "death_within_hospital_stay",
  "death_within_30_days", "death_within_90_days", "unexpected_icu_admission_from_general_ward", "icu_stay",
  "antibiotic_escalation_vanc_or_carbapenem", "inotropes_and_vasopressors_postop", "antihemorrhagics_postop", "crrt_postop"
))
binary_cols <- setdiff(binary_cols, c("postop_af_raw", "ventricular_arrhythmia_raw"))
for (cc in binary_cols) {
  if (!cc %in% names(final)) final[, (cc) := 0L]
  set(final, which(is.na(final[[cc]])), cc, 0L)
  final[, (cc) := as.integer(get(cc))]
}

# Composites aligned to the document.
has_any <- function(dt, cols) {
  present <- intersect(cols, names(dt))
  if (length(present) == 0L) return(rep(0L, nrow(dt)))
  as.integer(rowSums(dt[, ..present] == 1L, na.rm = TRUE) > 0L)
}
final[, any_stroke := has_any(.SD, c("cerebral_infarction", "tia", "intracerebral_hemorrhage", "subarachnoid_hemorrhage", "subdural_hemorrhage"))]
final[, intracranial_hemorrhage := has_any(.SD, c("intracerebral_hemorrhage", "subarachnoid_hemorrhage", "subdural_hemorrhage"))]
final[, ischemic_cerebrovascular_event := has_any(.SD, c("cerebral_infarction", "tia"))]
final[, postoperative_pulmonary_complication := has_any(.SD, c("pneumonia", "respiratory_failure", "pleural_effusion", "atelectasis", "pneumothorax", "bronchospasm", "aspiration_pneumonitis", "pulmonary_edema", "ards", "tracheobronchitis", "exacerbation_chronic_lung_disease"))]
final[, postoperative_infectious_complication := has_any(.SD, c("pneumonia", "surgical_site_infection", "sepsis", "peritonitis", "urinary_tract_infection"))]
final[, major_cardiovascular_event := has_any(.SD, c("acute_myocardial_infarction", "nonfatal_cardiac_arrest", "pulmonary_embolism", "any_stroke", "death_within_30_days"))]
final[, major_postoperative_complication := has_any(.SD, c("major_cardiovascular_event", "postoperative_pulmonary_complication", "postoperative_infectious_complication", "aki_creatinine", "reoperation", "icu_stay"))]

# Create composite intervals as earliest component interval where available.
earliest_interval <- function(.SD) {
  if (.N == 0L) return(numeric())
  do.call(pmin, c(.SD, list(na.rm = TRUE)))
}
composite_interval <- function(dt, cols) {
  present <- intersect(cols, names(dt))
  if (length(present) == 0L) return(rep(NA_real_, nrow(dt)))
  mat <- as.data.table(dt[, ..present])
  ans <- do.call(pmin, c(mat, list(na.rm = TRUE)))
  ans[is.infinite(ans)] <- NA_real_
  ans
}
final[, any_stroke_interval_after_surgery_min := composite_interval(.SD, c("cerebral_infarction_interval_after_surgery_min", "tia_interval_after_surgery_min", "intracerebral_hemorrhage_interval_after_surgery_min", "subarachnoid_hemorrhage_interval_after_surgery_min", "subdural_hemorrhage_interval_after_surgery_min"))]
final[, ischemic_cerebrovascular_event_interval_after_surgery_min := composite_interval(.SD, c("cerebral_infarction_interval_after_surgery_min", "tia_interval_after_surgery_min"))]
final[, intracranial_hemorrhage_interval_after_surgery_min := composite_interval(.SD, c("intracerebral_hemorrhage_interval_after_surgery_min", "subarachnoid_hemorrhage_interval_after_surgery_min", "subdural_hemorrhage_interval_after_surgery_min"))]
final[, postoperative_pulmonary_complication_interval_after_surgery_min := composite_interval(.SD, c("pneumonia_interval_after_surgery_min", "respiratory_failure_interval_after_surgery_min", "pleural_effusion_interval_after_surgery_min", "atelectasis_interval_after_surgery_min", "pneumothorax_interval_after_surgery_min", "bronchospasm_interval_after_surgery_min", "aspiration_pneumonitis_interval_after_surgery_min", "pulmonary_edema_interval_after_surgery_min", "ards_interval_after_surgery_min", "tracheobronchitis_interval_after_surgery_min", "exacerbation_chronic_lung_disease_interval_after_surgery_min"))]
final[, postoperative_infectious_complication_interval_after_surgery_min := composite_interval(.SD, c("pneumonia_interval_after_surgery_min", "surgical_site_infection_interval_after_surgery_min", "sepsis_interval_after_surgery_min", "peritonitis_interval_after_surgery_min", "urinary_tract_infection_interval_after_surgery_min"))]
final[, major_cardiovascular_event_interval_after_surgery_min := composite_interval(.SD, c("acute_myocardial_infarction_interval_after_surgery_min", "nonfatal_cardiac_arrest_interval_after_surgery_min", "pulmonary_embolism_interval_after_surgery_min", "any_stroke_interval_after_surgery_min", "death_time_from_orout_min"))]
final[, major_postoperative_complication_interval_after_surgery_min := composite_interval(.SD, c("major_cardiovascular_event_interval_after_surgery_min", "postoperative_pulmonary_complication_interval_after_surgery_min", "postoperative_infectious_complication_interval_after_surgery_min", "peak_creatinine_interval_after_surgery_min", "reoperation_interval_after_surgery_min", "icu_duration_min"))]

# Convert minute interval columns to day columns for final output, matching legacy naming style.
min_cols <- grep("_min$", names(final), value = TRUE)
for (cc in min_cols) final[, (sub("_min$", "_day", cc)) := num(get(cc)) / 1440]
final[, (min_cols) := NULL]

# Remove raw helper flags but keep history flags for auditability.
drop_cols <- intersect(c("postop_af_raw", "ventricular_arrhythmia_raw"), names(final))
if (length(drop_cols) > 0L) final[, (drop_cols) := NULL]
setorderv(final, c("subject_id", "hadm_id", "op_id"))
fwrite(final, out_defined)

cat("Building summary and dictionary ...\n")
summary_vars <- unique(c(
  "major_postoperative_complication", "major_cardiovascular_event", "postoperative_pulmonary_complication", "postoperative_infectious_complication",
  "death_within_hospital_stay", "death_within_30_days", "death_within_90_days", "reoperation", "icu_stay", "unexpected_icu_admission_from_general_ward",
  "acute_myocardial_infarction", "angina", "nonfatal_cardiac_arrest", "new_onset_af", "heart_failure", "new_onset_ventricular_arrhythmia",
  "any_stroke", "ischemic_cerebrovascular_event", "intracranial_hemorrhage", "cerebral_infarction", "tia", "hemiplegia", "paraplegia",
  "acute_kidney_failure_icd10", "aki_creatinine", "crrt_postop",
  "pneumonia", "respiratory_failure", "ards", "pleural_effusion", "atelectasis", "pneumothorax", "bronchospasm", "aspiration_pneumonitis", "pulmonary_edema", "tracheobronchitis", "exacerbation_chronic_lung_disease",
  "surgical_site_infection", "sepsis", "peritonitis", "urinary_tract_infection",
  "ventilation_postop", "iabp_postop", "ecmo_postop",
  "cerebrospinal_fluid_leak", "intestinal_ischemia", "ileus", "hepatic_failure", "acute_pancreatitis", "gastrointestinal_hemorrhage", "pulmonary_embolism", "shock", "dic",
  "antibiotic_escalation_vanc_or_carbapenem", "inotropes_and_vasopressors_postop", "antihemorrhagics_postop"
))
summary_vars <- intersect(summary_vars, names(final))
summary <- rbindlist(lapply(summary_vars, function(v) {
  day_col <- switch(v,
    death_within_hospital_stay = "death_time_from_orout_day",
    death_within_30_days = "death_time_from_orout_day",
    death_within_90_days = "death_time_from_orout_day",
    unexpected_icu_admission_from_general_ward = "interval_between_icu_in_and_orout_day",
    icu_stay = "icu_duration_day",
    aki_creatinine = "peak_creatinine_interval_after_surgery_day",
    paste0(v, "_interval_after_surgery_day")
  )
  interval <- if (day_col %in% names(final)) final[[day_col]] else rep(NA_real_, nrow(final))
  flag <- final[[v]]
  data.table(
    outcome = v,
    n_cases = sum(flag == 1L, na.rm = TRUE),
    total_ops = nrow(final),
    prevalence_pct = round(100 * sum(flag == 1L, na.rm = TRUE) / nrow(final), 3),
    median_interval_day = round(median(interval[flag == 1L], na.rm = TRUE), 3),
    p25_interval_day = round(quantile(interval[flag == 1L], 0.25, na.rm = TRUE, names = FALSE), 3),
    p75_interval_day = round(quantile(interval[flag == 1L], 0.75, na.rm = TRUE, names = FALSE), 3)
  )
}), use.names = TRUE)
for (cc in c("median_interval_day", "p25_interval_day", "p75_interval_day")) set(summary, which(!is.finite(summary[[cc]])), cc, NA_real_)
fwrite(summary, out_summary)

dict <- rbindlist(list(
  diag_defs[, .(variable = var, source = "diagnosis.csv", window = "0-30 days after orout_time", definition)],
  data.table(variable = c("aki_creatinine", "aki_category", "crrt_postop"), source = c("labs.csv + ward_vitals.csv", "labs.csv + ward_vitals.csv", "ward_vitals.csv"), window = c("SCr baseline 90d preop; postop peak 7d; CRRT 7d", "SCr baseline 90d preop; postop peak 7d; CRRT 7d", "0-7 days after orout_time"), definition = c("KDIGO SCr-only AKI without imputation; CRRT upgrades to stage 3", "KDIGO stage 0/1/2/3, NA if not evaluable and no CRRT", "Postoperative CRRT device/support flag")),
  data.table(variable = c("ventilation_postop", "iabp_postop", "ecmo_postop", "reoperation", "death_within_hospital_stay", "death_within_30_days", "death_within_90_days"), source = c("ward_vitals.csv", "ward_vitals.csv", "ward_vitals.csv", "operations.csv", "operations.csv", "operations.csv", "operations.csv"), window = c("0-30 days", "0-30 days", "0-30 days", "0-30 days", "hospital stay", "0-30 days", "0-90 days"), definition = c("ward_vitals vent==1", "ward_vitals iabp==1", "ward_vitals ecmo==1", "Any later operation in same admission", "Death before discharge", "Death within 30 days after OR exit", "Death within 90 days after OR exit"))
), fill = TRUE)
fwrite(dict, out_dictionary)

cat("Done.\n")
cat("Defined output: ", out_defined, "\n", sep = "")
cat("Summary output: ", out_summary, "\n", sep = "")
cat("Rows: ", nrow(final), ", Cols: ", ncol(final), "\n", sep = "")
cat("AKI evaluable: ", sum(final$aki_cr_evaluable == 1L, na.rm = TRUE), "\n", sep = "")
