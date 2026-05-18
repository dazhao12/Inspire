#!/usr/bin/env Rscript
# Timestamp: 2026-05-17T11:30:00Z

suppressPackageStartupMessages({
  library(data.table)
})

path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/01_raw/"
out_file <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/comorbidity_defined_latest.csv"

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

any_regex <- function(x, pattern) grepl(pattern, x, perl = TRUE)

atc_any_match <- function(dt, pattern) {
  as.integer(
    any_regex(dt$atc_code, pattern) |
      any_regex(dt$atc_code2, pattern) |
      any_regex(dt$atc_code3, pattern)
  )
}

calc_egfr_word_formula <- function(scr_mg_dl, age_years, sex_char) {
  ifelse(
    is.na(scr_mg_dl) | is.na(age_years) | is.na(sex_char) | scr_mg_dl <= 0 | age_years <= 0,
    NA_real_,
    175 * (scr_mg_dl ^ -1.154) * (age_years ^ -0.203) * ifelse(sex_char == "F", 0.742, 1.0)
  )
}

cat("Loading all operations (no first non-MAC filter) ...\n")
ops <- fread(
  file.path(path_raw, "operations.csv"),
  select = c(
    "subject_id", "hadm_id", "op_id", "asa", "age", "sex",
    "weight", "height", "orin_time"
  ),
  na.strings = c("", "NA")
)
ops[, `:=`(
  age = as.numeric(age),
  weight = as.numeric(weight),
  height = as.numeric(height),
  orin_time = as.numeric(orin_time),
  Male = fifelse(sex == "M", 1L, fifelse(sex == "F", 0L, NA_integer_))
)]

ops[, `:=`(weight_clean = weight, height_clean = height)]
ops[!is.na(age) & age >= 18 & !is.na(height_clean) & (height_clean < 100 | height_clean > 250), height_clean := NA_real_]
ops[!is.na(age) & age >= 18 & !is.na(weight_clean) & (weight_clean < 30 | weight_clean > 300), weight_clean := NA_real_]
ops[!is.na(age) & age < 18 & !is.na(height_clean) & (height_clean < 40 | height_clean > 250), height_clean := NA_real_]
ops[!is.na(age) & age < 18 & !is.na(weight_clean) & (weight_clean < 1.5 | weight_clean > 200), weight_clean := NA_real_]
ops[, BMI := fifelse(!is.na(height_clean) & height_clean > 0 & !is.na(weight_clean), weight_clean / ((height_clean / 100)^2), NA_real_)]
ops[!is.na(BMI) & (BMI < 10 | BMI > 100), BMI := NA_real_]

ops <- unique(ops[!is.na(subject_id) & !is.na(op_id) & !is.na(orin_time)])
anchor_index <- ops[, .(subject_id, hadm_id, op_id, orin_time, asa, age, sex, Male, BMI)]
setorderv(anchor_index, c("subject_id", "hadm_id", "op_id"))

cat("Loading diagnosis.csv ...\n")
diag <- fread(
  file.path(path_raw, "diagnosis.csv"),
  select = c("subject_id", "chart_time", "icd10_cm"),
  na.strings = c("", "NA")
)
diag <- diag[subject_id %in% anchor_index$subject_id]
diag[, `:=`(
  chart_time = as.numeric(chart_time),
  icd_clean = gsub("\\.", "", toupper(trimws(icd10_cm)))
)]
diag[, icd3 := substr(icd_clean, 1, 3)]
diag <- diag[!is.na(subject_id) & !is.na(chart_time) & !is.na(icd3) & nchar(icd3) == 3]

cat("Flagging diagnosis-based comorbidity definitions ...\n")
code_num <- suppressWarnings(as.integer(substr(diag$icd3, 2, 3)))
code_letter <- substr(diag$icd3, 1, 1)
diag[, `:=`(
  hypertension_dx = as.integer(any_regex(icd3, "^I1[0-6]$")),
  ischemic_heart_disease = as.integer(any_regex(icd3, "^I2[0-5]$")),
  heart_failure = as.integer(icd3 %chin% c("I42", "I43", "I50")),
  arrhythmia_dx = as.integer(any_regex(icd3, "^I4[7-9]$")),
  atrial_fibrillation_flutter = as.integer(icd3 == "I48"),
  peripheral_vascular_disease = as.integer(icd3 %chin% c("I70", "I71", "I73", "K55")),
  cerebrovascular_disease = as.integer(any_regex(icd3, "^I6[0-9]$") | icd3 %chin% c("G45", "G46")),
  dementia = as.integer(icd3 %chin% c("F01", "F02", "F03", "F05", "G30", "G31")),
  parkinsonism = as.integer(icd3 %chin% c("G20", "G21", "G22")),
  copd = as.integer(icd3 %chin% c("J41", "J42", "J43", "J44")),
  asthma = as.integer(icd3 == "J45"),
  renal_disease = as.integer(any_regex(icd3, "^N0[3-8]$") | icd3 %chin% c("N18", "N19", "I12", "I13", "Z49", "Z94", "Z99")),
  renal_dialysis = as.integer(icd3 == "Z49"),
  chronic_liver_disease = as.integer(icd3 == "B18" | any_regex(icd3, "^K7[0-7]$")),
  peptic_ulcer_disease = as.integer(any_regex(icd3, "^K2[5-8]$")),
  gerd = as.integer(icd3 == "K21"),
  obesity_icd = as.integer(icd3 == "E66"),
  diabetes_dx = as.integer(any_regex(icd3, "^E1[0-4]$")),
  hyperlipidemia_dx = as.integer(icd3 == "E78"),
  anemia_icd_only = as.integer(any_regex(icd3, "^D(5[0-9]|6[0-4])$")),
  connective_tissue_disease = as.integer(icd3 %chin% c("M05", "M06", "M31", "M32", "M33", "M34", "M35")),
  malignancy = as.integer(code_letter == "C" & ((!is.na(code_num) & code_num >= 0L & code_num <= 76L) | (!is.na(code_num) & code_num >= 81L & code_num <= 96L)))
)]

cat("Loading medications.csv ...\n")
meds <- fread(
  file.path(path_raw, "medications.csv"),
  select = c("subject_id", "chart_time", "atc_code", "atc_code2", "atc_code3"),
  na.strings = c("", "NA")
)
meds <- meds[subject_id %in% anchor_index$subject_id]
meds[, `:=`(
  chart_time = as.numeric(chart_time),
  atc_code = toupper(trimws(fifelse(is.na(atc_code), "", atc_code))),
  atc_code2 = toupper(trimws(fifelse(is.na(atc_code2), "", atc_code2))),
  atc_code3 = toupper(trimws(fifelse(is.na(atc_code3), "", atc_code3)))
)]
meds <- meds[!is.na(subject_id) & !is.na(chart_time)]
meds[, `:=`(
  hypertension_med = atc_any_match(.SD, "^C02|^C08C"),
  arrhythmia_med = atc_any_match(.SD, "^C01B|^C08D|^C07A"),
  diabetes_med = atc_any_match(.SD, "^A10"),
  diabetes_insulin_med = atc_any_match(.SD, "^A10A"),
  hyperlipidemia_med = atc_any_match(.SD, "^C10A")
)]

cat("Aggregating diagnosis and medication histories before OR-in time ...\n")
setkey(anchor_index, subject_id, orin_time)
setkey(diag, subject_id, chart_time)
setkey(meds, subject_id, chart_time)

diag_flag_cols <- c(
  "hypertension_dx", "ischemic_heart_disease", "heart_failure", "arrhythmia_dx",
  "atrial_fibrillation_flutter", "peripheral_vascular_disease", "cerebrovascular_disease",
  "dementia", "parkinsonism", "copd", "asthma", "renal_disease", "renal_dialysis",
  "chronic_liver_disease", "peptic_ulcer_disease", "gerd", "obesity_icd", "diabetes_dx",
  "hyperlipidemia_dx", "anemia_icd_only", "connective_tissue_disease", "malignancy"
)

diag_agg <- diag[
  anchor_index,
  on = .(subject_id, chart_time < orin_time),
  allow.cartesian = TRUE,
  nomatch = NA,
  c(
    list(
      hadm_id = i.hadm_id,
      op_id = i.op_id,
      asa = i.asa,
      age = i.age,
      sex = i.sex,
      Male = i.Male,
      BMI = i.BMI,
      diagnosis_records_preop = sum(!is.na(icd3))
    ),
    lapply(.SD, function(v) as.integer(any(v == 1L, na.rm = TRUE)))
  ),
  by = .EACHI,
  .SDcols = diag_flag_cols
]

med_flag_cols <- c("hypertension_med", "arrhythmia_med", "diabetes_med", "diabetes_insulin_med", "hyperlipidemia_med")
med_agg <- meds[
  anchor_index,
  on = .(subject_id, chart_time < orin_time),
  allow.cartesian = TRUE,
  nomatch = NA,
  c(
    list(
      op_id = i.op_id,
      medication_records_preop = sum(
        (!is.na(atc_code) & atc_code != "") |
          (!is.na(atc_code2) & atc_code2 != "") |
          (!is.na(atc_code3) & atc_code3 != "")
      )
    ),
    lapply(.SD, function(v) as.integer(any(v == 1L, na.rm = TRUE)))
  ),
  by = .EACHI,
  .SDcols = med_flag_cols
]

cat("Loading Hb and creatinine labs ...\n")
labs <- fread(
  file.path(path_raw, "labs.csv"),
  select = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
labs <- labs[subject_id %in% anchor_index$subject_id & item_name %chin% c("hb", "creatinine")]
labs[, `:=`(chart_time = as.numeric(chart_time), value = suppressWarnings(as.numeric(value)))]
labs <- labs[!is.na(subject_id) & !is.na(chart_time) & !is.na(value)]

window_index <- anchor_index[, .(
  subject_id,
  anchor_op_id = op_id,
  anchor_orin_time = orin_time,
  anchor_start_time = orin_time - 90 * 24 * 60
)]

labs_hb <- labs[item_name == "hb"]
hb_join <- labs_hb[window_index, on = .(subject_id), allow.cartesian = TRUE, nomatch = 0L]
hb_join <- hb_join[chart_time >= anchor_start_time & chart_time < anchor_orin_time]
hb_agg <- if (nrow(hb_join) > 0L) {
  hb_join[, {
    vals <- value[!is.na(value)]
    times <- chart_time[!is.na(value)]
    .(
      hb_n_preop = length(vals),
      hb_last_g_dl = if (length(times) > 0L) vals[which.max(times)] else NA_real_,
      hb_min_g_dl = if (length(vals) > 0L) min(vals) else NA_real_
    )
  }, by = .(op_id = anchor_op_id)]
} else {
  data.table(op_id = integer(), hb_n_preop = integer(), hb_last_g_dl = numeric(), hb_min_g_dl = numeric())
}

labs_cr <- labs[item_name == "creatinine"]
cr_join <- labs_cr[window_index, on = .(subject_id), allow.cartesian = TRUE, nomatch = 0L]
cr_join <- cr_join[chart_time >= anchor_start_time & chart_time < anchor_orin_time]
cr_agg <- if (nrow(cr_join) > 0L) {
  cr_join[, {
    vals <- value[!is.na(value)]
    times <- chart_time[!is.na(value)]
    if (length(vals) > 0L) {
      ord <- order(times, decreasing = TRUE)
      recent <- vals[ord][seq_len(min(2L, length(vals)))]
      cr_mean_recent <- mean(recent)
      cr_last <- vals[which.max(times)]
    } else {
      cr_mean_recent <- NA_real_
      cr_last <- NA_real_
    }
    .(
      creatinine_90d_n = length(vals),
      creatinine_last_mg_dl = cr_last,
      creatinine_recent_1to2_mean_90d_mg_dl = cr_mean_recent
    )
  }, by = .(op_id = anchor_op_id)]
} else {
  data.table(op_id = integer(), creatinine_90d_n = integer(), creatinine_last_mg_dl = numeric(), creatinine_recent_1to2_mean_90d_mg_dl = numeric())
}

cat("Building final comorbidity table ...\n")
final_dt <- merge(anchor_index, diag_agg, by = c("subject_id", "hadm_id", "op_id", "asa", "age", "sex", "Male", "BMI"), all.x = TRUE)
final_dt <- merge(final_dt, med_agg[, c("op_id", "medication_records_preop", med_flag_cols), with = FALSE], by = "op_id", all.x = TRUE)
final_dt <- merge(final_dt, hb_agg, by = "op_id", all.x = TRUE)
final_dt <- merge(final_dt, cr_agg, by = "op_id", all.x = TRUE)

for (j in c(diag_flag_cols, med_flag_cols)) {
  if (j %in% names(final_dt)) set(final_dt, which(is.na(final_dt[[j]])), j, 0L)
}
for (j in c("diagnosis_records_preop", "medication_records_preop", "hb_n_preop", "creatinine_90d_n")) {
  if (j %in% names(final_dt)) set(final_dt, which(is.na(final_dt[[j]])), j, 0L)
}

final_dt[, `:=`(
  hypertension = as.integer(hypertension_dx == 1L | hypertension_med == 1L),
  arrhythmia = as.integer(arrhythmia_dx == 1L | arrhythmia_med == 1L),
  obesity = as.integer(obesity_icd == 1L | (!is.na(BMI) & BMI >= 30)),
  diabetes = as.integer(diabetes_dx == 1L | diabetes_med == 1L),
  hyperlipidemia = as.integer(hyperlipidemia_dx == 1L | hyperlipidemia_med == 1L)
)]

final_dt[, diabetes_category := fifelse(diabetes == 1L & diabetes_insulin_med == 1L, 1L, fifelse(diabetes == 1L, 2L, NA_integer_))]
final_dt[, anemia_icd10 := as.integer(
  anemia_icd_only == 1L |
    (!is.na(hb_last_g_dl) & ((Male == 1L & hb_last_g_dl < 13) | (Male == 0L & hb_last_g_dl < 12)))
)]
final_dt[, anemia_preoperative := as.integer(!is.na(hb_last_g_dl) & ((Male == 1L & hb_last_g_dl < 13) | (Male == 0L & hb_last_g_dl < 12)))]
final_dt[, anemia_preop_severity := fifelse(
  anemia_preoperative != 1L,
  NA_integer_,
  fifelse(hb_last_g_dl >= 10, 1L, fifelse(hb_last_g_dl >= 7, 2L, 3L))
)]

final_dt[, egfr_word_formula := calc_egfr_word_formula(creatinine_recent_1to2_mean_90d_mg_dl, age, sex)]
final_dt[, egfr_stage_90d := fifelse(
  is.na(egfr_word_formula),
  NA_integer_,
  fifelse(egfr_word_formula >= 90, 1L, fifelse(egfr_word_formula >= 60, 2L, fifelse(egfr_word_formula >= 30, 3L, fifelse(egfr_word_formula >= 15, 4L, 5L))))
)]
final_dt[, renal_disease_category := egfr_stage_90d]
final_dt[renal_dialysis == 1L, renal_disease_category := 5L]

keep_cols <- c(
  "subject_id", "hadm_id", "op_id", "asa", "age", "sex", "Male", "BMI",
  "hypertension", "ischemic_heart_disease", "heart_failure", "arrhythmia",
  "atrial_fibrillation_flutter", "peripheral_vascular_disease", "cerebrovascular_disease",
  "dementia", "parkinsonism", "copd", "asthma", "renal_disease", "renal_disease_category",
  "renal_dialysis", "chronic_liver_disease", "peptic_ulcer_disease", "gerd", "obesity",
  "diabetes", "diabetes_category", "hyperlipidemia", "anemia_icd10", "anemia_preoperative",
  "anemia_preop_severity", "connective_tissue_disease", "malignancy", "hb_last_g_dl",
  "hb_min_g_dl", "creatinine_last_mg_dl", "creatinine_recent_1to2_mean_90d_mg_dl",
  "egfr_word_formula", "egfr_stage_90d"
)

final_out <- final_dt[, ..keep_cols]
final_out[, BMI := round(BMI, 1)]
setorderv(final_out, c("subject_id", "hadm_id", "op_id"))
fwrite(final_out, out_file)

cat("Done. Wrote: ", out_file, "\n", sep = "")
cat("Rows: ", nrow(final_out), ", Cols: ", ncol(final_out), "\n", sep = "")
