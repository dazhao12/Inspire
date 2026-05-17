#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

# ==============================================================================
# 0. Paths
# ==============================================================================
path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
path_processed_base <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_folder_name <- "Diagnosis_word_comorbidities_first_nonMAC_3_30_2026"
path_output <- file.path(path_processed_base, output_folder_name)

if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
}

# ==============================================================================
# 1. Load anchor operations: first non-MAC per subject_id + hadm_id
# ==============================================================================
cat("Loading operations and selecting first non-MAC anchor surgery per admission...\n")

ops <- fread(
  file.path(path_raw, "operations.csv"),
  select = c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate",
    "age", "sex", "weight", "height", "asa", "antype",
    "admission_time", "orin_time", "opstart_time", "anstart_time"
  ),
  na.strings = c("", "NA")
)

ops[, `:=`(
  age = as.numeric(age),
  weight = as.numeric(weight),
  height = as.numeric(height),
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time),
  opstart_time = as.numeric(opstart_time),
  anstart_time = as.numeric(anstart_time),
  opdate_num = suppressWarnings(as.numeric(opdate)),
  antype_clean = toupper(trimws(as.character(antype))),
  Male = fifelse(sex == "M", 1L, fifelse(sex == "F", 0L, NA_integer_)),
  BMI = fifelse(!is.na(height) & height > 0, weight / ((height / 100)^2), NA_real_)
)]

ops[, anchor_sort_time := fcoalesce(opstart_time, anstart_time, orin_time, opdate_num, admission_time)]
ops[, hadm_group := fifelse(is.na(hadm_id), paste0("MISSING_HADM_", op_id), as.character(hadm_id))]

anchor_ops <- ops[
  !is.na(subject_id) & !is.na(op_id) & !is.na(orin_time) & !is.na(antype_clean) & antype_clean != "MAC"
][order(subject_id, hadm_group, anchor_sort_time, op_id)][
  , .SD[1], by = .(subject_id, hadm_group)
]

setorderv(anchor_ops, c("subject_id", "hadm_id", "op_id"))

anchor_map <- anchor_ops[, .(
  subject_id, hadm_id, op_id, case_id, orin_time,
  antype, asa, age, sex, weight, height, BMI
)]
fwrite(anchor_map, file.path(path_output, "anchor_first_nonMAC_operations.csv"))

cat(sprintf("Anchor operations kept: %d\n", nrow(anchor_ops)))

# ==============================================================================
# 2. Load and clean diagnosis / medications / labs
# ==============================================================================
cat("Loading diagnosis.csv ...\n")
diag <- fread(
  file.path(path_raw, "diagnosis.csv"),
  select = c("subject_id", "chart_time", "icd10_cm"),
  na.strings = c("", "NA")
)
diag <- diag[subject_id %in% anchor_ops$subject_id]
diag[, `:=`(
  chart_time = as.numeric(chart_time),
  icd_clean = gsub("\\.", "", toupper(trimws(icd10_cm))),
  icd3 = substr(gsub("\\.", "", toupper(trimws(icd10_cm))), 1, 3)
)]
diag <- diag[!is.na(subject_id) & !is.na(chart_time) & !is.na(icd3) & nchar(icd3) == 3]

cat("Loading medications.csv ...\n")
meds <- fread(
  file.path(path_raw, "medications.csv"),
  select = c("subject_id", "chart_time", "atc_code", "atc_code2", "atc_code3"),
  na.strings = c("", "NA")
)
meds <- meds[subject_id %in% anchor_ops$subject_id]
meds[, `:=`(
  chart_time = as.numeric(chart_time),
  atc_code = toupper(trimws(fifelse(is.na(atc_code), "", atc_code))),
  atc_code2 = toupper(trimws(fifelse(is.na(atc_code2), "", atc_code2))),
  atc_code3 = toupper(trimws(fifelse(is.na(atc_code3), "", atc_code3)))
)]
meds <- meds[!is.na(subject_id) & !is.na(chart_time)]

cat("Loading labs.csv ...\n")
labs <- fread(
  file.path(path_raw, "labs.csv"),
  select = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
labs <- labs[subject_id %in% anchor_ops$subject_id & item_name %chin% c("hb", "creatinine")]
labs[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]
labs <- labs[!is.na(subject_id) & !is.na(chart_time) & !is.na(value)]

# ==============================================================================
# 3. Row-level flags for Word-defined comorbidities
# ==============================================================================
cat("Building row-level ICD and ATC flags...\n")

code_num <- suppressWarnings(as.integer(substr(diag$icd3, 2, 3)))
code_letter <- substr(diag$icd3, 1, 1)

diag[, `:=`(
  hypertension_dx = as.integer(str_detect(icd3, "^I1[0-6]$")),
  ischemic_heart_disease = as.integer(str_detect(icd3, "^I2[0-5]$")),
  heart_failure = as.integer(icd3 %chin% c("I42", "I43", "I50")),
  arrhythmia_dx = as.integer(str_detect(icd3, "^I4[7-9]$")),
  atrial_fibrillation_flutter = as.integer(icd3 == "I48"),
  pulmonary_hypertension = as.integer(icd3 == "I27"),
  peripheral_vascular_disease = as.integer(icd3 %chin% c("I70", "I71", "I73", "K55")),
  cerebrovascular_disease = as.integer(str_detect(icd3, "^I6[0-9]$") | icd3 %chin% c("G45", "G46")),
  dementia = as.integer(icd3 %chin% c("F01", "F02", "F03", "F05", "G30", "G31")),
  parkinsonism = as.integer(icd3 %chin% c("G20", "G21", "G22")),
  copd = as.integer(icd3 %chin% c("J41", "J42", "J43", "J44")),
  asthma = as.integer(icd3 == "J45"),
  renal_disease = as.integer(str_detect(icd3, "^N0[3-8]$") | icd3 %chin% c("N18", "N19", "I12", "I13", "Z49", "Z94", "Z99")),
  renal_dialysis = as.integer(icd3 == "Z49"),
  chronic_liver_disease = as.integer(icd3 == "B18" | str_detect(icd3, "^K7[0-7]$")),
  peptic_ulcer_disease = as.integer(str_detect(icd3, "^K2[5-8]$")),
  gerd = as.integer(icd3 == "K21"),
  obesity_icd = as.integer(icd3 == "E66"),
  diabetes_dx = as.integer(str_detect(icd3, "^E1[0-4]$")),
  hyperlipidemia_dx = as.integer(icd3 == "E78"),
  anemia_icd_only = as.integer(str_detect(icd3, "^D(5[0-9]|6[0-4])$")),
  connective_tissue_disease = as.integer(icd3 %chin% c("M05", "M06", "M31", "M32", "M33", "M34", "M35")),
  malignancy = as.integer(code_letter == "C" & ((!is.na(code_num) & code_num >= 0L & code_num <= 76L) | (!is.na(code_num) & code_num >= 81L & code_num <= 96L)))
)]

atc_any_match <- function(prefix_regex) {
  as.integer(
    str_detect(meds$atc_code, prefix_regex) |
      str_detect(meds$atc_code2, prefix_regex) |
      str_detect(meds$atc_code3, prefix_regex)
  )
}

meds[, `:=`(
  hypertension_med = atc_any_match("^C02|^C08C"),
  arrhythmia_med = atc_any_match("^C01B|^C08D|^C07A"),
  diabetes_med = atc_any_match("^A10"),
  diabetes_insulin_med = atc_any_match("^A10A"),
  hyperlipidemia_med = atc_any_match("^C10A")
)]

# ==============================================================================
# 4. Aggregate diagnosis and medications before anchor orin_time
# ==============================================================================
cat("Aggregating diagnosis and medication histories before anchor OR-in time...\n")

anchor_index <- anchor_ops[, .(
  subject_id, hadm_id, op_id, orin_time,
  asa, age, sex, Male, BMI
)]
setkey(anchor_index, subject_id, orin_time)
setkey(diag, subject_id, chart_time)
setkey(meds, subject_id, chart_time)

diag_flag_cols <- c(
  "hypertension_dx", "ischemic_heart_disease", "heart_failure", "arrhythmia_dx",
  "atrial_fibrillation_flutter", "pulmonary_hypertension", "peripheral_vascular_disease",
  "cerebrovascular_disease", "dementia", "parkinsonism", "copd", "asthma",
  "renal_disease", "renal_dialysis", "chronic_liver_disease", "peptic_ulcer_disease",
  "gerd", "obesity_icd", "diabetes_dx", "hyperlipidemia_dx", "anemia_icd_only",
  "connective_tissue_disease", "malignancy"
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

med_flag_cols <- c(
  "hypertension_med", "arrhythmia_med", "diabetes_med",
  "diabetes_insulin_med", "hyperlipidemia_med"
)

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

# ==============================================================================
# 5. Aggregate labs for Hb and creatinine
# ==============================================================================
cat("Aggregating preop Hb and creatinine...\n")

labs_hb <- labs[item_name == "hb"]
setkey(labs_hb, subject_id, chart_time)

hb_agg <- labs_hb[
  anchor_index,
  on = .(subject_id, chart_time < orin_time),
  allow.cartesian = TRUE,
  nomatch = NA,
  {
    hb_values <- value[!is.na(value)]
    hb_times <- chart_time[!is.na(value)]
    last_hb <- if (length(hb_times) > 0L) hb_values[which.max(hb_times)] else NA_real_
    min_hb <- if (length(hb_values) > 0L) min(hb_values) else NA_real_
    .(
      op_id = i.op_id,
      hb_n_preop = length(hb_values),
      hb_last_g_dl = last_hb,
      hb_min_g_dl = min_hb
    )
  },
  by = .EACHI
]

labs_cr <- labs[item_name == "creatinine"]
cr_window_index <- anchor_index[, .(
  subject_id, op_id, orin_time,
  start_time = orin_time - 90 * 24 * 60,
  age, sex
)]
setkey(labs_cr, subject_id, chart_time)

cr_agg <- labs_cr[
  cr_window_index,
  on = .(subject_id, chart_time >= start_time, chart_time < orin_time),
  allow.cartesian = TRUE,
  nomatch = NA,
  {
    cr_values <- value[!is.na(value)]
    cr_times <- chart_time[!is.na(value)]
    if (length(cr_values) > 0L) {
      ord <- order(cr_times, decreasing = TRUE)
      cr_recent <- cr_values[ord][seq_len(min(2L, length(cr_values)))]
      cr_mean_recent <- mean(cr_recent)
      cr_last <- cr_values[which.max(cr_times)]
    } else {
      cr_mean_recent <- NA_real_
      cr_last <- NA_real_
    }
    .(
      op_id = i.op_id,
      creatinine_90d_n = length(cr_values),
      creatinine_last_mg_dl = cr_last,
      creatinine_recent_mean_mg_dl = cr_mean_recent
    )
  },
  by = .EACHI
]

# Word-specified renal formula:
# eGFR = 175 * Scr^-1.154 * Age^-0.203 * 0.742 (if female)
calc_egfr_word_formula <- function(scr_mg_dl, age_years, sex_char) {
  ifelse(
    is.na(scr_mg_dl) | is.na(age_years) | is.na(sex_char),
    NA_real_,
    {
      is_female <- sex_char == "F"
      sex_mult <- ifelse(is_female, 0.742, 1.0)
      175 *
        (scr_mg_dl ^ -1.154) *
        (age_years ^ -0.203) *
        sex_mult
    }
  )
}

# ==============================================================================
# 6. Build final Word-defined comorbidity table
# ==============================================================================
cat("Building final comorbidity table...\n")

final_dt <- merge(anchor_index, diag_agg, by = c("subject_id", "hadm_id", "op_id", "asa", "age", "sex", "Male", "BMI"), all.x = TRUE)
final_dt <- merge(
  final_dt,
  med_agg[, c("op_id", "medication_records_preop", med_flag_cols), with = FALSE],
  by = "op_id",
  all.x = TRUE
)
final_dt <- merge(final_dt, hb_agg[, .(op_id, hb_n_preop, hb_last_g_dl, hb_min_g_dl)], by = "op_id", all.x = TRUE)
final_dt <- merge(final_dt, cr_agg[, .(op_id, creatinine_90d_n, creatinine_last_mg_dl, creatinine_recent_mean_mg_dl)], by = "op_id", all.x = TRUE)

flag_fill_zero <- c(diag_flag_cols, med_flag_cols)
for (j in flag_fill_zero) {
  if (j %in% names(final_dt)) {
    set(final_dt, which(is.na(final_dt[[j]])), j, 0L)
  }
}
for (j in c("diagnosis_records_preop", "medication_records_preop", "hb_n_preop", "creatinine_90d_n")) {
  if (j %in% names(final_dt)) {
    set(final_dt, which(is.na(final_dt[[j]])), j, 0L)
  }
}

final_dt[, `:=`(
  hypertension = as.integer(hypertension_dx == 1L | hypertension_med == 1L),
  arrhythmia = as.integer(arrhythmia_dx == 1L | arrhythmia_med == 1L),
  obesity = as.integer(obesity_icd == 1L | (!is.na(BMI) & BMI >= 30)),
  diabetes = as.integer(diabetes_dx == 1L | diabetes_med == 1L),
  hyperlipidemia = as.integer(hyperlipidemia_dx == 1L | hyperlipidemia_med == 1L)
)]

final_dt[, diabetes_category := fifelse(
  diabetes == 1L & diabetes_insulin_med == 1L, 1L,
  fifelse(diabetes == 1L, 2L, NA_integer_)
)]

final_dt[, anemia_icd10 := as.integer(
  anemia_icd_only == 1L |
    (!is.na(hb_min_g_dl) & (
      (Male == 1L & hb_min_g_dl < 13) |
        (Male == 0L & hb_min_g_dl < 12)
    ))
)]

final_dt[, anemia_preoperative := as.integer(
  !is.na(hb_last_g_dl) & (
    (Male == 1L & hb_last_g_dl < 13) |
      (Male == 0L & hb_last_g_dl < 12)
  )
)]

final_dt[, anemia_preop_severity := fifelse(
  anemia_preoperative != 1L, NA_integer_,
  fifelse(hb_last_g_dl >= 10, 1L,
    fifelse(hb_last_g_dl >= 7, 2L, 3L)
  )
)]

final_dt[, creatinine_two_value_mean_mg_dl := fifelse(creatinine_90d_n >= 2L, creatinine_recent_mean_mg_dl, NA_real_)]
final_dt[, egfr_word_formula := calc_egfr_word_formula(creatinine_two_value_mean_mg_dl, age, sex)]
final_dt[, renal_disease_category := fifelse(
  is.na(egfr_word_formula), NA_integer_,
  fifelse(egfr_word_formula >= 90, 1L,
    fifelse(egfr_word_formula >= 60, 2L,
      fifelse(egfr_word_formula >= 30, 3L,
        fifelse(egfr_word_formula >= 15, 4L, 5L)
      )
    )
  )
)]

keep_cols <- c(
  "subject_id", "hadm_id", "op_id", "asa", "age", "sex", "Male", "BMI",
  "hypertension", "ischemic_heart_disease", "heart_failure", "arrhythmia",
  "atrial_fibrillation_flutter", "pulmonary_hypertension", "peripheral_vascular_disease",
  "cerebrovascular_disease", "dementia", "parkinsonism", "copd", "asthma",
  "renal_disease", "renal_disease_category", "renal_dialysis", "chronic_liver_disease",
  "peptic_ulcer_disease", "gerd", "obesity", "diabetes", "diabetes_category",
  "hyperlipidemia", "anemia_icd10", "anemia_preoperative", "anemia_preop_severity",
  "connective_tissue_disease", "malignancy",
  "diagnosis_records_preop", "medication_records_preop", "hb_n_preop", "hb_last_g_dl",
  "hb_min_g_dl", "creatinine_90d_n", "creatinine_last_mg_dl", "creatinine_recent_mean_mg_dl",
  "creatinine_two_value_mean_mg_dl", "egfr_word_formula"
)

final_out <- final_dt[, ..keep_cols]
setorderv(final_out, c("subject_id", "hadm_id", "op_id"))
fwrite(final_out, file.path(path_output, "comorbidity_word_defined_anchor_first_nonMAC.csv"))

# ==============================================================================
# 7. Audit table: old Diagnosis_v1 versus Word definition
# ==============================================================================
cat("Writing Word-vs-old-script audit table...\n")

audit <- data.table(
  variable = c(
    "hypertension", "ischemic_heart_disease", "heart_failure", "arrhythmia",
    "atrial_fibrillation_flutter", "pulmonary_hypertension", "peripheral_vascular_disease",
    "cerebrovascular_disease", "dementia", "parkinsonism", "copd", "asthma",
    "renal_disease", "renal_disease_category", "renal_dialysis", "chronic_liver_disease",
    "peptic_ulcer_disease", "gerd", "obesity", "diabetes", "diabetes_category",
    "hyperlipidemia", "anemia_icd10", "anemia_preoperative", "anemia_preop_severity",
    "connective_tissue_disease", "malignancy"
  ),
  word_source = c(
    "ICD I10-I16 or ATC C02/C08C", "ICD I20-I25", "ICD I42/I43/I50", "ICD I47-I49 or ATC C01B/C08D/C07A",
    "ICD I48", "ICD I27", "ICD I70/I71/I73/K55",
    "ICD I60-I69/G45-G46", "ICD F01-F03/F05/G30/G31", "ICD G20-G22", "ICD J41-J44", "ICD J45",
    "ICD N03-N08/N18/N19/I12/I13/Z49/Z94/Z99", "eGFR from preop creatinine", "ICD Z49", "ICD B18/K70-K77",
    "ICD K25-K28", "ICD K21", "ICD E66 or BMI>=30", "ICD E10-E14 or ATC A10", "ATC A10A if diabetes=1",
    "ICD E78 or ATC C10A", "ICD D50-D64 or min Hb threshold", "last preop Hb threshold", "last preop Hb severity",
    "ICD M05/M06/M31-M35", "ICD C00-C76/C81-C96"
  ),
  old_script_status = c(
    "partial", "partial", "missing", "partial",
    "matched", "missing", "matched",
    "partial", "partial", "missing", "partial", "matched",
    "partial", "missing", "missing", "partial",
    "matched", "missing", "missing", "partial", "missing",
    "missing", "partial", "missing", "missing",
    "matched", "partial"
  ),
  main_issue = c(
    "old script added O10-O16 and ignored medication rule",
    "old script named CAD and mixed with angina/MI decomposition",
    "not implemented in old script",
    "old script had ICD-only plus a broader arrhythmia_any concept",
    "implemented as AF only; wording differs slightly",
    "not implemented in old script",
    "generally aligned",
    "old script missed G46",
    "old script missed F05/G31",
    "not implemented in old script",
    "old script only used J44",
    "aligned",
    "old script missed N03-N08 and category split",
    "not implemented in old script",
    "not implemented in old script",
    "old script only used B18/K70/K73/K74",
    "aligned",
    "not implemented in old script",
    "old script did not use BMI>=30",
    "old script used E08/E09 and missed medication rule",
    "not implemented in old script",
    "not implemented in old script",
    "old script used Hb minimum fallback but no dedicated preop anemia split",
    "not implemented in old script",
    "not implemented in old script",
    "aligned",
    "old script included broader C00-C99 minus C77-C80 proxy"
  )
)
fwrite(audit, file.path(path_output, "comorbidity_word_definition_audit.csv"))

# ==============================================================================
# 8. Summary outputs for prevalence and category distributions
# ==============================================================================
cat("Building summary tables...\n")

binary_vars <- c(
  "hypertension", "ischemic_heart_disease", "heart_failure", "arrhythmia",
  "atrial_fibrillation_flutter", "pulmonary_hypertension", "peripheral_vascular_disease",
  "cerebrovascular_disease", "dementia", "parkinsonism", "copd", "asthma",
  "renal_disease", "renal_dialysis", "chronic_liver_disease", "peptic_ulcer_disease",
  "gerd", "obesity", "diabetes", "hyperlipidemia", "anemia_icd10",
  "anemia_preoperative", "connective_tissue_disease", "malignancy"
)

summary_binary <- rbindlist(lapply(binary_vars, function(v) {
  data.table(
    variable = v,
    n_cases = sum(final_out[[v]] == 1L, na.rm = TRUE),
    total_ops = nrow(final_out),
    prevalence_pct = round(100 * mean(final_out[[v]] == 1L, na.rm = TRUE), 2)
  )
}), use.names = TRUE)
setorder(summary_binary, -prevalence_pct, variable)
fwrite(summary_binary, file.path(path_output, "comorbidity_prevalence_summary.csv"))

summary_category <- rbindlist(list(
  final_out[!is.na(renal_disease_category), .N, by = .(category = renal_disease_category)][, variable := "renal_disease_category"],
  final_out[!is.na(diabetes_category), .N, by = .(category = diabetes_category)][, variable := "diabetes_category"],
  final_out[!is.na(anemia_preop_severity), .N, by = .(category = anemia_preop_severity)][, variable := "anemia_preop_severity"]
), use.names = TRUE, fill = TRUE)

if (nrow(summary_category) > 0L) {
  summary_category[, prevalence_pct := round(100 * N / nrow(final_out), 2)]
  setcolorder(summary_category, c("variable", "category", "N", "prevalence_pct"))
  setorder(summary_category, variable, category)
}
fwrite(summary_category, file.path(path_output, "comorbidity_category_summary.csv"))

top10 <- summary_binary[1:min(10L, .N), .(
  summary_text = sprintf("%s: %d/%d (%.2f%%)", variable, n_cases, total_ops, prevalence_pct)
)]
fwrite(top10, file.path(path_output, "comorbidity_top10_summary_text.csv"))

cat("\nTop 10 comorbidity prevalence:\n")
print(summary_binary[1:min(10L, .N)])

cat("\nDone.\n")
