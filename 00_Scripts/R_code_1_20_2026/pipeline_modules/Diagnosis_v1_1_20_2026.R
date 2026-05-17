library(data.table)
library(stringr)

# ==============================================================================
# 1. Paths and inputs
# ==============================================================================
path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
path_processed_base <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_folder_name <- "Diagnosis_1_20_2026"
path_output <- file.path(path_processed_base, output_folder_name)

if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE)
}

cat("Loading operations and diagnosis...\n")

ops <- fread(
  file.path(path_raw, "operations.csv"),
  select = c("op_id", "subject_id", "hadm_id", "admission_time", "discharge_time", "orin_time"),
  na.strings = c("", "NA")
)
diag <- fread(
  file.path(path_raw, "diagnosis.csv"),
  select = c("subject_id", "chart_time", "icd10_cm"),
  na.strings = c("", "NA")
)

ops[, `:=`(
  admission_time = as.numeric(admission_time),
  discharge_time = as.numeric(discharge_time),
  orin_time = as.numeric(orin_time)
)]
diag[, chart_time := as.numeric(chart_time)]

ops <- ops[!is.na(op_id) & !is.na(subject_id)]
ops[, hadm_n := uniqueN(hadm_id), by = subject_id]

# Assign each diagnosis record to one admission stay first (current-stay priority).
diag[, rec_id := .I]
stay_map <- unique(
  ops[!is.na(hadm_id) & !is.na(admission_time) & !is.na(discharge_time),
      .(subject_id, hadm_id, admission_time, discharge_time)]
)
setkey(stay_map, subject_id, admission_time, discharge_time)

diag_stay_candidate <- stay_map[
  diag[, .(rec_id, subject_id, chart_time)],
  on = .(subject_id, admission_time <= chart_time, discharge_time >= chart_time),
  allow.cartesian = TRUE,
  nomatch = 0L
]

if (nrow(diag_stay_candidate) > 0L) {
  diag_stay_map <- diag_stay_candidate[
    order(rec_id, -admission_time, discharge_time)
  ][, .SD[1], by = rec_id][
    , .(rec_id, assigned_hadm_id = hadm_id)
  ]
} else {
  diag_stay_map <- data.table(rec_id = integer(), assigned_hadm_id = numeric())
}

diag <- merge(diag, diag_stay_map, by = "rec_id", all.x = TRUE)
diag[, rec_id := NULL]

cat("Merging operations with diagnosis by subject_id after stay assignment...\n")
dt <- merge(ops, diag, by = "subject_id", all.x = TRUE, allow.cartesian = TRUE)
dt[, icd3 := str_sub(str_to_upper(str_trim(icd10_cm)), 1, 3)]

# ==============================================================================
# 2. Feature flags (ICD-10 3-digit proxy mapping; updated pipeline ICD version)
# ==============================================================================
hit_set <- function(values) as.integer(!is.na(dt$icd3) & dt$icd3 %chin% values)
hit_regex <- function(pattern) as.integer(!is.na(dt$icd3) & str_detect(dt$icd3, pattern))

aids_opportunistic_codes <- c(
  "B37", "C53", "B38", "B45", "A07", "B25", "G93", "B00", "B39", "C46",
  "C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90",
  "C91", "C92", "C93", "C94", "C95", "C96", "A31", "A15", "A16", "A17",
  "A18", "A19", "B59", "Z87", "A81", "A02", "B58", "R64"
)

cat("Building diagnosis flag columns...\n")
dt[, `:=`(
  smoking_hit = hit_set(c("Z72", "F17")),
  drinking_hit = hit_set(c("F10", "K70")),
  hypertension_hit = hit_regex("^I1[0-6]|^O1[0-6]"),
  diabetes_hit = hit_regex("^E(08|09|10|11|13|14)$"),
  diabetes_any_hit = hit_regex("^E(08|09|10|11|13|14)$"),
  cerebrovasc_hit = as.integer(!is.na(icd3) & (str_detect(icd3, "^I6[0-9]") | icd3 == "G45")),
  dementia_hit = hit_set(c("F00", "F01", "F02", "F03", "G30")),
  hemi_para_hit = hit_set(c("G80", "G81", "G82", "G83", "G04", "G11")),
  mi_hit = hit_set(c("I21", "I22")),
  angina_hit = hit_set(c("I20")),
  af_hit = hit_set(c("I48")),
  cad_hit = hit_regex("^I2[0-5]"),
  arrhythmia_any_hit = hit_regex("^I4[7-9]"),
  copd_hit = hit_set(c("J44")),
  asthma_hit = hit_set(c("J45")),
  ards_hit = hit_set(c("J80")),
  renal_disease_hit = hit_set(c("N18", "N19", "I12", "I13", "Z49", "Z94", "Z99")),
  liver_disease_hit = hit_set(c("B18", "K70", "K73", "K74")),
  pud_hit = hit_regex("^K2[5-8]"),
  ctd_hit = hit_set(c("M05", "M06", "M32", "M33", "M34", "M31", "M35")),
  pvd_hit = hit_set(c("I70", "I71", "I73", "K55")),
  anemia_hit = hit_regex("^D5[0-9]|^D6[0-4]"),
  malignancy_hit = as.integer(
    !is.na(icd3) &
      (str_detect(icd3, "^C[0-6][0-9]") | str_detect(icd3, "^C7[0-6]") | str_detect(icd3, "^C[8-9][0-9]")) &
      !(icd3 %chin% c("C77", "C78", "C79", "C80"))
  ),
  metastatic_tumor_hit = hit_set(c("C77", "C78", "C79", "C80")),
  hiv_hit = hit_set(c("B20", "B21", "B22", "B23", "B24")),
  aids_opportunistic_raw_hit = hit_set(aids_opportunistic_codes)
)]

hit_cols <- c(
  "smoking_hit", "drinking_hit", "hypertension_hit", "diabetes_hit", "diabetes_any_hit", "cerebrovasc_hit",
  "dementia_hit", "hemi_para_hit", "mi_hit", "angina_hit", "af_hit", "cad_hit",
  "arrhythmia_any_hit", "copd_hit", "asthma_hit", "ards_hit", "renal_disease_hit",
  "liver_disease_hit", "pud_hit", "ctd_hit", "pvd_hit", "anemia_hit", "malignancy_hit",
  "metastatic_tumor_hit", "hiv_hit", "aids_opportunistic_raw_hit"
)

out_cols <- c(
  "smoking", "drinking", "hypertension", "diabetes", "diabetes_any", "cerebrovascular_disease", "dementia",
  "hemiplegia_paraplegia", "myocardial_infarction", "angina", "atrial_fibrillation",
  "coronary_artery_disease", "arrhythmia_any", "copd", "asthma", "ards", "renal_disease",
  "liver_disease", "peptic_ulcer_disease", "connective_tissue_disease", "peripheral_vascular_disease",
  "anemia", "malignancy", "metastatic_solid_tumor", "hiv", "aids", "aids_opportunistic_raw", "hiv_aids"
)

build_flags_for_window <- function(mask) {
  x <- dt[mask]
  if (nrow(x) == 0L) {
    out <- copy(ops[, .(op_id)])
    out[, (out_cols) := 0L]
    out[, `:=`(total_icd_count = 0L, other_icd_n = 0L)]
    return(out)
  }

  x[, all_zero_hit := as.integer(rowSums(.SD, na.rm = TRUE) == 0L), .SDcols = hit_cols]

  agg <- x[, .(
    smoking = max(smoking_hit, na.rm = TRUE),
    drinking = max(drinking_hit, na.rm = TRUE),
    hypertension = max(hypertension_hit, na.rm = TRUE),
    diabetes = max(diabetes_hit, na.rm = TRUE),
    diabetes_any = max(diabetes_any_hit, na.rm = TRUE),
    cerebrovascular_disease = max(cerebrovasc_hit, na.rm = TRUE),
    dementia = max(dementia_hit, na.rm = TRUE),
    hemiplegia_paraplegia = max(hemi_para_hit, na.rm = TRUE),
    myocardial_infarction = max(mi_hit, na.rm = TRUE),
    angina = max(angina_hit, na.rm = TRUE),
    atrial_fibrillation = max(af_hit, na.rm = TRUE),
    coronary_artery_disease = max(cad_hit, na.rm = TRUE),
    arrhythmia_any = max(arrhythmia_any_hit, na.rm = TRUE),
    copd = max(copd_hit, na.rm = TRUE),
    asthma = max(asthma_hit, na.rm = TRUE),
    ards = max(ards_hit, na.rm = TRUE),
    renal_disease = max(renal_disease_hit, na.rm = TRUE),
    liver_disease = max(liver_disease_hit, na.rm = TRUE),
    peptic_ulcer_disease = max(pud_hit, na.rm = TRUE),
    connective_tissue_disease = max(ctd_hit, na.rm = TRUE),
    peripheral_vascular_disease = max(pvd_hit, na.rm = TRUE),
    anemia = max(anemia_hit, na.rm = TRUE),
    malignancy = max(malignancy_hit, na.rm = TRUE),
    metastatic_solid_tumor = max(metastatic_tumor_hit, na.rm = TRUE),
    hiv_any = max(hiv_hit, na.rm = TRUE),
    aids_opportunistic_raw = max(aids_opportunistic_raw_hit, na.rm = TRUE),
    total_icd_count = .N,
    other_icd_n = sum(all_zero_hit, na.rm = TRUE)
  ), by = op_id]

  agg[, aids := as.integer(hiv_any == 1L & aids_opportunistic_raw == 1L)]
  agg[, hiv := as.integer(hiv_any == 1L & aids == 0L)]
  agg[, hiv_aids := as.integer(hiv_any == 1L)]
  agg[, hiv_any := NULL]

  out <- merge(ops[, .(op_id)], agg, by = "op_id", all.x = TRUE)
  for (j in names(out)) {
    if (j != "op_id" && anyNA(out[[j]])) {
      set(out, which(is.na(out[[j]])), j, 0L)
    }
  }
  out[]
}

cat("Computing 3 pre-op history windows...\n")

# 1) strict: pure history before current admission
mask_strict <- !is.na(dt$chart_time) & !is.na(dt$admission_time) & (dt$chart_time < dt$admission_time)

# 2) current_stay: diagnosis recorded after admission but before OR entry
same_stay <- !is.na(dt$assigned_hadm_id) & !is.na(dt$hadm_id) & (dt$assigned_hadm_id == dt$hadm_id)
fallback_single_stay <- is.na(dt$assigned_hadm_id) & !is.na(dt$hadm_n) & (dt$hadm_n == 1L)

mask_current_stay <- !is.na(dt$chart_time) & !is.na(dt$admission_time) & !is.na(dt$orin_time) &
  (dt$chart_time >= dt$admission_time) & (dt$chart_time < dt$orin_time) &
  (same_stay | fallback_single_stay)

# 3) cumulative_preop: all diagnosis prior to OR entry (recommended main analysis window)
mask_cumulative <- mask_strict | mask_current_stay

flags_strict <- build_flags_for_window(mask_strict)
flags_current_stay <- build_flags_for_window(mask_current_stay)
flags_cumulative <- build_flags_for_window(mask_cumulative)

# Keep pipeline compatibility: final = cumulative_preop
flags_final <- copy(flags_cumulative)

fwrite(flags_strict, file.path(path_output, "diag_preop_flags_strict.csv"))
fwrite(flags_current_stay, file.path(path_output, "diag_preop_flags_current_stay.csv"))
fwrite(flags_cumulative, file.path(path_output, "diag_preop_flags_cumulative.csv"))
fwrite(flags_final, file.path(path_output, "diag_preop_flags_final.csv"))

cat("Saved diagnosis flag tables for all 3 windows.\n")

# ==============================================================================
# 3. Summary outputs
# ==============================================================================
build_summary <- function(df, window_name) {
  long <- melt(
    df[, !c("total_icd_count", "other_icd_n")],
    id.vars = "op_id",
    variable.name = "Comorbidity",
    value.name = "Status"
  )
  long[, .(
    n_cases = sum(Status, na.rm = TRUE),
    total_ops = .N,
    prevalence_pct = round(sum(Status, na.rm = TRUE) / .N * 100, 2),
    window = window_name
  ), by = Comorbidity][order(-prevalence_pct)]
}

sum_strict <- build_summary(flags_strict, "history_strict")
sum_current <- build_summary(flags_current_stay, "history_preop_current_stay")
sum_cumulative <- build_summary(flags_cumulative, "history_cumulative_preop")

summary_all <- rbindlist(list(sum_strict, sum_current, sum_cumulative), use.names = TRUE)

summary_cmp <- dcast(
  summary_all,
  Comorbidity ~ window,
  value.var = "prevalence_pct"
)
summary_cmp[, `:=`(
  delta_cumulative_minus_strict = round(history_cumulative_preop - history_strict, 2),
  delta_cumulative_minus_current_stay = round(history_cumulative_preop - history_preop_current_stay, 2)
)]
summary_cmp <- summary_cmp[order(-abs(delta_cumulative_minus_strict))]

# Keep original summary file name for compatibility (use cumulative as default)
summary_cumulative_compat <- copy(sum_cumulative)[
  , .(
    Comorbidity_Label = str_to_title(str_replace_all(Comorbidity, "_", " ")),
    Comorbidity,
    n_cases,
    total_ops,
    prevalence_pct
  )
]

fwrite(summary_cumulative_compat, file.path(path_output, "diag_preop_summary_stats.csv"))
fwrite(summary_all, file.path(path_output, "diag_preop_summary_stats_all_windows.csv"))
fwrite(summary_cmp, file.path(path_output, "diag_preop_window_comparison.csv"))

cat("\nTop 15 prevalence differences (cumulative - strict):\n")
print(summary_cmp[1:min(15, .N)])
cat("\nDone.\n")
