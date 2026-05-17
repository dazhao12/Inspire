suppressPackageStartupMessages({
  library(data.table)
})

processed_root <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed"
raw_root <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw"
audit_dir <- file.path(processed_root, "Word_Validation_Audit_Pack_3_31_2026")
if (!dir.exists(audit_dir)) dir.create(audit_dir, recursive = TRUE)

set.seed(20260331)

pct <- function(x, n) round(ifelse(n > 0, 100 * x / n, NA_real_), 2)
fmt_time <- function(x) ifelse(is.na(x), NA_character_, as.character(as.numeric(x)))
safe_head_codes <- function(x, n = 5L) {
  if (length(x) == 0L) return(NA_character_)
  paste(head(unique(x), n), collapse = ";")
}
filtered_fread_by_first_col <- function(csv_path, ids, select_cols) {
  ids <- sort(unique(ids[!is.na(ids)]))
  if (length(ids) == 0L) {
    return(data.table::as.data.table(setNames(replicate(length(select_cols), logical(0), simplify = FALSE), select_cols)))
  }
  ids_file <- tempfile(pattern = "audit_ids_", fileext = ".txt")
  writeLines(as.character(ids), ids_file)
  on.exit(unlink(ids_file), add = TRUE)
  awk_cmd <- sprintf(
    "awk -F, 'NR==FNR {ids[$1]=1; next} FNR==1 || ($1 in ids)' %s %s",
    shQuote(ids_file),
    shQuote(csv_path)
  )
  fread(cmd = awk_cmd, select = select_cols, na.strings = c("", "NA"))
}
flag_match <- function(output_value, recomputed_value, tol = 1e-8) {
  if (length(output_value) == 0L) output_value <- NA
  if (length(recomputed_value) == 0L) recomputed_value <- NA
  output_value <- output_value[1]
  recomputed_value <- recomputed_value[1]
  if (is.na(output_value) && is.na(recomputed_value)) return(TRUE)
  if (is.na(output_value) != is.na(recomputed_value)) return(FALSE)
  if (is.numeric(output_value) || is.numeric(recomputed_value)) return(abs(output_value - recomputed_value) <= tol)
  identical(output_value, recomputed_value)
}

# -----------------------------------------------------------------------------
# 1. Load outputs used for validation
# -----------------------------------------------------------------------------
anchor_out <- fread(file.path(processed_root, "Demographics_Timeline_first_nonMAC_3_30_2026", "Demographic_Operation_Level.csv"))
anchor_map <- fread(file.path(processed_root, "Demographics_Timeline_first_nonMAC_3_30_2026", "Admission_First_NonMAC_Operation_Map.csv"))
comorb <- fread(file.path(processed_root, "Diagnosis_word_comorbidities_first_nonMAC_3_30_2026", "comorbidity_word_defined_anchor_first_nonMAC.csv"))
acute <- fread(file.path(processed_root, "Acute_Status_3mo_first_nonMAC_sepsis_A40_A41_3_30_2026", "acute_status_3mo_before_orin_first_nonMAC.csv"))
meds_out <- fread(file.path(processed_root, "Meds_Preop_word_first_nonMAC_3_30_2026", "preop_meds_word_defined_first_nonMAC.csv"))
labs_out <- fread(file.path(processed_root, "lab_data_first_nonMAC_3_30_2026", "preop_labs_features_current_stay.csv"))
vitals_out <- fread(file.path(processed_root, "Vials_pro_first_nonMAC_3_30_2026", "preop_baseline_final.csv"))
outcome <- fread(file.path(processed_root, "Outcomes_word_complications_first_nonMAC_sepsis_A40_A41_3_30_2026", "postop_complications_word_defined_first_nonMAC.csv"))
sample_pack_existing <- fread(file.path(audit_dir, "sample_chart_audit_key_variables_3_31_2026.csv"))
word_corrections <- fread(file.path(audit_dir, "word_definition_corrections_3_31_2026.csv"))

# -----------------------------------------------------------------------------
# 2. Load raw data subset needed for evidence
# -----------------------------------------------------------------------------
ops_raw <- fread(
  file.path(raw_root, "operations.csv"),
  select = c(
    "subject_id", "hadm_id", "op_id", "case_id", "opdate", "antype", "sex",
    "admission_time", "discharge_time", "orin_time", "orout_time",
    "opstart_time", "anstart_time", "icuin_time", "icuout_time",
    "inhosp_death_time", "allcause_death_time"
  ),
  na.strings = c("", "NA")
)
ops_raw[, `:=`(
  hadm_id = as.numeric(hadm_id),
  admission_time = as.numeric(admission_time),
  discharge_time = as.numeric(discharge_time),
  orin_time = as.numeric(orin_time),
  orout_time = as.numeric(orout_time),
  opstart_time = as.numeric(opstart_time),
  anstart_time = as.numeric(anstart_time),
  icuin_time = as.numeric(icuin_time),
  icuout_time = as.numeric(icuout_time),
  inhosp_death_time = as.numeric(inhosp_death_time),
  allcause_death_time = as.numeric(allcause_death_time),
  opdate_num = suppressWarnings(as.numeric(opdate)),
  antype_clean = toupper(trimws(as.character(antype)))
)]
ops_raw[, non_mac_flag := !is.na(antype_clean) & antype_clean != "MAC"]
ops_raw[, hadm_group := fifelse(is.na(hadm_id), paste0("MISSING_HADM_", op_id), as.character(hadm_id))]
ops_raw[, anchor_sort_time := fcoalesce(opstart_time, anstart_time, orin_time, opdate_num, admission_time)]

# -----------------------------------------------------------------------------
# 3. Anchor audit
# -----------------------------------------------------------------------------
anchor_selected <- ops_raw[
  !is.na(subject_id) & !is.na(op_id) & !is.na(orin_time) & non_mac_flag
][order(subject_id, hadm_group, anchor_sort_time, op_id)][
  , .SD[1], by = .(subject_id, hadm_group)
][, .(subject_id, hadm_group, selected_op_id = op_id)]

multi_adm <- ops_raw[
  , .(
    n_ops = .N,
    n_non_mac = sum(non_mac_flag, na.rm = TRUE)
  ),
  by = .(subject_id, hadm_group, hadm_id)
][n_ops > 1 & n_non_mac >= 1]

anchor_multi_sample <- multi_adm[sample(.N, min(.N, 20L))]

anchor_audit_long <- merge(
  ops_raw[anchor_multi_sample, on = .(subject_id, hadm_group)],
  anchor_selected,
  by = c("subject_id", "hadm_group"),
  all.x = TRUE
)
setorder(anchor_audit_long, subject_id, hadm_group, anchor_sort_time, op_id)
anchor_audit_long[, expected_anchor_by_rule := 0L]
anchor_audit_long[
  non_mac_flag == TRUE,
  expected_anchor_by_rule := as.integer(seq_len(.N) == 1L),
  by = .(subject_id, hadm_group)
]
anchor_audit_long[, selected_anchor := as.integer(op_id == selected_op_id)]
fwrite(anchor_audit_long, file.path(audit_dir, "anchor_multisurgery_audit_long_3_31_2026.csv"))

anchor_audit_summary <- anchor_audit_long[, .(
  n_ops = .N,
  n_non_mac = sum(non_mac_flag),
  expected_op_id = op_id[expected_anchor_by_rule == 1L][1],
  selected_op_id = selected_op_id[1]
), by = .(subject_id, hadm_group, hadm_id)]
anchor_audit_summary[, anchor_match := expected_op_id == selected_op_id]
fwrite(anchor_audit_summary, file.path(audit_dir, "anchor_multisurgery_audit_summary_3_31_2026.csv"))

# -----------------------------------------------------------------------------
# 4. Variable definition register
# -----------------------------------------------------------------------------
definition_register <- data.table(
  variable = c(
    "first_nonMAC_anchor","anchor_sort_time","hypertension","diabetes","anemia_preoperative","renal_disease_category",
    "cerebral_infarction","ventilation","sepsis","antibiotics","statins","opioids",
    "preop_creatinine_nearest","preop_sbp","death_within_30_days","reoperation","aki_creatinine","icu_stay"
  ),
  block = c(
    "Anchor","Anchor","Comorbidities","Comorbidities","Comorbidities","Comorbidities",
    "Acute status","Acute status","Acute status","Medication","Medication","Medication",
    "Laboratory","Preop vitals","Outcome","Outcome","Outcome","Outcome"
  ),
  output_file = c(
    "Admission_First_NonMAC_Operation_Map.csv","Admission_First_NonMAC_Operation_Map.csv","comorbidity_word_defined_anchor_first_nonMAC.csv",
    "comorbidity_word_defined_anchor_first_nonMAC.csv","comorbidity_word_defined_anchor_first_nonMAC.csv",
    "comorbidity_word_defined_anchor_first_nonMAC.csv","acute_status_3mo_before_orin_first_nonMAC.csv",
    "acute_status_3mo_before_orin_first_nonMAC.csv","acute_status_3mo_before_orin_first_nonMAC.csv",
    "preop_meds_word_defined_first_nonMAC.csv","preop_meds_word_defined_first_nonMAC.csv","preop_meds_word_defined_first_nonMAC.csv",
    "preop_labs_features_current_stay.csv","preop_baseline_final.csv","postop_complications_word_defined_first_nonMAC.csv",
    "postop_complications_word_defined_first_nonMAC.csv","postop_complications_word_defined_first_nonMAC.csv","postop_complications_word_defined_first_nonMAC.csv"
  ),
  source_table = c(
    "operations","operations","diagnosis + medications","diagnosis + medications","labs","diagnosis + labs",
    "diagnosis","ward_vitals","diagnosis","medications","medications","medications",
    "labs","ward_vitals + vitals","operations","operations","labs + ward_vitals","operations"
  ),
  time_window = c(
    "Per admission, choose first non-MAC surgery",
    "Sort by opstart_time -> anstart_time -> orin_time -> opdate -> admission_time",
    "Before anchor orin_time",
    "Before anchor orin_time",
    "Last hb with chart_time < anchor orin_time",
    "Renal diagnosis before anchor orin_time + creatinine within 90 days before anchor orin_time",
    "Within 3 months before anchor orin_time",
    "Within 3 months before anchor orin_time",
    "Within 3 months before anchor orin_time",
    "Longer of 14 days before orin_time or admission_time to orin_time",
    "Longer of 14 days before orin_time or admission_time to orin_time",
    "Longer of 14 days before orin_time or admission_time to orin_time",
    "Current-stay preop window: admission_time <= chart_time < orin_time after stay assignment",
    "Ward: max(admission_time, orin_time-1440) <= chart_time < orin_time; OR fallback: orin_time-120 <= chart_time < orin_time",
    "After orout_time and within 30 days",
    "Secondary operation orin_time > first orout_time and <= 30 days",
    "Peak creatinine within 7 days after orout_time, stage 3 upgraded by CRRT",
    "icuout_time > orout_time"
  ),
  code_rule = c(
    "Keep first surgery with non-MAC anesthesia per subject_id + hadm_id",
    "Tie-break by earliest available operative/anesthesia/OR time",
    "ICD I10-I16 OR ATC C02**** / C08C***",
    "ICD E10-E14 OR ATC A10****",
    "Last Hb <13 g/dL in men or <12 g/dL in women",
    "Renal ICD + average of 2 most recent Scr values in prior 90 days, then Word eGFR staging",
    "ICD I63",
    "ward_vitals item vent == 1",
    "ICD A40-A41",
    "ATC J01****",
    "ATC C10AA**",
    "ATC N02A***",
    "Nearest creatinine in current-stay preop window",
    "Ward mean SBP in preop window; if missing, OR induction SBP mean",
    "Inhosp death or all-cause death within 43,200 min after orout_time",
    "Next surgery in same admission within 30 days",
    "Peak creatinine / baseline >=1.5 or CRRT",
    "ICU out after OR out"
  )
)
fwrite(definition_register, file.path(audit_dir, "variable_definition_register_3_31_2026.csv"))

# -----------------------------------------------------------------------------
# 5. Build sample set for execution audit
# -----------------------------------------------------------------------------
sample_base_ids <- sample_pack_existing[variable %in% c(
  "hypertension","diabetes","anemia_preoperative","cerebral_infarction","ventilation","sepsis",
  "antibiotics","statins","opioids","death_within_30_days","reoperation","aki_creatinine","icu_stay"
), .(subject_id, hadm_id, op_id, block_cn, block_en, variable, sample_type)]

comorb_lookup_cols <- c("subject_id", "hadm_id", "op_id", "hypertension", "diabetes", "anemia_preoperative")
acute_lookup_cols <- c("subject_id", "hadm_id", "op_id", "cerebral_infarction", "ventilation", "sepsis")
meds_lookup_cols <- c("subject_id", "hadm_id", "op_id", "antibiotics", "statins", "opioids")
outcome_lookup_cols <- c("subject_id", "hadm_id", "op_id", "death_within_30_days", "reoperation", "aki_creatinine", "icu_stay")

lookup_long <- rbindlist(list(
  melt(
    comorb[, ..comorb_lookup_cols],
    id.vars = c("subject_id", "hadm_id", "op_id"),
    variable.name = "variable",
    value.name = "output_value"
  ),
  melt(
    acute[, ..acute_lookup_cols],
    id.vars = c("subject_id", "hadm_id", "op_id"),
    variable.name = "variable",
    value.name = "output_value"
  ),
  melt(
    meds_out[, ..meds_lookup_cols],
    id.vars = c("subject_id", "hadm_id", "op_id"),
    variable.name = "variable",
    value.name = "output_value"
  ),
  melt(
    outcome[, ..outcome_lookup_cols],
    id.vars = c("subject_id", "hadm_id", "op_id"),
    variable.name = "variable",
    value.name = "output_value"
  )
), use.names = TRUE, fill = TRUE)

sample_base <- merge(
  sample_base_ids,
  lookup_long,
  by = c("subject_id", "hadm_id", "op_id", "variable"),
  all.x = TRUE,
  sort = FALSE
)

renal_pos <- comorb[!is.na(renal_disease_category)][sample(.N, min(.N, 20L))]
renal_neg <- comorb[renal_disease == 1L & is.na(renal_disease_category)]
if (nrow(renal_neg) < 20L) {
  renal_neg <- rbindlist(list(renal_neg, comorb[renal_disease == 0L & is.na(renal_disease_category)]), fill = TRUE)
}
renal_neg <- renal_neg[sample(.N, min(.N, 20L))]
renal_sample <- rbindlist(list(
  renal_pos[, .(subject_id, hadm_id, op_id, sample_type = "positive", output_value = renal_disease_category)],
  renal_neg[, .(subject_id, hadm_id, op_id, sample_type = "negative", output_value = renal_disease_category)]
), fill = TRUE)
renal_sample[, `:=`(block_cn = "既往史", block_en = "Comorbidities", variable = "renal_disease_category")]

lab_pos <- labs_out[!is.na(preop_creatinine_nearest)][sample(.N, min(.N, 20L))]
lab_neg <- labs_out[is.na(preop_creatinine_nearest)][sample(.N, min(.N, 20L))]
lab_sample <- rbindlist(list(
  lab_pos[, .(subject_id, hadm_id, op_id, sample_type = "positive", output_value = preop_creatinine_nearest)],
  lab_neg[, .(subject_id, hadm_id, op_id, sample_type = "negative", output_value = preop_creatinine_nearest)]
), fill = TRUE)
lab_sample[, `:=`(block_cn = "实验室", block_en = "Laboratory", variable = "preop_creatinine_nearest")]

vital_pos <- vitals_out[!is.na(preop_sbp)][sample(.N, min(.N, 20L))]
vital_neg <- vitals_out[is.na(preop_sbp)][sample(.N, min(.N, 20L))]
vital_sample <- rbindlist(list(
  vital_pos[, .(subject_id, hadm_id, op_id, sample_type = "positive", output_value = preop_sbp)],
  vital_neg[, .(subject_id, hadm_id, op_id, sample_type = "negative", output_value = preop_sbp)]
), fill = TRUE)
vital_sample[, `:=`(block_cn = "术前体征", block_en = "Preop vitals", variable = "preop_sbp")]

sample_all <- rbindlist(list(
  sample_base[, .(subject_id, hadm_id, op_id, block_cn, block_en, variable, sample_type, output_value)],
  renal_sample,
  lab_sample,
  vital_sample
), fill = TRUE)

sample_all <- unique(sample_all, by = c("variable", "sample_type", "subject_id", "hadm_id", "op_id"))
sample_all <- merge(sample_all, anchor_out[, .(subject_id, hadm_id, op_id, case_id, asa, department, antype)], by = c("subject_id", "hadm_id", "op_id"), all.x = TRUE)

# -----------------------------------------------------------------------------
# 6. Load raw source subsets for sampled subjects
# -----------------------------------------------------------------------------
sample_subjects <- unique(sample_all$subject_id)
sample_ops <- unique(sample_all$op_id)

diag_raw <- filtered_fread_by_first_col(
  file.path(raw_root, "diagnosis.csv"),
  sample_subjects,
  c("subject_id", "chart_time", "icd10_cm")
)
diag_raw[, `:=`(
  chart_time = as.numeric(chart_time),
  icd3 = substr(gsub("\\.", "", toupper(trimws(icd10_cm))), 1, 3)
)]
diag_raw <- diag_raw[!is.na(chart_time) & !is.na(icd3)]

meds_raw <- filtered_fread_by_first_col(
  file.path(raw_root, "medications.csv"),
  sample_subjects,
  c("subject_id", "chart_time", "atc_code", "atc_code2", "atc_code3")
)
meds_raw[, `:=`(
  chart_time = as.numeric(chart_time),
  atc_code1 = toupper(trimws(fifelse(is.na(atc_code), "", atc_code))),
  atc_code2 = toupper(trimws(fifelse(is.na(atc_code2), "", atc_code2))),
  atc_code3 = toupper(trimws(fifelse(is.na(atc_code3), "", atc_code3)))
)]
meds_raw <- meds_raw[!is.na(chart_time)]

labs_raw <- filtered_fread_by_first_col(
  file.path(raw_root, "labs.csv"),
  sample_subjects,
  c("subject_id", "chart_time", "item_name", "value")
)
labs_raw[, `:=`(
  chart_time = as.numeric(chart_time),
  item_name = tolower(trimws(item_name)),
  value_num = suppressWarnings(as.numeric(value))
)]
labs_raw <- labs_raw[!is.na(chart_time)]

ward_raw <- filtered_fread_by_first_col(
  file.path(raw_root, "ward_vitals.csv"),
  sample_subjects,
  c("subject_id", "chart_time", "item_name", "value")
)
ward_raw[, `:=`(
  chart_time = as.numeric(chart_time),
  item_name = tolower(trimws(item_name)),
  value_num = suppressWarnings(as.numeric(value))
)]
ward_raw <- ward_raw[!is.na(chart_time)]

or_raw <- filtered_fread_by_first_col(
  file.path(raw_root, "vitals.csv"),
  sample_ops,
  c("op_id", "chart_time", "item_name", "value")
)
or_raw[, `:=`(
  chart_time = as.numeric(chart_time),
  item_name = tolower(trimws(item_name)),
  value_num = suppressWarnings(as.numeric(value))
)]
or_raw <- or_raw[!is.na(chart_time)]

anchor_meta <- merge(
  anchor_out[, .(subject_id, hadm_id, op_id, case_id, asa, department, antype)],
  ops_raw[, .(subject_id, hadm_id, op_id, sex, admission_time, discharge_time, orin_time, orout_time, icuin_time, icuout_time, inhosp_death_time, allcause_death_time)],
  by = c("subject_id", "hadm_id", "op_id"),
  all.x = TRUE
)

atc_hit <- function(dt, regex) {
  dt[grepl(regex, atc_code1) | grepl(regex, atc_code2) | grepl(regex, atc_code3)]
}

renal_codes <- c("N03","N04","N05","N06","N07","N08","N18","N19","I12","I13","Z49","Z94","Z99")
classify_renal_cat <- function(egfr) {
  if (is.na(egfr)) return(NA_integer_)
  if (egfr >= 90) return(1L)
  if (egfr >= 60) return(2L)
  if (egfr >= 30) return(3L)
  if (egfr >= 15) return(4L)
  5L
}
calc_egfr <- function(scr, age, sex) {
  if (is.na(scr) || is.na(age) || is.na(sex)) return(NA_real_)
  175 * (scr ^ -1.154) * (age ^ -0.203) * ifelse(sex == "F", 0.742, 1.0)
}
scalar_or_na <- function(x, mode = c("numeric", "character")) {
  mode <- match.arg(mode)
  if (length(x) == 0L || all(is.na(x))) {
    return(if (mode == "numeric") NA_real_ else NA_character_)
  }
  x[1]
}

eval_one <- function(row) {
  meta <- anchor_meta[subject_id == row$subject_id & op_id == row$op_id][1]
  if (nrow(meta) == 0L) {
    meta <- ops_raw[subject_id == row$subject_id & op_id == row$op_id][1]
  }
  sid <- row$subject_id
  hid <- row$hadm_id
  oid <- row$op_id
  anchor_orin_time <- scalar_or_na(meta$orin_time, "numeric")
  anchor_orout_time <- scalar_or_na(meta$orout_time, "numeric")
  anchor_admission_time <- scalar_or_na(meta$admission_time, "numeric")
  anchor_discharge_time <- scalar_or_na(meta$discharge_time, "numeric")
  sex <- scalar_or_na(meta$sex, "character")

  output_value <- row$output_value
  recomputed_value <- NA_real_
  evidence <- NA_character_
  source_table <- NA_character_
  time_window <- NA_character_
  linkage_risk <- "no"

  if (row$variable == "hypertension") {
    dd <- diag_raw[subject_id == sid & chart_time < anchor_orin_time & grepl("^I1[0-6]$", icd3)]
    mm <- atc_hit(meds_raw[subject_id == sid & chart_time < anchor_orin_time], "^C02|^C08C")
    recomputed_value <- as.integer(nrow(dd) > 0L | nrow(mm) > 0L)
    evidence <- sprintf("diag_n=%d diag_codes=%s med_n=%d med_atc=%s", nrow(dd), safe_head_codes(dd$icd3), nrow(mm), safe_head_codes(c(mm$atc_code1, mm$atc_code2, mm$atc_code3)))
    source_table <- "diagnosis + medications"
    time_window <- "chart_time < anchor orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "diabetes") {
    dd <- diag_raw[subject_id == sid & chart_time < anchor_orin_time & grepl("^E1[0-4]$", icd3)]
    mm <- atc_hit(meds_raw[subject_id == sid & chart_time < anchor_orin_time], "^A10")
    insulin <- atc_hit(meds_raw[subject_id == sid & chart_time < anchor_orin_time], "^A10A")
    recomputed_value <- as.integer(nrow(dd) > 0L | nrow(mm) > 0L)
    evidence <- sprintf("diag_n=%d diag_codes=%s med_n=%d insulin_n=%d", nrow(dd), safe_head_codes(dd$icd3), nrow(mm), nrow(insulin))
    source_table <- "diagnosis + medications"
    time_window <- "chart_time < anchor orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "anemia_preoperative") {
    hh <- labs_raw[subject_id == sid & item_name == "hb" & chart_time < anchor_orin_time & !is.na(value_num)]
    if (nrow(hh) > 0L) {
      hh <- hh[order(chart_time)]
      last_hb <- hh$value_num[nrow(hh)]
      recomputed_value <- as.integer((sex == "M" & last_hb < 13) | (sex == "F" & last_hb < 12))
      evidence <- sprintf("hb_n=%d last_hb=%.2f last_time=%s min_hb=%.2f sex=%s", nrow(hh), last_hb, fmt_time(hh$chart_time[nrow(hh)]), min(hh$value_num), sex)
    } else {
      recomputed_value <- 0L
      evidence <- "No Hb before anchor orin_time"
    }
    source_table <- "labs"
    time_window <- "last hb with chart_time < anchor orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "renal_disease_category") {
    dd <- diag_raw[subject_id == sid & chart_time < anchor_orin_time & icd3 %chin% renal_codes]
    cc <- labs_raw[subject_id == sid & item_name == "creatinine" & chart_time >= (anchor_orin_time - 90 * 24 * 60) & chart_time < anchor_orin_time & !is.na(value_num)]
    if (nrow(cc) >= 2L) {
      cc <- cc[order(-chart_time)]
      recent_two <- head(cc$value_num, 2L)
      scr_mean <- mean(recent_two)
      egfr <- calc_egfr(scr_mean, anchor_out[subject_id == sid & op_id == oid, Age][1], sex)
    } else {
      scr_mean <- NA_real_
      egfr <- NA_real_
    }
    recomputed_value <- if (nrow(dd) > 0L) classify_renal_cat(egfr) else NA_integer_
    evidence <- sprintf("renal_dx_n=%d renal_codes=%s cr_n_90d=%d scr_mean_recent2=%s egfr=%s", nrow(dd), safe_head_codes(dd$icd3), nrow(cc), ifelse(is.na(scr_mean), "NA", sprintf('%.3f', scr_mean)), ifelse(is.na(egfr), "NA", sprintf('%.2f', egfr)))
    source_table <- "diagnosis + labs"
    time_window <- "diagnosis before orin_time; creatinine within 90 days before orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "cerebral_infarction") {
    dd <- diag_raw[subject_id == sid & chart_time >= (anchor_orin_time - 90 * 24 * 60) & chart_time < anchor_orin_time & icd3 == "I63"]
    if (nrow(dd) > 0L) dd <- dd[order(-chart_time)]
    recomputed_value <- as.integer(nrow(dd) > 0L)
    evidence <- sprintf("diag_n=%d last_time=%s codes=%s", nrow(dd), ifelse(nrow(dd)>0, fmt_time(dd$chart_time[1]), NA_character_), safe_head_codes(dd$icd3))
    source_table <- "diagnosis"
    time_window <- "orin_time - 90d <= chart_time < orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "ventilation") {
    ww <- ward_raw[subject_id == sid & chart_time >= (anchor_orin_time - 90 * 24 * 60) & chart_time < anchor_orin_time & item_name == "vent" & value_num == 1]
    if (nrow(ww) > 0L) ww <- ww[order(-chart_time)]
    recomputed_value <- as.integer(nrow(ww) > 0L)
    evidence <- sprintf("ward_n=%d last_time=%s", nrow(ww), ifelse(nrow(ww)>0, fmt_time(ww$chart_time[1]), NA_character_))
    source_table <- "ward_vitals"
    time_window <- "orin_time - 90d <= chart_time < orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "sepsis") {
    dd <- diag_raw[subject_id == sid & chart_time >= (anchor_orin_time - 90 * 24 * 60) & chart_time < anchor_orin_time & icd3 %chin% c("A40","A41")]
    if (nrow(dd) > 0L) dd <- dd[order(-chart_time)]
    recomputed_value <- as.integer(nrow(dd) > 0L)
    evidence <- sprintf("diag_n=%d last_time=%s codes=%s", nrow(dd), ifelse(nrow(dd)>0, fmt_time(dd$chart_time[1]), NA_character_), safe_head_codes(dd$icd3))
    source_table <- "diagnosis"
    time_window <- "orin_time - 90d <= chart_time < orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "antibiotics") {
    start_time <- min(anchor_admission_time, anchor_orin_time - 14 * 24 * 60, na.rm = TRUE)
    mm <- atc_hit(meds_raw[subject_id == sid & chart_time >= start_time & chart_time < anchor_orin_time], "^J01")
    recomputed_value <- as.integer(nrow(mm) > 0L)
    evidence <- sprintf("med_n=%d start=%s last=%s atc=%s", nrow(mm), fmt_time(start_time), ifelse(nrow(mm)>0, fmt_time(max(mm$chart_time)), NA_character_), safe_head_codes(c(mm$atc_code1, mm$atc_code2, mm$atc_code3)))
    source_table <- "medications"
    time_window <- "max(admission window, 14d pre-op) to orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "statins") {
    start_time <- min(anchor_admission_time, anchor_orin_time - 14 * 24 * 60, na.rm = TRUE)
    mm <- atc_hit(meds_raw[subject_id == sid & chart_time >= start_time & chart_time < anchor_orin_time], "^C10AA")
    recomputed_value <- as.integer(nrow(mm) > 0L)
    evidence <- sprintf("med_n=%d start=%s last=%s atc=%s", nrow(mm), fmt_time(start_time), ifelse(nrow(mm)>0, fmt_time(max(mm$chart_time)), NA_character_), safe_head_codes(c(mm$atc_code1, mm$atc_code2, mm$atc_code3)))
    source_table <- "medications"
    time_window <- "max(admission window, 14d pre-op) to orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "opioids") {
    start_time <- min(anchor_admission_time, anchor_orin_time - 14 * 24 * 60, na.rm = TRUE)
    mm <- atc_hit(meds_raw[subject_id == sid & chart_time >= start_time & chart_time < anchor_orin_time], "^N02A")
    recomputed_value <- as.integer(nrow(mm) > 0L)
    evidence <- sprintf("med_n=%d start=%s last=%s atc=%s", nrow(mm), fmt_time(start_time), ifelse(nrow(mm)>0, fmt_time(max(mm$chart_time)), NA_character_), safe_head_codes(c(mm$atc_code1, mm$atc_code2, mm$atc_code3)))
    source_table <- "medications"
    time_window <- "max(admission window, 14d pre-op) to orin_time"
    linkage_risk <- "yes"
  } else if (row$variable == "preop_creatinine_nearest") {
    cc <- labs_raw[subject_id == sid & item_name == "creatinine" & chart_time >= anchor_admission_time & chart_time < anchor_orin_time & !is.na(value_num)]
    cc <- cc[chart_time <= anchor_discharge_time | is.na(anchor_discharge_time)]
    if (nrow(cc) > 0L) {
      cc <- cc[order(-chart_time)]
      recomputed_value <- cc$value_num[1]
      evidence <- sprintf("creat_n=%d last=%.3f last_time=%s", nrow(cc), recomputed_value, fmt_time(cc$chart_time[1]))
    } else {
      recomputed_value <- NA_real_
      evidence <- "No current-stay creatinine before orin_time"
    }
    source_table <- "labs"
    time_window <- "admission_time <= chart_time < orin_time in current stay"
    linkage_risk <- "yes"
  } else if (row$variable == "preop_sbp") {
    ww <- ward_raw[subject_id == sid & item_name == "nibp_sbp" & chart_time >= max(anchor_admission_time, anchor_orin_time - 1440, na.rm = TRUE) & chart_time < anchor_orin_time & !is.na(value_num)]
    oo <- or_raw[op_id == oid & item_name %chin% c("nibp_sbp","art_sbp") & chart_time >= (anchor_orin_time - 120) & chart_time < anchor_orin_time & !is.na(value_num)]
    ward_mean <- if (nrow(ww) > 0L) mean(ww$value_num) else NA_real_
    or_mean <- if (nrow(oo) > 0L) mean(oo$value_num) else NA_real_
    recomputed_value <- if (!is.na(ward_mean)) round(ward_mean, 1) else if (!is.na(or_mean)) round(or_mean, 1) else NA_real_
    evidence <- sprintf("ward_n=%d ward_mean=%s or_n=%d or_mean=%s", nrow(ww), ifelse(is.na(ward_mean), "NA", sprintf('%.1f', ward_mean)), nrow(oo), ifelse(is.na(or_mean), "NA", sprintf('%.1f', or_mean)))
    source_table <- "ward_vitals + vitals"
    time_window <- "Ward preop 24h or OR induction 120 min fallback"
    linkage_risk <- "yes"
  } else if (row$variable == "death_within_30_days") {
    recomputed_value <- as.integer(
      (!is.na(meta$inhosp_death_time) & meta$inhosp_death_time >= anchor_orout_time & meta$inhosp_death_time - anchor_orout_time <= 43200) |
        (!is.na(meta$allcause_death_time) & meta$allcause_death_time >= anchor_orout_time & meta$allcause_death_time - anchor_orout_time <= 43200)
    )
    evidence <- sprintf("inhosp=%s allcause=%s orout=%s", fmt_time(meta$inhosp_death_time), fmt_time(meta$allcause_death_time), fmt_time(anchor_orout_time))
    source_table <- "operations"
    time_window <- "0 to 30 days after orout_time"
  } else if (row$variable == "reoperation") {
    rr <- ops_raw[subject_id == sid & hadm_id == hid & op_id != oid & !is.na(orin_time) & orin_time > anchor_orout_time & orin_time <= anchor_orout_time + 30 * 24 * 60]
    if (nrow(rr) > 0L) rr <- rr[order(orin_time)]
    recomputed_value <- as.integer(nrow(rr) > 0L)
    evidence <- sprintf("reop_n=%d next_orin=%s", nrow(rr), ifelse(nrow(rr)>0, fmt_time(rr$orin_time[1]), NA_character_))
    source_table <- "operations"
    time_window <- "secondary orin_time > orout_time and <= 30d"
  } else if (row$variable == "aki_creatinine") {
    baseline <- labs_raw[subject_id == sid & item_name == "creatinine" & chart_time >= anchor_admission_time & chart_time < anchor_orin_time & !is.na(value_num)]
    baseline_val <- if (nrow(baseline) > 0L) min(baseline$value_num) else NA_real_
    postop <- labs_raw[subject_id == sid & item_name == "creatinine" & chart_time >= anchor_orout_time & chart_time <= anchor_orout_time + 7 * 24 * 60 & !is.na(value_num)]
    if (!is.na(baseline_val) && nrow(postop) > 0L) {
      peak_idx <- which.max(postop$value_num)
      peak_val <- postop$value_num[peak_idx]
      ratio <- peak_val / baseline_val
      stage <- if (ratio >= 3 || peak_val >= 4) 3L else if (ratio >= 2) 2L else if (ratio >= 1.5) 1L else 0L
    } else {
      peak_val <- NA_real_
      ratio <- NA_real_
      stage <- 0L
    }
    crrt <- ward_raw[subject_id == sid & item_name == "crrt" & value_num == 1 & chart_time >= anchor_orout_time]
    if (nrow(crrt) > 0L && stage < 3L) stage <- 3L
    recomputed_value <- as.integer(stage >= 1L)
    evidence <- sprintf("baseline=%s peak=%s ratio=%s crrt_n=%d stage=%d", ifelse(is.na(baseline_val), "NA", sprintf('%.3f', baseline_val)), ifelse(is.na(peak_val), "NA", sprintf('%.3f', peak_val)), ifelse(is.na(ratio), "NA", sprintf('%.3f', ratio)), nrow(crrt), stage)
    source_table <- "labs + ward_vitals"
    time_window <- "baseline current stay pre-op + peak creatinine 7d postop"
    linkage_risk <- "yes"
  } else if (row$variable == "icu_stay") {
    recomputed_value <- as.integer(!is.na(meta$icuout_time) & !is.na(anchor_orout_time) & meta$icuout_time > anchor_orout_time)
    evidence <- sprintf("icuin=%s icuout=%s orout=%s", fmt_time(meta$icuin_time), fmt_time(meta$icuout_time), fmt_time(anchor_orout_time))
    source_table <- "operations"
    time_window <- "icuout_time > orout_time"
  } else {
    stop(sprintf("Unsupported variable: %s", row$variable))
  }

  match_status <- if (flag_match(output_value, recomputed_value, tol = 0.11)) "match" else "mismatch"
  data.table(
    block_cn = row$block_cn,
    block_en = row$block_en,
    variable = row$variable,
    sample_type = row$sample_type,
    subject_id = sid,
    hadm_id = hid,
    op_id = oid,
    case_id = row$case_id,
    output_value = output_value,
    recomputed_value = recomputed_value,
    match_status = match_status,
    source_table = source_table,
    time_window = time_window,
    linkage_risk = linkage_risk,
    evidence_summary = evidence
  )
}

sample_evidence <- rbindlist(lapply(seq_len(nrow(sample_all)), function(i) eval_one(sample_all[i])), fill = TRUE)
fwrite(sample_evidence, file.path(audit_dir, "sample_evidence_auto_3_31_2026.csv"))

# -----------------------------------------------------------------------------
# 7. Auto-filled audit results and summary
# -----------------------------------------------------------------------------
word_issue_vars <- data.table(
  variable = c("sepsis", "renal_disease_category"),
  issue_type = c("definition_needs_word_revision", "definition_needs_word_revision")
)

linkage_risk_vars <- data.table(
  variable = c("hypertension","diabetes","anemia_preoperative","cerebral_infarction","ventilation","antibiotics","statins","opioids","preop_creatinine_nearest","preop_sbp","aki_creatinine"),
  issue_type = "raw_linkage_limitation_only"
)

var_summary <- sample_evidence[, .(
  n_samples = .N,
  n_matches = sum(match_status == "match"),
  n_mismatches = sum(match_status == "mismatch")
), by = .(block_cn, block_en, variable)]
var_summary[, mismatch_pct := pct(n_mismatches, n_samples)]

var_summary <- merge(var_summary, word_issue_vars, by = "variable", all.x = TRUE, sort = FALSE)
var_summary <- merge(var_summary, linkage_risk_vars, by = "variable", all.x = TRUE, sort = FALSE, suffixes = c("_word", "_link"))
var_summary[, conclusion_label := fcase(
  n_mismatches > 0, "code_logic_needs_fix",
  !is.na(issue_type_word), issue_type_word,
  !is.na(issue_type_link), issue_type_link,
  default = "definition_correct"
)]
var_summary[, notes := fifelse(
  n_mismatches > 0, "At least one sampled mismatch between output and recomputed raw evidence.",
  fifelse(conclusion_label == "definition_needs_word_revision", "Code/output sampled correctly, but Word wording should be revised.",
    fifelse(conclusion_label == "raw_linkage_limitation_only", "Sampled evidence matches output; remaining risk is raw-table linkage granularity.",
      "Sampled evidence matched output and no material wording issue was pre-flagged."
    )
  )
)]

anchor_summary_result <- data.table(
  block_cn = "Anchor/Operation ID",
  block_en = "Anchor/Operation ID",
  variable = c("first_nonMAC_anchor", "anchor_sort_time"),
  n_samples = nrow(anchor_audit_summary),
  n_matches = sum(anchor_audit_summary$anchor_match),
  n_mismatches = sum(!anchor_audit_summary$anchor_match),
  mismatch_pct = pct(sum(!anchor_audit_summary$anchor_match), nrow(anchor_audit_summary)),
  conclusion_label = if (all(anchor_audit_summary$anchor_match)) "definition_correct" else "code_logic_needs_fix",
  notes = if (all(anchor_audit_summary$anchor_match)) {
    "All sampled multi-surgery admissions selected the earliest non-MAC surgery under the implemented sort order."
  } else {
    "At least one sampled admission did not select the expected earliest non-MAC surgery."
  }
)

validation_results_summary <- rbindlist(list(anchor_summary_result, var_summary[, .(
  block_cn, block_en, variable, n_samples, n_matches, n_mismatches, mismatch_pct, conclusion_label, notes
)]), fill = TRUE)
fwrite(validation_results_summary, file.path(audit_dir, "validation_results_summary_3_31_2026.csv"))

audit_results_auto <- merge(
  sample_evidence,
  definition_register[, .(variable, word_definition = code_rule, code_rule)],
  by = "variable",
  all.x = TRUE,
  sort = FALSE
)[, .(
  block_cn, block_en, variable, word_definition, code_rule,
  source_table, time_window,
  anchor_rule = "first non-MAC surgery per subject_id + hadm_id",
  sample_type,
  subject_id, hadm_id, op_id, case_id,
  expected_flag = output_value,
  observed_flag = recomputed_value,
  raw_evidence = evidence_summary,
  issue_found = fifelse(match_status == "mismatch", "yes", "no"),
  issue_type = fifelse(match_status == "mismatch", "code_logic_needs_fix", ""),
  action_needed = fifelse(match_status == "mismatch", "Review code and raw evidence", "None from automated check"),
  reviewer = "auto_validation",
  review_date = as.character(Sys.Date())
)]
fwrite(audit_results_auto, file.path(audit_dir, "variable_chart_audit_results_auto_3_31_2026.csv"))

writeLines(c(
  "# Validation Execution Results",
  "",
  sprintf("Anchor sample admissions reviewed: %d", nrow(anchor_audit_summary)),
  sprintf("Priority variables with automated sample evidence: %d", nrow(validation_results_summary) - 2L),
  "",
  "Files generated:",
  "- anchor_multisurgery_audit_long_3_31_2026.csv",
  "- anchor_multisurgery_audit_summary_3_31_2026.csv",
  "- variable_definition_register_3_31_2026.csv",
  "- sample_evidence_auto_3_31_2026.csv",
  "- validation_results_summary_3_31_2026.csv",
  "- variable_chart_audit_results_auto_3_31_2026.csv"
), file.path(audit_dir, "validation_execution_results_3_31_2026.md"))

cat(sprintf("Saved validation execution results to %s\n", audit_dir))
