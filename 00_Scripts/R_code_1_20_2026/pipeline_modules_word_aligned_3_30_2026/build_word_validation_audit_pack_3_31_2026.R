suppressPackageStartupMessages({
  library(data.table)
})

processed_root <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed"
output_dir <- file.path(processed_root, "Word_Validation_Audit_Pack_3_31_2026")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

set.seed(20260331)

read_fast <- function(path, cols = NULL) {
  if (is.null(cols)) {
    fread(path)
  } else {
    fread(path, select = cols)
  }
}

anchor <- read_fast(
  file.path(processed_root, "Demographics_Timeline_first_nonMAC_3_30_2026", "Demographic_Operation_Level.csv")
)

comorb_path <- file.path(processed_root, "Diagnosis_word_comorbidities_first_nonMAC_3_30_2026", "comorbidity_word_defined_anchor_first_nonMAC.csv")
acute_path <- file.path(processed_root, "Acute_Status_3mo_first_nonMAC_sepsis_A40_A41_3_30_2026", "acute_status_3mo_before_orin_first_nonMAC.csv")
meds_path <- file.path(processed_root, "Meds_Preop_word_first_nonMAC_3_30_2026", "preop_meds_word_defined_first_nonMAC.csv")
outcome_path <- file.path(processed_root, "Outcomes_word_complications_first_nonMAC_sepsis_A40_A41_3_30_2026", "postop_complications_word_defined_first_nonMAC.csv")

comorb <- read_fast(comorb_path)
acute <- read_fast(acute_path)
meds <- read_fast(meds_path)
outcome <- read_fast(outcome_path)

validation_plan <- data.table(
  audit_order = 1:14,
  block_cn = c(
    "Anchor/Operation ID","Anchor/Operation ID",
    "Comorbidities","Comorbidities","Comorbidities","Comorbidities",
    "Acute status","Medication",
    "Laboratory","Preop vitals",
    "Outcome","Outcome","Outcome","Outcome"
  ),
  block_en = c(
    "Anchor/Operation ID","Anchor/Operation ID",
    "Comorbidities","Comorbidities","Comorbidities","Comorbidities",
    "Acute status","Medication",
    "Laboratory","Preoperative vitals",
    "Outcome","Outcome","Outcome","Outcome"
  ),
  variable = c(
    "first_nonMAC_anchor","anchor_sort_time",
    "hypertension","diabetes","anemia_preoperative","renal_disease_category",
    "cerebral_infarction","antibiotics",
    "preop_creatinine_nearest","preop_sbp",
    "death_within_30_days","reoperation","aki_creatinine","icu_stay"
  ),
  why_check_first_cn = c(
    "全流程的核心锚点定义，必须优先确认",
    "同一次住院多次手术时排序是否正确",
    "ICD+ATC 混合定义，容易错配",
    "ICD+ATC 分类定义，且与 diabetes category 相关",
    "ICD/Hb 双口径，最容易出现假阳性/假阴性",
    "ICD + Scr/eGFR 分级，规则复杂",
    "时间窗事件，最能暴露归属问题",
    "ATC 窗口定义，适合做药物 spot check",
    "实验室当前住院术前窗口的代表变量",
    "术前体征窗口和 OR fallback 的代表变量",
    "死亡时间逻辑和窗口逻辑的代表变量",
    "同次住院后续手术逻辑的代表变量",
    "ICD/lab/CRRT 混合定义，复杂度高",
    "住院级结局逻辑，适合核 ICU 归属"
  ),
  why_check_first_en = c(
    "Core anchor definition for the whole pipeline",
    "Verifies sorting when an admission has multiple surgeries",
    "Mixed ICD + ATC definition with higher mismatch risk",
    "Mixed ICD + ATC definition linked to diabetes category",
    "Dual ICD/Hb definition, prone to false positives/negatives",
    "ICD + Scr/eGFR staging with complex logic",
    "Time-window event, useful for checking event assignment",
    "ATC window definition, good for medication spot check",
    "Representative lab for the current-stay preop window",
    "Representative preop vital for Ward/OR fallback logic",
    "Representative endpoint for death timing logic",
    "Representative endpoint for same-admission reoperation logic",
    "Complex ICD/lab/CRRT hybrid definition",
    "Admission-level endpoint useful for ICU assignment checks"
  ),
  recommended_positive_review_n = 20L,
  recommended_negative_review_n = 20L
)
fwrite(validation_plan, file.path(output_dir, "validation_plan_3_31_2026.csv"))

audit_template <- data.table(
  block_cn = character(),
  block_en = character(),
  variable = character(),
  word_definition = character(),
  code_rule = character(),
  source_table = character(),
  time_window = character(),
  anchor_rule = character(),
  sample_type = character(),
  sample_row_id = integer(),
  subject_id = numeric(),
  hadm_id = numeric(),
  op_id = numeric(),
  case_id = character(),
  expected_flag = integer(),
  observed_flag = integer(),
  raw_evidence = character(),
  issue_found = character(),
  issue_type = character(),
  action_needed = character(),
  reviewer = character(),
  review_date = character()
)
fwrite(audit_template, file.path(output_dir, "variable_chart_audit_template_3_31_2026.csv"))

word_corrections <- data.table(
  section = c(
    "Complications","Complications","Medication","Acute status","Outcome","Comorbidities","Linkage note"
  ),
  item = c(
    "Death within 6month","ARDS","drugs for obstructive airway diseases","Sepsis","Sepsis","Diabetes category","diagnosis/medications/labs/ward_vitals linkage"
  ),
  current_word_text = c(
    "allcause_death_time after orout_time<=129600",
    "icd10_cm in diagnosis.csv (value: J80 and J96)",
    "atc_code in medications.csv (value N03****)",
    "icd10_cm in diagnosis.csv (value:I40-I41)",
    "icd10_cm in diagnosis.csv (value:I40-I41)",
    "1/2; Insulin-dependent / Others",
    "Implicitly treated as if event tables belong to one operation/admission"
  ),
  issue_cn = c(
    "129600 分钟实际等于 90 天，不是 6 个月",
    "“and” 有歧义，不清楚是同时满足还是任一满足",
    "N03**** 对应抗癫痫药，不是阻塞性气道疾病用药",
    "I40-I41 更接近心肌炎，不是临床常见 sepsis 编码",
    "术后 sepsis 同样存在 ICD 写法问题",
    "数值编码和文字描述不一致，是否把 Others 并入 2 类不清楚",
    "原始表缺少 hadm_id/op_id，不能写成完美主键归属"
  ),
  issue_en = c(
    "129,600 minutes equals 90 days, not 6 months",
    "The word 'and' is ambiguous: both codes vs either code",
    "N03**** refers to antiepileptics, not obstructive airway drugs",
    "I40-I41 is closer to myocarditis and is not a standard sepsis code set",
    "The postoperative sepsis definition has the same ICD issue",
    "Numeric coding and text description are inconsistent",
    "Raw source tables lack hadm_id/op_id, so perfect key linkage is not possible"
  ),
  recommended_word_text = c(
    "Death within 90 days: allcause_death_time after orout_time <= 129600; or if true 6-month mortality is intended, use <= 259200 minutes.",
    "ARDS: icd10_cm in diagnosis.csv (value: J80 or J96) after orout_time within 30 days; if both are required, state 'J80 and J96 both present'.",
    "drugs for obstructive airway diseases: atc_code in medications.csv (value: R03****).",
    "Sepsis: icd10_cm in diagnosis.csv (value: A40-A41) within 3 months before orin_time of the first surgery.",
    "Sepsis: icd10_cm in diagnosis.csv (value: A40-A41) after orout_time within 30 days.",
    "Diabetes category: 1 = insulin-dependent (A10A*** before surgery); 2 = non-insulin-dependent or other diabetes treatment.",
    "For diagnosis, medications, labs, and ward_vitals, records are linked to the anchor operation by subject_id plus admission/time-window assignment; perfect hadm_id/op_id linkage is unavailable in the raw source."
  )
)
fwrite(word_corrections, file.path(output_dir, "word_definition_corrections_3_31_2026.csv"))

writeLines(c(
  "# Word Definition Corrections",
  "",
  "This file lists Word definitions that should be revised before the document is considered the final source of truth.",
  "",
  paste0("- ", word_corrections$item, ": ", word_corrections$issue_en),
  "",
  "See `word_definition_corrections_3_31_2026.csv` for structured fields."
), file.path(output_dir, "word_definition_corrections_3_31_2026.md"))

sample_one <- function(dt, block_cn, block_en, variable, sample_n = 20L, context_cols = character(), extra_rule = NULL) {
  stopifnot(variable %in% names(dt))
  dt_use <- copy(dt)
  if (!is.null(extra_rule)) {
    dt_use <- extra_rule(dt_use)
  }
  pos <- dt_use[get(variable) == 1]
  neg <- dt_use[get(variable) == 0]
  if (nrow(pos) > 0L) pos <- pos[sample(.N, min(.N, sample_n))]
  if (nrow(neg) > 0L) neg <- neg[sample(.N, min(.N, sample_n))]
  keep_cols <- unique(c("subject_id", "hadm_id", "op_id", context_cols))
  keep_cols <- keep_cols[keep_cols %in% names(dt_use)]

  out_pos <- if (nrow(pos) > 0L) pos[, ..keep_cols] else data.table()
  out_neg <- if (nrow(neg) > 0L) neg[, ..keep_cols] else data.table()
  if (nrow(out_pos) > 0L) out_pos[, `:=`(block_cn = block_cn, block_en = block_en, variable = variable, sample_type = "positive", expected_flag = 1L)]
  if (nrow(out_neg) > 0L) out_neg[, `:=`(block_cn = block_cn, block_en = block_en, variable = variable, sample_type = "negative", expected_flag = 0L)]
  rbindlist(list(out_pos, out_neg), fill = TRUE)
}

sample_pack <- rbindlist(list(
  sample_one(comorb, "既往史", "Comorbidities", "hypertension", context_cols = c("age", "sex", "BMI", "diagnosis_records_preop", "medication_records_preop")),
  sample_one(comorb, "既往史", "Comorbidities", "diabetes", context_cols = c("diabetes_category", "diagnosis_records_preop", "medication_records_preop")),
  sample_one(comorb, "既往史", "Comorbidities", "anemia_preoperative", context_cols = c("sex", "hb_last_g_dl", "hb_min_g_dl", "hb_n_preop", "anemia_preop_severity")),
  sample_one(comorb, "既往史", "Comorbidities", "renal_disease", context_cols = c("creatinine_90d_n", "creatinine_last_mg_dl", "creatinine_two_value_mean_mg_dl", "egfr_word_formula", "renal_disease_category")),
  sample_one(acute, "术前3个月急性状态", "Acute status", "cerebral_infarction", context_cols = c("cerebral_infarction_interval_to_surgery_min", "cerebral_infarction_event_time", "cerebral_infarction_source")),
  sample_one(acute, "术前3个月急性状态", "Acute status", "ventilation", context_cols = c("ventilation_interval_to_surgery_min", "ventilation_event_time", "ventilation_source_value")),
  sample_one(acute, "术前3个月急性状态", "Acute status", "sepsis", context_cols = c("sepsis_interval_to_surgery_min", "sepsis_event_time", "sepsis_source")),
  sample_one(meds, "术前用药", "Preoperative medications", "antibiotics", context_cols = c("admission_time", "orin_time", "start_time", "med_records_in_window")),
  sample_one(meds, "术前用药", "Preoperative medications", "statins", context_cols = c("admission_time", "orin_time", "start_time", "med_records_in_window")),
  sample_one(meds, "术前用药", "Preoperative medications", "opioids", context_cols = c("admission_time", "orin_time", "start_time", "med_records_in_window")),
  sample_one(outcome, "术后并发症", "Outcome", "death_within_30_days", context_cols = c("orout_time", "discharge_time", "death_time_from_orout_min")),
  sample_one(outcome, "术后并发症", "Outcome", "reoperation", context_cols = c("orout_time", "reoperation_interval_after_surgery_min")),
  sample_one(outcome, "术后并发症", "Outcome", "aki_creatinine", context_cols = c("aki_category", "peak_creatinine_postop", "peak_creatinine_interval_after_surgery_min", "crrt_postop")),
  sample_one(outcome, "术后并发症", "Outcome", "icu_stay", context_cols = c("orin_time", "orout_time", "icu_duration_min", "interval_between_icu_in_and_orout_min"))
), fill = TRUE)

sample_pack <- merge(
  sample_pack,
  anchor[, .(subject_id, hadm_id, op_id, case_id, opdate, asa, department, antype)],
  by = c("subject_id", "hadm_id", "op_id"),
  all.x = TRUE,
  sort = FALSE
)
sample_pack[, sample_row_id := seq_len(.N)]
setcolorder(sample_pack, c(
  "sample_row_id", "block_cn", "block_en", "variable", "sample_type", "expected_flag",
  "subject_id", "hadm_id", "op_id", "case_id", "opdate", "asa", "department", "antype"
))
fwrite(sample_pack, file.path(output_dir, "sample_chart_audit_key_variables_3_31_2026.csv"))

definition_checklist <- data.table(
  audit_step = 1:8,
  step_cn = c(
    "确认 Word 原始定义",
    "确认输出变量名",
    "确认数据源表",
    "确认时间窗",
    "确认 first non-MAC 锚点",
    "抽查 20 个阳性病例",
    "抽查 20 个阴性病例",
    "记录问题并决定是否修文档"
  ),
  step_en = c(
    "Confirm original Word definition",
    "Confirm output variable name",
    "Confirm source table",
    "Confirm time window",
    "Confirm first non-MAC anchor rule",
    "Review 20 positive cases",
    "Review 20 negative cases",
    "Record issues and revise document if needed"
  ),
  deliverable = c(
    "Word row/paragraph ID",
    "Output column name",
    "operations/diagnosis/medications/labs/ward_vitals/vitals",
    "Exact time inequality",
    "Anchor op selection evidence",
    "False-positive screen",
    "False-negative screen",
    "Issue log + corrected wording"
  )
)
fwrite(definition_checklist, file.path(output_dir, "definition_checklist_3_31_2026.csv"))

cat(sprintf("Saved word validation audit pack to %s\n", output_dir))
