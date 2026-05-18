suppressPackageStartupMessages({
  library(data.table)
})

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required to build the extraction audit workbook.")
}

project_root <- "/N/project/analgesia_perioperation"
raw_dir <- file.path(project_root, "data", "INSPIRE_1.3", "raw")
processed_dir <- file.path(project_root, "data", "INSPIRE_1.3", "processed")
project_dir <- file.path(project_root, "projects", "Inspire_data_process_ZZ")
docs_dir <- file.path(project_dir, "docs")

if (!dir.exists(docs_dir)) {
  dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
}

audit_workbook_path <- file.path(docs_dir, "INSPIRE_extraction_audit_workbook.xlsx")
audit_summary_path <- file.path(docs_dir, "INSPIRE_extraction_audit_summary.md")

schema_dt <- fread(file.path(raw_dir, "schema.csv"), encoding = "UTF-8")

ops <- fread(file.path(raw_dir, "operations.csv"),
  select = c("op_id", "subject_id", "hadm_id", "admission_time", "orin_time", "orout_time", "discharge_time")
)
diag <- fread(file.path(raw_dir, "diagnosis.csv"), select = c("subject_id", "chart_time", "icd10_cm"))
labs <- fread(file.path(raw_dir, "labs.csv"), select = c("subject_id", "chart_time", "item_name", "value"))
meds <- fread(file.path(raw_dir, "medications.csv"), select = c("subject_id", "chart_time"))
ward <- fread(file.path(raw_dir, "ward_vitals.csv"), select = c("subject_id", "chart_time"))
vitals <- fread(file.path(raw_dir, "vitals.csv"), select = c("op_id", "subject_id", "chart_time", "item_name", "value"))

baseline_full <- fread(file.path(processed_dir, "periop_baseline_operations_core_plus_timeline.csv"))
processed_specs <- data.table(
  table_name = c(
    "demographics_subject_level",
    "baseline_operations_full",
    "baseline_operations_preop_only",
    "baseline_operations_intraop_only",
    "diagnosis_preop_flags",
    "labs_preop_window_7d",
    "labs_preop_window_30d",
    "labs_preop_window_any",
    "meds_preop_final",
    "vitals_preop_baseline",
    "vitals_intraop_full_complete",
    "intraop_drugs_fluids_total_sum",
    "outcomes_postop",
    "master_dataset_final"
  ),
  file_name = c(
    "preop_demographics_subject_level.csv",
    "periop_baseline_operations_core_plus_timeline.csv",
    "preop_baseline_operations_core.csv",
    "periop_timeline_operations_raw_and_derived.csv",
    "preop_diagnosis_flags_cumulative_preop.csv",
    "preop_labs_window_7d.csv",
    "preop_labs_window_30d.csv",
    "preop_labs_window_cumulative_preop.csv",
    "preop_medications_flags_current_stay.csv",
    "preop_vitals_baseline.csv",
    "intraop_vitals_timeseries.csv",
    "intraop_drugs_fluids_totals.csv",
    "postop_outcomes.csv",
    "periop_master_dataset_all_features.csv"
  ),
  primary_key_definition = c(
    "subject_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id + chart_time + min_from_entry",
    "op_id",
    "op_id",
    "op_id"
  ),
  phase = c(
    "preop",
    "periop",
    "preop",
    "periop",
    "preop",
    "preop",
    "preop",
    "preop",
    "preop",
    "preop",
    "intraop",
    "intraop",
    "postop",
    "periop"
  ),
  time_anchor = c(
    "opdate_or_admission",
    "periop_mixed_anchor",
    "admission_to_orin",
    "periop_mixed_anchor",
    "orin",
    "orin",
    "orin",
    "orin",
    "admission_to_orin",
    "orin",
    "orin_to_orout",
    "orin_to_orout",
    "periop_mixed_anchor",
    "periop_mixed_anchor"
  ),
  table_role = c(
    "subject_summary",
    "baseline_wide",
    "preop_feature_subset",
    "intraop_postop_timeline_subset",
    "preop_feature_table",
    "preop_feature_table",
    "preop_feature_table",
    "preop_feature_table",
    "preop_feature_table",
    "preop_feature_table",
    "intraop_timeseries_table",
    "intraop_feature_table",
    "postop_outcome_table",
    "wide_analysis_table"
  )
)

multi_hadm_subjects <- unique(ops[, .(n_hadm = uniqueN(hadm_id)), by = subject_id][n_hadm > 1, subject_id])

raw_table_review <- data.table(
  raw_table = c("operations", "diagnosis", "labs", "medications", "ward_vitals", "vitals"),
  likely_key = c(
    "op_id",
    "subject_id + chart_time + icd10_cm",
    "subject_id + chart_time + item_name",
    "subject_id + chart_time + drug_name/atc_code",
    "subject_id + chart_time + item_name",
    "op_id + chart_time + item_name"
  ),
  explicit_ids = c(
    "op_id, subject_id, hadm_id, case_id",
    "subject_id only",
    "subject_id only",
    "subject_id only",
    "subject_id only",
    "op_id, subject_id"
  ),
  time_fields = c(
    "admission_time, orin_time, orout_time, discharge_time, anstart_time, anend_time",
    "chart_time",
    "chart_time",
    "chart_time",
    "chart_time",
    "chart_time"
  ),
  relative_time_note = c(
    "Main timeline reference table",
    "Relative time but no hadm_id/op_id",
    "Relative time but no hadm_id/op_id",
    "Relative time but no hadm_id/op_id",
    "Relative time but no hadm_id/op_id",
    "Relative time with op_id"
  ),
  join_risk = c("low", "high", "high", "high", "high", "low"),
  main_audit_note = c(
    "Can anchor other tables if a reliable admission/surgery mapping exists",
    "High risk of cross-admission contamination when joined only by subject_id",
    "High risk of cross-admission contamination when joined only by subject_id",
    "High risk of cross-admission contamination when joined only by subject_id",
    "High risk of cross-admission contamination when joined only by subject_id",
    "Best-linked source for intraoperative extraction"
  )
)

build_subject_link_metrics <- function(dt, table_name) {
  subject_ids <- unique(dt$subject_id)
  multi_subject_ids <- intersect(subject_ids, multi_hadm_subjects)
  impacted_ops <- uniqueN(ops[subject_id %in% multi_subject_ids, op_id])
  data.table(
    raw_table = table_name,
    record_n = nrow(dt),
    subject_n = uniqueN(dt$subject_id),
    multi_hadm_subject_n = length(multi_subject_ids),
    pct_subjects_multi_hadm = round(length(multi_subject_ids) / uniqueN(dt$subject_id) * 100, 2),
    potential_impacted_operation_n = impacted_ops,
    pct_all_operations_potentially_impacted = round(impacted_ops / nrow(ops) * 100, 2)
  )
}

linkage_risk_metrics <- rbindlist(list(
  data.table(
    raw_table = "operations",
    record_n = nrow(ops),
    subject_n = uniqueN(ops$subject_id),
    multi_hadm_subject_n = length(multi_hadm_subjects),
    pct_subjects_multi_hadm = round(length(multi_hadm_subjects) / uniqueN(ops$subject_id) * 100, 2),
    potential_impacted_operation_n = uniqueN(ops[subject_id %in% multi_hadm_subjects, op_id]),
    pct_all_operations_potentially_impacted = round(uniqueN(ops[subject_id %in% multi_hadm_subjects, op_id]) / nrow(ops) * 100, 2)
  ),
  build_subject_link_metrics(diag, "diagnosis"),
  build_subject_link_metrics(labs, "labs"),
  build_subject_link_metrics(meds, "medications"),
  build_subject_link_metrics(ward, "ward_vitals"),
  data.table(
    raw_table = "vitals",
    record_n = nrow(vitals),
    subject_n = uniqueN(vitals$subject_id),
    multi_hadm_subject_n = length(intersect(unique(vitals$subject_id), multi_hadm_subjects)),
    pct_subjects_multi_hadm = round(length(intersect(unique(vitals$subject_id), multi_hadm_subjects)) / uniqueN(vitals$subject_id) * 100, 2),
    potential_impacted_operation_n = 0L,
    pct_all_operations_potentially_impacted = 0
  )
), fill = TRUE)

diagnosis_join <- merge(
  ops[, .(op_id, subject_id, orin_time)],
  diag[, .(subject_id, chart_time)],
  by = "subject_id",
  allow.cartesian = TRUE
)
diag_ops_le0 <- unique(diagnosis_join[chart_time <= 0, op_id])
diag_ops_le_orin <- unique(diagnosis_join[chart_time <= orin_time, op_id])
diag_ops_between0orin <- unique(diagnosis_join[chart_time > 0 & chart_time <= orin_time, op_id])
diagnosis_sensitivity <- data.table(
  metric = c(
    "diagnosis_records_total",
    "diagnosis_records_chart_time_le_0",
    "operations_with_any_diag_chart_time_le_0",
    "operations_with_any_diag_chart_time_le_orin",
    "operations_with_diag_between_0_and_orin",
    "operations_newly_covered_if_le_orin"
  ),
  value = c(
    nrow(diag),
    diag[chart_time <= 0, .N],
    length(diag_ops_le0),
    length(diag_ops_le_orin),
    length(diag_ops_between0orin),
    length(setdiff(diag_ops_le_orin, diag_ops_le0))
  )
)

raw_time_distribution <- data.table(
  raw_table = c("diagnosis", "labs", "medications", "ward_vitals"),
  negative_time_n = c(
    diag[chart_time < 0, .N],
    labs[chart_time < 0, .N],
    meds[chart_time < 0, .N],
    ward[chart_time < 0, .N]
  ),
  zero_time_n = c(
    diag[chart_time == 0, .N],
    labs[chart_time == 0, .N],
    meds[chart_time == 0, .N],
    ward[chart_time == 0, .N]
  ),
  positive_time_n = c(
    diag[chart_time > 0, .N],
    labs[chart_time > 0, .N],
    meds[chart_time > 0, .N],
    ward[chart_time > 0, .N]
  )
)

module_audit <- data.table(
  module = c(
    "demographics_timeline",
    "diagnosis_preop",
    "labs_preop",
    "meds_preop",
    "vitals_preop",
    "intraop_vitals_and_sum",
    "outcomes_postop",
    "master_merge"
  ),
  phase = c("preop", "preop", "preop", "preop", "preop", "intraop", "postop", "periop"),
  current_setting = c(
    "operations.csv only; subject summary + op baseline + timeline/QC flags",
    "join operations and diagnosis by subject_id; keep chart_time <= 0; aggregate ICD groups by op_id",
    "join operations and labs by subject_id; keep chart_time <= orin_time; create any/30d/7d nearest median mean features",
    "join operations and medications by subject_id; keep admission_time to orin_time; classify drugs by ATC and name keywords",
    "ward vitals last 24h + OR vitals last 120 min before orin_time; ward preferred over OR",
    "use vitals.csv with op_id; keep orin_time to orout_time; output time series and summed exposures",
    "ICD postop complications after orin_time to discharge; AKI from baseline creatinine and postop window; mortality from death timestamps",
    "left join baseline + diagnosis + labs30d + meds + preop vitals + intraop sum + outcomes"
  ),
  join_key_review = c(
    "reasonable",
    "high-risk: subject_id only",
    "high-risk: subject_id only",
    "high-risk: subject_id only",
    "medium-risk: ward_vitals uses subject_id only; OR vitals use op_id",
    "low-risk: op_id-based",
    "medium-risk: diagnosis and labs subcomponents inherit subject_id-only risk",
    "inherits all upstream risks"
  ),
  time_window_review = c(
    "mostly reasonable; timeline contains periop preop/intraop/postop fields",
    "likely too strict; chart_time <= 0 may undercapture true preop diagnoses",
    "windows are clear but multiple windows and stats may be redundant",
    "window is clinically reasonable but depends on admission_time semantics",
    "window is clinically plausible; source fallback should be documented more fully",
    "orin_time to orout_time is clear and consistent",
    "postop windows are explicit but ICD complications may include preexisting conditions recorded after surgery",
    "master table spans periop phases and should be treated as a wide analysis table, not a phase-pure table"
  ),
  row_definition_review = c(
    "stable",
    "stable one row per op_id",
    "stable one row per op_id",
    "stable one row per op_id",
    "stable one row per op_id",
    "time-series plus operation-level sum are clearly separated",
    "stable one row per op_id",
    "stable one row per op_id"
  ),
  missing_and_zero_review = c(
    "subject-level means may hide within-patient variability",
    "flags are filled to 0; acceptable if interpreted as no qualifying diagnosis under current rule",
    "continuous labs correctly stay NA; no implicit zero fill",
    "flags filled to 0; risk of conflating no record with no medication exposure",
    "continuous vitals remain NA; source coverage metadata could be richer than source_sbp only",
    "summed exposures use 0 naturally when absent, but not all columns represent additive quantities",
    "postop binary outcomes filled to 0 is acceptable; Survival_Days retained as continuous field",
    "master fills many upstream NAs to 0; this is convenient but blurs source-level missingness"
  ),
  aggregation_review = c(
    "subject-level mean is simple but may not be ideal for all demographics",
    "max(flag) by op_id is reasonable once join logic is trustworthy",
    "nearest/median/mean may be more than needed for every lab item",
    "max(flag) by op_id is reasonable after classification",
    "mean aggregation for baseline vitals is reasonable within selected windows",
    "sum is appropriate for volumes and doses, but questionable for concentrations/rates such as ppfi, rfti, etsevo, etdes, etiso",
    "AKI stage logic is clinically structured; postop ICD event logic needs stricter interpretation",
    "merge logic is straightforward but should not be the only place source semantics are inferred"
  ),
  output_field_review = c(
    "periop timeline phases in one table should be tagged more explicitly",
    "output fields are clear but definition should mention current strict chart_time rule",
    "output fields are clear but could be simplified to one preferred window if needed",
    "drug category outputs are clear; exposure source uncertainty should be documented",
    "output fields are clear; add broader source metadata if retained for interpretation",
    "time-series and sum outputs are clear but variable suitability for sum must be reviewed",
    "outcome fields are clear; distinguish incident postop events from diagnosis recapture risk",
    "master output should be documented as a periop wide table"
  ),
  must_fix = c("no", "yes", "yes", "yes", "no", "yes", "yes", "no"),
  priority = c("medium", "high", "high", "high", "medium", "high", "high", "medium"),
  recommendation = c(
    "Keep current structure but explicitly tag phase/time_anchor per field",
    "Re-review diagnosis window; compare <=0 versus admission-to-orin logic and document chosen rule",
    "Treat subject_id-only linkage as high-risk until admission-safe logic is available; reconsider whether all three windows and all three stats are needed",
    "Document that 0 can mean no matched record under current join logic; verify ATC + keyword rules for overlap",
    "Keep current extraction but expand source metadata beyond source_sbp if interpretability matters",
    "Split additive dose/volume variables from non-additive rate or concentration variables before using total-sum output",
    "Re-review postop ICD logic to avoid counting preexisting diagnoses merely charted after surgery; keep AKI and mortality logic but document anchors clearly",
    "Keep master as final wide table but do not use it as the only semantic source of truth"
  )
)

findings_priority <- data.table(
  severity = c("high", "high", "high", "high", "medium", "medium"),
  finding = c(
    "diagnosis/labs/medications/ward_vitals rely on subject_id-only linkage despite many multi-admission subjects",
    "diagnosis current preop definition uses chart_time <= 0 and likely undercaptures admission-to-surgery diagnoses",
    "intraop total-sum table mixes additive quantities with non-additive concentration/rate variables",
    "postop ICD complications may include preexisting conditions charted after surgery during the same stay",
    "periop_merged_dataset mixes preop, intraop, postop, QC, and outcome fields in one table",
    "medication and master zero-fill rules should be interpreted carefully because missing record and true absence can look the same"
  ),
  why_it_matters = c(
    "Can contaminate extracted preop features across admissions or surgeries",
    "Can miss true preoperative diagnosis burden and make prevalence unstable",
    "Can create misleading exposure summaries for physiologically non-additive variables",
    "Can overstate postoperative event incidence",
    "Makes downstream use easier but semantics harder to track",
    "Can distort interpretation if users assume 0 always means biologically or clinically absent"
  ),
  suggested_action = c(
    "Must review and, if possible, redesign linkage or explicitly mark these modules as high-risk",
    "Run side-by-side comparison and choose a clinically defensible preop rule",
    "Separate additive sum variables from rate/concentration variables or change aggregation",
    "Review whether incident-event logic needs stronger exclusion of preexisting diagnosis carryover",
    "Keep the table but strengthen field-level metadata and docs",
    "Keep fill rules only if clearly documented in dictionary and audit outputs"
  )
)

build_processed_table_audit <- function(spec_row) {
  spec_row <- as.list(spec_row)
  path <- file.path(processed_dir, spec_row[["file_name"]][[1]])
  dt0 <- fread(path, nrows = 0L)
  keep_cols <- intersect(c("op_id", "subject_id", "chart_time", "min_from_entry"), names(dt0))
  dt <- if (length(keep_cols) > 0L) fread(path, select = keep_cols) else data.table()
  key_cols <- trimws(unlist(strsplit(spec_row[["primary_key_definition"]][[1]], "\\+")))
  key_cols <- key_cols[key_cols %in% names(dt)]
  duplicate_n <- if (length(key_cols) > 0L) sum(duplicated(dt[, ..key_cols])) else NA_integer_
  unique_op_n <- if ("op_id" %in% names(dt)) uniqueN(dt$op_id) else NA_integer_
  unique_subject_n <- if ("subject_id" %in% names(dt)) uniqueN(dt$subject_id) else NA_integer_
  row_count_value <- as.integer(system(sprintf("wc -l < %s", shQuote(path)), intern = TRUE))
  data.table(
    table_name = spec_row[["table_name"]][[1]],
    file_name = spec_row[["file_name"]][[1]],
    row_count = row_count_value,
    column_count = ncol(dt0),
    key_definition = spec_row[["primary_key_definition"]][[1]],
    duplicate_key_n = duplicate_n,
    unique_op_id_n = unique_op_n,
    unique_subject_id_n = unique_subject_n,
    phase = spec_row[["phase"]][[1]],
    time_anchor = spec_row[["time_anchor"]][[1]],
    table_role = spec_row[["table_role"]][[1]]
  )
}

processed_table_audit <- rbindlist(
  lapply(seq_len(nrow(processed_specs)), function(i) build_processed_table_audit(processed_specs[i, ])),
  fill = TRUE
)
processed_table_audit[, row_count := row_count - 1L]

phase_catalog <- processed_specs[, .(
  table_name,
  file_name,
  phase,
  time_anchor,
  table_role,
  recommended_use = fifelse(
    phase == "postop", "outcome table",
    fifelse(phase == "intraop", "intraop feature table", fifelse(phase == "preop", "preop feature table", "periop-purpose table"))
  )
)]

summary_lines <- c(
  "# INSPIRE Extraction Audit Summary",
  "",
  "## Overall",
  "",
  sprintf("- operations 总数：%s", format(nrow(ops), big.mark = ",")),
  sprintf("- subject 总数：%s", format(uniqueN(ops$subject_id), big.mark = ",")),
  sprintf("- hadm 总数：%s", format(uniqueN(ops$hadm_id), big.mark = ",")),
  sprintf("- 多次住院病人数：%s", format(length(multi_hadm_subjects), big.mark = ",")),
  sprintf("- 来自多次住院病人的手术数：%s", format(uniqueN(ops[subject_id %in% multi_hadm_subjects, op_id]), big.mark = ",")),
  "",
  "## Top Findings",
  "",
  "1. `diagnosis`、`labs`、`medications`、`ward_vitals` 只有 `subject_id`，没有 `hadm_id/op_id`，当前提取逻辑存在跨住院串联风险。",
  sprintf("2. diagnosis 当前 `chart_time <= 0` 可覆盖 %s 台手术；若放宽到 `chart_time <= orin_time`，可覆盖 %s 台手术；真正新增覆盖约 %s 台手术，另有 %s 台手术在 0 到 orin_time 间存在诊断记录。",
    format(diagnosis_sensitivity[metric == "operations_with_any_diag_chart_time_le_0", value], big.mark = ","),
    format(diagnosis_sensitivity[metric == "operations_with_any_diag_chart_time_le_orin", value], big.mark = ","),
    format(diagnosis_sensitivity[metric == "operations_newly_covered_if_le_orin", value], big.mark = ","),
    format(diagnosis_sensitivity[metric == "operations_with_diag_between_0_and_orin", value], big.mark = ",")
  ),
  "3. intraop 汇总表里有些变量适合求和（液体量、失血量、尿量、推注剂量），但有些变量是浓度或持续速率，不适合直接求和。",
  "4. postop ICD 并发症当前按“术后到出院前”提取，仍需检查是否会把本次住院原有问题误计为术后事件。",
  "5. `periop_master_dataset_all_features.csv` 适合做宽表分析，但它混合了术前、术中、术后和 QC 字段，不能替代字段级语义说明。",
  "",
  "## Recommended Next Fixes",
  "",
  "- 优先复核 subject_id-only 模块的连接策略，并把这些模块继续标记为高风险。",
  "- 对 diagnosis 做 `<=0` 与 `<=orin_time` 的正式对比后，再确定统一术前定义。",
  "- 把 intraop 汇总变量分成“可求和”和“不可直接求和”两类。",
  "- 在数据字典里保留 `phase`、`time_anchor`、`join_risk` 字段，让每个 processed 字段都能追溯其阶段和风险。"
)

writeLines(summary_lines, audit_summary_path)

wb <- openxlsx::createWorkbook()

add_sheet <- function(sheet_name, dt) {
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = as.data.frame(dt), withFilter = TRUE)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dt), widths = "auto")
}

add_sheet("raw_table_review", raw_table_review)
add_sheet("linkage_risk_metrics", linkage_risk_metrics)
add_sheet("diagnosis_sensitivity", diagnosis_sensitivity)
add_sheet("raw_time_distribution", raw_time_distribution)
add_sheet("module_audit", module_audit)
add_sheet("findings_priority", findings_priority)
add_sheet("processed_table_audit", processed_table_audit)
add_sheet("phase_catalog", phase_catalog)

openxlsx::saveWorkbook(wb, audit_workbook_path, overwrite = TRUE)

cat(sprintf("Audit workbook written to %s\n", audit_workbook_path))
cat(sprintf("Audit summary written to %s\n", audit_summary_path))
