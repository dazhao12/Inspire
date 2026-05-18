suppressPackageStartupMessages({
  library(data.table)
})

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required to build the audit workbook.")
}

project_root <- "/N/project/analgesia_perioperation"
raw_dir <- file.path(project_root, "data", "INSPIRE_1.3", "raw")
processed_dir <- file.path(project_root, "data", "INSPIRE_1.3", "processed")
docs_dir <- file.path(project_root, "projects", "Inspire_data_process_ZZ", "docs")
workbook_path <- file.path(docs_dir, "INSPIRE_extraction_audit_cn.xlsx")
report_path <- file.path(docs_dir, "INSPIRE_extraction_audit_cn.md")

if (!dir.exists(docs_dir)) {
  dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
}

ops <- fread(file.path(raw_dir, "operations.csv"),
             select = c("op_id", "subject_id", "hadm_id", "admission_time", "orin_time", "orout_time", "discharge_time"))
diag <- fread(file.path(raw_dir, "diagnosis.csv"),
              select = c("subject_id", "chart_time", "icd10_cm"))
labs <- fread(file.path(raw_dir, "labs.csv"),
              select = c("subject_id", "chart_time", "item_name", "value"))
meds <- fread(file.path(raw_dir, "medications.csv"),
              select = c("subject_id", "chart_time", "drug_name"))
ward <- fread(file.path(raw_dir, "ward_vitals.csv"),
              select = c("subject_id", "chart_time", "item_name", "value"))
vitals <- fread(file.path(raw_dir, "vitals.csv"),
                select = c("op_id", "subject_id", "chart_time", "item_name", "value"))

by_subject <- ops[, .(n_ops = .N, n_hadm = uniqueN(hadm_id)), by = subject_id]
subjects_multi_ops <- by_subject[n_ops > 1, .N]
subjects_multi_hadm <- by_subject[n_hadm > 1, .N]

diag_join <- merge(ops[, .(op_id, subject_id, orin_time)], diag[, .(subject_id, chart_time)], by = "subject_id", allow.cartesian = TRUE)
diag_ops_le_zero <- uniqueN(diag_join[chart_time <= 0, op_id])
diag_ops_le_orin <- uniqueN(diag_join[chart_time <= orin_time, op_id])
diag_ops_between_zero_orin <- uniqueN(diag_join[chart_time > 0 & chart_time <= orin_time, op_id])

processed_specs <- data.table(
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
  key_col = c("subject_id", rep("op_id", 13))
)

processed_checks <- rbindlist(lapply(seq_len(nrow(processed_specs)), function(i) {
  spec <- processed_specs[i]
  dt <- fread(file.path(processed_dir, spec$file_name), select = spec$key_col)
  data.table(
    file_name = spec$file_name,
    key_col = spec$key_col,
    n_rows = nrow(dt),
    n_unique_key = uniqueN(dt[[spec$key_col]]),
    duplicate_key_n = nrow(dt) - uniqueN(dt[[spec$key_col]])
  )
}))

raw_tables <- data.table(
  raw_table = c("operations", "diagnosis", "labs", "medications", "ward_vitals", "vitals"),
  current_link_key = c("op_id / subject_id / hadm_id", "subject_id", "subject_id", "subject_id", "subject_id", "op_id"),
  time_field = c("admission_time / orin_time / orout_time / discharge_time", "chart_time", "chart_time", "chart_time", "chart_time", "chart_time"),
  current_risk = c("low", "high", "high", "high", "high", "low"),
  chinese_note = c(
    "主手术轴表，键最完整，适合作为所有 operation-level 表的基准",
    "无 hadm_id/op_id，按 subject_id 连接时容易跨住院串联",
    "无 hadm_id/op_id，按 subject_id 连接时容易跨住院串联",
    "无 hadm_id/op_id，按 subject_id 连接时容易跨住院串联",
    "无 hadm_id/op_id，按 subject_id 连接时容易跨住院串联",
    "有 op_id，术中连接最稳"
  )
)

module_audit <- data.table(
  phase = c("preop", "preop", "preop", "preop", "preop", "intraop", "intraop", "postop", "postop", "postop", "processed"),
  module = c(
    "demographics_timeline",
    "diagnosis",
    "labs",
    "meds",
    "preop_vitals",
    "intraop_timeseries",
    "intraop_summary",
    "postop_icd_complications",
    "AKI",
    "mortality",
    "master_dataset_final"
  ),
  current_logic = c(
    "由 operations.csv 生成 subject-level 和 operation-level demographics，以及 timeline durations 和 QC flags",
    "按 subject_id 连接 diagnosis，当前术前定义为 chart_time <= 0",
    "按 subject_id 连接 labs，窗口为 any / 30d / 7d，计算 nearest / median / mean",
    "按 subject_id 连接 medications，窗口为 admission_time 到 orin_time，ATC + 药名关键词打标",
    "病房 24h + OR 前 120 min，Ward 优先，OR 兜底",
    "按 op_id 连接 vitals，窗口为 orin_time 到 orout_time，生成完整术中时序宽表",
    "对术中药物/液体/血制品等变量直接 sum，另外做 summary 脚本",
    "术后 ICD 限定在术后到出院前",
    "baseline creatinine 取 admission 到术前最低值，postop creatinine 取 OR 出室到 min(出院, 7天)",
    "基于 admission/anend/discharge/death_time 定义院内死亡和 30/90/365 天死亡",
    "把术前、术中、术后字段全部合到一张 operation-level 宽表"
  ),
  main_problem = c(
    "subject-level 平均值会弱化患者随时间变化；baseline_full 混合了 preop / intraop / postop / QC 字段",
    "chart_time <= 0 可能过严；subject_id 连接有跨住院串联风险",
    "subject_id 连接高风险；three windows x three stats 变量量大且可能冗余",
    "subject_id 连接高风险；补 0 可能把无记录与未用药混在一起",
    "Ward 与 OR 逻辑基本合理，但来源字段过少，且 ward_vitals 仍有 subject_id 连接风险",
    "时序表逻辑较稳，但变量集合很大，直接用于分析前需要按用途筛选",
    "并非所有术中变量都适合直接求和，尤其持续输入或浓度类变量需要再解释",
    "可能把本次住院原有诊断误当成术后新发并发症",
    "AKI 规则总体清楚，但 baseline/postop 窗口需要继续确认是否符合预期临床口径",
    "死亡定义整体一致，但生存时间起点是否使用 anend_time 仍需明确记录",
    "适合总体分析，不适合直接拿来解释提取阶段；字段阶段混合严重"
  ),
  severity = c("medium", "high", "high", "high", "medium", "low", "medium", "medium", "medium", "low", "medium"),
  must_fix = c("no", "yes", "yes", "yes", "recommended", "no", "recommended", "recommended", "recommended", "recommended", "yes"),
  recommendation = c(
    "在字典中明确 phase；保留 operation-level 为主，subject-level 只用于患者层概况",
    "至少比较 chart_time <= 0 与 chart_time <= orin_time 的差异，并单独标记 subject_id 连接风险",
    "优先确定一个 canonical lab 窗口；评估 any/7d/30d 是否全部需要",
    "保留药物分类，但在文档中区分 无记录 与 未暴露；复核高频类别关键词",
    "增加来源字段说明，不仅限于 source_sbp；明确 ward/or 优先逻辑",
    "保留当前 op_id 连接逻辑，后续按分析目的拆成时序版与汇总版",
    "按变量物理意义区分 sum / mean / last / exposure-any 的更合理聚合方式",
    "补充说明这是 术后到出院前诊断事件，不一定等同于新发事件",
    "补充中文规则说明，单独记录 baseline 和 postop creatinine 的时间锚点",
    "把死亡定义写进说明文档，明确 Survival_Days 的起点",
    "在数据字典中为每列补 phase / time_anchor / join_risk"
  )
)

processed_roles <- data.table(
  file_name = c(
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
  phase_role = c(
    "periop",
    "preop",
    "periop_intraop_postop",
    "preop",
    "preop",
    "preop",
    "preop",
    "preop",
    "preop",
    "intraop",
    "intraop",
    "postop",
    "mixed_all_phases"
  ),
  usage_note = c(
    "主 baseline 表，但含 timeline / QC / postop 时长字段",
    "最适合单独作为术前 baseline 子表",
    "适合看手术和住院后半段相关 timeline",
    "术前诊断 flags",
    "术前 7 天 labs",
    "术前 30 天 labs，当前 master merge 默认使用这张",
    "任意术前历史 labs",
    "术前用药 flags",
    "术前 baseline vitals",
    "完整术中时序",
    "术中汇总暴露",
    "术后结局表",
    "全宽表，适合总体分析，不适合直接表达阶段边界"
  )
)

overview <- data.table(
  metric = c(
    "手术总数",
    "患者总数",
    "住院总数",
    "多次手术患者数",
    "多次住院患者数",
    "diagnosis 表 chart_time <= 0 的记录数",
    "diagnosis 术前覆盖 op_id 数（<=0）",
    "diagnosis 术前覆盖 op_id 数（<=orin_time）",
    "diagnosis 在 0 到 orin_time 之间新增涉及的 op_id 数"
  ),
  value = c(
    nrow(ops),
    uniqueN(ops$subject_id),
    uniqueN(ops$hadm_id),
    subjects_multi_ops,
    subjects_multi_hadm,
    sum(diag$chart_time <= 0, na.rm = TRUE),
    diag_ops_le_zero,
    diag_ops_le_orin,
    diag_ops_between_zero_orin
  ),
  interpretation = c(
    "operation-level 基准行数",
    "患者层样本量",
    "住院层样本量",
    "说明同一患者可能有多台手术",
    "说明按 subject_id 连接存在跨住院风险",
    "当前 diagnosis 脚本主要使用这部分记录",
    "当前 diagnosis 提取的 op_id 覆盖",
    "若按 入院后到手术前 定义术前 diagnosis 的 op_id 覆盖",
    "说明 <=0 与 <=orin_time 之间仍有额外术前记录"
  )
)

improvement_actions <- data.table(
  priority = c("P0", "P0", "P0", "P1", "P1", "P1", "P2"),
  action = c(
    "在所有 subject_id 连接模块中单独标记 admission-linkage 风险",
    "重审 diagnosis 的术前时间窗定义",
    "为 processed 字典补 phase / time_anchor / join_risk",
    "评估 labs 的 any / 30d / 7d 是否都保留",
    "区分 meds 的 无记录 和 未暴露 解释",
    "重审术中变量哪些适合 sum",
    "把 master 宽表拆解说明为 preop / intraop / postop 三段逻辑"
  ),
  expected_effect = c(
    "让使用者明确哪些模块有跨住院串联风险",
    "避免 diagnosis 术前定义过严或解释不清",
    "提高 processed 可解释性和追溯性",
    "减少不必要特征冗余",
    "提高 meds 解释透明度",
    "减少对术中汇总变量的误读",
    "让最终宽表更容易被临床和分析人员理解"
  )
)

wb <- openxlsx::createWorkbook()
add_sheet <- function(sheet_name, dt) {
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = as.data.frame(dt), withFilter = TRUE)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dt), widths = "auto")
}

add_sheet("overview_cn", overview)
add_sheet("raw_tables_cn", raw_tables)
add_sheet("module_audit_cn", module_audit)
add_sheet("processed_checks_cn", processed_checks)
add_sheet("processed_roles_cn", processed_roles)
add_sheet("actions_cn", improvement_actions)

openxlsx::saveWorkbook(wb, workbook_path, overwrite = TRUE)

report_lines <- c(
  "# INSPIRE 提取逻辑中文审查",
  "",
  "## 总结",
  "",
  sprintf("- 当前手术总数：%s", format(nrow(ops), big.mark = ",")),
  sprintf("- 当前患者总数：%s", format(uniqueN(ops$subject_id), big.mark = ",")),
  sprintf("- 当前住院总数：%s", format(uniqueN(ops$hadm_id), big.mark = ",")),
  sprintf("- 有多次手术的患者数：%s", format(subjects_multi_ops, big.mark = ",")),
  sprintf("- 有多次住院的患者数：%s", format(subjects_multi_hadm, big.mark = ",")),
  "",
  "## 目前最重要的问题",
  "",
  "1. `diagnosis`、`labs`、`medications`、`ward_vitals` 只有 `subject_id`，没有 `hadm_id/op_id`，当前代码按 `subject_id` 去连时存在跨住院串联风险。",
  "2. diagnosis 当前使用 `chart_time <= 0` 定义术前，较保守；按 `chart_time <= orin_time` 时，术前 diagnosis 记录覆盖会更宽。",
  "3. `periop_master_dataset_all_features.csv` 把术前、术中、术后和 QC 字段混在一起，适合总体分析，但不利于解释提取阶段。",
  "",
  "## diagnosis 时间窗核对",
  "",
  sprintf("- 当前 `chart_time <= 0` 覆盖的 op_id 数：%s", format(diag_ops_le_zero, big.mark = ",")),
  sprintf("- 如果改成 `chart_time <= orin_time`，覆盖的 op_id 数：%s", format(diag_ops_le_orin, big.mark = ",")),
  sprintf("- `0 ~ orin_time` 之间仍有 diagnosis 记录的 op_id 数：%s", format(diag_ops_between_zero_orin, big.mark = ",")),
  "",
  "## 术前、术中、术后的总体判断",
  "",
  "- 术前：问题最多，重点在 diagnosis/labs/meds/ward_vitals 的连接方式和时间窗。",
  "- 术中：`vitals.csv` 因为有 `op_id`，逻辑相对最稳；主要要优化的是汇总方式是否符合变量物理意义。",
  "- 术后：结局逻辑总体清楚，但 ICD 并发症更像“术后到出院前记录的事件”，需要避免被误解成“确定新发”。",
  "",
  "## 建议优先级",
  "",
  "- P0：标记并解释 subject_id 连接风险。",
  "- P0：重审 diagnosis 的术前定义。",
  "- P0：在数据字典里补 `phase / time_anchor / join_risk`。",
  "- P1：重审 labs 三种窗口是否都保留。",
  "- P1：重审 meds 的补 0 解释。",
  "- P1：重审 intraop sum 是否适用于所有变量。",
  "",
  sprintf("详细结构化结果见：`%s`", workbook_path)
)

writeLines(report_lines, report_path, useBytes = TRUE)

cat(sprintf("Audit workbook written to %s\n", workbook_path))
cat(sprintf("Audit markdown written to %s\n", report_path))
