#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

input_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Diagnosis_word_comorbidities_first_nonMAC_3_30_2026"

main_file <- file.path(input_dir, "comorbidity_word_defined_anchor_first_nonMAC.csv")
prev_file <- file.path(input_dir, "comorbidity_prevalence_summary.csv")
cat_file <- file.path(input_dir, "comorbidity_category_summary.csv")
audit_file <- file.path(input_dir, "comorbidity_word_definition_audit.csv")

cat("Loading comorbidity outputs...\n")

dt_main <- fread(main_file)
dt_prev <- fread(prev_file)
dt_cat <- fread(cat_file)
dt_audit <- fread(audit_file)

total_n <- nrow(dt_main)

status_map <- c(
  matched = "一致",
  partial = "部分不一致",
  missing = "旧版未实现"
)

issue_map <- c(
  "old script added O10-O16 and ignored medication rule" = "旧版额外纳入了 O10-O16，且未合并用药 ATC 规则。",
  "old script named CAD and mixed with angina/MI decomposition" = "旧版用 CAD/心绞痛/心梗拆分代理，和 Word 的缺血性心脏病口径不完全一致。",
  "not implemented in old script" = "旧版未单独实现该变量。",
  "old script had ICD-only plus a broader arrhythmia_any concept" = "旧版只用了 ICD，且混入了更宽泛的 arrhythmia_any 概念。",
  "implemented as AF only; wording differs slightly" = "旧版仅实现 AF，变量命名和 Word 略有差异。",
  "generally aligned" = "旧版与 Word 基本一致。",
  "old script missed G46" = "旧版漏掉了 G46。",
  "old script missed F05/G31" = "旧版漏掉了 F05 和 G31。",
  "old script only used J44" = "旧版仅使用 J44，未覆盖 J41-J43。",
  "aligned" = "旧版与 Word 一致。",
  "old script missed N03-N08 and category split" = "旧版漏掉 N03-N08，且没有肾病分级。",
  "old script only used B18/K70/K73/K74" = "旧版仅覆盖 B18、K70、K73、K74，未完整覆盖 K70-K77。",
  "old script did not use BMI>=30" = "旧版未纳入 BMI >= 30 的定义。",
  "old script used E08/E09 and missed medication rule" = "旧版使用了 E08/E09，且未纳入 A10**** 用药规则。",
  "old script used Hb minimum fallback but no dedicated preop anemia split" = "旧版虽用了 Hb 最低值，但没有单独区分术前贫血和术前贫血分级。",
  "old script included broader C00-C99 minus C77-C80 proxy" = "旧版采用更宽泛的恶性肿瘤代理口径，并非严格的 C00-C76、C81-C96。"
)

binary_meta <- data.table(
  variable = c(
    "hypertension", "ischemic_heart_disease", "heart_failure", "arrhythmia",
    "atrial_fibrillation_flutter", "pulmonary_hypertension", "peripheral_vascular_disease",
    "cerebrovascular_disease", "dementia", "parkinsonism", "copd", "asthma",
    "renal_disease", "renal_dialysis", "chronic_liver_disease", "peptic_ulcer_disease",
    "gerd", "obesity", "diabetes", "hyperlipidemia", "anemia_icd10",
    "anemia_preoperative", "connective_tissue_disease", "malignancy"
  ),
  中文名称 = c(
    "高血压", "缺血性心脏病", "心力衰竭", "心律失常",
    "房颤/房扑", "肺动脉高压", "外周血管病",
    "脑血管疾病", "痴呆", "帕金森病/帕金森综合征", "慢性阻塞性肺疾病", "哮喘",
    "肾脏疾病", "肾透析", "慢性肝病", "消化性溃疡",
    "胃食管反流病", "肥胖", "糖尿病", "高脂血症", "贫血（ICD/Hb最小值）",
    "术前贫血", "结缔组织病", "恶性肿瘤"
  ),
  定义 = c(
    "术前首次非MAC手术的 OR 入室时间前，diagnosis 中 ICD-10 为 I10-I16，或 medications 中 ATC 为 C02**** / C08C***。",
    "术前 ICD-10 为 I20-I25。",
    "术前 ICD-10 为 I42、I43、I50。",
    "术前 ICD-10 为 I47-I49，或 medications 中 ATC 为 C01B*** / C08D** / C07A**。",
    "术前 ICD-10 为 I48。",
    "术前 ICD-10 为 I27。",
    "术前 ICD-10 为 I70、I71、I73、K55。",
    "术前 ICD-10 为 I60-I69、G45-G46。",
    "术前 ICD-10 为 F01-F03、F05、G30、G31。",
    "术前 ICD-10 为 G20-G22。",
    "术前 ICD-10 为 J41-J44。",
    "术前 ICD-10 为 J45。",
    "术前 ICD-10 为 N03-N08、N18、N19、I12、I13、Z49、Z94、Z99。",
    "术前 ICD-10 为 Z49。",
    "术前 ICD-10 为 B18、K70-K77。",
    "术前 ICD-10 为 K25-K28。",
    "术前 ICD-10 为 K21。",
    "术前 ICD-10 为 E66，或 BMI >= 30。",
    "术前 ICD-10 为 E10-E14，或 medications 中 ATC 为 A10****。",
    "术前 ICD-10 为 E78，或 medications 中 ATC 为 C10A***。",
    "术前 ICD-10 为 D50-D64，或术前最小 Hb 男 <13 / 女 <12 g/dL。",
    "术前最后一次 Hb 男 <13 / 女 <12 g/dL。",
    "术前 ICD-10 为 M05、M06、M31-M35。",
    "术前 ICD-10 为 C00-C76、C81-C96。"
  )
)

category_meta <- data.table(
  variable = c(
    rep("renal_disease_category", 5),
    rep("diabetes_category", 2),
    rep("anemia_preop_severity", 3)
  ),
  category = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 1L, 2L, 3L),
  中文名称 = c(
    rep("肾病分级", 5),
    rep("糖尿病分型", 2),
    rep("术前贫血严重度", 3)
  ),
  分类说明 = c(
    "eGFR >= 90",
    "eGFR 60-90",
    "eGFR 30-60",
    "eGFR 15-30",
    "eGFR < 15",
    "1 = 胰岛素依赖型（A10A***）",
    "2 = 非胰岛素依赖型/其他糖尿病",
    "1 = 轻度：最后一次 Hb >= 10 g/dL",
    "2 = 中度：最后一次 Hb 7-9.9 g/dL",
    "3 = 重度：最后一次 Hb < 7 g/dL"
  ),
  定义 = c(
    rep("肾病患者中，取首次非MAC手术前 3 个月内最近 2 次肌酐均值，按 Word 公式 eGFR = 175 * Scr^-1.154 * Age^-0.203 * 0.742(女性) 计算并分级。", 5),
    rep("糖尿病患者中，若术前存在 A10A*** 记为胰岛素依赖型，否则归入非胰岛素依赖型/其他。", 2),
    rep("仅在术前贫血 = 1 的人群中，根据术前最后一次 Hb 分为轻/中/重度。", 3)
  )
)

asa_summary <- dt_main[, .N, by = .(category = asa)][order(category)]
asa_summary[, `:=`(
  variable = "asa",
  中文名称 = "ASA分级",
  分类说明 = fifelse(
    is.na(category),
    "ASA 缺失",
    fifelse(
      category %in% 1:5,
      paste0("ASA = ", category),
      paste0("ASA = ", category, "（超出 Word 定义 1-5 范围）")
    )
  ),
  定义 = "直接取 operations.csv 中首次非MAC锚定手术对应的 asa。",
  例数 = N,
  总例数 = total_n,
  百分比 = round(100 * N / total_n, 2),
  与旧版一致性 = "未比较",
  旧版主要问题 = "旧版 Diagnosis 脚本未单独审计 ASA。"
)]
asa_summary <- asa_summary[, .(
  变量名 = variable,
  中文名称,
  类型 = "ASA",
  分类值 = category,
  分类说明,
  定义,
  例数,
  总例数,
  百分比,
  与旧版一致性,
  旧版主要问题
)]

binary_summary <- merge(binary_meta, dt_prev, by = "variable", all.x = TRUE)
binary_summary <- merge(binary_summary, dt_audit, by = "variable", all.x = TRUE)
binary_summary[, `:=`(
  类型 = "二元变量",
  分类值 = 1L,
  分类说明 = "1 = 有该既往史",
  例数 = n_cases,
  总例数 = total_ops,
  百分比 = prevalence_pct,
  与旧版一致性 = fifelse(
    is.na(old_script_status),
    "未比较",
    status_map[old_script_status]
  ),
  旧版主要问题 = fifelse(is.na(main_issue), "", issue_map[main_issue])
)]
binary_summary <- binary_summary[, .(
  变量名 = variable,
  中文名称,
  类型,
  分类值,
  分类说明,
  定义,
  例数,
  总例数,
  百分比,
  与旧版一致性,
  旧版主要问题
)]

category_summary <- merge(category_meta, dt_cat, by = c("variable", "category"), all.x = TRUE)
category_summary <- merge(category_summary, dt_audit, by = "variable", all.x = TRUE)
category_summary[, `:=`(
  类型 = "分级变量",
  例数 = fifelse(is.na(N), 0L, N),
  总例数 = total_n,
  百分比 = fifelse(is.na(prevalence_pct), 0, prevalence_pct),
  与旧版一致性 = fifelse(
    is.na(old_script_status),
    "未比较",
    status_map[old_script_status]
  ),
  旧版主要问题 = fifelse(is.na(main_issue), "", issue_map[main_issue])
)]
category_summary <- category_summary[, .(
  变量名 = variable,
  中文名称,
  类型,
  分类值 = category,
  分类说明,
  定义,
  例数,
  总例数,
  百分比,
  与旧版一致性,
  旧版主要问题
)]

consistency_summary <- dt_audit[, .N, by = old_script_status][order(old_script_status)]
consistency_summary[, 中文分类 := fifelse(
  old_script_status %in% names(status_map),
  status_map[old_script_status],
  old_script_status
)]

all_summary <- rbindlist(
  list(binary_summary, category_summary, asa_summary),
  use.names = TRUE,
  fill = TRUE
)

setorder(all_summary, 类型, 中文名称, 分类值)

csv_file <- file.path(input_dir, "comorbidity_summary_cn_readable.csv")
binary_csv <- file.path(input_dir, "comorbidity_binary_summary_cn.csv")
category_csv <- file.path(input_dir, "comorbidity_category_summary_cn.csv")
asa_csv <- file.path(input_dir, "comorbidity_asa_summary_cn.csv")
xlsx_file <- file.path(input_dir, "comorbidity_summary_cn_readable.xlsx")

cat("Writing CSV outputs...\n")
fwrite(all_summary, csv_file)
fwrite(binary_summary, binary_csv)
fwrite(category_summary, category_csv)
fwrite(asa_summary, asa_csv)

cat("Writing Excel workbook...\n")
wb <- createWorkbook()

addWorksheet(wb, "总表")
writeDataTable(wb, "总表", all_summary)
setColWidths(wb, "总表", cols = 1:ncol(all_summary), widths = "auto")

addWorksheet(wb, "二元既往史")
writeDataTable(wb, "二元既往史", binary_summary)
setColWidths(wb, "二元既往史", cols = 1:ncol(binary_summary), widths = "auto")

addWorksheet(wb, "分级变量")
writeDataTable(wb, "分级变量", category_summary)
setColWidths(wb, "分级变量", cols = 1:ncol(category_summary), widths = "auto")

addWorksheet(wb, "ASA")
writeDataTable(wb, "ASA", asa_summary)
setColWidths(wb, "ASA", cols = 1:ncol(asa_summary), widths = "auto")

addWorksheet(wb, "一致性汇总")
writeDataTable(wb, "一致性汇总", consistency_summary)
setColWidths(wb, "一致性汇总", cols = 1:ncol(consistency_summary), widths = "auto")

saveWorkbook(wb, xlsx_file, overwrite = TRUE)

cat("Done.\n")
cat("CSV:", csv_file, "\n")
cat("XLSX:", xlsx_file, "\n")
