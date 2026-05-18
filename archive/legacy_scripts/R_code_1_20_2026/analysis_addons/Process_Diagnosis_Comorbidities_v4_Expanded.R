library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(data.table)

# ==============================================================================
# 1. 设置路径与读取数据
# ==============================================================================
path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
path_processed_base <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_folder_name <- "Diagnosis_Comorbidities_2_19_2026"
path_output <- file.path(path_processed_base, output_folder_name)

if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE)
}

cat("正在读取数据...\n")

# 读取手术表
col_types_ops <- cols(
  op_id = col_character(),
  subject_id = col_character(),
  hadm_id = col_character(),
  case_id = col_character(),
  opdate = col_double()
)

df_ops <- read_csv(file.path(path_raw, "operations.csv"), col_types = col_types_ops) %>%
  select(op_id, subject_id, hadm_id, case_id, opdate)

# 读取诊断表
col_types_diag <- cols(
  subject_id = col_character(),
  chart_time = col_double(),
  icd10_cm = col_character()
)
df_diag <- read_csv(file.path(path_raw, "diagnosis.csv"), col_types = col_types_diag)

# ==============================================================================
# 2. 数据清洗与关联
# ==============================================================================
cat("正在清洗 ICD 代码...\n")

# 预处理: 此数据源仅包含3位代码 (已验证)，所以我们需要将 Elixhauser 定义截断到3位
# 去除小数点，转大写，去空格
df_diag_clean <- df_diag %>%
  mutate(
    # 移除小数点，大写，去空格
    icd_clean = str_replace_all(str_to_upper(str_trim(icd10_cm)), "\\.", "")
  ) %>%
  filter(!is.na(icd_clean))

cat("正在关联数据 (Operations + Diagnosis)...\n")
# 关联: 仅保留术前诊断 (chart_time <= 0 或 NA)
df_merged <- df_ops %>%
  left_join(df_diag_clean, by = "subject_id") %>%
  filter(chart_time <= 0 | is.na(chart_time))

dt_merged <- as.data.table(df_merged)

# ==============================================================================
# 3. 特征工程：基于 Elixhauser 定义 (适配 3 位 ICD 代码) + V1 定义补充
# ==============================================================================
cat("正在计算 Elixhauser 并发症标志 (使用 3 位代码适配) + V1 补充定义...\n")
cat("正在补充 E14 系列代码...\n")
cat("注意：本脚本输出为 ICD3 proxy（非标准 CCI/Elixhauser 全精度实现）。\n")

# 辅助函数: 检查ICD是否在给定的3位代码列表中
check_codes_3digit <- function(icd_col, full_code_list) {
  # 1. 截取定义列表的前3位
  prefixes <- unique(substr(full_code_list, 1, 3))
  # 2. 检查 icd_col (本身已经是3位) 是否在列表中
  as.integer(icd_col %in% prefixes)
}

# 辅助函数: 简单的正则匹配 (用于V1定义)
check_regex <- function(icd_col, pattern) {
  as.integer(str_detect(replace_na(icd_col, ""), pattern))
}

# --- A. Elixhauser Code Lists (源自 comorbidity.html, v3逻辑) ---

# 1. Myocardial Infarction
codes_mi <- c("I21", "I22", "I252")

# 2. Congestive Heart Failure
codes_chf <- c("I110", "I130", "I132", "I255", "I420", "I425", "I426", "I427", "I428", "I429", "I43", "I50", "P290")

# 3. Peripheral Vascular Disease
codes_pvd <- c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I791", "I798", 
               "K551", "K558", "K559", "Z958", "Z959")

# 4. Cerebrovascular Disease
codes_cevd <- c("G45", "G46", "H340", "H341", "H342", 
                "I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68")

# 5. Dementia
codes_dementia <- c("F01", "F02", "F03", "F04", "F05", "F061", "F068", 
                    "G132", "G138", "G30", "G310", "G311", "G312", "G914", "G94", 
                    "R4181", "R54")

# 6. COPD
codes_copd <- c("J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", 
                "J60", "J61", "J62", "J63", "J64", "J65", "J66", "J67", "J684", 
                "J701", "J703")

# 7. Rheumatic Disease
codes_rheumd <- c("M05", "M06", "M315", "M32", "M33", "M34", "M351", "M353", "M360")

# 8. Peptic Ulcer Disease
codes_pud <- c("K25", "K26", "K27", "K28")

# 9. Mild Liver Disease
codes_mld <- c("B18", "K700", "K701", "K702", "K703", "K709", "K713", "K714", "K715", "K717", 
               "K73", "K74", "K760", "K762", "K763", "K764", "K768", "K769", "Z944")

# 10. Diabetes (Uncomplicated)
# [用户补充]：E140, E141, E149 -> E14 (3位)
codes_diab <- c("E080", "E081", "E086", "E088", "E089", "E090", "E091", "E096", "E098", "E099", 
                "E100", "E101", "E106", "E108", "E109", "E110", "E111", "E116", "E118", "E119", 
                "E130", "E131", "E136", "E138", "E139",
                "E140", "E141", "E149")

# 11. Diabetes (Complicated)
# [用户补充]：E142 - E148 -> E14 (3位)
codes_diabwc <- c("E082", "E083", "E084", "E085", "E092", "E093", "E094", "E095", 
                  "E102", "E103", "E104", "E105", "E112", "E113", "E114", "E115", 
                  "E132", "E133", "E134", "E135",
                  "E142", "E143", "E144", "E145", "E146", "E147", "E148")

# 12. Hemiplegia or Paraplegia
codes_hp <- c("G041", "G114", "G800", "G801", "G802", "G81", "G82", "G83")

# 13. Renal Disease (Moderate/Mild)
codes_renal <- c("I129", "I130", "I1310", "N03", "N05", "N181", "N182", "N183", "N184", "N189", "Z940")

# 14. Renal Disease (Severe)
codes_renals <- c("I120", "I1311", "I132", "N185", "N186", "N19", "N250", "Z49", "Z992")

# 15. Malignancy (Any, except skin)
# ICD3 proxy: use explicit 3-digit ranges to avoid ambiguous 2-digit entries such as C0/C1/C2/C9
codes_canc <- c(
  "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
  "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
  "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29",
  "C30", "C31", "C32", "C33", "C34", "C35", "C36", "C37", "C38", "C39",
  "C40", "C41", "C42", "C43", "C44", "C45", "C46", "C47", "C48", "C49",
  "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C59",
  "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69",
  "C70", "C71", "C72", "C73", "C74", "C75", "C76",
  "C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89",
  "C90", "C91", "C92", "C93", "C94", "C95", "C96", "C97"
)

# 16. Metastatic Solid Tumor
codes_metacanc <- c("C77", "C78", "C79", "C800", "C802")

# 17. Severe Liver Disease
codes_msld <- c("I850", "I864", "K704", "K711", "K721", "K729", "K765", "K766", "K767")

# 18. HIV
codes_hiv <- c("B20", "B21", "B22", "B23", "B24")

# 19. AIDS (HIV + opportunistic)
codes_aids <- c("B37", "C53", "B38", "B45", "A072", "B25", "G934", "B00", "B39", "A073", 
                "C46", "C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", 
                "C90", "C91", "C92", "C93", "C94", "C95", "C96", 
                "A31", "A15", "A16", "A17", "A18", "A19", 
                "B59", "Z8701", "A812", "A021", "B58", "R64")

# 20. Alcohol Abuse
codes_alcohol <- c("F10", "E52", "G621", "I426", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714", "Z721")

# 21. Drug Abuse
codes_drug <- c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "Z715", "Z722")

# 22. Psychoses
codes_psycho <- c("F20", "F22", "F23", "F24", "F25", "F28", "F29", "F302", "F312", "F315")

# 23. Depression
codes_depress <- c("F204", "F313", "F314", "F315", "F32", "F33", "F341", "F412", "F432")

# 24. Weight Loss
codes_wl <- c("E40", "E41", "E42", "E43", "E44", "E45", "E46", "R64", "R634")

# 25. Obesity
codes_obesity <- c("E66")


# Application of Flags
dt_merged[, `:=`(
  # --- Elixhauser V3 (3-digit adapted) ---
  mi = check_codes_3digit(icd_clean, codes_mi),
  chf = check_codes_3digit(icd_clean, codes_chf),
  pvd = check_codes_3digit(icd_clean, codes_pvd),
  cevd = check_codes_3digit(icd_clean, codes_cevd),
  dementia = check_codes_3digit(icd_clean, codes_dementia),
  copd = check_codes_3digit(icd_clean, codes_copd),
  rheumd = check_codes_3digit(icd_clean, codes_rheumd),
  pud = check_codes_3digit(icd_clean, codes_pud),
  mld = check_codes_3digit(icd_clean, codes_mld),
  diab_uncomp = check_codes_3digit(icd_clean, codes_diab),
  diab_comp = check_codes_3digit(icd_clean, codes_diabwc),
  # ICD3 下复杂度难稳定区分，主分析建议使用 diabetes_any
  diabetes_any = as.integer(str_detect(icd_clean, "^E(08|09|10|11|13|14)$")),
  hp = check_codes_3digit(icd_clean, codes_hp),
  renal = check_codes_3digit(icd_clean, codes_renal),
  renal_severe = check_codes_3digit(icd_clean, codes_renals),
  malignancy = check_codes_3digit(icd_clean, codes_canc),
  metastatic_cancer = check_codes_3digit(icd_clean, codes_metacanc),
  msld = check_codes_3digit(icd_clean, codes_msld),
  hiv = check_codes_3digit(icd_clean, codes_hiv),
  aids_opportunistic_raw = check_codes_3digit(icd_clean, codes_aids),
  # 保守策略：AIDS 需同时有 HIV 主码与机会性感染线索
  aids = as.integer(
    check_codes_3digit(icd_clean, codes_hiv) == 1L &
      check_codes_3digit(icd_clean, codes_aids) == 1L
  ),
  alcohol_abuse = check_codes_3digit(icd_clean, codes_alcohol),
  drug_abuse = check_codes_3digit(icd_clean, codes_drug),
  psychoses = check_codes_3digit(icd_clean, codes_psycho),
  depression = check_codes_3digit(icd_clean, codes_depress),
  weight_loss = check_codes_3digit(icd_clean, codes_wl),
  obesity = check_codes_3digit(icd_clean, codes_obesity),
  
  # --- V1 Supplement Definitions (Regex based, Broad Scope) ---
  # 定义在 V1 中有但在 Elixhauser V3 中缺失或定义显著不同的变量
  
  # 1. Hypertension (宽口径高血压)
  hypertension_broad = check_regex(icd_clean, "^I1[0-6]|^O1[0-6]"),
  
  # 2. Smoking (吸烟)
  smoking = check_regex(icd_clean, "^Z72|^F17"),
  
  # 3. Angina (心绞痛)
  angina = check_regex(icd_clean, "^I20"),
  
  # 4. Coronary Artery Disease (CAD) - 含 I25 (所以和 MI/Elixhauser 重叠度高)
  cad_broad = check_regex(icd_clean, "^I2[0-5]"),
  
  # 5. Arrhythmia Any (心律失常)
  arrhythmia = check_regex(icd_clean, "^I4[7-9]"),
  
  # 6. Atrial Fibrillation (房颤)
  afib = check_regex(icd_clean, "^I48"),
  
  # 7. Asthma (哮喘)
  asthma = check_regex(icd_clean, "^J45"),
  
  # 8. Anemia (贫血)
  anemia = check_regex(icd_clean, "^D5[0-9]|^D6[0-4]"),
  
  # 9. Connective Tissue Disease (CTD) - V1宽泛定义, 可能比 Rheumd 广
  ctd_broad = check_regex(icd_clean, "^M0[5-6]|^M3[1-6]")

)]

# ==============================================================================
# 4. 聚合 (Aggregation) 与 逻辑修正
# ==============================================================================
cat("正在聚合数据...\n")

df_final <- dt_merged[, .(
  subject_id = first(subject_id),
  hadm_id = first(hadm_id),
  case_id = first(case_id),
  opdate = first(opdate),
  
  # Elixhauser V3 Variables (Max)
  mi = max(mi),
  chf = max(chf),
  pvd = max(pvd),
  cevd = max(cevd),
  dementia = max(dementia),
  copd = max(copd),
  rheumd = max(rheumd),
  pud = max(pud),
  mld = max(mld),
  diab_uncomp = max(diab_uncomp),
  diab_comp = max(diab_comp),
  diabetes_any = max(diabetes_any),
  hp = max(hp),
  renal = max(renal),
  renal_severe = max(renal_severe),
  malignancy = max(malignancy),
  metastatic_cancer = max(metastatic_cancer),
  msld = max(msld),
  hiv = max(hiv),
  aids_opportunistic_raw = max(aids_opportunistic_raw),
  aids = max(aids),
  alcohol_abuse = max(alcohol_abuse),
  drug_abuse = max(drug_abuse),
  psychoses = max(psychoses),
  depression = max(depression),
  weight_loss = max(weight_loss),
  obesity = max(obesity),
  
  # V1 Supplement Variables (Max)
  hypertension_broad = max(hypertension_broad),
  smoking = max(smoking),
  angina = max(angina),
  cad_broad = max(cad_broad),
  arrhythmia = max(arrhythmia),
  afib = max(afib),
  asthma = max(asthma),
  anemia = max(anemia),
  ctd_broad = max(ctd_broad),
  
  total_icd_count = sum(!is.na(icd_clean))
), by = op_id]

# --- Post-Aggregation Hierarchy Rules (Rule from HTML comments) ---
# "AIDS AND HIV = final AID; HIV - AIDS = final HIV"
# "adjustment on moderate/severe disease: keep severe one"

# Update logic based on Elixhauser hierarchy
df_final <- df_final %>%
  mutate(
    # HIV/AIDS heirarchy
    hiv = if_else(aids == 1, 0, hiv), # If AIDS is present, HIV flag is cleared (subsumed by AIDS)

    # Diabetes: ICD3 情况下难稳定拆分复杂/非复杂，保留 diab_uncomp/diab_comp 作参考，
    # 主定义请使用 diabetes_any。

    # Liver: If Severe (msld), set Mild (mld) to 0
    mld = if_else(msld == 1, 0, mld),

    # Renal: If severe renal present, set non-severe renal to 0
    renal = if_else(renal_severe == 1, 0, renal),

    # Metastatic: If Metastatic, set Malignancy to 0 based on Elixhauser rules?
    # Usually: Metastatic cancer counts as separate. But specific rule "adjustment on moderate/severe" might imply hierarchy.
    # The standard Elixhauser index often counts them distinctively but for *coding* overlap, metastatic implies cancer.
    # However, to be safe and "keep severe one", we often clear the less severe.
    # Let's follow "keep severe one" for Metastatic vs Solid Tumor
    malignancy = if_else(metastatic_cancer == 1, 0, malignancy)
  )

# ==============================================================================
# 5. 排序与保存
# ==============================================================================
cat("正在排序与整理...\n")
setDT(df_final)
setorder(df_final, subject_id, opdate)

# 保存
file_name <- "Diagnosis_Preop_Comorbidities_Elixhauser_ICD3_Proxy_V4_Expanded.csv"
full_save_path <- file.path(path_output, file_name)

cat("正在保存结果到:", full_save_path, "\n")
fwrite(df_final, full_save_path)

# backward-compatible alias (legacy name)
legacy_file_name <- "Diagnosis_Preop_Comorbidities_Elixhauser_V3_Expanded.csv"
legacy_save_path <- file.path(path_output, legacy_file_name)
fwrite(df_final, legacy_save_path)

# 统计摘要
summary_stats <- melt(df_final, 
                      id.vars = c("op_id", "subject_id", "hadm_id", "case_id", "opdate", "total_icd_count"),
                      variable.name = "Comorbidity", 
                      value.name = "Status")

summary_table <- summary_stats[, .(
  n_cases = sum(Status, na.rm = TRUE),
  prevalence_pct = round(sum(Status, na.rm=TRUE) / .N * 100, 2)
), by = Comorbidity][order(-prevalence_pct)]

summary_file <- file.path(path_output, "Summary_Elixhauser_Prevalence_ICD3_Proxy_V4_Expanded.csv")
fwrite(summary_table, summary_file)

# backward-compatible alias (legacy name)
legacy_summary_file <- file.path(path_output, "Summary_Elixhauser_Prevalence_V3_Expanded.csv")
fwrite(summary_table, legacy_summary_file)

cat("完成！\n")
