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

# 预处理: 不再截取前3位，因为新定义包含4位代码 (如 I252)
# 去除小数点 (INSPIRE数据通常无点，但以防万一)，转大写，去空格
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
# 3. 特征工程：基于 Elixhauser 定义 (参考 comorbidity.html)
# ==============================================================================
cat("正在计算 Elixhauser 并发症标志...\n")

# 辅助函数: 检查ICD是否以给定列表中的任意前缀开头
check_codes <- function(icd_col, prefix_list) {
  # 构建正则: ^(Code1|Code2|...)
  pattern <- paste0("^(", paste(prefix_list, collapse = "|"), ")")
  as.integer(str_detect(replace_na(icd_col, ""), pattern))
}

# --- 定义 Code Lists (源自 comorbidity.html) ---

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
codes_diab <- c("E080", "E081", "E086", "E088", "E089", "E090", "E091", "E096", "E098", "E099", 
                "E100", "E101", "E106", "E108", "E109", "E110", "E111", "E116", "E118", "E119", 
                "E130", "E131", "E136", "E138", "E139")

# 11. Diabetes (Complicated)
codes_diabwc <- c("E082", "E083", "E084", "E085", "E092", "E093", "E094", "E095", 
                  "E102", "E103", "E104", "E105", "E112", "E113", "E114", "E115", 
                  "E132", "E133", "E134", "E135")

# 12. Hemiplegia or Paraplegia
codes_hp <- c("G041", "G114", "G800", "G801", "G802", "G81", "G82", "G83")

# 13. Renal Disease (Moderate/Mild) (Note: "renal" in HTML)
codes_renal <- c("I129", "I130", "I1310", "N03", "N05", "N181", "N182", "N183", "N184", "N189", "Z940")

# 14. Renal Disease (Severe) (Note: "renals" in HTML)
codes_renals <- c("I120", "I1311", "I132", "N185", "N186", "N19", "N250", "Z49", "Z992")

# 15. Malignancy (Any, except skin)
codes_canc <- c("C0", "C1", "C2", "C30", "C31", "C32", "C33", "C34", "C37", "C38", "C39", 
                "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49", 
                "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", 
                "C60", "C61", "C62", "C63", "C76", 
                "C801", "C81", "C82", "C83", "C84", "C85", "C88", "C9")

# 16. Metastatic Solid Tumor
codes_metacanc <- c("C77", "C78", "C79", "C800", "C802")

# 17. Severe Liver Disease
codes_msld <- c("I850", "I864", "K704", "K711", "K721", "K729", "K765", "K766", "K767")

# 18. HIV
codes_hiv <- c("B20")

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
  mi = check_codes(icd_clean, codes_mi),
  chf = check_codes(icd_clean, codes_chf),
  pvd = check_codes(icd_clean, codes_pvd),
  cevd = check_codes(icd_clean, codes_cevd),
  dementia = check_codes(icd_clean, codes_dementia),
  copd = check_codes(icd_clean, codes_copd),
  rheumd = check_codes(icd_clean, codes_rheumd),
  pud = check_codes(icd_clean, codes_pud),
  mld = check_codes(icd_clean, codes_mld),
  diab_uncomp = check_codes(icd_clean, codes_diab),
  diab_comp = check_codes(icd_clean, codes_diabwc),
  hp = check_codes(icd_clean, codes_hp),
  renal = check_codes(icd_clean, codes_renal),
  renal_severe = check_codes(icd_clean, codes_renals),
  malignancy = check_codes(icd_clean, codes_canc),
  metastatic_cancer = check_codes(icd_clean, codes_metacanc),
  msld = check_codes(icd_clean, codes_msld),
  hiv = check_codes(icd_clean, codes_hiv),
  aids = check_codes(icd_clean, codes_aids),
  alcohol_abuse = check_codes(icd_clean, codes_alcohol),
  drug_abuse = check_codes(icd_clean, codes_drug),
  psychoses = check_codes(icd_clean, codes_psycho),
  depression = check_codes(icd_clean, codes_depress),
  weight_loss = check_codes(icd_clean, codes_wl),
  obesity = check_codes(icd_clean, codes_obesity)
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
  
  # Basic Aggregation (Max)
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
  hp = max(hp),
  renal = max(renal),
  renal_severe = max(renal_severe),
  malignancy = max(malignancy),
  metastatic_cancer = max(metastatic_cancer),
  msld = max(msld),
  hiv = max(hiv),
  aids = max(aids),
  alcohol_abuse = max(alcohol_abuse),
  drug_abuse = max(drug_abuse),
  psychoses = max(psychoses),
  depression = max(depression),
  weight_loss = max(weight_loss),
  obesity = max(obesity),
  
  total_icd_count = sum(!is.na(icd_clean))
), by = op_id]

# --- Post-Aggregation Hierarchy Rules (Rule from HTML comments) ---
# "AIDS AND HIV = final AID; HIV - AIDS = final HIV"
# "adjustment on moderate/severe disease: keep severe one"

# Update logic based on Elixhauser hierarchy
df_final <- df_final %>%
  mutate(
    # HIV/AIDS hierarchy
    hiv = if_else(aids == 1, 0, hiv), # If AIDS is present, HIV flag is cleared (subsumed by AIDS)
    
    # Diabetes hierarchy: If complicated present, uncomplicated = 0? 
    # Usually Elixhauser keeps both or handles them. The HTML comment "keep severe one" usually applies to Liver/Renal.
    # Let's apply standard Elixhauser hierarchy logic:
    
    # Diabetes: If Diab_Comp, set Diab_Uncomp to 0
    diab_uncomp = if_else(diab_comp == 1, 0, diab_uncomp),

    # Liver: If Severe (msld), set Mild (mld) to 0
    mld = if_else(msld == 1, 0, mld),

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
file_name <- "Diagnosis_Preop_Comorbidities_Elixhauser.csv"
full_save_path <- file.path(path_output, file_name)

cat("正在保存结果到:", full_save_path, "\n")
fwrite(df_final, full_save_path)

# 统计摘要
summary_stats <- melt(df_final, 
                      id.vars = c("op_id", "subject_id", "hadm_id", "case_id", "opdate", "total_icd_count"),
                      variable.name = "Comorbidity", 
                      value.name = "Status")

summary_table <- summary_stats[, .(
  n_cases = sum(Status, na.rm = TRUE),
  prevalence_pct = round(sum(Status, na.rm=TRUE) / .N * 100, 2)
), by = Comorbidity][order(-prevalence_pct)]

summary_file <- file.path(path_output, "Summary_Elixhauser_Prevalence.csv")
fwrite(summary_table, summary_file)

cat("完成！\n")
