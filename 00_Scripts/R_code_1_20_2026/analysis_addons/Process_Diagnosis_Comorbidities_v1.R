library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(data.table) # 引入 data.table 提升大数据集处理效率

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

# 读取手术表 (保留完整元数据用于排序和ID)
# 确保 ID 列作为字符读取，防止科学计数法
col_types_ops <- cols(
  op_id = col_character(),
  subject_id = col_character(),
  hadm_id = col_character(),
  case_id = col_character(),
  opdate = col_double() # opdate 好像是数字？如果是日期字符串需调整
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

# 预处理 ICD 代码 (只取前3位，转大写)
df_diag_clean <- df_diag %>%
  mutate(
    # 清洗：大写 -> 去空格 ->取前3位
    icd3 = str_sub(str_to_upper(str_trim(icd10_cm)), 1, 3)
  ) %>%
  filter(!is.na(icd3)) # 移除无有效ICD的行

cat("正在关联数据 (Operations + Diagnosis)...\n")

# 关联逻辑：
# 1. 将诊断挂载到每次手术上 (by subject_id)
# 2. 过滤掉 chart_time > 0 的诊断 (保留术前/术中早期诊断)
# 3. 注意：如果某个 diagnosis 没有 chart_time (NA)，这里策略是保留还是丢弃？
#    通常 chart_time <= 0 意味着术前。如果 NA，无法判断时间，保守起见可能需要丢弃或检查。
#    *原代码逻辑是 filter(chart_time <= 0)*，这意味着 NA 会被丢弃。保持一致。

df_merged <- df_ops %>%
  left_join(df_diag_clean, by = "subject_id") %>%
  # 保留 chart_time <= 0 的诊断。
  # 重要：对于本来就没有诊断的病人 (subject_id匹配不到)，chart_time 为 NA。
  # 直接 filter 会把这些病人丢掉。我们需要先标记哪些是 valid diagnosis。
  filter(chart_time <= 0 | is.na(chart_time)) 
  # Note: 即使保留了 is.na(chart_time) 的行 (来自左连接的空匹配)，
  # 下面的 flag 计算逻辑会因为 icd3 为 NA 而全算作 0，符合预期。

# ==============================================================================
# 3. 特征工程：定义并发症 (Charlson / Elixhauser 混合宽口径)
# ==============================================================================
cat("正在计算并发症标志 (Feature Engineering)...\n")

# 使用 data.table 语法加速计算 (对于大表 dplyr 的 mutate 可能较慢)
dt_merged <- as.data.table(df_merged)

# 定义正则匹配函数简写
has_icd <- function(code_pattern) {
  # 返回 1 如果匹配，0 否则 (处理 NA 情况)
  as.integer(str_detect(replace_na(dt_merged$icd3, ""), code_pattern))
}
is_icd <- function(code_set) {
  as.integer(dt_merged$icd3 %in% code_set)
}

# 计算 Flags
# 逻辑：每一行 (每个手术-诊断对) 计算 flag
dt_merged[, `:=`(
  # --- 1. 生活方式 ---
  smoking_hit = is_icd(c('Z72', 'F17')),
  drinking_hit = is_icd(c('F10', 'K70')),
  
  # --- 2. 高血压 ---
  hypertension_hit = has_icd("^I1[0-6]|^O1[0-6]"),
  
  # --- 3. 糖尿病 ---
  diabetes_hit = has_icd("^E1[0-4]"),
  
  # --- 4. 脑血管/神经 ---
  cerebrovasc_hit = has_icd("^I6[0-9]") | is_icd('G45'),
  dementia_hit = is_icd(c('F00','F01','F02','F03','G30')),
  hemi_para_hit = is_icd(c('G80','G81','G82','G83','G04','G11')),
  
  # --- 5. 心脏相关 ---
  mi_hit = is_icd(c('I21','I22')),
  angina_hit = is_icd('I20'),
  af_hit = is_icd('I48'),
  cad_hit = has_icd("^I2[0-5]"),
  arrhythmia_any_hit = has_icd("^I4[7-9]"),
  
  # --- 6. 呼吸 ---
  copd_hit = is_icd('J44'),
  asthma_hit = is_icd('J45'),
  ards_hit = is_icd('J80'),
  
  # --- 7. 脏器功能 ---
  renal_disease_hit = is_icd(c('N18','N19','I12','I13','Z49','Z94','Z99')),
  liver_disease_hit = is_icd(c('B18','K70','K73','K74')),
  pud_hit = has_icd("^K2[5-8]"),
  
  # --- 8. 其他 ---
  ctd_hit = is_icd(c('M05','M06','M32','M33','M34','M31','M35')),
  pvd_hit = is_icd(c('I70','I71','I73','K55')),
  anemia_hit = has_icd("^D5[0-9]|^D6[0-4]"),
  
  # --- 9. 恶性肿瘤 (需复杂逻辑) ---
  # 恶性肿瘤: C00-C97 但排除 C77-C80 (继发)
  malignancy_hit = as.integer(
    (str_detect(replace_na(dt_merged$icd3, ""), "^C[0-6][0-9]|^C7[0-6]|^C[8-9][0-9]") &
     !dt_merged$icd3 %in% c('C77','C78','C79','C80'))
  ),
  
  # --- 10. 转移瘤 & HIV ---
  metastatic_tumor_hit = is_icd(c('C77','C78','C79','C80')),
  hiv_hit = is_icd(c('B20','B21','B22','B23','B24'))
)]

# ==============================================================================
# 4. 聚合 (Aggregation) - 按手术 ID 汇总
# ==============================================================================
cat("正在聚合数据...\n")

# 按 op_id 分组取 max
# 如果 op_id 下所有记录都是 NA (无诊断)，max 会得到 0 (因为我们上面函数处理了NA->0)
df_final <- dt_merged[, .(
  subject_id = first(subject_id), # 保留 ID 信息
  hadm_id = first(hadm_id),
  case_id = first(case_id),
  opdate = first(opdate),
  
  smoking = max(smoking_hit),
  drinking = max(drinking_hit),
  hypertension = max(hypertension_hit),
  diabetes = max(diabetes_hit),
  cerebrovascular_disease = max(cerebrovasc_hit),
  dementia = max(dementia_hit),
  hemiplegia_paraplegia = max(hemi_para_hit),
  myocardial_infarction = max(mi_hit),
  angina = max(angina_hit),
  atrial_fibrillation = max(af_hit),
  coronary_artery_disease = max(cad_hit),
  arrhythmia_any = max(arrhythmia_any_hit),
  copd = max(copd_hit),
  asthma = max(asthma_hit),
  ards = max(ards_hit),
  renal_disease = max(renal_disease_hit),
  liver_disease = max(liver_disease_hit),
  peptic_ulcer_disease = max(pud_hit),
  connective_tissue_disease = max(ctd_hit),
  peripheral_vascular_disease = max(pvd_hit),
  anemia = max(anemia_hit),
  malignancy = max(malignancy_hit),
  metastatic_solid_tumor = max(metastatic_tumor_hit),
  hiv_aids = max(hiv_hit),
  
  # 计算总诊断数 (排除 NA 行)
  total_icd_count = sum(!is.na(icd3))
), by = op_id]

# ==============================================================================
# 5. 排序与整理列
# ==============================================================================
cat("正在排序与整理...\n")
setorder(df_final, subject_id, opdate)

# 确保列顺序正确
col_order <- c(
  "op_id", "subject_id", "hadm_id", "case_id", "opdate",
  "smoking", "drinking", "hypertension", "diabetes", "cerebrovascular_disease", 
  "dementia", "hemiplegia_paraplegia", "myocardial_infarction", "angina", 
  "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", 
  "copd", "asthma", "ards", "renal_disease", "liver_disease", 
  "peptic_ulcer_disease", "connective_tissue_disease", 
  "peripheral_vascular_disease", "anemia", "malignancy", 
  "metastatic_solid_tumor", "hiv_aids", "total_icd_count"
)

df_final <- df_final[, ..col_order]

# ==============================================================================
# 6. 保存结果
# ==============================================================================
file_name <- "Diagnosis_Preop_Comorbidities.csv"
full_save_path <- file.path(path_output, file_name)

cat("正在保存结果到:", full_save_path, "\n")
fwrite(df_final, full_save_path)


# ==============================================================================
# 7. 生成统计摘要 (Optional)
# ==============================================================================
cat("生成统计摘要...\n")
summary_stats <- melt(df_final, 
                      id.vars = c("op_id", "subject_id", "hadm_id", "case_id", "opdate", "total_icd_count"),
                      variable.name = "Comorbidity", 
                      value.name = "Status")

summary_table <- summary_stats[, .(
  n_cases = sum(Status, na.rm = TRUE),
  prevalence_pct = round(sum(Status, na.rm=TRUE) / .N * 100, 2)
), by = Comorbidity][order(-prevalence_pct)]

summary_file <- file.path(path_output, "Summary_Comorbidities_Prevalence.csv")
fwrite(summary_table, summary_file)

cat("完成！\n")
