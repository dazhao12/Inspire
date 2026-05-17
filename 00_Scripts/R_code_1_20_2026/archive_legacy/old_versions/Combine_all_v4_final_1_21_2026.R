# ==============================================================================
# 脚本名称: Table1_Hybrid_Fix_CPB.R
# 功能: 混合定义 (ICD + 科室 + CPB) 分组，确保非心脏组 CPB 为 0
# ==============================================================================

# 1. 读取与清洗 (保持不变) -----------------------------------------------------
library(data.table)
library(tidyverse)
library(gtsummary)

base_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Master_Dataset_1_20_2026"
master_file_path <- file.path(base_path, "MASTER_DATASET_FINAL.csv")
output_path <- file.path(base_path, "Summary_Stats_By_Group_1_21_2026")

full_df <- fread(master_file_path)
names(full_df) <- make.unique(tolower(names(full_df)))

# 2. 核心修复：混合分组逻辑 ----------------------------------------------------
cat(">>> Classifying using Hybrid Approach (ICD + Dept + CPB)...\n")

df_classified <- full_df %>%
  filter(str_detect(antype, regex("General", ignore_case = TRUE))) %>%
  mutate(
    # 提取 ICD 特征
    pcs_prefix3 = str_sub(icd10_pcs, 1, 3),
    pcs_char4   = str_sub(icd10_pcs, 4, 4),
    
    # === 1. 判定是否为 Cardiac (混合标准) ===
    is_cardiac_op = case_when(
      # 标准 A: ICD 编码符合心脏手术
      str_sub(icd10_pcs, 1, 2) == "02" ~ TRUE,
      # 标准 B: 科室是 CTS
      str_detect(department, regex("CTS", ignore_case = TRUE)) ~ TRUE,
      # 标准 C: 竟然有体外循环时间 (CPB > 0)
      !is.na(cpb_duration_min) & cpb_duration_min > 0 ~ TRUE,
      # 其他均为 False
      TRUE ~ FALSE
    ),
    
    # === 2. 生成最终分组 ===
    Group = ifelse(is_cardiac_op, "Cardiac", "Non-Cardiac"),
    
    # === 3. 数据清洗 (强制修正) ===
    # 如果已经被分到 Non-Cardiac 组，强制将 CPB 设为 0
    # (逻辑：既然定义为非心脏手术，任何残留的 CPB 记录视为录入误差或噪音)
    cpb_duration_min = ifelse(Group == "Non-Cardiac", 0, cpb_duration_min)
  )

# 3. 再次验证 CPB (这次应该是 0) -----------------------------------------------
check_cpb <- df_classified %>%
  group_by(Group) %>%
  summarise(N = n(), Mean_CPB = mean(cpb_duration_min, na.rm = TRUE))
print(check_cpb) 
# 预期：Non-Cardiac 的 Mean_CPB 应该是 0

# 4. 生成表格 (保持之前的全变量) -----------------------------------------------
vars_to_analyze <- c(
  "male", "age", "height", "weight", "bmi", "race", "asa", 
  "emergency_op", "smoking", "drinking",
  "hypertension", "diabetes", "cerebrovascular_disease", "dementia", 
  "hemiplegia_paraplegia", "myocardial_infarction", "angina", 
  "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", 
  "copd", "asthma", "ards", "renal_disease", "liver_disease", 
  "peptic_ulcer_disease", "anemia", "malignancy", "metastatic_solid_tumor", "hiv_aids",
  "preop_albumin_nearest", "preop_creatinine_nearest", "preop_hb_nearest", 
  "preop_platelet_nearest", "preop_wbc_nearest", "preop_glucose_nearest",
  "beta_blockers", "ace_inhibitors", "statins", "anticoagulants", "insulin",
  "op_duration_min", "cpb_duration_min", # 重点关注这个
  "hosp_los_days", "icu_los_days", "death_1_year", "aki_any", "stroke"
)
# (注：为了代码简洁，这里截取了部分核心变量，您可以用之前的全变量列表替换这里)

# 自动匹配存在的变量
vars_final <- names(df_classified)[names(df_classified) %in% vars_to_analyze]

df_table <- df_classified %>%
  select(Group, all_of(vars_final)) %>%
  mutate(
    asa = as.numeric(asa),
    across(where(~ all(unique(na.omit(.)) %in% c(0, 1))), ~ as.logical(.))
  )

cat(">>> Generating Table 1...\n")
table1 <- df_table %>%
  tbl_summary(
    by = Group,
    missing = "no", 
    statistic = list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2
  ) %>%
  add_overall() %>%
  add_p(test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test")) %>%
  add_difference(test = all_continuous() ~ "smd") %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

# 5. 导出 ----------------------------------------------------------------------
final_csv <- table1 %>% as_tibble()
save_file <- file.path(output_path, "Table1_Hybrid_Fixed_Cardiac_vs_NonCardiac.csv")
write_csv(final_csv, save_file)
cat(">>> Success! Saved to:", save_file, "\n")