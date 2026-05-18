library(tidyverse)
library(data.table)

# ==============================================================================
# 1. 设置路径 (基于你之前的脚本路径)
# ==============================================================================
# 根目录
base_processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"

# 各个模块的具体文件路径 (请根据实际生成的文件名核对，这里基于你之前的代码)
# 1. Baseline (人口学 + 术中 + 时间)
file_baseline <- file.path(base_processed_path, "Baseline_data_1_20_2026/ops_baseline_full.csv")

# 2. Diagnosis (合并症)
file_diag     <- file.path(base_processed_path, "Diagnosis_1_20_2026/diag_preop_flags_final.csv")

# 3. Labs (化验 - 选用 30天窗口的版本)
file_labs     <- file.path(base_processed_path, "lab_data_v1_1_20_2026/preop_labs_features_30d.csv")

# 4. Meds (药物)
file_meds     <- file.path(base_processed_path, "Meds_Preop_1_20_2026/preop_meds.csv")

# 5. Vitals (体征)
file_vitals   <- file.path(base_processed_path, "Vials_pro_1_20_2026/preop_baseline_final.csv")

# 6. Outcomes (结局)
file_outcomes <- file.path(base_processed_path, "Outcomes_1_20_2026/postop_outcomes_final.csv")

# 输出路径
output_path <- file.path(base_processed_path, "Summary_Stats_1_21_2026")
if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# ==============================================================================
# 2. 读取与合并数据 (Merging)
# ==============================================================================
cat("Step 1: Reading and merging all data files...\n")

# 读取各表 (使用 fread 提升速度)
dt_base     <- fread(file_baseline)
dt_diag     <- fread(file_diag)
dt_labs     <- fread(file_labs)
dt_meds     <- fread(file_meds)
dt_vitals   <- fread(file_vitals)
dt_outcomes <- fread(file_outcomes)

# --- 执行合并 (Left Join 以 op_id 为主键) ---
# 注意：Lab 和 Meds 可能有 subject_id，但在 merge 时主要依靠 op_id 唯一对应
full_df <- dt_base %>%
  left_join(dt_diag,     by = "op_id") %>%
  left_join(dt_labs,     by = "op_id") %>%
  left_join(dt_meds,     by = c("op_id", "subject_id")) %>% # Meds 有 subject_id
  left_join(dt_vitals,   by = c("op_id", "subject_id")) %>% # Vitals 有 subject_id
  left_join(dt_outcomes, by = c("op_id", "subject_id"))     # Outcomes 有 subject_id

cat(sprintf("合并完成！总样本量: %d 行, 总变量数: %d 列\n", nrow(full_df), ncol(full_df)))

# ==============================================================================
# 3. 定义变量清单 (Schema Definition)
# ==============================================================================
cat("Step 2: Categorizing variables...\n")

# 根据你提供的变量列表，手动分类
# ------------------------------------------------------------------------------
# A. 连续变量 (Continuous Variables)
# ------------------------------------------------------------------------------
vars_cont <- c(
  # 人口学与体格
  "Age", "Height", "Weight", "BMI",
  
  # 手术时长
  "op_duration_min", "anesthesia_duration_min", "or_room_time_min", "cpb_duration_min",
  
  # 结局 - LOS & Survival Time
  "hosp_los_min", "hosp_los_days", "icu_los_min", "icu_los_days",
  "time_to_inhosp_death_days", "Survival_Days",
  
  # 术前体征 (Vitals)
  "preop_sbp", "preop_dbp", "preop_mbp", "preop_hr", "preop_spo2", "preop_rr", "preop_bt"
)

# 自动抓取所有的 Lab 变量 (nearest, median, mean)
vars_labs <- names(full_df)[grep("^preop_.*_(nearest|median|mean)$", names(full_df))]
vars_cont <- c(vars_cont, vars_labs)

# ------------------------------------------------------------------------------
# B. 分类变量 (Categorical Variables)
# ------------------------------------------------------------------------------
vars_cat <- c(
  # 基础信息
  "Male", "race", "asa", "Emergency_op", "department", "antype",
  
  # 并发症 (Comorbidities) - 从 smoking 到 hiv_aids
  "smoking", "drinking", "hypertension", "diabetes", "cerebrovascular_disease", 
  "dementia", "hemiplegia_paraplegia", "myocardial_infarction", "angina", 
  "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", "copd", 
  "asthma", "ards", "renal_disease", "liver_disease", "peptic_ulcer_disease", 
  "connective_tissue_disease", "peripheral_vascular_disease", "anemia", 
  "malignancy", "metastatic_solid_tumor", "hiv_aids",
  
  # 药物 (Meds)
  "Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics",
  "Statins", "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics",
  "Insulin", "Oral_hypoglycemics", "Systemic_corticosteroids", "Immunosuppressants",
  "Inhaled_bronchodilators", "Inhaled_corticosteroids", "Opioid_chronic_use",
  "Proton_pump_inhibitors", "Antidepressants", "Antipsychotics", "Antibiotics_systemic",
  "Thyroid_medications", "NSAIDs", "Antiemetics", "Mucolytics_expectorants",
  "Antihistamines", "H2_blockers", "Laxatives", "GI_prokinetics",
  "Benzodiazepines_sedatives", "Gabapentinoids", "Antiepileptics",
  "Osteoporosis_medications", "Vitamin_D_Calcium", "Smoking_cessation_drugs",
  
  # 结局 (Outcomes)
  "Stroke", "Cognitive_Decline", "Cardiac_Arrest", "Heart_Failure", "Myocardial_Injury",
  "Angina", "Arrhythmia_Vent", "Atrial_Fib", "Resp_Failure", "Pneumonia", "Sepsis",
  "Infection_Organ", "Infection_Unk",
  "AKI_Any", "AKI_Stage_1", "AKI_Stage_2", "AKI_Stage_3",
  "Death_In_Hospital", "Death_POD30", "Death_POD90", "Death_1_Year"
)

# 确保所有变量都在数据框中 (取交集，防止报错)
vars_cont <- intersect(vars_cont, names(full_df))
vars_cat  <- intersect(vars_cat, names(full_df))

# ==============================================================================
# 4. 统计计算 (Calculations)
# ==============================================================================
cat("Step 3: Calculating statistics...\n")

# --- 函数 A: 计算连续变量统计量 ---
calc_continuous <- function(df, vars) {
  df %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    summarise(
      Type = "Continuous",
      N_Valid = sum(!is.na(Value)),
      Missing_Pct = round(sum(is.na(Value)) / n() * 100, 1),
      Mean = round(mean(Value, na.rm = TRUE), 2),
      SD = round(sd(Value, na.rm = TRUE), 2),
      Median = round(median(Value, na.rm = TRUE), 2),
      Q1 = round(quantile(Value, 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(Value, 0.75, na.rm = TRUE), 2),
      Min = round(min(Value, na.rm = TRUE), 2),
      Max = round(max(Value, na.rm = TRUE), 2),
      .groups = 'drop'
    ) %>%
    mutate(
      # 生成一个格式化的描述列: "Mean (SD)" 和 "Median [IQR]"
      `Mean (SD)` = paste0(Mean, " (", SD, ")"),
      `Median [IQR]` = paste0(Median, " [", Q1, ", ", Q3, "]")
    )
}

# --- 函数 B: 计算分类变量统计量 ---
calc_categorical <- function(df, vars) {
  # 这里的逻辑是：大部分分类变量是 0/1 编码，我们只统计 "1" (Yes) 的数量
  # 如果有人口学变量(如race)不是0/1，需要分别处理。这里先处理 0/1 标志变量。
  
  # 分离 0/1 变量和 多分类变量
  # 假设 list 中的 comorbidities/meds/outcomes 都是 0/1
  # 只有 race, department, asa 可能不是 0/1，我们简单起见，统一按 Factor 处理
  
  df_long <- df %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.character)) %>% # 转为字符以便堆叠
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Level")
  
  df_long %>%
    group_by(Variable, Level) %>%
    summarise(Count = n(), .groups = 'drop_last') %>%
    mutate(
      Total_N = sum(Count),
      Pct = round(Count / Total_N * 100, 2)
    ) %>%
    ungroup() %>%
    # 筛选：如果是 0/1 变量，通常只关心 Level == "1"
    # 如果是 race/asa，我们保留所有 Level
    # 这里为了整洁，我们生成一个通用表
    mutate(
      Type = "Categorical",
      `n (%)` = paste0(Count, " (", Pct, "%)")
    ) %>%
    select(Variable, Level, Count, Total_N, Pct, `n (%)`, Type)
}

# --- 执行计算 ---
stats_cont <- calc_continuous(full_df, vars_cont)
stats_cat  <- calc_categorical(full_df, vars_cat)

# ==============================================================================
# 5. 格式化输出与保存
# ==============================================================================
cat("Step 4: Formatting and saving...\n")

# 保存连续变量表
file_cont <- file.path(output_path, "Table1_Continuous_Vars.csv")
write_csv(stats_cont, file_cont)

# 保存分类变量表
file_cat <- file.path(output_path, "Table1_Categorical_Vars.csv")
write_csv(stats_cat, file_cat)

# --- 生成一个精简的合并版 (High-Level Summary) ---
# 挑选分类变量中 Level="1" 的行（针对 0/1 变量），
# 对于多分类变量(如Race)，保留所有
summary_cat_clean <- stats_cat %>%
  filter(Level == "1" | Variable %in% c("race", "asa", "department", "Male")) %>%
  select(Variable, Level, `Stat` = `n (%)`)

summary_cont_clean <- stats_cont %>%
  select(Variable, `Stat` = `Mean (SD)`) %>% # 或者选 Median [IQR]
  mutate(Level = "Mean (SD)")

final_report <- bind_rows(summary_cont_clean, summary_cat_clean)

file_report <- file.path(output_path, "Table1_Summary_Report.csv")
write_csv(final_report, file_report)

# ==============================================================================
# 6. 打印预览
# ==============================================================================
cat("\n=======================================================\n")
cat("            统计描述生成完成 (Summary Generated)        \n")
cat("=======================================================\n")
cat("1. 连续变量表 (Continuous): \n")
print(head(stats_cont[, c("Variable", "N_Valid", "Mean (SD)", "Median [IQR]")]))
cat("\n2. 分类变量表 (Categorical - Top Rows): \n")
print(head(stats_cat[, c("Variable", "Level", "n (%)")]))
cat("\n")
cat(sprintf("文件已保存至: %s\n", output_path))
cat("包含文件:\n")
cat(" - Table1_Continuous_Vars.csv (所有连续变量的详细统计)\n")
cat(" - Table1_Categorical_Vars.csv (所有分类变量的频数统计)\n")
cat(" - Table1_Summary_Report.csv (合并后的概览)\n")