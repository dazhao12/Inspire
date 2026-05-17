# ==============================================================================
# 脚本名称: Table1_Final_Complete_Fixed.R
# 功能: 全变量、自动修复列名、强制卡方检验(防崩溃)、自动导出
# ==============================================================================

# 1. 加载包 --------------------------------------------------------------------
library(data.table)
library(tidyverse)
if (!requireNamespace("gtsummary", quietly = TRUE)) install.packages("gtsummary")
library(gtsummary)

# 2. 路径设置 ------------------------------------------------------------------
base_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Master_Dataset_1_20_2026"
master_file_path <- file.path(base_path, "MASTER_DATASET_FINAL.csv")
output_path <- file.path(base_path, "Summary_Stats_By_Group_1_21_2026")
if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# 3. 读取数据与清洗 ------------------------------------------------------------
cat(">>> [1/5] Reading Master Dataset...\n")
if (!file.exists(master_file_path)) stop("文件不存在，请检查路径！")
full_df <- fread(master_file_path)

cat(">>> [2/5] Cleaning column names...\n")
# 解决重复名 + 转小写
names(full_df) <- make.unique(tolower(names(full_df)))

# 4. 筛选与分组 ----------------------------------------------------------------
cat(">>> [3/5] Filtering (General Anesthesia) and Grouping...\n")
df_clean <- full_df %>%
  filter(str_detect(antype, regex("General", ignore_case = TRUE))) %>%
  mutate(
    Group = case_when(
      str_detect(department, regex("CTS", ignore_case = TRUE)) ~ "Cardiac",
      TRUE ~ "Non-Cardiac"
    )
  )

cat(sprintf("   Filtered N: %d (Cardiac: %d, Non-Cardiac: %d)\n", 
            nrow(df_clean), sum(df_clean$Group=="Cardiac"), sum(df_clean$Group=="Non-Cardiac")))

# 5. 变量清单 (已包含双重拼写保险) ---------------------------------------------
vars_to_analyze <- c(
  # --- Demographics ---
  "male", "age", "height", "weight", "bmi", "race", "asa", 
  "emergency_op", "smoking", "drinking",
  
  # --- Comorbidities ---
  "hypertension", "diabetes", "cerebrovascular_disease", "dementia", 
  "hemiplegia_paraplegia", "myocardial_infarction", "angina", 
  "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", 
  "copd", "asthma", "ards", 
  "renal_disease", "liver_disease", "peptic_ulcer_disease", 
  "connective_tissue_disease", "peripheral_vascular_disease", 
  "anemia", "malignancy", "metastatic_solid_tumor", "hiv_aids",
  
  # --- Labs (Nearest) ---
  "preop_albumin_nearest", "preop_alp_nearest", "preop_alt_nearest", 
  "preop_aptt_nearest", "preop_ast_nearest", "preop_be_nearest", 
  "preop_bun_nearest", "preop_calcium_nearest", "preop_chloride_nearest", 
  "preop_ck_nearest", "preop_ckmb_nearest", "preop_creatinine_nearest", 
  "preop_crp_nearest", "preop_d_dimer_nearest", "preop_fibrinogen_nearest", 
  "preop_glucose_nearest", "preop_hb_nearest", "preop_hba1c_nearest", 
  "preop_hco3_nearest", "preop_hct_nearest", "preop_ica_nearest", 
  "preop_lacate_nearest", "preop_lactate_nearest", # <--- 双拼写保险
  "preop_lymphocyte_nearest", "preop_paco2_nearest", 
  "preop_pao2_nearest", "preop_ph_nearest", "preop_phosphorus_nearest", 
  "preop_platelet_nearest", "preop_potassium_nearest", "preop_ptinr_nearest", 
  "preop_sao2_nearest", "preop_seg_nearest", "preop_sodium_nearest", 
  "preop_total_bilirubin_nearest", "preop_total_protein_nearest", 
  "preop_troponin_i_nearest", "preop_troponin_t_nearest", "preop_wbc_nearest",
  
  # --- Medications ---
  "beta_blockers", "calcium_channel_blockers", "ace_inhibitors", "arbs", 
  "diuretics", "statins", "antiplatelet_agents", "anticoagulants", 
  "nitrates", "antiarrhythmics", "insulin", "oral_hypoglycemics", 
  "systemic_corticosteroids", "immunosuppressants", "inhaled_bronchodilators", 
  "inhaled_corticosteroids", "opioid_chronic_use", "proton_pump_inhibitors", 
  "antidepressants", "antipsychotics", "antibiotics_systemic", 
  "thyroid_medications", "nsaids", "antiemetics", "mucolytics_expectorants", 
  "antihistamines", "h2_blockers", "laxatives", "gi_prokinetics", 
  "benzodiazepines_sedatives", "gabapentinoids", "antiepileptics", 
  "osteoporosis_medications", "vitamin_d_calcium", "smoking_cessation_drugs",
  
  # --- Vitals ---
  "preop_sbp", "preop_dbp", "preop_mbp", "preop_hr", 
  "preop_spo2", "preop_rr", "preop_bt",
  
  # --- Intra-op ---
  "op_duration_min", "anesthesia_duration_min", 
  "or_room_time_min", "cpb_duration_min",
  
  # --- Outcomes ---
  "hosp_los_days", "icu_los_days", 
  "stroke", "cognitive_decline", "cardiac_arrest", "heart_failure", 
  "myocardial_injury", "resp_failure", "pneumonia", "sepsis", 
  "infection_organ", "infection_unk", 
  "aki_any", "aki_stage_1", "aki_stage_2", "aki_stage_3", 
  "death_in_hospital", "death_pod30", "death_pod90", 
  "death_1_year", "death_long_term"
)

# 抓取存在的变量 (处理重复名)
vars_final <- names(df_clean)[names(df_clean) %in% vars_to_analyze]
vars_duplicates <- names(df_clean)[str_remove(names(df_clean), "\\.\\d+$") %in% vars_to_analyze]
vars_final <- unique(c(vars_final, vars_duplicates))

# 6. 生成表格 ------------------------------------------------------------------
cat(">>> [4/5] Generating Table 1 (This usually takes 2-5 minutes)...\n")

# 准备数据
df_table <- df_clean %>%
  select(Group, all_of(vars_final)) %>%
  mutate(
    asa = as.numeric(asa),
    # 智能转换分类变量 (0/1 -> TRUE/FALSE)
    across(where(~ all(unique(na.omit(.)) %in% c(0, 1))), ~ as.logical(.))
  )

# 【关键修复】定义标签 (之前报错是因为缺了这一段)
var_labels <- list(
  age ~ "Age (years)",
  male ~ "Male Sex",
  bmi ~ "BMI (kg/m2)",
  asa ~ "ASA Score",
  op_duration_min ~ "Surgery Duration (min)",
  cpb_duration_min ~ "CPB Duration (min)",
  preop_creatinine_nearest ~ "Pre-op Creatinine (Nearest)",
  preop_hb_nearest ~ "Pre-op Hb (Nearest)",
  death_1_year ~ "1-Year Mortality"
)

# 生成表格
table1 <- df_table %>%
  tbl_summary(
    by = Group,
    missing = "no", 
    label = var_labels,  # 现在 var_labels 已经定义了，不会报错了
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_overall() %>%
  # 使用卡方检验防止内存溢出
  add_p(
    test = list(
      all_continuous() ~ "t.test",
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_difference(test = all_continuous() ~ "smd") %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

cat(">>> Table generation successful!\n")

# 7. 导出 ----------------------------------------------------------------------
cat(">>> [5/5] Exporting...\n")
final_csv <- table1 %>% as_tibble()
save_file <- file.path(output_path, "Table1_Full_Variables_Cardiac_vs_NonCardiac_Final.csv")
write_csv(final_csv, save_file)

cat(">>> Success! File saved to:", save_file, "\n")