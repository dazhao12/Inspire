# ==============================================================================
# 脚本名称: Table1_Final_Full_Variables_Refined_v2.R
# 功能: 
#   1. 全变量清洗
#   2. 精细化定义心脏手术 (保留TAAA，剔除肺/肾/肝移植)
#   3. 生成 Table 1 统计表
#   4. [新功能] 导出筛选后的心脏病人群体数据 + 带标签的全量数据
# ==============================================================================

# 1. 加载包 --------------------------------------------------------------------
library(data.table)
library(tidyverse)
if (!requireNamespace("gtsummary", quietly = TRUE)) install.packages("gtsummary")
library(gtsummary)

# 解决可能的符号冲突
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

# 2. 路径设置 ------------------------------------------------------------------
base_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Master_Dataset_1_21_2026"
master_file_path <- file.path(base_path, "MASTER_DATASET_FINAL.csv")
output_path <- file.path(base_path, "Summary_Stats_By_Group_1_21_2026")

# 如果文件夹不存在，创建它
if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# 3. 读取数据与清洗 ------------------------------------------------------------
cat(">>> [1/6] Reading Master Dataset...\n")
full_df <- fread(master_file_path)

cat(">>> [2/6] Cleaning column names...\n")
# 解决重复名 + 转小写 (关键步骤)
names(full_df) <- make.unique(tolower(names(full_df)))

# 4. 核心逻辑：精细化心脏手术定义 ----------------------------------------------
cat(">>> [3/6] Applying Refined Clinical Classification...\n")

df_final <- full_df %>%
  # 1. 基础筛选：必须是全麻
  filter(str_detect(antype, regex("General", ignore_case = TRUE))) %>%
  mutate(
    # 2. 定义基础标志位
    Flag_ICD_02 = str_sub(icd10_pcs, 1, 2) == "02",         # 02开头 (心脏/大血管)
    Flag_Dept   = str_detect(department, regex("CTS", ignore_case = TRUE)), # CTS 科室
    Flag_CPB    = !is.na(cpb_duration_min) & cpb_duration_min > 0,          # 用了体外循环
    ICD_Sys_Code = str_sub(icd10_pcs, 1, 2), # 获取 ICD 系统代码
    
    # 3. 判定逻辑
    is_cardiac = case_when(
      # A. 铁定的心脏手术 (ICD=02)
      Flag_ICD_02 ~ TRUE,
      
      # B. "CTS科室 + CPB" 的复杂判断 (保留大血管，剔除器官移植)
      Flag_Dept & Flag_CPB ~ case_when(
        ICD_Sys_Code == "0B" ~ FALSE, # 剔除 呼吸系统 (肺移植)
        ICD_Sys_Code == "0T" ~ FALSE, # 剔除 泌尿系统 (肾切除)
        ICD_Sys_Code == "0F" ~ FALSE, # 剔除 肝胆系统 (肝切除)
        ICD_Sys_Code == "0D" ~ FALSE, # 剔除 消化系统 (食管)
        ICD_Sys_Code %in% c("04", "03") ~ TRUE, # 保留 动/静脉 (TAAA等大血管手术)
        TRUE ~ TRUE # 其他乱码/缺失但用了CPB的，保留
      ),
      
      # C. 其他情况 (如仅CPB但不在CTS) -> 剔除
      TRUE ~ FALSE
    ),
    
    Group = ifelse(is_cardiac, "Cardiac", "Non-Cardiac"),
    
    # 4. 数据清洗：非心脏组 CPB 强制归零 (防止统计偏差)
    cpb_duration_min = ifelse(Group == "Non-Cardiac", 0, cpb_duration_min)
  )

# 打印人数核对
cat("\n=== Final Group Counts ===\n")
print(table(df_final$Group))

# 5. 定义全量变量清单 ----------------------------------------------------------
# 直接使用您提供的所有变量，并转为小写
vars_to_analyze <- tolower(c(
  # Demographics
  "Male", "Age", "Height", "Weight", "BMI", "race", "asa", "Emergency_op", "smoking", "drinking",
  
  # Comorbidities
  "hypertension", "diabetes", "cerebrovascular_disease", "dementia", "hemiplegia_paraplegia", 
  "myocardial_infarction", "angina", "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", 
  "copd", "asthma", "ards", "renal_disease", "liver_disease", "peptic_ulcer_disease", 
  "connective_tissue_disease", "peripheral_vascular_disease", "anemia", "malignancy", "metastatic_solid_tumor", "hiv_aids",
  
  # Labs (Nearest)
  "preop_albumin_nearest", "preop_alp_nearest", "preop_alt_nearest", "preop_aptt_nearest", 
  "preop_ast_nearest", "preop_be_nearest", "preop_bun_nearest", "preop_calcium_nearest", 
  "preop_chloride_nearest", "preop_ck_nearest", "preop_ckmb_nearest", "preop_creatinine_nearest", 
  "preop_crp_nearest", "preop_d_dimer_nearest", "preop_fibrinogen_nearest", "preop_glucose_nearest", 
  "preop_hb_nearest", "preop_hba1c_nearest", "preop_hco3_nearest", "preop_hct_nearest", 
  "preop_ica_nearest", "preop_lacate_nearest", "preop_lactate_nearest", 
  "preop_lymphocyte_nearest", "preop_paco2_nearest", "preop_pao2_nearest", "preop_ph_nearest", 
  "preop_phosphorus_nearest", "preop_platelet_nearest", "preop_potassium_nearest", "preop_ptinr_nearest", 
  "preop_sao2_nearest", "preop_seg_nearest", "preop_sodium_nearest", "preop_total_bilirubin_nearest", 
  "preop_total_protein_nearest", "preop_troponin_i_nearest", "preop_troponin_t_nearest", "preop_wbc_nearest",
  
  # Medications
  "Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics", "Statins", 
  "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics", "Insulin", "Oral_hypoglycemics", 
  "Systemic_corticosteroids", "Immunosuppressants", "Inhaled_bronchodilators", "Inhaled_corticosteroids", 
  "Opioid_chronic_use", "Proton_pump_inhibitors", "Antidepressants", "Antipsychotics", "Antibiotics_systemic", 
  "Thyroid_medications", "NSAIDs", "Antiemetics", "Mucolytics_expectorants", "Antihistamines", 
  "H2_blockers", "Laxatives", "GI_prokinetics", "Benzodiazepines_sedatives", "Gabapentinoids", 
  "Antiepileptics", "Osteoporosis_medications", "Vitamin_D_Calcium", "Smoking_cessation_drugs",
  
  # Vitals
  "preop_sbp", "preop_dbp", "preop_mbp", "preop_hr", "preop_spo2", "preop_rr", "preop_bt",
  
  # Intra-op
  "op_duration_min", "anesthesia_duration_min", "or_room_time_min", "cpb_duration_min",
  
  # Outcomes
  "hosp_los_days", "icu_los_days", 
  "Stroke", "Cognitive_Decline", "Cardiac_Arrest", "Heart_Failure", "Myocardial_Injury", 
  "Resp_Failure", "Pneumonia", "Sepsis", "Infection_Organ", "Infection_Unk", 
  "AKI_Any", "AKI_Stage_1", "AKI_Stage_2", "AKI_Stage_3", 
  "Death_In_Hospital", "Death_POD30", "Death_POD90", "Death_1_Year", "Death_Long_Term"
))

# 自动处理变量存在性 (防止报错)
vars_final <- names(df_final)[names(df_final) %in% vars_to_analyze]
# 抓取重复名 (如 angina.1)
vars_duplicates <- names(df_final)[str_remove(names(df_final), "\\.\\d+$") %in% vars_to_analyze]
vars_final <- unique(c(vars_final, vars_duplicates))

cat(sprintf(">>> Analysis will include %d variables.\n", length(vars_final)))

# 6. 准备绘图数据 --------------------------------------------------------------
df_table <- df_final %>%
  select(Group, all_of(vars_final)) %>%
  mutate(
    # 处理 ASA (转数值求均值，或者转factor求分布，这里用数值)
    asa = as.numeric(asa),
    # 智能转换：将所有只有 0/1/NA 的列转为逻辑型 (显示 n(%))
    across(where(~ all(unique(na.omit(.)) %in% c(0, 1))), ~ as.logical(.))
  )

# 7. 自动化标签生成 (让表格更好看) ---------------------------------------------
generate_label <- function(x) {
  x %>% 
    str_replace_all("_", " ") %>% 
    str_to_title() %>%
    str_replace("Preop", "Pre-op") %>%
    str_replace("Cpb", "CPB") %>%
    str_replace("Bmi", "BMI") %>%
    str_replace("Asa", "ASA")
}

auto_labels <-  map(vars_final, ~ generate_label(.x))
names(auto_labels) <- vars_final

# 8. 生成表格 ------------------------------------------------------------------
cat(">>> [4/6] Generating Table 1 (This may take 3-5 minutes)...\n")

table1 <- df_table %>%
  tbl_summary(
    by = Group,
    missing = "no", 
    label = auto_labels,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  add_overall() %>%
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

# 9. 导出结果 (这是重点修改的部分) ---------------------------------------------
cat(">>> [5/6] Exporting Summary Statistics Table...\n")

# A. 导出 Table 1 统计表
final_csv <- table1 %>% as_tibble()
save_table_file <- file.path(output_path, "Table1_Summary_Stats_Refined.csv")
write_csv(final_csv, save_table_file)
cat(" -> Table 1 saved to:", save_table_file, "\n")

# B. 导出筛选后的心脏病人群体 (用于后续分析)
cat(">>> [6/6] Exporting Patient Datasets...\n")

df_cardiac_only <- df_final %>% filter(Group == "Cardiac")
save_cardiac_file <- file.path(output_path, "Cardiac_Patients_Only.csv")
fwrite(df_cardiac_only, save_cardiac_file)
cat(" -> [IMPORTANT] Cardiac cohort (N =", nrow(df_cardiac_only), ") saved to:", save_cardiac_file, "\n")

# C. 导出带有 Group 标记的完整数据 (可选，保留非心脏病人做对照)
save_full_tagged_file <- file.path(output_path, "Master_Dataset_With_Tags.csv")
fwrite(df_final, save_full_tagged_file)
cat(" -> Full tagged dataset saved to:", save_full_tagged_file, "\n")

cat("\n>>> All tasks completed successfully.\n")