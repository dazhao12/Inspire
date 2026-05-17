library(tidyverse)
library(data.table)

# ------------------------------------------------------------------
# 0. 设置路径与读取
# ------------------------------------------------------------------
# 输入数据路径
input_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
# 输出保存路径 (建议单独建一个 processed 文件夹，这里暂存在同级目录)
output_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Baseline_data_1_20_2026"

# 如果输出目录不存在，创建它
if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# 读取数据
ops_raw <- fread(file.path(input_path, "operations.csv"), na.strings = c("", "NA")) %>% 
  as_tibble()

# ------------------------------------------------------------------
# 1. 核心清洗逻辑 (生成全量宽表)
# ------------------------------------------------------------------
ops_baseline <- ops_raw %>%
  mutate(
    # --- 类型转换 ---
    Male = case_when(sex == 'M' ~ 1, sex == 'F' ~ 0, TRUE ~ NA_real_),
    across(c(height, weight, age), ~as.numeric(.)),
    across(ends_with("_time"), ~as.numeric(.)),
    
    # --- 指标计算 ---
    BMI = if_else(height > 0, round(weight / ((height/100)^2), 1), NA_real_),
    
    # 手术流程时间 (分钟)
    op_duration_min = if_else(opend_time > opstart_time, opend_time - opstart_time, NA_real_),
    anesthesia_duration_min = if_else(anend_time > anstart_time, anend_time - anstart_time, NA_real_),
    or_room_time_min = if_else(orout_time > orin_time, orout_time - orin_time, NA_real_),
    cpb_duration_min = if_else(cpboff_time > cpbon_time, cpboff_time - cpbon_time, NA_real_),
    
    # LOS
    hosp_los_min = if_else(discharge_time > admission_time, discharge_time - admission_time, NA_real_),
    hosp_los_days = if_else(discharge_time > admission_time, round((discharge_time - admission_time) / 1440, 1), NA_real_),
    icu_los_min = if_else(icuout_time > icuin_time, icuout_time - icuin_time, NA_real_),
    icu_los_days = if_else(icuout_time > icuin_time, round((icuout_time - icuin_time) / 1440, 1), NA_real_),
    
    # 死亡时间
    time_to_inhosp_death_min = if_else(inhosp_death_time >= admission_time & inhosp_death_time <= discharge_time, inhosp_death_time - admission_time, NA_real_),
    time_to_inhosp_death_days = if_else(inhosp_death_time >= admission_time & inhosp_death_time <= discharge_time, round((inhosp_death_time - admission_time) / 1440, 1), NA_real_),
    time_to_allcause_death_min = if_else(allcause_death_time > admission_time, allcause_death_time - admission_time, NA_real_),
    time_to_allcause_death_days = if_else(allcause_death_time > admission_time, round((allcause_death_time - admission_time) / 1440, 1), NA_real_)
  ) %>%
  
  # --- 最终全量字段选择 ---
  select(
    # ID
    op_id, subject_id, hadm_id, case_id, opdate,
    
    # 人口学与术前
    Male, Age = age, Height = height, Weight = weight, BMI,
    race, asa, Emergency_op = emop, department, 
    
    # 术中信息
    antype, icd10_pcs,
    op_duration_min, anesthesia_duration_min, or_room_time_min, cpb_duration_min,
    
    # 结局 (LOS & Death)
    hosp_los_min, hosp_los_days, icu_los_min, icu_los_days,
    time_to_inhosp_death_min, time_to_inhosp_death_days,
    time_to_allcause_death_min, time_to_allcause_death_days,
    
    # 原始 Raw 时间点 (放在最后，方便查阅但不干扰主要视野)
    admission_time_min_raw = admission_time,
    discharge_time_min_raw = discharge_time,
    opstart_time_min_raw = opstart_time,
    opend_time_min_raw = opend_time,
    anstart_time_min_raw = anstart_time,
    anend_time_min_raw = anend_time,
    cpbon_time_min_raw = cpbon_time,
    cpboff_time_min_raw = cpboff_time,
    inhosp_death_time_min_raw = inhosp_death_time,
    allcause_death_time_min_raw = allcause_death_time
  )

# ------------------------------------------------------------------
# 2. 拆分与保存 (Split and Save)
# ------------------------------------------------------------------

# 2.1 保存全量整合版本 (Full Baseline)
# -----------------------------------
fwrite(ops_baseline, file.path(output_path, "ops_baseline_full.csv"))
cat("文件 1: ops_baseline_full.csv 已保存。\n")

# 2.2 提取“ID + 术前信息” (Pre-operative Info)
# 包含：ID, 人口学(年龄/性别/BMI), 种族, ASA分级, 是否急诊, 科室, 入院时间
# -----------------------------------
ops_preop <- ops_baseline %>%
  select(
    op_id, subject_id, hadm_id, case_id, opdate, # 必须保留 ID
    Male, Age, Height, Weight, BMI,              # 身体特征
    race, asa, Emergency_op, department,         # 临床特征
    admission_time_min_raw                       # 关键时间点
  )

fwrite(ops_preop, file.path(output_path, "ops_preop_only.csv"))
cat("文件 2: ops_preop_only.csv (术前表) 已保存。\n")

# 2.3 提取“ID + 术中信息” (Intra-operative Info)
# 包含：ID, 手术/麻醉/CPB时长, 术式ICD, 麻醉类型, 以及相关时间点
# -----------------------------------
ops_intraop <- ops_baseline %>%
  select(
    op_id, subject_id, hadm_id, case_id, opdate, # 必须保留 ID
    icd10_pcs, antype,                           # 术式与麻醉方式
    op_duration_min, anesthesia_duration_min,    # 关键时长
    or_room_time_min, cpb_duration_min,
    
    # 结局 (LOS & Death)
    hosp_los_min, hosp_los_days, icu_los_min, icu_los_days,
    time_to_inhosp_death_min, time_to_inhosp_death_days,
    time_to_allcause_death_min, time_to_allcause_death_days,
    
    opstart_time_min_raw, opend_time_min_raw,    # 关键时间点
    anstart_time_min_raw, anend_time_min_raw,
    cpbon_time_min_raw, cpboff_time_min_raw,
    inhosp_death_time_min_raw ,allcause_death_time_min_raw
    
  )

fwrite(ops_intraop, file.path(output_path, "ops_intraop_only.csv"))
cat("文件 3: ops_intraop_only.csv (术中表) 已保存。\n")

# 2.4 (可选) 如果你想检查一下数据
print(head(ops_preop))
print(head(ops_intraop))
