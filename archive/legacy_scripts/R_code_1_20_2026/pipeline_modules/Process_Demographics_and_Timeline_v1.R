library(tidyverse)
library(data.table)

# ------------------------------------------------------------------
# 0. 设置路径与读取
# ------------------------------------------------------------------
# 输入数据路径
input_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
# 输出保存路径
output_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Demographics_Timeline_2_19_2026"

# 如果输出目录不存在，创建它
if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# 读取数据
cat("正在读取原始数据...\n")
ops_raw <- fread(file.path(input_path, "operations.csv"), na.strings = c("", "NA")) %>% 
  as_tibble()

# ------------------------------------------------------------------
# 1. 基础数预处理 (Common Preprocessing)
# ------------------------------------------------------------------
cat("正在进行基础预处理...\n")
ops_cleaned <- ops_raw %>%
  mutate(
    # --- 类型转换 ---
    # 性别转换为数值: M -> 1, F -> 0
    Male = case_when(sex == 'M' ~ 1, sex == 'F' ~ 0, TRUE ~ NA_real_),
    
    # 确保数值型
    across(c(height, weight, age), ~as.numeric(.)),
    
    # 计算 BMI
    BMI = if_else(height > 0, weight / ((height/100)^2), NA_real_),
    
    # 重命名/规范化列名以匹配需求
    Age = age,
    Height = height,
    Weight = weight,
    Emergency_op = emop
  )

# ------------------------------------------------------------------
# 2. 生成版本一：Subject Level Demographic 表
# ------------------------------------------------------------------
# 逻辑：以 subject_id 为行，连续变量取平均值，分类变量取第一个非空值
cat("正在生成 Subject Level Demographic 表...\n")

get_mode_or_first <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return(NA)
  return(first(x)) # 简化处理，取第一个有效值
}

demog_subject_level <- ops_cleaned %>%
  group_by(subject_id) %>%
  summarise(
    # 连续变量取平均值 (na.rm = TRUE)
    Age = mean(Age, na.rm = TRUE),
    Height = mean(Height, na.rm = TRUE),
    Weight = mean(Weight, na.rm = TRUE),
    BMI = mean(BMI, na.rm = TRUE),
    
    # 分类变量 (Male, race) - 假设每个病人是固定的，取第一个有效值
    Male = get_mode_or_first(Male),
    race = get_mode_or_first(race)
  ) %>%
  ungroup() %>%
  arrange(subject_id)

# 写入文件
fwrite(demog_subject_level, file.path(output_path, "Demographic_Subject_Level.csv"))
cat(" -> Demographic_Subject_Level.csv 已保存。\n")

# ------------------------------------------------------------------
# 3. 生成版本二：Operation Level Demographic 表
# ------------------------------------------------------------------
# 逻辑：以 op_id 为行，不合并，仅排序
cat("正在生成 Operation Level Demographic 表...\n")

demog_op_level <- ops_cleaned %>%
  select(
    subject_id,
    op_id,
    hadm_id,
    case_id,
    opdate,
    Male,
    Age,
    Height,
    Weight,
    BMI,
    race,
    asa,
    Emergency_op,
    department,
    antype,
    icd10_pcs
  ) %>%
  arrange(subject_id, opdate) # 升序排列

# 写入文件
fwrite(demog_op_level, file.path(output_path, "Demographic_Operation_Level.csv"))
cat(" -> Demographic_Operation_Level.csv 已保存。\n")

# ------------------------------------------------------------------
# 4. 生成版本三：时间相关表 (Time Related)
# ------------------------------------------------------------------
# 逻辑：涉及大量时间计算和逻辑检查
cat("正在生成 Time Related 表...\n")

timeline_data <- ops_cleaned %>%
  mutate(
    # 原始时间戳重命名 (Raw)
    admission_time_min_raw = admission_time,
    discharge_time_min_raw = discharge_time,
    opstart_time_min_raw = opstart_time,
    opend_time_min_raw = opend_time,
    anstart_time_min_raw = anstart_time,
    anend_time_min_raw = anend_time,
    cpbon_time_min_raw = cpbon_time,
    cpboff_time_min_raw = cpboff_time,
    inhosp_death_time_min_raw = inhosp_death_time,
    allcause_death_time_min_raw = allcause_death_time,
    
    # --- 时长计算 ---
    op_duration_min = if_else(opend_time > opstart_time, opend_time - opstart_time, NA_real_),
    anesthesia_duration_min = if_else(anend_time > anstart_time, anend_time - anstart_time, NA_real_),
    or_room_time_min = if_else(orout_time > orin_time, orout_time - orin_time, NA_real_),
    cpb_duration_min = if_else(cpboff_time > cpbon_time, cpboff_time - cpbon_time, NA_real_),
    
    hosp_los_min = if_else(discharge_time > admission_time, discharge_time - admission_time, NA_real_),
    hosp_los_days = if_else(discharge_time > admission_time, (discharge_time - admission_time) / 1440, NA_real_),
    
    icu_los_min = if_else(icuout_time > icuin_time, icuout_time - icuin_time, NA_real_),
    icu_los_days = if_else(icuout_time > icuin_time, (icuout_time - icuin_time) / 1440, NA_real_),
    
    # 死亡时间计算 (相对于入院时间)
    time_to_inhosp_death_min = if_else(!is.na(inhosp_death_time) & !is.na(admission_time), 
                                       inhosp_death_time - admission_time, NA_real_),
    time_to_inhosp_death_days = if_else(!is.na(inhosp_death_time) & !is.na(admission_time), 
                                        (inhosp_death_time - admission_time) / 1440, NA_real_),
    
    time_to_allcause_death_min = if_else(!is.na(allcause_death_time) & !is.na(admission_time), 
                                         allcause_death_time - admission_time, NA_real_),
    time_to_allcause_death_days = if_else(!is.na(allcause_death_time) & !is.na(admission_time), 
                                          (allcause_death_time - admission_time) / 1440, NA_real_)
  ) %>%
  arrange(subject_id, opdate)

# --- 逻辑检查 (Validation Checks) ---
# 检查1: 住院时间逻辑 (出院 < 入院)
timeline_data <- timeline_data %>%
  mutate(
    flag_los_error = (discharge_time_min_raw < admission_time_min_raw),
    flag_op_time_error = (opend_time_min_raw < opstart_time_min_raw),
    flag_death_before_admission = (allcause_death_time_min_raw < admission_time_min_raw)
  )

# 检查2: 同一个病人多次住院的时间顺序
# 如果一个病人的下一条记录的 opdate 早于上一条的 discharge (且是不同的 admission)，可能有问题
# 注意：这里我们只能简单检查 opdate 是否按顺序（已经arrange了）。
timeline_data <- timeline_data %>%
  group_by(subject_id) %>%
  mutate(
    prev_discharge_time = lag(discharge_time_min_raw),
    # 检查: 当前 opdate 是否早于上一次 discharge (用于发现重叠住院/手术记录)
    # 注意: admission_time_min_raw 是相对于 0 点的分钟数，如果是不同次住院，需要看 raw 数据是否是全局时间戳。
    # 假设 INSPIRE 数据中的时间是相对于某个 Reference Date 的分钟数。
    flag_overlap_with_prev = if_else(!is.na(prev_discharge_time) & admission_time_min_raw < prev_discharge_time, TRUE, FALSE)
  ) %>%
  ungroup()

# 输出检查报告
cat("\n--- 数据逻辑检查报告 ---\n")
n_los_err <- sum(timeline_data$flag_los_error, na.rm=TRUE)
n_op_err <- sum(timeline_data$flag_op_time_error, na.rm=TRUE)
n_death_err <- sum(timeline_data$flag_death_before_admission, na.rm=TRUE)
n_overlap <- sum(timeline_data$flag_overlap_with_prev, na.rm=TRUE)

cat(sprintf("住院时间倒置 (出院 < 入院): %d 例\n", n_los_err))
cat(sprintf("手术时间倒置 (结束 < 开始): %d 例\n", n_op_err))
cat(sprintf("死亡时间早于入院: %d 例\n", n_death_err))
cat(sprintf("当前入院时间早于上次出院 (可能的时间重叠): %d 例\n", n_overlap))

# 选择最终列
timeline_final <- timeline_data %>%
  select(
    op_id, subject_id, hadm_id, case_id, opdate,
    
    # 原始时间戳
    admission_time_min_raw, discharge_time_min_raw,
    opstart_time_min_raw, opend_time_min_raw,
    anstart_time_min_raw, anend_time_min_raw,
    cpbon_time_min_raw, cpboff_time_min_raw,
    inhosp_death_time_min_raw, allcause_death_time_min_raw,
    
    # 计算时长
    op_duration_min, anesthesia_duration_min,
    or_room_time_min, cpb_duration_min,
    hosp_los_min, hosp_los_days,
    icu_los_min, icu_los_days,
    time_to_inhosp_death_min, time_to_inhosp_death_days,
    time_to_allcause_death_min, time_to_allcause_death_days,
    
    # 质量控制标记
    flag_los_error, flag_op_time_error, 
    flag_death_before_admission, flag_overlap_with_prev
  )

# 写入文件
fwrite(timeline_final, file.path(output_path, "Time_Related_Data.csv"))
cat(" -> Time_Related_Data.csv 已保存。\n")

cat("\n所有处理完成。\n")
