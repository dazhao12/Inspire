library(tidyverse)
library(data.table)

# ==============================================================================
# 1. 路径设置
# ==============================================================================
# 基础路径
base_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Master_Dataset_1_20_2026"

# 您已经整理好的全量文件路径
master_file_path <- file.path(base_path, "MASTER_DATASET_FINAL.csv")

# 输出路径
output_path <- file.path(base_path, "Summary_Stats_By_Group_1_21_2026")
if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# ==============================================================================
# 2. 读取数据 (直接读取 Master File)
# ==============================================================================
if (file.exists(master_file_path)) {
  cat(">>> 正在读取全量数据文件: ", master_file_path, "\n")
  full_df <- fread(master_file_path)
  cat(">>> 数据读取成功！样本量: ", nrow(full_df), "\n")
} else {
  stop("错误：未找到文件 INSPIRE_Full_Master_1_21_2026.csv，请检查路径或先运行合并脚本。")
}

# ==============================================================================
# 3. 定义分组 (Cardiac vs Non-Cardiac)
# ==============================================================================
cat(">>> 正在定义分组...\n")

full_df <- full_df %>%
  mutate(
    Group = case_when(
      # 核心定义：CTS 为心脏组
      department == "CTS" ~ "Cardiac",
      # 其余均为非心脏组
      TRUE ~ "Non-Cardiac"
    )
  )

# 打印分组人数核对
print(table(full_df$Group))

# ==============================================================================
# 4. 定义变量清单 (严格按临床时间轴排序)
# ==============================================================================
# 提示：这里根据您提供的变量名列表进行了精选，去掉了过于冷门的化验，保留了核心指标

vars_timeline <- c(
  # ===================================================
  # A. 术前基线 (Pre-operative Baseline)
  # ===================================================
  
  # --- 1. 人口学 (Demographics) ---
  "Age", "Male", "BMI", "asa", "Emergency_op", 
  "smoking", "drinking",
  
  # --- 2. 术前合并症 (Comorbidities - History) ---
  # 注意：这是病史，不是术后并发症
  "hypertension", "diabetes", 
  "coronary_artery_disease", "myocardial_infarction", # 心脏病史
  "heart_failure", "atrial_fibrillation", "arrhythmia_any",
  "cerebrovascular_disease", "dementia",              # 神经病史
  "copd", "asthma",                                   # 呼吸病史
  "renal_disease", "liver_disease", 
  "peptic_ulcer_disease", "malignancy", "metastatic_solid_tumor",
  
  # --- 3. 术前用药 (Home Medications) ---
  "Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs",
  "Diuretics", "Statins", "Antiplatelet_agents", "Anticoagulants",
  "Insulin", "Oral_hypoglycemics", "Systemic_corticosteroids", 
  "Inhaled_bronchodilators", "Opioid_chronic_use", "NSAIDs",
  
  # --- 4. 术前体征 (Pre-op Vitals) ---
  "preop_sbp", "preop_dbp", "preop_hr", "preop_spo2", "preop_bt",
  
  # --- 5. 术前化验 (Pre-op Labs - Nearest) ---
  # 选取最接近手术的一个值，代表术前状态
  "preop_hb_nearest", "preop_hct_nearest", "preop_platelet_nearest", "preop_wbc_nearest", # 血常规
  "preop_creatinine_nearest", "preop_bun_nearest", # 肾功
  "preop_albumin_nearest", "preop_total_bilirubin_nearest", "preop_alt_nearest", "preop_ast_nearest", # 肝功
  "preop_glucose_nearest", "preop_sodium_nearest", "preop_potassium_nearest", # 电解质/血糖
  "preop_ptinr_nearest", "preop_aptt_nearest", # 凝血
  
  # ===================================================
  # B. 术中情况 (Intra-operative)
  # ===================================================
  "op_duration_min", 
  "anesthesia_duration_min", 
  "cpb_duration_min", # 体外循环时长 (心脏组特有)
  
  # ===================================================
  # C. 术后结局 (Post-operative Outcomes)
  # ===================================================
  
  # --- 1. 住院时长 (LOS) ---
  "hosp_los_days", "icu_los_days",
  
  # --- 2. 术后并发症 (Complications - In Hospital) ---
  # 神经
  "Stroke", "Cognitive_Decline",
  # 心血管
  "Cardiac_Arrest", "Myocardial_Injury", "Arrhythmia_Vent", "Atrial_Fib",
  # 呼吸/感染/肾脏
  "Pneumonia", "Resp_Failure", "Sepsis", "Infection_Organ",
  "AKI_Any", "AKI_Stage_3",
  
  # --- 3. 死亡结局 (Mortality) ---
  "Death_In_Hospital", 
  "Death_POD30", 
  "Death_1_Year"
)

# 容错处理：只保留数据中实际存在的列
vars_timeline <- intersect(vars_timeline, names(full_df))

# ==============================================================================
# 5. 统计计算 (生成三列表格)
# ==============================================================================
cat(">>> 正在计算统计量...\n")

# 辅助函数：格式化
fmt_num <- function(x) sprintf("%.2f", x)  # 连续变量保留2位小数
fmt_pct <- function(n, N) sprintf("%d (%.1f%%)", n, (n/N)*100) # 分类变量 n (%)

# 计算分母
N_total <- nrow(full_df)
N_cardiac <- sum(full_df$Group == "Cardiac", na.rm = TRUE)
N_noncardiac <- sum(full_df$Group == "Non-Cardiac", na.rm = TRUE)

results_list <- list()

for (var in vars_timeline) {
  
  vals <- full_df[[var]]
  grps <- full_df$Group
  
  # --- 智能判断变量类型 ---
  # 1. 移除 NA 后去重
  valid_vals <- na.omit(vals)
  unique_vals <- unique(valid_vals)
  
  # 2. 判定逻辑：
  #    - 如果只有 0和1，或者 TRUE/FALSE -> 分类 (Categorical)
  #    - 如果是字符型 -> 分类
  #    - 如果数值型且唯一值 > 5个 -> 连续 (Continuous)
  #    - 特殊修正：ASA 虽然是 1-6，但通常作为连续变量算均值展示，或者你可以改为分类
  
  is_binary <- (all(unique_vals %in% c(0, 1)) && length(unique_vals) <= 2) || is.logical(vals)
  is_numeric_many <- is.numeric(vals) && length(unique_vals) > 5
  
  # 初始化行
  row_res <- data.frame(
    Variable = var, 
    Type = "", 
    Overall = "", 
    Non_Cardiac = "", 
    Cardiac = ""
  )
  
  # --- 计算 ---
  if (is_numeric_many && !is_binary) {
    # >>> 连续变量 (Mean ± SD)
    row_res$Type <- "Mean ± SD"
    
    # Overall
    m <- mean(vals, na.rm=T); s <- sd(vals, na.rm=T)
    row_res$Overall <- paste0(fmt_num(m), " ± ", fmt_num(s))
    
    # Non-Cardiac
    v_nc <- vals[grps == "Non-Cardiac"]
    m <- mean(v_nc, na.rm=T); s <- sd(v_nc, na.rm=T)
    row_res$Non_Cardiac <- paste0(fmt_num(m), " ± ", fmt_num(s))
    
    # Cardiac
    v_c <- vals[grps == "Cardiac"]
    m <- mean(v_c, na.rm=T); s <- sd(v_c, na.rm=T)
    row_res$Cardiac <- paste0(fmt_num(m), " ± ", fmt_num(s))
    
  } else {
    # >>> 分类变量 n (%)
    # 默认计算 "1" (Yes) 的占比
    row_res$Type <- "n (%)"
    
    # Overall
    n_hits <- sum(vals == 1, na.rm=T)
    row_res$Overall <- fmt_pct(n_hits, N_total)
    
    # Non-Cardiac
    v_nc <- vals[grps == "Non-Cardiac"]
    n_hits <- sum(v_nc == 1, na.rm=T)
    row_res$Non_Cardiac <- fmt_pct(n_hits, N_noncardiac)
    
    # Cardiac
    v_c <- vals[grps == "Cardiac"]
    n_hits <- sum(v_c == 1, na.rm=T)
    row_res$Cardiac <- fmt_pct(n_hits, N_cardiac)
  }
  
  results_list[[var]] <- row_res
}

# 合并所有行
final_table <- bind_rows(results_list)

# ==============================================================================
# 6. 美化输出
# ==============================================================================
# 添加顶部 N 行
header_row <- data.frame(
  Variable = "Total Patients (N)",
  Type = "",
  Overall = as.character(N_total),
  Non_Cardiac = as.character(N_noncardiac),
  Cardiac = as.character(N_cardiac)
)

final_table <- bind_rows(header_row, final_table)

# 为了方便阅读，我们可以把变量名里的 "_" 替换为空格，首字母大写 (可选)
final_table$Variable <- str_to_title(str_replace_all(final_table$Variable, "_", " "))

# 保存
file_name <- "Table1_Cardiac_vs_NonCardiac_Timeline.csv"
save_full_path <- file.path(output_path, file_name)

write_csv(final_table, save_full_path)

cat("\n=======================================================\n")
cat("            Table 1 生成成功 (按临床时间轴)            \n")
cat("=======================================================\n")
cat("分组定义: \n - Cardiac: department == 'CTS'\n - Non-Cardiac: All others\n")
cat("-------------------------------------------------------\n")
print(head(final_table, 20))
cat("\n>>> 文件已保存至:", save_full_path, "\n")