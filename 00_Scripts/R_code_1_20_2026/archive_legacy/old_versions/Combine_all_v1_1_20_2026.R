library(data.table)
library(tidyverse)

# ==============================================================================
# 1. 环境设置与路径定义
# ==============================================================================
# 根目录 (根据你之前的代码设定)
base_processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"

# 定义各分表的具体路径 (请确保这些文件是你最新生成的版本)
files <- list(
  # 1. 基线表 (主表: 包含 ID, 人口学, 手术时间等)
  baseline = file.path(base_processed_path, "Baseline_data_1_20_2026", "ops_baseline_full.csv"),
  
  # 2. 诊断/合并症 (0/1 标志)
  diag     = file.path(base_processed_path, "Diagnosis_1_20_2026", "diag_preop_flags_final.csv"),
  
  # 3. 术前化验 (这里选用 30天窗口作为标准基线，如需其他窗口可修改)
  labs     = file.path(base_processed_path, "lab_data_v1_1_20_2026", "preop_labs_features_30d.csv"),
  
  # 4. 术前药物
  meds     = file.path(base_processed_path, "Meds_Preop_1_20_2026", "preop_meds.csv"),
  
  # 5. 术前生命体征 (Ward/OR Baseline)
  vitals   = file.path(base_processed_path, "Vials_pro_1_20_2026", "preop_baseline_final.csv"),
  
  # 6. 术后结局
  outcomes = file.path(base_processed_path, "Outcomes_1_20_2026", "postop_outcomes_final.csv")
)

# 输出路径
output_dir <- file.path(base_processed_path, "Master_Dataset_1_20_2026")
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 2. 读取数据
# ==============================================================================
cat("Step 1: 读取各部分数据...\n")

# 使用 fread 读取，并仅保留必要的连接键和特征列
# 主表
dt_base <- fread(files$baseline)
cat(sprintf("  -> 基线表 (Baseline): %d 行, %d 列\n", nrow(dt_base), ncol(dt_base)))

# 诊断表 (注意排除统计列)
dt_diag <- fread(files$diag, drop = c("total_icd_count", "other_icd_n"))
cat(sprintf("  -> 诊断表 (Diagnosis): %d 行, %d 列\n", nrow(dt_diag), ncol(dt_diag)))

# 化验表
dt_labs <- fread(files$labs)
cat(sprintf("  -> 化验表 (Labs 30d): %d 行, %d 列\n", nrow(dt_labs), ncol(dt_labs)))

# 药物表
dt_meds <- fread(files$meds)
cat(sprintf("  -> 药物表 (Meds): %d 行, %d 列\n", nrow(dt_meds), ncol(dt_meds)))

# 术前体征表
dt_vitals <- fread(files$vitals)
cat(sprintf("  -> 术前体征 (Preop Vitals): %d 行, %d 列\n", nrow(dt_vitals), ncol(dt_vitals)))

# 结局表
dt_outcomes <- fread(files$outcomes)
cat(sprintf("  -> 结局表 (Outcomes): %d 行, %d 列\n", nrow(dt_outcomes), ncol(dt_outcomes)))

# ==============================================================================
# 3. 执行合并 (Sequential Merge)
# ==============================================================================
cat("\nStep 2: 开始合并 (以 op_id 为核心键)...\n")

# 检查 op_id 是否唯一 (主表必须唯一)
if (anyDuplicated(dt_base$op_id)) warning("警告: 基线表中 op_id 有重复！")

# --- 3.1 合并诊断 ---
# 很多表可能包含 subject_id，为了防止列名冲突 (subject_id.x, subject_id.y)，
# 我们在 merge 前先把分表里的 subject_id 去掉 (如果它存在)，只用 op_id 连接
clean_merge <- function(main, part, suffix_name) {
  # 移除 part 中除了 op_id 以外的重复 ID 列 (如 subject_id)
  cols_to_drop <- intersect(names(part), c("subject_id", "hadm_id", "case_id"))
  part_clean <- part[, !..cols_to_drop] 
  
  # 执行 Left Join
  merged <- merge(main, part_clean, by = "op_id", all.x = TRUE)
  return(merged)
}

# 链式合并
master_dt <- dt_base %>%
  clean_merge(dt_diag, "diag") %>%
  clean_merge(dt_labs, "labs") %>%
  clean_merge(dt_meds, "meds") %>%
  clean_merge(dt_vitals, "vitals") %>%
  clean_merge(dt_outcomes, "outcomes")

# ==============================================================================
# 4. 数据完整性检查 (Quality Check)
# ==============================================================================
cat("\nStep 3: 生成拼接质量报告...\n")

total_rows <- nrow(master_dt)

# 定义要检查的关键列 (代表各表的连接情况)
check_cols <- c(
  "Baseline" = "op_id",               # 必然存在
  "Diagnosis" = "hypertension",       # 来自诊断表
  "Labs" = "preop_creatinine_nearest",# 来自化验表
  "Meds" = "Beta_blockers",           # 来自药物表
  "Vitals" = "preop_sbp",             # 来自体征表
  "Outcomes" = "Death_POD30"          # 来自结局表
)

# 计算匹配率
qc_report <- data.frame(
  Table = names(check_cols),
  Column_Checked = check_cols,
  Matched_Rows = sapply(check_cols, function(col) sum(!is.na(master_dt[[col]]))),
  Total_Rows = total_rows
) %>%
  mutate(
    Match_Rate_Pct = round(Matched_Rows / Total_Rows * 100, 2),
    Missing_Rows = Total_Rows - Matched_Rows
  )

print(qc_report, row.names = FALSE)

# ==============================================================================
# 5. 最终清洗与保存
# ==============================================================================
cat("\nStep 4: 最终清洗与保存...\n")

# 5.1 填补 0/1 标志变量的 NA
# 逻辑：对于 Diagnosis, Meds, Outcomes 中的二分类变量，如果 merge 后是 NA，
# 通常意味着该病人在分表中没有记录 -> 视为 0 (无病/无药/无并发症)
# 注意：化验(Labs)和体征(Vitals)的 NA 是真正的缺失值，不能填 0！

# 识别二分类列：通常是 integer 类型且取值只有 0, 1 或 NA
# 这里简单通过列名特征或手动指定来处理
# (为了安全，这里只演示对明确的标志列填 0)

# 获取诊断和药物的列名
diag_cols <- setdiff(names(dt_diag), "op_id")
meds_cols <- setdiff(names(dt_meds), c("op_id", "subject_id"))
outcome_flags <- c("Stroke", "AKI_Any", "Death_POD30", "Death_In_Hospital") # 举例

cols_to_fill_zero <- c(diag_cols, meds_cols, outcome_flags)
# 仅保留 master_dt 中实际存在的列
cols_to_fill_zero <- intersect(cols_to_fill_zero, names(master_dt))

cat(sprintf("  -> 正在将 %d 个标志变量的 NA 填补为 0 (假设无记录即无发生)...\n", length(cols_to_fill_zero)))

# data.table 高效填补
for (j in cols_to_fill_zero) {
  set(master_dt, which(is.na(master_dt[[j]])), j, 0)
}

# 5.2 保存
final_file <- file.path(output_dir, "MASTER_DATASET_FINAL.csv")
fwrite(master_dt, final_file)

cat("=======================================================\n")
cat("汇总完成！\n")
cat(sprintf("最终数据维度: %d 行 x %d 列\n", nrow(master_dt), ncol(master_dt)))
cat(sprintf("文件已保存至: %s\n", final_file))
cat("=======================================================\n")