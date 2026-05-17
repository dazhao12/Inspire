library(data.table)
library(tidyverse)

# ==============================================================================
# 1. 环境设置与路径定义
# ==============================================================================
# 根目录
base_processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"

# 定义各分表的具体路径 
# (注意：这里加入了第 7 项：Intra-op Summary)
files <- list(
  # 1. 基线表 (主表)
  baseline = file.path(base_processed_path, "Baseline_data_1_20_2026", "ops_baseline_full.csv"),
  
  # 2. 诊断/合并症
  diag     = file.path(base_processed_path, "Diagnosis_1_20_2026", "diag_preop_flags_final.csv"),
  
  # 3. 术前化验
  labs     = file.path(base_processed_path, "lab_data_v1_1_20_2026", "preop_labs_features_30d.csv"),
  
  # 4. 术前药物
  meds     = file.path(base_processed_path, "Meds_Preop_1_20_2026", "preop_meds.csv"),
  
  # 5. 术前体征 (Pre-op Baseline) - 注意这是术前的
  vitals_pre = file.path(base_processed_path, "Vials_pro_1_20_2026", "preop_baseline_final.csv"),
  
  # 6. 术后结局
  outcomes = file.path(base_processed_path, "Outcomes_1_20_2026", "postop_outcomes_final.csv"),
  
  # 7. [新增] 术中药物/液体/出入量汇总 (从刚才生成的文件夹中读取)
  intra_op = file.path(base_processed_path, "Vials_intra_1_21_final_2026", "drugs_fluids_total_sum.csv")
)

# 输出路径
output_dir <- file.path(base_processed_path, "Master_Dataset_1_21_2026")
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 2. 读取数据
# ==============================================================================
cat("Step 1: Reading datasets...\n")

dt_base     <- fread(files$baseline)
cat(sprintf("  -> Baseline:       %d rows, %d cols\n", nrow(dt_base), ncol(dt_base)))

dt_diag     <- fread(files$diag, drop = c("total_icd_count", "other_icd_n"))
cat(sprintf("  -> Diagnosis:      %d rows, %d cols\n", nrow(dt_diag), ncol(dt_diag)))

dt_labs     <- fread(files$labs)
cat(sprintf("  -> Preop Labs:     %d rows, %d cols\n", nrow(dt_labs), ncol(dt_labs)))

dt_meds     <- fread(files$meds)
cat(sprintf("  -> Preop Meds:     %d rows, %d cols\n", nrow(dt_meds), ncol(dt_meds)))

dt_vitals_pre <- fread(files$vitals_pre)
cat(sprintf("  -> Preop Vitals:   %d rows, %d cols\n", nrow(dt_vitals_pre), ncol(dt_vitals_pre)))

dt_outcomes <- fread(files$outcomes)
cat(sprintf("  -> Outcomes:       %d rows, %d cols\n", nrow(dt_outcomes), ncol(dt_outcomes)))

# [新增] 读取术中汇总
dt_intra    <- fread(files$intra_op)
cat(sprintf("  -> Intra-op Sums:  %d rows, %d cols (New Added)\n", nrow(dt_intra), ncol(dt_intra)))

# ==============================================================================
# 3. 执行合并 (Sequential Merge)
# ==============================================================================
cat("\nStep 2: Merging all tables by 'op_id'...\n")

# 检查 op_id 唯一性
if (anyDuplicated(dt_base$op_id)) warning("WARNING: Duplicate op_id in Baseline!")

# 定义清理合并函数 (自动去除重复的 subject_id 等列)
clean_merge <- function(main, part) {
  # 移除 part 中除了 op_id 以外的重复 ID 列
  cols_to_drop <- intersect(names(part), c("subject_id", "hadm_id", "case_id", "surgery_number"))
  # 确保不删除 op_id
  cols_to_drop <- setdiff(cols_to_drop, "op_id")
  
  part_clean <- part[, !..cols_to_drop] 
  
  # Left Join
  merged <- merge(main, part_clean, by = "op_id", all.x = TRUE)
  return(merged)
}

# 链式合并
master_dt <- dt_base %>%
  clean_merge(dt_diag) %>%
  clean_merge(dt_labs) %>%
  clean_merge(dt_meds) %>%
  clean_merge(dt_vitals_pre) %>%
  clean_merge(dt_intra) %>%    # <--- 合并术中数据
  clean_merge(dt_outcomes)

# ==============================================================================
# 4. 数据完整性检查 (Quality Check)
# ==============================================================================
cat("\nStep 3: Quality Check Report...\n")

# 定义检查列
check_cols <- c(
  "Baseline (op_id)"   = "op_id",
  "Diagnosis (HTN)"    = "hypertension",
  "Labs (Cr)"          = "preop_creatinine_nearest",
  "Preop Meds (BB)"    = "Beta_blockers",
  "Preop Vitals (SBP)" = "preop_sbp",
  "Intra-op (Propofol)"= "ppf",          # <--- 检查新数据
  "Intra-op (EBL)"     = "ebl",          # <--- 检查失血量
  "Outcomes (Death)"   = "Death_POD30"
)

qc_report <- data.frame(
  Table = names(check_cols),
  Column = check_cols,
  Matched = sapply(check_cols, function(col) sum(!is.na(master_dt[[col]]))),
  Total = nrow(master_dt)
) %>%
  mutate(Match_Rate = paste0(round(Matched/Total*100, 1), "%"))

print(qc_report, row.names = FALSE)

# ==============================================================================
# 5. 最终清洗与保存
# ==============================================================================
cat("\nStep 4: Final Cleaning & Exporting...\n")

# --- 5.1 填补 0/1 标志变量 ---
# 诊断、术前药物、结局: NA -> 0
diag_cols <- setdiff(names(dt_diag), "op_id")
meds_cols <- setdiff(names(dt_meds), c("op_id", "subject_id"))
outcome_flags <- c("Stroke", "AKI_Any", "Death_POD30", "Death_In_Hospital") 

cols_to_fill_zero <- intersect(c(diag_cols, meds_cols, outcome_flags), names(master_dt))

# --- 5.2 [关键] 填补术中药物/液体的 NA ---
# 逻辑：如果合并后 Intra-op 某药为 NA，说明汇总表中没记录，意味着使用量为 0
intra_cols <- setdiff(names(dt_intra), c("op_id", "subject_id", "surgery_number"))
# 将术中变量也加入到填补列表中
cols_to_fill_zero <- unique(c(cols_to_fill_zero, intra_cols))

# 执行填补
cat(sprintf("  -> Filling NAs with 0 for %d flag/drug columns...\n", length(cols_to_fill_zero)))
for (j in cols_to_fill_zero) {
  # 只有在该列确实存在 NA 时才执行，节省时间
  if (any(is.na(master_dt[[j]]))) {
    set(master_dt, which(is.na(master_dt[[j]])), j, 0)
  }
}

# --- 5.3 保存 ---
final_file <- file.path(output_dir, "MASTER_DATASET_FINAL.csv")
fwrite(master_dt, final_file)

cat("=======================================================\n")
cat("SUCCESS! Master Dataset Created.\n")
cat(sprintf("File saved to: %s\n", final_file))
cat(sprintf("Dimensions: %d rows x %d cols\n", nrow(master_dt), ncol(master_dt)))
cat("=======================================================\n")