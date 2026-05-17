library(data.table)

# ==============================================================================
# 1. 环境设置与路径定义
# ==============================================================================
raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_pro_1_20_2026"

if (!dir.exists(processed_path)) {
  dir.create(processed_path, recursive = TRUE)
}

# 明确窗口定义，避免“魔法数字”
WARD_WINDOW_MIN <- 1440  # 术前 24h
OR_WINDOW_MIN <- 120     # 入室前 120min

# ==============================================================================
# 2. 读取数据
# ==============================================================================
cat("正在读取数据...\n")

ops <- fread(
  file.path(raw_path, "operations.csv"),
  select = c("op_id", "subject_id", "hadm_id", "admission_time", "orin_time")
)

ward_vitals <- fread(
  file.path(raw_path, "ward_vitals.csv"),
  select = c("subject_id", "chart_time", "item_name", "value")
)

or_vitals <- fread(
  file.path(raw_path, "vitals.csv"),
  select = c("op_id", "chart_time", "item_name", "value")
)

# ==============================================================================
# 3. 预处理
# ==============================================================================
cat("正在清洗数据...\n")

ops[, `:=`(
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time)
)]
ops <- unique(ops[!is.na(op_id) & !is.na(subject_id) & !is.na(orin_time)])

ward_vitals[, `:=`(
  chart_time = as.numeric(chart_time),
  value = as.numeric(value)
)]
or_vitals[, `:=`(
  chart_time = as.numeric(chart_time),
  value = as.numeric(value)
)]

ward_items <- c("nibp_sbp", "nibp_dbp", "nibp_mbp", "hr", "spo2", "rr", "bt")
ward_subset <- ward_vitals[item_name %in% ward_items & !is.na(chart_time)]

# 统一 OR item 到分组名
or_vitals[item_name %in% c("nibp_sbp", "art_sbp"), item_group := "sbp"]
or_vitals[item_name %in% c("nibp_dbp", "art_dbp"), item_group := "dbp"]
or_vitals[item_name %in% c("nibp_mbp", "art_mbp"), item_group := "mbp"]
or_vitals[item_name == "hr", item_group := "hr"]
or_vitals[item_name == "spo2", item_group := "spo2"]
or_vitals[item_name == "rr", item_group := "rr"]
or_vitals[item_name == "bt", item_group := "bt"]
or_subset <- or_vitals[!is.na(item_group) & !is.na(chart_time)]

# 为每台手术显式构建窗口起点
ops_win <- copy(ops)
ops_win[, ward_window_start := fifelse(
  is.na(admission_time),
  orin_time - WARD_WINDOW_MIN,
  pmax(admission_time, orin_time - WARD_WINDOW_MIN)
)]
ops_win[, or_window_start := orin_time - OR_WINDOW_MIN]

# ==============================================================================
# 4. Ward 24h 基线
# ==============================================================================
cat("计算 Ward 基线 (窗口: max(admission, orin-24h) <= chart_time < orin_time)...\n")

ward_matched <- ward_subset[
  ops_win,
  on = .(
    subject_id,
    chart_time >= ward_window_start,
    chart_time < orin_time
  ),
  nomatch = NULL,
  .(op_id = i.op_id, item_name = x.item_name, value = x.value)
]

ward_agg <- ward_matched[, .(val_mean = mean(value, na.rm = TRUE)), by = .(op_id, item_name)]
ward_base <- dcast(ward_agg, op_id ~ item_name, value.var = "val_mean")
setnames(
  ward_base,
  old = c("nibp_sbp", "nibp_dbp", "nibp_mbp", "hr", "spo2", "rr", "bt"),
  new = c("ward_sbp", "ward_dbp", "ward_mbp", "ward_hr", "ward_spo2", "ward_rr", "ward_bt"),
  skip_absent = TRUE
)

# ==============================================================================
# 5. OR 120min 基线
# ==============================================================================
cat("计算 OR 基线 (窗口: orin-120 <= chart_time < orin_time)...\n")

or_matched <- or_subset[
  ops_win,
  on = .(
    op_id,
    chart_time >= or_window_start,
    chart_time < orin_time
  ),
  nomatch = NULL,
  .(op_id = i.op_id, item_group = x.item_group, value = x.value)
]

or_agg <- or_matched[, .(val_mean = mean(value, na.rm = TRUE)), by = .(op_id, item_group)]
or_base <- dcast(or_agg, op_id ~ item_group, value.var = "val_mean")
setnames(or_base, old = names(or_base)[-1], new = paste0("or_", names(or_base)[-1]))

# ==============================================================================
# 6. 合并并执行优先规则 (Ward 优先, OR 兜底)
# ==============================================================================
cat("合并并应用优先规则: Ward 优先, OR 兜底...\n")

final_dt <- merge(ops[, .(subject_id, hadm_id, op_id)], ward_base, by = "op_id", all.x = TRUE)
final_dt <- merge(final_dt, or_base, by = "op_id", all.x = TRUE)
setDT(final_dt)

vitals_short <- c("sbp", "dbp", "mbp", "hr", "spo2", "rr", "bt")

for (v in vitals_short) {
  ward_col <- paste0("ward_", v)
  or_col <- paste0("or_", v)
  preop_col <- paste0("preop_", v)
  source_col <- paste0("source_", v)

  final_dt[, (preop_col) := round(fcoalesce(get(ward_col), get(or_col)), 1)]
  final_dt[, (source_col) := fcase(
    !is.na(get(ward_col)), "Ward",
    !is.na(get(or_col)), "OR_Induction",
    default = "Missing"
  )]
}

setorder(final_dt, subject_id, hadm_id, op_id)

# ==============================================================================
# 7. 保存主输出
# ==============================================================================
output_file <- file.path(processed_path, "preop_baseline_final.csv")
cat(sprintf("保存结果至: %s\n", output_file))

cols_to_keep <- c(
  "subject_id", "hadm_id", "op_id",
  "preop_sbp", "preop_dbp", "preop_mbp",
  "preop_hr", "preop_spo2", "preop_rr", "preop_bt",
  "source_sbp", "source_dbp", "source_mbp",
  "source_hr", "source_spo2", "source_rr", "source_bt"
)
fwrite(final_dt[, ..cols_to_keep], output_file)

cat("主表已保存。\n")

# ==============================================================================
# 8. 统计描述与覆盖度
# ==============================================================================
cat("生成术前体征统计表...\n")

vital_vars <- c("preop_sbp", "preop_dbp", "preop_mbp", "preop_hr", "preop_spo2", "preop_rr", "preop_bt")
total_ops <- nrow(final_dt)

long_dt <- melt(
  final_dt,
  id.vars = "op_id",
  measure.vars = vital_vars,
  variable.name = "vital_sign",
  value.name = "value"
)

summary_stats <- long_dt[!is.na(value), .(
  N_Present = .N,
  Coverage_Pct = (.N / total_ops) * 100,
  Mean = mean(value),
  SD = sd(value),
  Median = median(value),
  Q1 = quantile(value, 0.25),
  Q3 = quantile(value, 0.75),
  Min = min(value),
  Max = max(value)
), by = vital_sign]

summary_stats[, `:=`(
  Coverage_Pct = round(Coverage_Pct, 2),
  Mean = round(Mean, 1),
  SD = round(SD, 1),
  Median = round(Median, 1),
  Q1 = round(Q1, 1),
  Q3 = round(Q3, 1),
  Min = round(Min, 1),
  Max = round(Max, 1),
  `Mean (SD)` = paste0(round(Mean, 1), " (", round(SD, 1), ")"),
  `Median [IQR]` = paste0(round(Median, 1), " [", round(Q1, 1), ", ", round(Q3, 1), "]")
)]

name_map <- c(
  preop_sbp = "Systolic BP (mmHg)",
  preop_dbp = "Diastolic BP (mmHg)",
  preop_mbp = "Mean BP (mmHg)",
  preop_hr = "Heart Rate (bpm)",
  preop_spo2 = "SpO2 (%)",
  preop_rr = "Resp Rate (bpm)",
  preop_bt = "Body Temp (C)"
)
summary_stats[, vital_sign := name_map[as.character(vital_sign)]]

stats_file <- file.path(processed_path, "preop_vitals_summary_coverage.csv")
fwrite(summary_stats, stats_file)
cat(sprintf("统计表已保存至: %s\n", stats_file))

# ==============================================================================
# 9. 新增来源覆盖 QC 表
# ==============================================================================
cat("生成来源覆盖 QC 表 (Ward 优先, OR 兜底)...\n")

source_qc <- rbindlist(lapply(vitals_short, function(v) {
  src <- final_dt[[paste0("source_", v)]]
  data.table(
    vital = paste0("preop_", v),
    n_total = total_ops,
    n_from_ward = sum(src == "Ward", na.rm = TRUE),
    n_from_or = sum(src == "OR_Induction", na.rm = TRUE),
    n_missing = sum(src == "Missing", na.rm = TRUE)
  )
}))

source_qc[, `:=`(
  pct_from_ward = round(100 * n_from_ward / n_total, 2),
  pct_from_or = round(100 * n_from_or / n_total, 2),
  pct_missing = round(100 * n_missing / n_total, 2),
  ward_window = "max(admission_time, orin_time-1440) <= chart_time < orin_time",
  or_window = "orin_time-120 <= chart_time < orin_time",
  selection_rule = "Ward_first_then_OR_fallback"
)]

source_qc_file <- file.path(processed_path, "preop_vitals_source_coverage.csv")
fwrite(source_qc, source_qc_file)
cat(sprintf("来源覆盖表已保存至: %s\n", source_qc_file))

cat("全部完成！\n")
