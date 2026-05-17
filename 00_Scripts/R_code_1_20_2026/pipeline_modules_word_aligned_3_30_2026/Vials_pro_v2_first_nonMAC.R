suppressPackageStartupMessages({
  library(data.table)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
} else {
  getwd()
}
source(file.path(script_dir, "anchor_first_nonmac_utils.R"))

raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_pro_first_nonMAC_3_30_2026"
if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)

WARD_WINDOW_MIN <- 1440
OR_WINDOW_MIN <- 120

cat("Loading first non-MAC anchor operations and vital tables ...\n")
ops <- load_first_nonmac_anchor_ops(
  raw_path = raw_path,
  extra_cols = c("admission_time", "orin_time")
)
write_anchor_map(ops, processed_path)
ops <- unique(ops[, .(subject_id, hadm_id, op_id, admission_time, orin_time)])

ward_vitals <- fread(
  file.path(raw_path, "ward_vitals.csv"),
  select = c("subject_id", "chart_time", "item_name", "value")
)
or_vitals <- fread(
  file.path(raw_path, "vitals.csv"),
  select = c("op_id", "chart_time", "item_name", "value")
)

ops[, `:=`(
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time)
)]
ward_vitals[, `:=`(
  chart_time = as.numeric(chart_time),
  value = as.numeric(value)
)]
or_vitals[, `:=`(
  chart_time = as.numeric(chart_time),
  value = as.numeric(value)
)]

ward_items <- c("nibp_sbp", "nibp_dbp", "nibp_mbp", "hr", "spo2", "rr", "bt")
ward_subset <- ward_vitals[subject_id %in% ops$subject_id & item_name %in% ward_items & !is.na(chart_time)]

or_vitals[item_name %in% c("nibp_sbp", "art_sbp"), item_group := "sbp"]
or_vitals[item_name %in% c("nibp_dbp", "art_dbp"), item_group := "dbp"]
or_vitals[item_name %in% c("nibp_mbp", "art_mbp"), item_group := "mbp"]
or_vitals[item_name == "hr", item_group := "hr"]
or_vitals[item_name == "spo2", item_group := "spo2"]
or_vitals[item_name == "rr", item_group := "rr"]
or_vitals[item_name == "bt", item_group := "bt"]
or_subset <- or_vitals[op_id %in% ops$op_id & !is.na(item_group) & !is.na(chart_time)]

ops_win <- copy(ops)
ops_win[, ward_window_start := fifelse(
  is.na(admission_time),
  orin_time - WARD_WINDOW_MIN,
  pmax(admission_time, orin_time - WARD_WINDOW_MIN)
)]
ops_win[, or_window_start := orin_time - OR_WINDOW_MIN]

ward_matched <- ward_subset[
  ops_win,
  on = .(subject_id, chart_time >= ward_window_start, chart_time < orin_time),
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

or_matched <- or_subset[
  ops_win,
  on = .(op_id, chart_time >= or_window_start, chart_time < orin_time),
  nomatch = NULL,
  .(op_id = i.op_id, item_group = x.item_group, value = x.value)
]
or_agg <- or_matched[, .(val_mean = mean(value, na.rm = TRUE)), by = .(op_id, item_group)]
or_base <- dcast(or_agg, op_id ~ item_group, value.var = "val_mean")
setnames(or_base, old = names(or_base)[-1], new = paste0("or_", names(or_base)[-1]))

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
cols_to_keep <- c(
  "subject_id", "hadm_id", "op_id",
  "preop_sbp", "preop_dbp", "preop_mbp",
  "preop_hr", "preop_spo2", "preop_rr", "preop_bt",
  "source_sbp", "source_dbp", "source_mbp",
  "source_hr", "source_spo2", "source_rr", "source_bt"
)
fwrite(final_dt[, ..cols_to_keep], file.path(processed_path, "preop_baseline_final.csv"))

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
fwrite(summary_stats, file.path(processed_path, "preop_vitals_summary_coverage.csv"))

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
  selection_rule = "Ward_first_then_OR_fallback",
  anchor_rule = "first_nonMAC_per_subject_id_hadm_id"
)]
fwrite(source_qc, file.path(processed_path, "preop_vitals_source_coverage.csv"))

cat("Done: Vials_pro_v2_first_nonMAC.R\n")
