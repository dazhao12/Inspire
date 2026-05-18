#!/usr/bin/env Rscript
# Timestamp: 2026-05-17T15:05:00Z
# Purpose: Convert extracted all-op intraoperative vitals to a complete 5-minute relative-time grid.
# This is reshape/resampling only: no outlier cleaning, no calibration, and no imputation.

suppressPackageStartupMessages({
  library(data.table)
})

base_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed"
in_long <- file.path(base_dir, "intraop_vitals_clean_before_impute_no_calibration_all_op_extracted.csv")
window_qc <- file.path(base_dir, "intraop_vitals_extract_window_qc_20260517.csv")
out_wide <- file.path(base_dir, "intraop_vitals_wide_5min_grid_before_impute_no_calibration_all_ops.csv")
out_summary <- file.path(base_dir, "intraop_vitals_wide_5min_grid_summary_20260517.csv")

grid_step_min <- 5
median_fun <- function(x) median(x, na.rm = TRUE)

cat("Reading extraction window QC ...\n")
win <- fread(
  window_qc,
  select = c(
    "op_id", "subject_id", "hadm_id", "case_id", "vital_extract_start_time",
    "vital_extract_end_time", "extract_window_valid", "time_qc_flag"
  )
)
win[, `:=`(
  vital_extract_start_time = as.numeric(vital_extract_start_time),
  vital_extract_end_time = as.numeric(vital_extract_end_time)
)]
win <- win[extract_window_valid == TRUE]
win[, window_duration_min := vital_extract_end_time - vital_extract_start_time]
win <- win[!is.na(window_duration_min) & window_duration_min >= 0]
win[, max_grid_min_from_entry := floor(window_duration_min / grid_step_min) * grid_step_min]

cat("Building complete 5-minute operation grid ...\n")
grid <- win[, .(
  grid_min_from_entry = seq(0, max_grid_min_from_entry, by = grid_step_min)
), by = .(subject_id, hadm_id, op_id, case_id, vital_extract_start_time, vital_extract_end_time, time_qc_flag)]
grid[, chart_time := vital_extract_start_time + grid_min_from_entry]

cat("Reading long intraoperative vitals ...\n")
dt <- fread(in_long, na.strings = c("", "NA", "NULL", "(Null)", "null"))
dt[, `:=`(
  chart_time = as.numeric(chart_time),
  value = as.numeric(value)
)]
dt <- dt[!is.na(op_id) & !is.na(chart_time) & !is.na(item_name) & !is.na(value)]

dt <- merge(
  dt,
  win[, .(op_id, vital_extract_start_time, max_grid_min_from_entry)],
  by = "op_id",
  all.x = FALSE,
  all.y = FALSE
)
dt[, min_from_entry := chart_time - vital_extract_start_time]
dt <- dt[!is.na(min_from_entry) & min_from_entry >= 0]
dt[, grid_min_from_entry := round(min_from_entry / grid_step_min) * grid_step_min]
dt[grid_min_from_entry < 0, grid_min_from_entry := 0]
dt[grid_min_from_entry > max_grid_min_from_entry, grid_min_from_entry := max_grid_min_from_entry]

cat("Aggregating observed values into nearest 5-minute bins using median ...\n")
agg <- dt[, .(value = median_fun(value)), by = .(op_id, grid_min_from_entry, item_name)]
rm(dt); gc()

cat("Casting aggregated 5-minute bins to wide format ...\n")
wide_obs <- dcast(
  agg,
  op_id + grid_min_from_entry ~ item_name,
  value.var = "value",
  fun.aggregate = median_fun
)
rm(agg); gc()

cat("Merging observed bins onto complete operation grid ...\n")
wide <- merge(grid, wide_obs, by = c("op_id", "grid_min_from_entry"), all.x = TRUE, sort = FALSE)
rm(grid, wide_obs); gc()

id_cols <- c(
  "subject_id", "hadm_id", "op_id", "case_id", "chart_time", "grid_min_from_entry",
  "vital_extract_start_time", "vital_extract_end_time", "time_qc_flag"
)
setcolorder(wide, c(id_cols, setdiff(names(wide), id_cols)))
setorder(wide, subject_id, hadm_id, op_id, grid_min_from_entry)

cat("Writing complete 5-minute grid wide table ...\n")
fwrite(wide, out_wide)

value_cols <- setdiff(names(wide), id_cols)
summary <- data.table(
  metric = c(
    "grid_step_min", "n_rows", "n_ops", "n_value_columns", "n_rows_with_any_observed_value",
    "median_grid_rows_per_op", "p25_grid_rows_per_op", "p75_grid_rows_per_op"
  ),
  value = c(
    grid_step_min,
    nrow(wide),
    uniqueN(wide$op_id),
    length(value_cols),
    sum(rowSums(!is.na(wide[, ..value_cols])) > 0L),
    median(wide[, .N, by = op_id]$N),
    quantile(wide[, .N, by = op_id]$N, 0.25, names = FALSE),
    quantile(wide[, .N, by = op_id]$N, 0.75, names = FALSE)
  )
)
fwrite(summary, out_summary)

cat("Done.\n")
cat("Output: ", out_wide, "\n", sep = "")
cat("Rows: ", nrow(wide), ", Cols: ", ncol(wide), "\n", sep = "")
cat("Unique op_id: ", uniqueN(wide$op_id), "\n", sep = "")
cat("Summary: ", out_summary, "\n", sep = "")
