#!/usr/bin/env Rscript
# Timestamp: 2026-05-17T15:05:00Z
# Purpose: Convert extracted all-op intraoperative vitals from long to observed-time wide format.
# This is reshape only: no outlier cleaning, no calibration, no imputation, no regular time grid.

suppressPackageStartupMessages({
  library(data.table)
})

base_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed"
in_long <- file.path(base_dir, "intraop_vitals_clean_before_impute_no_calibration_all_op_extracted.csv")
window_qc <- file.path(base_dir, "intraop_vitals_extract_window_qc_20260517.csv")
out_wide <- file.path(base_dir, "intraop_vitals_wide_observed_before_impute_no_calibration_all_ops.csv")
out_duplicate_qc <- file.path(base_dir, "intraop_vitals_wide_observed_duplicate_qc_20260517.csv")

median_fun <- function(x) median(x, na.rm = TRUE)

cat("Reading extraction window QC ...\n")
win <- fread(
  window_qc,
  select = c("op_id", "vital_extract_start_time", "vital_extract_end_time", "time_qc_flag")
)
win[, `:=`(
  vital_extract_start_time = as.numeric(vital_extract_start_time),
  vital_extract_end_time = as.numeric(vital_extract_end_time)
)]

cat("Reading long intraoperative vitals ...\n")
dt <- fread(in_long, na.strings = c("", "NA", "NULL", "(Null)", "null"))
dt[, `:=`(
  chart_time = as.numeric(chart_time),
  value = as.numeric(value)
)]
dt <- dt[!is.na(op_id) & !is.na(chart_time) & !is.na(item_name) & !is.na(value)]

cat("Adding relative time from extraction-window start ...\n")
dt <- merge(dt, win[, .(op_id, vital_extract_start_time)], by = "op_id", all.x = TRUE)
dt[, min_from_entry := chart_time - vital_extract_start_time]
dt <- dt[!is.na(min_from_entry)]

cat("Skipping full duplicate QC in the main wide build; duplicate values are still resolved by median during dcast.\n")
cat("Casting observed-time long table to wide format using median for duplicates ...\n")
wide <- dcast(
  dt,
  subject_id + hadm_id + op_id + case_id + chart_time + min_from_entry ~ item_name,
  value.var = "value",
  fun.aggregate = median_fun
)
setorder(wide, subject_id, hadm_id, op_id, chart_time)

cat("Writing observed-time wide table ...\n")
fwrite(wide, out_wide)

cat("Done.\n")
cat("Output: ", out_wide, "\n", sep = "")
cat("Rows: ", nrow(wide), ", Cols: ", ncol(wide), "\n", sep = "")
cat("Unique op_id: ", uniqueN(wide$op_id), "\n", sep = "")
