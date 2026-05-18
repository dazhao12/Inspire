#!/usr/bin/env Rscript
# Timestamp: 2026-05-17T11:05:00Z

suppressPackageStartupMessages({
  library(data.table)
})

raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/01_raw/operations.csv"
out_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517/anesthesia_timeline_all_ops_latest.csv"

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

cat("Reading operations.csv ...\n")
ops <- fread(
  raw_path,
  select = c(
    "subject_id", "hadm_id", "op_id", "case_id", "opdate",
    "department", "antype", "admission_time", "orin_time",
    "orout_time", "discharge_time"
  ),
  na.strings = c("", "NA")
)

cat("Building timeline extract (all operations, no first non-MAC filter) ...\n")
ops[, postop7_end := orout_time + 7 * 24 * 60]
ops[, postop30_start := orout_time]
ops[, postop30_end := orout_time + 30 * 24 * 60]

out <- ops[, .(
  subject_id, hadm_id, op_id, case_id, opdate, department, antype,
  admission_time, orin_time, orout_time, discharge_time,
  postop7_end, postop30_start, postop30_end
)]
setorder(out, subject_id, hadm_id, op_id)

fwrite(out, out_path)

cat("Done.\n")
cat("Output: ", out_path, "\n", sep = "")
cat("Rows: ", nrow(out), ", Cols: ", ncol(out), "\n", sep = "")
