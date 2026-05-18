#!/usr/bin/env Rscript
# Timestamp: 2026-05-17T13:10:00Z
# Purpose: Extract all-operation intraoperative raw vitals in long format.
# Output is intentionally raw/extracted only: no calibration, no outlier cleaning,
# no imputation, and no wide-format reshaping.

suppressPackageStartupMessages({
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

raw_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/01_raw"
out_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517"
ops_file <- file.path(raw_dir, "operations.csv")
vitals_file <- file.path(raw_dir, "vitals.csv")

out_file <- file.path(out_dir, "intraop_vitals_clean_before_impute_no_calibration_all_op_extracted.csv")
window_qc_file <- file.path(out_dir, "intraop_vitals_extract_window_qc_20260517.csv")
item_summary_file <- file.path(out_dir, "intraop_vitals_long_item_summary_20260517.csv")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Same item set used by the previous INSPIRE intraoperative vitals scripts.
target_items <- c(
  "hr", "rr", "spo2", "etco2", "bt",
  "nibp_sbp", "nibp_dbp", "nibp_mbp", "art_sbp", "art_dbp", "art_mbp",
  "fio2", "vt", "minvol", "pip", "peep", "pplat", "pmean", "etgas", "cpat", "o2", "air", "n2o",
  "cvp", "pap_sbp", "pap_dbp", "pap_mbp", "ci", "svi", "bis", "cbro2",
  "stii", "stiii", "sti", "stv5",
  "etsevo", "etdes", "etiso",
  "eph", "phe", "pepi", "nepi", "epi", "epii", "dopai", "dobui", "ntgi", "mlni", "vaso",
  "ppf", "ppfi", "rfti", "ftn", "sft", "aft", "mdz",
  "ns", "hs", "psa", "hns", "hes", "d5w", "d10w", "d50w", "alb5", "alb20",
  "rbc", "ffp", "pc", "pheresis", "cryo",
  "ebl", "uo", "ds"
)

time_cols_for_match <- c("orin_time", "orout_time", "anstart_time", "anend_time")
match_tolerance_min <- 30
or_op_excess_threshold_min <- 120
post_end_buffer_min <- 30
max_anesthesia_duration_min <- 24 * 60

num <- function(x) as.numeric(x)

min_dist_to_other_times <- function(t, same_ops) {
  if (is.na(t) || nrow(same_ops) == 0L) {
    return(rep(NA_real_, nrow(same_ops)))
  }
  dist <- rep(Inf, nrow(same_ops))
  for (cc in time_cols_for_match) {
    dist <- pmin(dist, abs(same_ops[[cc]] - t), na.rm = TRUE)
  }
  dist[is.infinite(dist)] <- NA_real_
  dist
}

cat("Reading operations.csv ...\n")
ops <- fread(
  ops_file,
  select = c(
    "op_id", "subject_id", "hadm_id", "case_id", "orin_time", "orout_time",
    "opstart_time", "opend_time", "anstart_time", "anend_time"
  ),
  na.strings = c("", "NA", "NULL", "(Null)", "null")
)

for (cc in c("orin_time", "orout_time", "opstart_time", "opend_time", "anstart_time", "anend_time")) {
  ops[, (cc) := num(get(cc))]
}

ops[, `:=`(
  or_duration = orout_time - orin_time,
  op_duration = opend_time - opstart_time,
  an_duration = anend_time - anstart_time,
  anstart_minus_orin = anstart_time - orin_time,
  anend_minus_orout = anend_time - orout_time
)]

ops[, valid_operation_time := !is.na(opstart_time) & !is.na(opend_time) &
      !is.na(orin_time) & !is.na(orout_time) &
      opend_time >= opstart_time & opstart_time >= orin_time & opend_time <= orout_time &
      op_duration > 0 & op_duration <= max_anesthesia_duration_min]

ops[, valid_anesthesia_time := !is.na(anstart_time) & !is.na(anend_time)]
ops[is.na(an_duration) | an_duration < 0 | an_duration > max_anesthesia_duration_min,
    valid_anesthesia_time := FALSE]

cat("Detecting anesthesia times that match another operation in the same admission ...\n")
ops[, `:=`(
  anesthesia_matches_other_op = FALSE,
  anesthesia_matched_other_op_id = as.numeric(NA),
  anesthesia_match_start_dist_min = as.numeric(NA),
  anesthesia_match_end_dist_min = as.numeric(NA)
)]

candidate_idx <- ops[
  !is.na(anstart_time) & !is.na(anend_time) &
    (
      abs(anstart_minus_orin) > or_op_excess_threshold_min |
        abs(anend_minus_orout) > or_op_excess_threshold_min |
        an_duration < 0 |
        an_duration > max_anesthesia_duration_min
    ),
  which = TRUE
]

for (ii in candidate_idx) {
  row <- ops[ii]
  same_ops <- ops[subject_id == row$subject_id & hadm_id == row$hadm_id & op_id != row$op_id]
  if (nrow(same_ops) == 0L) next

  start_dist <- min_dist_to_other_times(row$anstart_time, same_ops)
  end_dist <- min_dist_to_other_times(row$anend_time, same_ops)
  match_dt <- copy(same_ops)
  match_dt[, `:=`(start_dist = start_dist, end_dist = end_dist)]

  both_match <- match_dt[!is.na(start_dist) & !is.na(end_dist) &
                           start_dist <= match_tolerance_min & end_dist <= match_tolerance_min]
  start_only_match <- match_dt[abs(row$anstart_minus_orin) > or_op_excess_threshold_min &
                                 !is.na(start_dist) & start_dist <= match_tolerance_min]
  end_only_match <- match_dt[abs(row$anend_minus_orout) > or_op_excess_threshold_min &
                               !is.na(end_dist) & end_dist <= match_tolerance_min]

  if (nrow(both_match) > 0L) {
    both_match[, `:=`(match_max_dist = pmax(start_dist, end_dist), match_sum_dist = start_dist + end_dist)]
    setorder(both_match, match_max_dist, match_sum_dist)
    best <- both_match[1]
  } else if (nrow(start_only_match) > 0L) {
    setorder(start_only_match, start_dist)
    best <- start_only_match[1]
  } else if (nrow(end_only_match) > 0L) {
    setorder(end_only_match, end_dist)
    best <- end_only_match[1]
  } else {
    next
  }

  ops[ii, `:=`(
    anesthesia_matches_other_op = TRUE,
    anesthesia_matched_other_op_id = best$op_id,
    anesthesia_match_start_dist_min = best$start_dist,
    anesthesia_match_end_dist_min = best$end_dist,
    valid_anesthesia_time = FALSE
  )]
}

ops[, `:=`(
  vital_extract_start_time = orin_time,
  vital_extract_end_time = orout_time,
  original_orout_time = orout_time
)]

ops[, or_window_much_longer_than_op := valid_operation_time &
      !is.na(or_duration) & !is.na(op_duration) &
      (or_duration - op_duration > or_op_excess_threshold_min)]

ops[or_window_much_longer_than_op == TRUE,
    vital_extract_end_time := fifelse(
      valid_anesthesia_time == TRUE,
      pmin(orout_time, pmax(opend_time, anend_time, na.rm = TRUE) + post_end_buffer_min),
      pmin(orout_time, opend_time + post_end_buffer_min)
    )]

ops[, time_qc_flag := "use_or_window"]
ops[!valid_anesthesia_time & anesthesia_matches_other_op == TRUE,
    time_qc_flag := "use_or_window_anesthesia_matches_other_op"]
ops[!valid_anesthesia_time & !anesthesia_matches_other_op & !is.na(an_duration) & an_duration < 0,
    time_qc_flag := "use_or_window_invalid_anesthesia_negative_duration"]
ops[!valid_anesthesia_time & !anesthesia_matches_other_op & !is.na(an_duration) & an_duration > max_anesthesia_duration_min,
    time_qc_flag := "use_or_window_invalid_anesthesia_duration_gt_24h"]
ops[or_window_much_longer_than_op == TRUE,
    time_qc_flag := "corrected_orout_by_op_an_end_plus_30"]
ops[
  time_qc_flag == "use_or_window" &
    (
      abs(anstart_minus_orin) > or_op_excess_threshold_min |
        abs(anend_minus_orout) > or_op_excess_threshold_min
    ),
  time_qc_flag := "moderate_anesthesia_or_offset_keep_or_window"
]

ops[, extract_window_valid := !is.na(vital_extract_start_time) & !is.na(vital_extract_end_time) &
      vital_extract_end_time >= vital_extract_start_time]

window_qc <- ops[, .(
  op_id, subject_id, hadm_id, case_id,
  orin_time, orout_time, opstart_time, opend_time, anstart_time, anend_time,
  vital_extract_start_time, vital_extract_end_time, original_orout_time,
  or_duration, op_duration, an_duration,
  anstart_minus_orin, anend_minus_orout,
  valid_operation_time, valid_anesthesia_time, anesthesia_matches_other_op,
  anesthesia_matched_other_op_id, anesthesia_match_start_dist_min, anesthesia_match_end_dist_min,
  or_window_much_longer_than_op, extract_window_valid, time_qc_flag
)]
setorder(window_qc, subject_id, hadm_id, op_id)
fwrite(window_qc, window_qc_file)

cat("Window QC written: ", window_qc_file, "\n", sep = "")
cat("Window QC flag counts:\n")
print(window_qc[, .N, by = time_qc_flag][order(-N)])

if (dry_run) {
  cat("Dry run requested; skipping vitals.csv extraction.\n")
  quit(status = 0L)
}

cat("Reading vitals.csv ...\n")
vitals <- fread(
  vitals_file,
  select = c("op_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA", "NULL", "(Null)", "null")
)

cat("Filtering target items and numeric values ...\n")
vitals <- vitals[item_name %in% target_items]
vitals[, `:=`(chart_time = num(chart_time), value = num(value))]
vitals <- vitals[!is.na(chart_time) & !is.na(value)]

cat("Applying operation-specific extraction windows ...\n")
window_for_join <- ops[extract_window_valid == TRUE,
                       .(op_id, subject_id, hadm_id, case_id,
                         vital_extract_start_time, vital_extract_end_time)]

vitals_intraop <- merge(vitals, window_for_join, by = "op_id", all.x = FALSE, all.y = FALSE)
vitals_intraop <- vitals_intraop[
  chart_time >= vital_extract_start_time & chart_time <= vital_extract_end_time,
  .(op_id, subject_id, hadm_id, case_id, chart_time, item_name, value)
]
setcolorder(vitals_intraop, c("op_id", "subject_id", "hadm_id", "case_id", "chart_time", "item_name", "value"))
setorder(vitals_intraop, op_id, chart_time, item_name)

cat("Writing long-format intraoperative vitals ...\n")
fwrite(vitals_intraop, out_file)

item_summary <- vitals_intraop[, .(
  n_rows = .N,
  n_ops = uniqueN(op_id),
  n_subjects = uniqueN(subject_id),
  min_chart_time = min(chart_time, na.rm = TRUE),
  max_chart_time = max(chart_time, na.rm = TRUE)
), by = item_name]
setorder(item_summary, item_name)
fwrite(item_summary, item_summary_file)

cat("Done.\n")
cat("Output: ", out_file, "\n", sep = "")
cat("Rows: ", nrow(vitals_intraop), ", Cols: ", ncol(vitals_intraop), "\n", sep = "")
cat("Unique op_id: ", uniqueN(vitals_intraop$op_id), "\n", sep = "")
cat("Item summary: ", item_summary_file, "\n", sep = "")
