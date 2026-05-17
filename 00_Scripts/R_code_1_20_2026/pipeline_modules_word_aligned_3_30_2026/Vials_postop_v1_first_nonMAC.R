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
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_postop_first72h_first_nonMAC_3_31_2026"
if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)

IMMEDIATE_ICU_THRESHOLD_MIN <- 20
FIRST24H_MIN <- 1440
FIRST72H_MIN <- 4320

target_items <- c("nibp_sbp", "nibp_dbp", "nibp_mbp", "hr", "spo2", "rr", "bt", "fio2")

calc_density <- function(x) {
  times <- sort(unique(x))
  if (length(times) == 0L) {
    return(list(
      n_timepoints = 0L,
      median_gap_min = NA_real_,
      min_gap_min = NA_real_,
      max_gap_min = NA_real_
    ))
  }
  if (length(times) == 1L) {
    return(list(
      n_timepoints = 1L,
      median_gap_min = NA_real_,
      min_gap_min = NA_real_,
      max_gap_min = NA_real_
    ))
  }
  gaps <- diff(times)
  list(
    n_timepoints = length(times),
    median_gap_min = median(gaps),
    min_gap_min = min(gaps),
    max_gap_min = max(gaps)
  )
}

cat("Loading first non-MAC anchor operations and postop ward vitals ...\n")
ops <- load_first_nonmac_anchor_ops(
  raw_path = raw_path,
  extra_cols = c("admission_time", "orin_time", "orout_time", "discharge_time", "icuin_time", "icuout_time")
)
write_anchor_map(ops, processed_path)
ops <- unique(ops[, .(
  op_id, subject_id, hadm_id, admission_time, orin_time, orout_time,
  discharge_time, icuin_time, icuout_time
)])
setorderv(ops, c("subject_id", "hadm_id", "orin_time", "op_id"))
ops[, surgery_number := rowid(subject_id)]
ops[, `:=`(
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time),
  orout_time = as.numeric(orout_time),
  discharge_time = as.numeric(discharge_time),
  icuin_time = as.numeric(icuin_time),
  icuout_time = as.numeric(icuout_time)
)]

max_ops_debug <- suppressWarnings(as.integer(Sys.getenv("MAX_OPS_FOR_DEBUG", "")))
if (!is.na(max_ops_debug) && max_ops_debug > 0L && nrow(ops) > max_ops_debug) {
  ops <- ops[seq_len(max_ops_debug)]
  cat(sprintf("Debug mode: restricting to first %d anchor operations.\n", max_ops_debug))
}

ward_vitals <- fread(
  file.path(raw_path, "ward_vitals.csv"),
  select = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA", "NULL", "(Null)", "null")
)
ward_vitals[, `:=`(
  chart_time = as.numeric(chart_time),
  value = as.numeric(value)
)]
ward_vitals <- ward_vitals[
  subject_id %in% ops$subject_id &
    item_name %in% target_items &
    !is.na(chart_time) &
    !is.na(value)
]

ops_window <- ops[!is.na(orout_time) & !is.na(discharge_time) & discharge_time > orout_time]
ops_window[, `:=`(
  postop_start = orout_time,
  postop_end = pmin(discharge_time, orout_time + FIRST72H_MIN)
)]

postop_long <- ward_vitals[
  ops_window,
  on = .(subject_id, chart_time > postop_start, chart_time <= postop_end),
  nomatch = NULL,
  allow.cartesian = TRUE,
  .(
    subject_id = i.subject_id,
    hadm_id = i.hadm_id,
    op_id = i.op_id,
    surgery_number = i.surgery_number,
    chart_time = x.chart_time,
    item_name = x.item_name,
    value = x.value,
    orin_time = i.orin_time,
    orout_time = i.orout_time,
    discharge_time = i.discharge_time,
    icuin_time = i.icuin_time,
    icuout_time = i.icuout_time
  )
]

postop_long[, `:=`(
  min_from_orout = chart_time - orout_time,
  min_from_orin = chart_time - orin_time,
  care_location = fcase(
    !is.na(icuin_time) & !is.na(icuout_time) & chart_time >= icuin_time & chart_time <= icuout_time, "ICU",
    default = "Ward_or_nonICU"
  ),
  postop_period = fcase(
    chart_time - orout_time <= FIRST24H_MIN, "first24h",
    default = "24to72h"
  )
)]
cat(sprintf("Matched %s postop rows within first 72h.\n", format(nrow(postop_long), big.mark = ",")))
setorder(postop_long, subject_id, hadm_id, surgery_number, op_id, chart_time, item_name)
fwrite(postop_long, file.path(processed_path, "postop_vitals_long_first72h.csv"))

postop_wide <- dcast(
  postop_long,
  subject_id + hadm_id + op_id + surgery_number + chart_time + min_from_orout + care_location + postop_period ~ item_name,
  value.var = "value",
  fun.aggregate = mean,
  na.rm = TRUE
)
cat(sprintf("Built %s postop timepoints in wide format.\n", format(nrow(postop_wide), big.mark = ",")))
setorder(postop_wide, subject_id, hadm_id, surgery_number, op_id, chart_time)
fwrite(postop_wide, file.path(processed_path, "postop_vitals_wide_first72h.csv"))

postop_first24 <- postop_long[min_from_orout <= FIRST24H_MIN]
postop_72h <- postop_long[min_from_orout <= FIRST72H_MIN]

build_summary <- function(dt, label) {
  if (nrow(dt) == 0L) {
    return(data.table())
  }

  out <- dt[order(op_id, item_name, chart_time), .(
    n_records = .N,
    n_timepoints = uniqueN(chart_time),
    first_value = value[1],
    last_value = value[.N],
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE)
  ), by = .(subject_id, hadm_id, op_id, surgery_number, item_name)]

  wide <- dcast(out, subject_id + hadm_id + op_id + surgery_number ~ item_name, value.var = c(
    "n_records", "n_timepoints", "first_value", "last_value",
    "mean_value", "median_value", "min_value", "max_value", "sd_value"
  ))
  fwrite(wide, file.path(processed_path, sprintf("postop_vitals_summary_%s.csv", label)))
  invisible(wide)
}

build_summary(postop_first24, "first24h")
build_summary(postop_72h, "first72h")

density_24h <- unique(
  postop_first24[, .(subject_id, hadm_id, op_id, surgery_number, care_location, chart_time)]
)[order(op_id, care_location, chart_time), {
  density <- calc_density(chart_time)
  as.data.table(density)
}, by = .(subject_id, hadm_id, op_id, surgery_number, care_location)]

density_any_24h <- unique(
  postop_first24[, .(subject_id, hadm_id, op_id, surgery_number, chart_time)]
)[order(op_id, chart_time), {
  density <- calc_density(chart_time)
  as.data.table(density)
}, by = .(subject_id, hadm_id, op_id, surgery_number)]
setnames(
  density_any_24h,
  old = c("n_timepoints", "median_gap_min", "min_gap_min", "max_gap_min"),
  new = c("n_timepoints_any_24h", "median_gap_any_24h_min", "min_gap_any_24h_min", "max_gap_any_24h_min")
)

care_location_summary <- unique(ops[, .(
  subject_id, hadm_id, op_id, surgery_number, orout_time, discharge_time, icuin_time, icuout_time
)])
care_location_summary[, interval_between_orout_and_icuin_min := fifelse(
  !is.na(icuin_time) & !is.na(orout_time),
  icuin_time - orout_time,
  NA_real_
)]
care_location_summary[, icu_stay_after_surgery := as.integer(
  !is.na(icuout_time) & !is.na(orout_time) & icuout_time > orout_time
)]
care_location_summary[, postop_destination_by_timeline := fcase(
  !is.na(icuin_time) & !is.na(icuout_time) & icuout_time > orout_time &
    icuin_time >= orout_time & (icuin_time - orout_time) <= IMMEDIATE_ICU_THRESHOLD_MIN, "ICU_direct",
  !is.na(icuin_time) & !is.na(icuout_time) & icuout_time > orout_time &
    icuin_time > orout_time + IMMEDIATE_ICU_THRESHOLD_MIN, "Ward_then_ICU",
  !is.na(icuout_time) & !is.na(orout_time) & icuout_time > orout_time, "ICU_after_surgery_uncertain_entry",
  !is.na(discharge_time) & !is.na(orout_time) & discharge_time > orout_time, "Ward_only_or_nonICU",
  default = "Unknown"
)]
care_location_summary[, first_postop_location := fcase(
  postop_destination_by_timeline %chin% c("ICU_direct", "ICU_after_surgery_uncertain_entry"), "ICU",
  postop_destination_by_timeline %chin% c("Ward_then_ICU", "Ward_only_or_nonICU"), "Ward_or_PACU_first",
  default = "Unknown"
)]

care_location_summary <- merge(
  care_location_summary,
  density_any_24h,
  by = c("subject_id", "hadm_id", "op_id", "surgery_number"),
  all.x = TRUE
)
care_location_summary[, density_based_monitoring_24h := fcase(
  !is.na(median_gap_any_24h_min) & median_gap_any_24h_min <= 15, "ICU_like_dense",
  !is.na(n_timepoints_any_24h) & n_timepoints_any_24h >= 24, "ICU_like_dense",
  !is.na(n_timepoints_any_24h) & n_timepoints_any_24h <= 8, "Ward_like_sparse",
  !is.na(n_timepoints_any_24h), "Intermediate_density",
  default = "No_postop_vitals_first24h"
)]
care_location_summary[, final_postop_monitoring_class := fcase(
  first_postop_location == "ICU", "ICU",
  density_based_monitoring_24h == "ICU_like_dense", "ICU_like_no_timeline_flag",
  first_postop_location == "Ward_or_PACU_first", "Ward_or_PACU",
  default = "Unknown"
)]
setorder(care_location_summary, subject_id, hadm_id, surgery_number, op_id)
fwrite(care_location_summary, file.path(processed_path, "postop_care_location_summary.csv"))
fwrite(density_24h, file.path(processed_path, "postop_monitoring_density_by_location_first24h.csv"))

notes_dt <- data.table(
  note = c(
    "Postoperative vitals are extracted from ward_vitals.csv only because that raw source contains ward and ICU bedside measurements after surgery.",
    "Matching rule for postoperative vital extraction: subject_id plus postoperative time window where orout_time < chart_time <= min(discharge_time, orout_time + 4320).",
    "Per-record care_location is ICU when chart_time falls within icuin_time to icuout_time; otherwise Ward_or_nonICU.",
    "Patient-level postop_destination_by_timeline uses a 20-minute threshold after orout_time to label direct ICU transfer versus Ward_then_ICU.",
    "density_based_monitoring_24h is a secondary QC heuristic based on measurement frequency in the first 24 hours and should not replace icuin_time/icuout_time when those timestamps are available.",
    "ward_vitals.csv contains subject_id only, so linkage remains time-window based and may still carry cross-admission risk in multi-admission subjects."
  )
)
fwrite(notes_dt, file.path(processed_path, "postop_vitals_notes.csv"))

cat("Done: Vials_postop_v1_first_nonMAC.R\n")
