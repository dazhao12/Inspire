#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required for lab workbook output.")
}

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
} else {
  getwd()
}
source(file.path(script_dir, "anchor_first_nonmac_utils.R"))

base_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026"
if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)

window_day_to_min <- function(days) days * 24 * 60

target_items <- c(
  "glucose", "creatinine", "hct", "potassium", "sodium", "hb", "wbc",
  "platelet", "chloride", "lymphocyte", "seg", "bun", "calcium",
  "phosphorus", "albumin", "total_bilirubin", "alt", "ast",
  "total_protein", "alp", "crp", "sao2", "hco3", "ptinr", "ph",
  "pao2", "paco2", "aptt", "ica", "fibrinogen", "be", "lacate",
  "ckmb", "ck", "troponin_i", "hba1c", "troponin_t", "d_dimer"
)

lab_unit_map <- c(
  glucose = "mg_dL", creatinine = "mg_dL", hct = "pct", potassium = "mmol_L",
  sodium = "mmol_L", hb = "g_dL", wbc = "10e3_uL", platelet = "10e3_uL",
  chloride = "mmol_L", lymphocyte = "pct", seg = "pct", bun = "mg_dL",
  calcium = "mg_dL", phosphorus = "mg_dL", albumin = "g_dL",
  total_bilirubin = "mg_dL", alt = "U_L", ast = "U_L", total_protein = "g_dL",
  alp = "U_L", crp = "mg_L", sao2 = "pct", hco3 = "mmol_L", ptinr = "INR",
  ph = "unitless", pao2 = "mmHg", paco2 = "mmHg", aptt = "sec",
  ica = "mmol_L", fibrinogen = "mg_dL", be = "mmol_L", lacate = "mmol_L",
  ckmb = "unknown_unit", ck = "U_L", troponin_i = "ng_mL", hba1c = "pct",
  troponin_t = "ng_mL", d_dimer = "ug_mL_FEU"
)

append_unit_suffix <- function(dt_wide, prefix_name) {
  nm <- names(dt_wide)
  lab_cols <- grep(sprintf("^%s_.*_(nearest|median|mean)$", prefix_name), nm, value = TRUE)
  if (length(lab_cols) == 0L) return(copy(dt_wide))

  new_names <- nm
  pattern <- sprintf("^%s_(.*)_(nearest|median|mean)$", prefix_name)
  for (col in lab_cols) {
    parts <- regmatches(col, regexec(pattern, col))[[1]]
    if (length(parts) == 3) {
      lab_name <- parts[2]
      stat_name <- parts[3]
      unit <- if (!is.na(lab_unit_map[lab_name])) unname(lab_unit_map[lab_name]) else "unknown_unit"
      new_names[match(col, nm)] <- paste0(prefix_name, "_", lab_name, "_", stat_name, "_", unit)
    }
  }
  out <- copy(dt_wide)
  setnames(out, nm, new_names)
  out
}

calc_stats_wide <- function(anchor_dt, dt_window, prefix_name, nearest_mode = c("last", "first")) {
  nearest_mode <- match.arg(nearest_mode)
  if (nrow(dt_window) == 0L) {
    out <- copy(anchor_dt[, .(subject_id, hadm_id, op_id)])
    setorderv(out, c("subject_id", "hadm_id", "op_id"))
    return(out)
  }

  stats <- dt_window[, {
    nearest_idx <- if (nearest_mode == "last") which.max(chart_time) else which.min(chart_time)
    .(
      val_nearest = value[nearest_idx],
      val_median = median(value, na.rm = TRUE),
      val_mean = mean(value, na.rm = TRUE)
    )
  }, by = .(op_id, item_name)]

  for (v in c("val_median", "val_mean")) {
    set(stats, i = which(is.nan(stats[[v]])), j = v, value = NA_real_)
  }

  stats[, item_name := paste0(prefix_name, "_", item_name)]
  wide <- dcast(stats, op_id ~ item_name, value.var = c("val_nearest", "val_median", "val_mean"))

  old_names <- names(wide)
  new_names <- gsub("val_nearest_(.*)", "\\1_nearest", old_names)
  new_names <- gsub("val_median_(.*)", "\\1_median", new_names)
  new_names <- gsub("val_mean_(.*)", "\\1_mean", new_names)
  setnames(wide, old_names, new_names)

  out <- merge(anchor_dt[, .(subject_id, hadm_id, op_id)], wide, by = "op_id", all.x = TRUE)
  setorderv(out, c("subject_id", "hadm_id", "op_id"))
  out
}

coverage_one <- function(dt_wide, window_name, prefix_name) {
  nearest_cols <- grep(sprintf("^%s_.*_nearest$", prefix_name), names(dt_wide), value = TRUE)
  any_lab <- if (length(nearest_cols) == 0L) rep(FALSE, nrow(dt_wide)) else rowSums(!is.na(dt_wide[, ..nearest_cols])) > 0

  out <- data.table(
    window = window_name,
    n_ops = nrow(dt_wide),
    n_ops_any_lab = sum(any_lab),
    pct_ops_any_lab = round(mean(any_lab) * 100, 2)
  )

  key_cols <- intersect(
    c(
      paste0(prefix_name, "_creatinine_nearest"),
      paste0(prefix_name, "_hb_nearest"),
      paste0(prefix_name, "_wbc_nearest")
    ),
    names(dt_wide)
  )
  if (length(key_cols) > 0L) {
    for (col in key_cols) {
      metric_name <- sub(sprintf("^%s_", prefix_name), "", col)
      out[, (paste0("pct_", metric_name)) := round(mean(!is.na(dt_wide[[col]])) * 100, 2)]
    }
  }
  out
}

cat("Loading first non-MAC anchor operations ...\n")
ops <- load_first_nonmac_anchor_ops(
  raw_path = base_path,
  extra_cols = c("admission_time", "discharge_time", "orin_time", "orout_time")
)
write_anchor_map(ops, processed_path)

cat("Loading stay history from operations.csv ...\n")
stays <- fread(
  file.path(base_path, "operations.csv"),
  select = c("subject_id", "hadm_id", "admission_time", "discharge_time"),
  na.strings = c("", "NA")
)
stays[, `:=`(
  hadm_id = as.numeric(hadm_id),
  admission_time = as.numeric(admission_time),
  discharge_time = as.numeric(discharge_time)
)]
stays <- stays[!is.na(subject_id) & !is.na(hadm_id) & !is.na(admission_time) & !is.na(discharge_time)]
stays <- unique(
  stays[, .(
    admission_time = min(admission_time, na.rm = TRUE),
    discharge_time = max(discharge_time, na.rm = TRUE)
  ), by = .(subject_id, hadm_id)]
)
setorder(stays, subject_id, admission_time, discharge_time, hadm_id)
stays[, `:=`(
  previous_discharge_time = shift(discharge_time),
  next_admission_time = shift(admission_time, type = "lead")
), by = subject_id]
stays[, stay_n := .N, by = subject_id]

ops <- merge(
  ops,
  stays[, .(subject_id, hadm_id, previous_discharge_time, next_admission_time, stay_n)],
  by = c("subject_id", "hadm_id"),
  all.x = TRUE,
  sort = FALSE
)
ops[is.na(stay_n), stay_n := 1L]

cat("Loading labs.csv ...\n")
labs <- fread(
  file.path(base_path, "labs.csv"),
  select = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
labs[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]
labs <- labs[subject_id %in% ops$subject_id & item_name %chin% target_items & !is.na(chart_time) & !is.na(value)]

labs[, rec_id := .I]
stay_map <- unique(
  stays[, .(subject_id, hadm_id, admission_time, discharge_time)]
)
setkey(stay_map, subject_id, admission_time, discharge_time)

lab_stay_candidate <- stay_map[
  labs[, .(rec_id, subject_id, chart_time)],
  on = .(subject_id, admission_time <= chart_time, discharge_time >= chart_time),
  allow.cartesian = TRUE,
  nomatch = 0L
]
if (nrow(lab_stay_candidate) > 0L) {
  lab_stay_map <- lab_stay_candidate[
    order(rec_id, -admission_time, discharge_time)
  ][, .SD[1], by = rec_id][
    , .(rec_id, assigned_hadm_id = hadm_id)
  ]
} else {
  lab_stay_map <- data.table(rec_id = integer(), assigned_hadm_id = numeric())
}

labs <- merge(labs, lab_stay_map, by = "rec_id", all.x = TRUE)
labs[, rec_id := NULL]

cat("Joining labs to anchor operations ...\n")
setkey(ops, subject_id)
setkey(labs, subject_id)
joined <- merge(
  labs[, .(subject_id, chart_time, item_name, value, assigned_hadm_id)],
  ops[, .(
    subject_id, hadm_id, stay_n, op_id,
    admission_time, discharge_time, orin_time, orout_time,
    previous_discharge_time, next_admission_time
  )],
  by = "subject_id",
  allow.cartesian = TRUE
)

joined[, same_stay := !is.na(assigned_hadm_id) & !is.na(hadm_id) & assigned_hadm_id == hadm_id]
joined[, fallback_single_stay := is.na(assigned_hadm_id) & !is.na(stay_n) & stay_n == 1L]
joined[, in_current_stay := same_stay | fallback_single_stay]

pre_joined <- joined[!is.na(orin_time) & chart_time < orin_time]
post_joined <- joined[!is.na(orout_time) & chart_time >= orout_time]

pre_window_specs <- data.table(
  file_stub = c(
    "current_stay", "attributable_60d", "attributable_30d",
    "attributable_15d", "attributable_7d", "cumulative_preop"
  ),
  days = c(NA_real_, 60, 30, 15, 7, NA_real_),
  window = c(
    "current_stay_preop", "attributable_preop_60d", "attributable_preop_30d",
    "attributable_preop_15d", "attributable_preop_7d", "cumulative_preop"
  )
)

post_window_specs <- data.table(
  file_stub = c(
    "current_stay", "attributable_7d", "attributable_15d",
    "attributable_30d", "attributable_60d", "cumulative_postop"
  ),
  days = c(NA_real_, 7, 15, 30, 60, NA_real_),
  window = c(
    "current_stay_postop", "attributable_postop_7d", "attributable_postop_15d",
    "attributable_postop_30d", "attributable_postop_60d", "cumulative_postop"
  )
)

get_pre_window <- function(spec_row) {
  if (spec_row$file_stub == "current_stay") {
    return(pre_joined[
      !is.na(admission_time) &
        chart_time >= admission_time &
        in_current_stay
    ])
  }
  if (spec_row$file_stub == "cumulative_preop") {
    return(copy(pre_joined))
  }
  lower_bound <- ifelse(
    is.na(pre_joined$previous_discharge_time),
    pre_joined$admission_time - window_day_to_min(spec_row$days),
    pmax(pre_joined$previous_discharge_time, pre_joined$admission_time - window_day_to_min(spec_row$days))
  )
  pre_joined[!is.na(admission_time) & !is.na(lower_bound) & chart_time >= lower_bound]
}

get_post_window <- function(spec_row) {
  if (spec_row$file_stub == "current_stay") {
    return(post_joined[
      in_current_stay &
        !is.na(discharge_time) &
        chart_time <= discharge_time
    ])
  }
  if (spec_row$file_stub == "cumulative_postop") {
    return(copy(post_joined))
  }
  upper_bound <- ifelse(
    is.na(post_joined$next_admission_time),
    post_joined$orout_time + window_day_to_min(spec_row$days),
    pmin(post_joined$next_admission_time, post_joined$orout_time + window_day_to_min(spec_row$days))
  )
  post_joined[!is.na(upper_bound) & chart_time <= upper_bound]
}

cat("Building preop lab windows ...\n")
pre_summary <- rbindlist(lapply(seq_len(nrow(pre_window_specs)), function(i) {
  spec <- pre_window_specs[i]
  dt_window <- get_pre_window(spec)
  out <- calc_stats_wide(ops, dt_window, prefix_name = "preop", nearest_mode = "last")
  fwrite(out, file.path(processed_path, sprintf("preop_labs_features_%s.csv", spec$file_stub)))
  fwrite(append_unit_suffix(out, "preop"), file.path(processed_path, sprintf("preop_labs_features_%s_with_units.csv", spec$file_stub)))
  coverage_one(out, spec$window, "preop")
}), fill = TRUE)
fwrite(pre_summary, file.path(processed_path, "preop_labs_window_summary.csv"))

cat("Building postop lab windows ...\n")
post_summary <- rbindlist(lapply(seq_len(nrow(post_window_specs)), function(i) {
  spec <- post_window_specs[i]
  dt_window <- get_post_window(spec)
  out <- calc_stats_wide(ops, dt_window, prefix_name = "postop", nearest_mode = "first")
  fwrite(out, file.path(processed_path, sprintf("postop_labs_features_%s.csv", spec$file_stub)))
  fwrite(append_unit_suffix(out, "postop"), file.path(processed_path, sprintf("postop_labs_features_%s_with_units.csv", spec$file_stub)))
  coverage_one(out, spec$window, "postop")
}), fill = TRUE)
fwrite(post_summary, file.path(processed_path, "postop_labs_window_summary.csv"))

notes_dt <- data.table(
  note = c(
    "Anchor op is the first non-MAC surgery per subject_id + hadm_id.",
    "Preop current-stay window: admission_time <= chart_time < orin_time and the lab is attributable to the current admission.",
    "Preop attributable X-day window: max(previous_discharge_time, admission_time - X days) <= chart_time < orin_time; if previous_discharge_time is unavailable, lower bound becomes admission_time - X days.",
    "Postop current-stay window: orout_time <= chart_time <= discharge_time and the lab is attributable to the current admission.",
    "Postop attributable X-day window: orout_time <= chart_time <= min(next_admission_time, orout_time + X days); if next_admission_time is unavailable, upper bound becomes orout_time + X days.",
    "Nearest preop value means the most recent value before OR-in within the selected window.",
    "Nearest postop value means the earliest value after OR-out within the selected window."
  )
)
fwrite(notes_dt, file.path(processed_path, "labs_attributable_window_notes.csv"))

cat("Done: Lab_v3_first_nonMAC_attributable_windows.R\n")
