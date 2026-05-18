library(data.table)
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required for lab workbook output.")
}

# ==============================================================================
# 0. Paths
# ==============================================================================
base_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_v1_1_20_2026"

if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)

# ==============================================================================
# 1. Load operations and labs
# ==============================================================================
cat("Loading operations.csv and labs.csv ...\n")

ops <- fread(
  file.path(base_path, "operations.csv"),
  select = c("op_id", "subject_id", "hadm_id", "admission_time", "discharge_time", "orin_time"),
  na.strings = c("", "NA")
)
ops[, `:=`(
  hadm_id = as.numeric(hadm_id),
  admission_time = as.numeric(admission_time),
  discharge_time = as.numeric(discharge_time),
  orin_time = as.numeric(orin_time)
)]
ops <- ops[!is.na(orin_time)]
ops <- ops[!is.na(op_id) & !is.na(subject_id)]
ops[, hadm_n := uniqueN(hadm_id), by = subject_id]

labs <- fread(
  file.path(base_path, "labs.csv"),
  select = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
labs[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]

target_items <- c(
  "glucose", "creatinine", "hct", "potassium", "sodium", "hb", "wbc",
  "platelet", "chloride", "lymphocyte", "seg", "bun", "calcium",
  "phosphorus", "albumin", "total_bilirubin", "alt", "ast",
  "total_protein", "alp", "crp", "sao2", "hco3", "ptinr", "ph",
  "pao2", "paco2", "aptt", "ica", "fibrinogen", "be", "lacate",
  "ckmb", "ck", "troponin_i", "hba1c", "troponin_t", "d_dimer"
)
labs <- labs[item_name %chin% target_items & !is.na(chart_time)]

# Assign each lab record to one admission stay first (current-stay priority).
labs[, rec_id := .I]
stay_map <- unique(
  ops[!is.na(hadm_id) & !is.na(admission_time) & !is.na(discharge_time),
      .(subject_id, hadm_id, admission_time, discharge_time)]
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

cat("Merging by subject_id after stay assignment ...\n")
setkey(ops, subject_id)
setkey(labs, subject_id)
joined <- merge(labs, ops, by = "subject_id", allow.cartesian = TRUE)

# Shared pre-op eligibility (no future records)
joined <- joined[!is.na(orin_time) & chart_time < orin_time]
joined[, same_stay := !is.na(assigned_hadm_id) & !is.na(hadm_id) & (assigned_hadm_id == hadm_id)]
joined[, fallback_single_stay := is.na(assigned_hadm_id) & !is.na(hadm_n) & (hadm_n == 1L)]
joined[, in_current_stay := same_stay | fallback_single_stay]

# ==============================================================================
# 2. Window definitions
# ==============================================================================
cat("Applying pre-op windows ...\n")

# 1) strict history: pure history before current admission
dt_strict <- joined[!is.na(admission_time) & chart_time < admission_time]

# 2) current stay pre-op: admission to OR entry
dt_current <- joined[
  !is.na(admission_time) & chart_time >= admission_time & chart_time < orin_time & in_current_stay
]

# 3) cumulative pre-op: all records before OR entry
dt_cumulative <- joined[
  (!is.na(admission_time) & chart_time < admission_time) |
    (!is.na(admission_time) & chart_time >= admission_time & chart_time < orin_time & in_current_stay)
]

# current-stay subwindows
dt_90d <- dt_current[chart_time >= (orin_time - 90 * 24 * 60)]
dt_30d <- dt_current[chart_time >= (orin_time - 30 * 24 * 60)]
dt_7d <- dt_current[chart_time >= (orin_time - 7 * 24 * 60)]

# ==============================================================================
# 3. Aggregation helper
# ==============================================================================
calc_stats_wide <- function(dt_window, window_name) {
  cat(sprintf("Computing lab stats for window: %s ...\n", window_name))
  if (nrow(dt_window) == 0L) {
    out <- copy(ops[, .(op_id)])
    return(out)
  }

  stats <- dt_window[, .(
    val_nearest = value[which.max(chart_time)],
    val_median = median(value, na.rm = TRUE),
    val_mean = mean(value, na.rm = TRUE)
  ), by = .(op_id, item_name)]

  # avoid NaN from all-NA groups
  for (v in c("val_median", "val_mean")) {
    set(stats, i = which(is.nan(stats[[v]])), j = v, value = NA_real_)
  }

  stats[, item_name := paste0("preop_", item_name)]
  wide <- dcast(stats, op_id ~ item_name, value.var = c("val_nearest", "val_median", "val_mean"))

  old_names <- names(wide)
  new_names <- gsub("val_nearest_(.*)", "\\1_nearest", old_names)
  new_names <- gsub("val_median_(.*)", "\\1_median", new_names)
  new_names <- gsub("val_mean_(.*)", "\\1_mean", new_names)
  setnames(wide, old_names, new_names)

  # force one row per op_id for downstream consistency
  merge(ops[, .(op_id)], wide, by = "op_id", all.x = TRUE)
}

res_strict <- calc_stats_wide(dt_strict, "history_strict")
res_current <- calc_stats_wide(dt_current, "history_preop_current_stay")
res_cumulative <- calc_stats_wide(dt_cumulative, "history_cumulative_preop")
res_90d <- calc_stats_wide(dt_90d, "current_stay_90d")
res_30d <- calc_stats_wide(dt_30d, "current_stay_30d")
res_7d <- calc_stats_wide(dt_7d, "current_stay_7d")

# ==============================================================================
# 4. Save outputs
# ==============================================================================
cat("Saving outputs ...\n")

# backward-compatible files used by the pipeline:
# - any -> current_stay (explicitly bounded; no infinite history)
fwrite(res_current, file.path(processed_path, "preop_labs_features_any.csv"))
fwrite(res_30d, file.path(processed_path, "preop_labs_features_30d.csv"))
fwrite(res_7d, file.path(processed_path, "preop_labs_features_7d.csv"))

# additional explicit files
fwrite(res_strict, file.path(processed_path, "preop_labs_features_strict_history.csv"))
fwrite(res_current, file.path(processed_path, "preop_labs_features_current_stay.csv"))
fwrite(res_cumulative, file.path(processed_path, "preop_labs_features_cumulative_preop.csv"))
fwrite(res_90d, file.path(processed_path, "preop_labs_features_90d.csv"))

# add unit suffixes for lab variable names (keep op_id unchanged)
lab_unit_map <- c(
  glucose = "mg_dL",
  creatinine = "mg_dL",
  hct = "pct",
  potassium = "mmol_L",
  sodium = "mmol_L",
  hb = "g_dL",
  wbc = "10e3_uL",
  platelet = "10e3_uL",
  chloride = "mmol_L",
  lymphocyte = "pct",
  seg = "pct",
  bun = "mg_dL",
  calcium = "mg_dL",
  phosphorus = "mg_dL",
  albumin = "g_dL",
  total_bilirubin = "mg_dL",
  alt = "U_L",
  ast = "U_L",
  total_protein = "g_dL",
  alp = "U_L",
  crp = "mg_L",
  sao2 = "pct",
  hco3 = "mmol_L",
  ptinr = "INR",
  ph = "unitless",
  pao2 = "mmHg",
  paco2 = "mmHg",
  aptt = "sec",
  ica = "mmol_L",
  fibrinogen = "mg_dL",
  be = "mmol_L",
  lacate = "mmol_L",
  ckmb = "unknown_unit",
  ck = "U_L",
  troponin_i = "ng_mL",
  hba1c = "pct",
  troponin_t = "ng_mL",
  d_dimer = "ug_mL_FEU"
)

append_unit_suffix <- function(dt_wide) {
  nm <- names(dt_wide)
  lab_cols <- grep("^preop_.*_(nearest|median|mean)$", nm, value = TRUE)
  if (length(lab_cols) == 0L) return(copy(dt_wide))

  new_names <- nm
  for (col in lab_cols) {
    parts <- regmatches(col, regexec("^preop_(.*)_(nearest|median|mean)$", col))[[1]]
    if (length(parts) == 3) {
      lab_name <- parts[2]
      stat_name <- parts[3]
      unit <- if (!is.na(lab_unit_map[lab_name])) unname(lab_unit_map[lab_name]) else "unknown_unit"
      new_col <- paste0("preop_", lab_name, "_", stat_name, "_", unit)
      new_names[match(col, nm)] <- new_col
    }
  }
  out <- copy(dt_wide)
  setnames(out, nm, new_names)
  out
}

res_strict_units <- append_unit_suffix(res_strict)
res_current_units <- append_unit_suffix(res_current)
res_cumulative_units <- append_unit_suffix(res_cumulative)

fwrite(res_strict_units, file.path(processed_path, "preop_labs_features_strict_history_with_units.csv"))
fwrite(res_current_units, file.path(processed_path, "preop_labs_features_current_stay_with_units.csv"))
fwrite(res_cumulative_units, file.path(processed_path, "preop_labs_features_cumulative_preop_with_units.csv"))

# ==============================================================================
# 5. Window comparison summary
# ==============================================================================
coverage_one <- function(dt_wide, window_name) {
  nearest_cols <- grep("_nearest$", names(dt_wide), value = TRUE)
  any_lab <- if (length(nearest_cols) == 0L) rep(FALSE, nrow(dt_wide)) else rowSums(!is.na(dt_wide[, ..nearest_cols])) > 0

  out <- data.table(
    window = window_name,
    n_ops = nrow(dt_wide),
    n_ops_any_lab = sum(any_lab),
    pct_ops_any_lab = round(mean(any_lab) * 100, 2)
  )

  key_cols <- c("preop_creatinine_nearest", "preop_hb_nearest", "preop_wbc_nearest")
  for (k in key_cols) {
    if (k %in% names(dt_wide)) {
      out[, (paste0("pct_", k, "_non_missing")) := round(mean(!is.na(dt_wide[[k]])) * 100, 2)]
    } else {
      out[, (paste0("pct_", k, "_non_missing")) := NA_real_]
    }
  }
  out
}

cmp <- rbindlist(list(
  coverage_one(res_strict, "history_strict"),
  coverage_one(res_current, "history_preop_current_stay"),
  coverage_one(res_cumulative, "history_cumulative_preop"),
  coverage_one(res_90d, "current_stay_90d"),
  coverage_one(res_30d, "current_stay_30d"),
  coverage_one(res_7d, "current_stay_7d")
), use.names = TRUE)

fwrite(cmp, file.path(processed_path, "preop_labs_window_comparison.csv"))

# split variable coverage summaries into 3 sheets (instead of one merged detail table)
build_window_var_summary <- function(dt_wide_units, window_name) {
  lab_cols <- setdiff(names(dt_wide_units), "op_id")
  if (length(lab_cols) == 0L) {
    return(data.table(window = window_name))
  }
  out <- rbindlist(lapply(lab_cols, function(col_name) {
    x <- dt_wide_units[[col_name]]
    data.table(
      variable_with_unit = col_name,
      n_non_missing = sum(!is.na(x)),
      pct_non_missing = round(mean(!is.na(x)) * 100, 2)
    )
  }))
  out[]
}

sum_strict <- build_window_var_summary(res_strict_units, "history_strict")
sum_current <- build_window_var_summary(res_current_units, "history_preop_current_stay")
sum_cumulative <- build_window_var_summary(res_cumulative_units, "history_cumulative_preop")

fwrite(sum_strict, file.path(processed_path, "preop_labs_summary_strict_with_units.csv"))
fwrite(sum_current, file.path(processed_path, "preop_labs_summary_current_stay_with_units.csv"))
fwrite(sum_cumulative, file.path(processed_path, "preop_labs_summary_cumulative_preop_with_units.csv"))

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "history_strict")
openxlsx::writeData(wb, "history_strict", sum_strict)
openxlsx::addWorksheet(wb, "current_stay_preop")
openxlsx::writeData(wb, "current_stay_preop", sum_current)
openxlsx::addWorksheet(wb, "cumulative_preop")
openxlsx::writeData(wb, "cumulative_preop", sum_cumulative)
openxlsx::saveWorkbook(
  wb,
  file.path(processed_path, "preop_labs_three_windows_with_units.xlsx"),
  overwrite = TRUE
)

cat("\nLab window comparison:\n")
print(cmp)
cat("\nDone.\n")
