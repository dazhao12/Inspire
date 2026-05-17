#!/usr/bin/env Rscript

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
processed_root <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_dir <- file.path(processed_root, "Postop_Lab_Attributable_Window_Coverage_3_31_2026")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

target_items <- c(
  "glucose", "creatinine", "hct", "potassium", "sodium", "hb", "wbc",
  "platelet", "chloride", "lymphocyte", "seg", "bun", "calcium",
  "phosphorus", "albumin", "total_bilirubin", "alt", "ast",
  "total_protein", "alp", "crp", "sao2", "hco3", "ptinr", "ph",
  "pao2", "paco2", "aptt", "ica", "fibrinogen", "be", "lacate",
  "ckmb", "ck", "troponin_i", "hba1c", "troponin_t", "d_dimer"
)

cat("Loading anchor operations ...\n")
anchor_ops <- load_first_nonmac_anchor_ops(
  raw_path = raw_path,
  extra_cols = c("admission_time", "discharge_time", "orin_time", "orout_time")
)
anchor_ops[, hadm_n := uniqueN(hadm_group), by = subject_id]

cat("Loading stay history from operations.csv ...\n")
stays <- fread(
  file.path(raw_path, "operations.csv"),
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
stays[, next_admission_time := shift(admission_time, type = "lead"), by = subject_id]

anchor_ops <- merge(
  anchor_ops,
  stays[, .(subject_id, hadm_id, next_admission_time)],
  by = c("subject_id", "hadm_id"),
  all.x = TRUE,
  sort = FALSE
)

cat("Loading labs.csv ...\n")
labs <- fread(
  file.path(raw_path, "labs.csv"),
  select = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
labs[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]
labs <- labs[
  subject_id %in% anchor_ops$subject_id &
    item_name %chin% target_items &
    !is.na(chart_time)
]

labs[, rec_id := .I]
stay_map <- unique(
  anchor_ops[!is.na(hadm_id) & !is.na(admission_time) & !is.na(discharge_time),
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

cat("Joining labs to anchor operations ...\n")
setkey(anchor_ops, subject_id)
setkey(labs, subject_id)
joined <- merge(
  labs[, .(subject_id, chart_time, item_name, value, assigned_hadm_id)],
  anchor_ops[, .(subject_id, hadm_id, hadm_n, op_id, orout_time, discharge_time, next_admission_time)],
  by = "subject_id",
  allow.cartesian = TRUE
)
joined <- joined[!is.na(orout_time) & chart_time >= orout_time]
joined[, same_stay := !is.na(assigned_hadm_id) & !is.na(hadm_id) & (assigned_hadm_id == hadm_id)]
joined[, fallback_single_stay := is.na(assigned_hadm_id) & !is.na(hadm_n) & (hadm_n == 1L)]
joined[, in_current_stay := same_stay | fallback_single_stay]

window_specs <- data.table(
  window = c(
    "current_stay_postop",
    "attributable_postop_7d",
    "attributable_postop_15d",
    "attributable_postop_30d",
    "attributable_postop_60d",
    "cumulative_postop"
  ),
  label_cn = c(
    "当前住院术后",
    "可归因术后7天",
    "可归因术后15天",
    "可归因术后30天",
    "可归因术后60天",
    "累计术后"
  ),
  label_en = c(
    "Current stay postop",
    "Attributable postop 7d",
    "Attributable postop 15d",
    "Attributable postop 30d",
    "Attributable postop 60d",
    "Cumulative postop"
  ),
  days = c(NA_real_, 7, 15, 30, 60, NA_real_)
)

within_window <- function(dt, window_name, days_forward = NA_real_) {
  if (window_name == "current_stay_postop") {
    return(dt[in_current_stay == TRUE & !is.na(discharge_time) & chart_time <= discharge_time])
  }
  if (window_name == "cumulative_postop") {
    return(copy(dt))
  }
  upper_bound <- ifelse(
    is.na(dt$next_admission_time),
    dt$orout_time + days_forward * 24 * 60,
    pmin(dt$next_admission_time, dt$orout_time + days_forward * 24 * 60)
  )
  dt[!is.na(upper_bound) & chart_time <= upper_bound]
}

coverage_one <- function(dt_window, spec_row, total_ops) {
  if (nrow(dt_window) == 0L) {
    return(data.table(
      window = spec_row$window,
      变量 = spec_row$label_cn,
      variable_en = spec_row$label_en,
      n_ops = total_ops,
      n_ops_any_lab = 0L,
      pct_ops_any_lab = 0,
      pct_hb = 0,
      pct_creatinine = 0,
      pct_wbc = 0
    ))
  }
  op_item <- unique(dt_window[, .(op_id, item_name)])
  ops_any <- uniqueN(op_item$op_id)
  item_pct <- function(item_name_target) {
    round(100 * uniqueN(op_item[item_name == item_name_target]$op_id) / total_ops, 2)
  }
  data.table(
    window = spec_row$window,
    变量 = spec_row$label_cn,
    variable_en = spec_row$label_en,
    n_ops = total_ops,
    n_ops_any_lab = ops_any,
    pct_ops_any_lab = round(100 * ops_any / total_ops, 2),
    pct_hb = item_pct("hb"),
    pct_creatinine = item_pct("creatinine"),
    pct_wbc = item_pct("wbc")
  )
}

item_coverage_one <- function(dt_window, spec_row, total_ops) {
  present <- unique(dt_window[, .(op_id, item_name)])
  if (nrow(present) == 0L) {
    return(data.table(
      window = spec_row$window,
      变量 = spec_row$label_cn,
      variable_en = spec_row$label_en,
      item_name = target_items,
      n_ops_with_item = 0L,
      pct_ops_with_item = 0
    ))
  }
  cov_dt <- present[, .(n_ops_with_item = uniqueN(op_id)), by = item_name]
  cov_dt <- merge(data.table(item_name = target_items), cov_dt, by = "item_name", all.x = TRUE)
  cov_dt[is.na(n_ops_with_item), n_ops_with_item := 0L]
  cov_dt[, `:=`(
    window = spec_row$window,
    变量 = spec_row$label_cn,
    variable_en = spec_row$label_en,
    pct_ops_with_item = round(100 * n_ops_with_item / total_ops, 2)
  )]
  setcolorder(cov_dt, c("window", "变量", "variable_en", "item_name", "n_ops_with_item", "pct_ops_with_item"))
  cov_dt
}

total_ops <- nrow(anchor_ops)

cat("Calculating postop coverage across candidate windows ...\n")
coverage_summary <- rbindlist(lapply(seq_len(nrow(window_specs)), function(i) {
  spec <- window_specs[i]
  dt_window <- within_window(joined, spec$window, spec$days)
  coverage_one(dt_window, spec, total_ops)
}), fill = TRUE)

item_coverage <- rbindlist(lapply(seq_len(nrow(window_specs)), function(i) {
  spec <- window_specs[i]
  dt_window <- within_window(joined, spec$window, spec$days)
  item_coverage_one(dt_window, spec, total_ops)
}), fill = TRUE)

hb_cr_compare <- item_coverage[item_name %in% c("hb", "creatinine")][
  order(match(window, window_specs$window), item_name)
]

notes <- data.table(
  note_cn = c(
    "first_nonMAC 规则：每次住院若有多台手术，仅用首次非 MAC 手术作为该次住院锚点。",
    "current_stay_postop 定义：orout_time <= chart_time <= discharge_time，且化验被归到本次住院。",
    "attributable_postop_Xd 定义：orout_time <= chart_time <= min(next_admission_time, orout_time + X天)；若无 next_admission_time，则退化为 orout_time <= chart_time <= orout_time + X天。",
    "cumulative_postop 定义：所有 chart_time >= orout_time，不限制是否进入后续住院，因此覆盖率最高但最容易跨住院。"
  ),
  note_en = c(
    "first_nonMAC rule: for admissions with multiple surgeries, only the first non-MAC surgery is used as the admission anchor.",
    "current_stay_postop is defined as OR-out <= chart_time <= discharge_time, with the lab assigned to the current admission.",
    "attributable_postop_Xd is defined as OR-out <= chart_time <= min(next_admission_time, OR-out + X days); if next_admission_time is unavailable, it reduces to OR-out <= chart_time <= OR-out + X days.",
    "cumulative_postop includes all chart_time >= OR-out regardless of later admissions, so it has the highest coverage but the greatest risk of crossing admissions."
  )
)

fwrite(coverage_summary, file.path(output_dir, "postop_lab_attributable_window_coverage_summary.csv"))
fwrite(item_coverage, file.path(output_dir, "postop_lab_attributable_window_item_coverage.csv"))
fwrite(hb_cr_compare, file.path(output_dir, "postop_lab_attributable_window_hb_creatinine_compare.csv"))
fwrite(notes, file.path(output_dir, "postop_lab_attributable_window_notes.csv"))

cat("Done. Saved postop coverage summaries to:\n")
cat(output_dir, "\n")
