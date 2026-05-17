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
output_dir <- file.path(processed_root, "Lab_Attributable_Window_Coverage_3_31_2026")
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
  extra_cols = c("admission_time", "discharge_time", "orin_time")
)

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
stays[, previous_discharge_time := shift(discharge_time), by = subject_id]

anchor_ops <- merge(
  anchor_ops,
  stays[, .(subject_id, hadm_id, previous_discharge_time)],
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

cat("Joining labs to anchor operations ...\n")
setkey(anchor_ops, subject_id)
setkey(labs, subject_id)
joined <- merge(
  labs[, .(subject_id, chart_time, item_name, value)],
  anchor_ops[, .(subject_id, hadm_id, op_id, admission_time, orin_time, previous_discharge_time)],
  by = "subject_id",
  allow.cartesian = TRUE
)
joined <- joined[!is.na(orin_time) & chart_time < orin_time]

window_specs <- data.table(
  window = c(
    "current_stay_preop",
    "attributable_60d",
    "attributable_30d",
    "attributable_15d",
    "attributable_7d",
    "cumulative_preop"
  ),
  label_cn = c(
    "当前住院术前",
    "可归因术前60天",
    "可归因术前30天",
    "可归因术前15天",
    "可归因术前7天",
    "累计术前"
  ),
  label_en = c(
    "Current stay preop",
    "Attributable preop 60d",
    "Attributable preop 30d",
    "Attributable preop 15d",
    "Attributable preop 7d",
    "Cumulative preop"
  ),
  days = c(NA_real_, 60, 30, 15, 7, NA_real_)
)

within_window <- function(dt, window_name, days_back = NA_real_) {
  if (window_name == "current_stay_preop") {
    return(dt[!is.na(admission_time) & chart_time >= admission_time])
  }
  if (window_name == "cumulative_preop") {
    return(copy(dt))
  }
  lower_bound <- ifelse(
    is.na(dt$previous_discharge_time),
    dt$admission_time - days_back * 24 * 60,
    pmax(dt$previous_discharge_time, dt$admission_time - days_back * 24 * 60)
  )
  dt[!is.na(lower_bound) & chart_time >= lower_bound]
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

cat("Calculating coverage across candidate attributable windows ...\n")
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
    "current_stay_preop 定义：admission_time <= chart_time < orin_time。",
    "attributable_Xd 定义：max(previous_discharge_time, admission_time - X天) <= chart_time < orin_time；若无 previous_discharge_time，则退化为 admission_time - X天 <= chart_time < orin_time。",
    "cumulative_preop 定义：所有 chart_time < orin_time，不限制是否属于本次住院，因此覆盖率最高但最容易跨住院。"
  ),
  note_en = c(
    "first_nonMAC rule: for admissions with multiple surgeries, only the first non-MAC surgery is used as the admission anchor.",
    "current_stay_preop is defined as admission_time <= chart_time < OR-in.",
    "attributable_Xd is defined as max(previous_discharge_time, admission_time - X days) <= chart_time < OR-in; if previous_discharge_time is unavailable, it reduces to admission_time - X days <= chart_time < OR-in.",
    "cumulative_preop includes all chart_time < OR-in, regardless of admission boundary, so it has the highest coverage but the greatest risk of crossing admissions."
  )
)

fwrite(coverage_summary, file.path(output_dir, "lab_attributable_window_coverage_summary.csv"))
fwrite(item_coverage, file.path(output_dir, "lab_attributable_window_item_coverage.csv"))
fwrite(hb_cr_compare, file.path(output_dir, "lab_attributable_window_hb_creatinine_compare.csv"))
fwrite(notes, file.path(output_dir, "lab_attributable_window_notes.csv"))

cat("Done. Saved coverage summaries to:\n")
cat(output_dir, "\n")
