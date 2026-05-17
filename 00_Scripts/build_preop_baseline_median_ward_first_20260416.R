#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

source('/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/R_code_1_20_2026/pipeline_modules_word_aligned_3_30_2026/anchor_first_nonmac_utils.R')

raw_path <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/'
out_file <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_baseline_final_median_latest.csv'
out_source_qc <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_baseline_final_median_source_coverage.csv'

WARD_WINDOW_MIN <- 1440  # 24h
OR_WINDOW_MIN <- 120     # 120min

cat('Loading first non-MAC anchor operations ...\n')
ops <- load_first_nonmac_anchor_ops(
  raw_path = raw_path,
  extra_cols = c('admission_time', 'orin_time')
)
ops <- unique(ops[!is.na(op_id) & !is.na(subject_id) & !is.na(orin_time), .(
  op_id, subject_id, hadm_id,
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time)
)])

cat('Loading ward_vitals and vitals ...\n')
ward_vitals <- fread(
  file.path(raw_path, 'ward_vitals.csv'),
  select = c('subject_id', 'chart_time', 'item_name', 'value'),
  na.strings = c('', 'NA')
)
or_vitals <- fread(
  file.path(raw_path, 'vitals.csv'),
  select = c('op_id', 'chart_time', 'item_name', 'value'),
  na.strings = c('', 'NA')
)

ward_vitals[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]
or_vitals[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]

ward_items <- c('nibp_sbp', 'nibp_dbp', 'nibp_mbp', 'hr', 'spo2', 'rr', 'bt')
ward_subset <- ward_vitals[
  subject_id %in% ops$subject_id &
    item_name %chin% ward_items &
    !is.na(chart_time) & !is.na(value)
]

or_vitals[item_name %chin% c('nibp_sbp', 'art_sbp'), item_group := 'sbp']
or_vitals[item_name %chin% c('nibp_dbp', 'art_dbp'), item_group := 'dbp']
or_vitals[item_name %chin% c('nibp_mbp', 'art_mbp'), item_group := 'mbp']
or_vitals[item_name == 'hr', item_group := 'hr']
or_vitals[item_name == 'spo2', item_group := 'spo2']
or_vitals[item_name == 'rr', item_group := 'rr']
or_vitals[item_name == 'bt', item_group := 'bt']
or_subset <- or_vitals[
  op_id %in% ops$op_id &
    !is.na(item_group) &
    !is.na(chart_time) & !is.na(value)
]

ops_win <- copy(ops)
ops_win[, ward_window_start := fifelse(
  is.na(admission_time),
  orin_time - WARD_WINDOW_MIN,
  pmax(admission_time, orin_time - WARD_WINDOW_MIN)
)]
ops_win[, or_window_start := orin_time - OR_WINDOW_MIN]

cat('Matching Ward window and aggregating median ...\n')
ward_matched <- ward_subset[
  ops_win,
  on = .(subject_id, chart_time >= ward_window_start, chart_time < orin_time),
  nomatch = NULL,
  .(op_id = i.op_id, item_name = x.item_name, value = x.value)
]

ward_agg <- ward_matched[, .(val_median = median(value, na.rm = TRUE)), by = .(op_id, item_name)]
ward_base <- dcast(ward_agg, op_id ~ item_name, value.var = 'val_median')
setnames(
  ward_base,
  old = c('nibp_sbp', 'nibp_dbp', 'nibp_mbp', 'hr', 'spo2', 'rr', 'bt'),
  new = c('ward_sbp', 'ward_dbp', 'ward_mbp', 'ward_hr', 'ward_spo2', 'ward_rr', 'ward_bt'),
  skip_absent = TRUE
)

cat('Matching OR window and aggregating median ...\n')
or_matched <- or_subset[
  ops_win,
  on = .(op_id, chart_time >= or_window_start, chart_time < orin_time),
  nomatch = NULL,
  .(op_id = i.op_id, item_group = x.item_group, value = x.value)
]

or_agg <- or_matched[, .(val_median = median(value, na.rm = TRUE)), by = .(op_id, item_group)]
or_base <- dcast(or_agg, op_id ~ item_group, value.var = 'val_median')
setnames(or_base, old = names(or_base)[-1], new = paste0('or_', names(or_base)[-1]))

cat('Applying Ward-first OR-fallback rule ...\n')
final_dt <- merge(ops[, .(subject_id, hadm_id, op_id)], ward_base, by = 'op_id', all.x = TRUE)
final_dt <- merge(final_dt, or_base, by = 'op_id', all.x = TRUE)
setDT(final_dt)

vitals_short <- c('sbp', 'dbp', 'mbp', 'hr', 'spo2', 'rr', 'bt')
for (v in vitals_short) {
  ward_col <- paste0('ward_', v)
  or_col <- paste0('or_', v)
  preop_col <- paste0('preop_', v)
  source_col <- paste0('source_', v)

  final_dt[, (preop_col) := round(fcoalesce(get(ward_col), get(or_col)), 1)]
  final_dt[, (source_col) := fcase(
    !is.na(get(ward_col)), 'Ward',
    !is.na(get(or_col)), 'OR_Induction',
    default = 'Missing'
  )]
}

setorder(final_dt, subject_id, hadm_id, op_id)

cols_to_keep <- c(
  'subject_id', 'hadm_id', 'op_id',
  'preop_sbp', 'preop_dbp', 'preop_mbp',
  'preop_hr', 'preop_spo2', 'preop_rr', 'preop_bt',
  'source_sbp', 'source_dbp', 'source_mbp',
  'source_hr', 'source_spo2', 'source_rr', 'source_bt'
)

fwrite(final_dt[, ..cols_to_keep], out_file)

source_qc <- rbindlist(lapply(vitals_short, function(v) {
  src <- final_dt[[paste0('source_', v)]]
  n_total <- nrow(final_dt)
  data.table(
    vital = paste0('preop_', v),
    n_total = n_total,
    n_from_ward = sum(src == 'Ward', na.rm = TRUE),
    n_from_or = sum(src == 'OR_Induction', na.rm = TRUE),
    n_missing = sum(src == 'Missing', na.rm = TRUE),
    pct_from_ward = round(100 * sum(src == 'Ward', na.rm = TRUE) / n_total, 2),
    pct_from_or = round(100 * sum(src == 'OR_Induction', na.rm = TRUE) / n_total, 2),
    pct_missing = round(100 * sum(src == 'Missing', na.rm = TRUE) / n_total, 2),
    ward_window = 'max(admission_time, orin_time-1440) <= chart_time < orin_time',
    or_window = 'orin_time-120 <= chart_time < orin_time',
    aggregation = 'median',
    selection_rule = 'Ward_first_then_OR_fallback'
  )
}))
fwrite(source_qc, out_source_qc)

cat('Done. Wrote: ', out_file, '\n', sep = '')
cat('Source QC: ', out_source_qc, '\n', sep = '')
cat('Rows: ', nrow(final_dt), ', Cols: ', length(cols_to_keep), '\n', sep = '')
