#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

base_path <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/01_source_raw/'
out_path <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_labs_attributable_90d_latest.csv'

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

window_day_to_min <- function(days) days * 24 * 60

# Keep the same item set as existing attributable lab files.
target_items <- c(
  'glucose', 'creatinine', 'hct', 'potassium', 'sodium', 'hb', 'wbc',
  'platelet', 'chloride', 'lymphocyte', 'seg', 'bun', 'calcium',
  'phosphorus', 'albumin', 'total_bilirubin', 'alt', 'ast',
  'total_protein', 'alp', 'crp', 'sao2', 'hco3', 'ptinr', 'ph',
  'pao2', 'paco2', 'aptt', 'ica', 'fibrinogen', 'be', 'lacate',
  'ckmb', 'ck', 'troponin_i', 'hba1c', 'troponin_t', 'd_dimer'
)

calc_stats_wide <- function(anchor_dt, dt_window, prefix_name, nearest_mode = c('last', 'first')) {
  nearest_mode <- match.arg(nearest_mode)
  if (nrow(dt_window) == 0L) {
    out <- copy(anchor_dt[, .(op_id, subject_id, hadm_id)])
    setorderv(out, c('op_id'))
    return(out)
  }

  stats <- dt_window[, {
    nearest_idx <- if (nearest_mode == 'last') which.max(chart_time) else which.min(chart_time)
    .(
      val_nearest = value[nearest_idx],
      val_median = median(value, na.rm = TRUE),
      val_mean = mean(value, na.rm = TRUE)
    )
  }, by = .(op_id, item_name)]

  for (v in c('val_median', 'val_mean')) {
    set(stats, i = which(is.nan(stats[[v]])), j = v, value = NA_real_)
  }

  stats[, item_name := paste0(prefix_name, '_', item_name)]
  wide <- dcast(stats, op_id ~ item_name, value.var = c('val_nearest', 'val_median', 'val_mean'))

  old_names <- names(wide)
  new_names <- gsub('val_nearest_(.*)', '\\1_nearest', old_names)
  new_names <- gsub('val_median_(.*)', '\\1_median', new_names)
  new_names <- gsub('val_mean_(.*)', '\\1_mean', new_names)
  setnames(wide, old_names, new_names)

  out <- merge(anchor_dt[, .(op_id, subject_id, hadm_id)], wide, by = 'op_id', all.x = TRUE)
  setorderv(out, c('op_id'))
  out
}

cat('Loading operations.csv (all op_id, no first non-MAC anchor) ...\n')
ops <- fread(
  file.path(base_path, 'operations.csv'),
  select = c('op_id', 'subject_id', 'hadm_id', 'orin_time'),
  na.strings = c('', 'NA')
)
ops[, `:=`(
  orin_time = as.numeric(orin_time),
  subject_id = as.numeric(subject_id),
  hadm_id = as.numeric(hadm_id)
)]
ops <- unique(ops[!is.na(subject_id) & !is.na(op_id) & !is.na(orin_time)])
ops[, preop_90d_lower_bound := orin_time - window_day_to_min(90)]

cat('Loading labs.csv ...\n')
labs <- fread(
  file.path(base_path, 'labs.csv'),
  select = c('subject_id', 'chart_time', 'item_name', 'value'),
  na.strings = c('', 'NA')
)
labs[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]
labs <- labs[
  subject_id %in% ops$subject_id &
    item_name %chin% target_items &
    !is.na(chart_time) &
    !is.na(value)
]

cat('Joining labs to operations by subject_id ...\n')
setkey(ops, subject_id)
setkey(labs, subject_id)
joined <- merge(
  labs[, .(subject_id, chart_time, item_name, value)],
  ops[, .(subject_id, op_id, orin_time, preop_90d_lower_bound)],
  by = 'subject_id',
  allow.cartesian = TRUE
)

pre_90d <- joined[
  chart_time < orin_time &
    chart_time >= preop_90d_lower_bound
]

cat('Building wide preop 90d features ...\n')
out <- calc_stats_wide(
  anchor_dt = ops,
  dt_window = pre_90d,
  prefix_name = 'preop',
  nearest_mode = 'last'
)

# Ensure full expected column set (same style as existing files).
expected_cols <- c('op_id', 'subject_id', 'hadm_id')
for (item in target_items) {
  expected_cols <- c(
    expected_cols,
    paste0('preop_', item, '_nearest'),
    paste0('preop_', item, '_median'),
    paste0('preop_', item, '_mean')
  )
}
for (col in setdiff(expected_cols, names(out))) {
  out[, (col) := NA_real_]
}
setcolorder(out, expected_cols)
setorderv(out, 'op_id')

fwrite(out, out_path)

cat('Done. Wrote: ', out_path, '\n', sep = '')
cat('Rows: ', nrow(out), ', Cols: ', ncol(out), '\n', sep = '')
