#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

source('/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/R_code_1_20_2026/pipeline_modules_word_aligned_3_30_2026/anchor_first_nonmac_utils.R')

path_raw <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/'
path_out <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/acute_status_preop_latest.csv'

dir.create(dirname(path_out), recursive = TRUE, showWarnings = FALSE)

assign_events_to_stays <- function(events_dt, stays_dt) {
  if (nrow(events_dt) == 0L) {
    out <- copy(events_dt)
    out[, assigned_hadm_id := NA_real_]
    return(out)
  }

  out <- copy(events_dt)
  out[, rec_id := .I]

  stay_map <- unique(stays_dt[, .(subject_id, hadm_id, admission_time, discharge_time)])
  setkey(stay_map, subject_id, admission_time, discharge_time)

  event_stay_candidate <- stay_map[
    out[, .(rec_id, subject_id, chart_time)],
    on = .(subject_id, admission_time <= chart_time, discharge_time >= chart_time),
    allow.cartesian = TRUE,
    nomatch = 0L
  ]

  if (nrow(event_stay_candidate) > 0L) {
    event_stay_map <- event_stay_candidate[
      order(rec_id, -admission_time, discharge_time)
    ][
      , .SD[1], by = rec_id
    ][
      , .(rec_id, assigned_hadm_id = hadm_id)
    ]
  } else {
    event_stay_map <- data.table(rec_id = integer(), assigned_hadm_id = numeric())
  }

  out <- merge(out, event_stay_map, by = 'rec_id', all.x = TRUE, sort = FALSE)
  out[, rec_id := NULL]
  out
}

aggregate_event <- function(matched_dt, keep_expr, event_filter_expr, value_col_name = NULL) {
  kept <- matched_dt[eval(keep_expr) & eval(event_filter_expr)]

  if (nrow(kept) == 0L) {
    out <- data.table(op_id = integer(), flag = integer(), interval_min = numeric(), event_chart_time = numeric())
    if (!is.null(value_col_name)) {
      out[, source_value := numeric()]
    } else {
      out[, source_code := character()]
    }
    return(out)
  }

  kept[, interval_to_surgery_min := orin_time - chart_time]
  kept <- kept[order(op_id, interval_to_surgery_min, chart_time)]
  first_hit <- kept[, .SD[1], by = op_id]

  out <- first_hit[, .(
    op_id = op_id,
    flag = 1L,
    interval_min = interval_to_surgery_min,
    event_chart_time = chart_time
  )]

  if (!is.null(value_col_name)) {
    out[, source_value := first_hit[[value_col_name]]]
  } else {
    out[, source_code := first_hit[['icd3']]]
  }

  out
}

cat('Loading anchor operations and stays ...\n')
anchor_ops <- load_first_nonmac_anchor_ops(raw_path = path_raw, extra_cols = c('admission_time', 'discharge_time'))

stays <- fread(
  file.path(path_raw, 'operations.csv'),
  select = c('subject_id', 'hadm_id', 'admission_time', 'discharge_time'),
  na.strings = c('', 'NA')
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

anchor_index <- anchor_ops[, .(subject_id, hadm_id, op_id, case_id, admission_time, discharge_time, orin_time)]
anchor_index <- anchor_index[!is.na(admission_time) & !is.na(orin_time) & orin_time > admission_time]
setorderv(anchor_index, c('subject_id', 'hadm_id', 'op_id'))

anchor_window <- anchor_index[, .(
  subject_id, hadm_id, op_id, case_id, admission_time, orin_time,
  start = admission_time,
  end = orin_time - 1
)]

cat('Loading diagnosis.csv and assigning admissions ...\n')
diag <- fread(
  file.path(path_raw, 'diagnosis.csv'),
  select = c('subject_id', 'chart_time', 'icd10_cm'),
  na.strings = c('', 'NA')
)
diag <- diag[subject_id %in% anchor_index$subject_id]
diag[, `:=`(
  chart_time = as.numeric(chart_time),
  icd3 = substr(gsub('\\.', '', toupper(trimws(icd10_cm))), 1, 3)
)]
diag <- diag[!is.na(subject_id) & !is.na(chart_time) & !is.na(icd3) & nchar(icd3) == 3]
diag <- assign_events_to_stays(diag[, .(subject_id, chart_time, icd3)], stays)

cat('Loading ward_vitals.csv and assigning admissions ...\n')
ward_file <- file.path(path_raw, 'ward_vitals.csv')
ward <- fread(
  cmd = sprintf("grep -iE ',(vent|iabp|ecmo|fio2),' %s", shQuote(ward_file)),
  header = FALSE,
  col.names = c('subject_id', 'chart_time', 'item_name', 'value'),
  na.strings = c('', 'NA')
)
ward <- ward[subject_id %in% anchor_index$subject_id]
ward[, `:=`(
  chart_time = as.numeric(chart_time),
  item_name = tolower(trimws(item_name)),
  value = suppressWarnings(as.numeric(value))
)]
ward <- ward[!is.na(subject_id) & !is.na(chart_time) & item_name %chin% c('vent', 'iabp', 'ecmo', 'fio2')]
ward <- assign_events_to_stays(ward[, .(subject_id, chart_time, item_name, value)], stays)

diag_event_defs <- list(
  list(var = 'acute_myocardial_infarction', codes = c('I21', 'I22', 'I23')),
  list(var = 'cerebral_infarction', codes = c('I63')),
  list(var = 'cardiac_arrest', codes = c('I46')),
  list(var = 'ards', codes = c('J80')),
  list(var = 'pulmonary_embolism', codes = c('I26')),
  list(var = 'sepsis', codes = c('A40', 'A41')),
  list(var = 'pneumonia', codes = c('J12', 'J13', 'J14', 'J15', 'J16', 'J17', 'J18')),
  list(var = 'shock', codes = c('R57'))
)

ward_event_defs <- list(
  list(var = 'ventilation', item = 'vent', rule = 'value == 1'),
  list(var = 'iabp', item = 'iabp', rule = 'value == 1'),
  list(var = 'ecmo', item = 'ecmo', rule = 'value == 1'),
  list(var = 'oxygen_therapy', item = 'fio2', rule = 'value > 30')
)

cat('Matching diagnosis events ...\n')
all_diag_codes <- unique(unlist(lapply(diag_event_defs, `[[`, 'codes')))
diag_union <- diag[icd3 %chin% all_diag_codes]
diag_union[, `:=`(start = chart_time, end = chart_time)]
setkey(diag_union, subject_id, start, end)
setkey(anchor_window, subject_id, start, end)
diag_matched <- foverlaps(
  diag_union,
  anchor_window,
  by.x = c('subject_id', 'start', 'end'),
  by.y = c('subject_id', 'start', 'end'),
  type = 'within',
  nomatch = 0L
)
diag_matched[, keep_event := !is.na(assigned_hadm_id) & assigned_hadm_id == hadm_id]

diag_result <- copy(anchor_index[, .(subject_id, hadm_id, op_id, orin_time)])
for (def in diag_event_defs) {
  cat('  diagnosis:', def$var, '\n')
  agg <- aggregate_event(
    diag_matched,
    keep_expr = quote(keep_event),
    event_filter_expr = substitute(icd3 %chin% codes, list(codes = def$codes))
  )
  setnames(
    agg,
    c('flag', 'interval_min', 'event_chart_time', 'source_code'),
    c(def$var, paste0(def$var, '_interval_to_surgery_min'), paste0(def$var, '_event_time'), paste0(def$var, '_source'))
  )
  diag_result <- merge(diag_result, agg, by = 'op_id', all.x = TRUE, sort = FALSE)
  set(diag_result, which(is.na(diag_result[[def$var]])), def$var, 0L)
}

cat('Matching ward support events ...\n')
ward_union <- ward[
  (item_name == 'vent' & value == 1) |
    (item_name == 'iabp' & value == 1) |
    (item_name == 'ecmo' & value == 1) |
    (item_name == 'fio2' & !is.na(value) & value > 30)
]
ward_union[, `:=`(start = chart_time, end = chart_time)]
setkey(ward_union, subject_id, start, end)
ward_matched <- foverlaps(
  ward_union,
  anchor_window,
  by.x = c('subject_id', 'start', 'end'),
  by.y = c('subject_id', 'start', 'end'),
  type = 'within',
  nomatch = 0L
)
ward_matched[, keep_event := !is.na(assigned_hadm_id) & assigned_hadm_id == hadm_id]

ward_result <- copy(anchor_index[, .(subject_id, hadm_id, op_id, orin_time)])
for (def in ward_event_defs) {
  cat('  ward:', def$var, '\n')
  event_filter_expr <- if (def$rule == 'value == 1') {
    substitute(item_name == item & value == 1, list(item = def$item))
  } else {
    substitute(item_name == item & !is.na(value) & value > 30, list(item = def$item))
  }
  agg <- aggregate_event(
    ward_matched,
    keep_expr = quote(keep_event),
    event_filter_expr = event_filter_expr,
    value_col_name = 'value'
  )
  setnames(
    agg,
    c('flag', 'interval_min', 'event_chart_time', 'source_value'),
    c(def$var, paste0(def$var, '_interval_to_surgery_min'), paste0(def$var, '_event_time'), paste0(def$var, '_source_value'))
  )
  ward_result <- merge(ward_result, agg, by = 'op_id', all.x = TRUE, sort = FALSE)
  set(ward_result, which(is.na(ward_result[[def$var]])), def$var, 0L)
}

acute_final <- merge(
  diag_result,
  ward_result[, !c('subject_id', 'hadm_id', 'orin_time')],
  by = 'op_id',
  all.x = TRUE,
  sort = FALSE
)
setcolorder(acute_final, c(
  'subject_id', 'hadm_id', 'op_id', 'orin_time',
  'acute_myocardial_infarction', 'acute_myocardial_infarction_interval_to_surgery_min',
  'cerebral_infarction', 'cerebral_infarction_interval_to_surgery_min',
  'cardiac_arrest', 'cardiac_arrest_interval_to_surgery_min',
  'ards', 'ards_interval_to_surgery_min',
  'pulmonary_embolism', 'pulmonary_embolism_interval_to_surgery_min',
  'sepsis', 'sepsis_interval_to_surgery_min',
  'pneumonia', 'pneumonia_interval_to_surgery_min',
  'shock', 'shock_interval_to_surgery_min',
  'ventilation', 'ventilation_interval_to_surgery_min',
  'iabp', 'iabp_interval_to_surgery_min',
  'ecmo', 'ecmo_interval_to_surgery_min',
  'oxygen_therapy', 'oxygen_therapy_interval_to_surgery_min'
))
setorderv(acute_final, c('subject_id', 'hadm_id', 'op_id'))
fwrite(acute_final, path_out)

cat('Done. Wrote: ', path_out, '\n', sep = '')
