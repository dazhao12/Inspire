#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

source('/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/R_code_1_20_2026/pipeline_modules_word_aligned_3_30_2026/anchor_first_nonmac_utils.R')

path_raw <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/'
path_out <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/acute_status_preop_unrestricted_latest.csv'

dir.create(dirname(path_out), recursive = TRUE, showWarnings = FALSE)

cat('Loading first non-MAC anchor operations ...\n')
anchor <- load_first_nonmac_anchor_ops(raw_path = path_raw)[
  !is.na(subject_id) & !is.na(op_id) & !is.na(orin_time),
  .(subject_id, hadm_id, op_id, orin_time)
]
setorderv(anchor, c('subject_id', 'hadm_id', 'op_id'))

cat('Loading diagnosis.csv ...\n')
diag <- fread(
  file.path(path_raw, 'diagnosis.csv'),
  select = c('subject_id', 'chart_time', 'icd10_cm'),
  na.strings = c('', 'NA')
)
diag <- diag[subject_id %in% anchor$subject_id]
diag[, `:=`(
  chart_time = as.numeric(chart_time),
  icd3 = substr(gsub('\\.', '', toupper(trimws(icd10_cm))), 1, 3)
)]
diag <- diag[!is.na(subject_id) & !is.na(chart_time) & !is.na(icd3) & nchar(icd3) == 3]

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

cat('Loading ward_vitals.csv ...\n')
ward_file <- file.path(path_raw, 'ward_vitals.csv')
ward <- fread(
  cmd = sprintf("grep -iE ',(vent|iabp|ecmo|fio2),' %s", shQuote(ward_file)),
  header = FALSE,
  col.names = c('subject_id', 'chart_time', 'item_name', 'value'),
  na.strings = c('', 'NA')
)
ward <- ward[subject_id %in% anchor$subject_id]
ward[, `:=`(
  chart_time = as.numeric(chart_time),
  item_name = tolower(trimws(item_name)),
  value = suppressWarnings(as.numeric(value))
)]
ward <- ward[!is.na(subject_id) & !is.na(chart_time) & item_name %chin% c('vent', 'iabp', 'ecmo', 'fio2')]

ward_event_defs <- list(
  list(var = 'ventilation', item = 'vent', rule = 'value == 1'),
  list(var = 'iabp', item = 'iabp', rule = 'value == 1'),
  list(var = 'ecmo', item = 'ecmo', rule = 'value == 1'),
  list(var = 'oxygen_therapy', item = 'fio2', rule = 'value > 30')
)

result <- copy(anchor)

cat('Aggregating diagnosis events (any time before surgery) ...\n')
for (def in diag_event_defs) {
  cat('  diagnosis:', def$var, '\n')
  sub <- diag[icd3 %chin% def$codes, .(subject_id, event_time = chart_time, matched_event_time = chart_time, source_code = icd3)]
  if (nrow(sub) == 0L) {
    result[, (def$var) := 0L]
    result[, (paste0(def$var, '_interval_to_surgery_min')) := as.numeric(NA)]
    result[, (paste0(def$var, '_event_time')) := as.numeric(NA)]
    result[, (paste0(def$var, '_source')) := NA_character_]
    next
  }

  setkey(sub, subject_id, event_time)
  hit <- sub[result, on = .(subject_id, event_time < orin_time), mult = 'last']

  # For non-equi join, source_code is the reliable match indicator.
  flag <- as.integer(!is.na(hit$source_code))
  result[, (def$var) := flag]
  result[, (paste0(def$var, '_interval_to_surgery_min')) := fifelse(flag == 1L, orin_time - hit$matched_event_time, as.numeric(NA))]
  result[, (paste0(def$var, '_event_time')) := fifelse(flag == 1L, hit$matched_event_time, as.numeric(NA))]
  result[, (paste0(def$var, '_source')) := hit$source_code]
}

cat('Aggregating ward events (any time before surgery) ...\n')
for (def in ward_event_defs) {
  cat('  ward:', def$var, '\n')
  sub <- if (def$rule == 'value == 1') {
    ward[item_name == def$item & value == 1, .(subject_id, event_time = chart_time, matched_event_time = chart_time, source_value = value)]
  } else {
    ward[item_name == def$item & !is.na(value) & value > 30, .(subject_id, event_time = chart_time, matched_event_time = chart_time, source_value = value)]
  }

  if (nrow(sub) == 0L) {
    result[, (def$var) := 0L]
    result[, (paste0(def$var, '_interval_to_surgery_min')) := as.numeric(NA)]
    result[, (paste0(def$var, '_event_time')) := as.numeric(NA)]
    result[, (paste0(def$var, '_source_value')) := as.numeric(NA)]
    next
  }

  setkey(sub, subject_id, event_time)
  hit <- sub[result, on = .(subject_id, event_time < orin_time), mult = 'last']

  # For non-equi join, source_value is the reliable match indicator.
  flag <- as.integer(!is.na(hit$source_value))
  result[, (def$var) := flag]
  result[, (paste0(def$var, '_interval_to_surgery_min')) := fifelse(flag == 1L, orin_time - hit$matched_event_time, as.numeric(NA))]
  result[, (paste0(def$var, '_event_time')) := fifelse(flag == 1L, hit$matched_event_time, as.numeric(NA))]
  result[, (paste0(def$var, '_source_value')) := hit$source_value]
}

setcolorder(result, c(
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
  'oxygen_therapy', 'oxygen_therapy_interval_to_surgery_min',
  'acute_myocardial_infarction_event_time', 'acute_myocardial_infarction_source',
  'cerebral_infarction_event_time', 'cerebral_infarction_source',
  'cardiac_arrest_event_time', 'cardiac_arrest_source',
  'ards_event_time', 'ards_source',
  'pulmonary_embolism_event_time', 'pulmonary_embolism_source',
  'sepsis_event_time', 'sepsis_source',
  'pneumonia_event_time', 'pneumonia_source',
  'shock_event_time', 'shock_source',
  'ventilation_event_time', 'ventilation_source_value',
  'iabp_event_time', 'iabp_source_value',
  'ecmo_event_time', 'ecmo_source_value',
  'oxygen_therapy_event_time', 'oxygen_therapy_source_value'
))

setorderv(result, c('subject_id', 'hadm_id', 'op_id'))
fwrite(result, path_out)

cat('Done. Wrote: ', path_out, '\n', sep = '')
