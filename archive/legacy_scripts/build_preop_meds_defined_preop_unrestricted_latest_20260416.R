#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

source('/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/R_code_1_20_2026/pipeline_modules_word_aligned_3_30_2026/anchor_first_nonmac_utils.R')

path_raw <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/'
out_file <- '/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_meds_defined_preop_unrestricted_latest.csv'

cat('Loading first non-MAC anchor operations ...\n')
anchor_ops <- load_first_nonmac_anchor_ops(
  raw_path = path_raw,
  extra_cols = c('admission_time', 'orin_time', 'case_id')
)
anchor_index <- anchor_ops[, .(
  subject_id,
  hadm_id,
  op_id,
  case_id,
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time)
)]
anchor_index <- anchor_index[!is.na(subject_id) & !is.na(op_id) & !is.na(orin_time)]
anchor_index[, start_time := as.numeric(NA)]
setorderv(anchor_index, c('subject_id', 'hadm_id', 'op_id'))

cat('Loading medications.csv ...\n')
meds <- fread(
  file.path(path_raw, 'medications.csv'),
  select = c('subject_id', 'chart_time', 'atc_code', 'atc_code2', 'atc_code3'),
  na.strings = c('', 'NA')
)
meds <- meds[subject_id %in% anchor_index$subject_id]
meds[, `:=`(
  chart_time = as.numeric(chart_time),
  atc_code1 = toupper(trimws(fifelse(is.na(atc_code), '', atc_code))),
  atc_code2 = toupper(trimws(fifelse(is.na(atc_code2), '', atc_code2))),
  atc_code3 = toupper(trimws(fifelse(is.na(atc_code3), '', atc_code3)))
)]
meds <- meds[!is.na(subject_id) & !is.na(chart_time)]

cat('Flagging medication classes by ATC ...\n')
category_defs <- list(
  beta_blockers = '^C07A',
  calcium_channel_blockers = '^C08',
  acei = '^C09A',
  arb = '^C09C',
  diuretics = '^C03',
  other_antihypertensive = '^C02',
  amiodarone = '^C01BD01$',
  other_antiarrhythmics = '^C01BC',
  statins = '^C10AA',
  antiplatelets = '^B01AC',
  anticoagulants = '^B01AA|^B01AB|^B01AE|^B01AF|^B01AX',
  thrombolytics = '^B01AD',
  antifibrinolytics = '^B02A',
  nitrates = '^C01DA',
  insulins = '^A10A',
  antidiabetics = '^A10B',
  corticosteroids = '^H02',
  thyroid_hormones = '^H03AA',
  antithyroids = '^H03AB',
  antibiotics = '^J01',
  antimycotics = '^J02',
  anti_tuberculosis = '^J04',
  antivirals = '^J05',
  ivig = '^J06BA02$',
  antineoplastic = '^L01',
  immunosuppression = '^L04',
  nsaids = '^M01A|^N02B',
  antiepileptics = '^N03',
  antiparkinson = '^N04',
  psycholeptics = '^N05',
  psychoanaleptics = '^N06',
  drugs_for_obstructive_airway_diseases = '^R03',
  antihistamines = '^R06',
  inotropes_and_vasopressors = '^C01C',
  bile_and_liver_therapy = '^A05',
  serotonin_5ht3_antagonists = '^A04AA',
  h2_receptor_antagonists = '^A02BA',
  proton_pump_inhibitors = '^A02BC',
  other_drugs_for_acid_related_disorder = '^A02A|^A02BB|^A02BX',
  opioids = '^N02A'
)
category_cols <- names(category_defs)

for (nm in category_cols) {
  p <- category_defs[[nm]]
  meds[, (nm) := as.integer(
    grepl(p, atc_code1) | grepl(p, atc_code2) | grepl(p, atc_code3)
  )]
}

cat('Aggregating meds at subject+time and building cumulative exposure ...\n')
meds_time <- meds[, c(
  list(n_records = .N),
  lapply(.SD, function(v) as.integer(any(v == 1L, na.rm = TRUE)))
), by = .(subject_id, chart_time), .SDcols = category_cols]

setorderv(meds_time, c('subject_id', 'chart_time'))
meds_time[, cum_n_records := cumsum(n_records), by = subject_id]
for (nm in category_cols) {
  meds_time[, (paste0('cum_', nm)) := cummax(get(nm)), by = subject_id]
}

cum_cols <- c('cum_n_records', paste0('cum_', category_cols))
meds_cum <- meds_time[, c('subject_id', 'chart_time', cum_cols), with = FALSE]
setkey(meds_cum, subject_id, chart_time)

cat('Matching cumulative pre-op exposure to anchors (chart_time < orin_time) ...\n')
hit <- meds_cum[anchor_index, on = .(subject_id, chart_time < orin_time), mult = 'last']

out <- copy(anchor_index)
out[, med_records_in_window := as.integer(0)]
matched <- !is.na(hit$cum_n_records)
out[matched, med_records_in_window := as.integer(hit$cum_n_records[matched])]

for (nm in category_cols) {
  out[, (nm) := as.integer(0)]
  v <- hit[[paste0('cum_', nm)]]
  out[matched, (nm) := as.integer(v[matched])]
}

setcolorder(out, c(
  'subject_id', 'hadm_id', 'op_id', 'case_id', 'admission_time',
  'orin_time', 'start_time', 'med_records_in_window', category_cols
))
setorderv(out, c('subject_id', 'hadm_id', 'op_id'))

fwrite(out, out_file)

cat('Done. Wrote: ', out_file, '\n', sep = '')
cat('Rows: ', nrow(out), ', Cols: ', ncol(out), '\n', sep = '')
cat('Any-med prevalence: ', round(100 * mean(out$med_records_in_window > 0), 2), '%\n', sep = '')
