#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

# ==============================================================================
# 0. Paths
# ==============================================================================
path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
path_processed_base <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_folder <- "Meds_Preop_word_first_nonMAC_3_30_2026"
path_output <- file.path(path_processed_base, output_folder)

if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
}

WINDOW_14D_MIN <- 14 * 24 * 60

# ==============================================================================
# 1. Load anchor operations
# ==============================================================================
cat("Loading operations and selecting first non-MAC anchor surgery per admission...\n")

ops <- fread(
  file.path(path_raw, "operations.csv"),
  select = c(
    "subject_id", "hadm_id", "op_id", "case_id", "opdate",
    "antype", "admission_time", "orin_time", "opstart_time", "anstart_time"
  ),
  na.strings = c("", "NA")
)

ops[, `:=`(
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time),
  opstart_time = as.numeric(opstart_time),
  anstart_time = as.numeric(anstart_time),
  opdate_num = suppressWarnings(as.numeric(opdate)),
  antype_clean = toupper(trimws(as.character(antype)))
)]
ops[, anchor_sort_time := fcoalesce(opstart_time, anstart_time, orin_time, opdate_num, admission_time)]
ops[, hadm_group := fifelse(is.na(hadm_id), paste0("MISSING_HADM_", op_id), as.character(hadm_id))]

anchor_ops <- ops[
  !is.na(subject_id) & !is.na(op_id) & !is.na(orin_time) & !is.na(antype_clean) & antype_clean != "MAC"
][order(subject_id, hadm_group, anchor_sort_time, op_id)][
  , .SD[1], by = .(subject_id, hadm_group)
]

setorderv(anchor_ops, c("subject_id", "hadm_id", "op_id"))

anchor_index <- anchor_ops[, .(
  subject_id, hadm_id, op_id, case_id, admission_time, orin_time
)]
anchor_index[, start_time := fifelse(
  is.na(admission_time),
  orin_time - WINDOW_14D_MIN,
  pmin(admission_time, orin_time - WINDOW_14D_MIN)
)]
anchor_index <- anchor_index[!is.na(orin_time) & !is.na(start_time) & start_time < orin_time]

fwrite(anchor_index, file.path(path_output, "anchor_first_nonMAC_operations.csv"))
cat(sprintf("Anchor operations kept: %d\n", nrow(anchor_index)))

# ==============================================================================
# 2. Load medications and standardize ATC
# ==============================================================================
cat("Loading medications.csv ...\n")

meds <- fread(
  file.path(path_raw, "medications.csv"),
  select = c("subject_id", "chart_time", "atc_code", "atc_code2", "atc_code3"),
  na.strings = c("", "NA")
)

meds <- meds[subject_id %in% anchor_index$subject_id]
meds[, `:=`(
  chart_time = as.numeric(chart_time),
  atc_code1 = toupper(trimws(fifelse(is.na(atc_code), "", atc_code))),
  atc_code2 = toupper(trimws(fifelse(is.na(atc_code2), "", atc_code2))),
  atc_code3 = toupper(trimws(fifelse(is.na(atc_code3), "", atc_code3)))
)]
meds <- meds[!is.na(subject_id) & !is.na(chart_time)]
meds[, `:=`(start = chart_time, end = chart_time)]
setkey(meds, subject_id, start, end)

anchor_window <- anchor_index[, .(
  subject_id, hadm_id, op_id, case_id, admission_time, orin_time,
  start = start_time,
  end = orin_time - 1
)]
setkey(anchor_window, subject_id, start, end)

cat("Matching medication records to anchor windows ...\n")
meds_matched <- foverlaps(
  meds,
  anchor_window,
  by.x = c("subject_id", "start", "end"),
  by.y = c("subject_id", "start", "end"),
  type = "within",
  nomatch = 0L
)

# ==============================================================================
# 3. Word-defined medication categories using ATC only
# ==============================================================================
cat("Flagging Word-defined medication categories ...\n")

atc_match <- function(prefix_regex) {
  as.integer(
    grepl(prefix_regex, meds_matched$atc_code1) |
      grepl(prefix_regex, meds_matched$atc_code2) |
      grepl(prefix_regex, meds_matched$atc_code3)
  )
}

meds_matched[, `:=`(
  beta_blockers = atc_match("^C07A"),
  calcium_channel_blockers = atc_match("^C08"),
  acei = atc_match("^C09A"),
  arb = atc_match("^C09C"),
  diuretics = atc_match("^C03"),
  other_antihypertensive = atc_match("^C02"),
  amiodarone = atc_match("^C01BD01$"),
  other_antiarrhythmics = atc_match("^C01BC"),
  statins = atc_match("^C10AA"),
  antiplatelets = atc_match("^B01AC"),
  anticoagulants = atc_match("^B01AA|^B01AB|^B01AE|^B01AF|^B01AX"),
  thrombolytics = atc_match("^B01AD"),
  antifibrinolytics = atc_match("^B02A"),
  nitrates = atc_match("^C01DA"),
  insulins = atc_match("^A10A"),
  antidiabetics = atc_match("^A10B"),
  corticosteroids = atc_match("^H02"),
  thyroid_hormones = atc_match("^H03AA"),
  antithyroids = atc_match("^H03AB"),
  antibiotics = atc_match("^J01"),
  antimycotics = atc_match("^J02"),
  anti_tuberculosis = atc_match("^J04"),
  antivirals = atc_match("^J05"),
  ivig = atc_match("^J06BA02$"),
  antineoplastic = atc_match("^L01"),
  immunosuppression = atc_match("^L04"),
  nsaids = atc_match("^M01A|^N02B"),
  antiepileptics = atc_match("^N03"),
  antiparkinson = atc_match("^N04"),
  psycholeptics = atc_match("^N05"),
  psychoanaleptics = atc_match("^N06"),
  drugs_for_obstructive_airway_diseases = atc_match("^R03"),
  antihistamines = atc_match("^R06"),
  inotropes_and_vasopressors = atc_match("^C01C"),
  bile_and_liver_therapy = atc_match("^A05"),
  serotonin_5ht3_antagonists = atc_match("^A04AA"),
  h2_receptor_antagonists = atc_match("^A02BA"),
  proton_pump_inhibitors = atc_match("^A02BC"),
  other_drugs_for_acid_related_disorder = atc_match("^A02A|^A02BB|^A02BX"),
  opioids = atc_match("^N02A")
)]

category_cols <- c(
  "beta_blockers", "calcium_channel_blockers", "acei", "arb", "diuretics",
  "other_antihypertensive", "amiodarone", "other_antiarrhythmics", "statins",
  "antiplatelets", "anticoagulants", "thrombolytics", "antifibrinolytics", "nitrates",
  "insulins", "antidiabetics", "corticosteroids", "thyroid_hormones", "antithyroids",
  "antibiotics", "antimycotics", "anti_tuberculosis", "antivirals", "ivig",
  "antineoplastic", "immunosuppression", "nsaids", "antiepileptics", "antiparkinson",
  "psycholeptics", "psychoanaleptics", "drugs_for_obstructive_airway_diseases",
  "antihistamines", "inotropes_and_vasopressors", "bile_and_liver_therapy",
  "serotonin_5ht3_antagonists", "h2_receptor_antagonists", "proton_pump_inhibitors",
  "other_drugs_for_acid_related_disorder", "opioids"
)

# ==============================================================================
# 4. Aggregate to one row per anchor operation
# ==============================================================================
cat("Aggregating to one row per anchor operation ...\n")

if (nrow(meds_matched) > 0L) {
  meds_agg <- meds_matched[, c(
    list(
      subject_id = subject_id[1],
      hadm_id = hadm_id[1],
      case_id = case_id[1],
      med_records_in_window = .N
    ),
    lapply(.SD, function(v) as.integer(any(v == 1L, na.rm = TRUE)))
  ), by = op_id, .SDcols = category_cols]
} else {
  meds_agg <- anchor_index[, .(subject_id, hadm_id, op_id, case_id, med_records_in_window = 0L)]
}

meds_final <- merge(
  anchor_index[, .(subject_id, hadm_id, op_id, case_id, admission_time, orin_time, start_time)],
  meds_agg,
  by = c("subject_id", "hadm_id", "op_id", "case_id"),
  all.x = TRUE
)

if (!"med_records_in_window" %in% names(meds_final)) {
  meds_final[, med_records_in_window := 0L]
}

for (j in category_cols) {
  if (!j %in% names(meds_final)) meds_final[, (j) := 0L]
  set(meds_final, which(is.na(meds_final[[j]])), j, 0L)
}
set(meds_final, which(is.na(meds_final$med_records_in_window)), "med_records_in_window", 0L)

setorderv(meds_final, c("subject_id", "hadm_id", "op_id"))
fwrite(meds_final, file.path(path_output, "preop_meds_word_defined_first_nonMAC.csv"))

# ==============================================================================
# 5. Summary outputs
# ==============================================================================
cat("Building summary outputs ...\n")

summary_defs <- data.table(
  variable = category_cols,
  label_cn = c(
    "beta blockers", "calcium channel blockers", "ACEI", "ARB", "Diuretics",
    "Other antihypertensive", "amiodarone", "Other antiarrhythmics", "statins",
    "Antiplatelets", "anticoagulants", "thrombolytics", "antifibrinolytics", "Nitrates",
    "insulins", "antidiabetics", "corticosteroids", "Thyroid hormones", "antithyroids",
    "antibiotics", "antimycotics", "Anti-tuberculosis", "antivirals", "IVIG",
    "Antineoplastic", "Immunosuppression", "NSAIDS", "antiepileptics", "antiparkinson",
    "psycholeptics", "psychoanaleptics", "drugs for obstructive airway diseases",
    "antihistamines", "Intropes and vasopressors", "bile and liver therapy",
    "serotonin (5ht3) antagonists", "h2-receptor antagonists", "proton pump inhibitors",
    "Other drugs for acid related disorder", "opioids"
  )
)

summary_out <- rbindlist(lapply(seq_along(category_cols), function(i) {
  var <- category_cols[i]
  data.table(
    variable = var,
    label = summary_defs$label_cn[i],
    n_cases = sum(meds_final[[var]] == 1L, na.rm = TRUE),
    total_ops = nrow(meds_final),
    prevalence_pct = round(100 * mean(meds_final[[var]] == 1L, na.rm = TRUE), 2)
  )
}), use.names = TRUE)

setorder(summary_out, -prevalence_pct, variable)
fwrite(summary_out, file.path(path_output, "preop_meds_word_summary.csv"))

window_summary <- data.table(
  total_ops = nrow(meds_final),
  ops_any_med = sum(rowSums(meds_final[, ..category_cols]) > 0),
  ops_any_med_pct = round(100 * mean(rowSums(meds_final[, ..category_cols]) > 0), 2),
  median_med_records_in_window = round(median(meds_final$med_records_in_window, na.rm = TRUE), 1),
  p25_med_records_in_window = round(quantile(meds_final$med_records_in_window, 0.25, na.rm = TRUE, names = FALSE), 1),
  p75_med_records_in_window = round(quantile(meds_final$med_records_in_window, 0.75, na.rm = TRUE, names = FALSE), 1)
)
fwrite(window_summary, file.path(path_output, "preop_meds_word_window_summary.csv"))

notes <- data.table(
  note_type = c("anchor_definition", "window_definition", "atc_rule", "obstructive_airway_note", "linkage_risk"),
  note = c(
    "If an admission has multiple surgeries, anchor op is the first non-MAC surgery.",
    "Medication window = max(2 weeks before orin_time, admission_time to orin_time). Implemented as [min(admission_time, orin_time-14d), orin_time).",
    "Word-defined medication categories are implemented using ATC rules only, not drug-name keyword fallback.",
    "Word pasted by user listed obstructive airway diseases as N03****, which is inconsistent with ATC anatomy; v2 uses the clinically standard R03****.",
    "medications.csv only contains subject_id; linkage to anchor surgery is time-window based and may still carry cross-admission risk within the same subject."
  )
)
fwrite(notes, file.path(path_output, "preop_meds_word_notes.csv"))

cat("\nTop 20 medication prevalence:\n")
print(summary_out[1:min(20L, .N)])
cat("\nDone.\n")
