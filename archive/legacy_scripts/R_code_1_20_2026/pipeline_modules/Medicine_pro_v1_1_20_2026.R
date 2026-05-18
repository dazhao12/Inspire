#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

# ==============================================================================
# 1) Paths
# ==============================================================================
path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
path_processed_base <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_folder <- "Meds_Preop_1_20_2026"
path_output <- file.path(path_processed_base, output_folder)

if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
  cat("Created output directory:", path_output, "\n")
}

cat("Loading operations and medications...\n")

ops_df <- fread(
  file.path(path_raw, "operations.csv"),
  select = c("subject_id", "hadm_id", "op_id", "admission_time", "orin_time", "orout_time")
)

meds_df <- fread(
  file.path(path_raw, "medications.csv"),
  select = c("subject_id", "chart_time", "drug_name", "drug_name2", "drug_name3",
             "atc_code", "atc_code2", "atc_code3"),
  colClasses = list(
    numeric = c("subject_id", "chart_time")
  )
)

# ==============================================================================
# 2) Prepare operation windows
# ==============================================================================
setDT(ops_df)
ops_df <- unique(ops_df[!is.na(subject_id) & !is.na(op_id)])
setorder(ops_df, subject_id, hadm_id, orin_time, op_id)
ops_df[, prev_orout_time := shift(orout_time, type = "lag"), by = .(subject_id, hadm_id)]

build_ops_window <- function(kind) {
  dt <- copy(ops_df)

  if (kind == "history_strict") {
    dt <- dt[!is.na(admission_time)]
    dt[, `:=`(start_time = -Inf, end_time = admission_time)]
  } else if (kind == "preop_current_stay") {
    dt <- dt[!is.na(admission_time) & !is.na(orin_time)]
    dt[, `:=`(start_time = admission_time, end_time = orin_time)]
  } else if (kind == "preop_immediate_24h") {
    dt <- dt[!is.na(admission_time) & !is.na(orin_time)]
    dt[, prev_orout_floor := fifelse(is.na(prev_orout_time), -Inf, prev_orout_time)]
    dt[, start_time := pmax(admission_time, prev_orout_floor, orin_time - 24 * 60, na.rm = TRUE)]
    dt[, end_time := orin_time]
    dt[, prev_orout_floor := NULL]
  } else if (kind == "preop_immediate_48h") {
    dt <- dt[!is.na(admission_time) & !is.na(orin_time)]
    dt[, prev_orout_floor := fifelse(is.na(prev_orout_time), -Inf, prev_orout_time)]
    dt[, start_time := pmax(admission_time, prev_orout_floor, orin_time - 48 * 60, na.rm = TRUE)]
    dt[, end_time := orin_time]
    dt[, prev_orout_floor := NULL]
  } else if (kind == "preop_immediate_7d") {
    dt <- dt[!is.na(admission_time) & !is.na(orin_time)]
    dt[, prev_orout_floor := fifelse(is.na(prev_orout_time), -Inf, prev_orout_time)]
    dt[, start_time := pmax(admission_time, prev_orout_floor, orin_time - 7 * 24 * 60, na.rm = TRUE)]
    dt[, end_time := orin_time]
    dt[, prev_orout_floor := NULL]
  } else {
    stop("Unknown window kind: ", kind)
  }

  dt <- dt[start_time < end_time]
  unique(dt[, .(subject_id, hadm_id, op_id, start_time, end_time)])
}

window_order <- c(
  "preop_immediate_24h",
  "preop_immediate_48h",
  "preop_immediate_7d",
  "preop_current_stay",
  "history_strict"
)

ops_windows <- lapply(window_order, build_ops_window)
names(ops_windows) <- window_order

# ==============================================================================
# 3) Clean medication text and ATC fields
# ==============================================================================
cat("Cleaning medication fields...\n")

setDT(meds_df)
meds_clean <- meds_df[
  !is.na(subject_id) & !is.na(chart_time),
  .(
    subject_id,
    chart_time,
    drug_lower = str_to_lower(str_squish(paste(
      fifelse(is.na(drug_name), "", drug_name),
      fifelse(is.na(drug_name2), "", drug_name2),
      fifelse(is.na(drug_name3), "", drug_name3)
    ))),
    atc_code1 = str_to_upper(fifelse(is.na(atc_code), "", atc_code)),
    atc_code2 = str_to_upper(fifelse(is.na(atc_code2), "", atc_code2)),
    atc_code3 = str_to_upper(fifelse(is.na(atc_code3), "", atc_code3))
  )
]

rm(meds_df)
gc()

# ==============================================================================
# 4) Define medication categories and flag records
# ==============================================================================
cat("Flagging medication categories...\n")

category_defs <- list(
  list(name = "Beta_blockers", atc = "^C07", drug = "metoprolol|bisoprolol|atenolol|propranolol|carvedilol|nebivolol|nadolol|sotalol|labetalol"),
  list(name = "Calcium_channel_blockers", atc = "^C08", drug = "diltiazem|verapamil|amlodipine|nifedipine|felodipine|nicardipine|clevidipine|lacidipine|lercanidipine|isradipine"),
  list(name = "ACE_inhibitors", atc = "^C09A", drug = "benazepril|captopril|enalapril|fosinopril|lisinopril|perindopril|quinapril|ramipril|trandolapril"),
  list(name = "ARBs", atc = "^C09C", drug = "losartan|valsartan|irbesartan|telmisartan|olmesartan|sartan"),
  list(name = "Diuretics", atc = "^C03", drug = "furosemide|torsemide|bumetanide|hydrochlorothiazide|chlorthalidone|indapamide|spironolactone|eplerenone"),
  list(name = "Statins", atc = "^C10AA", drug = "atorvastatin|rosuvastatin|simvastatin|pravastatin|fluvastatin|pitavastatin|statin"),
  list(name = "Antiplatelet_agents", atc = "^B01AC", drug = "aspirin| asa |clopidogrel|prasugrel|ticagrelor|ticlopidine"),
  list(name = "Anticoagulants", atc = "^B01AA|^B01AE|^B01AF|^B01AX", drug = "warfarin|acenocoumarol|rivaroxaban|apixaban|edoxaban|dabigatran|enoxaparin|dalteparin|fondaparinux|heparin"),
  list(name = "Nitrates", atc = "^C01DA", drug = "nitroglycerin|glyceryl trinitrate|isosorbide dinitrate|isosorbide mononitrate"),
  list(name = "Antiarrhythmics", atc = "^C01B", drug = "amiodarone|sotalol|flecainide|propafenone|dofetilide|dronedarone"),
  list(name = "Insulin", atc = "^A10A", drug = "insulin"),
  list(name = "Oral_hypoglycemics", atc = "^A10B", drug = "metformin|glimepiride|gliclazide|glipizide|repaglinide|sitagliptin|linagliptin|vildagliptin|saxagliptin|acarbose|pioglitazone|empagliflozin|dapagliflozin|canagliflozin"),
  list(name = "Systemic_corticosteroids", atc = "^H02A", drug = "prednisone|prednisolone|methylprednisolone|dexamethasone|hydrocortisone"),
  list(name = "Immunosuppressants", atc = "^L04", drug = "cyclosporine|tacrolimus|mycophenolate|azathioprine|sirolimus|everolimus"),
  list(name = "Inhaled_bronchodilators", atc = "^R03AC|^R03BB|^R03AL", drug = "salbutamol|albuterol|formoterol|salmeterol|olodaterol|indacaterol|ipratropium|tiotropium|glycopyrronium"),
  list(name = "Inhaled_corticosteroids", atc = "^R03BA", drug = "budesonide|fluticasone|beclomethasone|mometasone|ciclesonide"),
  list(name = "Opioid_chronic_use", atc = "^N02A", drug = "morphine|oxycodone|hydrocodone|fentanyl|tramadol|buprenorphine|hydromorphone"),
  list(name = "Proton_pump_inhibitors", atc = "^A02BC", drug = "omeprazole|pantoprazole|esomeprazole|lansoprazole|rabeprazole"),
  list(name = "Antidepressants", atc = "^N06A", drug = "fluoxetine|sertraline|citalopram|escitalopram|paroxetine|venlafaxine|duloxetine|mirtazapine|amitriptyline|imipramine"),
  list(name = "Antipsychotics", atc = "^N05A", drug = "haloperidol|risperidone|olanzapine|quetiapine|aripiprazole|clozapine|ziprasidone"),
  list(name = "Antibiotics_systemic", atc = "^J01", drug = "amoxicillin|ampicillin|cefazolin|ceftriaxone|cefepime|piperacillin|tazobactam|vancomycin|ciprofloxacin|levofloxacin|azithromycin|clarithromycin|metronidazole"),
  list(name = "Thyroid_medications", atc = "^H03A|^H03B", drug = "levothyroxine|thyroxine|liothyronine|propylthiouracil|methimazole"),
  list(name = "NSAIDs", atc = "^M01A", drug = "ibuprofen|diclofenac|ketorolac|naproxen|celecoxib|etoricoxib"),
  list(name = "Antiemetics", atc = "^A04A", drug = "ondansetron|granisetron|palonosetron|metoclopramide|domperidone"),
  list(name = "Mucolytics_expectorants", atc = "^R05CA|^R05CB", drug = "ambroxol|acetylcysteine|bromhexine|guaifenesin|carbocysteine"),
  list(name = "Antihistamines", atc = "^R06", drug = "cetirizine|levocetirizine|loratadine|desloratadine|fexofenadine|chlorpheniramine|diphenhydramine"),
  list(name = "H2_blockers", atc = "^A02BA", drug = "famotidine|cimetidine|ranitidine|nizatidine"),
  list(name = "Laxatives", atc = "^A06A", drug = "bisacodyl|lactulose|macrogol|polyethylene glycol|senna|sennoside|sodium picosulfate"),
  list(name = "GI_prokinetics", atc = "^A03FA", drug = "itopride|mosapride|domperidone|cisapride"),
  list(name = "Benzodiazepines_sedatives", atc = "^N05BA|^N05CD|^N05CF", drug = "diazepam|lorazepam|clonazepam|alprazolam|midazolam|zolpidem|zopiclone|eszopiclone"),
  list(name = "Gabapentinoids", atc = "^N03AX", drug = "gabapentin|pregabalin"),
  list(name = "Antiepileptics", atc = "^N03A", drug = "valproate|valproic acid|carbamazepine|oxcarbazepine|phenytoin|levetiracetam|lamotrigine|topiramate"),
  list(name = "Osteoporosis_medications", atc = "^M05B", drug = "alendronate|risedronate|ibandronate|zoledronic acid|denosumab"),
  list(name = "Vitamin_D_Calcium", atc = "^A11CC|^A12A", drug = "cholecalciferol|ergocalciferol|calcitriol|alfacalcidol|calcium carbonate|calcium citrate"),
  list(name = "Smoking_cessation_drugs", atc = "^N07BA|^N07BB", drug = "varenicline|bupropion|nicotine patch|nicotine gum|nicotine lozenge")
)

category_cols <- vapply(category_defs, function(x) x$name, character(1))

for (def in category_defs) {
  meds_clean[, (def$name) := as.integer(
    str_detect(atc_code1, def$atc) |
      str_detect(atc_code2, def$atc) |
      str_detect(atc_code3, def$atc) |
      str_detect(drug_lower, def$drug)
  )]
}

meds_flags <- meds_clean[, c("subject_id", "chart_time", category_cols), with = FALSE]
setkey(meds_flags, subject_id, chart_time)

rm(meds_clean)
gc()

# ==============================================================================
# 5) Aggregate flags under each window definition
# ==============================================================================
cat("Aggregating windows (24h / 48h / 7d / current_stay / history)...\n")

aggregate_window <- function(window_name, window_ops) {
  cat("  - processing:", window_name, "\n")

  agg <- meds_flags[
    window_ops,
    on = .(subject_id, chart_time >= start_time, chart_time < end_time),
    nomatch = NA,
    c(
      list(subject_id_out = i.subject_id, hadm_id = i.hadm_id, op_id = i.op_id),
      lapply(.SD, function(v) as.integer(any(v == 1L, na.rm = TRUE)))
    ),
    by = .EACHI,
    .SDcols = category_cols
  ]

  keep_cols <- c("subject_id_out", "hadm_id", "op_id", category_cols)
  agg <- agg[, ..keep_cols]
  setnames(agg, "subject_id_out", "subject_id")
  setorderv(agg, c("subject_id", "hadm_id", "op_id"))
  unique(agg, by = c("subject_id", "hadm_id", "op_id"))
}

window_results <- mapply(
  FUN = aggregate_window,
  window_name = names(ops_windows),
  window_ops = ops_windows,
  SIMPLIFY = FALSE
)

# ==============================================================================
# 6) Save outputs
# ==============================================================================
output_files <- list(
  preop_immediate_24h = "preop_meds_immediate_24h.csv",
  preop_immediate_48h = "preop_meds_immediate_48h.csv",
  preop_immediate_7d = "preop_meds_immediate_7d.csv",
  preop_current_stay = "preop_meds_current_stay.csv",
  history_strict = "preop_meds_history_strict.csv"
)

for (nm in names(output_files)) {
  fwrite(window_results[[nm]], file.path(path_output, output_files[[nm]]))
}

# Keep backward-compatible primary output as the recommended main window (48h).
fwrite(window_results[["preop_immediate_48h"]], file.path(path_output, "preop_meds.csv"))

# ==============================================================================
# 7) Summaries and window comparison
# ==============================================================================
calc_med_summary <- function(dt, window_name) {
  dt_sum <- dt[, lapply(.SD, function(v) sum(v, na.rm = TRUE)), .SDcols = category_cols]
  out <- melt(dt_sum, measure.vars = category_cols, variable.name = "Medication_Var", value.name = "n_cases")
  out[, `:=`(
    total_ops = nrow(dt),
    prevalence_pct = round(100 * n_cases / nrow(dt), 2),
    window = window_name
  )]
  out[]
}

window_summary_long <- rbindlist(Map(
  f = calc_med_summary,
  dt = window_results,
  window_name = names(window_results)
), use.names = TRUE)

summary_main <- window_summary_long[window == "preop_immediate_48h"][
  order(-prevalence_pct),
  .(Medication_Label = str_to_title(str_replace_all(Medication_Var, "_", " ")),
    Medication_Var, n_cases, total_ops, prevalence_pct)
]
fwrite(summary_main, file.path(path_output, "preop_meds_summary_stats.csv"))

window_any_summary <- rbindlist(lapply(names(window_results), function(w) {
  dt <- copy(window_results[[w]])
  dt[, any_med := as.integer(rowSums(.SD) > 0), .SDcols = category_cols]
  data.table(
    window = w,
    n_ops = nrow(dt),
    any_med_n = sum(dt$any_med, na.rm = TRUE),
    any_med_pct = round(100 * mean(dt$any_med, na.rm = TRUE), 2)
  )
}))

window_compare <- dcast(
  window_summary_long,
  Medication_Var + n_cases + total_ops ~ window,
  value.var = "prevalence_pct"
)

# Keep a clean prevalence matrix plus delta vs 48h.
window_prev <- dcast(
  window_summary_long,
  Medication_Var ~ window,
  value.var = "prevalence_pct"
)

for (nm in c("preop_immediate_24h", "preop_immediate_7d", "preop_current_stay", "history_strict")) {
  window_prev[, (paste0("delta_vs_48h_", nm)) := get(nm) - preop_immediate_48h]
}

setorder(window_prev, -preop_immediate_48h)

fwrite(window_any_summary, file.path(path_output, "preop_meds_window_any_summary.csv"))
fwrite(window_prev, file.path(path_output, "preop_meds_window_category_prevalence.csv"))
fwrite(window_compare, file.path(path_output, "preop_meds_window_category_long.csv"))

cat("\n=== Main window: preop_immediate_48h (Top 20) ===\n")
print(head(summary_main, 20))
cat("\nSaved files to:", path_output, "\n")
cat("Done!\n")
