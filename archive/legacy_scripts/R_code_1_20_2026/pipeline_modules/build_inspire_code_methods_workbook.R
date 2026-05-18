suppressPackageStartupMessages({
  library(data.table)
})

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required to build the workbook.")
}

project_root <- "/N/project/analgesia_perioperation"
project_dir <- file.path(project_root, "projects", "Inspire_data_process_ZZ")
docs_dir <- file.path(project_dir, "docs")
workbook_path <- file.path(docs_dir, "INSPIRE_code_methods_workbook.xlsx")

if (!dir.exists(docs_dir)) {
  dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
}

project_layout <- data.table(
  folder = c(
    "R_code_1_20_2026/pipeline_modules",
    "docs",
    "R_code_1_20_2026/pipeline_modules",
    "R_code_1_20_2026/analysis_addons",
    "intermediate",
    "archive_legacy_data"
  ),
  purpose = c(
    "Current pipeline entry points and helper scripts",
    "Code-method documentation workbook and project notes",
    "Current R modules used by the canonical INSPIRE pipeline",
    "Useful analysis scripts that are not part of the canonical pipeline",
    "Intermediate module outputs used to assemble final processed data",
    "Historical processed outputs kept only for compatibility reruns"
  ),
  use_now = c("yes", "yes", "yes", "optional", "check only", "archive only")
)

pipeline_overview <- data.table(
  step_order = 1:8,
  module_name = c(
    "Demographics and timeline",
    "Preop diagnosis flags",
    "Preop labs",
    "Preop medications",
    "Preop vitals baseline",
    "Intraop vitals and fluids",
    "Postop outcomes",
    "Canonical merge"
  ),
  script = c(
    "R_code_1_20_2026/pipeline_modules/Process_Demographics_and_Timeline_v1.R",
    "R_code_1_20_2026/pipeline_modules/Diagnosis_v1_1_20_2026.R",
    "R_code_1_20_2026/pipeline_modules/Lab_v1_1_20_2026.R",
    "R_code_1_20_2026/pipeline_modules/Medicine_pro_v1_1_20_2026.R",
    "R_code_1_20_2026/pipeline_modules/Vials_pro_v1_1_20_2026.R",
    "R_code_1_20_2026/pipeline_modules/Vials_intra_v1_1_21_2026.R + Vials_intra_summary_v1_1_21_2026.R",
    "R_code_1_20_2026/pipeline_modules/Outcome_1_20_2026.R",
    "R_code_1_20_2026/pipeline_modules/run_inspire_pipeline.R"
  ),
  main_raw_inputs = c(
    "operations.csv",
    "operations.csv + diagnosis.csv",
    "operations.csv + labs.csv",
    "operations.csv + medications.csv",
    "operations.csv + ward_vitals.csv + vitals.csv",
    "operations.csv + vitals.csv",
    "operations.csv + diagnosis.csv + labs.csv",
    "intermediate/current/*.csv"
  ),
  row_level = c(
    "subject-level and op-level",
    "one row per op_id",
    "one row per op_id",
    "one row per op_id",
    "one row per op_id",
    "time-series wide table and one row per op_id sum table",
    "one row per op_id",
    "one row per op_id"
  ),
  key_window = c(
    "all available operations; timeline uses raw admission/OR/discharge times",
    "strict preop: diagnosis chart_time <= 0 in source workflow",
    "preop only; any, 30d, and 7d windows before orin_time",
    "admission_time to orin_time",
    "ward: last 24h before orin_time; OR baseline: last 120 min before orin_time",
    "orin_time to orout_time",
    "postop ICD: after orin_time to discharge; AKI: baseline preop and postop through min(discharge, orout+7d); mortality relative to current admission/surgery",
    "left join baseline + diagnosis + labs30d + meds + preop vitals + intraop sum + outcomes"
  ),
  main_outputs = c(
    "Demographic_Subject_Level.csv; Demographic_Operation_Level.csv; Time_Related_Data.csv",
    "diag_preop_flags_final.csv",
    "preop_labs_features_any.csv(current-stay preop); preop_labs_features_30d.csv; preop_labs_features_7d.csv",
    "preop_meds.csv",
    "preop_baseline_final.csv",
    "vital_intraop_full_complete.csv; drugs_fluids_total_sum.csv; drugs_fluids_descriptive_stats.csv",
    "postop_outcomes_final.csv; postop_outcomes_summary.csv",
    "periop_baseline_operations_core_plus_timeline.csv; periop_master_dataset_all_features.csv; final processed csv files"
  )
)

external_summary <- data.table(
  section = c(
    "Purpose",
    "Source data",
    "Analysis unit",
    "Pipeline order",
    "Demographics and timeline",
    "Diagnosis variables",
    "Laboratory features",
    "Medication screening",
    "Preoperative vitals",
    "Intraoperative variables",
    "Postoperative outcomes",
    "Final processed outputs",
    "Interpretation notes"
  ),
  content = c(
    "Summarizes how the INSPIRE processed analysis dataset is built from raw source tables for collaborator-facing explanation.",
    "Canonical workflow uses operations.csv, diagnosis.csv, labs.csv, medications.csv, ward_vitals.csv, and vitals.csv.",
    "Main analysis level is one operation per row using op_id; subject_id is retained for patient linkage and subject-level summaries.",
    "Demographics/timeline -> diagnosis -> labs -> medications -> preop vitals -> intraop -> outcomes -> final merge.",
    "Creates subject-level demographics, operation-level baseline fields, derived durations, and timeline QC flags from operations.csv.",
    "Links diagnosis to operations by subject_id, keeps strict preop records, cleans ICD to 3-character prefixes, and aggregates broad comorbidity flags to one row per op_id.",
    "Links labs by subject_id, keeps preop labs before orin_time, and creates nearest/median/mean features in any, 30-day, and 7-day windows.",
    "Screens medications between admission_time and orin_time using both ATC-code patterns and medication-name keywords; aggregates category flags to one row per op_id.",
    "Builds baseline vitals from ward values in the last 24h before surgery and OR induction values in the last 120 min before orin_time, prioritizing ward values when both exist.",
    "Builds a full intraoperative time-series table and an operation-level summed exposure table for drugs, fluids, blood products, blood loss, and urine output between orin_time and orout_time.",
    "Defines ICD-based postoperative complications within the current stay, AKI using KDIGO creatinine rules, and mortality using admission/discharge/death timing.",
    "Final processed data are written to data/INSPIRE_1.3/processed/, including periop_baseline_operations_core_plus_timeline.csv and periop_master_dataset_all_features.csv.",
    "Binary indicators from diagnosis/medication/outcome modules are commonly filled with 0 after operation-level aggregation; continuous variables such as labs and vital signs are not implicitly zero-filled."
  )
)

demog_timeline <- data.table(
  component = c(
    "Subject-level demographics",
    "Operation-level demographics",
    "BMI",
    "Sex coding",
    "Timeline durations",
    "Timeline QC flags"
  ),
  rule = c(
    "Group by subject_id; Age/Height/Weight/BMI use mean across operations; Male and race use first non-missing value",
    "Keep one row per op_id with subject_id, op_id, hadm_id, case_id, opdate, demographics, ASA, department, antype, icd10_pcs",
    "BMI = weight / (height/100)^2 when height > 0",
    "Male = 1 if sex == M; 0 if sex == F; else NA",
    "Compute op_duration, anesthesia_duration, OR room time, CPB duration, hospital LOS, ICU LOS, time-to-death in minutes and days",
    "flag_los_error, flag_op_time_error, flag_death_before_admission, flag_overlap_with_prev"
  ),
  source_script = "Process_Demographics_and_Timeline_v1.R"
)

diagnosis_flags <- data.table(
  variable = c(
    "smoking", "drinking", "hypertension", "diabetes", "diabetes_any", "cerebrovascular_disease",
    "dementia", "hemiplegia_paraplegia", "myocardial_infarction", "angina",
    "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", "copd",
    "asthma", "ards", "renal_disease", "liver_disease", "peptic_ulcer_disease",
    "connective_tissue_disease", "peripheral_vascular_disease", "anemia",
    "malignancy", "metastatic_solid_tumor", "hiv", "aids", "aids_opportunistic_raw", "hiv_aids"
  ),
  icd_rule = c(
    "icd3 in Z72, F17",
    "icd3 in F10, K70",
    "icd3 matches ^I1[0-6] or ^O1[0-6]",
    "icd3 matches ^E(08|09|10|11|13|14)$",
    "icd3 matches ^E(08|09|10|11|13|14)$ (ICD3 proxy main diabetes definition)",
    "icd3 matches ^I6[0-9] or == G45",
    "icd3 in F00, F01, F02, F03, G30",
    "icd3 in G80, G81, G82, G83, G04, G11",
    "icd3 in I21, I22",
    "icd3 == I20",
    "icd3 == I48",
    "icd3 matches ^I2[0-5]",
    "icd3 matches ^I4[7-9]",
    "icd3 == J44",
    "icd3 == J45",
    "icd3 == J80",
    "icd3 in N18, N19, I12, I13, Z49, Z94, Z99",
    "icd3 in B18, K70, K73, K74",
    "icd3 matches ^K2[5-8]",
    "icd3 in M05, M06, M32, M33, M34, M31, M35",
    "icd3 in I70, I71, I73, K55",
    "icd3 matches ^D5[0-9] or ^D6[0-4]",
    "Cancer ICD3 blocks except C77-C80 metastasis codes",
    "icd3 in C77, C78, C79, C80",
    "hiv_any=1 and aids=0 (where hiv_any is icd3 in B20-B24)",
    "hiv_any=1 and aids_opportunistic_raw=1 within same window",
    "icd3 in AIDS opportunistic proxy set: B37/C53/B38/B45/A07/B25/G93/B00/B39/C46/C81-C96/A31/A15-A19/B59/Z87/A81/A02/B58/R64",
    "Compatibility field: hiv_any (icd3 in B20-B24)"
  ),
  aggregation = "Within each op_id, use max(hit) across matched diagnosis rows; fill missing with 0",
  source_script = "Diagnosis_v1_1_20_2026.R"
)

labs_windows <- data.table(
  window_name = c("any_preop", "preop_30d", "preop_7d"),
  window_rule = c(
    "chart_time <= orin_time",
    "chart_time between orin_time - 30*24*60 and orin_time",
    "chart_time between orin_time - 7*24*60 and orin_time"
  ),
  features = c(
    "nearest, median, mean",
    "nearest, median, mean",
    "nearest, median, mean"
  ),
  naming_rule = c(
    "preop_<item>_nearest / _median / _mean",
    "preop_<item>_nearest / _median / _mean",
    "preop_<item>_nearest / _median / _mean"
  ),
  row_level = "one row per op_id",
  source_script = "Lab_v1_1_20_2026.R"
)

lab_items <- data.table(
  item_name = c(
    "glucose", "creatinine", "hct", "potassium", "sodium", "hb", "wbc", "platelet",
    "chloride", "lymphocyte", "seg", "bun", "calcium", "phosphorus", "albumin",
    "total_bilirubin", "alt", "ast", "total_protein", "alp", "crp", "sao2", "hco3",
    "ptinr", "ph", "pao2", "paco2", "aptt", "ica", "fibrinogen", "be", "lacate",
    "ckmb", "ck", "troponin_i", "hba1c", "troponin_t", "d_dimer"
  ),
  source_script = "Lab_v1_1_20_2026.R"
)

med_categories <- data.table(
  category = c(
    "Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics",
    "Statins", "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics",
    "Insulin", "Oral_hypoglycemics", "Systemic_corticosteroids", "Immunosuppressants",
    "Inhaled_bronchodilators", "Inhaled_corticosteroids", "Opioid_chronic_use",
    "Proton_pump_inhibitors", "Antidepressants", "Antipsychotics",
    "Antibiotics_systemic", "Thyroid_medications", "NSAIDs", "Antiemetics",
    "Mucolytics_expectorants", "Antihistamines", "H2_blockers", "Laxatives",
    "GI_prokinetics", "Benzodiazepines_sedatives", "Gabapentinoids",
    "Antiepileptics", "Osteoporosis_medications", "Vitamin_D_Calcium",
    "Smoking_cessation_drugs"
  ),
  atc_pattern = c(
    "^C07", "^C08", "^C09A", "^C09C", "^C03",
    "^C10AA", "^B01AC", "^B01AA|^B01AE|^B01AF|^B01AX", "^C01DA", "^C01B",
    "^A10A", "^A10B", "^H02A", "^L04",
    "^R03AC|^R03BB|^R03AL", "^R03BA", "^N02A",
    "^A02BC", "^N06A", "^N05A",
    "^J01", "^H03A|^H03B", "^M01A", "^A04A",
    "^R05CA|^R05CB", "^R06", "^A02BA", "^A06A",
    "^A03FA", "^N05BA|^N05CD|^N05CF", "^N03AX",
    "^N03A", "^M05B", "^A11CC|^A12A",
    "^N07BA|^N07BB"
  ),
  keyword_examples = c(
    "metoprolol, bisoprolol, carvedilol",
    "diltiazem, verapamil, amlodipine",
    "benazepril, captopril, lisinopril",
    "losartan, valsartan, telmisartan",
    "furosemide, hydrochlorothiazide, spironolactone",
    "atorvastatin, rosuvastatin, simvastatin",
    "aspirin, clopidogrel, ticagrelor",
    "warfarin, rivaroxaban, apixaban, heparin",
    "nitroglycerin, isosorbide",
    "amiodarone, flecainide, dronedarone",
    "insulin",
    "metformin, sitagliptin, empagliflozin",
    "prednisone, dexamethasone, hydrocortisone",
    "cyclosporine, tacrolimus, mycophenolate",
    "salbutamol, tiotropium, ipratropium",
    "budesonide, fluticasone",
    "morphine, oxycodone, fentanyl",
    "omeprazole, pantoprazole",
    "fluoxetine, sertraline, duloxetine",
    "haloperidol, risperidone, quetiapine",
    "cefazolin, vancomycin, levofloxacin",
    "levothyroxine, methimazole",
    "ibuprofen, diclofenac, celecoxib",
    "ondansetron, metoclopramide",
    "ambroxol, acetylcysteine",
    "cetirizine, loratadine, diphenhydramine",
    "famotidine, cimetidine",
    "bisacodyl, lactulose, senna",
    "itopride, mosapride, domperidone",
    "diazepam, lorazepam, zolpidem",
    "gabapentin, pregabalin",
    "valproate, carbamazepine, levetiracetam",
    "alendronate, denosumab",
    "cholecalciferol, calcium carbonate",
    "varenicline, bupropion, nicotine patch"
  ),
  exposure_window = "Medication chart_time between admission_time and orin_time",
  aggregation = "Within each subject_id + op_id, use max(flag); final left join to all ops and fill missing with 0",
  source_script = "Medicine_pro_v1_1_20_2026.R"
)

preop_vitals <- data.table(
  component = c(
    "Ward baseline window",
    "OR induction window",
    "Item harmonization",
    "Final preop variables",
    "Source tracking"
  ),
  rule = c(
    "ward_vitals joined by subject_id; keep values from admission_time to orin_time and additionally within last 1440 min before orin_time; mean by op_id and item",
    "vitals joined by op_id; keep values from last 120 min before orin_time; mean by op_id and grouped item",
    "Map nibp/art pairs into sbp, dbp, mbp; keep hr, spo2, rr, bt",
    "preop_sbp/dbp/mbp/hr/spo2/rr/bt = ward value first, else OR induction value",
    "source_sbp indicates Ward, OR_Induction, or Missing"
  ),
  source_script = "Vials_pro_v1_1_20_2026.R"
)

intraop_logic <- data.table(
  component = c(
    "Intraop time-series extraction",
    "Intraop time window",
    "Time-series row definition",
    "Total-sum aggregation",
    "Summary stats for totals"
  ),
  rule = c(
    "Read vitals.csv and keep predefined target_items including physiologic signals, anesthetics, vasoactives, fluids, blood products, blood loss, urine output",
    "Keep records with chart_time between orin_time and orout_time",
    "Wide table indexed by subject_id + op_id + surgery_number + chart_time + min_from_entry",
    "For existing sum columns, sum within op_id and merge back unique subject_id + surgery_number",
    "For each summed variable, report n users, percent users, mean(SD) in all ops, median[IQR] among users"
  ),
  source_script = c(
    "Vials_intra_v1_1_21_2026.R",
    "Vials_intra_v1_1_21_2026.R",
    "Vials_intra_v1_1_21_2026.R",
    "Vials_intra_v1_1_21_2026.R",
    "Vials_intra_summary_v1_1_21_2026.R"
  )
)

outcomes_def <- data.table(
  outcome_group = c(
    "Stroke", "Cognitive_Decline", "Cardiac_Arrest", "Heart_Failure", "Myocardial_Injury",
    "Angina", "Arrhythmia_Vent", "Atrial_Fib", "Resp_Failure", "Pneumonia",
    "Sepsis", "Infection_Organ", "Infection_Unk", "AKI baseline", "AKI postop window",
    "AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "Death_In_Hospital",
    "Death_POD30", "Death_POD90", "Death_1_Year", "Death_Long_Term", "Survival_Days"
  ),
  definition = c(
    "Postop diagnosis ICD in I63, I64, I65, I66 during current stay after surgery",
    "ICD R41 during current stay after surgery",
    "ICD I46 during current stay after surgery",
    "ICD I50, I11, I13 during current stay after surgery",
    "ICD I21 during current stay after surgery",
    "ICD I20 during current stay after surgery",
    "ICD matches ^I47 or ^I49 during current stay after surgery",
    "ICD matches ^I48 during current stay after surgery",
    "ICD matches ^J96 during current stay after surgery",
    "ICD in J18, J15, J17, J12, J16, J13, J09 during current stay after surgery",
    "ICD in A40, A41 during current stay after surgery",
    "ICD in K65, K57, J85, G06 during current stay after surgery",
    "ICD in A49, B34, J22 during current stay after surgery",
    "Baseline creatinine = minimum creatinine from admission_time to before orin_time",
    "Postop creatinine window = from orout_time to min(discharge_time, orout_time + 7 days)",
    "KDIGO stage 1 if delta >= 0.3 within 48h or ratio 1.5 to <2.0",
    "KDIGO stage 2 if ratio 2.0 to <3.0",
    "KDIGO stage 3 if ratio >= 3.0 or creatinine >= 4.0",
    "death_time between admission_time and discharge_time",
    "Survival_Days between 0 and 30",
    "Survival_Days between 0 and 90",
    "Survival_Days between 0 and 365",
    "death_time exists regardless of timing",
    "Round((death_time - anend_time) / 1440, 1)"
  ),
  aggregation = c(
    rep("Within op_id, use max(flag) after postop diagnosis filtering", 13),
    "One baseline value per op_id",
    "Use all postop creatinine values in allowed window",
    "Highest AKI stage within op_id",
    "Highest AKI stage within op_id",
    "Highest AKI stage within op_id",
    "Computed directly per op_id",
    "Computed directly per op_id",
    "Computed directly per op_id",
    "Computed directly per op_id",
    "Computed directly per op_id",
    "Continuous days field"
  ),
  source_script = "Outcome_1_20_2026.R"
)

final_outputs <- data.table(
  processed_file = c(
    "preop_demographics_subject_level.csv",
    "periop_baseline_operations_core_plus_timeline.csv",
    "preop_baseline_operations_core.csv",
    "periop_timeline_operations_raw_and_derived.csv",
    "preop_diagnosis_flags_cumulative_preop.csv",
    "preop_labs_window_7d.csv",
    "preop_labs_window_30d.csv",
    "preop_labs_window_cumulative_preop.csv",
    "preop_medications_flags_current_stay.csv",
    "preop_vitals_baseline.csv",
    "intraop_vitals_timeseries.csv",
    "intraop_drugs_fluids_totals.csv",
    "postop_outcomes.csv",
    "periop_master_dataset_all_features.csv"
  ),
  built_from = c(
    "Demographics_Timeline module",
    "Demographics operation + timeline combined in run_inspire_pipeline.R",
    "Preop subset of periop_baseline_operations_core_plus_timeline",
    "Periop timeline subset of periop_baseline_operations_core_plus_timeline",
    "Diagnosis module",
    "Lab module",
    "Lab module",
    "Lab module",
    "Medication module",
    "Preop vitals module",
    "Intraop module",
    "Intraop module",
    "Outcome module",
    "Baseline + diagnosis + labs30d + meds + preop vitals + intraop sum + outcomes"
  ),
  primary_key = c(
    "subject_id",
    rep("op_id", 13)
  ),
  default_use = c(
    "subject-level demographics only",
    "canonical baseline table",
    "preop-only baseline subset",
    "intraop/postop timeline subset",
    "preop diagnosis covariates",
    "optional lab feature set",
    "default lab feature set in master merge",
    "optional historical lab feature set",
    "preop medication covariates",
    "preop vital covariates",
    "full intraop time series",
    "intraop sum features used in master merge",
    "postop outcomes",
    "main analysis table"
  )
)

wb <- openxlsx::createWorkbook()

add_sheet <- function(sheet_name, dt) {
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = as.data.frame(dt), withFilter = TRUE)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dt), widths = "auto")
}

add_sheet("project_layout", project_layout)
add_sheet("pipeline_overview", pipeline_overview)
add_sheet("external_summary", external_summary)
add_sheet("demog_timeline", demog_timeline)
add_sheet("diagnosis_flags", diagnosis_flags)
add_sheet("labs_windows", labs_windows)
add_sheet("lab_items", lab_items)
add_sheet("med_categories", med_categories)
add_sheet("preop_vitals", preop_vitals)
add_sheet("intraop_logic", intraop_logic)
add_sheet("outcomes_def", outcomes_def)
add_sheet("final_outputs", final_outputs)

openxlsx::saveWorkbook(wb, workbook_path, overwrite = TRUE)
cat(sprintf("Workbook written to %s\n", workbook_path))
