library(data.table)

key_cols <- c("subject_id", "hadm_id", "op_id")

output_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/First_nonMAC_Master_Table_3_31_2026"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source_defs <- list(
  list(
    name = "demographics_operation",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Demographics_Timeline_first_nonMAC_3_30_2026/Demographic_Operation_Level.csv",
    type = "base"
  ),
  list(
    name = "timeline",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Demographics_Timeline_first_nonMAC_3_30_2026/Time_Related_Data.csv"
  ),
  list(
    name = "comorbidity",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Diagnosis_word_comorbidities_first_nonMAC_3_30_2026/comorbidity_word_defined_anchor_first_nonMAC.csv"
  ),
  list(
    name = "acute_status",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Acute_Status_3mo_first_nonMAC_sepsis_A40_A41_3_30_2026/acute_status_3mo_before_orin_first_nonMAC.csv"
  ),
  list(
    name = "preop_meds",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Meds_Preop_word_first_nonMAC_3_30_2026/preop_meds_word_defined_first_nonMAC.csv"
  ),
  list(
    name = "preop_vitals",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_pro_first_nonMAC_3_30_2026/preop_baseline_final.csv"
  ),
  list(
    name = "intraop_totals",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026/drugs_fluids_total_sum.csv"
  ),
  list(
    name = "outcomes",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Outcomes_word_complications_first_nonMAC_sepsis_A40_A41_3_30_2026/postop_complications_word_defined_first_nonMAC.csv"
  ),
  list(
    name = "preop_labs_current_stay",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/preop_labs_features_current_stay.csv"
  ),
  list(
    name = "preop_labs_attributable_7d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/preop_labs_features_attributable_7d.csv"
  ),
  list(
    name = "preop_labs_attributable_15d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/preop_labs_features_attributable_15d.csv"
  ),
  list(
    name = "preop_labs_attributable_30d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/preop_labs_features_attributable_30d.csv"
  ),
  list(
    name = "preop_labs_attributable_60d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/preop_labs_features_attributable_60d.csv"
  ),
  list(
    name = "preop_labs_cumulative_preop",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/preop_labs_features_cumulative_preop.csv"
  ),
  list(
    name = "postop_labs_current_stay",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/postop_labs_features_current_stay.csv"
  ),
  list(
    name = "postop_labs_attributable_7d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/postop_labs_features_attributable_7d.csv"
  ),
  list(
    name = "postop_labs_attributable_15d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/postop_labs_features_attributable_15d.csv"
  ),
  list(
    name = "postop_labs_attributable_30d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/postop_labs_features_attributable_30d.csv"
  ),
  list(
    name = "postop_labs_attributable_60d",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/postop_labs_features_attributable_60d.csv"
  ),
  list(
    name = "postop_labs_cumulative_postop",
    path = "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/lab_data_first_nonMAC_attributable_windows_3_31_2026/postop_labs_features_cumulative_postop.csv"
  )
)

read_dt <- function(path) {
  fread(path)
}

check_unique_keys <- function(dt, source_name) {
  dup_n <- dt[, .N, by = key_cols][N > 1, .N]
  if (dup_n > 0) {
    stop(sprintf("%s has %s duplicated key rows", source_name, dup_n))
  }
}

normalize_join_table <- function(dt, existing_names, source_name, is_base = FALSE) {
  keep_cols <- names(dt)
  overlap_cols <- setdiff(intersect(keep_cols, existing_names), key_cols)
  if (length(overlap_cols) > 0 && !is_base) {
    rename_map <- paste0(source_name, "__", overlap_cols)
    setnames(dt, overlap_cols, rename_map)
  }
  dt
}

manifest <- rbindlist(lapply(source_defs, function(x) {
  data.table(source_name = x$name, path = x$path, file_exists = file.exists(x$path))
}))

if (!all(manifest$file_exists)) {
  stop("Some input source files are missing.")
}

base_def <- source_defs[[1]]
master_dt <- read_dt(base_def$path)
check_unique_keys(master_dt, base_def$name)
manifest[source_name == base_def$name, `:=`(rows = nrow(master_dt), cols = ncol(master_dt), duplicated_keys = 0L)]

for (src in source_defs[-1]) {
  dt <- read_dt(src$path)
  check_unique_keys(dt, src$name)
  manifest[source_name == src$name, `:=`(rows = nrow(dt), cols = ncol(dt), duplicated_keys = 0L)]
  dt <- normalize_join_table(dt, names(master_dt), src$name)
  master_dt <- merge(master_dt, dt, by = key_cols, all.x = TRUE, sort = FALSE)
}

setcolorder(master_dt, c(key_cols, setdiff(names(master_dt), key_cols)))

source_column_map <- rbindlist(lapply(source_defs, function(src) {
  dt <- fread(src$path, nrows = 0)
  original_cols <- names(dt)
  out_cols <- original_cols
  if (src$name != base_def$name) {
    overlap_with_base <- setdiff(intersect(original_cols, names(fread(base_def$path, nrows = 0))), key_cols)
    overlap_with_prev <- setdiff(original_cols, key_cols)
    out_cols <- ifelse(
      original_cols %in% key_cols,
      original_cols,
      ifelse(original_cols %in% intersect(original_cols, names(master_dt)) & !(original_cols %in% names(fread(base_def$path, nrows = 0))), original_cols, original_cols)
    )
  }
  data.table(source_name = src$name, original_column = original_cols)
}), fill = TRUE)

final_column_source <- rbindlist(lapply(source_defs, function(src) {
  dt <- fread(src$path, nrows = 0)
  cols <- names(dt)
  data.table(
    source_name = src$name,
    output_column = if (src$name == base_def$name) cols else c(key_cols, setdiff(names(normalize_join_table(copy(dt), names(fread(base_def$path, nrows = 0)), src$name)), key_cols))
  )
}), fill = TRUE)

final_column_source <- unique(final_column_source)
final_column_source <- final_column_source[output_column %in% names(master_dt)]

summary_dt <- data.table(
  metric = c("rows", "columns", "unique_subjects", "unique_hadm", "unique_ops"),
  value = c(
    nrow(master_dt),
    ncol(master_dt),
    uniqueN(master_dt$subject_id),
    uniqueN(master_dt$hadm_id),
    uniqueN(master_dt$op_id)
  )
)

notes_dt <- data.table(
  note = c(
    "Master table keeps one row per subject_id + hadm_id + op_id.",
    "Time-series and long-format tables were excluded, including vital_intraop_full_complete.csv and postop_vitals_long*.csv.",
    "Single-row feature tables were merged by subject_id + hadm_id + op_id using left joins from Demographic_Operation_Level as the base.",
    "If a non-key column name already existed in the growing master table, the incoming column was renamed with a source_name__ prefix to avoid overwriting."
  )
)

fwrite(master_dt, file.path(output_dir, "first_nonMAC_master_table.csv"))
saveRDS(master_dt, file.path(output_dir, "first_nonMAC_master_table.rds"))
fwrite(manifest, file.path(output_dir, "first_nonMAC_master_table_source_manifest.csv"))
fwrite(final_column_source, file.path(output_dir, "first_nonMAC_master_table_column_source.csv"))
fwrite(summary_dt, file.path(output_dir, "first_nonMAC_master_table_summary.csv"))
fwrite(notes_dt, file.path(output_dir, "first_nonMAC_master_table_notes.csv"))

cat("Wrote master table to:", output_dir, "\n")
