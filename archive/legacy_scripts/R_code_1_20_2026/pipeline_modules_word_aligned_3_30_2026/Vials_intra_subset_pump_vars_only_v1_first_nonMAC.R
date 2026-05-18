suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_full_complete_grouped_columns.csv")
output_file <- file.path(processed_path, "vital_intraop_pump_vars_only.csv")
summary_file <- file.path(processed_path, "vital_intraop_pump_vars_only_summary.csv")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

id_time_cols <- c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry")
pump_vars <- c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi", "ppfi", "rfti")

header_cols <- names(fread(input_file, nrows = 0, showProgress = FALSE))
keep_cols <- intersect(c(id_time_cols, pump_vars), header_cols)
missing_pump_vars <- setdiff(pump_vars, keep_cols)

dt <- fread(input_file, select = keep_cols, showProgress = TRUE)
setcolorder(dt, keep_cols)
fwrite(dt, output_file)

summary_dt <- data.table(
  source_file = basename(input_file),
  output_file = basename(output_file),
  n_rows = nrow(dt),
  n_subjects = uniqueN(dt$subject_id),
  n_hadm = uniqueN(dt$hadm_id),
  n_ops = uniqueN(dt$op_id),
  kept_columns = paste(keep_cols, collapse = ";"),
  missing_requested_pump_vars = if (length(missing_pump_vars)) paste(missing_pump_vars, collapse = ";") else ""
)
fwrite(summary_dt, summary_file)

cat("Pump-vars-only file written to:\n", output_file, "\n", sep = "")
cat("Summary written to:\n", summary_file, "\n", sep = "")
