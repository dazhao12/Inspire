suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_full_complete_grouped_columns.csv")
output_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_raw.csv")
summary_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_summary.csv")
op_list_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_op_list.csv")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

id_time_cols <- c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry")
pump_vars <- c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi", "ppfi", "rfti")
gas_vars <- c("etgas", "etsevo", "etdes", "etiso", "n2o", "o2", "air", "fio2")

header_cols <- names(fread(input_file, nrows = 0, showProgress = FALSE))
keep_cols <- intersect(c(id_time_cols, pump_vars, gas_vars), header_cols)
exposure_vars <- intersect(c(pump_vars, gas_vars), header_cols)

missing_requested_cols <- setdiff(c(id_time_cols, pump_vars, gas_vars), keep_cols)
if (length(exposure_vars) == 0L) {
  stop("No requested exposure variables found in input file.")
}

dt <- fread(input_file, select = keep_cols, showProgress = TRUE)
dt[, any_exposure_row := Reduce(`|`, lapply(.SD, function(x) !is.na(x) & x > 0)), .SDcols = exposure_vars]
exposed_ops <- unique(dt[any_exposure_row == TRUE, .(subject_id, hadm_id, op_id)])
setorder(exposed_ops, subject_id, hadm_id, op_id)

subset_dt <- dt[op_id %in% exposed_ops$op_id][, any_exposure_row := NULL]
setcolorder(subset_dt, keep_cols)
setorder(subset_dt, subject_id, hadm_id, surgery_number, chart_time, op_id)

fwrite(subset_dt, output_file)
fwrite(exposed_ops, op_list_file)

summary_dt <- data.table(
  source_file = basename(input_file),
  output_file = basename(output_file),
  n_rows_source = nrow(dt),
  n_rows_output = nrow(subset_dt),
  n_subjects_output = uniqueN(subset_dt$subject_id),
  n_hadm_output = uniqueN(subset_dt$hadm_id),
  n_ops_output = uniqueN(subset_dt$op_id),
  kept_columns = paste(keep_cols, collapse = ";"),
  exposure_variables = paste(exposure_vars, collapse = ";"),
  missing_requested_columns = if (length(missing_requested_cols)) paste(missing_requested_cols, collapse = ";") else "",
  op_pct_retained = round(100 * uniqueN(subset_dt$op_id) / uniqueN(dt$op_id), 2),
  row_pct_retained = round(100 * nrow(subset_dt) / nrow(dt), 2)
)
fwrite(summary_dt, summary_file)

cat("Pump + gas exposed raw subset written to:\n", output_file, "\n", sep = "")
cat("Operation list written to:\n", op_list_file, "\n", sep = "")
cat("Summary written to:\n", summary_file, "\n", sep = "")
