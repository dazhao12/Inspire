suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_pump_vars_only.csv")
output_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed.csv")
summary_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_summary.csv")
op_list_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_op_list.csv")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

id_cols <- c("subject_id", "hadm_id", "op_id")
pump_vars <- c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi", "ppfi", "rfti")

header_cols <- names(fread(input_file, nrows = 0, showProgress = FALSE))
present_pump_vars <- intersect(pump_vars, header_cols)
missing_pump_vars <- setdiff(pump_vars, present_pump_vars)
if (length(present_pump_vars) == 0L) {
  stop("No requested pump variables found in input file.")
}

dt <- fread(input_file, showProgress = TRUE)
dt[, any_pump_exposure_row := Reduce(`|`, lapply(.SD, function(x) !is.na(x) & x > 0)), .SDcols = present_pump_vars]
exposed_ops <- unique(dt[any_pump_exposure_row == TRUE, ..id_cols])
setorder(exposed_ops, subject_id, hadm_id, op_id)

subset_dt <- dt[op_id %in% exposed_ops$op_id][, any_pump_exposure_row := NULL]
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
  kept_pump_vars = paste(present_pump_vars, collapse = ";"),
  missing_requested_pump_vars = if (length(missing_pump_vars)) paste(missing_pump_vars, collapse = ";") else "",
  op_pct_retained = round(100 * uniqueN(subset_dt$op_id) / uniqueN(dt$op_id), 2),
  row_pct_retained = round(100 * nrow(subset_dt) / nrow(dt), 2)
)
fwrite(summary_dt, summary_file)

cat("Pump-vars-only exposed subset written to:\n", output_file, "\n", sep = "")
cat("Operation list written to:\n", op_list_file, "\n", sep = "")
cat("Summary written to:\n", summary_file, "\n", sep = "")
