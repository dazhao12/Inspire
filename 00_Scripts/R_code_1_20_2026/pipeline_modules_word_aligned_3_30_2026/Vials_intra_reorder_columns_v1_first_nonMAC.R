suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_full_complete.csv")
output_file <- file.path(processed_path, "vital_intraop_full_complete_grouped_columns.csv")
dictionary_file <- file.path(processed_path, "vital_intraop_full_complete_grouped_columns_dictionary.csv")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

group_definitions <- list(
  id_time = c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry"),
  hemodynamics = c(
    "art_sbp", "art_mbp", "art_dbp",
    "nibp_sbp", "nibp_mbp", "nibp_dbp",
    "pap_sbp", "pap_mbp", "pap_dbp",
    "hr", "cvp", "ci", "svi", "bt"
  ),
  respiratory_ventilation = c(
    "rr", "spo2", "etco2", "fio2", "vt", "minvol",
    "o2", "air", "peep", "pip", "pmean", "pplat", "cbro2"
  ),
  anesthesia_sedation = c(
    "bis", "etgas", "etdes", "etiso", "etsevo", "n2o",
    "ppf", "ppfi", "ftn", "aft", "sft", "rfti", "mdz"
  ),
  vasoactive_drugs = c("epi", "epii", "nepi", "phe", "pepi", "eph", "dopai", "dobui", "mlni", "ntgi", "vaso"),
  fluids_output = c(
    "ebl", "uo",
    "ns", "hns", "hs", "psa", "d5w", "d10w", "d50w", "hes", "alb5", "alb20"
  ),
  blood_products = c("rbc", "ffp", "pc", "pheresis", "cryo"),
  ecg_st_segment = c("sti", "stii", "stiii", "stv5"),
  review_unmapped = c("cpat", "ds")
)

original_cols <- names(fread(input_file, nrows = 0, showProgress = FALSE))
present_groups <- lapply(group_definitions, intersect, y = original_cols)
ordered_cols <- unlist(present_groups, use.names = FALSE)
remaining_cols <- setdiff(original_cols, ordered_cols)
final_cols <- c(ordered_cols, remaining_cols)

column_dictionary <- rbindlist(lapply(names(present_groups), function(group_name) {
  cols <- present_groups[[group_name]]
  if (length(cols) == 0L) return(NULL)
  data.table(
    column_name = cols,
    column_group = group_name,
    group_order = match(group_name, names(present_groups)),
    column_order_within_group = seq_along(cols)
  )
}), use.names = TRUE, fill = TRUE)

if (length(remaining_cols) > 0L) {
  column_dictionary <- rbind(
    column_dictionary,
    data.table(
      column_name = remaining_cols,
      column_group = "unclassified_remaining",
      group_order = length(present_groups) + 1L,
      column_order_within_group = seq_along(remaining_cols)
    ),
    use.names = TRUE,
    fill = TRUE
  )
}

dt <- fread(input_file, showProgress = TRUE)
setcolorder(dt, final_cols)
fwrite(dt, output_file)
fwrite(column_dictionary, dictionary_file)

cat("Reordered file written to:\n", output_file, "\n", sep = "")
cat("Column dictionary written to:\n", dictionary_file, "\n", sep = "")
