suppressPackageStartupMessages({
  library(data.table)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
} else {
  getwd()
}
source(file.path(script_dir, "anchor_first_nonmac_utils.R"))

raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)

raw_output_file <- file.path(processed_path, "vital_intraop_full_complete_median.csv")
grouped_output_file <- file.path(processed_path, "vital_intraop_full_complete_median_grouped_columns.csv")
dictionary_file <- file.path(processed_path, "vital_intraop_full_complete_median_grouped_columns_dictionary.csv")
duplicate_qc_file <- file.path(processed_path, "vital_intraop_full_complete_median_duplicate_qc.csv")

target_items <- c(
  "hr", "rr", "spo2", "etco2", "bt",
  "nibp_sbp", "nibp_dbp", "nibp_mbp", "art_sbp", "art_dbp", "art_mbp",
  "fio2", "vt", "minvol", "pip", "peep", "pplat", "pmean", "etgas", "cpat", "o2", "air", "n2o",
  "cvp", "pap_sbp", "pap_dbp", "pap_mbp", "ci", "svi", "bis", "cbro2",
  "stii", "stiii", "sti", "stv5",
  "etsevo", "etdes", "etiso",
  "eph", "phe", "pepi", "nepi", "epi", "epii", "dopai", "dobui", "ntgi", "mlni", "vaso",
  "ppf", "ppfi", "rfti", "ftn", "sft", "aft", "mdz",
  "ns", "hs", "psa", "hns", "hes", "d5w", "d10w", "d50w", "alb5", "alb20",
  "rbc", "ffp", "pc", "pheresis", "cryo",
  "ebl", "uo", "ds"
)

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

cat("Loading first non-MAC anchor operations ...\n")
ops <- load_first_nonmac_anchor_ops(raw_path = raw_path, extra_cols = c("orin_time", "orout_time"))
write_anchor_map(ops, processed_path)
ops <- ops[, .(op_id, subject_id, hadm_id, orin_time, orout_time)]
setorderv(ops, c("subject_id", "hadm_id", "orin_time", "op_id"))
ops[, surgery_number := rowid(subject_id)]

cat("Reading raw vitals.csv and filtering target items ...\n")
vitals <- fread(
  file.path(raw_path, "vitals.csv"),
  select = c("op_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA", "NULL", "(Null)", "null")
)
vitals <- vitals[op_id %in% ops$op_id & item_name %in% target_items]
vitals[, `:=`(chart_time = as.numeric(chart_time), value = as.numeric(value))]
vitals <- vitals[!is.na(chart_time) & !is.na(value)]

cat("Keeping intraoperative window only ...\n")
vitals_intraop <- merge(vitals, ops, by = "op_id", all.x = FALSE)
vitals_intraop <- vitals_intraop[chart_time >= orin_time & chart_time <= orout_time]
vitals_intraop[, min_from_entry := chart_time - orin_time]

cat("Computing duplicate QC ...\n")
dup_long <- vitals_intraop[, .(
  n_records_same_time = .N,
  n_unique_values_same_time = uniqueN(value)
), by = .(item_name, op_id, chart_time)]
duplicate_qc <- dup_long[, .(
  n_timepoints = .N,
  n_duplicate_timepoints = sum(n_records_same_time > 1L),
  pct_duplicate_timepoints = round(100 * mean(n_records_same_time > 1L), 4),
  n_duplicate_diffvalue_timepoints = sum(n_records_same_time > 1L & n_unique_values_same_time > 1L),
  pct_duplicate_diffvalue_timepoints = round(100 * mean(n_records_same_time > 1L & n_unique_values_same_time > 1L), 4),
  max_records_same_time = max(n_records_same_time),
  max_unique_values_same_time = max(n_unique_values_same_time)
), by = item_name]
setorder(duplicate_qc, item_name)
fwrite(duplicate_qc, duplicate_qc_file)

cat("Casting to wide format using median for duplicate same-time values ...\n")
median_fun <- function(x) median(x, na.rm = TRUE)
final_wide <- dcast(
  vitals_intraop,
  subject_id + hadm_id + op_id + surgery_number + chart_time + min_from_entry ~ item_name,
  value.var = "value",
  fun.aggregate = median_fun
)
setorder(final_wide, subject_id, hadm_id, surgery_number, chart_time, op_id)
fwrite(final_wide, raw_output_file)

cat("Reordering columns by clinical block ...\n")
original_cols <- names(final_wide)
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

grouped_dt <- copy(final_wide)
setcolorder(grouped_dt, final_cols)
fwrite(grouped_dt, grouped_output_file)
fwrite(column_dictionary, dictionary_file)

cat("Median raw wide file written to:\n", raw_output_file, "\n", sep = "")
cat("Grouped median wide file written to:\n", grouped_output_file, "\n", sep = "")
cat("Grouped column dictionary written to:\n", dictionary_file, "\n", sep = "")
cat("Duplicate QC written to:\n", duplicate_qc_file, "\n", sep = "")
