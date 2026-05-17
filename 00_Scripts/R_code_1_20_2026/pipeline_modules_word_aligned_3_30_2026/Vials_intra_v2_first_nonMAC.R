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

additive_vars <- c(
  "eph", "phe", "epi", "ppf", "ftn", "sft", "aft", "mdz",
  "ns", "hs", "psa", "hns", "hes", "d5w", "d10w", "d50w", "alb5", "alb20",
  "rbc", "ffp", "pc", "pheresis", "cryo", "ebl", "uo", "ds"
)
non_additive_vars <- c(
  "etsevo", "etdes", "etiso", "pepi", "nepi", "epii", "dopai", "dobui",
  "ntgi", "mlni", "vaso", "ppfi", "rfti"
)

normalize_unit_token <- function(unit_value) {
  if (is.na(unit_value) || unit_value == "") {
    return("unitless")
  }
  x <- unit_value
  x <- gsub("%", "pct", x, fixed = TRUE)
  x <- gsub("/nL", "per_nL", x, fixed = TRUE)
  x <- gsub("/min", "_min", x, fixed = TRUE)
  x <- gsub("/h", "_h", x, fixed = TRUE)
  x <- gsub("/m2", "_m2", x, fixed = TRUE)
  x <- gsub("/kg", "_kg", x, fixed = TRUE)
  x <- gsub("/mL", "_mL", x, fixed = TRUE)
  x <- gsub("/L", "_L", x, fixed = TRUE)
  x <- gsub("/", "_", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- gsub("[^A-Za-z0-9_]+", "", x)
  x <- gsub("__+", "_", x)
  tolower(x)
}

build_intra_unit_map_from_parameters <- function(parameters_path) {
  if (!file.exists(parameters_path)) {
    return(setNames(character(), character()))
  }
  dt <- fread(parameters_path, encoding = "UTF-8")
  if (length(names(dt)) > 0L) {
    names(dt)[1] <- sub("^\\ufeff", "", names(dt)[1])
  }
  required_cols <- c("Table", "Label", "Unit")
  if (!all(required_cols %in% names(dt))) {
    return(setNames(character(), character()))
  }
  dt[, `:=`(
    Table = tolower(trimws(Table)),
    Label = tolower(trimws(Label)),
    Unit = trimws(Unit)
  )]
  dt <- unique(dt[Table == "vitals", .(Label, unit_token = vapply(Unit, normalize_unit_token, character(1)))], by = "Label")
  unit_map <- setNames(dt$unit_token, dt$Label)
  unit_map["ds"] <- "ml"
  unit_map
}

unit_map <- build_intra_unit_map_from_parameters(file.path(raw_path, "parameters.csv"))
get_unit <- function(var_name) {
  if (var_name %in% names(unit_map)) return(unname(unit_map[[var_name]]))
  "raw_source_unit"
}

cat("Loading first non-MAC anchor operations and intraop vitals ...\n")
ops <- load_first_nonmac_anchor_ops(
  raw_path = raw_path,
  extra_cols = c("orin_time", "orout_time")
)
write_anchor_map(ops, processed_path)
ops <- ops[, .(op_id, subject_id, hadm_id, orin_time, orout_time)]
setorderv(ops, c("subject_id", "hadm_id", "orin_time", "op_id"))
ops[, surgery_number := rowid(subject_id)]

vitals <- fread(
  file.path(raw_path, "vitals.csv"),
  select = c("op_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA", "NULL", "(Null)", "null")
)
vitals <- vitals[op_id %in% ops$op_id & item_name %in% target_items]
vitals[, `:=`(value = as.numeric(value), chart_time = as.numeric(chart_time))]
vitals <- vitals[!is.na(value) & !is.na(chart_time)]

vitals_intraop <- merge(vitals, ops, by = "op_id", all.x = FALSE)
vitals_intraop <- vitals_intraop[chart_time >= orin_time & chart_time <= orout_time]
vitals_intraop[, min_from_entry := chart_time - orin_time]

final_wide <- dcast(
  vitals_intraop,
  subject_id + hadm_id + op_id + surgery_number + chart_time + min_from_entry ~ item_name,
  value.var = "value",
  fun.aggregate = mean,
  na.rm = TRUE
)
setorder(final_wide, subject_id, hadm_id, surgery_number, chart_time, op_id)
fwrite(final_wide, file.path(processed_path, "vital_intraop_full_complete.csv"))

existing_additive <- intersect(additive_vars, names(final_wide))

sum_dt <- final_wide[, lapply(.SD, sum, na.rm = TRUE), by = op_id, .SDcols = existing_additive]
if (length(existing_additive) > 0L) {
  setnames(
    sum_dt,
    old = existing_additive,
    new = vapply(existing_additive, function(v) paste0(v, "_sum_", get_unit(v)), character(1))
  )
}

meta_info <- unique(ops[, .(op_id, subject_id, hadm_id, surgery_number)])
final_output_sum <- copy(meta_info)
final_output_sum <- merge(final_output_sum, sum_dt, by = "op_id", all.x = TRUE)
measure_cols <- setdiff(names(final_output_sum), c("subject_id", "hadm_id", "op_id", "surgery_number"))
final_output_sum[, has_intraop_total_record := as.integer(rowSums(!is.na(.SD)) > 0L), .SDcols = measure_cols]
setcolorder(
  final_output_sum,
  c("subject_id", "hadm_id", "op_id", "surgery_number", "has_intraop_total_record",
    setdiff(names(final_output_sum), c("subject_id", "hadm_id", "op_id", "surgery_number", "has_intraop_total_record")))
)
setorder(final_output_sum, subject_id, hadm_id, op_id)
fwrite(final_output_sum, file.path(processed_path, "drugs_fluids_total_sum.csv"))

agg_contract <- data.table(
  source_var = existing_additive,
  output_var = vapply(existing_additive, function(v) paste0(v, "_sum_", get_unit(v)), character(1)),
  aggregation = "sum",
  unit = vapply(existing_additive, get_unit, character(1))
)
fwrite(agg_contract, file.path(processed_path, "drugs_fluids_aggregation_contract.csv"))

notes_dt <- data.table(
  note = c(
    "Anchor op is the first non-MAC surgery per subject_id + hadm_id.",
    "Only anchor op_id rows are kept for intraoperative time-series and totals.",
    "Timeseries window is orin_time <= chart_time <= orout_time.",
    "The totals table includes summed additive drugs/fluids/products/loss-output variables only.",
    "Pump infusion drugs, anesthetic gases, and rate-style variables are excluded from the totals table."
  )
)
fwrite(notes_dt, file.path(processed_path, "drugs_fluids_notes.csv"))

cat("Done: Vials_intra_v2_first_nonMAC.R\n")
