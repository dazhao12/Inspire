#!/usr/bin/env Rscript
# Timestamp: 2026-05-17T11:05:00Z

suppressPackageStartupMessages({
  library(data.table)
})

base_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/01_raw/"
out_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517/preop_labs_attributable_90d_all_ops_nearest_latest.csv"

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

window_day_to_min <- function(days) days * 24 * 60

target_items <- c(
  "glucose", "creatinine", "hct", "potassium", "sodium", "hb", "wbc",
  "platelet", "chloride", "lymphocyte", "seg", "bun", "calcium",
  "phosphorus", "albumin", "total_bilirubin", "alt", "ast",
  "total_protein", "alp", "crp", "sao2", "hco3", "ptinr", "ph",
  "pao2", "paco2", "aptt", "ica", "fibrinogen", "be", "lacate",
  "ckmb", "ck", "troponin_i", "hba1c", "troponin_t", "d_dimer"
)

cat("Loading operations.csv (all op_id, no first non-MAC filter) ...\n")
ops <- fread(
  file.path(base_path, "operations.csv"),
  select = c("op_id", "subject_id", "hadm_id", "orin_time"),
  na.strings = c("", "NA")
)
ops[, `:=`(
  orin_time = as.numeric(orin_time),
  subject_id = as.numeric(subject_id),
  hadm_id = as.numeric(hadm_id)
)]
ops <- unique(ops[!is.na(subject_id) & !is.na(op_id) & !is.na(orin_time)])
ops[, preop_90d_lower_bound := orin_time - window_day_to_min(90)]

cat("Loading labs.csv ...\n")
labs <- fread(
  file.path(base_path, "labs.csv"),
  select = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
labs[, `:=`(
  chart_time = as.numeric(chart_time),
  value = suppressWarnings(as.numeric(value))
)]
labs <- labs[
  subject_id %in% ops$subject_id &
    item_name %chin% target_items &
    !is.na(chart_time) &
    !is.na(value)
]

cat("Joining labs to operations by subject_id ...\n")
setkey(ops, subject_id)
setkey(labs, subject_id)
joined <- merge(
  labs[, .(subject_id, chart_time, item_name, value)],
  ops[, .(subject_id, op_id, orin_time, preop_90d_lower_bound)],
  by = "subject_id",
  allow.cartesian = TRUE
)

pre_90d <- joined[
  chart_time < orin_time &
    chart_time >= preop_90d_lower_bound
]

cat("Selecting nearest value within preop 90d window ...\n")
nearest_dt <- pre_90d[, .(preop_value = value[which.max(chart_time)]), by = .(op_id, item_name)]
nearest_dt[, item_name := paste0("preop_", item_name)]
wide <- dcast(nearest_dt, op_id ~ item_name, value.var = "preop_value")

out <- merge(ops[, .(op_id, subject_id, hadm_id)], wide, by = "op_id", all.x = TRUE)

expected_cols <- c("op_id", "subject_id", "hadm_id", paste0("preop_", target_items))
for (col in setdiff(expected_cols, names(out))) {
  out[, (col) := NA_real_]
}
setcolorder(out, expected_cols)
setorderv(out, "op_id")

fwrite(out, out_path)

cat("Done. Wrote: ", out_path, "\n", sep = "")
cat("Rows: ", nrow(out), ", Cols: ", ncol(out), "\n", sep = "")
