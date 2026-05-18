suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_raw.csv")
clean_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_clean.csv")
threshold_file <- file.path(processed_path, "vital_intraop_pump_gas_threshold_rules.csv")
summary_file <- file.path(processed_path, "vital_intraop_pump_gas_missingness_and_usage_summary.csv")
cleaning_summary_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_cleaning_summary.csv")
imputation_md_file <- file.path(processed_path, "vital_intraop_pump_gas_imputation_recommendation.md")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

id_time_cols <- c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry")
threshold_rules <- data.table(
  variable = c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi", "ppfi", "rfti",
               "etgas", "etsevo", "etdes", "etiso", "n2o", "o2", "air", "fio2"),
  variable_group = c(rep("pump", 9), rep("gas_or_oxygen", 8)),
  unit = c("ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/h", "ug/mL", "ng/mL",
           "vol%", "vol%", "vol%", "vol%", "L/min", "L/min", "L/min", "%"),
  keep_zero = TRUE,
  positive_min = c(0.005, 0.01, 2, 1.5, 0.25, 0.05, 300, 2.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 21),
  positive_max = c(3, 3, 30, 30, 1, 10, 3000, 10, 20, 10, 8, 18, 5, 15, 15, 15, 100),
  low_value_note = c("", "", "", "", "", "", "0<pepi<300 flagged as low suspicious", "", "",
                     "", "", "", "", "", "", "", "0<fio2<21 flagged as below physiologic floor"),
  cleaning_rule = c(
    "Keep 0 or 0.005-3; negative and out-of-range values set to NA.",
    "Keep 0 or 0.01-3; negative and out-of-range values set to NA.",
    "Keep 0 or 2-30; negative and out-of-range values set to NA.",
    "Keep 0 or 1.5-30; negative and out-of-range values set to NA.",
    "Keep 0 or 0.25-1; negative and out-of-range values set to NA.",
    "Keep 0 or 0.05-10; negative and out-of-range values set to NA.",
    "Keep 0 or 300-3000; 0<value<300 flagged as low suspicious and set to NA.",
    "Keep 0 or 2.5-10; negative and out-of-range values set to NA.",
    "Keep 0 or 0.5-20; negative and out-of-range values set to NA.",
    "Keep 0-10; negative and >10 values set to NA.",
    "Keep 0-8; negative and >8 values set to NA.",
    "Keep 0-18; negative and >18 values set to NA.",
    "Keep 0-5; negative and >5 values set to NA.",
    "Keep 0-15; negative and >15 values set to NA.",
    "Keep 0-15; negative and >15 values set to NA.",
    "Keep 0-15; negative and >15 values set to NA.",
    "Keep 0 or 21-100; 0<fio2<21 and >100 set to NA."
  )
)

dt_raw <- fread(input_file, showProgress = TRUE)
available_rules <- threshold_rules[variable %in% names(dt_raw)]
measure_vars <- available_rules$variable
dt_clean <- copy(dt_raw)

cleaning_flags <- rbindlist(lapply(measure_vars, function(v) {
  rules <- available_rules[variable == v]
  x_raw <- dt_raw[[v]]
  x_clean <- copy(x_raw)

  neg_flag <- !is.na(x_raw) & x_raw < 0
  below_flag <- !is.na(x_raw) & x_raw > 0 & x_raw < rules$positive_min
  above_flag <- !is.na(x_raw) & x_raw > rules$positive_max
  total_flag <- neg_flag | below_flag | above_flag

  x_clean[total_flag] <- NA_real_
  dt_clean[[v]] <- x_clean

  data.table(
    variable = v,
    n_rows = length(x_raw),
    n_negative_to_na = sum(neg_flag),
    n_below_positive_min_to_na = sum(below_flag),
    n_above_positive_max_to_na = sum(above_flag),
    n_total_set_to_na = sum(total_flag),
    pct_total_set_to_na = round(100 * mean(total_flag), 4),
    note = rules$low_value_note
  )
}), use.names = TRUE, fill = TRUE)

setcolorder(dt_clean, names(dt_raw))
fwrite(dt_clean, clean_file)
fwrite(available_rules, threshold_file)
fwrite(cleaning_flags, cleaning_summary_file)

compute_usage_summary <- function(dt_obj, rules_dt, stage_label) {
  vars <- rules_dt$variable
  rbindlist(lapply(vars, function(v) {
    x <- dt_obj[[v]]
    nonmiss <- x[!is.na(x)]
    pos <- nonmiss[nonmiss > 0]
    op_dt <- dt_obj[, .(
      any_nonmissing = any(!is.na(get(v))),
      any_zero = any(get(v) == 0, na.rm = TRUE),
      any_positive = any(get(v) > 0, na.rm = TRUE)
    ), by = op_id]

    data.table(
      stage = stage_label,
      variable = v,
      variable_group = rules_dt[variable == v, variable_group],
      unit = rules_dt[variable == v, unit],
      n_rows = length(x),
      n_missing = sum(is.na(x)),
      missing_pct = round(100 * mean(is.na(x)), 4),
      n_zero = sum(nonmiss == 0),
      zero_pct_nonmissing = if (length(nonmiss)) round(100 * mean(nonmiss == 0), 4) else NA_real_,
      n_positive = sum(nonmiss > 0),
      positive_pct_nonmissing = if (length(nonmiss)) round(100 * mean(nonmiss > 0), 4) else NA_real_,
      ops_any_nonmissing = sum(op_dt$any_nonmissing),
      ops_any_zero = sum(op_dt$any_zero),
      ops_any_positive = sum(op_dt$any_positive),
      p1_positive = if (length(pos)) as.numeric(quantile(pos, 0.01, names = FALSE)) else NA_real_,
      p5_positive = if (length(pos)) as.numeric(quantile(pos, 0.05, names = FALSE)) else NA_real_,
      p50_positive = if (length(pos)) as.numeric(quantile(pos, 0.50, names = FALSE)) else NA_real_,
      p95_positive = if (length(pos)) as.numeric(quantile(pos, 0.95, names = FALSE)) else NA_real_,
      p99_positive = if (length(pos)) as.numeric(quantile(pos, 0.99, names = FALSE)) else NA_real_,
      max_positive = if (length(pos)) max(pos) else NA_real_
    )
  }), use.names = TRUE, fill = TRUE)
}

summary_raw <- compute_usage_summary(dt_raw, available_rules, "raw")
summary_clean <- compute_usage_summary(dt_clean, available_rules, "clean")
summary_dt <- merge(
  summary_raw,
  cleaning_flags[, .(variable, n_negative_to_na, n_below_positive_min_to_na, n_above_positive_max_to_na, n_total_set_to_na, pct_total_set_to_na, note)],
  by = "variable",
  all.x = TRUE
)
summary_dt <- merge(
  summary_dt,
  summary_clean[, .(
    variable,
    clean_n_missing = n_missing,
    clean_missing_pct = missing_pct,
    clean_n_zero = n_zero,
    clean_zero_pct_nonmissing = zero_pct_nonmissing,
    clean_n_positive = n_positive,
    clean_positive_pct_nonmissing = positive_pct_nonmissing,
    clean_ops_any_nonmissing = ops_any_nonmissing,
    clean_ops_any_zero = ops_any_zero,
    clean_ops_any_positive = ops_any_positive,
    clean_p1_positive = p1_positive,
    clean_p5_positive = p5_positive,
    clean_p50_positive = p50_positive,
    clean_p95_positive = p95_positive,
    clean_p99_positive = p99_positive,
    clean_max_positive = max_positive
  )],
  by = "variable",
  all.x = TRUE
)
summary_dt[, variable_order := match(variable, available_rules$variable)]
setorder(summary_dt, variable_order)
summary_dt[, variable_order := NULL]
fwrite(summary_dt, summary_file)

imputation_lines <- c(
  "# INSPIRE Pump + Gas Variable Imputation Recommendation",
  "",
  "## Scope",
  "",
  "This note applies to:",
  "- Pump variables: nepi, epii, dobui, dopai, mlni, ntgi, pepi, ppfi, rfti",
  "- Gas / oxygen variables: etgas, etsevo, etdes, etiso, n2o, o2, air, fio2",
  "",
  "## Interpretation rules",
  "",
  "- NA does not mean drug stopped.",
  "- Zero means the source explicitly recorded 0 at that timestamp.",
  "- Positive values indicate observed exposure or setpoint updates.",
  "- Sparse positive records should be interpreted as state-update streams, not dense monitoring streams.",
  "",
  "## Recommended downstream imputation strategy",
  "",
  "### Pump variables",
  "",
  "- Do not apply mean, median, or MICE directly to the raw minute-level NA values.",
  "- Reconstruct a regularized 5-minute state table later.",
  "- Before first positive record, set state to 0.",
  "- After an explicit 0 record, set state to 0 from that timestamp onward.",
  "- Between updates, carry the most recent value forward (LOCF).",
  "- Main analysis for vasoactive infusions: 30-minute capped LOCF.",
  "- Sensitivity analyses: 60-minute capped LOCF and carry-to-orout.",
  "- For ppfi and rfti, more aggressive state continuation is acceptable; optionally add a sensitivity analysis trimming the last 10 minutes before OR exit.",
  "",
  "### Gas / oxygen variables",
  "",
  "- Treat etgas, etsevo, etdes, etiso, n2o, o2, air, and fio2 as continuous state variables.",
  "- If a regularized time grid is needed later, use short-window LOCF or linear interpolation within a case.",
  "- Do not overwrite the current cleaned table with interpolated values at this stage.",
  "",
  "## Current deliverables",
  "",
  "- Raw exposed subset preserved.",
  "- Threshold-screened clean subset preserved.",
  "- No 5-minute reconstruction is generated in this step."
)
writeLines(imputation_lines, imputation_md_file)

cat("Clean file written to:\n", clean_file, "\n", sep = "")
cat("Threshold rules written to:\n", threshold_file, "\n", sep = "")
cat("Missingness / usage summary written to:\n", summary_file, "\n", sep = "")
cat("Cleaning summary written to:\n", cleaning_summary_file, "\n", sep = "")
cat("Imputation recommendation written to:\n", imputation_md_file, "\n", sep = "")
