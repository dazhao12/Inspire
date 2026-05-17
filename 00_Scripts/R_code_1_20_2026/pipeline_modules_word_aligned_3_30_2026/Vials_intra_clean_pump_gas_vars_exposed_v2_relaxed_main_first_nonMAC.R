suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_raw.csv")

main_clean_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_clean_relaxed_main.csv")
main_threshold_file <- file.path(processed_path, "vital_intraop_pump_gas_threshold_rules_relaxed_main.csv")
main_summary_file <- file.path(processed_path, "vital_intraop_pump_gas_missingness_and_usage_summary_relaxed_main.csv")
main_cleaning_summary_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_cleaning_summary_relaxed_main.csv")

strict_clean_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_exposed_clean_strict_sensitivity.csv")
strict_threshold_file <- file.path(processed_path, "vital_intraop_pump_gas_threshold_rules_strict_sensitivity.csv")
strict_summary_file <- file.path(processed_path, "vital_intraop_pump_gas_missingness_and_usage_summary_strict_sensitivity.csv")
strict_cleaning_summary_file <- file.path(processed_path, "vital_intraop_pump_gas_vars_cleaning_summary_strict_sensitivity.csv")

comparison_file <- file.path(processed_path, "vital_intraop_pump_gas_threshold_strategy_comparison.csv")
note_file <- file.path(processed_path, "vital_intraop_pump_gas_threshold_strategy_note.md")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

build_rules <- function(relaxed = TRUE) {
  rules <- data.table(
    variable = c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi", "ppfi", "rfti",
                 "etgas", "etsevo", "etdes", "etiso", "n2o", "o2", "air", "fio2"),
    variable_group = c(rep("pump", 9), rep("gas_or_oxygen", 8)),
    unit = c("ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/h", "ug/mL", "ng/mL",
             "vol%", "vol%", "vol%", "vol%", "L/min", "L/min", "L/min", "%"),
    keep_zero = TRUE,
    positive_min = c(0.005, 0.01, 2, 1.5, 0.25, 0.05, if (relaxed) 10 else 300, 2.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 21),
    positive_max = c(3, if (relaxed) 5 else 3, 30, 30, if (relaxed) 10 else 1, 10, 3000, 10, 20, 10, 8, 18, 5, 15, 15, 15, 100),
    threshold_version = if (relaxed) "relaxed_main" else "strict_sensitivity"
  )

  rules[, low_value_note := ""]
  rules[variable == "pepi", low_value_note := if (relaxed) "0<pepi<10 flagged as low suspicious" else "0<pepi<300 flagged as low suspicious"]
  rules[variable == "fio2", low_value_note := "0<fio2<21 flagged as below physiologic floor"]

  rules[, cleaning_rule := sprintf("Keep 0 or %.3f-%.3f; negative and out-of-range values set to NA.", positive_min, positive_max)]
  rules[variable == "pepi", cleaning_rule := if (relaxed) {
    "Keep 0 or 10-3000; 0<value<10 flagged as low suspicious and set to NA."
  } else {
    "Keep 0 or 300-3000; 0<value<300 flagged as low suspicious and set to NA."
  }]
  rules[variable == "epii", cleaning_rule := if (relaxed) {
    "Keep 0 or 0.01-5; negative and out-of-range values set to NA."
  } else {
    "Keep 0 or 0.01-3; negative and out-of-range values set to NA."
  }]
  rules[variable == "mlni", cleaning_rule := if (relaxed) {
    "Keep 0 or 0.25-10; negative and out-of-range values set to NA."
  } else {
    "Keep 0 or 0.25-1; negative and out-of-range values set to NA."
  }]
  rules[variable == "fio2", cleaning_rule := "Keep 0 or 21-100; 0<fio2<21 and >100 set to NA."]
  rules
}

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
      threshold_version = unique(rules_dt[variable == v, threshold_version]),
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

apply_rules <- function(dt_raw, rules_dt) {
  dt_clean <- copy(dt_raw)
  measure_vars <- intersect(rules_dt$variable, names(dt_raw))

  flags <- rbindlist(lapply(measure_vars, function(v) {
    rule <- rules_dt[variable == v]
    x_raw <- dt_raw[[v]]
    x_clean <- copy(x_raw)

    neg_flag <- !is.na(x_raw) & x_raw < 0
    below_flag <- !is.na(x_raw) & x_raw > 0 & x_raw < rule$positive_min
    above_flag <- !is.na(x_raw) & x_raw > rule$positive_max
    total_flag <- neg_flag | below_flag | above_flag

    x_clean[total_flag] <- NA_real_
    dt_clean[[v]] <- x_clean

    data.table(
      threshold_version = rule$threshold_version,
      variable = v,
      n_rows = length(x_raw),
      n_negative_to_na = sum(neg_flag),
      n_below_positive_min_to_na = sum(below_flag),
      n_above_positive_max_to_na = sum(above_flag),
      n_total_set_to_na = sum(total_flag),
      pct_total_set_to_na = round(100 * mean(total_flag), 4),
      note = rule$low_value_note
    )
  }), use.names = TRUE, fill = TRUE)

  list(clean = dt_clean, flags = flags)
}

write_version_outputs <- function(dt_raw, rules_dt, clean_file, threshold_file, summary_file, cleaning_summary_file) {
  res <- apply_rules(dt_raw, rules_dt)
  dt_clean <- res$clean
  flags <- res$flags

  fwrite(dt_clean, clean_file)
  fwrite(rules_dt, threshold_file)
  fwrite(flags, cleaning_summary_file)

  summary_raw <- compute_usage_summary(dt_raw, rules_dt, "raw")
  summary_clean <- compute_usage_summary(dt_clean, rules_dt, "clean")
  summary_dt <- merge(
    summary_raw,
    flags[, .(variable, n_negative_to_na, n_below_positive_min_to_na, n_above_positive_max_to_na, n_total_set_to_na, pct_total_set_to_na, note)],
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
  summary_dt[, variable_order := match(variable, rules_dt$variable)]
  setorder(summary_dt, variable_order)
  summary_dt[, variable_order := NULL]
  fwrite(summary_dt, summary_file)

  list(clean = dt_clean, flags = flags, summary = summary_dt)
}

dt_raw <- fread(input_file, showProgress = TRUE)
rules_relaxed <- build_rules(relaxed = TRUE)
rules_strict <- build_rules(relaxed = FALSE)

out_relaxed <- write_version_outputs(dt_raw, rules_relaxed, main_clean_file, main_threshold_file, main_summary_file, main_cleaning_summary_file)
out_strict <- write_version_outputs(dt_raw, rules_strict, strict_clean_file, strict_threshold_file, strict_summary_file, strict_cleaning_summary_file)

comparison_dt <- merge(
  out_relaxed$flags[, .(variable, relaxed_n_total_set_to_na = n_total_set_to_na, relaxed_pct_total_set_to_na = pct_total_set_to_na)],
  out_strict$flags[, .(variable, strict_n_total_set_to_na = n_total_set_to_na, strict_pct_total_set_to_na = pct_total_set_to_na)],
  by = "variable",
  all = TRUE
)
comparison_dt <- merge(
  comparison_dt,
  rules_relaxed[, .(variable, relaxed_positive_min = positive_min, relaxed_positive_max = positive_max)],
  by = "variable",
  all.x = TRUE
)
comparison_dt <- merge(
  comparison_dt,
  rules_strict[, .(variable, strict_positive_min = positive_min, strict_positive_max = positive_max)],
  by = "variable",
  all.x = TRUE
)
comparison_dt[, delta_n_total_set_to_na := relaxed_n_total_set_to_na - strict_n_total_set_to_na]
fwrite(comparison_dt, comparison_file)

note_lines <- c(
  "# Pump + Gas Threshold Strategy Comparison",
  "",
  "Main analysis now uses relaxed thresholds for three variables:",
  "- pepi: 10-3000 ug/h",
  "- epii: 0.01-5 ug/kg/min",
  "- mlni: 0.25-10 ug/kg/min",
  "",
  "Strict sensitivity thresholds are also exported:",
  "- pepi: 300-3000 ug/h",
  "- epii: 0.01-3 ug/kg/min",
  "- mlni: 0.25-1 ug/kg/min",
  "",
  "All other variables keep the same thresholds as the previous cleaning version.",
  "",
  "Recommended interpretation:",
  "- Use relaxed_main as the primary analytic cleaning table.",
  "- Use strict_sensitivity as a sensitivity analysis table.",
  "- Percentile trimming, if used later, should be applied after threshold cleaning rather than instead of threshold cleaning."
)
writeLines(note_lines, note_file)

cat("Relaxed-main clean file written to:\n", main_clean_file, "\n", sep = "")
cat("Strict-sensitivity clean file written to:\n", strict_clean_file, "\n", sep = "")
cat("Comparison file written to:\n", comparison_file, "\n", sep = "")
cat("Threshold strategy note written to:\n", note_file, "\n", sep = "")
