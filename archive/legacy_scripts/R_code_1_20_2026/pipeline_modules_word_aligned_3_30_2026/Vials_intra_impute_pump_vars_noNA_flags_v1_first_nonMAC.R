suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed.csv")
output_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputed_noNA_with_flags.csv")
summary_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputed_noNA_with_flags_summary.csv")
case_qc_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputed_noNA_with_flags_case_qc.csv")
threshold_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputed_noNA_with_flags_thresholds.csv")
note_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputed_noNA_with_flags_note.md")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

pump_vars <- c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi", "ppfi", "rfti")

thresholds <- data.table(
  variable = pump_vars,
  positive_min = c(0.005, 0.01, 2, 1.5, 0.25, 0.05, 10, 2.5, 0.5),
  positive_max = c(3, 5, 30, 30, 10, 10, 3000, 10, 20),
  strategy = c(
    rep("Before first positive = 0; explicit 0 stays 0; positive values carry forward to OR end; no NA retained.", 7),
    rep("Before first positive = 0; explicit 0 stays 0; target concentrations carry forward to OR end; no NA retained.", 2)
  )
)

impute_series_no_na <- function(x) {
  n <- length(x)
  out <- rep(0, n)
  flag <- rep(1L, n)
  state <- rep("imputed_pre_exposure_zero", n)

  started <- FALSE
  last_obs_val <- 0

  for (i in seq_len(n)) {
    xi <- x[i]

    if (!is.na(xi)) {
      out[i] <- xi
      flag[i] <- 0L
      state[i] <- if (xi > 0) "observed_positive" else "observed_zero"
      started <- started || xi > 0 || started
      last_obs_val <- xi
      next
    }

    if (!started) {
      out[i] <- 0
      flag[i] <- 1L
      state[i] <- "imputed_pre_exposure_zero"
      next
    }

    out[i] <- last_obs_val
    flag[i] <- 1L
    state[i] <- if (last_obs_val > 0) "imputed_carry_forward_positive" else "imputed_carry_forward_zero"
  }

  list(value = out, flag = flag, state = state)
}

dt <- fread(input_file, showProgress = TRUE)
setorder(dt, subject_id, hadm_id, surgery_number, chart_time, op_id)

summary_list <- vector("list", length(pump_vars))
case_qc_list <- vector("list", length(pump_vars))

for (idx in seq_along(pump_vars)) {
  v <- pump_vars[idx]
  rule <- thresholds[variable == v]

  x_raw <- dt[[v]]
  neg_flag <- !is.na(x_raw) & x_raw < 0
  below_flag <- !is.na(x_raw) & x_raw > 0 & x_raw < rule$positive_min
  above_flag <- !is.na(x_raw) & x_raw > rule$positive_max
  threshold_flag <- neg_flag | below_flag | above_flag

  x_clean <- copy(x_raw)
  x_clean[threshold_flag] <- NA_real_
  dt[, temp_value_for_impute := x_clean]

  tmp <- dt[, .(row_id = .I, value = temp_value_for_impute), by = op_id]
  tmp[, c("imputed_value", "imputed_flag", "imputation_state") := {
    res <- impute_series_no_na(value)
    list(res$value, res$flag, res$state)
  }, by = op_id]

  dt[[v]] <- tmp$imputed_value
  dt[[paste0(v, "_imputed_flag")]] <- tmp$imputed_flag
  dt[, temp_value_for_impute := NULL]

  state_counts <- tmp[, .N, by = imputation_state]
  state_wide <- dcast(state_counts, . ~ imputation_state, value.var = "N", fill = 0)

  summary_list[[idx]] <- data.table(
    variable = v,
    n_rows = nrow(dt),
    n_threshold_to_na_before_fill = sum(threshold_flag),
    pct_threshold_to_na_before_fill = round(100 * mean(threshold_flag), 4),
    n_observed_after_threshold = sum(tmp$imputed_flag == 0L),
    pct_observed_after_threshold = round(100 * mean(tmp$imputed_flag == 0L), 4),
    n_imputed = sum(tmp$imputed_flag == 1L),
    pct_imputed = round(100 * mean(tmp$imputed_flag == 1L), 4),
    n_na_after_final_imputation = sum(is.na(tmp$imputed_value))
  )
  summary_list[[idx]] <- cbind(summary_list[[idx]], state_wide)

  case_qc_list[[idx]] <- tmp[, .(
    n_rows = .N,
    n_observed = sum(imputed_flag == 0L),
    n_imputed = sum(imputed_flag == 1L),
    n_observed_positive = sum(imputation_state == "observed_positive"),
    n_observed_zero = sum(imputation_state == "observed_zero"),
    n_imputed_pre_exposure_zero = sum(imputation_state == "imputed_pre_exposure_zero"),
    n_imputed_carry_forward_positive = sum(imputation_state == "imputed_carry_forward_positive"),
    n_imputed_carry_forward_zero = sum(imputation_state == "imputed_carry_forward_zero")
  ), by = op_id]
  case_qc_list[[idx]][, variable := v]
}

fwrite(dt, output_file)
fwrite(thresholds, threshold_file)
summary_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
case_qc_dt <- rbindlist(case_qc_list, use.names = TRUE, fill = TRUE)
fwrite(summary_dt, summary_file)
fwrite(case_qc_dt, case_qc_file)

note_lines <- c(
  "# Pump Variable No-NA Imputation With Flags",
  "",
  "This table keeps the 9 pump-related variables and removes all NA values after deterministic rule-based filling.",
  "",
  "Rules:",
  "- Apply relaxed main threshold cleaning first.",
  "- Values failing thresholds are set to NA before filling.",
  "- Before the first positive value in a case, fill with 0.",
  "- Observed 0 remains 0.",
  "- After a positive value appears, carry the last observed value forward to OR end.",
  "- No final NA is retained.",
  "",
  "For each variable, a companion column `<var>_imputed_flag` is added:",
  "- 0 = observed value after threshold cleaning",
  "- 1 = rule-based imputed value",
  "",
  "This version is suitable for models that require complete numeric inputs, but it is more aggressive than capped LOCF because it carries positive values to OR end."
)
writeLines(note_lines, note_file)

cat("No-NA imputed file written to:\n", output_file, "\n", sep = "")
cat("Threshold file written to:\n", threshold_file, "\n", sep = "")
cat("Summary written to:\n", summary_file, "\n", sep = "")
cat("Case QC written to:\n", case_qc_file, "\n", sep = "")
cat("Note written to:\n", note_file, "\n", sep = "")
