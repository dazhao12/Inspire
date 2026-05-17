suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed.csv")
output_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputed_main30.csv")
summary_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputation_summary_main30.csv")
case_qc_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputation_case_qc_main30.csv")
threshold_file <- file.path(processed_path, "vital_intraop_pump_vars_only_exposed_imputation_thresholds_main30.csv")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

id_cols <- c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry")
vaso_vars <- c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi")
anes_vars <- c("ppfi", "rfti")
measure_vars <- c(vaso_vars, anes_vars)

thresholds <- data.table(
  variable = measure_vars,
  positive_min = c(0.005, 0.01, 2, 1.5, 0.25, 0.05, 10, 2.5, 0.5),
  positive_max = c(3, 5, 30, 30, 10, 10, 3000, 10, 20),
  cap_minutes = c(rep(30, length(vaso_vars)), rep(Inf, length(anes_vars))),
  imputation_rule = c(
    rep("Before first positive = 0; explicit 0 carries forward; positive values use 30-min capped LOCF.", length(vaso_vars)),
    rep("Before first positive = 0; explicit 0 carries forward; positive values carry to next update or OR end.", length(anes_vars))
  )
)

impute_series <- function(x, t, cap_minutes) {
  n <- length(x)
  out <- rep(NA_real_, n)
  state <- rep("unknown", n)

  started <- FALSE
  last_obs_val <- NA_real_
  last_obs_time <- NA_real_

  for (i in seq_len(n)) {
    xi <- x[i]
    ti <- t[i]

    if (!is.na(xi)) {
      out[i] <- xi
      if (xi > 0) {
        state[i] <- "observed_positive"
      } else {
        state[i] <- "observed_zero"
      }
      started <- TRUE
      last_obs_val <- xi
      last_obs_time <- ti
      next
    }

    if (!started) {
      out[i] <- 0
      state[i] <- "pre_exposure_zero"
      next
    }

    if (!is.na(last_obs_val) && last_obs_val == 0) {
      out[i] <- 0
      state[i] <- "carry_zero_after_observed_zero"
      next
    }

    if (!is.na(last_obs_val) && last_obs_val > 0) {
      if (is.infinite(cap_minutes) || (ti - last_obs_time) <= cap_minutes) {
        out[i] <- last_obs_val
        state[i] <- "carried_positive"
      } else {
        out[i] <- NA_real_
        state[i] <- "capped_to_na"
      }
    }
  }

  list(value = out, state = state)
}

dt <- fread(input_file, showProgress = TRUE)
setorder(dt, subject_id, hadm_id, surgery_number, chart_time, op_id)

summary_list <- vector("list", length(measure_vars))
case_qc_list <- vector("list", length(measure_vars))

for (idx in seq_along(measure_vars)) {
  v <- measure_vars[idx]
  rule <- thresholds[variable == v]

  x_raw <- dt[[v]]
  neg_flag <- !is.na(x_raw) & x_raw < 0
  below_flag <- !is.na(x_raw) & x_raw > 0 & x_raw < rule$positive_min
  above_flag <- !is.na(x_raw) & x_raw > rule$positive_max
  threshold_flag <- neg_flag | below_flag | above_flag

  x_clean <- copy(x_raw)
  x_clean[threshold_flag] <- NA_real_

  dt[, temp_value_for_impute := x_clean]
  tmp <- dt[, .(row_id = .I, min_from_entry, value = temp_value_for_impute), by = op_id]
  tmp[, c("imputed_value", "imputation_state") := {
    res <- impute_series(value, min_from_entry, cap_minutes = rule$cap_minutes)
    list(res$value, res$state)
  }, by = op_id]

  dt[[v]] <- tmp$imputed_value
  dt[, temp_value_for_impute := NULL]

  state_counts <- tmp[, .N, by = imputation_state]
  state_wide <- dcast(state_counts, . ~ imputation_state, value.var = "N", fill = 0)

  summary_list[[idx]] <- data.table(
    variable = v,
    n_rows = nrow(dt),
    n_threshold_to_na = sum(threshold_flag),
    pct_threshold_to_na = round(100 * mean(threshold_flag), 4),
    n_missing_after_imputation = sum(is.na(tmp$imputed_value)),
    pct_missing_after_imputation = round(100 * mean(is.na(tmp$imputed_value)), 4)
  )
  summary_list[[idx]] <- cbind(summary_list[[idx]], state_wide)

  case_qc_list[[idx]] <- tmp[, .(
    n_rows = .N,
    n_observed_positive = sum(imputation_state == "observed_positive"),
    n_observed_zero = sum(imputation_state == "observed_zero"),
    n_pre_exposure_zero = sum(imputation_state == "pre_exposure_zero"),
    n_carried_positive = sum(imputation_state == "carried_positive"),
    n_carry_zero_after_observed_zero = sum(imputation_state == "carry_zero_after_observed_zero"),
    n_capped_to_na = sum(imputation_state == "capped_to_na")
  ), by = op_id]
  case_qc_list[[idx]][, variable := v]
}

fwrite(dt, output_file)
fwrite(thresholds, threshold_file)
summary_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
case_qc_dt <- rbindlist(case_qc_list, use.names = TRUE, fill = TRUE)
fwrite(summary_dt, summary_file)
fwrite(case_qc_dt, case_qc_file)

cat("Imputed main30 file written to:\n", output_file, "\n", sep = "")
cat("Threshold file written to:\n", threshold_file, "\n", sep = "")
cat("Summary written to:\n", summary_file, "\n", sep = "")
cat("Case QC written to:\n", case_qc_file, "\n", sep = "")
