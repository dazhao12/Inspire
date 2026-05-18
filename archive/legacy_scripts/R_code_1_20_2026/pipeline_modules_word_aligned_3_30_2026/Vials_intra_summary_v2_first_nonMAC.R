suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "drugs_fluids_total_sum.csv")

dt_sum <- fread(input_file)
id_cols <- c("op_id", "subject_id", "hadm_id", "surgery_number")
target_vars <- setdiff(names(dt_sum), id_cols)

parse_var_meta <- function(v) {
  if (grepl("_any_use_flag$", v)) {
    base <- sub("_any_use_flag$", "", v)
    return(list(base_var = base, agg = "any_use", unit = "flag"))
  }
  m <- regexec("^(.*)_(sum|mean)_([^_].*)$", v)
  p <- regmatches(v, m)[[1]]
  if (length(p) == 4L) {
    return(list(base_var = p[2], agg = p[3], unit = p[4]))
  }
  list(base_var = v, agg = "unknown", unit = "unknown_unit")
}

meta_dt <- rbindlist(lapply(target_vars, function(v) {
  x <- parse_var_meta(v)
  data.table(variable = v, base_var = x$base_var, agg = x$agg, unit = x$unit)
}))

calc_numeric_stats <- function(x) {
  n_total <- length(x)
  n_non_missing <- sum(!is.na(x))
  x_nonzero <- x[!is.na(x) & x > 0]
  n_users <- length(x_nonzero)
  pct_users <- if (n_total > 0) 100 * n_users / n_total else NA_real_
  mean_all <- mean(x, na.rm = TRUE)
  sd_all <- sd(x, na.rm = TRUE)

  if (n_users > 0) {
    median_users <- median(x_nonzero)
    q1_users <- quantile(x_nonzero, 0.25)
    q3_users <- quantile(x_nonzero, 0.75)
  } else {
    median_users <- NA_real_
    q1_users <- NA_real_
    q3_users <- NA_real_
  }

  list(
    N_Total = n_total,
    N_Non_Missing = n_non_missing,
    N_Users = n_users,
    Pct_Users = round(pct_users, 2),
    Mean_All = round(mean_all, 4),
    SD_All = round(sd_all, 4),
    Mean_SD_All = sprintf("%.4f (%.4f)", mean_all, sd_all),
    Median_IQR_Users = ifelse(
      is.na(median_users),
      "NA",
      sprintf("%.4f [%.4f - %.4f]", median_users, q1_users, q3_users)
    )
  )
}

calc_flag_stats <- function(x) {
  n_total <- length(x)
  n_non_missing <- sum(!is.na(x))
  n_users <- sum(x == 1, na.rm = TRUE)
  pct_users <- if (n_total > 0) 100 * n_users / n_total else NA_real_
  mean_all <- mean(x, na.rm = TRUE)
  sd_all <- sd(x, na.rm = TRUE)

  list(
    N_Total = n_total,
    N_Non_Missing = n_non_missing,
    N_Users = n_users,
    Pct_Users = round(pct_users, 2),
    Mean_All = round(mean_all, 4),
    SD_All = round(sd_all, 4),
    Mean_SD_All = sprintf("%.4f (%.4f)", mean_all, sd_all),
    Median_IQR_Users = "NA"
  )
}

summary_list <- lapply(target_vars, function(v) {
  meta <- meta_dt[variable == v]
  stats <- if (meta$agg == "any_use") calc_flag_stats(dt_sum[[v]]) else calc_numeric_stats(dt_sum[[v]])
  data.table(
    Variable = v,
    Base_Variable = meta$base_var,
    Aggregation = meta$agg,
    Unit = meta$unit,
    N_Total = stats$N_Total,
    N_Non_Missing = stats$N_Non_Missing,
    N_Users = stats$N_Users,
    Pct_Users = stats$Pct_Users,
    Mean_SD_All = stats$Mean_SD_All,
    Median_IQR_Users = stats$Median_IQR_Users,
    Raw_Rate = stats$Pct_Users
  )
})

final_stats_table <- rbindlist(summary_list, use.names = TRUE)
setorder(final_stats_table, -Raw_Rate, Variable)
fwrite(final_stats_table, file.path(processed_path, "drugs_fluids_descriptive_stats.csv"))

cat("Done: Vials_intra_summary_v2_first_nonMAC.R\n")
