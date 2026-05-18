suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_full_complete.csv")
params_file <- file.path(raw_path, "parameters.csv")

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(processed_path, paste0("timeseries_qc_first_nonMAC_", stamp))
plot_dir <- file.path(out_dir, "timeseries_priority_var_plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

id_time_cols <- c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry")

group_id_time <- c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry")
group_hemo_monitor <- c(
  "art_dbp", "art_mbp", "art_sbp",
  "nibp_dbp", "nibp_mbp", "nibp_sbp",
  "pap_dbp", "pap_mbp", "pap_sbp", "cvp",
  "hr", "ci", "svi", "spo2", "cbro2",
  "rr", "etco2", "fio2", "minvol", "vt",
  "peep", "pip", "pmean", "pplat",
  "bis", "bt", "sti", "stii", "stiii", "stv5"
)
group_gas_volatile <- c("air", "o2", "n2o", "etdes", "etgas", "etiso", "etsevo")
group_fluids_blood_io <- c(
  "ns", "hs", "hns", "d5w", "d10w", "d50w", "psa",
  "hes", "alb5", "alb20",
  "rbc", "ffp", "pc", "pheresis", "cryo",
  "ebl", "uo"
)
group_bolus <- c("eph", "epi", "phe", "vaso", "mdz", "ftn", "sft", "aft", "ppf")
group_rate <- c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi")
group_target <- c("ppfi", "rfti")
group_unmapped <- c("cpat", "ds")

priority_vars <- c(group_hemo_monitor, group_fluids_blood_io, group_bolus, group_rate, group_target)

summary_mode_sum <- c(group_fluids_blood_io, group_bolus)
summary_mode_distribution <- c(group_hemo_monitor, group_rate, group_target)

monitor_limits <- list(
  art_dbp = c(10, 150),
  art_mbp = c(20, 180),
  art_sbp = c(30, 260),
  nibp_dbp = c(10, 150),
  nibp_mbp = c(20, 180),
  nibp_sbp = c(30, 260),
  pap_dbp = c(0, 60),
  pap_mbp = c(0, 80),
  pap_sbp = c(0, 120),
  cvp = c(-5, 40),
  hr = c(20, 250),
  ci = c(0.5, 10),
  svi = c(5, 150),
  spo2 = c(30, 100),
  cbro2 = c(0, 100),
  rr = c(2, 80),
  etco2 = c(0, 80),
  fio2 = c(20, 100),
  minvol = c(0, 40),
  vt = c(0, 2000),
  peep = c(0, 30),
  pip = c(0, 80),
  pmean = c(0, 40),
  pplat = c(0, 60),
  bis = c(0, 100),
  bt = c(30, 43),
  sti = c(-10, 10),
  stii = c(-10, 10),
  stiii = c(-10, 10),
  stv5 = c(-10, 10)
)

gas_limits <- list(
  air = c(0, 20),
  o2 = c(0, 20),
  n2o = c(0, 20),
  etdes = c(0, 25),
  etgas = c(0, 25),
  etiso = c(0, 10),
  etsevo = c(0, 10)
)

fluid_limits <- list(
  ns = c(0, 5000),
  hs = c(0, 5000),
  hns = c(0, 5000),
  d5w = c(0, 2000),
  d10w = c(0, 2000),
  d50w = c(0, 1000),
  psa = c(0, 5000),
  hes = c(0, 2000),
  alb5 = c(0, 2000),
  alb20 = c(0, 1000),
  ebl = c(0, 5000),
  uo = c(0, 3000)
)

blood_limits <- list(
  rbc = c(0, 20),
  ffp = c(0, 20),
  pc = c(0, 20),
  pheresis = c(0, 10),
  cryo = c(0, 20)
)

bolus_limits <- list(
  eph = c(0, 100),
  epi = c(0, 1000),
  phe = c(0, 5000),
  vaso = c(0, 20),
  mdz = c(0, 100),
  ftn = c(0, 5000),
  sft = c(0, 1000),
  aft = c(0, 5000),
  ppf = c(0, 5000)
)

rate_limits <- list(
  nepi = c(0, 5),
  epii = c(0, 5),
  dobui = c(0, 50),
  dopai = c(0, 50),
  mlni = c(0, 2),
  ntgi = c(0, 20),
  pepi = c(0, 5000)
)

target_limits <- list(
  ppfi = c(0, 20),
  rfti = c(0, 20)
)

all_limits <- c(monitor_limits, gas_limits, fluid_limits, blood_limits, bolus_limits, rate_limits, target_limits)

get_variable_group <- function(v) {
  if (v %in% group_id_time) return("id_time")
  if (v %in% group_hemo_monitor) return("hemodynamic_respiratory_monitoring")
  if (v %in% group_gas_volatile) return("medical_gas_and_volatile_anesthetic")
  if (v %in% group_fluids_blood_io) return("fluids_blood_products_input_output")
  if (v %in% group_bolus) return("intermittent_bolus_drugs")
  if (v %in% group_rate) return("continuous_infusion_rate_drugs")
  if (v %in% group_target) return("target_concentration")
  if (v %in% group_unmapped) return("unmapped_review")
  "unclassified"
}

get_summary_role <- function(v) {
  if (v %in% group_hemo_monitor) return("monitoring_distribution")
  if (v %in% group_gas_volatile) return("gas_distribution")
  if (v %in% group_fluids_blood_io) return("volume_or_units_sum")
  if (v %in% group_bolus) return("bolus_sum")
  if (v %in% group_rate) return("infusion_rate_distribution")
  if (v %in% group_target) return("target_concentration_distribution")
  if (v %in% group_unmapped) return("review_only")
  if (v %in% group_id_time) return("identifier")
  "other"
}

fmt_threshold <- function(v) {
  lim <- all_limits[[v]]
  if (is.null(lim)) return(NA_character_)
  sprintf("[%s, %s]", lim[1], lim[2])
}

last_nonmissing <- function(x) {
  idx <- which(!is.na(x))
  if (!length(idx)) return(NA_real_)
  x[idx[length(idx)]]
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_median <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  median(x)
}

safe_positive_median <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (!length(x)) return(NA_real_)
  median(x)
}

safe_positive_quantile <- function(x, prob = 0.75) {
  x <- x[!is.na(x) & x > 0]
  if (!length(x)) return(NA_real_)
  as.numeric(quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

sample_values <- function(x, n = 20L) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  ux <- unique(x)
  ux <- ux[seq_len(min(length(ux), n))]
  paste(ux, collapse = "|")
}

flag_outliers <- function(v, x) {
  y <- as.numeric(x)
  out <- rep(FALSE, length(y))
  lim <- all_limits[[v]]
  if (!is.null(lim)) {
    out <- (!is.na(y)) & (y < lim[1] | y > lim[2])
  }
  if (v %in% names(blood_limits)) {
    out <- out | ((!is.na(y)) & (abs(y - round(y)) > 1e-8))
  }
  out
}

cat("Loading intraoperative first_nonMAC timeseries ...\n")
dt <- fread(input_file, na.strings = c("", "NA", "NULL", "(Null)", "null"), showProgress = TRUE)
params <- fread(params_file, encoding = "UTF-8")
params_vitals <- params[tolower(Table) == "vitals", .(Label, Table, Unit, Description)]

all_cols <- names(dt)
variable_cols <- setdiff(all_cols, id_time_cols)
mapped_labels <- params_vitals$Label

dict_dt <- data.table(variable = all_cols)
dict_dt[, `:=`(
  mapped_flag = variable %in% mapped_labels,
  variable_group = vapply(variable, get_variable_group, character(1)),
  summary_role = vapply(variable, get_summary_role, character(1))
)]
dict_dt <- merge(
  dict_dt,
  params_vitals,
  by.x = "variable",
  by.y = "Label",
  all.x = TRUE,
  sort = FALSE
)
setcolorder(dict_dt, c("variable", "mapped_flag", "Table", "Unit", "Description", "variable_group", "summary_role"))
dict_dt[variable %in% id_time_cols, mapped_flag := TRUE]
dict_dt[variable %in% id_time_cols, Table := "derived"]
dict_dt[variable %in% id_time_cols, Unit := fifelse(variable == "min_from_entry", "min", NA_character_)]
dict_dt[variable %in% id_time_cols, Description := c(
  "INSPIRE subject identifier",
  "INSPIRE hospital admission identifier",
  "Anchor first non-MAC operation identifier",
  "Original within-subject surgery sequence number",
  "Chart time within operation source table",
  "Minutes from OR entry / orin_time"
)]
dict_dt[, outlier_threshold := vapply(variable, fmt_threshold, character(1))]
dict_dt[, note := fifelse(variable %in% group_unmapped, "Review only; not used in main summaries.", NA_character_)]
fwrite(dict_dt, file.path(out_dir, "timeseries_variable_dictionary_aligned.csv"))

unmapped_vars <- dict_dt[variable_group == "unmapped_review", variable]
mapped_qc_vars <- dict_dt[!(variable_group %in% c("id_time", "unmapped_review")), variable]

threshold_rows <- rbindlist(lapply(names(all_limits), function(v) {
  data.table(
    variable = v,
    variable_group = get_variable_group(v),
    outlier_low = all_limits[[v]][1],
    outlier_high = all_limits[[v]][2],
    special_rule = if (v %in% names(blood_limits)) "flag non-integer and out-of-range" else "range flag"
  )
}), use.names = TRUE, fill = TRUE)
fwrite(threshold_rows, file.path(out_dir, "timeseries_outlier_thresholds.csv"))

cat("Building overall QC table ...\n")
qc_rows <- rbindlist(lapply(variable_cols, function(v) {
  x <- dt[[v]]
  x_num <- as.numeric(x)
  nonmiss <- x_num[!is.na(x_num)]
  out_flag <- flag_outliers(v, x_num)
  n_total <- length(x_num)
  n_nonmissing <- length(nonmiss)
  n_missing <- n_total - n_nonmissing
  n_nonzero <- sum(nonmiss != 0)
  n_negative <- sum(nonmiss < 0)
  n_unique <- uniqueN(nonmiss)
  outlier_n <- sum(out_flag, na.rm = TRUE)
  qvals <- if (n_nonmissing > 0L) quantile(nonmiss, probs = c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm = TRUE, names = FALSE) else rep(NA_real_, 5)
  data.table(
    variable = v,
    variable_group = get_variable_group(v),
    summary_role = get_summary_role(v),
    mapped_flag = v %in% mapped_labels,
    unit = dict_dt[variable == v, Unit][1],
    description = dict_dt[variable == v, Description][1],
    n_total = n_total,
    n_nonmissing = n_nonmissing,
    n_missing = n_missing,
    missing_pct = round(100 * n_missing / n_total, 4),
    n_nonzero = n_nonzero,
    nonzero_pct = round(100 * n_nonzero / n_total, 4),
    n_negative = n_negative,
    n_unique = n_unique,
    min_value = if (n_nonmissing > 0L) min(nonmiss) else NA_real_,
    p01 = qvals[1],
    p05 = qvals[2],
    p50 = qvals[3],
    p95 = qvals[4],
    p99 = qvals[5],
    max_value = if (n_nonmissing > 0L) max(nonmiss) else NA_real_,
    outlier_flag_count = outlier_n,
    outlier_flag_pct = round(100 * outlier_n / n_total, 4),
    outlier_threshold = fmt_threshold(v)
  )
}), use.names = TRUE, fill = TRUE)
setorder(qc_rows, variable_group, variable)
fwrite(qc_rows, file.path(out_dir, "timeseries_overall_qc.csv"))

cat("Building unmapped variable review ...\n")
unmapped_review <- rbindlist(lapply(unmapped_vars, function(v) {
  x <- as.numeric(dt[[v]])
  nonmiss <- x[!is.na(x)]
  qvals <- if (length(nonmiss) > 0L) quantile(nonmiss, probs = c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm = TRUE, names = FALSE) else rep(NA_real_, 5)
  data.table(
    variable = v,
    n_total = length(x),
    n_nonmissing = length(nonmiss),
    missing_pct = round(100 * (length(x) - length(nonmiss)) / length(x), 4),
    n_nonzero = sum(nonmiss != 0),
    min_value = if (length(nonmiss) > 0L) min(nonmiss) else NA_real_,
    p01 = qvals[1],
    p05 = qvals[2],
    p50 = qvals[3],
    p95 = qvals[4],
    p99 = qvals[5],
    max_value = if (length(nonmiss) > 0L) max(nonmiss) else NA_real_,
    sample_nonmissing_values = sample_values(nonmiss, n = 20L)
  )
}), use.names = TRUE, fill = TRUE)
fwrite(unmapped_review, file.path(out_dir, "unmapped_variables_review.csv"))

cat("Building case-level summaries ...\n")
op_base <- unique(dt[, .(op_id, subject_id, hadm_id, surgery_number)])
case_den <- dt[, .(total_timepoints = .N), by = .(op_id)]
op_base <- merge(op_base, case_den, by = "op_id", all.x = TRUE)
setorder(op_base, subject_id, hadm_id, surgery_number, op_id)

case_summary_file <- file.path(out_dir, "timeseries_case_level_summary.csv")
missing_case_file <- file.path(out_dir, "timeseries_missingness_by_case.csv")
if (file.exists(case_summary_file)) file.remove(case_summary_file)
if (file.exists(missing_case_file)) file.remove(missing_case_file)

case_variable_rollup <- vector("list", length(priority_vars))

for (i in seq_along(priority_vars)) {
  v <- priority_vars[i]
  cat(sprintf("  [%d/%d] %s\n", i, length(priority_vars), v))
  sub_dt <- dt[, .(op_id, value = as.numeric(get(v)))]
  case_dt <- sub_dt[, .(
    n_nonmissing_timepoints = sum(!is.na(value)),
    n_positive_timepoints = sum(value > 0, na.rm = TRUE),
    any_record_flag = as.integer(any(!is.na(value))),
    any_use_flag = as.integer(any(value > 0, na.rm = TRUE)),
    sum_value = if (v %in% summary_mode_sum) sum(value, na.rm = TRUE) else NA_real_,
    max_value = if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
    last_value = last_nonmissing(value),
    median_value = if (v %in% summary_mode_distribution) safe_median(value) else NA_real_,
    mean_value = if (v %in% summary_mode_distribution) safe_mean(value) else NA_real_
  ), by = .(op_id)]

  case_dt <- merge(op_base, case_dt, by = "op_id", all.x = TRUE, sort = FALSE)
  case_dt[, `:=`(
    variable = v,
    variable_group = get_variable_group(v),
    summary_role = get_summary_role(v),
    total_timepoints = fifelse(is.na(total_timepoints), 0L, total_timepoints),
    missing_timepoints = fifelse(is.na(total_timepoints), NA_integer_, total_timepoints - n_nonmissing_timepoints),
    missing_pct = fifelse(total_timepoints > 0, round(100 * (total_timepoints - n_nonmissing_timepoints) / total_timepoints, 4), NA_real_)
  )]
  setcolorder(case_dt, c(
    "subject_id", "hadm_id", "op_id", "surgery_number",
    "variable", "variable_group", "summary_role",
    "total_timepoints", "n_nonmissing_timepoints", "missing_timepoints", "missing_pct",
    "any_record_flag", "any_use_flag", "n_positive_timepoints",
    "sum_value", "max_value", "last_value", "median_value", "mean_value"
  ))
  fwrite(case_dt, case_summary_file, append = i > 1L)
  fwrite(
    case_dt[, .(
      subject_id, hadm_id, op_id, surgery_number,
      variable, variable_group,
      total_timepoints, n_nonmissing_timepoints, missing_timepoints, missing_pct, any_record_flag
    )],
    missing_case_file,
    append = i > 1L
  )

  case_variable_rollup[[i]] <- case_dt[, .(
    n_cases = .N,
    record_cases = sum(any_record_flag == 1, na.rm = TRUE),
    use_cases = sum(any_use_flag == 1, na.rm = TRUE),
    median_missing_pct = median(missing_pct, na.rm = TRUE),
    p75_missing_pct = quantile(missing_pct, probs = 0.75, na.rm = TRUE, names = FALSE),
    median_positive_metric = if (v %in% summary_mode_sum) safe_positive_median(sum_value) else safe_positive_median(mean_value),
    p75_positive_metric = if (v %in% summary_mode_sum) safe_positive_quantile(sum_value, prob = 0.75) else safe_positive_quantile(mean_value, prob = 0.75)
  )][, `:=`(
    variable = v,
    variable_group = get_variable_group(v),
    summary_role = get_summary_role(v),
    record_case_pct = round(100 * record_cases / n_cases, 4),
    use_case_pct = round(100 * use_cases / n_cases, 4)
  )]
}

case_variable_rollup_dt <- rbindlist(case_variable_rollup, use.names = TRUE, fill = TRUE)
setorder(case_variable_rollup_dt, variable_group, variable)
fwrite(case_variable_rollup_dt, file.path(out_dir, "timeseries_case_variable_rollup.csv"))

run_summary <- data.table(
  input_file = input_file,
  params_file = params_file,
  output_dir = out_dir,
  n_rows = nrow(dt),
  n_ops = uniqueN(dt$op_id),
  n_subjects = uniqueN(dt$subject_id),
  n_hadm = uniqueN(dt$hadm_id),
  n_total_columns = ncol(dt),
  n_variable_columns = length(variable_cols),
  n_mapped_vitals = length(intersect(variable_cols, mapped_labels)),
  n_unmapped_review = length(unmapped_vars)
)
fwrite(run_summary, file.path(out_dir, "timeseries_qc_run_summary.csv"))

cat("Building plots ...\n")
priority_plot_dt <- case_variable_rollup_dt[variable_group %in% c(
  "fluids_blood_products_input_output",
  "intermittent_bolus_drugs",
  "continuous_infusion_rate_drugs",
  "target_concentration",
  "hemodynamic_respiratory_monitoring"
)]

qc_plot_dt <- qc_rows[variable %in% priority_vars]
qc_plot_dt[, variable := factor(variable, levels = qc_plot_dt[order(missing_pct, decreasing = TRUE), variable])]
p_missing <- ggplot(qc_plot_dt, aes(x = variable, y = missing_pct, fill = variable_group)) +
  geom_col(width = 0.8) +
  coord_flip() +
  labs(title = "Priority Variables: Missingness by Timepoint", x = NULL, y = "Missing (%)") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
ggsave(file.path(plot_dir, "priority_variable_missingness_overall.png"), p_missing, width = 10, height = 12, dpi = 300)

record_use_dt <- melt(
  priority_plot_dt[, .(variable, variable_group, record_case_pct, use_case_pct)],
  id.vars = c("variable", "variable_group"),
  variable.name = "metric",
  value.name = "pct"
)
record_use_dt[, metric := factor(metric, levels = c("record_case_pct", "use_case_pct"), labels = c("Recorded in case", "Used in case"))]
record_use_dt[, variable := factor(variable, levels = priority_plot_dt[order(use_case_pct, decreasing = TRUE), variable])]
p_record_use <- ggplot(record_use_dt, aes(x = variable, y = pct, fill = metric)) +
  geom_col(position = "dodge", width = 0.75) +
  coord_flip() +
  labs(title = "Priority Variables: Case-level Recording vs Use", x = NULL, y = "Cases (%)") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
ggsave(file.path(plot_dir, "priority_variable_case_record_vs_use.png"), p_record_use, width = 10, height = 12, dpi = 300)

case_summary_plot <- fread(case_summary_file, select = c("variable", "variable_group", "summary_role", "sum_value", "mean_value"))
case_summary_plot[, plot_value := fifelse(summary_role %in% c("volume_or_units_sum", "bolus_sum"), sum_value, mean_value)]
case_summary_plot <- case_summary_plot[!is.na(plot_value) & plot_value > 0]
if (nrow(case_summary_plot) > 0L) {
  selected_plot_vars <- unique(c(
    intersect(c("ns", "hs", "rbc", "ffp", "ebl", "uo"), unique(case_summary_plot$variable)),
    intersect(c("nepi", "pepi", "vaso", "ppf", "ftn", "ppfi", "rfti"), unique(case_summary_plot$variable))
  ))
  plot_dist_dt <- case_summary_plot[variable %in% selected_plot_vars]
  if (nrow(plot_dist_dt) > 0L) {
    plot_dist_dt[, variable := factor(variable, levels = selected_plot_vars)]
    p_dist <- ggplot(plot_dist_dt, aes(x = variable, y = plot_value)) +
      geom_boxplot(outlier.size = 0.3) +
      scale_y_log10() +
      coord_flip() +
      labs(title = "Selected Priority Variables: Positive Case-level Distribution", x = NULL, y = "Value (log10 scale)") +
      theme_bw(base_size = 10)
    ggsave(file.path(plot_dir, "selected_priority_variable_distribution_log10.png"), p_dist, width = 9, height = 6, dpi = 300)
  }
}

points_per_case <- op_base[, .(op_id, total_timepoints)]
p_points <- ggplot(points_per_case, aes(x = total_timepoints)) +
  geom_histogram(bins = 50, fill = "#4C78A8", color = "white") +
  labs(title = "Timepoints per Case", x = "Number of timepoints", y = "Number of cases") +
  theme_bw(base_size = 10)
ggsave(file.path(plot_dir, "timepoints_per_case_histogram.png"), p_points, width = 8, height = 5, dpi = 300)

cat("Done: Vials_intra_timeseries_qc_v1_first_nonMAC.R\n")
cat(sprintf("Outputs written to: %s\n", out_dir))
