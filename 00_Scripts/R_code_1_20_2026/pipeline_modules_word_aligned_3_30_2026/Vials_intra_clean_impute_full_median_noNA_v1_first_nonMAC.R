suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_full_complete_median_grouped_columns.csv")
anchor_file <- file.path(processed_path, "anchor_first_nonMAC_operations.csv")
operations_file <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/operations.csv"

clean_file <- file.path(processed_path, "vital_intraop_full_complete_median_clean.csv")
screening_plan_file <- file.path(processed_path, "vital_intraop_full_complete_median_outlier_screening_plan.csv")
imputation_plan_file <- file.path(processed_path, "vital_intraop_full_complete_median_imputation_plan.csv")
final_file <- file.path(processed_path, "vital_intraop_full_complete_median_final_noNA_with_flags.csv")
flag_dictionary_file <- file.path(processed_path, "vital_intraop_full_complete_median_final_noNA_flag_dictionary.csv")
method_notes_file <- file.path(processed_path, "vital_intraop_full_complete_median_method_notes.md")
qc_summary_file <- file.path(processed_path, "vital_intraop_full_complete_median_qc_summary.csv")

if (!file.exists(input_file)) stop("Input file not found: ", input_file)
if (!file.exists(anchor_file)) stop("Anchor file not found: ", anchor_file)
if (!file.exists(operations_file)) stop("Operations file not found: ", operations_file)

id_cols <- c("subject_id", "hadm_id", "op_id", "surgery_number", "chart_time", "min_from_entry")
bt_low_special_values <- c(20.6, 22.0, 27.6)
merged_bp_cols <- c("sbp_merged", "mbp_merged", "dbp_merged")

variable_plan <- rbindlist(list(
  data.table(
    variable = c("art_sbp", "art_mbp", "art_dbp", "nibp_sbp", "nibp_mbp", "nibp_dbp", "pap_sbp", "pap_mbp", "pap_dbp", "hr", "cvp", "ci", "svi", "bt"),
    column_group = "hemodynamics",
    unit = c(rep("mmHg", 9), "/min", "mmHg", "L/min/m2", "mL/m2", "Celsius"),
    strategy = "continuous_monitor",
    keep_zero = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
    lower = c(20, 10, 0, 20, 10, 0, 0, 0, 0, 20, -5, 0.5, 5, 30),
    upper = c(300, 250, 200, 300, 250, 200, 150, 100, 80, 220, 40, 10, 150, 43),
    fill_rule = "Prediction-safe filling: before the first observed value use the global median; after observation starts, use forward fill only; if the entire case is missing, use global median."
  ),
  data.table(
    variable = c("rr", "spo2", "etco2", "fio2", "vt", "minvol", "o2", "air", "peep", "pip", "pmean", "pplat", "cbro2"),
    column_group = "respiratory_ventilation",
    unit = c("/min", "%", "mmHg", "%", "mL", "L/min", "L/min", "L/min", "cmH2O", "cmH2O", "cmH2O", "cmH2O", "%"),
    strategy = "continuous_monitor",
    keep_zero = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    lower = c(2, 30, 5, 21, 10, 0.1, 0, 0, 0, 0, 0, 0, 15),
    upper = c(60, 100, 100, 100, 3000, 60, 15, 15, 30, 60, 60, 60, 100),
    fill_rule = "Prediction-safe filling: before the first observed value use the global median; after observation starts, use forward fill only; if the entire case is missing, use global median."
  ),
  data.table(
    variable = c("bis", "etgas", "etdes", "etiso", "etsevo", "n2o"),
    column_group = "anesthesia_sedation",
    unit = c("", "vol%", "vol%", "vol%", "vol%", "L/min"),
    strategy = "continuous_monitor",
    keep_zero = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    lower = c(0, 0, 0, 0, 0, 0),
    upper = c(100, 10, 18, 5, 8, 15),
    fill_rule = "Prediction-safe filling: before the first observed value use the global median; after observation starts, use forward fill only; if the entire case is missing, use global median."
  ),
  data.table(
    variable = c("ppfi", "rfti"),
    column_group = "anesthesia_sedation",
    unit = c("ug/mL", "ng/mL"),
    strategy = "tci_state",
    keep_zero = TRUE,
    lower = c(2.5, 0.5),
    upper = c(10, 20),
    fill_rule = "Before first positive fill 0; explicit 0 stays 0; otherwise carry forward to next update or OR end."
  ),
  data.table(
    variable = c("ppf", "ftn", "aft", "sft", "mdz"),
    column_group = "anesthesia_sedation",
    unit = c("mg", "ug", "ug", "ug", "mg"),
    strategy = "event_zero_fill",
    keep_zero = TRUE,
    lower = c(0, 0, 0, 0, 0),
    upper = c(1000, 5000, 10000, 1000, 50),
    fill_rule = "Negative or implausibly large event doses set to NA, then missing values filled with 0."
  ),
  data.table(
    variable = c("epi", "phe", "eph", "vaso"),
    column_group = "vasoactive_drugs",
    unit = c("ug", "ug", "mg", "Unit"),
    strategy = "event_zero_fill",
    keep_zero = TRUE,
    lower = c(0, 0, 0, 0),
    upper = c(5000, 5000, 200, 100),
    fill_rule = "Negative or implausibly large bolus doses set to NA, then missing values filled with 0."
  ),
  data.table(
    variable = c("epii", "nepi", "pepi", "dopai", "dobui", "mlni", "ntgi"),
    column_group = "vasoactive_drugs",
    unit = c("ug/kg/min", "ug/kg/min", "ug/h", "ug/kg/min", "ug/kg/min", "ug/kg/min", "ug/kg/min"),
    strategy = "infusion_state_noNA",
    keep_zero = TRUE,
    lower = c(0.01, 0.005, 10, 1.5, 2, 0.25, 0.05),
    upper = c(10, 3, 3000, 30, 30, 10, 10),
    fill_rule = "Before first positive fill 0; explicit 0 stays 0; once infusion starts, carry the last positive rate forward to OR end unless an explicit 0 is observed."
  ),
  data.table(
    variable = c("ebl", "uo", "ns", "hns", "hs", "psa", "d5w", "d10w", "d50w", "hes", "alb5", "alb20"),
    column_group = c(rep("fluids_output", 12)),
    unit = c(rep("mL", 12)),
    strategy = "event_zero_fill",
    keep_zero = TRUE,
    lower = rep(0, 12),
    upper = c(10000, 10000, 10000, 10000, 10000, 10000, 5000, 5000, 5000, 10000, 5000, 5000),
    fill_rule = "Negative or implausibly large event quantities set to NA, then missing values filled with 0."
  ),
  data.table(
    variable = c("rbc", "ffp", "pc", "pheresis", "cryo"),
    column_group = "blood_products",
    unit = "Unit",
    strategy = "event_zero_fill",
    keep_zero = TRUE,
    lower = rep(0, 5),
    upper = rep(20, 5),
    fill_rule = "Negative or implausibly large blood product units set to NA, then missing values filled with 0."
  ),
  data.table(
    variable = c("sti", "stii", "stiii", "stv5"),
    column_group = "ecg_st_segment",
    unit = "mV",
    strategy = "continuous_monitor",
    keep_zero = TRUE,
    lower = rep(-10, 4),
    upper = rep(10, 4),
    fill_rule = "Prediction-safe filling: before the first observed value use the global median; after observation starts, use forward fill only; if the entire case is missing, use global median."
  ),
  data.table(
    variable = "cpat",
    column_group = "review_unmapped",
    unit = "raw_source_unit",
    strategy = "global_median_fill",
    keep_zero = TRUE,
    lower = 0,
    upper = 1000,
    fill_rule = "Out-of-range values set to NA; all missing values then filled with the global median."
  ),
  data.table(
    variable = "ds",
    column_group = "review_unmapped",
    unit = "mL",
    strategy = "event_zero_fill",
    keep_zero = TRUE,
    lower = 0,
    upper = 10000,
    fill_rule = "Negative or implausibly large quantities set to NA, then missing values filled with 0."
  )
), use.names = TRUE)

continuous_fill <- function(x, time, global_value) {
  out <- x
  flag <- as.integer(is.na(x))
  obs_idx <- which(!is.na(x))
  fill_val <- ifelse(is.na(global_value), 0, global_value)

  if (length(obs_idx) == 0L) {
    return(list(value = rep(fill_val, length(x)), flag = rep(1L, length(x))))
  }

  first_obs <- obs_idx[1]
  if (first_obs > 1L) {
    out[seq_len(first_obs - 1L)] <- fill_val
  }

  if (first_obs < length(x)) {
    tail_idx <- seq.int(first_obs + 1L, length(x))
    if (length(tail_idx) > 0L) {
      locf_source <- out[first_obs:length(x)]
      locf_filled <- nafill(locf_source, type = "locf")
      out[first_obs:length(x)] <- locf_filled
    }
  }

  remaining_na <- is.na(out)
  if (any(remaining_na)) {
    out[remaining_na] <- fill_val
  }

  list(value = out, flag = flag)
}

continuous_fill_with_source <- function(x, source, global_value) {
  out <- x
  out_source <- source
  flag <- integer(length(x))
  fill_val <- ifelse(is.na(global_value), 0, global_value)
  obs_idx <- which(!is.na(x))

  if (length(obs_idx) == 0L) {
    return(list(
      value = rep(fill_val, length(x)),
      flag = rep(1L, length(x)),
      source = rep("GLOBAL_MEDIAN", length(x))
    ))
  }

  first_obs <- obs_idx[1]
  if (first_obs > 1L) {
    idx <- seq_len(first_obs - 1L)
    out[idx] <- fill_val
    out_source[idx] <- "GLOBAL_MEDIAN"
    flag[idx] <- 1L
  }

  out[first_obs] <- x[first_obs]
  out_source[first_obs] <- source[first_obs]
  flag[first_obs] <- 0L

  if (first_obs < length(x)) {
    for (i in seq.int(first_obs + 1L, length(x))) {
      if (!is.na(x[i])) {
        out[i] <- x[i]
        out_source[i] <- source[i]
        flag[i] <- 0L
      } else {
        out[i] <- out[i - 1L]
        out_source[i] <- "LOCF"
        flag[i] <- 1L
      }
    }
  }

  remaining_na <- is.na(out)
  if (any(remaining_na)) {
    out[remaining_na] <- fill_val
    out_source[remaining_na] <- "GLOBAL_MEDIAN"
    flag[remaining_na] <- 1L
  }

  list(value = out, flag = flag, source = out_source)
}

is_valid_bp_triplet <- function(sbp, mbp, dbp) {
  !is.na(sbp) & !is.na(mbp) & !is.na(dbp) & sbp >= mbp & mbp >= dbp
}

compute_bp_triplet_global <- function(sbp, mbp, dbp) {
  valid <- is_valid_bp_triplet(sbp, mbp, dbp)
  if (!any(valid)) {
    return(list(sbp = 120, mbp = 80, dbp = 60))
  }

  out <- list(
    sbp = median(sbp[valid]),
    mbp = median(mbp[valid]),
    dbp = median(dbp[valid])
  )

  # Guard against pathological columnwise medians that break triplet ordering.
  out$sbp <- max(out$sbp, out$mbp, out$dbp)
  out$dbp <- min(out$sbp, out$mbp, out$dbp)
  out$mbp <- min(out$sbp, max(out$mbp, out$dbp))
  out
}

merge_bp_triplet <- function(art_sbp, art_mbp, art_dbp, nibp_sbp, nibp_mbp, nibp_dbp) {
  art_valid <- is_valid_bp_triplet(art_sbp, art_mbp, art_dbp)
  nibp_valid <- is_valid_bp_triplet(nibp_sbp, nibp_mbp, nibp_dbp)

  source <- ifelse(art_valid, "ART", ifelse(nibp_valid, "NIBP", NA_character_))
  sbp <- ifelse(art_valid, art_sbp, ifelse(nibp_valid, nibp_sbp, NA_real_))
  mbp <- ifelse(art_valid, art_mbp, ifelse(nibp_valid, nibp_mbp, NA_real_))
  dbp <- ifelse(art_valid, art_dbp, ifelse(nibp_valid, nibp_dbp, NA_real_))

  list(sbp = sbp, mbp = mbp, dbp = dbp, source = source)
}

fill_bp_triplet_with_source <- function(sbp, mbp, dbp, source, global_triplet) {
  n <- length(sbp)
  out_sbp <- sbp
  out_mbp <- mbp
  out_dbp <- dbp
  out_source <- source
  flag <- integer(n)
  obs_idx <- which(is_valid_bp_triplet(sbp, mbp, dbp))

  if (length(obs_idx) == 0L) {
    return(list(
      sbp = rep(global_triplet$sbp, n),
      mbp = rep(global_triplet$mbp, n),
      dbp = rep(global_triplet$dbp, n),
      flag = rep(1L, n),
      source = rep("GLOBAL_MEDIAN", n)
    ))
  }

  first_obs <- obs_idx[1]
  if (first_obs > 1L) {
    idx <- seq_len(first_obs - 1L)
    out_sbp[idx] <- global_triplet$sbp
    out_mbp[idx] <- global_triplet$mbp
    out_dbp[idx] <- global_triplet$dbp
    out_source[idx] <- "GLOBAL_MEDIAN"
    flag[idx] <- 1L
  }

  out_sbp[first_obs] <- sbp[first_obs]
  out_mbp[first_obs] <- mbp[first_obs]
  out_dbp[first_obs] <- dbp[first_obs]
  out_source[first_obs] <- source[first_obs]
  flag[first_obs] <- 0L

  if (first_obs < n) {
    for (i in seq.int(first_obs + 1L, n)) {
      if (is_valid_bp_triplet(sbp[i], mbp[i], dbp[i])) {
        out_sbp[i] <- sbp[i]
        out_mbp[i] <- mbp[i]
        out_dbp[i] <- dbp[i]
        out_source[i] <- source[i]
        flag[i] <- 0L
      } else {
        out_sbp[i] <- out_sbp[i - 1L]
        out_mbp[i] <- out_mbp[i - 1L]
        out_dbp[i] <- out_dbp[i - 1L]
        out_source[i] <- "LOCF"
        flag[i] <- 1L
      }
    }
  }

  invalid_after_fill <- !is_valid_bp_triplet(out_sbp, out_mbp, out_dbp)
  if (any(invalid_after_fill)) {
    out_sbp[invalid_after_fill] <- global_triplet$sbp
    out_mbp[invalid_after_fill] <- global_triplet$mbp
    out_dbp[invalid_after_fill] <- global_triplet$dbp
    out_source[invalid_after_fill] <- "GLOBAL_MEDIAN"
    flag[invalid_after_fill] <- 1L
  }

  list(sbp = out_sbp, mbp = out_mbp, dbp = out_dbp, flag = flag, source = out_source)
}

fill_bp_triplet <- function(sbp, mbp, dbp, global_triplet) {
  res <- fill_bp_triplet_with_source(
    sbp = sbp,
    mbp = mbp,
    dbp = dbp,
    source = rep("OBSERVED", length(sbp)),
    global_triplet = global_triplet
  )
  list(sbp = res$sbp, mbp = res$mbp, dbp = res$dbp, flag = res$flag)
}

tci_state_fill <- function(x) {
  out <- numeric(length(x))
  flag <- integer(length(x))
  started <- FALSE
  last_val <- 0

  for (i in seq_along(x)) {
    xi <- x[i]
    if (!is.na(xi)) {
      out[i] <- xi
      flag[i] <- 0L
      if (xi > 0) started <- TRUE
      last_val <- xi
    } else if (!started) {
      out[i] <- 0
      flag[i] <- 1L
    } else {
      out[i] <- last_val
      flag[i] <- 1L
    }
  }
  list(value = out, flag = flag)
}

infusion_state_fill <- function(x, time, orout_time, cap_minutes = 60) {
  n <- length(x)
  out <- numeric(length(x))
  flag <- integer(length(x))
  started <- FALSE
  last_val <- 0

  for (i in seq_along(x)) {
    xi <- x[i]

    if (!is.na(xi)) {
      out[i] <- xi
      flag[i] <- 0L
      if (xi > 0) started <- TRUE
      last_val <- xi
      next
    }

    if (!started) {
      out[i] <- 0
      flag[i] <- 1L
      next
    }

    if (last_val == 0) {
      out[i] <- 0
      flag[i] <- 1L
      next
    }

    out[i] <- last_val
    flag[i] <- 1L
  }

  list(value = out, flag = flag)
}

event_zero_fill <- function(x) {
  flag <- as.integer(is.na(x))
  x[is.na(x)] <- 0
  list(value = x, flag = flag)
}

global_median_fill <- function(x, global_value) {
  fill_val <- ifelse(is.na(global_value), 0, global_value)
  flag <- as.integer(is.na(x))
  x[is.na(x)] <- fill_val
  list(value = x, flag = flag)
}

clean_bt_case <- function(x, has_cpb_case) {
  y <- x
  if (!isTRUE(has_cpb_case)) {
    y[!is.na(y) & (y < 30 | y > 43)] <- NA_real_
    return(y)
  }

  y[!is.na(y) & y > 43] <- NA_real_

  special_mask <- !is.na(y) & y %in% bt_low_special_values
  other_low_mask <- !is.na(y) & y < 30 & !special_mask
  y[other_low_mask] <- NA_real_

  normal_idx <- which(!is.na(y) & y > 30 & y <= 43)
  special_idx <- which(special_mask)

  if (length(special_idx) == 0L) {
    return(y)
  }

  if (length(normal_idx) == 0L) {
    y[special_idx] <- NA_real_
    return(y)
  }

  first_normal <- min(normal_idx)
  last_normal <- max(normal_idx)
  outside_idx <- special_idx[special_idx < first_normal | special_idx > last_normal]
  if (length(outside_idx) > 0L) {
    y[outside_idx] <- NA_real_
  }

  y
}

merge_bp_component <- function(primary, fallback) {
  source <- ifelse(!is.na(primary), "ART", ifelse(!is.na(fallback), "NIBP", NA_character_))
  value <- primary
  value[is.na(value)] <- fallback[is.na(value)]
  list(value = value, source = source)
}

dt <- fread(input_file, showProgress = TRUE)
anchor_dt <- fread(anchor_file, select = c("op_id", "orout_time"), showProgress = FALSE)
anchor_dt[, orout_time := as.numeric(orout_time)]
dt <- merge(dt, anchor_dt, by = "op_id", all.x = TRUE)
ops_dt <- fread(operations_file, select = c("op_id", "cpbon_time", "cpboff_time"), showProgress = FALSE)
ops_dt[, has_cpb := !is.na(cpbon_time) & !is.na(cpboff_time)]
dt <- merge(dt, ops_dt[, .(op_id, has_cpb)], by = "op_id", all.x = TRUE)
dt[is.na(has_cpb), has_cpb := FALSE]
setorder(dt, subject_id, hadm_id, surgery_number, chart_time, op_id)

available_vars <- intersect(variable_plan$variable, names(dt))
variable_plan <- variable_plan[variable %in% available_vars]

global_medians <- dt[, lapply(.SD, function(x) {
  nonmiss <- x[!is.na(x)]
  if (length(nonmiss) == 0L) return(NA_real_)
  median(nonmiss)
}), .SDcols = available_vars]
global_median_map <- as.list(global_medians[1])

screening_plan <- copy(variable_plan)[, .(
  variable, column_group, unit, strategy, keep_zero, lower, upper,
  cleaning_rule = fifelse(
    keep_zero,
    sprintf("Keep 0 or values within [%.3f, %.3f]; set negative/out-of-range values to NA before imputation.", lower, upper),
    sprintf("Keep values within [%.3f, %.3f]; set out-of-range values to NA before imputation.", lower, upper)
  )
)]
screening_plan[variable %in% c("art_sbp", "art_dbp"), cleaning_rule :=
  "Retain values unless the same-time ART triplet violates art_sbp >= art_mbp >= art_dbp; if ART triplet conflict occurs, set art_sbp/art_mbp/art_dbp to NA together."
]
screening_plan[variable == "art_mbp", cleaning_rule :=
  "Set art_mbp = 6 to NA. If the same-time ART triplet violates art_sbp >= art_mbp >= art_dbp, set art_sbp/art_mbp/art_dbp to NA together. In non-CPB cases, isolated art_mbp without art_sbp/art_dbp is excluded from merged MBP fallback logic."
]
screening_plan[variable == "etco2", cleaning_rule :=
  "Set etco2 = 0 or any value outside [5.000, 100.000] to NA before imputation."
]
screening_plan[variable == "pmean", cleaning_rule :=
  "Set pmean = 0, 1 or 3 to NA before imputation; other values outside [0.000, 60.000] are also set to NA."
]
screening_plan[variable == "pip", cleaning_rule :=
  "Set pip = 1 to NA before imputation; other values outside [0.000, 60.000] are also set to NA."
]
screening_plan[variable == "bt", cleaning_rule :=
  "Non-CPB cases: keep values within [30.000, 43.000]; set 20.6/22.0/27.6 and other out-of-range values to NA. CPB cases: keep values within [30.000, 43.000] and also allow 20.6/22.0/27.6 only between the first and last >30 Celsius observations within the case; set all other out-of-range values to NA before imputation."
]
screening_plan[variable == "cbro2", cleaning_rule :=
  "Keep values within [15.000, 100.000]; set all values <15 or >100 to NA before imputation."
]
screening_plan[variable %in% c("pap_sbp", "pap_dbp"), cleaning_rule :=
  "If pap_mbp = 1 or 2 at the same time point, set pap_sbp/pap_mbp/pap_dbp to NA together. If the same-time PAP triplet violates pap_sbp >= pap_mbp >= pap_dbp, set pap_sbp/pap_mbp/pap_dbp to NA together."
]
screening_plan[variable == "pap_mbp", cleaning_rule :=
  "Set the entire PAP triplet to NA when pap_mbp = 1 or 2. Also set the entire PAP triplet to NA when the same-time PAP triplet violates pap_sbp >= pap_mbp >= pap_dbp."
]
screening_plan[variable == "pepi", cleaning_rule :=
  "Set pepi = 0.25 or 10 to NA before infusion-state imputation; values outside [10.000, 3000.000] except explicit 0 are also set to NA."
]
screening_plan[variable == "mlni", cleaning_rule :=
  "Set mlni = 9.93 or 39.325 to NA before infusion-state imputation; values outside [0.250, 10.000] except explicit 0 are also set to NA."
]
imputation_plan <- copy(variable_plan)[, .(variable, column_group, strategy, fill_rule)]
imputation_plan[variable == "bt", fill_rule :=
  "Prediction-safe filling after CPB-aware cleaning: before the first observed value use the global median; after observation starts use forward fill only; if the entire case is missing, use global median."
]
imputation_plan[variable == "cbro2", fill_rule :=
  "After threshold cleaning, use forward fill only within the case; if values are still missing, use the global median. No backward fill or future-based interpolation is used."
]
imputation_plan[variable %in% c("art_sbp", "art_mbp", "art_dbp"), `:=`(
  strategy = "art_triplet_continuous_monitor",
  fill_rule = "After ART triplet cleaning, only valid ART SBP/MBP/DBP triplets are treated as observed. Before the first observed triplet use the ART global triplet fallback, after observation starts carry the full ART triplet forward together, and use the ART global triplet if the entire case is missing."
)]
imputation_plan[variable %in% c("pap_sbp", "pap_mbp", "pap_dbp"), `:=`(
  strategy = "pap_triplet_continuous_monitor",
  fill_rule = "After PAP triplet cleaning, only valid PAP SBP/MBP/DBP triplets are treated as observed. Before the first observed triplet use the PAP global triplet fallback, after observation starts carry the full PAP triplet forward together, and use the PAP global triplet if the entire case is missing."
)]
imputation_plan <- rbind(
  imputation_plan,
  data.table(
    variable = merged_bp_cols,
    column_group = "hemodynamics",
    strategy = "merged_bp_continuous_monitor",
    fill_rule = c(
      "Use a valid ART SBP/MBP/DBP triplet first; otherwise use a valid NIBP triplet. SBP/MBP/DBP are not mixed across devices. Before the first observed merged triplet use the merged global triplet fallback, after observation starts carry the full triplet forward together, and use the merged global triplet if the entire case is missing.",
      "Use a valid ART SBP/MBP/DBP triplet first; otherwise use a valid NIBP triplet. SBP/MBP/DBP are not mixed across devices. Before the first observed merged triplet use the merged global triplet fallback, after observation starts carry the full triplet forward together, and use the merged global triplet if the entire case is missing.",
      "Use a valid ART SBP/MBP/DBP triplet first; otherwise use a valid NIBP triplet. SBP/MBP/DBP are not mixed across devices. Before the first observed merged triplet use the merged global triplet fallback, after observation starts carry the full triplet forward together, and use the merged global triplet if the entire case is missing."
    )
  ),
  use.names = TRUE,
  fill = TRUE
)
fwrite(screening_plan, screening_plan_file)
fwrite(imputation_plan, imputation_plan_file)

clean_dt <- copy(dt)
flag_dictionary_list <- vector("list", length(available_vars))
qc_metrics <- list()

for (i in seq_along(available_vars)) {
  v <- available_vars[i]
  rule <- variable_plan[variable == v]
  x_raw <- dt[[v]]

  if (v == "bt") {
    tmp_bt <- dt[, .(row_id = .I, value = get(v), has_cpb = has_cpb[1]), by = op_id]
    tmp_bt[, cleaned := clean_bt_case(value, has_cpb[1]), by = op_id]
    setorder(tmp_bt, row_id)
    x_clean <- tmp_bt$cleaned
  } else {
    flag_low <- !is.na(x_raw) & !((rule$keep_zero) & x_raw == 0) & x_raw < rule$lower
    flag_high <- !is.na(x_raw) & x_raw > rule$upper
    x_clean <- copy(x_raw)
    x_clean[flag_low | flag_high] <- NA_real_
    if (v == "art_mbp") {
      x_clean[!is.na(x_raw) & x_raw == 6] <- NA_real_
      qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
        metric = "art_mbp_eq_6_rows_cleaned",
        value = sum(!is.na(x_raw) & x_raw == 6)
      )
    }
    if (v == "etco2") {
      x_clean[!is.na(x_raw) & x_raw == 0] <- NA_real_
      qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
        metric = "etco2_eq_0_rows_cleaned",
        value = sum(!is.na(x_raw) & x_raw == 0)
      )
    }
    if (v == "pmean") {
      x_clean[!is.na(x_raw) & x_raw %in% c(0, 1, 3)] <- NA_real_
      qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
        metric = "pmean_eq_0_1_3_rows_cleaned",
        value = sum(!is.na(x_raw) & x_raw %in% c(0, 1, 3))
      )
    }
    if (v == "pip") {
      x_clean[!is.na(x_raw) & x_raw == 1] <- NA_real_
      qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
        metric = "pip_eq_1_rows_cleaned",
        value = sum(!is.na(x_raw) & x_raw == 1)
      )
    }
    if (v == "cbro2") {
      qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
        metric = "cbro2_lt15_or_gt100_rows_cleaned",
        value = sum(!is.na(x_raw) & (x_raw < 15 | x_raw > 100))
      )
    }
    if (v == "pepi") {
      x_clean[!is.na(x_raw) & x_raw %in% c(0.25, 10)] <- NA_real_
      qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
        metric = "pepi_eq_0.25_or_10_rows_cleaned",
        value = sum(!is.na(x_raw) & x_raw %in% c(0.25, 10))
      )
    }
    if (v == "mlni") {
      x_clean[!is.na(x_raw) & x_raw %in% c(9.93, 39.325)] <- NA_real_
      qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
        metric = "mlni_eq_9.93_or_39.325_rows_cleaned",
        value = sum(!is.na(x_raw) & x_raw %in% c(9.93, 39.325))
      )
    }
  }
  clean_dt[[v]] <- x_clean
}

# ART triplet conflict cleaning.
art_triplet <- clean_dt[, !is.na(art_sbp) & !is.na(art_mbp) & !is.na(art_dbp)]
art_conflict <- art_triplet & !(clean_dt$art_sbp >= clean_dt$art_mbp & clean_dt$art_mbp >= clean_dt$art_dbp)
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "art_triplet_conflict_rows_cleaned",
  value = sum(art_conflict)
)
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "art_triplet_conflict_ops_cleaned",
  value = clean_dt[art_conflict, uniqueN(op_id)]
)
if (any(art_conflict)) {
  clean_dt[art_conflict, c("art_sbp", "art_mbp", "art_dbp") := .(NA_real_, NA_real_, NA_real_)]
}

# PAP low-MBP and PAP triplet conflict cleaning.
pap_low <- clean_dt[, !is.na(pap_mbp) & pap_mbp %in% c(1, 2)]
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "pap_mbp_eq_1_or_2_rows_cleaned",
  value = sum(pap_low)
)
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "pap_mbp_eq_1_or_2_ops_cleaned",
  value = clean_dt[pap_low, uniqueN(op_id)]
)
if (any(pap_low)) {
  clean_dt[pap_low, c("pap_sbp", "pap_mbp", "pap_dbp") := .(NA_real_, NA_real_, NA_real_)]
}
pap_triplet <- clean_dt[, !is.na(pap_sbp) & !is.na(pap_mbp) & !is.na(pap_dbp)]
pap_conflict <- pap_triplet & !(clean_dt$pap_sbp >= clean_dt$pap_mbp & clean_dt$pap_mbp >= clean_dt$pap_dbp)
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "pap_triplet_conflict_rows_cleaned",
  value = sum(pap_conflict)
)
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "pap_triplet_conflict_ops_cleaned",
  value = clean_dt[pap_conflict, uniqueN(op_id)]
)
if (any(pap_conflict)) {
  clean_dt[pap_conflict, c("pap_sbp", "pap_mbp", "pap_dbp") := .(NA_real_, NA_real_, NA_real_)]
}

# Build merged blood pressure observed values before no-NA filling.
merged_obs <- merge_bp_triplet(
  clean_dt$art_sbp, clean_dt$art_mbp, clean_dt$art_dbp,
  clean_dt$nibp_sbp, clean_dt$nibp_mbp, clean_dt$nibp_dbp
)

qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "merged_bp_art_triplet_rows_used_before_fill",
  value = sum(merged_obs$source == "ART", na.rm = TRUE)
)
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "merged_bp_nibp_triplet_rows_used_before_fill",
  value = sum(merged_obs$source == "NIBP", na.rm = TRUE)
)
qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "merged_bp_rows_without_valid_triplet_before_fill",
  value = sum(is.na(merged_obs$source))
)

clean_dt[, `:=`(
  sbp_merged = merged_obs$sbp,
  mbp_merged = merged_obs$mbp,
  dbp_merged = merged_obs$dbp
)]

qc_metrics[[length(qc_metrics) + 1L]] <- data.table(
  metric = "merged_bp_conflict_rows_after_merge_before_fill",
  value = clean_dt[, sum(!is.na(sbp_merged) & !is.na(mbp_merged) & !is.na(dbp_merged) & !(sbp_merged >= mbp_merged & mbp_merged >= dbp_merged))]
)

fwrite(clean_dt[, !c("orout_time", "has_cpb"), with = FALSE], clean_file)

final_dt <- copy(clean_dt)
art_global_map <- compute_bp_triplet_global(
  final_dt$art_sbp,
  final_dt$art_mbp,
  final_dt$art_dbp
)
pap_global_map <- compute_bp_triplet_global(
  final_dt$pap_sbp,
  final_dt$pap_mbp,
  final_dt$pap_dbp
)
merged_global_map <- compute_bp_triplet_global(
  final_dt$sbp_merged,
  final_dt$mbp_merged,
  final_dt$dbp_merged
)
merged_source_metrics <- list()
final_dt[, merged_bp_source_obs := merged_obs$source]

triplet_original_groups <- list(
  art = c("art_sbp", "art_mbp", "art_dbp"),
  pap = c("pap_sbp", "pap_mbp", "pap_dbp")
)
triplet_original_vars <- unlist(triplet_original_groups, use.names = FALSE)

for (i in seq_along(available_vars)) {
  v <- available_vars[i]
  if (v %in% triplet_original_vars) {
    next
  }
  rule <- variable_plan[variable == v]
  global_value <- as.numeric(global_median_map[[v]])

  tmp <- final_dt[, .(
    row_id = .I,
    chart_time = chart_time,
    value = get(v),
    orout_time = orout_time[1]
  ), by = op_id]

  if (rule$strategy == "continuous_monitor") {
    tmp[, c("imputed_value", "imputed_flag") := {
      res <- continuous_fill(value, chart_time, global_value)
      list(res$value, res$flag)
    }, by = op_id]
  } else if (rule$strategy == "tci_state") {
    tmp[, c("imputed_value", "imputed_flag") := {
      res <- tci_state_fill(value)
      list(res$value, res$flag)
    }, by = op_id]
  } else if (rule$strategy == "infusion_state_noNA") {
    tmp[, c("imputed_value", "imputed_flag") := {
      res <- infusion_state_fill(value, chart_time, orout_time[1], cap_minutes = 60)
      list(res$value, res$flag)
    }, by = op_id]
  } else if (rule$strategy == "event_zero_fill") {
    tmp[, c("imputed_value", "imputed_flag") := {
      res <- event_zero_fill(value)
      list(res$value, res$flag)
    }, by = op_id]
  } else if (rule$strategy == "global_median_fill") {
    tmp[, c("imputed_value", "imputed_flag") := {
      res <- global_median_fill(value, global_value)
      list(res$value, res$flag)
    }, by = op_id]
  } else {
    stop("Unhandled strategy for variable: ", v)
  }

  final_dt[[v]] <- tmp$imputed_value
  final_dt[[paste0(v, "_imputed_flag")]] <- tmp$imputed_flag

  flag_dictionary_list[[i]] <- data.table(
    variable = v,
    flag_column = paste0(v, "_imputed_flag"),
    column_group = rule$column_group,
    strategy = rule$strategy,
    unit = rule$unit,
    global_median_fallback = global_value,
    flag_definition = "0 = observed after threshold cleaning; 1 = rule-based imputed value"
  )
}

for (group_name in names(triplet_original_groups)) {
  vars <- triplet_original_groups[[group_name]]
  global_triplet <- switch(
    group_name,
    art = art_global_map,
    pap = pap_global_map,
    stop("Unhandled original BP triplet group: ", group_name)
  )

  tmp_triplet <- final_dt[, .(
    row_id = .I,
    sbp = get(vars[1]),
    mbp = get(vars[2]),
    dbp = get(vars[3])
  ), by = op_id]

  tmp_triplet[, c("sbp_imputed", "mbp_imputed", "dbp_imputed", "triplet_imputed_flag") := {
    res <- fill_bp_triplet(sbp, mbp, dbp, global_triplet)
    list(res$sbp, res$mbp, res$dbp, res$flag)
  }, by = op_id]

  final_dt[, (vars[1]) := tmp_triplet$sbp_imputed]
  final_dt[, (vars[2]) := tmp_triplet$mbp_imputed]
  final_dt[, (vars[3]) := tmp_triplet$dbp_imputed]
  final_dt[, (paste0(vars[1], "_imputed_flag")) := tmp_triplet$triplet_imputed_flag]
  final_dt[, (paste0(vars[2], "_imputed_flag")) := tmp_triplet$triplet_imputed_flag]
  final_dt[, (paste0(vars[3], "_imputed_flag")) := tmp_triplet$triplet_imputed_flag]

  for (v in vars) {
    rule <- variable_plan[variable == v]
    flag_dictionary_list[[length(flag_dictionary_list) + 1L]] <- data.table(
      variable = v,
      flag_column = paste0(v, "_imputed_flag"),
      column_group = rule$column_group,
      strategy = sprintf("%s_triplet_continuous_monitor", group_name),
      unit = rule$unit,
      global_median_fallback = as.numeric(global_triplet[[sub(paste0("^", group_name, "_"), "", v)]]),
      flag_definition = sprintf("0 = observed from a valid %s BP triplet after cleaning; 1 = %s triplet-aware imputed value", toupper(group_name), group_name)
    )
  }
}

tmp_bp <- final_dt[, .(
  row_id = .I,
  sbp = sbp_merged,
  mbp = mbp_merged,
  dbp = dbp_merged,
  source = merged_bp_source_obs
), by = op_id]

tmp_bp[, c("sbp_imputed", "mbp_imputed", "dbp_imputed", "triplet_imputed_flag", "imputed_source") := {
  res <- fill_bp_triplet_with_source(sbp, mbp, dbp, source, merged_global_map)
  list(res$sbp, res$mbp, res$dbp, res$flag, res$source)
}, by = op_id]

final_dt[, `:=`(
  sbp_merged = tmp_bp$sbp_imputed,
  mbp_merged = tmp_bp$mbp_imputed,
  dbp_merged = tmp_bp$dbp_imputed,
  sbp_merged_imputed_flag = tmp_bp$triplet_imputed_flag,
  mbp_merged_imputed_flag = tmp_bp$triplet_imputed_flag,
  dbp_merged_imputed_flag = tmp_bp$triplet_imputed_flag
)]

for (bp_var in merged_bp_cols) {
  flag_dictionary_list[[length(flag_dictionary_list) + 1L]] <- data.table(
    variable = bp_var,
    flag_column = paste0(bp_var, "_imputed_flag"),
    column_group = "hemodynamics",
    strategy = "merged_bp_triplet_monitor",
    unit = "mmHg",
    global_median_fallback = as.numeric(merged_global_map[[sub("_merged$", "", bp_var)]]),
    flag_definition = "0 = observed from a valid ART/NIBP BP triplet after cleaning; 1 = triplet-aware imputed value"
  )
}

src_tab <- data.table(source = tmp_bp$imputed_source)[, .N, by = source]
src_tab[, metric := sprintf("merged_bp_source_%s_rows", source)]
merged_source_metrics[[length(merged_source_metrics) + 1L]] <- src_tab[, .(metric, value = N)]

flag_dictionary <- rbindlist(flag_dictionary_list, use.names = TRUE, fill = TRUE)
fwrite(flag_dictionary, flag_dictionary_file)

hemo_vars <- variable_plan[column_group == "hemodynamics", variable]
hemo_vars <- append(hemo_vars, merged_bp_cols, after = match("nibp_dbp", hemo_vars))
value_cols_order <- c(
  id_cols,
  hemo_vars,
  unlist(lapply(setdiff(unique(variable_plan$column_group), "hemodynamics"), function(g) {
    variable_plan[column_group == g, variable]
  }), use.names = FALSE)
)
value_cols_order <- intersect(value_cols_order, names(final_dt))
flag_cols_order <- paste0(setdiff(value_cols_order, id_cols), "_imputed_flag")
flag_cols_order <- intersect(flag_cols_order, names(final_dt))
final_cols <- c(value_cols_order, flag_cols_order)
setcolorder(final_dt, c("orout_time", setdiff(names(final_dt), "orout_time")))
fwrite(final_dt[, ..final_cols], final_file)

qc_dt <- rbindlist(qc_metrics, use.names = TRUE, fill = TRUE)
qc_dt <- rbind(
  qc_dt,
  rbindlist(merged_source_metrics, use.names = TRUE, fill = TRUE),
  data.table(metric = "art_triplet_conflict_rows_remaining_after_clean", value = final_dt[, sum(!is.na(art_sbp) & !is.na(art_mbp) & !is.na(art_dbp) & !(art_sbp >= art_mbp & art_mbp >= art_dbp))]),
  data.table(metric = "pap_triplet_conflict_rows_remaining_after_clean", value = final_dt[, sum(!is.na(pap_sbp) & !is.na(pap_mbp) & !is.na(pap_dbp) & !(pap_sbp >= pap_mbp & pap_mbp >= pap_dbp))]),
  data.table(metric = "pap_mbp_eq_1_or_2_rows_remaining_after_clean", value = final_dt[, sum(!is.na(pap_mbp) & pap_mbp %in% c(1, 2))]),
  data.table(metric = "merged_bp_conflict_rows_final", value = final_dt[, sum(!is.na(sbp_merged) & !is.na(mbp_merged) & !is.na(dbp_merged) & !(sbp_merged >= mbp_merged & mbp_merged >= dbp_merged))]),
  data.table(metric = "merged_bp_conflict_ops_final", value = final_dt[!is.na(sbp_merged) & !is.na(mbp_merged) & !is.na(dbp_merged) & !(sbp_merged >= mbp_merged & mbp_merged >= dbp_merged), uniqueN(op_id)]),
  data.table(metric = "final_total_clinical_na_count", value = final_dt[, sum(sapply(.SD, function(x) sum(is.na(x)))), .SDcols = c(available_vars, merged_bp_cols)]),
  data.table(metric = "final_variables_with_any_na", value = final_dt[, sum(sapply(.SD, function(x) any(is.na(x)))), .SDcols = c(available_vars, merged_bp_cols)])
)
fwrite(qc_dt, qc_summary_file)

note_lines <- c(
  "# INSPIRE First Non-MAC Intraoperative Median Table: Cleaning and No-NA Imputation",
  "",
  "This workflow starts from the median-aggregated intraoperative wide table derived from raw vitals.csv.",
  "",
  "Outputs:",
  "- Raw median grouped table",
  "- Threshold-cleaned table with out-of-range values set to NA",
  "- Final no-NA table with one `_imputed_flag` column for every clinical variable",
  "",
  "Block-specific rules:",
  "- Continuous monitoring variables: threshold cleaning -> before the first observed value use the global median -> after observation starts use forward fill only; no backward fill or future-based interpolation is used.",
  "- Body temperature (bt) uses a CPB-aware exception: non-CPB cases treat 20.6/22.0/27.6 as invalid; CPB cases retain 20.6/22.0/27.6 only when they occur between the first and last >30 Celsius observations within the same case.",
  "- Cerebral oximetry (cbro2) uses a stricter cleaning rule: values <15 or >100 are set to NA; after cleaning, forward fill is used first and any remaining missing values fall back to the global median.",
  "- etco2 = 0, pip = 1, and pmean = 0/1/3 are treated as invalid and set to NA before imputation.",
  "- Original ART and PAP blood pressure columns use triplet-aware no-NA filling: only valid SBP/MBP/DBP triplets are treated as observed, and the full triplet is carried forward together.",
  "- Blood pressure merging: use a valid ART SBP/MBP/DBP triplet first; otherwise use a valid NIBP triplet. SBP/MBP/DBP are not mixed across devices, and merged BP imputation carries the full triplet forward together.",
  "- ART triplet conflicts (art_sbp >= art_mbp >= art_dbp violated) clear the full ART triplet before any merged BP logic is applied.",
  "- PAP cleaning: pap_mbp = 1/2 clears the full PAP triplet; PAP triplet conflicts (pap_sbp >= pap_mbp >= pap_dbp violated) also clear the full triplet.",
  "- TCI variables (ppfi, rfti): before first positive value fill 0; explicit 0 stays 0; otherwise carry forward to next update or OR end.",
  "- Vasoactive infusions: before first positive value fill 0; explicit 0 stays 0; once a positive infusion rate is observed, the last positive rate is carried forward to OR end unless an explicit 0 is later observed. pepi = 0.25/10 and mlni = 9.93/39.325 are treated as invalid.",
  "- Bolus drugs, fluids, blood products, blood loss, urine output and drainage: missing values are filled with 0 after threshold cleaning.",
  "- cpat: global median fallback because the parameter dictionary remains incomplete.",
  "",
  "Leakage note:",
  "- This version avoids backward fill and future-based linear interpolation for continuous monitoring variables.",
  "- For predictive modeling with train/test splitting, global medians should be re-estimated on the training data only.",
  "Every final clinical variable is guaranteed to have no NA values in the final no-NA modeling table."
)
writeLines(note_lines, method_notes_file)

cat("Clean table written to:\n", clean_file, "\n", sep = "")
cat("Outlier screening plan written to:\n", screening_plan_file, "\n", sep = "")
cat("Imputation plan written to:\n", imputation_plan_file, "\n", sep = "")
cat("Final no-NA table written to:\n", final_file, "\n", sep = "")
cat("Flag dictionary written to:\n", flag_dictionary_file, "\n", sep = "")
cat("Method notes written to:\n", method_notes_file, "\n", sep = "")
cat("QC summary written to:\n", qc_summary_file, "\n", sep = "")
