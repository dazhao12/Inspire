#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

# ==============================================================================
# 0. Paths
# ==============================================================================
path_raw <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
path_processed_base <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
output_folder_name <- "Acute_Status_3mo_first_nonMAC_sepsis_A40_A41_3_30_2026"
path_output <- file.path(path_processed_base, output_folder_name)

if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
}

WINDOW_3MO_MIN <- 90 * 24 * 60

# ==============================================================================
# 1. Anchor operation: first non-MAC per subject_id + hadm_id
# ==============================================================================
cat("Loading operations and selecting first non-MAC anchor surgery per admission...\n")

ops <- fread(
  file.path(path_raw, "operations.csv"),
  select = c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate",
    "antype", "admission_time", "orin_time", "opstart_time", "anstart_time"
  ),
  na.strings = c("", "NA")
)

ops[, `:=`(
  admission_time = as.numeric(admission_time),
  orin_time = as.numeric(orin_time),
  opstart_time = as.numeric(opstart_time),
  anstart_time = as.numeric(anstart_time),
  opdate_num = suppressWarnings(as.numeric(opdate)),
  antype_clean = toupper(trimws(as.character(antype)))
)]
ops[, anchor_sort_time := fcoalesce(opstart_time, anstart_time, orin_time, opdate_num, admission_time)]
ops[, hadm_group := fifelse(is.na(hadm_id), paste0("MISSING_HADM_", op_id), as.character(hadm_id))]

anchor_ops <- ops[
  !is.na(subject_id) & !is.na(op_id) & !is.na(orin_time) & !is.na(antype_clean) & antype_clean != "MAC"
][order(subject_id, hadm_group, anchor_sort_time, op_id)][
  , .SD[1], by = .(subject_id, hadm_group)
]

setorderv(anchor_ops, c("subject_id", "hadm_id", "op_id"))

anchor_index <- anchor_ops[, .(
  subject_id, hadm_id, op_id, case_id, orin_time,
  window_start = orin_time - WINDOW_3MO_MIN
)]

fwrite(anchor_index, file.path(path_output, "anchor_first_nonMAC_operations.csv"))
cat(sprintf("Anchor operations kept: %d\n", nrow(anchor_index)))

# ==============================================================================
# 2. Load diagnosis and ward_vitals
# ==============================================================================
cat("Loading diagnosis.csv ...\n")
diag <- fread(
  file.path(path_raw, "diagnosis.csv"),
  select = c("subject_id", "chart_time", "icd10_cm"),
  na.strings = c("", "NA")
)
diag <- diag[subject_id %in% anchor_index$subject_id]
diag[, `:=`(
  chart_time = as.numeric(chart_time),
  icd3 = substr(gsub("\\.", "", toupper(trimws(icd10_cm))), 1, 3)
)]
diag <- diag[!is.na(subject_id) & !is.na(chart_time) & !is.na(icd3) & nchar(icd3) == 3]

cat("Loading ward_vitals.csv ...\n")
ward_file <- file.path(path_raw, "ward_vitals.csv")
ward <- fread(
  cmd = sprintf("grep -iE ',(vent|iabp|ecmo|fio2),' %s", shQuote(ward_file)),
  header = FALSE,
  col.names = c("subject_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA")
)
ward <- ward[subject_id %in% anchor_index$subject_id]
ward[, `:=`(
  chart_time = as.numeric(chart_time),
  item_name = tolower(trimws(item_name)),
  value = suppressWarnings(as.numeric(value))
)]
ward <- ward[!is.na(subject_id) & !is.na(chart_time) & item_name %chin% c("vent", "iabp", "ecmo", "fio2")]

# ==============================================================================
# 3. Define event mappings
# ==============================================================================
cat("Building acute event flags within 3 months before anchor OR-in time...\n")

diag_event_defs <- list(
  list(var = "acute_myocardial_infarction", label_cn = "急性心肌梗死", codes = c("I21", "I22", "I23")),
  list(var = "cerebral_infarction", label_cn = "脑梗死", codes = c("I63")),
  list(var = "cardiac_arrest", label_cn = "心搏骤停", codes = c("I46")),
  list(var = "ards", label_cn = "ARDS", codes = c("J80")),
  list(var = "pulmonary_embolism", label_cn = "肺栓塞", codes = c("I26")),
  list(var = "sepsis", label_cn = "脓毒症", codes = c("A40", "A41")),
  list(var = "pneumonia", label_cn = "肺炎", codes = c("J12", "J13", "J14", "J15", "J16", "J17", "J18")),
  list(var = "shock", label_cn = "休克", codes = c("R57"))
)

ward_event_defs <- list(
  list(var = "ventilation", label_cn = "机械通气", item = "vent", rule = "value == 1"),
  list(var = "iabp", label_cn = "IABP", item = "iabp", rule = "value == 1"),
  list(var = "ecmo", label_cn = "ECMO", item = "ecmo", rule = "value == 1"),
  list(var = "oxygen_therapy", label_cn = "氧疗", item = "fio2", rule = "value > 30")
)

# ==============================================================================
# 4. Aggregate diagnosis-based acute events
# ==============================================================================
anchor_window <- anchor_index[, .(
  subject_id, hadm_id, op_id, case_id, orin_time,
  start = window_start,
  end = orin_time - 1
)]
setkey(anchor_window, subject_id, start, end)

diag_result <- copy(anchor_index[, .(subject_id, hadm_id, op_id, case_id, orin_time)])

all_diag_codes <- unique(unlist(lapply(diag_event_defs, `[[`, "codes")))
diag_union <- diag[icd3 %chin% all_diag_codes]
diag_union[, `:=`(start = chart_time, end = chart_time)]
setkey(diag_union, subject_id, start, end)
diag_matched <- foverlaps(
  diag_union,
  anchor_window,
  by.x = c("subject_id", "start", "end"),
  by.y = c("subject_id", "start", "end"),
  type = "within",
  nomatch = 0L
)

if (nrow(diag_matched) > 0L) {
  diag_matched[, `:=`(
    anchor_op_id = op_id,
    event_chart_time_raw = chart_time,
    anchor_orin_time = orin_time,
    interval_to_surgery_min = orin_time - chart_time
  )]
}

for (def in diag_event_defs) {
  cat("  - diagnosis event:", def$var, "\n")
  matched <- if (nrow(diag_matched) > 0L) diag_matched[icd3 %chin% def$codes] else diag_matched

  if (nrow(matched) > 0L) {
    agg <- matched[
      order(anchor_op_id, interval_to_surgery_min, chart_time)
    ][
      , .SD[1], by = anchor_op_id
    ][
      , .(
        op_id = anchor_op_id,
        flag = 1L,
        interval_min = interval_to_surgery_min,
        event_chart_time = event_chart_time_raw,
        source_code = icd3
      )
    ]
  } else {
    agg <- data.table(
      op_id = integer(),
      flag = integer(),
      interval_min = numeric(),
      event_chart_time = numeric(),
      source_code = character()
    )
  }

  setnames(
    agg,
    c("flag", "interval_min", "event_chart_time", "source_code"),
    c(def$var, paste0(def$var, "_interval_to_surgery_min"), paste0(def$var, "_event_time"), paste0(def$var, "_source"))
  )

  diag_result <- merge(diag_result, agg, by = "op_id", all.x = TRUE)
  for (j in c(def$var)) {
    set(diag_result, which(is.na(diag_result[[j]])), j, 0L)
  }
}

# ==============================================================================
# 5. Aggregate ward-based acute support events
# ==============================================================================
ward_result <- copy(anchor_index[, .(subject_id, hadm_id, op_id, case_id, orin_time)])

ward_union <- ward[
  (item_name == "vent" & value == 1) |
    (item_name == "iabp" & value == 1) |
    (item_name == "ecmo" & value == 1) |
    (item_name == "fio2" & !is.na(value) & value > 30)
]
ward_union[, `:=`(start = chart_time, end = chart_time)]
setkey(ward_union, subject_id, start, end)
ward_matched <- foverlaps(
  ward_union,
  anchor_window,
  by.x = c("subject_id", "start", "end"),
  by.y = c("subject_id", "start", "end"),
  type = "within",
  nomatch = 0L
)

if (nrow(ward_matched) > 0L) {
  ward_matched[, `:=`(
    anchor_op_id = op_id,
    event_chart_time_raw = chart_time,
    anchor_orin_time = orin_time,
    interval_to_surgery_min = orin_time - chart_time
  )]
}

for (def in ward_event_defs) {
  cat("  - ward event:", def$var, "\n")
  if (def$rule == "value == 1") {
    matched <- if (nrow(ward_matched) > 0L) ward_matched[item_name == def$item & value == 1] else ward_matched
  } else if (def$rule == "value > 30") {
    matched <- if (nrow(ward_matched) > 0L) ward_matched[item_name == def$item & !is.na(value) & value > 30] else ward_matched
  } else {
    stop("Unknown ward rule: ", def$rule)
  }

  if (nrow(matched) > 0L) {
    agg <- matched[
      order(anchor_op_id, interval_to_surgery_min, chart_time)
    ][
      , .SD[1], by = anchor_op_id
    ][
      , .(
        op_id = anchor_op_id,
        flag = 1L,
        interval_min = interval_to_surgery_min,
        event_chart_time = event_chart_time_raw,
        source_value = value
      )
    ]
  } else {
    agg <- data.table(
      op_id = integer(),
      flag = integer(),
      interval_min = numeric(),
      event_chart_time = numeric(),
      source_value = numeric()
    )
  }

  setnames(
    agg,
    c("flag", "interval_min", "event_chart_time", "source_value"),
    c(def$var, paste0(def$var, "_interval_to_surgery_min"), paste0(def$var, "_event_time"), paste0(def$var, "_source_value"))
  )

  ward_result <- merge(ward_result, agg, by = "op_id", all.x = TRUE)
  for (j in c(def$var)) {
    set(ward_result, which(is.na(ward_result[[j]])), j, 0L)
  }
}

# ==============================================================================
# 6. Build final output
# ==============================================================================
cat("Building final acute status table...\n")

acute_final <- merge(
  diag_result,
  ward_result[, !c("subject_id", "hadm_id", "case_id", "orin_time")],
  by = "op_id",
  all.x = TRUE
)

setcolorder(acute_final, c(
  "subject_id", "hadm_id", "op_id", "case_id", "orin_time",
  "acute_myocardial_infarction", "acute_myocardial_infarction_interval_to_surgery_min",
  "cerebral_infarction", "cerebral_infarction_interval_to_surgery_min",
  "cardiac_arrest", "cardiac_arrest_interval_to_surgery_min",
  "ards", "ards_interval_to_surgery_min",
  "pulmonary_embolism", "pulmonary_embolism_interval_to_surgery_min",
  "sepsis", "sepsis_interval_to_surgery_min",
  "pneumonia", "pneumonia_interval_to_surgery_min",
  "shock", "shock_interval_to_surgery_min",
  "ventilation", "ventilation_interval_to_surgery_min",
  "iabp", "iabp_interval_to_surgery_min",
  "ecmo", "ecmo_interval_to_surgery_min",
  "oxygen_therapy", "oxygen_therapy_interval_to_surgery_min"
))

setorderv(acute_final, c("subject_id", "hadm_id", "op_id"))

fwrite(acute_final, file.path(path_output, "acute_status_3mo_before_orin_first_nonMAC.csv"))

# ==============================================================================
# 7. Summary outputs
# ==============================================================================
cat("Building summary outputs...\n")

summary_defs <- rbindlist(list(
  data.table(
    variable = vapply(diag_event_defs, `[[`, character(1), "var"),
    label_cn = vapply(diag_event_defs, `[[`, character(1), "label_cn"),
    source_type = "diagnosis"
  ),
  data.table(
    variable = vapply(ward_event_defs, `[[`, character(1), "var"),
    label_cn = vapply(ward_event_defs, `[[`, character(1), "label_cn"),
    source_type = "ward_vitals"
  )
), use.names = TRUE)

summary_out <- rbindlist(lapply(seq_len(nrow(summary_defs)), function(i) {
  var <- summary_defs$variable[i]
  interval_var <- paste0(var, "_interval_to_surgery_min")
  flag_vec <- acute_final[[var]]
  interval_vec <- acute_final[[interval_var]]

  data.table(
    variable = var,
    label_cn = summary_defs$label_cn[i],
    source_type = summary_defs$source_type[i],
    n_cases = sum(flag_vec == 1L, na.rm = TRUE),
    total_ops = nrow(acute_final),
    prevalence_pct = round(100 * mean(flag_vec == 1L, na.rm = TRUE), 2),
    median_interval_min = round(median(interval_vec, na.rm = TRUE), 1),
    p25_interval_min = round(quantile(interval_vec, 0.25, na.rm = TRUE, names = FALSE), 1),
    p75_interval_min = round(quantile(interval_vec, 0.75, na.rm = TRUE, names = FALSE), 1)
  )
}), use.names = TRUE)

for (j in c("median_interval_min", "p25_interval_min", "p75_interval_min")) {
  set(summary_out, which(!is.finite(summary_out[[j]])), j, NA_real_)
}

setorder(summary_out, -prevalence_pct, variable)
fwrite(summary_out, file.path(path_output, "acute_status_3mo_summary.csv"))

meta_notes <- data.table(
  note_type = c("window_definition", "anchor_definition", "sepsis_rule", "linkage_risk", "interval_rule"),
  note = c(
    "Acute status window = [orin_time - 90 days, orin_time).",
    "If an admission has multiple surgeries, anchor op is the first non-MAC surgery.",
    "Sepsis in v2 uses the clinically common ICD-10 family A40-A41.",
    "diagnosis.csv and ward_vitals.csv only contain subject_id; linkage to the anchor surgery is time-window based and may still carry cross-admission risk within the same subject.",
    "If an event occurs multiple times within 3 months, interval_to_surgery_min uses the closest event before surgery."
  )
)
fwrite(meta_notes, file.path(path_output, "acute_status_3mo_notes.csv"))

cat("\nTop acute event prevalence:\n")
print(summary_out[1:min(10L, .N)])
cat("\nDone.\n")
