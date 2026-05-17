library(data.table)

input_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/operations.csv"
output_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Death_Time_Consistency_Audit_3_31_2026"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ops <- fread(
  input_path,
  select = c(
    "op_id", "subject_id", "hadm_id", "case_id",
    "admission_time", "discharge_time", "orin_time", "orout_time",
    "inhosp_death_time", "allcause_death_time"
  )
)

num_cols <- c(
  "admission_time", "discharge_time", "orin_time", "orout_time",
  "inhosp_death_time", "allcause_death_time"
)
ops[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# Stay-level audit uses one row per subject_id + hadm_id, while preserving
# whether repeated operation rows disagree on death-related fields.
stay_audit <- ops[
  order(subject_id, hadm_id, op_id),
  .(
    op_count = .N,
    op_ids = paste(op_id, collapse = ";"),
    case_ids = paste(unique(case_id), collapse = ";"),
    admission_time = first(admission_time),
    discharge_time = first(discharge_time),
    first_orin_time = suppressWarnings(min(orin_time, na.rm = TRUE)),
    first_orout_time = suppressWarnings(min(orout_time, na.rm = TRUE)),
    inhosp_death_time = first(inhosp_death_time),
    allcause_death_time = first(allcause_death_time),
    unique_admission_time_n = uniqueN(admission_time),
    unique_discharge_time_n = uniqueN(discharge_time),
    unique_inhosp_death_time_n = uniqueN(inhosp_death_time),
    unique_allcause_death_time_n = uniqueN(allcause_death_time)
  ),
  by = .(subject_id, hadm_id)
]

stay_audit[!is.finite(first_orin_time), first_orin_time := NA_real_]
stay_audit[!is.finite(first_orout_time), first_orout_time := NA_real_]

stay_audit[, row_level_value_conflict := (
  unique_admission_time_n > 1 |
    unique_discharge_time_n > 1 |
    unique_inhosp_death_time_n > 1 |
    unique_allcause_death_time_n > 1
)]

stay_audit[, has_inhosp_death := !is.na(inhosp_death_time)]
stay_audit[, has_allcause_death := !is.na(allcause_death_time)]

stay_audit[, `:=`(
  inhosp_vs_admission_min = fifelse(has_inhosp_death, inhosp_death_time - admission_time, NA_real_),
  inhosp_vs_discharge_min = fifelse(has_inhosp_death, inhosp_death_time - discharge_time, NA_real_),
  allcause_vs_admission_min = fifelse(has_allcause_death, allcause_death_time - admission_time, NA_real_),
  allcause_vs_discharge_min = fifelse(has_allcause_death, allcause_death_time - discharge_time, NA_real_),
  allcause_minus_inhosp_min = fifelse(has_inhosp_death & has_allcause_death, allcause_death_time - inhosp_death_time, NA_real_)
)]

stay_audit[, inhosp_within_stay := has_inhosp_death & inhosp_death_time >= admission_time & inhosp_death_time <= discharge_time]
stay_audit[, inhosp_before_admission := has_inhosp_death & inhosp_death_time < admission_time]
stay_audit[, inhosp_after_discharge := has_inhosp_death & inhosp_death_time > discharge_time]

stay_audit[, allcause_before_admission := has_allcause_death & allcause_death_time < admission_time]
stay_audit[, allcause_before_discharge := has_allcause_death & allcause_death_time < discharge_time]
stay_audit[, allcause_equal_discharge := has_allcause_death & allcause_death_time == discharge_time]
stay_audit[, allcause_after_discharge := has_allcause_death & allcause_death_time > discharge_time]

stay_audit[, both_present := has_inhosp_death & has_allcause_death]
stay_audit[, both_equal := both_present & allcause_death_time == inhosp_death_time]
stay_audit[, allcause_before_inhosp := both_present & allcause_death_time < inhosp_death_time]
stay_audit[, allcause_after_inhosp := both_present & allcause_death_time > inhosp_death_time]

stay_audit[, allcause_is_day_aligned := has_allcause_death & (allcause_death_time %% 1440 == 0)]
stay_audit[, inhosp_is_day_aligned := has_inhosp_death & (inhosp_death_time %% 1440 == 0)]

stay_audit[, audit_category := fifelse(
  !has_inhosp_death & !has_allcause_death, "no_death_record",
  fifelse(
    has_inhosp_death & !has_allcause_death,
    fifelse(inhosp_within_stay, "inhosp_only_within_stay", "inhosp_only_outside_stay"),
    fifelse(
      !has_inhosp_death & has_allcause_death,
      fifelse(allcause_before_discharge, "allcause_only_before_discharge", "allcause_only_after_discharge"),
      fifelse(
        row_level_value_conflict, "both_present_row_conflict",
        fifelse(
          inhosp_within_stay & allcause_after_discharge, "both_present_consistent_progression",
          fifelse(
            inhosp_within_stay & both_equal, "both_present_same_timestamp",
            fifelse(
              inhosp_after_discharge & allcause_after_discharge, "both_present_inhosp_outside_stay",
              fifelse(
                allcause_before_inhosp, "both_present_allcause_before_inhosp",
                fifelse(
                  allcause_before_discharge, "both_present_allcause_before_discharge",
                  "both_present_other_pattern"
                )
              )
            )
          )
        )
      )
    )
  )
)]

summary_dt <- rbindlist(list(
  stay_audit[, .(metric = "total_stays", n = .N)],
  stay_audit[, .(metric = "stays_with_inhosp_death", n = sum(has_inhosp_death))],
  stay_audit[, .(metric = "stays_with_allcause_death", n = sum(has_allcause_death))],
  stay_audit[, .(metric = "stays_with_both_death_fields", n = sum(both_present))],
  stay_audit[, .(metric = "inhosp_within_stay", n = sum(inhosp_within_stay))],
  stay_audit[, .(metric = "inhosp_after_discharge", n = sum(inhosp_after_discharge))],
  stay_audit[, .(metric = "allcause_after_discharge", n = sum(allcause_after_discharge))],
  stay_audit[, .(metric = "allcause_before_discharge", n = sum(allcause_before_discharge))],
  stay_audit[, .(metric = "allcause_before_inhosp", n = sum(allcause_before_inhosp))],
  stay_audit[, .(metric = "row_level_value_conflict", n = sum(row_level_value_conflict))]
))

category_dt <- stay_audit[, .(n = .N), by = audit_category][order(-n, audit_category)]
category_dt[, pct := round(100 * n / sum(n), 4)]

notes_dt <- data.table(
  note = c(
    "All time fields are treated as relative minute timestamps from the raw operations.csv.",
    "Main stay-level consistency check: inhosp_death_time should lie within [admission_time, discharge_time].",
    "Main long-term consistency check: allcause_death_time is expected to be on or after discharge_time in most stays.",
    "row_level_value_conflict indicates repeated operation rows within the same subject_id + hadm_id disagree on at least one death-related field.",
    "allcause_is_day_aligned checks whether allcause_death_time is a multiple of 1440 minutes, suggesting day-level granularity."
  )
)

setorder(stay_audit, -has_inhosp_death, -has_allcause_death, subject_id, hadm_id)

fwrite(stay_audit, file.path(output_dir, "death_time_consistency_audit_stay_level.csv"))
fwrite(summary_dt, file.path(output_dir, "death_time_consistency_summary.csv"))
fwrite(category_dt, file.path(output_dir, "death_time_consistency_category_summary.csv"))
fwrite(notes_dt, file.path(output_dir, "death_time_consistency_notes.csv"))

cat("Wrote death time audit outputs to:", output_dir, "\n")
