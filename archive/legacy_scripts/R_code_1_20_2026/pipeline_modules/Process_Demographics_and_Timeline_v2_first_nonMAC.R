library(tidyverse)
library(data.table)

# ------------------------------------------------------------------
# 0. Paths and read
# ------------------------------------------------------------------
input_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
output_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Demographics_Timeline_first_nonMAC_3_30_2026"

if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

cat("Reading raw operations.csv ...\n")
ops_raw <- fread(file.path(input_path, "operations.csv"), na.strings = c("", "NA")) %>%
  as_tibble()

# ------------------------------------------------------------------
# 1. Common preprocessing
# ------------------------------------------------------------------
cat("Running common preprocessing ...\n")
ops_cleaned <- ops_raw %>%
  mutate(
    Male = case_when(sex == "M" ~ 1, sex == "F" ~ 0, TRUE ~ NA_real_),
    across(c(height, weight, age), ~ as.numeric(.)),
    BMI = if_else(height > 0, weight / ((height / 100)^2), NA_real_),
    Age = age,
    Height = height,
    Weight = weight,
    Emergency_op = emop,
    antype_clean = toupper(trimws(as.character(antype))),
    non_mac_flag = !is.na(antype_clean) & antype_clean != "MAC",
    opdate_num = suppressWarnings(as.numeric(opdate)),
    # Sort key for "first operation" within admission:
    # prefer true intraop timestamps, fallback to opdate/admission, then Inf.
    anchor_sort_time = coalesce(opstart_time, anstart_time, orin_time, opdate_num, admission_time),
    anchor_sort_time = if_else(is.na(anchor_sort_time), Inf, anchor_sort_time),
    # Avoid accidental collapsing when hadm_id is missing.
    hadm_group = if_else(is.na(hadm_id), paste0("MISSING_HADM_", op_id), as.character(hadm_id))
  )

# ------------------------------------------------------------------
# 2. Anchor operation selection: first non-MAC per subject_id + hadm_id
# ------------------------------------------------------------------
cat("Selecting anchor operations (first non-MAC per admission) ...\n")

admission_index <- ops_cleaned %>%
  group_by(subject_id, hadm_group) %>%
  summarise(
    hadm_id = first(hadm_id),
    n_ops_in_admission = n(),
    has_non_mac = any(non_mac_flag),
    .groups = "drop"
  )

anchor_ops <- ops_cleaned %>%
  filter(non_mac_flag) %>%
  group_by(subject_id, hadm_group) %>%
  arrange(anchor_sort_time, op_id, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(
    admission_index %>% select(subject_id, hadm_group, n_ops_in_admission),
    by = c("subject_id", "hadm_group")
  ) %>%
  arrange(subject_id, hadm_id, op_id)

anchor_dup_check <- anchor_ops %>%
  count(subject_id, hadm_group) %>%
  filter(n > 1)
if (nrow(anchor_dup_check) > 0) {
  stop("Anchor operation selection failed: found >1 op_id for some subject_id + hadm_id groups.")
}

n_adm_total <- nrow(admission_index)
n_adm_with_nonmac <- sum(admission_index$has_non_mac, na.rm = TRUE)
n_adm_without_nonmac <- n_adm_total - n_adm_with_nonmac

cat("\n--- Anchor Selection QC ---\n")
cat(sprintf("Total admissions (subject_id + hadm_id groups): %d\n", n_adm_total))
cat(sprintf("Admissions with >=1 non-MAC surgery: %d\n", n_adm_with_nonmac))
cat(sprintf("Admissions without non-MAC surgery (excluded): %d\n", n_adm_without_nonmac))
cat(sprintf("Final anchor operations kept: %d\n", nrow(anchor_ops)))

anchor_map <- anchor_ops %>%
  select(
    subject_id, hadm_id, op_id, case_id, opdate,
    antype, antype_clean, non_mac_flag, anchor_sort_time, n_ops_in_admission = n_ops_in_admission
  ) %>%
  arrange(subject_id, hadm_id, op_id)

fwrite(anchor_map, file.path(output_path, "Admission_First_NonMAC_Operation_Map.csv"))
cat(" -> Admission_First_NonMAC_Operation_Map.csv saved.\n")

ops_anchor <- ops_cleaned %>%
  semi_join(anchor_ops %>% select(op_id), by = "op_id") %>%
  arrange(subject_id, hadm_id, op_id)

# ------------------------------------------------------------------
# 3. Subject-level demographic table
# ------------------------------------------------------------------
cat("Building subject-level demographic table ...\n")

get_first_non_na <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  first(x)
}

demog_subject_level <- ops_anchor %>%
  group_by(subject_id) %>%
  summarise(
    Age = mean(Age, na.rm = TRUE),
    Height = mean(Height, na.rm = TRUE),
    Weight = mean(Weight, na.rm = TRUE),
    BMI = mean(BMI, na.rm = TRUE),
    Male = get_first_non_na(Male),
    race = get_first_non_na(race),
    n_anchor_admissions = n(),
    .groups = "drop"
  ) %>%
  arrange(subject_id)

fwrite(demog_subject_level, file.path(output_path, "Demographic_Subject_Level.csv"))
cat(" -> Demographic_Subject_Level.csv saved.\n")

# ------------------------------------------------------------------
# 4. Operation-level demographic table (anchor only)
# ------------------------------------------------------------------
cat("Building operation-level demographic table (anchor only) ...\n")

demog_op_level <- ops_anchor %>%
  select(
    subject_id,
    hadm_id,
    op_id,
    case_id,
    opdate,
    Male,
    Age,
    Height,
    Weight,
    BMI,
    race,
    asa,
    Emergency_op,
    department,
    antype,
    icd10_pcs
  ) %>%
  arrange(subject_id, hadm_id, op_id)

fwrite(demog_op_level, file.path(output_path, "Demographic_Operation_Level.csv"))
cat(" -> Demographic_Operation_Level.csv saved.\n")

# ------------------------------------------------------------------
# 5. Time-related table (anchor only)
# ------------------------------------------------------------------
cat("Building time-related table (anchor only) ...\n")

timeline_data <- ops_anchor %>%
  mutate(
    admission_time_min_raw = admission_time,
    discharge_time_min_raw = discharge_time,
    opstart_time_min_raw = opstart_time,
    opend_time_min_raw = opend_time,
    anstart_time_min_raw = anstart_time,
    anend_time_min_raw = anend_time,
    cpbon_time_min_raw = cpbon_time,
    cpboff_time_min_raw = cpboff_time,
    inhosp_death_time_min_raw = inhosp_death_time,
    allcause_death_time_min_raw = allcause_death_time,
    op_duration_min = if_else(opend_time > opstart_time, opend_time - opstart_time, NA_real_),
    anesthesia_duration_min = if_else(anend_time > anstart_time, anend_time - anstart_time, NA_real_),
    or_room_time_min = if_else(orout_time > orin_time, orout_time - orin_time, NA_real_),
    cpb_duration_min = if_else(cpboff_time > cpbon_time, cpboff_time - cpbon_time, NA_real_),
    hosp_los_min = if_else(discharge_time > admission_time, discharge_time - admission_time, NA_real_),
    hosp_los_days = if_else(discharge_time > admission_time, (discharge_time - admission_time) / 1440, NA_real_),
    icu_los_min = if_else(icuout_time > icuin_time, icuout_time - icuin_time, NA_real_),
    icu_los_days = if_else(icuout_time > icuin_time, (icuout_time - icuin_time) / 1440, NA_real_),
    time_to_inhosp_death_min = if_else(
      !is.na(inhosp_death_time) & !is.na(admission_time),
      inhosp_death_time - admission_time,
      NA_real_
    ),
    time_to_inhosp_death_days = if_else(
      !is.na(inhosp_death_time) & !is.na(admission_time),
      (inhosp_death_time - admission_time) / 1440,
      NA_real_
    ),
    time_to_allcause_death_min = if_else(
      !is.na(allcause_death_time) & !is.na(admission_time),
      allcause_death_time - admission_time,
      NA_real_
    ),
    time_to_allcause_death_days = if_else(
      !is.na(allcause_death_time) & !is.na(admission_time),
      (allcause_death_time - admission_time) / 1440,
      NA_real_
    )
  ) %>%
  arrange(subject_id, hadm_id, op_id)

timeline_data <- timeline_data %>%
  mutate(
    flag_los_error = (discharge_time_min_raw < admission_time_min_raw),
    flag_op_time_error = (opend_time_min_raw < opstart_time_min_raw),
    flag_death_before_admission = (allcause_death_time_min_raw < admission_time_min_raw)
  )

timeline_data <- timeline_data %>%
  arrange(subject_id, admission_time_min_raw, opstart_time_min_raw, op_id) %>%
  group_by(subject_id) %>%
  mutate(
    prev_discharge_time = lag(discharge_time_min_raw),
    flag_overlap_with_prev = if_else(
      !is.na(prev_discharge_time) & admission_time_min_raw < prev_discharge_time,
      TRUE,
      FALSE
    )
  ) %>%
  ungroup()

cat("\n--- Time Data QC ---\n")
n_los_err <- sum(timeline_data$flag_los_error, na.rm = TRUE)
n_op_err <- sum(timeline_data$flag_op_time_error, na.rm = TRUE)
n_death_err <- sum(timeline_data$flag_death_before_admission, na.rm = TRUE)
n_overlap <- sum(timeline_data$flag_overlap_with_prev, na.rm = TRUE)
cat(sprintf("LOS inversion (discharge < admission): %d\n", n_los_err))
cat(sprintf("Operation time inversion (end < start): %d\n", n_op_err))
cat(sprintf("Death before admission: %d\n", n_death_err))
cat(sprintf("Admission overlaps with previous discharge: %d\n", n_overlap))

timeline_final <- timeline_data %>%
  select(
    op_id, subject_id, hadm_id, case_id, opdate,
    admission_time_min_raw, discharge_time_min_raw,
    opstart_time_min_raw, opend_time_min_raw,
    anstart_time_min_raw, anend_time_min_raw,
    cpbon_time_min_raw, cpboff_time_min_raw,
    inhosp_death_time_min_raw, allcause_death_time_min_raw,
    op_duration_min, anesthesia_duration_min,
    or_room_time_min, cpb_duration_min,
    hosp_los_min, hosp_los_days,
    icu_los_min, icu_los_days,
    time_to_inhosp_death_min, time_to_inhosp_death_days,
    time_to_allcause_death_min, time_to_allcause_death_days,
    flag_los_error, flag_op_time_error,
    flag_death_before_admission, flag_overlap_with_prev
  ) %>%
  arrange(subject_id, hadm_id, op_id)

fwrite(timeline_final, file.path(output_path, "Time_Related_Data.csv"))
cat(" -> Time_Related_Data.csv saved.\n")

cat("\nAll done.\n")
