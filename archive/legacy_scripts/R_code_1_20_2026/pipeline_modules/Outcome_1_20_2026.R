library(data.table)
library(tidyverse)

# ==============================================================================
# 1. 环境设置与数据读取
# ==============================================================================
raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Outcomes_1_20_2026"

if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)

cat("Step 1: 读取原始数据...\n")

ops <- fread(
  file.path(raw_path, "operations.csv"),
  select = c(
    "op_id", "subject_id", "hadm_id", "admission_time", "orin_time",
    "orout_time", "anend_time", "discharge_time",
    "allcause_death_time", "inhosp_death_time"
  )
)

diagnosis <- fread(
  file.path(raw_path, "diagnosis.csv"),
  select = c("subject_id", "chart_time", "icd10_cm")
)

labs_core <- fread(
  file.path(raw_path, "labs.csv"),
  select = c("subject_id", "item_name", "chart_time", "value")
)

time_cols <- c(
  "admission_time", "orin_time", "orout_time", "anend_time",
  "discharge_time", "allcause_death_time", "inhosp_death_time"
)
ops[, (time_cols) := lapply(.SD, as.numeric), .SDcols = time_cols]
diagnosis[, chart_time := as.numeric(chart_time)]
diagnosis[, icd10_cm := toupper(trimws(icd10_cm))]
labs_core[, chart_time := as.numeric(chart_time)]
labs_core[, lab_value := as.numeric(value)]
labs_core[, item_name := tolower(trimws(item_name))]

creatinine <- labs_core[item_name == "creatinine" & !is.na(lab_value)]
troponin <- labs_core[item_name %in% c("troponin_i", "troponin_t") & !is.na(lab_value)]

# 手术序列锚点：同一次住院内，避免把后续手术后的事件归到前一次手术
setorder(ops, subject_id, hadm_id, orin_time, op_id)
ops[, prev_orout_time := shift(orout_time, type = "lag"), by = .(subject_id, hadm_id)]
ops[, next_orin_time := shift(orin_time, type = "lead"), by = .(subject_id, hadm_id)]

ops[, baseline_start := admission_time]
ops[!is.na(prev_orout_time) & !is.na(admission_time), baseline_start := pmax(admission_time, prev_orout_time)]
ops[!is.na(prev_orout_time) & is.na(admission_time), baseline_start := prev_orout_time]

ops[, icd_window_end := discharge_time]
ops[!is.na(next_orin_time) & !is.na(discharge_time), icd_window_end := pmin(discharge_time, next_orin_time)]
ops[!is.na(next_orin_time) & is.na(discharge_time), icd_window_end := next_orin_time]

ops[, aki_window_end := discharge_time]
ops[!is.na(orout_time) & !is.na(aki_window_end), aki_window_end := pmin(aki_window_end, orout_time + 7 * 1440)]
ops[!is.na(orout_time) & is.na(aki_window_end), aki_window_end := orout_time + 7 * 1440]
ops[!is.na(next_orin_time) & !is.na(aki_window_end), aki_window_end := pmin(aki_window_end, next_orin_time)]
ops[!is.na(next_orin_time) & is.na(aki_window_end), aki_window_end := next_orin_time]

# ==============================================================================
# 2. 术后并发症 (ICD-10): raw + incident 双版本
# ==============================================================================
cat("Step 2: 计算术后并发症 (raw + incident 双版本)...\n")

icd_outcome_cols <- c(
  "Stroke", "Stroke_Broad", "Cognitive_Decline", "Cardiac_Arrest",
  "Heart_Failure", "Myocardial_Injury", "Myocardial_Infarction_ICD", "Angina",
  "Arrhythmia_Vent", "Atrial_Fib", "Resp_Failure",
  "Pneumonia", "Sepsis", "Infection_Organ", "Infection_Unk"
)

calc_icd_flags <- function(dt) {
  dt[, `:=`(
    # 主口径：仅 I63/I64；敏感性口径保留 I65/I66
    Stroke            = as.integer(icd10_cm %in% c("I63", "I64")),
    Stroke_Broad      = as.integer(icd10_cm %in% c("I63", "I64", "I65", "I66")),
    Cognitive_Decline = as.integer(icd10_cm == "R41"),
    Cardiac_Arrest    = as.integer(icd10_cm == "I46"),
    Heart_Failure     = as.integer(icd10_cm %in% c("I50", "I11", "I13")),
    Myocardial_Injury = as.integer(icd10_cm == "I21"),
    Myocardial_Infarction_ICD = as.integer(icd10_cm == "I21"),
    Angina            = as.integer(icd10_cm == "I20"),
    Arrhythmia_Vent   = as.integer(grepl("^I47|^I49", icd10_cm)),
    Atrial_Fib        = as.integer(grepl("^I48", icd10_cm)),
    Resp_Failure      = as.integer(grepl("^J96", icd10_cm)),
    Pneumonia         = as.integer(icd10_cm %in% c("J18", "J15", "J17", "J12", "J16", "J13", "J09")),
    Sepsis            = as.integer(icd10_cm %in% c("A40", "A41")),
    Infection_Organ   = as.integer(icd10_cm %in% c("K65", "K57", "J85", "G06")),
    Infection_Unk     = as.integer(icd10_cm %in% c("A49", "B34", "J22"))
  )]
}

diag_postop <- diagnosis[
  ops,
  on = .(
    subject_id,
    chart_time > orin_time,
    chart_time <= icd_window_end
  ),
  .(op_id, icd10_cm),
  nomatch = NULL
]

diag_preop <- diagnosis[
  ops,
  on = .(
    subject_id,
    chart_time < orin_time
  ),
  .(op_id, icd10_cm),
  nomatch = NULL
]

calc_icd_flags(diag_postop)
calc_icd_flags(diag_preop)

outcomes_icd_raw <- diag_postop[, lapply(.SD, max), by = op_id, .SDcols = icd_outcome_cols]
outcomes_icd_preop <- diag_preop[, lapply(.SD, max), by = op_id, .SDcols = icd_outcome_cols]

icd_base <- ops[, .(op_id)]
outcomes_icd_raw <- merge(icd_base, outcomes_icd_raw, by = "op_id", all.x = TRUE)
outcomes_icd_preop <- merge(icd_base, outcomes_icd_preop, by = "op_id", all.x = TRUE)

for (nm in icd_outcome_cols) {
  set(outcomes_icd_raw, which(is.na(outcomes_icd_raw[[nm]])), nm, 0L)
  set(outcomes_icd_preop, which(is.na(outcomes_icd_preop[[nm]])), nm, 0L)
}

# incident 规则：术后有 + 术前无
outcomes_icd_incident <- copy(outcomes_icd_raw)
for (nm in icd_outcome_cols) {
  outcomes_icd_incident[, (nm) := as.integer(
    outcomes_icd_raw[[nm]] == 1L & outcomes_icd_preop[[nm]] == 0L
  )]
}

# 输出一张 ICD 事件差异统计，便于审计
icd_compare <- rbindlist(lapply(icd_outcome_cols, function(nm) {
  raw_n <- sum(outcomes_icd_raw[[nm]], na.rm = TRUE)
  incident_n <- sum(outcomes_icd_incident[[nm]], na.rm = TRUE)
  overlap_n <- sum(outcomes_icd_raw[[nm]] == 1L & outcomes_icd_preop[[nm]] == 1L, na.rm = TRUE)
  data.table(
    Outcome = nm,
    Raw_Count = raw_n,
    Incident_Count = incident_n,
    Preop_Overlap_Count = overlap_n,
    Overlap_Pct_Of_Raw = round(100 * overlap_n / ifelse(raw_n > 0, raw_n, 1), 2)
  )
}))
fwrite(icd_compare, file.path(processed_path, "postop_icd_raw_vs_incident_comparison.csv"))

# ==============================================================================
# 3. AKI (KDIGO 标准)
# ==============================================================================
cat("Step 3: 计算 AKI (KDIGO 规则)...\n")

baseline_dt <- creatinine[
  ops,
  on = .(subject_id, chart_time >= baseline_start, chart_time < orin_time),
  .(op_id, scr = lab_value),
  nomatch = 0L
][, .(baseline_scr = min(scr, na.rm = TRUE)), by = op_id]
baseline_dt[is.infinite(baseline_scr), baseline_scr := NA]

postop_dt <- creatinine[
  ops,
  on = .(subject_id, chart_time >= orout_time, chart_time <= aki_window_end),
  .(op_id, chart_time = x.chart_time, scr = x.lab_value, orout_time = i.orout_time),
  nomatch = 0L
]

aki_calc <- merge(postop_dt, baseline_dt, by = "op_id")
aki_calc <- aki_calc[!is.na(baseline_scr) & !is.na(scr)]

aki_calc[, `:=`(
  delta = scr - baseline_scr,
  ratio = scr / baseline_scr,
  is_48h = (chart_time <= orout_time + 48 * 60)
)]

aki_calc[, `:=`(
  stage3 = (ratio >= 3.0) | (scr >= 4.0),
  stage2 = (ratio >= 2.0 & ratio < 3.0),
  stage1 = (is_48h & delta >= 0.3) | (ratio >= 1.5 & ratio < 2.0)
)]

outcomes_aki <- aki_calc[, .(
  AKI_Max_Stage = max(fcase(stage3, 3L, stage2, 2L, stage1, 1L, default = 0L))
), by = op_id]

aki_eval <- merge(
  ops[, .(op_id)],
  baseline_dt[, .(op_id, has_baseline_scr = as.integer(!is.na(baseline_scr)))],
  by = "op_id",
  all.x = TRUE
)
aki_eval <- merge(
  aki_eval,
  unique(postop_dt[, .(op_id)])[,
    .(op_id, has_postop_scr = 1L)
  ],
  by = "op_id",
  all.x = TRUE
)
aki_eval[is.na(has_baseline_scr), has_baseline_scr := 0L]
aki_eval[is.na(has_postop_scr), has_postop_scr := 0L]
aki_eval[, AKI_Evaluable := as.integer(has_baseline_scr == 1L & has_postop_scr == 1L)]

outcomes_aki[, `:=`(
  AKI_Any = as.integer(AKI_Max_Stage >= 1),
  AKI_Stage_1 = as.integer(AKI_Max_Stage == 1),
  AKI_Stage_2 = as.integer(AKI_Max_Stage == 2),
  AKI_Stage_3 = as.integer(AKI_Max_Stage == 3)
)]

outcomes_aki <- merge(aki_eval[, .(op_id, AKI_Evaluable)], outcomes_aki, by = "op_id", all.x = TRUE)
for (nm in c("AKI_Max_Stage", "AKI_Any", "AKI_Stage_1", "AKI_Stage_2", "AKI_Stage_3")) {
  set(outcomes_aki, which(is.na(outcomes_aki[[nm]])), nm, 0L)
}

# 术后肌钙蛋白监测信息（提示 MINS 风险；非标准 URL 诊断）
troponin_preop <- troponin[
  ops,
  on = .(subject_id, chart_time >= baseline_start, chart_time < orin_time),
  .(op_id, troponin = x.lab_value),
  nomatch = 0L
][, .(
  Troponin_Preop_Measured = as.integer(.N > 0L),
  Troponin_Preop_Max = suppressWarnings(max(troponin, na.rm = TRUE))
), by = op_id]
troponin_preop[is.infinite(Troponin_Preop_Max), Troponin_Preop_Max := NA_real_]

troponin_postop <- troponin[
  ops,
  on = .(subject_id, chart_time >= orout_time, chart_time <= aki_window_end),
  .(op_id, troponin = x.lab_value),
  nomatch = 0L
][, .(
  Troponin_Postop_Measured = as.integer(.N > 0L),
  Troponin_Postop_Max = suppressWarnings(max(troponin, na.rm = TRUE))
), by = op_id]
troponin_postop[is.infinite(Troponin_Postop_Max), Troponin_Postop_Max := NA_real_]

troponin_flags <- merge(ops[, .(op_id)], troponin_preop, by = "op_id", all.x = TRUE)
troponin_flags <- merge(troponin_flags, troponin_postop, by = "op_id", all.x = TRUE)
for (nm in c("Troponin_Preop_Measured", "Troponin_Postop_Measured")) {
  set(troponin_flags, which(is.na(troponin_flags[[nm]])), nm, 0L)
}
troponin_flags[, MINS_Possible_Troponin_Rise := as.integer(
  Troponin_Preop_Measured == 1L &
    Troponin_Postop_Measured == 1L &
    !is.na(Troponin_Postop_Max) &
    !is.na(Troponin_Preop_Max) &
    Troponin_Postop_Max > Troponin_Preop_Max
)]

# ==============================================================================
# 4. 死亡结局
# ==============================================================================
cat("Step 4: 计算死亡结局...\n")

outcomes_death <- ops[, .(
  op_id, admission_time, anend_time, discharge_time,
  allcause_death_time, inhosp_death_time
)]

outcomes_death[, death_time := fcoalesce(allcause_death_time, inhosp_death_time)]
outcomes_death[, Survival_Days := round((death_time - anend_time) / 1440.0, 1)]

outcomes_death[, `:=`(
  Death_In_Hospital = as.integer(
    !is.na(death_time) &
      death_time <= discharge_time &
      death_time >= admission_time
  ),
  Death_POD30 = 0L,
  Death_POD90 = 0L,
  Death_1_Year = 0L,
  Death_Long_Term = as.integer(!is.na(death_time))
)]

outcomes_death[!is.na(Survival_Days) & Survival_Days >= 0 & Survival_Days <= 30, Death_POD30 := 1L]
outcomes_death[!is.na(Survival_Days) & Survival_Days >= 0 & Survival_Days <= 90, Death_POD90 := 1L]
outcomes_death[!is.na(Survival_Days) & Survival_Days >= 0 & Survival_Days <= 365, Death_1_Year := 1L]

# ==============================================================================
# 5. 合并输出：raw + incident
# ==============================================================================
cat("Step 5: 合并并输出 raw/incident 两个版本...\n")

build_final_df <- function(icd_dt) {
  out <- ops[, .(op_id, subject_id)] %>%
    merge(icd_dt, by = "op_id", all.x = TRUE) %>%
    merge(
      outcomes_aki[, .(op_id, AKI_Evaluable, AKI_Any, AKI_Stage_1, AKI_Stage_2, AKI_Stage_3)],
      by = "op_id",
      all.x = TRUE
    ) %>%
    merge(
      troponin_flags[, .(op_id, Troponin_Preop_Measured, Troponin_Postop_Measured, MINS_Possible_Troponin_Rise)],
      by = "op_id",
      all.x = TRUE
    ) %>%
    merge(
      outcomes_death[, .(op_id, Death_In_Hospital, Death_POD30, Death_POD90, Death_1_Year, Death_Long_Term, Survival_Days)],
      by = "op_id", all.x = TRUE
    )

  cols_to_fill <- setdiff(names(out), c("op_id", "subject_id", "Survival_Days"))
  for (j in cols_to_fill) {
    set(out, which(is.na(out[[j]])), j, 0L)
  }
  out[]
}

final_df_raw <- build_final_df(outcomes_icd_raw)
final_df_incident <- build_final_df(outcomes_icd_incident)

file_raw <- file.path(processed_path, "postop_outcomes_final_raw.csv")
file_incident <- file.path(processed_path, "postop_outcomes_final_incident.csv")
file_compat <- file.path(processed_path, "postop_outcomes_final.csv") # 兼容旧流程：保留 raw

fwrite(final_df_raw, file_raw)
fwrite(final_df_incident, file_incident)
fwrite(final_df_raw, file_compat)

cat(sprintf("已保存 raw: %s\n", file_raw))
cat(sprintf("已保存 incident: %s\n", file_incident))
cat(sprintf("已保存兼容文件 (raw): %s\n", file_compat))

# ==============================================================================
# 6. 总结报告
# ==============================================================================
build_summary_table <- function(df) {
  outcome_cols <- setdiff(names(df), c("op_id", "subject_id", "Survival_Days"))
  out <- data.table(Outcome = outcome_cols)
  out[, `:=`(
    Count = sapply(outcome_cols, function(x) sum(df[[x]], na.rm = TRUE)),
    Rate_Pct = sapply(outcome_cols, function(x) round(mean(df[[x]], na.rm = TRUE) * 100, 2))
  )]
  out[]
}

summary_raw <- build_summary_table(final_df_raw)
summary_incident <- build_summary_table(final_df_incident)

summary_cmp <- merge(
  summary_raw[, .(Outcome, Count_Raw = Count, Rate_Pct_Raw = Rate_Pct)],
  summary_incident[, .(Outcome, Count_Incident = Count, Rate_Pct_Incident = Rate_Pct)],
  by = "Outcome",
  all = TRUE
)
summary_cmp[, `:=`(
  Delta_Count = Count_Incident - Count_Raw,
  Delta_Rate_Pct = round(Rate_Pct_Incident - Rate_Pct_Raw, 4)
)]
setorder(summary_cmp, Delta_Rate_Pct)

fwrite(summary_raw, file.path(processed_path, "postop_outcomes_summary_raw.csv"))
fwrite(summary_incident, file.path(processed_path, "postop_outcomes_summary_incident.csv"))
fwrite(summary_raw, file.path(processed_path, "postop_outcomes_summary.csv")) # 兼容旧流程：保留 raw
fwrite(summary_cmp, file.path(processed_path, "postop_outcomes_summary_raw_vs_incident.csv"))

cat("\n=======================================================\n")
cat("POST-OPERATIVE OUTCOMES SUMMARY (RAW TOP 15)\n")
cat("=======================================================\n")
print(summary_raw[order(-Rate_Pct)][1:min(15, .N)])
cat("\n=======================================================\n")
cat("RAW vs INCIDENT DELTA (TOP 15 ABS CHANGE)\n")
cat("=======================================================\n")
print(summary_cmp[order(-abs(Delta_Rate_Pct))][1:min(15, .N)])
cat("Done!\n")
