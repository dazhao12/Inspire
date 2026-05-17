suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

project_root <- "/N/project/analgesia_perioperation"
processed_root <- file.path(project_root, "data", "INSPIRE_1.3", "processed")
legacy_script_root <- file.path(project_root, "projects", "Inspire_data_process_ZZ", "R_code_1_20_2026")
legacy_module_root <- file.path(legacy_script_root, "pipeline_modules")
intermediate_root <- file.path(project_root, "projects", "Inspire_data_process_ZZ", "intermediate")
documents_root <- file.path(project_root, "documents", "INSPIRE_1.3", "processing_docs")
archive_legacy_root <- file.path(project_root, "projects", "Inspire_data_process_ZZ", "archive_legacy_data")
archive_module_outputs_root <- file.path(archive_legacy_root, "legacy_module_outputs")

args <- commandArgs(trailingOnly = TRUE)
skip_legacy <- "--skip-legacy" %in% args
release_date_arg <- grep("^--release-date=", args, value = TRUE)
release_date <- if (length(release_date_arg) == 1L) {
  sub("^--release-date=", "", release_date_arg)
} else {
  as.character(Sys.Date())
}

release_dir <- processed_root
documents_dir <- file.path(documents_root, release_date)
intermediate_data_dir <- file.path(intermediate_root, "current")

canonical_files <- list(
  demographics_subject = "preop_demographics_subject_level.csv",
  baseline_full = "periop_baseline_operations_core_plus_timeline.csv",
  baseline_preop = "preop_baseline_operations_core.csv",
  baseline_timeline = "periop_timeline_operations_raw_and_derived.csv",
  diagnosis = "preop_diagnosis_flags_cumulative_preop.csv",
  diagnosis_current = "preop_diagnosis_flags_current_stay.csv",
  diagnosis_cumulative = "preop_diagnosis_flags_cumulative_preop.csv",
  labs_7d = "preop_labs_window_7d.csv",
  labs_30d = "preop_labs_window_30d.csv",
  labs_current_stay = "preop_labs_window_current_stay.csv",
  labs_cumulative = "preop_labs_window_cumulative_preop.csv",
  labs_all_history = "preop_labs_window_cumulative_preop.csv",
  meds = "preop_medications_flags_current_stay.csv",
  vitals_preop = "preop_vitals_baseline.csv",
  vitals_intraop_ts = "intraop_vitals_timeseries.csv",
  intraop_totals = "intraop_drugs_fluids_totals.csv",
  outcomes = "postop_outcomes.csv",
  outcomes_incident = "postop_outcomes_incident.csv",
  master = "periop_master_dataset_all_features.csv"
)

dir_create <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

dir_create(intermediate_data_dir)
dir_create(release_dir)
dir_create(documents_dir)
dir_create(archive_module_outputs_root)

message_line <- function(...) {
  cat(sprintf(...), "\n", sep = "")
}

run_legacy_script <- function(script_name) {
  script_path <- file.path(legacy_module_root, script_name)
  if (!file.exists(script_path)) {
    stop(sprintf("Legacy script not found: %s", script_path))
  }
  message_line("Running legacy module: %s", script_name)
  result <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", script_path),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) {
    cat(paste(result, collapse = "\n"), "\n")
    stop(sprintf("Legacy module failed: %s", script_name))
  }
}

legacy_output_specs <- list(
  demographics_subject = c("Demographics_Timeline_2_19_2026", "Demographic_Subject_Level.csv"),
  demographics_operation = c("Demographics_Timeline_2_19_2026", "Demographic_Operation_Level.csv"),
  timeline_operation = c("Demographics_Timeline_2_19_2026", "Time_Related_Data.csv"),
  diagnosis = c("Diagnosis_1_20_2026", "diag_preop_flags_final.csv"),
  diagnosis_current = c("Diagnosis_1_20_2026", "diag_preop_flags_current_stay.csv"),
  diagnosis_cumulative = c("Diagnosis_1_20_2026", "diag_preop_flags_cumulative.csv"),
  diagnosis_summary = c("Diagnosis_1_20_2026", "diag_preop_summary_stats.csv"),
  labs_7d = c("lab_data_v1_1_20_2026", "preop_labs_features_7d.csv"),
  labs_30d = c("lab_data_v1_1_20_2026", "preop_labs_features_30d.csv"),
  labs_any = c("lab_data_v1_1_20_2026", "preop_labs_features_any.csv"),
  labs_current = c("lab_data_v1_1_20_2026", "preop_labs_features_current_stay.csv"),
  labs_cumulative = c("lab_data_v1_1_20_2026", "preop_labs_features_cumulative_preop.csv"),
  meds = c("Meds_Preop_1_20_2026", "preop_meds.csv"),
  meds_summary = c("Meds_Preop_1_20_2026", "preop_meds_summary_stats.csv"),
  vitals_preop = c("Vials_pro_1_20_2026", "preop_baseline_final.csv"),
  vitals_preop_summary = c("Vials_pro_1_20_2026", "preop_vitals_summary_coverage.csv"),
  vitals_intraop = c("Vials_intra_1_21_final_2026", "vital_intraop_full_complete.csv"),
  intraop_total_sum = c("Vials_intra_1_21_final_2026", "drugs_fluids_total_sum.csv"),
  intraop_summary = c("Vials_intra_1_21_final_2026", "drugs_fluids_descriptive_stats.csv"),
  outcomes = c("Outcomes_1_20_2026", "postop_outcomes_final.csv"),
  outcomes_summary = c("Outcomes_1_20_2026", "postop_outcomes_summary.csv")
)

resolve_legacy_output_path <- function(dir_name, file_name) {
  top_level_path <- file.path(processed_root, dir_name, file_name)
  archived_path <- file.path(archive_module_outputs_root, dir_name, file_name)
  if (file.exists(top_level_path)) {
    return(top_level_path)
  }
  archived_path
}

archive_legacy_generated_outputs <- function() {
  generated_dirs <- c(
    "Demographics_Timeline_2_19_2026",
    "Diagnosis_1_20_2026",
    "lab_data_v1_1_20_2026",
    "Meds_Preop_1_20_2026",
    "Vials_pro_1_20_2026",
    "Vials_intra_1_21_final_2026",
    "Outcomes_1_20_2026"
  )

  for (dir_name in generated_dirs) {
    src_dir <- file.path(processed_root, dir_name)
    dst_dir <- file.path(archive_module_outputs_root, dir_name)
    if (!dir.exists(src_dir)) {
      next
    }
    if (dir.exists(dst_dir)) {
      unlink(dst_dir, recursive = TRUE, force = TRUE)
    }
    ok <- file.rename(src_dir, dst_dir)
    if (!ok) {
      stop(sprintf("Failed to archive legacy output directory: %s", src_dir))
    }
  }
}

legacy_scripts <- c(
  "Process_Demographics_and_Timeline_v1.R",
  "Diagnosis_v1_1_20_2026.R",
  "Lab_v1_1_20_2026.R",
  "Medicine_pro_v1_1_20_2026.R",
  "Vials_pro_v1_1_20_2026.R",
  "Vials_intra_v1_1_21_2026.R",
  "Vials_intra_summary_v1_1_21_2026.R",
  "Outcome_1_20_2026.R"
)

assert_files_exist <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(sprintf("Required files are missing:\n%s", paste(missing, collapse = "\n")))
  }
}

if (!skip_legacy) {
  for (script_name in legacy_scripts) {
    run_legacy_script(script_name)
  }
}

legacy_outputs <- lapply(legacy_output_specs, function(parts) resolve_legacy_output_path(parts[1], parts[2]))
legacy_output_optional <- list(
  outcomes_incident = resolve_legacy_output_path("Outcomes_1_20_2026", "postop_outcomes_final_incident.csv")
)

assert_files_exist(unlist(legacy_outputs, use.names = FALSE))

copy_csv <- function(src, dest) {
  dir_create(dirname(dest))
  ok <- file.copy(src, dest, overwrite = TRUE)
  if (!ok) {
    stop(sprintf("Failed to copy %s -> %s", src, dest))
  }
}

raw_operations_path <- file.path(project_root, "data", "INSPIRE_1.3", "raw", "operations.csv")
raw_parameters_path <- file.path(project_root, "data", "INSPIRE_1.3", "raw", "parameters.csv")
id_map <- unique(fread(raw_operations_path, select = c("op_id", "subject_id", "hadm_id")))
setkey(id_map, op_id)

standardize_table_file <- function(file_path) {
  if (!file.exists(file_path)) {
    return(invisible(FALSE))
  }

  header <- names(fread(file_path, nrows = 0L, showProgress = FALSE))
  if (!("op_id" %in% header) && ("subject_id" %in% header)) {
    dt <- fread(file_path, showProgress = FALSE)
    setorderv(dt, "subject_id", na.last = TRUE)
    fwrite(dt, file_path)
    return(invisible(TRUE))
  }

  if (!("op_id" %in% header)) {
    return(invisible(FALSE))
  }

  dt <- fread(file_path, showProgress = FALSE)
  need_subject <- !("subject_id" %in% names(dt))
  need_hadm <- !("hadm_id" %in% names(dt))

  if (need_subject || need_hadm) {
    add_cols <- c("op_id")
    if (need_subject) add_cols <- c(add_cols, "subject_id")
    if (need_hadm) add_cols <- c(add_cols, "hadm_id")
    add_map <- unique(id_map[, ..add_cols])
    dt <- merge(dt, add_map, by = "op_id", all.x = TRUE, sort = FALSE)
  }

  id_cols <- c("subject_id", "hadm_id", "op_id")
  keep_first <- id_cols[id_cols %in% names(dt)]
  setcolorder(dt, c(keep_first, setdiff(names(dt), keep_first)))
  setorderv(dt, keep_first, na.last = TRUE)
  fwrite(dt, file_path)
  invisible(TRUE)
}

normalize_unit_token <- function(unit_value) {
  if (is.na(unit_value) || unit_value == "") {
    return("unitless")
  }
  x <- unit_value
  x <- gsub("%", "pct", x, fixed = TRUE)
  x <- gsub("/nL", "per_nL", x, fixed = TRUE)
  x <- gsub("/min", "_min", x, fixed = TRUE)
  x <- gsub("/h", "_h", x, fixed = TRUE)
  x <- gsub("/m2", "_m2", x, fixed = TRUE)
  x <- gsub("/kg", "_kg", x, fixed = TRUE)
  x <- gsub("/mL", "_mL", x, fixed = TRUE)
  x <- gsub("/L", "_L", x, fixed = TRUE)
  x <- gsub("/", "_", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- gsub("[^A-Za-z0-9_]+", "", x)
  x <- gsub("__+", "_", x)
  tolower(x)
}

build_unit_maps_from_parameters <- function(parameters_path) {
  if (!file.exists(parameters_path)) {
    return(list(labs = setNames(character(), character()), vitals = setNames(character(), character())))
  }
  dt <- fread(parameters_path, encoding = "UTF-8")
  if (length(names(dt)) > 0L) {
    names(dt)[1] <- sub("^\\ufeff", "", names(dt)[1])
  }
  required_cols <- c("Table", "Label", "Unit")
  if (!all(required_cols %in% names(dt))) {
    return(list(labs = setNames(character(), character()), vitals = setNames(character(), character())))
  }
  dt[, `:=`(
    Table = tolower(trimws(Table)),
    Label = tolower(trimws(Label)),
    Unit = trimws(Unit)
  )]
  dt[, unit_token := vapply(Unit, normalize_unit_token, character(1))]

  labs_dt <- unique(dt[Table == "labs", .(Label, unit_token)])
  vitals_dt <- unique(dt[Table == "vitals", .(Label, unit_token)])
  labs_map <- setNames(labs_dt$unit_token, labs_dt$Label)
  vitals_map <- setNames(vitals_dt$unit_token, vitals_dt$Label)
  vitals_map["ds"] <- "ml"

  list(labs = labs_map, vitals = vitals_map)
}

unit_maps <- build_unit_maps_from_parameters(raw_parameters_path)
lab_unit_map_from_params <- unit_maps$labs
intra_unit_map_from_params <- unit_maps$vitals

rename_labs_cols_with_units <- function(col_names) {
  out <- col_names
  idx <- grep("^preop_.*_(nearest|median|mean)(_.+)?$", col_names)
  if (length(idx) == 0L) {
    return(out)
  }
  for (i in idx) {
    col_name <- col_names[i]
    parts <- regmatches(col_name, regexec("^preop_(.*)_(nearest|median|mean)(_.+)?$", col_name))[[1]]
    if (length(parts) < 3L) {
      next
    }
    item_name <- parts[2]
    stat_name <- parts[3]
    unit_name <- if (!is.na(lab_unit_map_from_params[item_name])) unname(lab_unit_map_from_params[item_name]) else "unknown_unit"
    out[i] <- sprintf("preop_%s_%s_%s", item_name, stat_name, unit_name)
  }
  out
}

rename_intra_cols_with_units <- function(col_names) {
  out <- col_names
  idx <- grep("^[a-z0-9]+_(sum|mean)_.+$", col_names)
  if (length(idx) == 0L) {
    return(out)
  }
  for (i in idx) {
    col_name <- col_names[i]
    parts <- regmatches(col_name, regexec("^([a-z0-9]+)_(sum|mean)_(.+)$", col_name))[[1]]
    if (length(parts) != 4L) {
      next
    }
    base_name <- parts[2]
    agg_name <- parts[3]
    if (!is.na(intra_unit_map_from_params[base_name])) {
      unit_name <- unname(intra_unit_map_from_params[base_name])
      out[i] <- sprintf("%s_%s_%s", base_name, agg_name, unit_name)
    }
  }
  out
}

apply_parameter_unit_suffixes <- function(file_path, apply_labs = FALSE, apply_intra = FALSE) {
  if (!file.exists(file_path)) {
    return(invisible(FALSE))
  }
  old_names <- names(fread(file_path, nrows = 0L, showProgress = FALSE))
  new_names <- old_names
  if (apply_labs) {
    new_names <- rename_labs_cols_with_units(new_names)
  }
  if (apply_intra) {
    new_names <- rename_intra_cols_with_units(new_names)
  }
  if (identical(old_names, new_names)) {
    return(invisible(FALSE))
  }
  if (anyDuplicated(new_names)) {
    stop(sprintf("Duplicate columns after unit normalization: %s", file_path))
  }
  dt <- fread(file_path, showProgress = FALSE)
  setnames(dt, old = names(dt), new = new_names)
  fwrite(dt, file_path)
  invisible(TRUE)
}

shell_line_count <- function(path) {
  nrow(fread(path, showProgress = FALSE))
}

detect_value_type <- function(x, name) {
  if (name %in% c("op_id", "subject_id", "hadm_id", "case_id", "surgery_number")) {
    return("identifier")
  }
  if (grepl("_time_min_raw$", name) || grepl("_time$", name)) {
    return("relative_time_min")
  }
  if (grepl("_days$", name)) {
    return("continuous_days")
  }
  if (grepl("_min$", name) || grepl("^min_from_entry$", name)) {
    return("continuous_minutes")
  }
  if (is.character(x)) {
    return("categorical_text")
  }
  if (is.logical(x)) {
    return("logical_flag")
  }
  x_non_na <- x[!is.na(x)]
  if (length(x_non_na) > 0L && all(x_non_na %in% c(0, 1))) {
    return("binary_flag")
  }
  if (is.integer(x)) {
    return("integer_numeric")
  }
  if (is.numeric(x)) {
    return("continuous_numeric")
  }
  class(x)[1]
}

humanize_name <- function(name) {
  label <- name
  label <- gsub("_", " ", label, fixed = TRUE)
  label <- stringr::str_squish(label)
  label <- stringr::str_to_title(label)
  label <- gsub("\\bCpb\\b", "CPB", label)
  label <- gsub("\\bIcu\\b", "ICU", label)
  label <- gsub("\\bBmi\\b", "BMI", label)
  label <- gsub("\\bAki\\b", "AKI", label)
  label <- gsub("\\bPod\\b", "POD", label)
  label <- gsub("\\bSbp\\b", "SBP", label)
  label <- gsub("\\bDbp\\b", "DBP", label)
  label <- gsub("\\bMbp\\b", "MBP", label)
  label <- gsub("\\bHr\\b", "HR", label)
  label <- gsub("\\bSpo2\\b", "SpO2", label)
  label <- gsub("\\bRr\\b", "RR", label)
  label <- gsub("\\bBt\\b", "BT", label)
  label
}

lab_item_cn <- c(
  albumin = "白蛋白",
  alp = "碱性磷酸酶",
  alt = "丙氨酸转氨酶",
  aptt = "活化部分凝血活酶时间",
  ast = "天门冬氨酸转氨酶",
  be = "碱剩余",
  bun = "尿素氮",
  calcium = "总钙",
  chloride = "氯",
  ck = "肌酸激酶",
  ckmb = "肌酸激酶同工酶",
  creatinine = "肌酐",
  crp = "C 反应蛋白",
  d_dimer = "D-二聚体",
  fibrinogen = "纤维蛋白原",
  glucose = "葡萄糖",
  hb = "血红蛋白",
  hba1c = "糖化血红蛋白",
  hco3 = "碳酸氢根",
  hct = "红细胞压积",
  ica = "离子钙",
  lacate = "乳酸",
  lactate = "乳酸",
  lymphocyte = "淋巴细胞",
  paco2 = "动脉二氧化碳分压",
  pao2 = "动脉氧分压",
  ph = "酸碱度",
  phosphorus = "磷",
  platelet = "血小板",
  potassium = "钾",
  ptinr = "凝血酶原时间 INR",
  sao2 = "血氧饱和度",
  seg = "中性粒细胞比例",
  sodium = "钠",
  total_bilirubin = "总胆红素",
  total_protein = "总蛋白",
  troponin_i = "肌钙蛋白 I",
  troponin_t = "肌钙蛋白 T",
  wbc = "白细胞计数"
)

intra_item_cn <- c(
  etsevo = "呼气末七氟醚记录值总和",
  etdes = "呼气末地氟醚记录值总和",
  etiso = "呼气末异氟醚记录值总和",
  eph = "麻黄碱总量",
  phe = "去氧肾上腺素总量",
  pepi = "去甲肾上腺素前体记录值总和",
  nepi = "去甲肾上腺素总量",
  epi = "肾上腺素总量",
  epii = "肾上腺素持续泵注记录值总和",
  dopai = "多巴胺总量",
  dobui = "多巴酚丁胺总量",
  ntgi = "硝酸甘油总量",
  mlni = "米力农总量",
  vaso = "血管加压素总量",
  ppf = "丙泊酚总量",
  ppfi = "丙泊酚持续泵注记录值总和",
  rfti = "瑞芬太尼总量",
  ftn = "芬太尼总量",
  sft = "舒芬太尼总量",
  aft = "阿芬太尼总量",
  mdz = "咪达唑仑总量",
  ns = "生理盐水总量",
  hs = "高张盐水总量",
  psa = "平衡液总量",
  hns = "半盐水总量",
  hes = "羟乙基淀粉总量",
  d5w = "5% 葡萄糖总量",
  d10w = "10% 葡萄糖总量",
  d50w = "50% 葡萄糖总量",
  alb5 = "5% 白蛋白总量",
  alb20 = "20% 白蛋白总量",
  rbc = "红细胞输入总量",
  ffp = "新鲜冰冻血浆总量",
  pc = "血小板输入总量",
  pheresis = "单采成分血总量",
  cryo = "冷沉淀总量",
  ebl = "估计失血量",
  uo = "尿量",
  ds = "引流量"
)

extract_intraop_base_var <- function(column_name) {
  if (grepl("_any_use_flag$", column_name)) {
    return(sub("_any_use_flag$", "", column_name))
  }
  if (grepl("_(sum|mean)_[^_].*$", column_name)) {
    return(sub("_(sum|mean)_[^_].*$", "", column_name))
  }
  column_name
}

is_intraop_derived_column <- function(column_name) {
  base_name <- extract_intraop_base_var(column_name)
  base_name %in% names(intra_item_cn)
}

exact_desc_cn <- c(
  op_id = "手术唯一标识",
  subject_id = "患者唯一标识",
  hadm_id = "住院唯一标识",
  case_id = "手术病例标识",
  surgery_number = "同一患者手术序号",
  opdate = "手术日期",
  Male = "性别，男=1，女=0",
  Age = "手术时年龄",
  Height = "身高",
  Weight = "体重",
  BMI = "体重指数",
  race = "种族",
  asa = "ASA 分级",
  Emergency_op = "急诊手术标记",
  department = "手术科室",
  antype = "麻醉类型",
  icd10_pcs = "ICD-10-PCS 手术编码",
  op_duration_min = "手术持续时间",
  anesthesia_duration_min = "麻醉持续时间",
  or_room_time_min = "手术间停留时间",
  cpb_duration_min = "体外循环持续时间",
  hosp_los_min = "住院时长（分钟）",
  hosp_los_days = "住院时长（天）",
  icu_los_min = "ICU 停留时长（分钟）",
  icu_los_days = "ICU 停留时长（天）",
  time_to_inhosp_death_min = "入院至院内死亡时间（分钟）",
  time_to_inhosp_death_days = "入院至院内死亡时间（天）",
  time_to_allcause_death_min = "入院至全因死亡时间（分钟）",
  time_to_allcause_death_days = "入院至全因死亡时间（天）",
  admission_time_min_raw = "原始入院时间戳（相对参考点分钟）",
  discharge_time_min_raw = "原始出院时间戳（相对参考点分钟）",
  opstart_time_min_raw = "原始手术开始时间戳（相对参考点分钟）",
  opend_time_min_raw = "原始手术结束时间戳（相对参考点分钟）",
  anstart_time_min_raw = "原始麻醉开始时间戳（相对参考点分钟）",
  anend_time_min_raw = "原始麻醉结束时间戳（相对参考点分钟）",
  cpbon_time_min_raw = "原始体外循环开始时间戳（相对参考点分钟）",
  cpboff_time_min_raw = "原始体外循环结束时间戳（相对参考点分钟）",
  inhosp_death_time_min_raw = "原始院内死亡时间戳（相对参考点分钟）",
  allcause_death_time_min_raw = "原始全因死亡时间戳（相对参考点分钟）",
  flag_los_error = "出院时间早于入院时间的异常标记",
  flag_op_time_error = "手术结束时间早于手术开始时间的异常标记",
  flag_death_before_admission = "死亡时间早于入院时间的异常标记",
  flag_overlap_with_prev = "当前住院时间与前次记录可能重叠的标记",
  chart_time = "监测记录时间戳（相对参考点分钟）",
  min_from_entry = "距入手术室时间差（分钟）",
  preop_sbp = "术前收缩压",
  preop_dbp = "术前舒张压",
  preop_mbp = "术前平均动脉压",
  preop_hr = "术前心率",
  preop_spo2 = "术前血氧饱和度",
  preop_rr = "术前呼吸频率",
  preop_bt = "术前体温",
  source_sbp = "术前收缩压数据来源",
  Stroke = "术后脑卒中",
  Cognitive_Decline = "术后认知功能下降",
  Cardiac_Arrest = "术后心脏骤停",
  Heart_Failure = "术后心力衰竭",
  Myocardial_Injury = "术后心肌损伤",
  Angina = "术后心绞痛",
  Arrhythmia_Vent = "术后室性心律失常",
  Atrial_Fib = "术后房颤",
  Resp_Failure = "术后呼吸衰竭",
  Pneumonia = "术后肺炎",
  Sepsis = "术后脓毒症",
  Infection_Organ = "术后器官特异性感染",
  Infection_Unk = "术后未特指感染",
  AKI_Any = "术后任意级别 AKI",
  AKI_Stage_1 = "术后 AKI 1 级",
  AKI_Stage_2 = "术后 AKI 2 级",
  AKI_Stage_3 = "术后 AKI 3 级",
  Death_In_Hospital = "本次住院期间死亡",
  Death_POD30 = "术后 30 天内死亡",
  Death_POD90 = "术后 90 天内死亡",
  Death_1_Year = "术后 1 年内死亡",
  Death_Long_Term = "长期死亡状态标记",
  Survival_Days = "距麻醉结束至死亡的生存天数"
)

unit_map <- c(
  Height = "cm",
  Weight = "kg",
  BMI = "kg/m^2",
  op_duration_min = "min",
  anesthesia_duration_min = "min",
  or_room_time_min = "min",
  cpb_duration_min = "min",
  hosp_los_min = "min",
  hosp_los_days = "days",
  icu_los_min = "min",
  icu_los_days = "days",
  time_to_inhosp_death_min = "min",
  time_to_inhosp_death_days = "days",
  time_to_allcause_death_min = "min",
  time_to_allcause_death_days = "days",
  preop_sbp = "mmHg",
  preop_dbp = "mmHg",
  preop_mbp = "mmHg",
  preop_hr = "bpm",
  preop_spo2 = "%",
  preop_rr = "breaths/min",
  preop_bt = "C",
  ebl = "raw_source_unit",
  uo = "raw_source_unit",
  ds = "raw_source_unit",
  Survival_Days = "days"
)

describe_column <- function(column_name, table_name, table_time_window) {
  if (column_name %in% names(exact_desc_cn)) {
    return(unname(exact_desc_cn[[column_name]]))
  }

  if (grepl("^(.*)_sum_([^_].*)$", column_name)) {
    parts <- regmatches(column_name, regexec("^(.*)_sum_([^_].*)$", column_name))[[1]]
    base_name <- parts[2]
    unit_name <- parts[3]
    base_label <- if (base_name %in% names(intra_item_cn)) intra_item_cn[[base_name]] else humanize_name(base_name)
    return(sprintf("术中%s（手术内总量，单位 %s）", base_label, unit_name))
  }

  if (grepl("^(.*)_mean_([^_].*)$", column_name)) {
    parts <- regmatches(column_name, regexec("^(.*)_mean_([^_].*)$", column_name))[[1]]
    base_name <- parts[2]
    unit_name <- parts[3]
    base_label <- if (base_name %in% names(intra_item_cn)) intra_item_cn[[base_name]] else humanize_name(base_name)
    return(sprintf("术中%s（手术内平均记录值，单位 %s）", base_label, unit_name))
  }

  if (grepl("^(.*)_any_use_flag$", column_name)) {
    base_name <- sub("_any_use_flag$", "", column_name)
    base_label <- if (base_name %in% names(intra_item_cn)) intra_item_cn[[base_name]] else humanize_name(base_name)
    return(sprintf("术中%s是否有使用记录（0/1）", base_label))
  }

  if (grepl("^source_", column_name)) {
    var_name <- sub("^source_", "", column_name)
    return(sprintf("术前%s数据来源（Ward 优先，OR 兜底）", humanize_name(var_name)))
  }

  lab_match <- regexec("^preop_(.*)_(nearest|median|mean)(?:_([^_].*))?$", column_name)
  lab_parts <- regmatches(column_name, lab_match)[[1]]
  if (length(lab_parts) >= 3L) {
    item_name <- lab_parts[2]
    stat_name <- lab_parts[3]
    unit_name <- if (length(lab_parts) >= 4L && !is.na(lab_parts[4]) && lab_parts[4] != "") lab_parts[4] else "raw_source_unit"
    item_label <- if (item_name %in% names(lab_item_cn)) lab_item_cn[[item_name]] else item_name
    stat_label <- c(nearest = "最近值", median = "中位数", mean = "均值")[[stat_name]]
    return(sprintf("术前%s窗口内%s的%s（单位 %s）", table_time_window, item_label, stat_label, unit_name))
  }

  if (column_name %in% names(intra_item_cn)) {
    prefix <- if (table_name == "vitals_intraop_full_complete") "术中时序表中的" else "术中汇总表中的"
    return(sprintf("%s%s", prefix, intra_item_cn[[column_name]]))
  }

  if (grepl("^(ward|or)_", column_name)) {
    return(sprintf("术前基线构建过程中的%s", humanize_name(column_name)))
  }

  if (column_name %in% c("smoking", "drinking", "hypertension", "diabetes", "diabetes_any", "cerebrovascular_disease",
                         "dementia", "hemiplegia_paraplegia", "myocardial_infarction", "angina",
                         "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", "copd",
                         "asthma", "ards", "renal_disease", "liver_disease", "peptic_ulcer_disease",
                         "connective_tissue_disease", "peripheral_vascular_disease", "anemia",
                         "malignancy", "metastatic_solid_tumor", "hiv", "aids", "aids_opportunistic_raw", "hiv_aids")) {
    return(sprintf("术前诊断标记：%s", humanize_name(column_name)))
  }

  if (column_name %in% c("Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics",
                         "Statins", "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics",
                         "Insulin", "Oral_hypoglycemics", "Systemic_corticosteroids", "Immunosuppressants",
                         "Inhaled_bronchodilators", "Inhaled_corticosteroids", "Opioid_chronic_use",
                         "Proton_pump_inhibitors", "Antidepressants", "Antipsychotics", "Antibiotics_systemic",
                         "Thyroid_medications", "NSAIDs", "Antiemetics", "Mucolytics_expectorants",
                         "Antihistamines", "H2_blockers", "Laxatives", "GI_prokinetics",
                         "Benzodiazepines_sedatives", "Gabapentinoids", "Antiepileptics",
                         "Osteoporosis_medications", "Vitamin_D_Calcium", "Smoking_cessation_drugs")) {
    return(sprintf("术前入院至入手术室时间窗内是否使用%s", humanize_name(column_name)))
  }

  sprintf("%s字段", humanize_name(column_name))
}

infer_unit <- function(column_name) {
  if (grepl("_(sum|mean)_([^_].*)$", column_name)) {
    parts <- regmatches(column_name, regexec("_(sum|mean)_([^_].*)$", column_name))[[1]]
    return(parts[3])
  }
  if (grepl("_any_use_flag$", column_name)) {
    return("0_or_1")
  }
  if (grepl("^source_", column_name)) {
    return("category_text")
  }
  if (column_name %in% names(unit_map)) {
    return(unname(unit_map[[column_name]]))
  }
  if (grepl("_min$", column_name) || grepl("_time_min_raw$", column_name) || grepl("^min_from_entry$", column_name)) {
    return("min")
  }
  if (grepl("_days$", column_name)) {
    return("days")
  }
  if (grepl("^preop_.*_(nearest|median|mean)$", column_name)) {
    return("raw_source_unit")
  }
  if (grepl("^preop_.*_(nearest|median|mean)_([^_].*)$", column_name)) {
    parts <- regmatches(column_name, regexec("^preop_.*_(nearest|median|mean)_([^_].*)$", column_name))[[1]]
    return(parts[3])
  }
  if (column_name %in% c("flag_los_error", "flag_op_time_error", "flag_death_before_admission", "flag_overlap_with_prev")) {
    return("boolean")
  }
  if (column_name %in% c("Male", "Emergency_op")) {
    return("0_or_1")
  }
  if (column_name %in% names(intra_item_cn)) {
    return("raw_source_unit")
  }
  ""
}

na_meaning_for_column <- function(column_name, table_name) {
  if (column_name %in% c("op_id", "subject_id", "hadm_id", "case_id", "surgery_number")) {
    return("Missing only if source identifier is absent")
  }
  if (grepl("^flag_", column_name)) {
    return("NA means logic check not evaluable from source timestamps")
  }
  if (table_name %in% c(
    "diagnosis_preop_flags",
    "diagnosis_preop_flags_current_stay",
    "diagnosis_preop_flags_cumulative",
    "meds_preop_final",
    "outcomes_postop",
    "outcomes_postop_incident"
  )) {
    return("NA has been standardized to 0 in this table")
  }
  if (table_name == "master_dataset_final" &&
      (column_name %in% names(intra_item_cn) ||
       grepl("^preop_", column_name) == FALSE && column_name %in% c("smoking", "drinking", "hypertension", "diabetes", "diabetes_any",
       "cerebrovascular_disease", "dementia", "hemiplegia_paraplegia", "myocardial_infarction", "angina",
       "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", "copd", "asthma", "ards",
       "renal_disease", "liver_disease", "peptic_ulcer_disease", "connective_tissue_disease",
       "peripheral_vascular_disease", "anemia", "malignancy", "metastatic_solid_tumor", "hiv", "aids", "aids_opportunistic_raw", "hiv_aids",
       "Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics", "Statins",
       "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics", "Insulin", "Oral_hypoglycemics",
       "Systemic_corticosteroids", "Immunosuppressants", "Inhaled_bronchodilators", "Inhaled_corticosteroids",
       "Opioid_chronic_use", "Proton_pump_inhibitors", "Antidepressants", "Antipsychotics",
       "Antibiotics_systemic", "Thyroid_medications", "NSAIDs", "Antiemetics", "Mucolytics_expectorants",
       "Antihistamines", "H2_blockers", "Laxatives", "GI_prokinetics", "Benzodiazepines_sedatives",
       "Gabapentinoids", "Antiepileptics", "Osteoporosis_medications", "Vitamin_D_Calcium",
       "Smoking_cessation_drugs", "Stroke", "Cognitive_Decline", "Cardiac_Arrest", "Heart_Failure",
       "Myocardial_Injury", "Angina", "Arrhythmia_Vent", "Atrial_Fib", "Resp_Failure", "Pneumonia",
       "Sepsis", "Infection_Organ", "Infection_Unk", "AKI_Any", "AKI_Stage_1", "AKI_Stage_2",
       "AKI_Stage_3", "Death_In_Hospital", "Death_POD30", "Death_POD90", "Death_1_Year", "Death_Long_Term"))) {
    return("NA from missing module rows has been standardized to 0 in master")
  }
  "NA means no valid source value within the defined extraction window"
}

zero_fill_for_column <- function(column_name, table_name) {
  if (table_name %in% c(
    "diagnosis_preop_flags",
    "diagnosis_preop_flags_current_stay",
    "diagnosis_preop_flags_cumulative",
    "meds_preop_final",
    "outcomes_postop",
    "outcomes_postop_incident"
  )) {
    return("yes")
  }
  if (table_name == "master_dataset_final" &&
      (column_name %in% names(intra_item_cn) ||
       column_name %in% c("smoking", "drinking", "hypertension", "diabetes", "diabetes_any", "cerebrovascular_disease",
                          "dementia", "hemiplegia_paraplegia", "myocardial_infarction", "angina",
                          "atrial_fibrillation", "coronary_artery_disease", "arrhythmia_any", "copd",
                          "asthma", "ards", "renal_disease", "liver_disease", "peptic_ulcer_disease",
                          "connective_tissue_disease", "peripheral_vascular_disease", "anemia", "malignancy",
                          "metastatic_solid_tumor", "hiv", "aids", "aids_opportunistic_raw", "hiv_aids", "Beta_blockers", "Calcium_channel_blockers",
                          "ACE_inhibitors", "ARBs", "Diuretics", "Statins", "Antiplatelet_agents",
                          "Anticoagulants", "Nitrates", "Antiarrhythmics", "Insulin", "Oral_hypoglycemics",
                          "Systemic_corticosteroids", "Immunosuppressants", "Inhaled_bronchodilators",
                          "Inhaled_corticosteroids", "Opioid_chronic_use", "Proton_pump_inhibitors",
                          "Antidepressants", "Antipsychotics", "Antibiotics_systemic", "Thyroid_medications",
                          "NSAIDs", "Antiemetics", "Mucolytics_expectorants", "Antihistamines", "H2_blockers",
                          "Laxatives", "GI_prokinetics", "Benzodiazepines_sedatives", "Gabapentinoids",
                          "Antiepileptics", "Osteoporosis_medications", "Vitamin_D_Calcium",
                          "Smoking_cessation_drugs", "Stroke", "Cognitive_Decline", "Cardiac_Arrest",
                          "Heart_Failure", "Myocardial_Injury", "Angina", "Arrhythmia_Vent", "Atrial_Fib",
                          "Resp_Failure", "Pneumonia", "Sepsis", "Infection_Organ", "Infection_Unk",
                          "AKI_Any", "AKI_Stage_1", "AKI_Stage_2", "AKI_Stage_3", "Death_In_Hospital",
                          "Death_POD30", "Death_POD90", "Death_1_Year", "Death_Long_Term"))) {
    return("yes")
  }
  "no"
}

source_table_for_column <- function(column_name) {
  baseline_cols <- c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate", "Male", "Age", "Height", "Weight", "BMI",
    "race", "asa", "Emergency_op", "department", "antype", "icd10_pcs", "op_duration_min",
    "anesthesia_duration_min", "or_room_time_min", "cpb_duration_min", "hosp_los_min", "hosp_los_days",
    "icu_los_min", "icu_los_days", "time_to_inhosp_death_min", "time_to_inhosp_death_days",
    "time_to_allcause_death_min", "time_to_allcause_death_days", "admission_time_min_raw",
    "discharge_time_min_raw", "opstart_time_min_raw", "opend_time_min_raw", "anstart_time_min_raw",
    "anend_time_min_raw", "cpbon_time_min_raw", "cpboff_time_min_raw", "inhosp_death_time_min_raw",
    "allcause_death_time_min_raw", "flag_los_error", "flag_op_time_error", "flag_death_before_admission",
    "flag_overlap_with_prev"
  )
  diagnosis_cols <- c(
    "smoking", "drinking", "hypertension", "diabetes", "diabetes_any", "cerebrovascular_disease", "dementia",
    "hemiplegia_paraplegia", "myocardial_infarction", "angina", "atrial_fibrillation",
    "coronary_artery_disease", "arrhythmia_any", "copd", "asthma", "ards", "renal_disease",
    "liver_disease", "peptic_ulcer_disease", "connective_tissue_disease", "peripheral_vascular_disease",
    "anemia", "malignancy", "metastatic_solid_tumor", "hiv", "aids", "aids_opportunistic_raw", "hiv_aids"
  )
  meds_cols <- c(
    "Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics", "Statins",
    "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics", "Insulin",
    "Oral_hypoglycemics", "Systemic_corticosteroids", "Immunosuppressants", "Inhaled_bronchodilators",
    "Inhaled_corticosteroids", "Opioid_chronic_use", "Proton_pump_inhibitors", "Antidepressants",
    "Antipsychotics", "Antibiotics_systemic", "Thyroid_medications", "NSAIDs", "Antiemetics",
    "Mucolytics_expectorants", "Antihistamines", "H2_blockers", "Laxatives", "GI_prokinetics",
    "Benzodiazepines_sedatives", "Gabapentinoids", "Antiepileptics", "Osteoporosis_medications",
    "Vitamin_D_Calcium", "Smoking_cessation_drugs"
  )
  outcomes_cols <- c(
    "Stroke", "Cognitive_Decline", "Cardiac_Arrest", "Heart_Failure", "Myocardial_Injury", "Angina",
    "Arrhythmia_Vent", "Atrial_Fib", "Resp_Failure", "Pneumonia", "Sepsis", "Infection_Organ",
    "Infection_Unk", "AKI_Any", "AKI_Stage_1", "AKI_Stage_2", "AKI_Stage_3", "Death_In_Hospital",
    "Death_POD30", "Death_POD90", "Death_1_Year", "Death_Long_Term", "Survival_Days"
  )
  if (column_name %in% baseline_cols) {
    return(canonical_files$baseline_full)
  }
  if (column_name %in% diagnosis_cols) {
    return(canonical_files$diagnosis)
  }
  if (grepl("^preop_.*_(nearest|median|mean)(_.+)?$", column_name)) {
    return(canonical_files$labs_30d)
  }
  if (column_name %in% meds_cols) {
    return(canonical_files$meds)
  }
  if (column_name %in% c("preop_sbp", "preop_dbp", "preop_mbp", "preop_hr", "preop_spo2", "preop_rr", "preop_bt") ||
      grepl("^source_", column_name)) {
    return(canonical_files$vitals_preop)
  }
  if (column_name %in% names(intra_item_cn) || is_intraop_derived_column(column_name) || column_name == "surgery_number") {
    return(canonical_files$intraop_totals)
  }
  if (column_name %in% outcomes_cols) {
    return(canonical_files$outcomes)
  }
  ""
}

infer_phase <- function(column_name, table_name) {
  diagnosis_cols <- c(
    "smoking", "drinking", "hypertension", "diabetes", "diabetes_any", "cerebrovascular_disease", "dementia",
    "hemiplegia_paraplegia", "myocardial_infarction", "angina", "atrial_fibrillation",
    "coronary_artery_disease", "arrhythmia_any", "copd", "asthma", "ards", "renal_disease",
    "liver_disease", "peptic_ulcer_disease", "connective_tissue_disease", "peripheral_vascular_disease",
    "anemia", "malignancy", "metastatic_solid_tumor", "hiv", "aids", "aids_opportunistic_raw", "hiv_aids"
  )
  meds_cols <- c(
    "Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics", "Statins",
    "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics", "Insulin",
    "Oral_hypoglycemics", "Systemic_corticosteroids", "Immunosuppressants", "Inhaled_bronchodilators",
    "Inhaled_corticosteroids", "Opioid_chronic_use", "Proton_pump_inhibitors", "Antidepressants",
    "Antipsychotics", "Antibiotics_systemic", "Thyroid_medications", "NSAIDs", "Antiemetics",
    "Mucolytics_expectorants", "Antihistamines", "H2_blockers", "Laxatives", "GI_prokinetics",
    "Benzodiazepines_sedatives", "Gabapentinoids", "Antiepileptics", "Osteoporosis_medications",
    "Vitamin_D_Calcium", "Smoking_cessation_drugs"
  )
  outcome_cols <- c(
    "Stroke", "Cognitive_Decline", "Cardiac_Arrest", "Heart_Failure", "Myocardial_Injury", "Angina",
    "Arrhythmia_Vent", "Atrial_Fib", "Resp_Failure", "Pneumonia", "Sepsis", "Infection_Organ",
    "Infection_Unk", "AKI_Any", "AKI_Stage_1", "AKI_Stage_2", "AKI_Stage_3", "Death_In_Hospital",
    "Death_POD30", "Death_POD90", "Death_1_Year", "Death_Long_Term", "Survival_Days"
  )
  baseline_preop_cols <- c(
    "Male", "Age", "Height", "Weight", "BMI", "race", "asa", "Emergency_op", "department",
    "admission_time_min_raw", "opdate", "icd10_pcs", "antype"
  )
  intraop_timeline_cols <- c(
    "op_duration_min", "anesthesia_duration_min", "or_room_time_min", "cpb_duration_min",
    "opstart_time_min_raw", "opend_time_min_raw", "anstart_time_min_raw", "anend_time_min_raw",
    "cpbon_time_min_raw", "cpboff_time_min_raw", "chart_time", "min_from_entry"
  )
  postop_cols <- c(
    "discharge_time_min_raw", "inhosp_death_time_min_raw", "allcause_death_time_min_raw",
    "hosp_los_min", "hosp_los_days", "icu_los_min", "icu_los_days",
    "time_to_inhosp_death_min", "time_to_inhosp_death_days",
    "time_to_allcause_death_min", "time_to_allcause_death_days"
  )

  if (column_name %in% c("op_id", "subject_id", "hadm_id", "case_id", "surgery_number")) {
    return("identifier")
  }
  if (grepl("^flag_", column_name)) {
    return("qc")
  }
  if (column_name %in% outcome_cols || table_name %in% c("outcomes_postop", "outcomes_postop_incident")) {
    return("postop")
  }
  if (column_name %in% names(intra_item_cn) || is_intraop_derived_column(column_name) ||
      table_name %in% c("vitals_intraop_full_complete", "intraop_drugs_fluids_total_sum") ||
      column_name %in% intraop_timeline_cols) {
    return("intraop")
  }
  if (grepl("^preop_", column_name) || column_name %in% diagnosis_cols || column_name %in% meds_cols ||
      table_name %in% c("diagnosis_preop_flags", "diagnosis_preop_flags_current_stay",
                        "diagnosis_preop_flags_cumulative", "labs_preop_window_7d",
                        "labs_preop_window_30d", "labs_preop_window_any",
                        "labs_preop_window_current_stay", "labs_preop_window_cumulative",
                        "meds_preop_final", "vitals_preop_baseline",
                        "demographics_subject_level") ||
      column_name %in% baseline_preop_cols) {
    return("preop")
  }
  if (column_name %in% postop_cols) {
    return("postop")
  }
  "periop"
}

infer_time_anchor <- function(column_name, table_name) {
  if (grepl("^flag_", column_name)) {
    return("multi_anchor_qc")
  }
  if (table_name %in% c("diagnosis_preop_flags", "diagnosis_preop_flags_current_stay",
                        "diagnosis_preop_flags_cumulative", "labs_preop_window_7d",
                        "labs_preop_window_30d", "labs_preop_window_any",
                        "labs_preop_window_current_stay", "labs_preop_window_cumulative",
                        "vitals_preop_baseline") ||
      grepl("^preop_", column_name) || grepl("^source_", column_name)) {
    return("orin")
  }
  if (table_name == "meds_preop_final" ||
      column_name %in% c("Beta_blockers", "Calcium_channel_blockers", "ACE_inhibitors", "ARBs", "Diuretics",
                         "Statins", "Antiplatelet_agents", "Anticoagulants", "Nitrates", "Antiarrhythmics",
                         "Insulin", "Oral_hypoglycemics", "Systemic_corticosteroids", "Immunosuppressants",
                         "Inhaled_bronchodilators", "Inhaled_corticosteroids", "Opioid_chronic_use",
                         "Proton_pump_inhibitors", "Antidepressants", "Antipsychotics", "Antibiotics_systemic",
                         "Thyroid_medications", "NSAIDs", "Antiemetics", "Mucolytics_expectorants",
                         "Antihistamines", "H2_blockers", "Laxatives", "GI_prokinetics",
                         "Benzodiazepines_sedatives", "Gabapentinoids", "Antiepileptics",
                         "Osteoporosis_medications", "Vitamin_D_Calcium", "Smoking_cessation_drugs")) {
    return("admission_to_orin")
  }
  if (table_name %in% c("vitals_intraop_full_complete", "intraop_drugs_fluids_total_sum") ||
      column_name %in% names(intra_item_cn) || is_intraop_derived_column(column_name) ||
      column_name %in% c("chart_time", "min_from_entry", "surgery_number")) {
    return("orin_to_orout")
  }
  if (table_name %in% c("outcomes_postop", "outcomes_postop_incident") ||
      column_name %in% c("Stroke", "Cognitive_Decline", "Cardiac_Arrest", "Heart_Failure", "Myocardial_Injury",
                         "Angina", "Arrhythmia_Vent", "Atrial_Fib", "Resp_Failure", "Pneumonia", "Sepsis",
                         "Infection_Organ", "Infection_Unk")) {
    return("orin_to_discharge")
  }
  if (column_name %in% c("AKI_Any", "AKI_Stage_1", "AKI_Stage_2", "AKI_Stage_3")) {
    return("admission_preop_and_orout_to_discharge")
  }
  if (column_name %in% c("Death_In_Hospital", "Death_POD30", "Death_POD90", "Death_1_Year",
                         "Death_Long_Term", "Survival_Days", "inhosp_death_time_min_raw",
                         "allcause_death_time_min_raw", "time_to_inhosp_death_min", "time_to_inhosp_death_days",
                         "time_to_allcause_death_min", "time_to_allcause_death_days")) {
    return("admission_anend_discharge")
  }
  if (column_name %in% c("discharge_time_min_raw", "hosp_los_min", "hosp_los_days", "icu_los_min", "icu_los_days")) {
    return("admission_to_discharge")
  }
  if (column_name %in% c("op_duration_min", "anesthesia_duration_min", "or_room_time_min", "cpb_duration_min",
                         "opstart_time_min_raw", "opend_time_min_raw", "anstart_time_min_raw", "anend_time_min_raw",
                         "cpbon_time_min_raw", "cpboff_time_min_raw")) {
    return("orin_or_orout")
  }
  if (table_name %in% c("baseline_operations_full", "baseline_operations_preop_only",
                        "baseline_operations_intraop_only", "demographics_subject_level") ||
      column_name %in% c("Male", "Age", "Height", "Weight", "BMI", "race", "asa", "Emergency_op",
                         "department", "antype", "icd10_pcs", "admission_time_min_raw", "opdate")) {
    return("opdate_or_admission")
  }
  "periop_mixed_anchor"
}

infer_join_risk <- function(column_name, table_name, source_table_name) {
  if (table_name == "master_dataset_final") {
    table_id <- source_table_name
  } else {
    table_id <- table_name
  }

  if (table_id %in% c(
    "diagnosis_preop_flags",
    "diagnosis_preop_flags_current_stay",
    "diagnosis_preop_flags_cumulative",
    "labs_preop_window_7d",
    "labs_preop_window_30d",
    "labs_preop_window_any",
    "labs_preop_window_current_stay",
    "labs_preop_window_cumulative",
    "meds_preop_final"
  )) {
    return("high")
  }
  if (table_id == "vitals_preop_baseline") {
    return("medium")
  }
  if (table_id %in% c("baseline_operations_full", "baseline_operations_preop_only",
                      "baseline_operations_intraop_only", "demographics_subject_level",
                      "vitals_intraop_full_complete", "intraop_drugs_fluids_total_sum",
                      "outcomes_postop")) {
    return("low")
  }
  "medium"
}

build_baseline_tables <- function() {
  demog <- fread(legacy_outputs$demographics_operation)
  timeline <- fread(legacy_outputs$timeline_operation)

  timeline_drop <- intersect(names(timeline), c("subject_id", "hadm_id", "case_id", "opdate"))
  timeline_keep <- timeline[, !..timeline_drop]
  baseline_full <- merge(demog, timeline_keep, by = "op_id", all.x = TRUE)
  setorder(baseline_full, subject_id, opdate, op_id)

  baseline_preop_cols <- c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate", "Male", "Age", "Height", "Weight", "BMI",
    "race", "asa", "Emergency_op", "department", "admission_time_min_raw"
  )
  baseline_intra_cols <- c(
    "op_id", "subject_id", "hadm_id", "case_id", "opdate", "icd10_pcs", "antype",
    "op_duration_min", "anesthesia_duration_min", "or_room_time_min", "cpb_duration_min",
    "hosp_los_min", "hosp_los_days", "icu_los_min", "icu_los_days",
    "time_to_inhosp_death_min", "time_to_inhosp_death_days",
    "time_to_allcause_death_min", "time_to_allcause_death_days",
    "opstart_time_min_raw", "opend_time_min_raw", "anstart_time_min_raw", "anend_time_min_raw",
    "cpbon_time_min_raw", "cpboff_time_min_raw", "inhosp_death_time_min_raw",
    "allcause_death_time_min_raw"
  )

  baseline_preop <- baseline_full[, ..baseline_preop_cols]
  baseline_intra <- baseline_full[, ..baseline_intra_cols]

  baseline_paths <- list(
    full = file.path(intermediate_data_dir, canonical_files$baseline_full),
    preop = file.path(intermediate_data_dir, canonical_files$baseline_preop),
    intra = file.path(intermediate_data_dir, canonical_files$baseline_timeline)
  )

  dir_create(dirname(baseline_paths$full))
  dir_create(dirname(baseline_paths$preop))
  dir_create(dirname(baseline_paths$intra))

  fwrite(baseline_full, baseline_paths$full)
  fwrite(baseline_preop, baseline_paths$preop)
  fwrite(baseline_intra, baseline_paths$intra)

  baseline_paths
}

copy_specs <- data.table(
  table_name = c(
    "demographics_subject_level",
    "diagnosis_preop_flags",
    "diagnosis_preop_flags_current_stay",
    "summary_diagnosis_preop",
    "labs_preop_window_7d",
    "labs_preop_window_30d",
    "labs_preop_window_any",
    "labs_preop_window_current_stay",
    "meds_preop_final",
    "summary_meds_preop",
    "vitals_preop_baseline",
    "summary_vitals_preop_coverage",
    "vitals_intraop_full_complete",
    "intraop_drugs_fluids_total_sum",
    "summary_intraop_drugs_fluids",
    "outcomes_postop",
    "summary_outcomes_postop"
  ),
  source_path = c(
    legacy_outputs$demographics_subject,
    legacy_outputs$diagnosis,
    legacy_outputs$diagnosis_current,
    legacy_outputs$diagnosis_summary,
    legacy_outputs$labs_7d,
    legacy_outputs$labs_30d,
    legacy_outputs$labs_cumulative,
    legacy_outputs$labs_current,
    legacy_outputs$meds,
    legacy_outputs$meds_summary,
    legacy_outputs$vitals_preop,
    legacy_outputs$vitals_preop_summary,
    legacy_outputs$vitals_intraop,
    legacy_outputs$intraop_total_sum,
    legacy_outputs$intraop_summary,
    legacy_outputs$outcomes,
    legacy_outputs$outcomes_summary
  ),
  file_name = c(
    canonical_files$demographics_subject,
    canonical_files$diagnosis,
    canonical_files$diagnosis_current,
    "summary_diagnosis_preop.csv",
    canonical_files$labs_7d,
    canonical_files$labs_30d,
    canonical_files$labs_all_history,
    canonical_files$labs_current_stay,
    canonical_files$meds,
    "summary_meds_preop.csv",
    canonical_files$vitals_preop,
    "summary_vitals_preop_coverage.csv",
    canonical_files$vitals_intraop_ts,
    canonical_files$intraop_totals,
    "summary_intraop_drugs_fluids.csv",
    canonical_files$outcomes,
    "summary_outcomes_postop.csv"
  )
)

for (i in seq_len(nrow(copy_specs))) {
  copy_csv(copy_specs$source_path[i], file.path(intermediate_data_dir, copy_specs$file_name[i]))
}

if (file.exists(legacy_output_optional$outcomes_incident)) {
  copy_csv(
    legacy_output_optional$outcomes_incident,
    file.path(intermediate_data_dir, canonical_files$outcomes_incident)
  )
}

summary_specs <- data.table(
  summary_name = c(
    "summary_diagnosis_preop",
    "summary_meds_preop",
    "summary_vitals_preop_coverage",
    "summary_intraop_drugs_fluids",
    "summary_outcomes_postop"
  ),
  summary_file = c(
    "summary_diagnosis_preop.csv",
    "summary_meds_preop.csv",
    "summary_vitals_preop_coverage.csv",
    "summary_intraop_drugs_fluids.csv",
    "summary_outcomes_postop.csv"
  ),
  workbook_sheet = c(
    "diag_summary",
    "meds_summary",
    "vitals_summary",
    "intraop_summary",
    "outcomes_summary"
  ),
  target_table = c(
    canonical_files$diagnosis,
    canonical_files$meds,
    canonical_files$vitals_preop,
    canonical_files$intraop_totals,
    canonical_files$outcomes
  ),
  summary_purpose = c(
    "Prevalence summary for preoperative diagnosis flags",
    "Prevalence summary for preoperative medication flags",
    "Coverage and distribution summary for preoperative vitals",
    "Usage and dose summary for intraoperative drugs and fluids",
    "Event rate summary for postoperative outcomes"
  )
)

baseline_paths <- build_baseline_tables()

intermediate_standardize_files <- unique(c(
  canonical_files$demographics_subject,
  canonical_files$baseline_full,
  canonical_files$baseline_preop,
  canonical_files$baseline_timeline,
  canonical_files$diagnosis,
  canonical_files$diagnosis_current,
  canonical_files$labs_7d,
  canonical_files$labs_30d,
  canonical_files$labs_current_stay,
  canonical_files$labs_all_history,
  canonical_files$meds,
  canonical_files$vitals_preop,
  canonical_files$vitals_intraop_ts,
  canonical_files$intraop_totals,
  canonical_files$outcomes,
  canonical_files$outcomes_incident
))

for (file_name in intermediate_standardize_files) {
  standardize_table_file(file.path(intermediate_data_dir, file_name))
}

for (file_name in c(
  canonical_files$labs_7d,
  canonical_files$labs_30d,
  canonical_files$labs_current_stay,
  canonical_files$labs_all_history
)) {
  apply_parameter_unit_suffixes(file.path(intermediate_data_dir, file_name), apply_labs = TRUE)
}

apply_parameter_unit_suffixes(
  file.path(intermediate_data_dir, canonical_files$intraop_totals),
  apply_intra = TRUE
)

read_intermediate <- function(file_name, ...) {
  fread(file.path(intermediate_data_dir, file_name), ...)
}

build_master_dataset <- function() {
  dt_base <- read_intermediate(canonical_files$baseline_full)
  dt_diag <- read_intermediate(canonical_files$diagnosis)
  dt_labs <- read_intermediate(canonical_files$labs_30d)
  dt_meds <- read_intermediate(canonical_files$meds)
  dt_vitals_pre <- read_intermediate(canonical_files$vitals_preop)
  dt_intra <- read_intermediate(canonical_files$intraop_totals)
  dt_outcomes <- read_intermediate(canonical_files$outcomes)

  drop_if_present <- function(dt, cols) {
    cols <- intersect(names(dt), cols)
    if (length(cols) == 0L) {
      return(dt)
    }
    dt[, !..cols]
  }

  dt_diag <- drop_if_present(dt_diag, c("total_icd_count", "other_icd_n"))

  clean_merge <- function(main, part) {
    cols_to_drop <- intersect(names(part), c("subject_id", "hadm_id", "case_id", "surgery_number", "opdate"))
    cols_to_drop <- setdiff(cols_to_drop, "op_id")
    part_clean <- part[, !..cols_to_drop]
    merge(main, part_clean, by = "op_id", all.x = TRUE)
  }

  master_dt <- dt_base |>
    clean_merge(dt_diag) |>
    clean_merge(dt_labs) |>
    clean_merge(dt_meds) |>
    clean_merge(dt_vitals_pre) |>
    clean_merge(dt_intra) |>
    clean_merge(dt_outcomes)

  diag_cols <- setdiff(names(dt_diag), "op_id")
  meds_cols <- setdiff(names(dt_meds), c("op_id", "subject_id"))
  intra_cols <- setdiff(names(dt_intra), c("op_id", "subject_id", "surgery_number"))
  outcome_fill_cols <- setdiff(names(dt_outcomes), c("op_id", "subject_id", "Survival_Days"))
  fill_zero_cols <- unique(c(diag_cols, meds_cols, intra_cols, outcome_fill_cols))
  fill_zero_cols <- intersect(fill_zero_cols, names(master_dt))

  for (col_name in fill_zero_cols) {
    if (anyNA(master_dt[[col_name]])) {
      set(master_dt, i = which(is.na(master_dt[[col_name]])), j = col_name, value = 0)
    }
  }

  master_path <- file.path(intermediate_data_dir, canonical_files$master)
  dir_create(dirname(master_path))
  fwrite(master_dt, master_path)
  master_path
}

master_path <- build_master_dataset()
standardize_table_file(master_path)

release_copy_files <- c(
  canonical_files$demographics_subject,
  canonical_files$baseline_full,
  canonical_files$baseline_preop,
  canonical_files$baseline_timeline,
  canonical_files$diagnosis,
  canonical_files$diagnosis_current,
  canonical_files$labs_7d,
  canonical_files$labs_30d,
  canonical_files$labs_current_stay,
  canonical_files$labs_all_history,
  canonical_files$meds,
  canonical_files$vitals_preop,
  canonical_files$vitals_intraop_ts,
  canonical_files$intraop_totals,
  canonical_files$outcomes,
  canonical_files$outcomes_incident,
  canonical_files$master
)

release_copy_files <- release_copy_files[file.exists(file.path(intermediate_data_dir, release_copy_files))]

for (file_name in release_copy_files) {
  copy_csv(file.path(intermediate_data_dir, file_name), file.path(release_dir, file_name))
  standardize_table_file(file.path(release_dir, file_name))
}

stale_release_summary_files <- file.path(release_dir, summary_specs$summary_file)
unlink(stale_release_summary_files[file.exists(stale_release_summary_files)])
stale_documents_summary_files <- file.path(documents_dir, summary_specs$summary_file)
unlink(stale_documents_summary_files[file.exists(stale_documents_summary_files)])
stale_release_metadata_files <- file.path(release_dir, c(
  "schema_contract.csv",
  "data_dictionary.csv",
  "qc_table_summary.csv",
  "qc_field_missingness.csv",
  "qc_time_logic.csv",
  "qc_regression_checks.csv",
  "release_manifest.csv"
))
unlink(stale_release_metadata_files[file.exists(stale_release_metadata_files)])

table_contracts <- data.table(
  table_name = c(
    "demographics_subject_level",
    "baseline_operations_full",
    "baseline_operations_preop_only",
    "baseline_operations_intraop_only",
    "diagnosis_preop_flags",
    "labs_preop_window_7d",
    "labs_preop_window_30d",
    "labs_preop_window_any",
    "meds_preop_final",
    "vitals_preop_baseline",
    "vitals_intraop_full_complete",
    "intraop_drugs_fluids_total_sum",
    "outcomes_postop",
    "master_dataset_final"
  ),
  file_name = c(
    canonical_files$demographics_subject,
    canonical_files$baseline_full,
    canonical_files$baseline_preop,
    canonical_files$baseline_timeline,
    canonical_files$diagnosis,
    canonical_files$labs_7d,
    canonical_files$labs_30d,
    canonical_files$labs_all_history,
    canonical_files$meds,
    canonical_files$vitals_preop,
    canonical_files$vitals_intraop_ts,
    canonical_files$intraop_totals,
    canonical_files$outcomes,
    canonical_files$master
  ),
  primary_key = c(
    "subject_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id",
    "op_id + chart_time + min_from_entry",
    "op_id",
    "op_id",
    "op_id"
  ),
  grain = c(
    "subject",
    "operation",
    "operation",
    "operation",
    "operation",
    "operation",
    "operation",
    "operation",
    "operation",
    "operation",
    "intraoperative_timepoint",
    "operation",
    "operation",
    "operation"
  ),
  row_definition = c(
    "One row per subject",
    "One row per operation with demographics and timeline fields",
    "One row per operation with preoperative baseline subset",
    "One row per operation with intraoperative and postoperative timeline subset",
    "One row per operation with preoperative diagnosis flags",
    "One row per operation with preoperative labs in the 7-day window",
    "One row per operation with preoperative labs in the 30-day window",
    "One row per operation with preoperative labs using any available preoperative history",
    "One row per operation with preoperative medication flags",
    "One row per operation with preoperative baseline vitals",
    "One row per intraoperative measurement timestamp",
    "One row per operation with intraoperative additive sums plus non-additive mean and any_use flags",
    "One row per operation with postoperative outcomes",
    "One row per operation after left-joining all canonical operation-level modules"
  ),
  time_window = c(
    "Across available operations for each subject",
    "Operation index stay and perioperative timeline",
    "Admission to OR entry for baseline preoperative fields",
    "Intraoperative and postoperative timeline fields",
    "Diagnosis history_cumulative_preop (chart_time < orin_time)",
    "Within 7 days before OR entry",
    "Within 30 days before OR entry",
    "history_cumulative_preop (chart_time < orin_time), retained as legacy alias file",
    "Primary output is preop_immediate_48h; additional windows available in module output folder",
    "Ward 24h before OR entry plus OR 120 min before OR entry fallback",
    "OR entry to OR exit",
    "OR entry to OR exit",
    "Postoperative in-hospital and defined follow-up windows",
    "Baseline plus diagnosis, labs 30d, meds, preop vitals, intraop sum, outcomes"
  ),
  duplicate_key_allowed = c("no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no"),
  phase = c(
    "preop",
    "periop",
    "preop",
    "periop",
    "preop",
    "preop",
    "preop",
    "preop",
    "preop",
    "preop",
    "intraop",
    "intraop",
    "postop",
    "periop"
  ),
  time_anchor = c(
    "opdate_or_admission",
    "periop_mixed_anchor",
    "admission_to_orin",
    "periop_mixed_anchor",
    "orin",
    "orin",
    "orin",
    "orin",
    "admission_to_orin",
    "orin",
    "orin_to_orout",
    "orin_to_orout",
    "periop_mixed_anchor",
    "periop_mixed_anchor"
  ),
  join_risk = c(
    "low",
    "low",
    "low",
    "low",
    "high",
    "high",
    "high",
    "high",
    "high",
    "medium",
    "low",
    "low",
    "low",
    "mixed_upstream"
  ),
  source_script = c(
    "Process_Demographics_and_Timeline_v1.R",
    "run_inspire_pipeline.R",
    "run_inspire_pipeline.R",
    "run_inspire_pipeline.R",
    "Diagnosis_v1_1_20_2026.R",
    "Lab_v1_1_20_2026.R",
    "Lab_v1_1_20_2026.R",
    "Lab_v1_1_20_2026.R",
    "Medicine_pro_v1_1_20_2026.R",
    "Vials_pro_v1_1_20_2026.R",
    "Vials_intra_v1_1_21_2026.R",
    "Vials_intra_v1_1_21_2026.R",
    "Outcome_1_20_2026.R",
    "run_inspire_pipeline.R"
  ),
  upstream_dependencies = c(
    "operations.csv",
    "Demographic_Operation_Level.csv; Time_Related_Data.csv",
    canonical_files$baseline_full,
    canonical_files$baseline_full,
    "operations.csv; diagnosis.csv",
    "operations.csv; labs.csv",
    "operations.csv; labs.csv",
    "operations.csv; labs.csv",
    "operations.csv; medications.csv",
    "operations.csv; ward_vitals.csv; vitals.csv",
    "operations.csv; vitals.csv",
    canonical_files$vitals_intraop_ts,
    "operations.csv; diagnosis.csv; labs.csv",
    paste(
      canonical_files$baseline_full,
      canonical_files$diagnosis,
      canonical_files$labs_30d,
      canonical_files$meds,
      canonical_files$vitals_preop,
      canonical_files$intraop_totals,
      canonical_files$outcomes,
      sep = "; "
    )
  )
)

table_contracts <- rbind(
  table_contracts,
  data.table(
    table_name = c(
      "diagnosis_preop_flags_current_stay",
      "labs_preop_window_current_stay"
    ),
    file_name = c(
      canonical_files$diagnosis_current,
      canonical_files$labs_current_stay
    ),
    primary_key = c("op_id", "op_id"),
    grain = c("operation", "operation"),
    row_definition = c(
      "One row per operation with diagnosis flags from current admission preoperative window",
      "One row per operation with preoperative labs from current admission before OR entry"
    ),
    time_window = c(
      "history_preop_current_stay (admission_time <= chart_time < orin_time)",
      "history_preop_current_stay (admission_time <= chart_time < orin_time)"
    ),
    duplicate_key_allowed = c("no", "no"),
    phase = c("preop", "preop"),
    time_anchor = c("orin", "orin"),
    join_risk = c("high", "high"),
    source_script = c(
      "Diagnosis_v1_1_20_2026.R",
      "Lab_v1_1_20_2026.R"
    ),
    upstream_dependencies = c(
      "operations.csv; diagnosis.csv",
      "operations.csv; labs.csv"
    )
  ),
  fill = TRUE
)

if (file.exists(file.path(release_dir, canonical_files$outcomes_incident))) {
  table_contracts <- rbind(
    table_contracts,
    data.table(
      table_name = "outcomes_postop_incident",
      file_name = canonical_files$outcomes_incident,
      primary_key = "op_id",
      grain = "operation",
      row_definition = "One row per operation with postoperative incident-only outcomes",
      time_window = "Postoperative in-hospital and defined follow-up windows with preop-existing ICD excluded for incident logic",
      duplicate_key_allowed = "no",
      phase = "postop",
      time_anchor = "periop_mixed_anchor",
      join_risk = "low",
      source_script = "Outcome_1_20_2026.R",
      upstream_dependencies = "operations.csv; diagnosis.csv; labs.csv"
    ),
    fill = TRUE
  )
}

build_table_qc_summary <- function(table_contracts_dt) {
  baseline_n <- nrow(fread(file.path(release_dir, canonical_files$baseline_full), select = "op_id"))
  results <- vector("list", nrow(table_contracts_dt))
  for (i in seq_len(nrow(table_contracts_dt))) {
    spec <- table_contracts_dt[i]
    file_path <- file.path(release_dir, spec$file_name)
    select_cols <- fread(file_path, nrows = 0L)
    dt <- fread(file_path, select = intersect(c("op_id", "subject_id", "chart_time", "min_from_entry"), names(select_cols)))
    row_count <- shell_line_count(file_path)
    key_cols <- trimws(unlist(strsplit(spec$primary_key, "\\+")))
    key_cols <- key_cols[key_cols %in% names(dt)]
    duplicate_key_n <- if (length(key_cols) > 0L) {
      dup_matrix <- duplicated(dt[, ..key_cols])
      sum(dup_matrix, na.rm = TRUE)
    } else {
      NA_integer_
    }
    missing_primary_key_n <- if (length(key_cols) > 0L) {
      sum(!complete.cases(dt[, ..key_cols]))
    } else {
      NA_integer_
    }
    unique_op_n <- if ("op_id" %in% names(dt)) uniqueN(dt$op_id) else NA_integer_
    coverage_pct <- if (spec$grain == "operation") round(unique_op_n / baseline_n * 100, 2) else NA_real_
    results[[i]] <- data.table(
      table_name = spec$table_name,
      file_name = spec$file_name,
      grain = spec$grain,
      row_count = row_count,
      column_count = ncol(select_cols),
      unique_op_id_n = unique_op_n,
      baseline_operation_n = if (spec$grain == "operation") baseline_n else NA_integer_,
      missing_vs_baseline_n = if (spec$grain == "operation") baseline_n - unique_op_n else NA_integer_,
      coverage_pct_vs_baseline = coverage_pct,
      duplicate_primary_key_n = duplicate_key_n,
      missing_primary_key_n = missing_primary_key_n,
      source_script = spec$source_script
    )
  }
  rbindlist(results, fill = TRUE)
}

qc_summary <- build_table_qc_summary(table_contracts)

critical_fields <- list(
  demographics_subject_level = c("subject_id", "Age", "BMI", "Male", "race"),
  baseline_operations_full = c("op_id", "subject_id", "Age", "BMI", "asa", "department", "antype"),
  diagnosis_preop_flags = c("smoking", "hypertension", "diabetes", "malignancy"),
  diagnosis_preop_flags_current_stay = c("smoking", "hypertension", "diabetes", "malignancy"),
  labs_preop_window_30d = c("preop_creatinine_nearest_mg_dl", "preop_hb_nearest_g_dl", "preop_wbc_nearest_per_nl"),
  labs_preop_window_current_stay = c("preop_creatinine_nearest_mg_dl", "preop_hb_nearest_g_dl", "preop_wbc_nearest_per_nl"),
  meds_preop_final = c("Beta_blockers", "Statins", "Anticoagulants"),
  vitals_preop_baseline = c("preop_sbp", "preop_hr", "preop_spo2"),
  intraop_drugs_fluids_total_sum = c("ppf_sum_mg", "ebl_sum_ml", "uo_sum_ml", "ppfi_any_use_flag"),
  outcomes_postop = c("Death_POD30", "AKI_Any", "Stroke"),
  outcomes_postop_incident = c("Death_POD30", "AKI_Any", "Stroke"),
  master_dataset_final = c("Age", "hypertension", "preop_creatinine_nearest_mg_dl", "preop_sbp", "ppf_sum_mg", "ebl_sum_ml", "AKI_Any", "Death_POD30")
)

build_field_missingness <- function(field_map) {
  results <- list()
  idx <- 1L
  for (table_id in names(field_map)) {
    file_name <- table_contracts[table_name == table_id, file_name]
    dt <- fread(file.path(release_dir, file_name))
    for (field_name in intersect(field_map[[table_id]], names(dt))) {
      x <- dt[[field_name]]
      zero_count <- if (is.numeric(x)) sum(x == 0, na.rm = TRUE) else NA_integer_
      results[[idx]] <- data.table(
        table_name = table_id,
        field_name = field_name,
        row_count = nrow(dt),
        missing_n = sum(is.na(x)),
        missing_pct = round(mean(is.na(x)) * 100, 2),
        zero_n = zero_count,
        zero_pct = if (is.numeric(x)) round(mean(x == 0, na.rm = TRUE) * 100, 2) else NA_real_,
        non_missing_n = sum(!is.na(x))
      )
      idx <- idx + 1L
    }
  }
  rbindlist(results, fill = TRUE)
}

qc_field_missingness <- build_field_missingness(critical_fields)

baseline_dt <- fread(file.path(release_dir, canonical_files$baseline_full))
qc_time_logic <- data.table(
  metric = c(
    "flag_los_error_n",
    "flag_op_time_error_n",
    "flag_death_before_admission_n",
    "flag_overlap_with_prev_n"
  ),
  count = c(
    sum(baseline_dt$flag_los_error, na.rm = TRUE),
    sum(baseline_dt$flag_op_time_error, na.rm = TRUE),
    sum(baseline_dt$flag_death_before_admission, na.rm = TRUE),
    sum(baseline_dt$flag_overlap_with_prev, na.rm = TRUE)
  ),
  total_operations = nrow(baseline_dt),
  pct = round(c(
    mean(baseline_dt$flag_los_error, na.rm = TRUE),
    mean(baseline_dt$flag_op_time_error, na.rm = TRUE),
    mean(baseline_dt$flag_death_before_admission, na.rm = TRUE),
    mean(baseline_dt$flag_overlap_with_prev, na.rm = TRUE)
  ) * 100, 4)
)
build_regression_checks <- function() {
  prototype_root <- file.path(processed_root, "Unified_Data_2026_02_19")
  prototype_master <- file.path(prototype_root, "Master_Dataset_Final.csv")
  prototype_baseline <- file.path(prototype_root, "Baseline_Operations_Full.csv")
  if (!file.exists(prototype_master) || !file.exists(prototype_baseline)) {
    return(data.table(
      artifact = "prototype",
      metric = "availability",
      new_value = NA_character_,
      reference_value = NA_character_,
      abs_diff = NA_real_,
      tolerance = NA_real_,
      status = "reference_missing"
    ))
  }

  new_master <- fread(file.path(release_dir, canonical_files$master))
  old_master <- fread(prototype_master)
  new_baseline <- fread(file.path(release_dir, canonical_files$baseline_full))
  old_baseline <- fread(prototype_baseline)

  results <- list()
  push_result <- function(artifact, metric, new_value, reference_value, abs_diff, tolerance, status) {
    results[[length(results) + 1L]] <<- data.table(
      artifact = artifact,
      metric = metric,
      new_value = as.character(new_value),
      reference_value = as.character(reference_value),
      abs_diff = abs_diff,
      tolerance = tolerance,
      status = status
    )
  }

  push_result(
    "baseline_operations_full",
    "row_count",
    nrow(new_baseline),
    nrow(old_baseline),
    abs(nrow(new_baseline) - nrow(old_baseline)),
    0,
    ifelse(nrow(new_baseline) == nrow(old_baseline), "pass", "fail")
  )
  push_result(
    "master_dataset_final",
    "row_count",
    nrow(new_master),
    nrow(old_master),
    abs(nrow(new_master) - nrow(old_master)),
    0,
    ifelse(nrow(new_master) == nrow(old_master), "pass", "fail")
  )

  compare_fields <- list(
    hypertension = list(type = "prevalence_pct", tolerance = 0.01),
    diabetes = list(type = "prevalence_pct", tolerance = 0.01),
    preop_creatinine_nearest_mg_dl = list(type = "coverage_pct", tolerance = 0.01),
    preop_sbp = list(type = "coverage_pct", tolerance = 0.01),
    ppf_sum_mg = list(type = "mean", tolerance = 0.01),
    ebl_sum_ml = list(type = "mean", tolerance = 0.01),
    AKI_Any = list(type = "prevalence_pct", tolerance = 0.01),
    Death_POD30 = list(type = "prevalence_pct", tolerance = 0.01)
  )

  for (field_name in names(compare_fields)) {
    if (!field_name %in% names(new_master) || !field_name %in% names(old_master)) {
      next
    }
    spec <- compare_fields[[field_name]]
    new_x <- new_master[[field_name]]
    old_x <- old_master[[field_name]]
    if (spec$type == "coverage_pct") {
      new_value <- mean(!is.na(new_x)) * 100
      old_value <- mean(!is.na(old_x)) * 100
    } else if (spec$type == "prevalence_pct") {
      new_value <- mean(new_x, na.rm = TRUE) * 100
      old_value <- mean(old_x, na.rm = TRUE) * 100
    } else {
      new_value <- mean(new_x, na.rm = TRUE)
      old_value <- mean(old_x, na.rm = TRUE)
    }
    diff_value <- abs(new_value - old_value)
    push_result(
      "master_dataset_final",
      sprintf("%s_%s", field_name, spec$type),
      round(new_value, 6),
      round(old_value, 6),
      round(diff_value, 6),
      spec$tolerance,
      ifelse(diff_value <= spec$tolerance, "pass", "fail")
    )
  }

  rbindlist(results, fill = TRUE)
}

qc_regression <- build_regression_checks()

build_dictionary_for_table <- function(spec_row) {
  file_path <- file.path(release_dir, spec_row$file_name)
  dt <- fread(file_path, nrows = 1000L)
  row_definition <- spec_row$row_definition
  time_window <- spec_row$time_window
  table_name <- spec_row$table_name
  column_names <- names(dt)
  rows <- vector("list", length(column_names))
  for (i in seq_along(column_names)) {
    column_name <- column_names[i]
    x <- dt[[column_name]]
    source_table_name <- if (table_name == "master_dataset_final") source_table_for_column(column_name) else spec_row$file_name
    rows[[i]] <- data.table(
      table_name = table_name,
      file_name = spec_row$file_name,
      column_name = column_name,
      description_cn = describe_column(column_name, table_name, time_window),
      source_table = source_table_name,
      source_script = spec_row$source_script,
      row_definition = row_definition,
      time_window = time_window,
      phase = infer_phase(column_name, table_name),
      time_anchor = infer_time_anchor(column_name, table_name),
      join_risk = infer_join_risk(column_name, table_name, source_table_name),
      key_role = if (column_name %in% trimws(unlist(strsplit(spec_row$primary_key, "\\+")))) "key" else "data",
      value_type = detect_value_type(x, column_name),
      unit = infer_unit(column_name),
      na_meaning = na_meaning_for_column(column_name, table_name),
      zero_filled = zero_fill_for_column(column_name, table_name)
    )
  }
  rbindlist(rows, fill = TRUE)
}

dictionary_specs <- table_contracts[table_name %in% c(
  "demographics_subject_level",
  "baseline_operations_full",
  "diagnosis_preop_flags",
  "diagnosis_preop_flags_current_stay",
  "labs_preop_window_7d",
  "labs_preop_window_30d",
  "labs_preop_window_any",
  "labs_preop_window_current_stay",
  "meds_preop_final",
  "vitals_preop_baseline",
  "vitals_intraop_full_complete",
  "intraop_drugs_fluids_total_sum",
  "outcomes_postop",
  "master_dataset_final"
)]

data_dictionary <- rbindlist(
  lapply(seq_len(nrow(dictionary_specs)), function(i) build_dictionary_for_table(dictionary_specs[i])),
  fill = TRUE
)

process_file_index <- merge(
  table_contracts[, .(
    table_name,
    data_file = file_name,
    source_script,
    row_definition,
    time_window,
    primary_key,
    phase,
    time_anchor,
    join_risk
  )],
  summary_specs[, .(
    target_table,
    summary_purpose,
    summary_sheet = workbook_sheet
  )],
  by.x = "data_file",
  by.y = "target_table",
  all.x = TRUE
)
process_file_index[, data_file_relative_path := file.path("data", "INSPIRE_1.3", "processed", data_file)]
process_file_index[, summary_workbook_relative_path := fifelse(
  is.na(summary_sheet),
  "",
  file.path("documents", "INSPIRE_1.3", "processing_docs", release_date, "process_summary_workbook.xlsx")
)]

read_release_table <- function(file_name) {
  file_path <- file.path(release_dir, file_name)
  if (!file.exists(file_path)) {
    return(NULL)
  }
  fread(file_path)
}

count_row_differences <- function(dt_a, dt_b, id_col = "op_id") {
  if (is.null(dt_a) || is.null(dt_b) || !(id_col %in% names(dt_a)) || !(id_col %in% names(dt_b))) {
    return(NA_integer_)
  }
  common_cols <- setdiff(intersect(names(dt_a), names(dt_b)), c("subject_id", "hadm_id", "op_id"))
  if (length(common_cols) == 0L) {
    return(0L)
  }
  a <- copy(dt_a[, c(id_col, common_cols), with = FALSE])
  b <- copy(dt_b[, c(id_col, common_cols), with = FALSE])
  setkeyv(a, id_col)
  setkeyv(b, id_col)
  b_aligned <- b[a, on = id_col]
  diff_row <- rep(FALSE, nrow(a))
  for (col_name in common_cols) {
    x <- a[[col_name]]
    y <- b_aligned[[col_name]]
    neq <- (x != y) & !(is.na(x) & is.na(y))
    neq[is.na(neq)] <- FALSE
    diff_row <- diff_row | neq
  }
  sum(diff_row)
}

build_binary_compare <- function(file_a, file_b, label_a, label_b) {
  dt_a <- read_release_table(file_a)
  dt_b <- read_release_table(file_b)
  if (is.null(dt_a) || is.null(dt_b) || !("op_id" %in% names(dt_a)) || !("op_id" %in% names(dt_b))) {
    return(data.table())
  }
  common_cols <- setdiff(intersect(names(dt_a), names(dt_b)), c("subject_id", "hadm_id", "op_id"))
  if (length(common_cols) == 0L) {
    return(data.table())
  }
  a <- copy(dt_a[, c("op_id", common_cols), with = FALSE])
  b <- copy(dt_b[, c("op_id", common_cols), with = FALSE])
  setkey(a, op_id)
  setkey(b, op_id)
  b_aligned <- b[a, on = "op_id"]

  rows <- lapply(common_cols, function(col_name) {
    x <- a[[col_name]]
    y <- b_aligned[[col_name]]
    diff_flag <- (x != y) & !(is.na(x) & is.na(y))
    diff_flag[is.na(diff_flag)] <- FALSE
    data.table(
      variable = col_name,
      prevalence_pct_a = round(mean(as.numeric(x), na.rm = TRUE) * 100, 3),
      prevalence_pct_b = round(mean(as.numeric(y), na.rm = TRUE) * 100, 3),
      delta_pct = round(mean(as.numeric(y), na.rm = TRUE) * 100 - mean(as.numeric(x), na.rm = TRUE) * 100, 3),
      n_diff_rows = sum(diff_flag)
    )
  })
  out <- rbindlist(rows, fill = TRUE)
  setnames(out, c("prevalence_pct_a", "prevalence_pct_b"), c(
    paste0("prevalence_pct_", label_a),
    paste0("prevalence_pct_", label_b)
  ))
  out[order(-n_diff_rows, variable)]
}

build_lab_coverage_compare <- function(file_a, file_b, label_a, label_b) {
  dt_a <- read_release_table(file_a)
  dt_b <- read_release_table(file_b)
  if (is.null(dt_a) || is.null(dt_b) || !("op_id" %in% names(dt_a)) || !("op_id" %in% names(dt_b))) {
    return(data.table())
  }
  candidate_cols <- intersect(names(dt_a), names(dt_b))
  lab_cols <- setdiff(grep("^preop_.*_nearest$", candidate_cols, value = TRUE), c("op_id"))
  if (length(lab_cols) == 0L) {
    return(data.table())
  }
  a <- copy(dt_a[, c("op_id", lab_cols), with = FALSE])
  b <- copy(dt_b[, c("op_id", lab_cols), with = FALSE])
  setkey(a, op_id)
  setkey(b, op_id)
  b_aligned <- b[a, on = "op_id"]

  rows <- lapply(lab_cols, function(col_name) {
    x <- a[[col_name]]
    y <- b_aligned[[col_name]]
    non_missing_diff <- xor(!is.na(x), !is.na(y))
    data.table(
      variable = col_name,
      coverage_pct_a = round(mean(!is.na(x)) * 100, 3),
      coverage_pct_b = round(mean(!is.na(y)) * 100, 3),
      delta_pct = round(mean(!is.na(y)) * 100 - mean(!is.na(x)) * 100, 3),
      n_non_missing_status_diff = sum(non_missing_diff)
    )
  })
  out <- rbindlist(rows, fill = TRUE)
  setnames(out, c("coverage_pct_a", "coverage_pct_b"), c(
    paste0("coverage_pct_", label_a),
    paste0("coverage_pct_", label_b)
  ))
  out[order(-abs(delta_pct), variable)]
}

diag_final_dt <- read_release_table(canonical_files$diagnosis)
diag_cur_dt <- read_release_table(canonical_files$diagnosis_current)
labs_all_dt <- read_release_table(canonical_files$labs_all_history)
labs_cur_dt <- read_release_table(canonical_files$labs_current_stay)
out_raw_dt <- read_release_table(canonical_files$outcomes)
out_inc_dt <- read_release_table(canonical_files$outcomes_incident)

variant_overview <- data.table(
  domain = c("diagnosis", "labs", "outcomes"),
  file_a = c(
    canonical_files$diagnosis,
    canonical_files$labs_all_history,
    canonical_files$outcomes
  ),
  file_b = c(
    canonical_files$diagnosis_current,
    canonical_files$labs_current_stay,
    canonical_files$outcomes_incident
  ),
  relation = c(
    "window_compare",
    "window_compare",
    "definition_compare"
  ),
  is_identical = c(
    !is.null(diag_final_dt) && !is.null(diag_cur_dt) && identical(diag_final_dt, diag_cur_dt),
    !is.null(labs_all_dt) && !is.null(labs_cur_dt) && identical(labs_all_dt, labs_cur_dt),
    !is.null(out_raw_dt) && !is.null(out_inc_dt) && identical(out_raw_dt, out_inc_dt)
  ),
  n_rows_with_any_difference = c(
    count_row_differences(diag_final_dt, diag_cur_dt),
    count_row_differences(labs_all_dt, labs_cur_dt),
    count_row_differences(out_raw_dt, out_inc_dt)
  ),
  recommendation = c(
    "保留双版本用于敏感性分析（main=history_cumulative_preop, sensitivity=current_stay）",
    "保留双版本用于敏感性分析",
    "保留双版本（raw 与 incident 口径不同）"
  )
)

diag_window_compare <- build_binary_compare(
  canonical_files$diagnosis,
  canonical_files$diagnosis_current,
  "main",
  "current_stay"
)
labs_window_compare <- build_lab_coverage_compare(
  canonical_files$labs_all_history,
  canonical_files$labs_current_stay,
  "main",
  "current_stay"
)
outcome_incident_compare <- build_binary_compare(
  canonical_files$outcomes,
  canonical_files$outcomes_incident,
  "raw",
  "incident"
)


write_documents_workbook <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("openxlsx is not installed; skipping Excel workbook generation.")
    return(invisible(NULL))
  }

  wb <- openxlsx::createWorkbook()

  add_sheet_from_dt <- function(sheet_name, dt) {
    if (is.null(dt) || nrow(dt) == 0L) {
      return(invisible(NULL))
    }
    openxlsx::addWorksheet(wb, sheetName = sheet_name)
    openxlsx::writeDataTable(wb, sheet = sheet_name, x = as.data.frame(dt), withFilter = TRUE)
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dt), widths = "auto")
  }

  add_sheet_from_dt("process_file_index", process_file_index)
  add_sheet_from_dt("data_dictionary", data_dictionary)
  add_sheet_from_dt("diag_summary", fread(file.path(intermediate_data_dir, "summary_diagnosis_preop.csv")))
  add_sheet_from_dt("meds_summary", fread(file.path(intermediate_data_dir, "summary_meds_preop.csv")))
  add_sheet_from_dt("vitals_summary", fread(file.path(intermediate_data_dir, "summary_vitals_preop_coverage.csv")))
  add_sheet_from_dt("intraop_summary", fread(file.path(intermediate_data_dir, "summary_intraop_drugs_fluids.csv")))
  add_sheet_from_dt("outcomes_summary", fread(file.path(intermediate_data_dir, "summary_outcomes_postop.csv")))
  add_sheet_from_dt("variant_overview", variant_overview)
  add_sheet_from_dt("diag_main_vs_current", diag_window_compare)
  add_sheet_from_dt("labs_main_vs_current", labs_window_compare)
  add_sheet_from_dt("out_raw_vs_inc", outcome_incident_compare)

  workbook_path <- file.path(documents_dir, "process_summary_workbook.xlsx")
  openxlsx::saveWorkbook(wb, file = workbook_path, overwrite = TRUE)
  workbook_path
}

write_diagnosis_workbook <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("openxlsx is not installed; skipping diagnosis workbook generation.")
    return(invisible(NULL))
  }

  dt_main <- read_release_table(canonical_files$diagnosis)
  dt_current <- read_release_table(canonical_files$diagnosis_current)
  if (is.null(dt_main) || is.null(dt_current)) {
    warning("Diagnosis files are missing in release_dir; skipping diagnosis workbook.")
    return(invisible(NULL))
  }

  # main file currently represents cumulative preop history
  dt_cumulative <- copy(dt_main)

  strict_path <- resolve_legacy_output_path("Diagnosis_1_20_2026", "diag_preop_flags_strict.csv")
  dt_strict <- if (file.exists(strict_path)) fread(strict_path) else NULL
  if (!is.null(dt_strict) && "op_id" %in% names(dt_strict)) {
    standardize_table_file(strict_path)
    dt_strict <- fread(strict_path)
  }

  wb <- openxlsx::createWorkbook()
  add_sheet_from_dt <- function(sheet_name, dt) {
    if (is.null(dt) || nrow(dt) == 0L) {
      return(invisible(NULL))
    }
    openxlsx::addWorksheet(wb, sheetName = sheet_name)
    openxlsx::writeDataTable(wb, sheet = sheet_name, x = as.data.frame(dt), withFilter = TRUE)
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dt), widths = "auto")
  }

  info <- data.table(
    sheet = c("cumulative_main", "current_stay", "strict_history", "main_vs_current"),
    definition = c(
      "history_cumulative_preop: chart_time < orin_time; 当前主分析口径",
      "history_preop_current_stay: admission_time <= chart_time < orin_time",
      "history_strict: chart_time < admission_time（若可用）",
      "main(cumulative) 与 current_stay 的变量差异对照"
    ),
    source_file = c(
      canonical_files$diagnosis,
      canonical_files$diagnosis_current,
      if (is.null(dt_strict)) "not_available_in_release" else "diag_preop_flags_strict.csv (legacy archive)",
      "derived_comparison_table"
    )
  )

  add_sheet_from_dt("readme", info)
  add_sheet_from_dt("cumulative_main", dt_cumulative)
  add_sheet_from_dt("current_stay", dt_current)
  add_sheet_from_dt("strict_history", dt_strict)
  add_sheet_from_dt("main_vs_current", diag_window_compare)

  workbook_path <- file.path(documents_dir, "diagnosis_variants_workbook.xlsx")
  openxlsx::saveWorkbook(wb, file = workbook_path, overwrite = TRUE)
  workbook_path
}

write_labs_workbook <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("openxlsx is not installed; skipping labs workbook generation.")
    return(invisible(NULL))
  }

  dt_main <- read_release_table(canonical_files$labs_all_history)
  dt_current <- read_release_table(canonical_files$labs_current_stay)
  dt_30d <- read_release_table(canonical_files$labs_30d)
  dt_7d <- read_release_table(canonical_files$labs_7d)

  if (is.null(dt_main) || is.null(dt_current) || is.null(dt_30d) || is.null(dt_7d)) {
    warning("One or more labs files are missing in release_dir; skipping labs workbook.")
    return(invisible(NULL))
  }

  cmp_main_current <- build_lab_coverage_compare(
    canonical_files$labs_all_history,
    canonical_files$labs_current_stay,
    "main",
    "current_stay"
  )
  cmp_main_30d <- build_lab_coverage_compare(
    canonical_files$labs_all_history,
    canonical_files$labs_30d,
    "main",
    "window_30d"
  )
  cmp_main_7d <- build_lab_coverage_compare(
    canonical_files$labs_all_history,
    canonical_files$labs_7d,
    "main",
    "window_7d"
  )

  strict_path <- resolve_legacy_output_path("lab_data_v1_1_20_2026", "preop_labs_features_strict_history.csv")
  dt_strict <- if (file.exists(strict_path)) fread(strict_path) else NULL
  if (!is.null(dt_strict) && "op_id" %in% names(dt_strict)) {
    standardize_table_file(strict_path)
    dt_strict <- fread(strict_path)
  }

  wb <- openxlsx::createWorkbook()
  add_sheet_from_dt <- function(sheet_name, dt) {
    if (is.null(dt) || nrow(dt) == 0L) {
      return(invisible(NULL))
    }
    openxlsx::addWorksheet(wb, sheetName = sheet_name)
    openxlsx::writeDataTable(wb, sheet = sheet_name, x = as.data.frame(dt), withFilter = TRUE)
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(dt), widths = "auto")
  }

  info <- data.table(
    sheet = c(
      "main_all_history",
      "current_stay",
      "window_30d",
      "window_7d",
      "strict_history",
      "main_vs_current",
      "main_vs_30d",
      "main_vs_7d"
    ),
    definition = c(
      "main: history_cumulative_preop (chart_time < orin_time)",
      "history_preop_current_stay (admission_time <= chart_time < orin_time)",
      "术前 30 天窗口",
      "术前 7 天窗口",
      "history_strict (chart_time < admission_time; 若可用)",
      "main vs current_stay 覆盖率差异",
      "main vs 30d 覆盖率差异",
      "main vs 7d 覆盖率差异"
    ),
    source_file = c(
      canonical_files$labs_all_history,
      canonical_files$labs_current_stay,
      canonical_files$labs_30d,
      canonical_files$labs_7d,
      if (is.null(dt_strict)) "not_available_in_release" else "preop_labs_features_strict_history.csv (legacy archive)",
      "derived_comparison_table",
      "derived_comparison_table",
      "derived_comparison_table"
    )
  )

  add_sheet_from_dt("readme", info)
  add_sheet_from_dt("main_all_history", dt_main)
  add_sheet_from_dt("current_stay", dt_current)
  add_sheet_from_dt("window_30d", dt_30d)
  add_sheet_from_dt("window_7d", dt_7d)
  add_sheet_from_dt("strict_history", dt_strict)
  add_sheet_from_dt("main_vs_current", cmp_main_current)
  add_sheet_from_dt("main_vs_30d", cmp_main_30d)
  add_sheet_from_dt("main_vs_7d", cmp_main_7d)

  workbook_path <- file.path(documents_dir, "labs_variants_workbook.xlsx")
  openxlsx::saveWorkbook(wb, file = workbook_path, overwrite = TRUE)
  workbook_path
}

documents_workbook_path <- write_documents_workbook()
diagnosis_workbook_path <- write_diagnosis_workbook()
labs_workbook_path <- write_labs_workbook()

archive_legacy_generated_outputs()

message_line("Pipeline complete.")
message_line("Intermediate directory: %s", intermediate_data_dir)
message_line("Processed data directory: %s", release_dir)
message_line("Documents directory: %s", documents_dir)
if (!is.null(documents_workbook_path) && file.exists(documents_workbook_path)) {
  message_line("Documents workbook: %s", documents_workbook_path)
}
if (!is.null(diagnosis_workbook_path) && file.exists(diagnosis_workbook_path)) {
  message_line("Diagnosis workbook: %s", diagnosis_workbook_path)
}
if (!is.null(labs_workbook_path) && file.exists(labs_workbook_path)) {
  message_line("Labs workbook: %s", labs_workbook_path)
}
