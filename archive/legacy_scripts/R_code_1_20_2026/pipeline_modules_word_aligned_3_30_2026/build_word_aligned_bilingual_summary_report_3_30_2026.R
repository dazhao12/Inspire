suppressPackageStartupMessages({
  library(data.table)
})
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required for workbook output.")
}

processed_root <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed"
output_dir <- file.path(processed_root, "Word_Aligned_first_nonMAC_bilingual_summary_3_30_2026")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

path_demo <- file.path(processed_root, "Demographics_Timeline_first_nonMAC_3_30_2026")
path_comorb <- file.path(processed_root, "Diagnosis_word_comorbidities_first_nonMAC_3_30_2026")
path_acute <- file.path(processed_root, "Acute_Status_3mo_first_nonMAC_sepsis_A40_A41_3_30_2026")
path_meds <- file.path(processed_root, "Meds_Preop_word_first_nonMAC_3_30_2026")
path_labs <- file.path(processed_root, "lab_data_first_nonMAC_3_30_2026")
path_vitals <- file.path(processed_root, "Vials_pro_first_nonMAC_3_30_2026")
path_intra <- file.path(processed_root, "Vials_intra_first_nonMAC_3_30_2026")
path_outcome <- file.path(processed_root, "Outcomes_word_complications_first_nonMAC_sepsis_A40_A41_3_30_2026")

pct <- function(x, n) round(ifelse(n > 0, 100 * x / n, NA_real_), 2)

safe_num_summary <- function(x) {
  n_total <- length(x)
  n_nonmiss <- sum(!is.na(x))
  if (n_nonmiss == 0L) {
    return(list(
      n_total = n_total,
      n_nonmissing = 0L,
      missing_n = n_total,
      missing_pct = pct(n_total, n_total),
      mean = NA_real_,
      sd = NA_real_,
      median = NA_real_,
      p25 = NA_real_,
      p75 = NA_real_
    ))
  }
  list(
    n_total = n_total,
    n_nonmissing = n_nonmiss,
    missing_n = n_total - n_nonmiss,
    missing_pct = pct(n_total - n_nonmiss, n_total),
    mean = round(mean(x, na.rm = TRUE), 2),
    sd = round(sd(x, na.rm = TRUE), 2),
    median = round(median(x, na.rm = TRUE), 2),
    p25 = round(quantile(x, 0.25, na.rm = TRUE), 2),
    p75 = round(quantile(x, 0.75, na.rm = TRUE), 2)
  )
}

make_numeric_summary <- function(dt, vars, labels_dt, block_cn, block_en) {
  rbindlist(lapply(vars, function(v) {
    lab <- labels_dt[variable == v]
    s <- safe_num_summary(dt[[v]])
    data.table(
      block_cn = block_cn,
      block_en = block_en,
      variable = v,
      variable_cn = if (nrow(lab) > 0) lab$label_cn else v,
      variable_en = if (nrow(lab) > 0) lab$label_en else v,
      n_total = s$n_total,
      n_nonmissing = s$n_nonmissing,
      missing_n = s$missing_n,
      missing_pct = s$missing_pct,
      mean = s$mean,
      sd = s$sd,
      median = s$median,
      p25 = s$p25,
      p75 = s$p75
    )
  }), fill = TRUE)
}

make_binary_summary <- function(dt, vars, labels_dt, block_cn, block_en, interval_suffix = NULL) {
  rbindlist(lapply(vars, function(v) {
    lab <- labels_dt[variable == v]
    x <- dt[[v]]
    n_total <- length(x)
    n_nonmiss <- sum(!is.na(x))
    n_case <- sum(x == 1, na.rm = TRUE)
    out <- data.table(
      block_cn = block_cn,
      block_en = block_en,
      variable = v,
      variable_cn = if (nrow(lab) > 0) lab$label_cn else v,
      variable_en = if (nrow(lab) > 0) lab$label_en else v,
      n_total = n_total,
      n_nonmissing = n_nonmiss,
      missing_n = n_total - n_nonmiss,
      missing_pct = pct(n_total - n_nonmiss, n_total),
      n_cases = n_case,
      prevalence_pct = pct(n_case, n_total)
    )

    if (!is.null(interval_suffix)) {
      interval_col <- paste0(v, interval_suffix)
      if (interval_col %in% names(dt)) {
        iv <- dt[[interval_col]]
        iv_pos <- iv[x == 1 & !is.na(x)]
        out[, `:=`(
          interval_n_nonmissing = sum(!is.na(iv_pos)),
          interval_missing_among_cases_n = sum(is.na(iv_pos)),
          interval_missing_among_cases_pct = pct(sum(is.na(iv_pos)), length(iv_pos)),
          median_interval_min = round(median(iv_pos, na.rm = TRUE), 2),
          p25_interval_min = round(quantile(iv_pos, 0.25, na.rm = TRUE), 2),
          p75_interval_min = round(quantile(iv_pos, 0.75, na.rm = TRUE), 2)
        )]
        if (length(iv_pos) == 0 || all(is.na(iv_pos))) {
          out[, `:=`(median_interval_min = NA_real_, p25_interval_min = NA_real_, p75_interval_min = NA_real_)]
        }
      }
    }
    out
  }), fill = TRUE)
}

make_category_summary <- function(dt, var_name, levels_dt, block_cn, block_en) {
  x <- dt[[var_name]]
  n_total <- length(x)
  n_nonmiss <- sum(!is.na(x))
  tab <- as.data.table(table(x, useNA = "no"))
  if (nrow(tab) == 0) {
    tab <- data.table(x = character(), N = integer())
  }
  setnames(tab, c("category", "N"))
  lab <- merge(tab, levels_dt[variable == var_name], by = "category", all.x = TRUE, sort = FALSE)
  if (!("label_cn" %in% names(lab))) lab[, label_cn := as.character(category)]
  if (!("label_en" %in% names(lab))) lab[, label_en := as.character(category)]
  lab[, `:=`(
    block_cn = block_cn,
    block_en = block_en,
    variable = var_name,
    n_total = n_total,
    n_nonmissing = n_nonmiss,
    missing_n = n_total - n_nonmiss,
    missing_pct = pct(n_total - n_nonmiss, n_total),
    prevalence_pct = pct(N, n_total)
  )]
  setcolorder(lab, c("block_cn", "block_en", "variable", "category", "label_cn", "label_en",
                     "N", "prevalence_pct", "n_total", "n_nonmissing", "missing_n", "missing_pct"))
  lab
}

write_sheet_pair <- function(wb, base_name, dt, output_dir) {
  dt_cn <- copy(dt)
  names(dt_cn) <- sub("^block_cn$", "板块", names(dt_cn))
  names(dt_cn) <- sub("^variable_cn$", "变量", names(dt_cn))
  names(dt_cn) <- sub("^label_cn$", "分类", names(dt_cn))
  names(dt_cn) <- sub("^category$", "编码", names(dt_cn))
  setnames(dt_cn, names(dt_cn), make.unique(names(dt_cn)))
  fwrite(dt_cn, file.path(output_dir, paste0(base_name, "_cn.csv")))

  dt_en <- copy(dt)
  names(dt_en) <- sub("^block_en$", "Block", names(dt_en))
  names(dt_en) <- sub("^variable_en$", "Variable", names(dt_en))
  names(dt_en) <- sub("^label_en$", "Category", names(dt_en))
  names(dt_en) <- sub("^category$", "Code", names(dt_en))
  setnames(dt_en, names(dt_en), make.unique(names(dt_en)))
  fwrite(dt_en, file.path(output_dir, paste0(base_name, "_en.csv")))

  openxlsx::addWorksheet(wb, paste0(base_name, "_cn"))
  openxlsx::writeData(wb, paste0(base_name, "_cn"), dt_cn, withFilter = TRUE)
  openxlsx::freezePane(wb, paste0(base_name, "_cn"), firstRow = TRUE)

  openxlsx::addWorksheet(wb, paste0(base_name, "_en"))
  openxlsx::writeData(wb, paste0(base_name, "_en"), dt_en, withFilter = TRUE)
  openxlsx::freezePane(wb, paste0(base_name, "_en"), firstRow = TRUE)
}

comorb_labels <- data.table(
  variable = c(
    "hypertension","ischemic_heart_disease","heart_failure","arrhythmia","atrial_fibrillation_flutter",
    "pulmonary_hypertension","peripheral_vascular_disease","cerebrovascular_disease","dementia","parkinsonism",
    "copd","asthma","renal_disease","renal_dialysis","chronic_liver_disease","peptic_ulcer_disease","gerd",
    "obesity","diabetes","hyperlipidemia","anemia_icd10","anemia_preoperative","connective_tissue_disease","malignancy"
  ),
  label_cn = c(
    "高血压","缺血性心脏病","心力衰竭","心律失常","房颤/房扑","肺动脉高压","外周血管病","脑血管病","痴呆","帕金森病/帕金森综合征",
    "慢性阻塞性肺疾病","哮喘","肾病","肾透析","慢性肝病","消化性溃疡","胃食管反流病","肥胖","糖尿病","高脂血症","贫血(ICD/Hb)","术前贫血","结缔组织病","恶性肿瘤"
  ),
  label_en = c(
    "Hypertension","Ischemic heart disease","Heart failure","Arrhythmia","Atrial fibrillation/flutter","Pulmonary hypertension",
    "Peripheral vascular disease","Cerebrovascular disease","Dementia","Parkinson's disease/parkinsonism","COPD","Asthma",
    "Renal disease","Renal dialysis","Chronic liver disease","Peptic ulcer disease","GERD","Obesity","Diabetes","Hyperlipidemia",
    "Anemia (ICD/Hb)","Preoperative anemia","Connective tissue disease","Malignancy"
  )
)

acute_labels <- data.table(
  variable = c("acute_myocardial_infarction","cerebral_infarction","cardiac_arrest","ards","pulmonary_embolism","sepsis","pneumonia","shock","ventilation","iabp","ecmo","oxygen_therapy"),
  label_cn = c("急性心肌梗死","脑梗死","心搏骤停","ARDS","肺栓塞","脓毒症","肺炎","休克","机械通气","主动脉内球囊反搏","ECMO","氧疗"),
  label_en = c("Acute myocardial infarction","Cerebral infarction","Cardiac arrest","ARDS","Pulmonary embolism","Sepsis","Pneumonia","Shock","Ventilation","IABP","ECMO","Oxygen therapy")
)

med_labels <- data.table(
  variable = c(
    "beta_blockers","calcium_channel_blockers","acei","arb","diuretics","other_antihypertensive","amiodarone","other_antiarrhythmics",
    "statins","antiplatelets","anticoagulants","thrombolytics","antifibrinolytics","nitrates","insulins","antidiabetics",
    "corticosteroids","thyroid_hormones","antithyroids","antibiotics","antimycotics","anti_tuberculosis","antivirals","ivig",
    "antineoplastic","immunosuppression","nsaids","antiepileptics","antiparkinson","psycholeptics","psychoanaleptics",
    "drugs_for_obstructive_airway_diseases","antihistamines","inotropes_and_vasopressors","bile_and_liver_therapy",
    "serotonin_5ht3_antagonists","h2_receptor_antagonists","proton_pump_inhibitors","other_drugs_for_acid_related_disorder","opioids"
  ),
  label_cn = c(
    "β受体阻滞剂","钙通道阻滞剂","ACEI","ARB","利尿剂","其他降压药","胺碘酮","其他抗心律失常药","他汀类","抗血小板药",
    "抗凝药","溶栓药","抗纤溶药","硝酸酯类","胰岛素","口服降糖药","糖皮质激素","甲状腺激素","抗甲状腺药","抗生素",
    "抗真菌药","抗结核药","抗病毒药","静脉丙球","抗肿瘤药","免疫抑制剂","NSAIDs","抗癫痫药","抗帕金森药","镇静催眠药",
    "精神兴奋/抗抑郁药","阻塞性气道疾病用药","抗组胺药","正性肌力药/血管活性药","胆汁与护肝治疗","5-HT3拮抗剂",
    "H2受体拮抗剂","质子泵抑制剂","其他酸相关疾病用药","阿片类"
  ),
  label_en = c(
    "Beta-blockers","Calcium channel blockers","ACE inhibitors","ARBs","Diuretics","Other antihypertensives","Amiodarone","Other antiarrhythmics",
    "Statins","Antiplatelets","Anticoagulants","Thrombolytics","Antifibrinolytics","Nitrates","Insulins","Antidiabetics","Corticosteroids",
    "Thyroid hormones","Antithyroid agents","Antibiotics","Antimycotics","Anti-tuberculosis drugs","Antivirals","IVIG","Antineoplastic agents",
    "Immunosuppressants","NSAIDs","Antiepileptics","Antiparkinson drugs","Psycholeptics","Psychoanaleptics","Drugs for obstructive airway diseases",
    "Antihistamines","Inotropes and vasopressors","Bile and liver therapy","Serotonin (5-HT3) antagonists","H2-receptor antagonists",
    "Proton pump inhibitors","Other drugs for acid-related disorders","Opioids"
  )
)

outcome_labels <- data.table(
  variable = c(
    "death_within_hospital_stay","death_within_30_days","death_within_6month","unexpected_icu_admission_from_general_ward","reoperation",
    "acute_myocardial_infarction","angina","pulmonary_embolism","cardiac_arrest","new_onset_af","iabp_postop","ecmo_postop",
    "cerebral_infarction","intracerebral_hemorrhage","subarachnoid_hemorrhage","subdural_hemorrhage","hemiplegia","paraplegia","tia",
    "cerebrospinal_fluid_leak","ards","pneumonia","pleural_effusion","intestinal_ischemia","ileus","peritonitis","hepatic_failure",
    "acute_pancreatitis","gastrointestinal_hemorrhage","acute_kidney_failure_icd10","aki_creatinine","crrt_postop","shock","dic",
    "vocal_cord_larynx_paralysis","sepsis","antibiotic_escalation_vanc_or_carbapenem","inotropes_and_vasopressors_postop",
    "antiepileptics_new_postop","antihemorrhagics_postop","ventilation_postop","icu_stay"
  ),
  label_cn = c(
    "住院死亡","30天死亡","术后3个月死亡","普通病房转入ICU","再次手术","急性心肌梗死","心绞痛","肺栓塞","心搏骤停","新发房颤",
    "术后IABP","术后ECMO","脑梗死","脑内出血","蛛网膜下腔出血","硬膜下出血","偏瘫","截瘫","短暂性脑缺血发作","脑脊液漏","ARDS","肺炎","胸腔积液",
    "肠缺血","肠梗阻","腹膜炎","肝衰竭","急性胰腺炎","消化道出血","急性肾损伤(ICD)","AKI(肌酐)","术后CRRT","休克","DIC","声带/喉麻痹",
    "脓毒症","抗生素升级(万古/碳青霉烯)","术后正性肌力药/血管活性药","术后新发抗癫痫药","术后止血药","术后机械通气","ICU住院"
  ),
  label_en = c(
    "In-hospital death","Death within 30 days","Death within 3 months","Unexpected ICU admission from ward","Reoperation",
    "Acute myocardial infarction","Angina","Pulmonary embolism","Cardiac arrest","New-onset AF","Postoperative IABP","Postoperative ECMO",
    "Cerebral infarction","Intracerebral hemorrhage","Subarachnoid hemorrhage","Subdural hemorrhage","Hemiplegia","Paraplegia","TIA",
    "Cerebrospinal fluid leak","ARDS","Pneumonia","Pleural effusion","Intestinal ischemia","Ileus","Peritonitis","Hepatic failure",
    "Acute pancreatitis","Gastrointestinal hemorrhage","Acute kidney failure (ICD-10)","AKI (creatinine)","Postoperative CRRT","Shock","DIC",
    "Vocal cord/larynx paralysis","Sepsis","Antibiotic escalation (vanc/carbapenem)","Postoperative inotropes/vasopressors","New postoperative antiepileptics",
    "Postoperative antihemorrhagics","Postoperative ventilation","ICU stay"
  )
)

lab_labels <- data.table(
  variable = c("glucose","creatinine","hct","potassium","sodium","hb","wbc","platelet","chloride","lymphocyte","seg","bun",
               "calcium","phosphorus","albumin","total_bilirubin","alt","ast","total_protein","alp","crp","sao2","hco3","ptinr","ph",
               "pao2","paco2","aptt","ica","fibrinogen","be","lacate","ckmb","ck","troponin_i","hba1c","troponin_t","d_dimer"),
  label_cn = c("血糖","肌酐","红细胞压积","钾","钠","血红蛋白","白细胞","血小板","氯","淋巴细胞","中性粒细胞比例","尿素氮","钙","磷","白蛋白",
               "总胆红素","ALT","AST","总蛋白","碱性磷酸酶","C反应蛋白","血氧饱和度","碳酸氢根","INR","pH","PaO2","PaCO2","APTT","离子钙","纤维蛋白原","碱剩余","乳酸","CK-MB","CK","肌钙蛋白I","HbA1c","肌钙蛋白T","D-二聚体"),
  label_en = c("Glucose","Creatinine","Hematocrit","Potassium","Sodium","Hemoglobin","White blood cells","Platelet","Chloride","Lymphocyte","Segmented neutrophils","BUN","Calcium","Phosphorus","Albumin","Total bilirubin","ALT","AST","Total protein","ALP","CRP","SaO2","HCO3","PT-INR","pH","PaO2","PaCO2","APTT","Ionized calcium","Fibrinogen","Base excess","Lactate","CK-MB","CK","Troponin I","HbA1c","Troponin T","D-dimer")
)

vitals_labels <- data.table(
  variable = c("Systolic BP (mmHg)","Diastolic BP (mmHg)","Mean BP (mmHg)","Heart Rate (bpm)","SpO2 (%)","Resp Rate (bpm)","Body Temp (C)"),
  label_cn = c("收缩压","舒张压","平均动脉压","心率","血氧饱和度","呼吸频率","体温"),
  label_en = c("Systolic BP","Diastolic BP","Mean BP","Heart rate","SpO2","Respiratory rate","Body temperature")
)

intra_labels <- data.table(
  variable = c("ebl","hs","psa","uo","ppf","eph","etsevo","ftn","etdes"),
  label_cn = c("估计失血量","高张液","平衡盐液","尿量","丙泊酚","麻黄碱","呼气末七氟醚","芬太尼","呼气末地氟醚"),
  label_en = c("Estimated blood loss","Hypertonic saline","Plasma solution A","Urine output","Propofol","Ephedrine","End-tidal sevoflurane","Fentanyl","End-tidal desflurane")
)

demo_num_labels <- data.table(
  variable = c("Age","Height","Weight","BMI"),
  label_cn = c("年龄","身高","体重","BMI"),
  label_en = c("Age","Height","Weight","BMI")
)

demo_levels <- rbindlist(list(
  data.table(variable = "Male", category = c("0","1"), label_cn = c("女","男"), label_en = c("Female","Male")),
  data.table(variable = "Emergency_op", category = c("0","1"), label_cn = c("择期","急诊"), label_en = c("Elective","Emergency"))
), fill = TRUE)

wb <- openxlsx::createWorkbook()

demo_op <- fread(file.path(path_demo, "Demographic_Operation_Level.csv"))
comorb <- fread(file.path(path_comorb, "comorbidity_word_defined_anchor_first_nonMAC.csv"))
acute <- fread(file.path(path_acute, "acute_status_3mo_before_orin_first_nonMAC.csv"))
meds <- fread(file.path(path_meds, "preop_meds_word_defined_first_nonMAC.csv"))
labs_current <- fread(file.path(path_labs, "preop_labs_features_current_stay.csv"))
labs_window <- fread(file.path(path_labs, "preop_labs_window_summary.csv"))
vitals_sum <- fread(file.path(path_vitals, "preop_vitals_summary_coverage.csv"))
vitals_source <- fread(file.path(path_vitals, "preop_vitals_source_coverage.csv"))
intra_sum <- fread(file.path(path_intra, "drugs_fluids_descriptive_stats.csv"))
outcome <- fread(file.path(path_outcome, "postop_complications_word_defined_first_nonMAC.csv"))
outcome_duration <- fread(file.path(path_outcome, "postop_complications_duration_summary.csv"))

n_anchor <- nrow(demo_op)
overview <- data.table(
  block_cn = c("总体","总体","总体","总体","总体","总体","总体","总体"),
  block_en = c("Overview","Overview","Overview","Overview","Overview","Overview","Overview","Overview"),
  variable = c("anchor_operations","unique_subjects","unique_admissions","lab_current_rows","preop_vitals_rows","intraop_total_rows","intraop_timeseries_rows","intraop_timeseries_unique_ops"),
  variable_cn = c("锚定手术数","唯一患者数","唯一住院数","术前实验室当前住院行数","术前体征行数","术中总量行数","术中时序行数","术中时序涉及手术数"),
  variable_en = c("Anchor operations","Unique subjects","Unique admissions","Preop labs current-stay rows","Preop vitals rows","Intraop total rows","Intraop timeseries rows","Intraop timeseries unique operations"),
  value = c(
    n_anchor,
    uniqueN(demo_op$subject_id),
    uniqueN(demo_op[, .(subject_id, hadm_id)]),
    nrow(labs_current),
    nrow(fread(file.path(path_vitals, "preop_baseline_final.csv"), select = c("op_id"))),
    nrow(fread(file.path(path_intra, "drugs_fluids_total_sum.csv"), select = c("op_id"))),
    nrow(fread(file.path(path_intra, "vital_intraop_full_complete.csv"), select = c("op_id"))),
    uniqueN(fread(file.path(path_intra, "vital_intraop_full_complete.csv"), select = c("subject_id","hadm_id","op_id")))
  ),
  note_cn = c(
    "每次住院保留首次非MAC手术",
    "来自锚定手术表",
    "subject_id + hadm_id",
    "当前住院术前窗口",
    "每台锚定手术1行",
    "每台锚定手术1行",
    "术中时序宽表",
    "有术中记录的手术数"
  ),
  note_en = c(
    "First non-MAC surgery kept per admission",
    "Derived from anchor operation table",
    "subject_id + hadm_id",
    "Current-stay preoperative window",
    "One row per anchor operation",
    "One row per anchor operation",
    "Intraoperative timeseries wide table",
    "Operations with intraoperative records"
  )
)
write_sheet_pair(wb, "overview", overview, output_dir)

demo_numeric <- make_numeric_summary(demo_op, c("Age","Height","Weight","BMI"), demo_num_labels, "人口学", "Demographics")
write_sheet_pair(wb, "demographics_numeric", demo_numeric, output_dir)

demo_cat <- rbindlist(list(
  make_category_summary(data.table(Male = as.character(demo_op$Male)), "Male", demo_levels, "人口学", "Demographics"),
  make_category_summary(data.table(Emergency_op = as.character(demo_op$Emergency_op)), "Emergency_op", demo_levels, "人口学", "Demographics"),
  make_category_summary(data.table(asa = as.character(demo_op$asa)), "asa", data.table(variable = "asa", category = as.character(sort(unique(na.omit(demo_op$asa)))), label_cn = paste0("ASA ", sort(unique(na.omit(demo_op$asa)))), label_en = paste0("ASA ", sort(unique(na.omit(demo_op$asa))))), "人口学", "Demographics"),
  make_category_summary(data.table(race = as.character(demo_op$race)), "race", data.table(variable = "race", category = unique(as.character(demo_op$race)), label_cn = unique(as.character(demo_op$race)), label_en = unique(as.character(demo_op$race))), "人口学", "Demographics"),
  make_category_summary(data.table(department = as.character(demo_op$department)), "department", data.table(variable = "department", category = unique(as.character(demo_op$department)), label_cn = unique(as.character(demo_op$department)), label_en = unique(as.character(demo_op$department))), "人口学", "Demographics"),
  make_category_summary(data.table(antype = as.character(demo_op$antype)), "antype", data.table(variable = "antype", category = unique(as.character(demo_op$antype)), label_cn = unique(as.character(demo_op$antype)), label_en = unique(as.character(demo_op$antype))), "人口学", "Demographics")
), fill = TRUE)
write_sheet_pair(wb, "demographics_categorical", demo_cat, output_dir)

comorb_binary <- make_binary_summary(comorb, comorb_labels$variable, comorb_labels, "既往史", "Comorbidities")
write_sheet_pair(wb, "comorbidity_binary", comorb_binary, output_dir)

comorb_cat_levels <- rbindlist(list(
  data.table(variable = "diabetes_category", category = c("1","2"), label_cn = c("胰岛素依赖","非胰岛素依赖/其他"), label_en = c("Insulin-dependent","Non-insulin-dependent/other")),
  data.table(variable = "anemia_preop_severity", category = c("1","2","3"), label_cn = c("轻度","中度","重度"), label_en = c("Mild","Moderate","Severe")),
  data.table(variable = "renal_disease_category", category = c("1","2","3","4","5"), label_cn = c("1级","2级","3级","4级","5级"), label_en = c("Stage 1","Stage 2","Stage 3","Stage 4","Stage 5"))
), fill = TRUE)
comorb_cat <- rbindlist(list(
  make_category_summary(data.table(diabetes_category = as.character(comorb$diabetes_category)), "diabetes_category", comorb_cat_levels, "既往史分级", "Comorbidity categories"),
  make_category_summary(data.table(anemia_preop_severity = as.character(comorb$anemia_preop_severity)), "anemia_preop_severity", comorb_cat_levels, "既往史分级", "Comorbidity categories"),
  make_category_summary(data.table(renal_disease_category = as.character(comorb$renal_disease_category)), "renal_disease_category", comorb_cat_levels, "既往史分级", "Comorbidity categories")
), fill = TRUE)
write_sheet_pair(wb, "comorbidity_category", comorb_cat, output_dir)

acute_binary <- make_binary_summary(acute, acute_labels$variable, acute_labels, "术前3个月急性状态", "Acute status within 3 months", "_interval_to_surgery_min")
write_sheet_pair(wb, "acute_status", acute_binary, output_dir)

med_binary <- make_binary_summary(meds, med_labels$variable, med_labels, "术前用药", "Preoperative medications")
write_sheet_pair(wb, "medications", med_binary, output_dir)

lab_nearest_cols <- grep("^preop_.*_nearest$", names(labs_current), value = TRUE)
lab_nearest_summary <- rbindlist(lapply(lab_nearest_cols, function(v) {
  base_var <- sub("^preop_", "", sub("_nearest$", "", v))
  lab <- lab_labels[variable == base_var]
  s <- safe_num_summary(labs_current[[v]])
  data.table(
    block_cn = "术前实验室",
    block_en = "Preoperative labs",
    variable = v,
    variable_cn = if (nrow(lab) > 0) lab$label_cn else base_var,
    variable_en = if (nrow(lab) > 0) lab$label_en else base_var,
    n_total = s$n_total,
    n_nonmissing = s$n_nonmissing,
    missing_n = s$missing_n,
    missing_pct = s$missing_pct,
    mean = s$mean,
    sd = s$sd,
    median = s$median,
    p25 = s$p25,
    p75 = s$p75
  )
}), fill = TRUE)
write_sheet_pair(wb, "labs_current_nearest", lab_nearest_summary, output_dir)

labs_window_out <- copy(labs_window)
labs_window_out[, `:=`(
  block_cn = "术前实验室窗口覆盖",
  block_en = "Preoperative lab window coverage",
  variable_cn = c("严格既往史","当前住院术前","累计术前","当前住院90天","当前住院30天","当前住院7天"),
  variable_en = c("Strict history","Current stay preop","Cumulative preop","Current stay 90d","Current stay 30d","Current stay 7d")
)]
setcolorder(labs_window_out, c("block_cn","block_en","window","variable_cn","variable_en", setdiff(names(labs_window_out), c("block_cn","block_en","window","variable_cn","variable_en"))))
write_sheet_pair(wb, "labs_window", labs_window_out, output_dir)

vitals_merge <- copy(vitals_sum)
vitals_merge[, `:=`(
  n_total = 112042L,
  n_missing = 112042L - N_Present,
  pct_missing = round(100 - Coverage_Pct, 2),
  key = vital_sign,
  variable = vital_sign
)]
vitals_merge <- merge(vitals_merge, vitals_labels, by.x = "key", by.y = "variable", all.x = TRUE)
vitals_merge[, `:=`(block_cn = "术前体征", block_en = "Preoperative vitals")]
setcolorder(vitals_merge, c("block_cn","block_en","vital_sign","label_cn","label_en", setdiff(names(vitals_merge), c("block_cn","block_en","vital_sign","label_cn","label_en"))))
write_sheet_pair(wb, "preop_vitals", vitals_merge, output_dir)

intra_sum[, key := Base_Variable]
intra_sum <- merge(intra_sum, intra_labels, by.x = "key", by.y = "variable", all.x = TRUE)
intra_sum[is.na(label_cn), label_cn := Base_Variable]
intra_sum[is.na(label_en), label_en := Base_Variable]
intra_sum[, `:=`(
  block_cn = "术中药液与事件",
  block_en = "Intraoperative drugs, fluids, and events",
  missing_n = N_Total - N_Non_Missing,
  missing_pct = pct(N_Total - N_Non_Missing, N_Total)
)]
write_sheet_pair(wb, "intraop", intra_sum, output_dir)

outcome_binary_vars <- outcome_labels$variable[outcome_labels$variable %in% names(outcome)]
outcome_binary <- make_binary_summary(outcome, outcome_binary_vars, outcome_labels, "术后并发症", "Postoperative complications", "_interval_after_surgery_min")
write_sheet_pair(wb, "outcomes_binary", outcome_binary, output_dir)

duration_labels <- data.table(
  metric = c("icu_duration_min","hospital_stay_duration_min","death_time_from_orout_min"),
  label_cn = c("ICU时长(分钟)","术后住院时长(分钟)","距术毕死亡时间(分钟)"),
  label_en = c("ICU duration (min)","Postoperative hospital stay duration (min)","Time to death from OR-out (min)")
)
outcome_duration <- merge(outcome_duration, duration_labels, by = "metric", all.x = TRUE)
outcome_duration[, `:=`(block_cn = "术后时长指标", block_en = "Postoperative duration metrics")]
write_sheet_pair(wb, "outcomes_duration", outcome_duration, output_dir)

openxlsx::saveWorkbook(wb, file.path(output_dir, "word_aligned_first_nonMAC_summary_bilingual.xlsx"), overwrite = TRUE)
cat(sprintf("Saved bilingual summary report to %s\n", output_dir))
