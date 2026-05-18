suppressPackageStartupMessages({
  library(data.table)
})

processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_first_nonMAC_3_30_2026"
input_file <- file.path(processed_path, "vital_intraop_full_complete.csv")

qc_dirs <- list.dirs(processed_path, recursive = FALSE, full.names = TRUE)
qc_dirs <- qc_dirs[grepl("^timeseries_qc_first_nonMAC_", basename(qc_dirs))]
if (!length(qc_dirs)) {
  stop("No timeseries_qc_first_nonMAC_* directory found.")
}
latest_qc_dir <- qc_dirs[which.max(file.info(qc_dirs)$mtime)]

rollup_file <- file.path(latest_qc_dir, "timeseries_case_variable_rollup.csv")
overall_file <- file.path(latest_qc_dir, "timeseries_overall_qc.csv")
dict_file <- file.path(latest_qc_dir, "timeseries_variable_dictionary_aligned.csv")

if (!file.exists(rollup_file) || !file.exists(overall_file) || !file.exists(dict_file)) {
  stop("Latest QC directory is missing one or more required files.")
}

rollup_dt <- fread(rollup_file)
overall_dt <- fread(overall_file)
dict_dt <- fread(dict_file)

priority_map <- rbindlist(list(
  data.table(variable = c("ns", "hs", "hns", "d5w", "d10w", "d50w", "psa"),
             top_category_cn = "液体与出入量",
             subgroup_cn = "晶体液",
             variable_cn = c("生理盐水", "羟乙基淀粉相关液体HS", "高渗盐水相关液体HNS", "5%葡萄糖", "10%葡萄糖", "50%葡萄糖", "Plasma solution A"),
             display_order = 1:7),
  data.table(variable = c("hes", "alb5", "alb20"),
             top_category_cn = "液体与出入量",
             subgroup_cn = "胶体/白蛋白",
             variable_cn = c("羟乙基淀粉", "5%白蛋白", "20%白蛋白"),
             display_order = 8:10),
  data.table(variable = c("rbc", "ffp", "pc", "pheresis", "cryo"),
             top_category_cn = "血制品",
             subgroup_cn = "血制品",
             variable_cn = c("红细胞", "新鲜冰冻血浆", "血小板浓缩液", "单采血小板", "冷沉淀"),
             display_order = 11:15),
  data.table(variable = c("ebl", "uo"),
             top_category_cn = "液体与出入量",
             subgroup_cn = "出入量",
             variable_cn = c("估计失血量", "尿量"),
             display_order = 16:17),
  data.table(variable = c("eph", "epi", "phe", "vaso"),
             top_category_cn = "升压/血管活性药",
             subgroup_cn = "间断推注",
             variable_cn = c("麻黄碱", "肾上腺素推注", "去氧肾上腺素推注", "加压素推注"),
             display_order = 18:21),
  data.table(variable = c("nepi", "epii", "dobui", "dopai", "mlni", "ntgi", "pepi"),
             top_category_cn = "升压/血管活性药",
             subgroup_cn = "持续输注",
             variable_cn = c("去甲肾上腺素", "肾上腺素持续输注", "多巴酚丁胺", "多巴胺", "米力农", "硝酸甘油", "去氧肾上腺素持续输注"),
             display_order = 22:28),
  data.table(variable = c("mdz", "ftn", "sft", "aft", "ppf"),
             top_category_cn = "麻醉/镇静镇痛药",
             subgroup_cn = "间断推注",
             variable_cn = c("咪达唑仑", "芬太尼", "舒芬太尼", "阿芬太尼", "丙泊酚推注"),
             display_order = 29:33),
  data.table(variable = c("ppfi", "rfti"),
             top_category_cn = "麻醉/镇静镇痛药",
             subgroup_cn = "靶控浓度",
             variable_cn = c("丙泊酚靶控浓度", "瑞芬太尼靶控浓度"),
             display_order = 34:35),
  data.table(variable = c("air", "o2", "n2o"),
             top_category_cn = "麻醉相关气体",
             subgroup_cn = "医用气体流量",
             variable_cn = c("空气流量", "氧气流量", "笑气流量"),
             display_order = 36:38),
  data.table(variable = c("etdes", "etgas", "etiso", "etsevo"),
             top_category_cn = "麻醉相关气体",
             subgroup_cn = "吸入麻醉末梢浓度",
             variable_cn = c("地氟烷末梢浓度", "挥发性麻醉气体末梢浓度", "异氟烷末梢浓度", "七氟烷末梢浓度"),
             display_order = 39:42)
))

summary_role_cn <- c(
  volume_or_units_sum = "按手术累计总量/单位数",
  bolus_sum = "按手术累计推注总量",
  infusion_rate_distribution = "按手术内正值速率的中位水平",
  target_concentration_distribution = "按手术内正值靶控浓度的中位水平",
  gas_distribution = "按手术是否出现正值记录",
  monitoring_distribution = "按时间点分布描述"
)

category_rules_dt <- data.table(
  variable_group = c(
    "hemodynamic_respiratory_monitoring",
    "medical_gas_and_volatile_anesthetic",
    "fluids_blood_products_input_output",
    "intermittent_bolus_drugs",
    "continuous_infusion_rate_drugs",
    "target_concentration",
    "unmapped_review"
  ),
  category_cn = c(
    "生命体征/血流动力学/呼吸监测",
    "医用气体与吸入麻醉相关",
    "液体/血制品/出入量",
    "间断推注药物",
    "持续输注速率药物",
    "靶控/目标浓度",
    "待确认变量"
  ),
  examples_cn = c(
    "art_mbp, hr, spo2, etco2, fio2, bt, ci",
    "air, o2, n2o, etdes, etiso, etsevo",
    "ns, hs, hes, rbc, ffp, ebl, uo",
    "eph, epi, phe, vaso, mdz, ftn, ppf",
    "nepi, epii, dobui, dopai, mlni, ntgi, pepi",
    "ppfi, rfti",
    "cpat, ds"
  ),
  missing_rule_cn = c(
    "保留 NA，不用 0 替代；分析时按缺失处理。",
    "保留 NA，不用 0 替代；只有明确记录为 0 才视为未使用。",
    "保留 NA，不用 0 替代；缺失与未使用必须区分。",
    "保留 NA，不用 0 替代；推注药缺失不等于未用药。",
    "保留 NA，不用 0 替代；速率缺失与停泵不同。",
    "保留 NA，不用 0 替代；缺失不推断为未启用 TCI。",
    "全部先保留原值，仅做审阅，不进入正式清洗后的分析表。 "
  ),
  zero_rule_cn = c(
    "0 可保留，通常代表有效记录中的低值或无读数状态。",
    "0 可保留，通常代表该时间点未开启该气体/挥发麻醉。",
    "0 可保留，通常代表该时间点未输入/未输出。",
    "0 可保留，通常代表该时间点无推注。",
    "0 可保留，通常代表该时间点未输注。",
    "0 可保留，通常代表该时间点未启用或靶浓度为 0。",
    "0 暂不解释，先保留供人工核对。 "
  ),
  negative_rule_cn = c(
    "负值直接置 NA，并保留原始副本与 flag。",
    "负值直接置 NA，并保留原始副本与 flag。",
    "负值直接置 NA，并保留原始副本与 flag。",
    "负值直接置 NA，并保留原始副本与 flag。",
    "负值直接置 NA，并保留原始副本与 flag。",
    "负值直接置 NA，并保留原始副本与 flag。",
    "负值先保留并重点核查字段含义。 "
  ),
  threshold_rule_cn = c(
    "超出生理阈值的时间点置 NA；建议不做自动插补，短缺口另行处理。",
    "超出预设范围的时间点置 NA；保留原始值表以便追溯。",
    "超出保守上限的时间点置 NA；极端大值优先怀疑单位或录入错误。",
    "超出保守上限的时间点置 NA；孤立极大单次剂量建议人工复核。",
    "超出保守上限的时间点置 NA；若连续多点异常，应优先排查单位问题。",
    "超出保守上限的时间点置 NA；不把靶浓度当作给药总量。",
    "暂不做阈值清洗，仅输出分布与样例供确认。 "
  ),
  special_rule_cn = c(
    "监测变量后续如需建模，可针对极短缺口单独做 LOCF/线性插补，但不要覆盖主清洗表。",
    "医用气体和挥发麻醉建议同时保留原始数值与是否>0 的使用标记。",
    "血制品若出现非整数，建议先置 NA 并保留 review 标记；若后续确认允许半单位，再单独放宽。",
    "推注药更适合按每台手术总量/是否使用汇总，不建议做时间插补。",
    "持续泵药建议保留原始速率；下游可按时间加权均值或正值中位数聚合。",
    "TCI 变量建议仅作浓度/启用情况描述，不与 bolus 总量直接合并。",
    "cpat、ds 在含义确认前不进入正式液体/药物/监测分析。 "
  ),
  recommended_action_cn = c(
    "建议清洗版：NA 保留，负值置 NA，超阈值置 NA。",
    "建议清洗版：NA 保留，负值置 NA，超阈值置 NA。",
    "建议清洗版：NA 保留，负值置 NA，超阈值置 NA；血制品非整数先置 NA。",
    "建议清洗版：NA 保留，负值置 NA，超阈值置 NA。",
    "建议清洗版：NA 保留，负值置 NA，超阈值置 NA。",
    "建议清洗版：NA 保留，负值置 NA，超阈值置 NA。",
    "建议清洗版：仅 review，不进入 cleaned analytic table。"
  )
)

variable_cleaning_map <- copy(dict_dt[variable_group != "id_time", .(
  variable,
  variable_group,
  mapped_flag,
  Unit,
  Description,
  outlier_threshold
)])
setnames(variable_cleaning_map, c("Unit", "Description"), c("unit", "description"))
variable_cleaning_map <- merge(
  variable_cleaning_map,
  priority_map[, .(variable, variable_cn, top_category_cn, subgroup_cn)],
  by = "variable",
  all.x = TRUE
)
variable_cleaning_map <- merge(
  variable_cleaning_map,
  overall_dt[, .(variable, missing_pct, nonzero_pct, outlier_flag_pct)],
  by = "variable",
  all.x = TRUE
)
variable_cleaning_map <- merge(
  variable_cleaning_map,
  category_rules_dt[, .(variable_group, category_cn, recommended_action_cn)],
  by = "variable_group",
  all.x = TRUE
)
variable_cleaning_map[is.na(variable_cn), variable_cn := variable]
variable_cleaning_map[is.na(top_category_cn), top_category_cn := fifelse(
  variable_group == "hemodynamic_respiratory_monitoring", "生命体征/监测",
  fifelse(variable_group == "medical_gas_and_volatile_anesthetic", "麻醉相关气体", "其他")
)]
variable_cleaning_map[is.na(subgroup_cn), subgroup_cn := variable_group]

compute_case_usage_from_raw <- function(csv_file, vars) {
  select_cols <- c("op_id", vars)
  dt <- fread(csv_file, select = select_cols, na.strings = c("", "NA", "NULL", "(Null)", "null"), showProgress = FALSE)
  n_cases_total <- uniqueN(dt$op_id)
  res <- rbindlist(lapply(vars, function(v) {
    tmp <- dt[, .(
      any_record_flag = any(!is.na(get(v))),
      any_use_flag = any(get(v) > 0, na.rm = TRUE)
    ), by = op_id]
    data.table(
      variable = v,
      n_cases = n_cases_total,
      record_cases = tmp[, sum(any_record_flag)],
      use_cases = tmp[, sum(any_use_flag)]
    )
  }))
  res[, `:=`(
    record_case_pct = round(record_cases / n_cases * 100, 4),
    use_case_pct = round(use_cases / n_cases * 100, 4),
    median_missing_pct = NA_real_,
    p75_missing_pct = NA_real_,
    median_positive_metric = NA_real_,
    p75_positive_metric = NA_real_
  )]
  res
}

gas_vars <- c("air", "o2", "n2o", "etdes", "etgas", "etiso", "etsevo")
gas_rollup_dt <- compute_case_usage_from_raw(input_file, gas_vars)
gas_rollup_dt[, `:=`(
  variable_group = "medical_gas_and_volatile_anesthetic",
  summary_role = "gas_distribution"
)]

usage_dt <- rbindlist(list(
  rollup_dt[variable %in% priority_map$variable],
  gas_rollup_dt
), fill = TRUE, use.names = TRUE)

usage_dt <- merge(
  usage_dt,
  overall_dt[, .(variable, unit, description, missing_pct, nonzero_pct, p50, p95, p99, max_value, outlier_flag_pct, outlier_threshold)],
  by = "variable",
  all.x = TRUE
)
usage_dt <- merge(
  usage_dt,
  priority_map,
  by = "variable",
  all.x = TRUE
)
usage_dt[is.na(variable_cn), variable_cn := variable]
usage_dt[, summary_role_cn := summary_role_cn[summary_role]]
usage_dt[is.na(summary_role_cn), summary_role_cn := "按当前表中记录方式汇总"]

usage_dt[, use_note_cn := fifelse(
  top_category_cn == "液体与出入量" & subgroup_cn == "出入量", "该变量更偏向监测/记录，不完全等同于真实累计最终值。",
  fifelse(top_category_cn == "血制品", "优先看手术级使用比例，再结合总量/单位数解释。",
          fifelse(top_category_cn == "升压/血管活性药" & subgroup_cn == "持续输注", "优先按正值速率中位数解释，不建议直接求总量。",
                  fifelse(top_category_cn == "麻醉相关气体", "这里的使用率定义为手术内任一时间点 >0。", ""))
  )
)]

setorder(usage_dt, display_order, variable)
usage_export <- usage_dt[, .(
  一级类别 = top_category_cn,
  二级类别 = subgroup_cn,
  变量 = variable,
  中文名称 = variable_cn,
  单位 = unit,
  英文描述 = description,
  有记录手术数 = as.integer(record_cases),
  有记录手术占比_pct = round(record_case_pct, 2),
  有使用手术数 = as.integer(use_cases),
  有使用手术占比_pct = round(use_case_pct, 2),
  时间点缺失率_pct = round(missing_pct, 2),
  时间点非零率_pct = round(nonzero_pct, 2),
  时间点P50 = p50,
  时间点P95 = p95,
  时间点P99 = p99,
  最大值 = max_value,
  阳性手术代表值中位数 = median_positive_metric,
  阳性手术代表值P75 = p75_positive_metric,
  异常值标记率_pct = round(outlier_flag_pct, 4),
  统计口径 = summary_role_cn,
  解释备注 = use_note_cn
)]

fwrite(usage_export, file.path(latest_qc_dir, "timeseries_priority_usage_summary_zh.csv"))

md_lines <- c(
  "# INSPIRE 首次非 MAC 术中时序数据：液体/血制品/升压药/麻醉药使用情况中文汇总",
  "",
  sprintf("- 结果目录：`%s`", latest_qc_dir),
  sprintf("- 数据规模：`%s` 行时间点，`%s` 台手术。", format(overall_dt$n_total[1], big.mark = ","), format(unique(rollup_dt$n_cases[1]), big.mark = ",")),
  "- 使用率口径：某台手术中变量至少有 1 个 `>0` 时间点即定义为“有使用”。",
  "- 缺失值未按 0 处理。",
  ""
)

for (cat_name in unique(usage_export$一级类别)) {
  md_lines <- c(md_lines, sprintf("## %s", cat_name), "")
  sub_dt <- usage_export[一级类别 == cat_name]
  md_lines <- c(md_lines, "| 二级类别 | 变量 | 中文名称 | 单位 | 有使用手术数 | 有使用手术占比(%) | 时间点缺失率(%) | 时间点P50 | 时间点P95 | 阳性手术代表值中位数 | 统计口径 |")
  md_lines <- c(md_lines, "|---|---|---|---:|---:|---:|---:|---:|---:|---:|---|")
  for (i in seq_len(nrow(sub_dt))) {
    r <- sub_dt[i]
    md_lines <- c(md_lines, sprintf(
      "| %s | %s | %s | %s | %s | %.2f | %.2f | %s | %s | %s | %s |",
      r$二级类别,
      r$变量,
      r$中文名称,
      ifelse(is.na(r$单位), "", r$单位),
      format(r$有使用手术数, big.mark = ","),
      r$有使用手术占比_pct,
      r$时间点缺失率_pct,
      ifelse(is.na(r$时间点P50), "", format(signif(r$时间点P50, 4), trim = TRUE)),
      ifelse(is.na(r$时间点P95), "", format(signif(r$时间点P95, 4), trim = TRUE)),
      ifelse(is.na(r$阳性手术代表值中位数), "", format(signif(r$阳性手术代表值中位数, 4), trim = TRUE)),
      r$统计口径
    ))
  }
  md_lines <- c(md_lines, "")
}

writeLines(md_lines, file.path(latest_qc_dir, "timeseries_priority_usage_summary_zh.md"))

cleaning_category_export <- category_rules_dt[, .(
  变量类别 = category_cn,
  变量示例 = examples_cn,
  缺失值处理 = missing_rule_cn,
  零值处理 = zero_rule_cn,
  负值处理 = negative_rule_cn,
  超阈值处理 = threshold_rule_cn,
  特殊规则 = special_rule_cn,
  建议清洗动作 = recommended_action_cn
)]
fwrite(cleaning_category_export, file.path(latest_qc_dir, "timeseries_recommended_cleaning_rules_by_category_zh.csv"))

cleaning_threshold_export <- variable_cleaning_map[, .(
  一级类别 = top_category_cn,
  二级类别 = subgroup_cn,
  变量类别 = category_cn,
  变量 = variable,
  中文名称 = variable_cn,
  单位 = unit,
  英文描述 = description,
  是否参数字典已映射 = mapped_flag,
  当前异常值阈值 = outlier_threshold,
  时间点缺失率_pct = round(missing_pct, 2),
  时间点非零率_pct = round(nonzero_pct, 2),
  异常值标记率_pct = round(outlier_flag_pct, 4),
  建议清洗动作 = recommended_action_cn
)]
setorder(cleaning_threshold_export, 一级类别, 二级类别, 变量)
fwrite(cleaning_threshold_export, file.path(latest_qc_dir, "timeseries_recommended_cleaning_thresholds_by_variable_zh.csv"))

clean_md <- c(
  "# INSPIRE 首次非 MAC 术中时序数据：建议清洗版（保守规则）",
  "",
  "## 总原则",
  "",
  "- 缺失值保留为 `NA`，不默认填 0。",
  "- 负值直接置 `NA`，并保留原始副本与异常 flag。",
  "- 超出保守阈值的时间点置 `NA`，不在主清洗表中自动插补。",
  "- 0 值通常保留，因为常代表“已记录但该时间点未使用/未输入”。",
  "- `cpat`、`ds` 在含义确认前不进入正式 cleaned analytic table。",
  "",
  "## 分类规则",
  ""
)

for (i in seq_len(nrow(cleaning_category_export))) {
  r <- cleaning_category_export[i]
  clean_md <- c(
    clean_md,
    sprintf("### %s", r$变量类别),
    "",
    sprintf("- 变量示例：%s", r$变量示例),
    sprintf("- 缺失值处理：%s", r$缺失值处理),
    sprintf("- 0 值处理：%s", r$零值处理),
    sprintf("- 负值处理：%s", r$负值处理),
    sprintf("- 超阈值处理：%s", r$超阈值处理),
    sprintf("- 特殊规则：%s", r$特殊规则),
    sprintf("- 建议清洗动作：%s", r$建议清洗动作),
    ""
  )
}

writeLines(clean_md, file.path(latest_qc_dir, "timeseries_recommended_cleaning_rules_by_category_zh.md"))

cat(sprintf("Usage summary and cleaning-rule reports written to: %s\n", latest_qc_dir))
