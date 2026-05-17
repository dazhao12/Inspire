library(data.table)

# ==============================================================================
# 1. 环境设置与路径
# ==============================================================================
raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_1_21_final_2026"

if (!dir.exists(processed_path)) {
  dir.create(processed_path, recursive = TRUE)
}

# ==============================================================================
# 2. 变量定义
# ==============================================================================

# 用于提取术中时序的变量
target_items <- c(
  "hr", "rr", "spo2", "etco2", "bt",
  "nibp_sbp", "nibp_dbp", "nibp_mbp", "art_sbp", "art_dbp", "art_mbp",
  "fio2", "vt", "minvol", "pip", "peep", "pplat", "pmean", "etgas", "cpat", "o2", "air", "n2o",
  "cvp", "pap_sbp", "pap_dbp", "pap_mbp", "ci", "svi", "bis", "cbro2",
  "stii", "stiii", "sti", "stv5",
  "etsevo", "etdes", "etiso",
  "eph", "phe", "pepi", "nepi", "epi", "epii", "dopai", "dobui", "ntgi", "mlni", "vaso",
  "ppf", "ppfi", "rfti", "ftn", "sft", "aft", "mdz",
  "ns", "hs", "psa", "hns", "hes", "d5w", "d10w", "d50w", "alb5", "alb20",
  "rbc", "ffp", "pc", "pheresis", "cryo",
  "ebl", "uo", "ds"
)

# 汇总变量分类：
# 1) 可加总（剂量/体积/计数）
additive_vars <- c(
  "eph", "phe", "epi", "ppf", "ftn", "sft", "aft", "mdz",
  "ns", "hs", "psa", "hns", "hes", "d5w", "d10w", "d50w", "alb5", "alb20",
  "rbc", "ffp", "pc", "pheresis", "cryo",
  "ebl", "uo", "ds"
)

# 2) 不可直接加总（浓度/速率/泵注记录值）
non_additive_vars <- c(
  "etsevo", "etdes", "etiso",
  "pepi", "nepi", "epii", "dopai", "dobui", "ntgi", "mlni", "vaso",
  "ppfi", "rfti"
)

# 单位映射：若无法从源数据明确判断，统一标记 raw_source_unit
unit_map <- c(
  etsevo = "volpct",
  etdes = "volpct",
  etiso = "volpct",
  ns = "ml", hs = "ml", psa = "ml", hns = "ml", hes = "ml",
  d5w = "ml", d10w = "ml", d50w = "ml", alb5 = "ml", alb20 = "ml",
  ebl = "ml", uo = "ml", ds = "ml",
  rbc = "unit", ffp = "unit", pc = "unit", pheresis = "unit", cryo = "unit"
)

get_unit <- function(var_name) {
  if (var_name %in% names(unit_map)) return(unname(unit_map[[var_name]]))
  "raw_source_unit"
}

# ==============================================================================
# 3. 生成术中时序宽表
# ==============================================================================
cat(">>> Step 1: 读取手术时间信息...\n")
ops <- fread(
  file.path(raw_path, "operations.csv"),
  select = c("op_id", "subject_id", "orin_time", "orout_time")
)
ops[, `:=`(orin_time = as.numeric(orin_time), orout_time = as.numeric(orout_time))]
ops[, surgery_number := rowid(subject_id)]

cat(">>> Step 2: 读取并过滤术中记录...\n")
vitals <- fread(
  file.path(raw_path, "vitals.csv"),
  select = c("op_id", "chart_time", "item_name", "value"),
  na.strings = c("", "NA", "NULL", "(Null)", "null")
)
vitals <- vitals[item_name %in% target_items]
vitals[, `:=`(value = as.numeric(value), chart_time = as.numeric(chart_time))]
vitals <- vitals[!is.na(value) & !is.na(chart_time)]

cat(">>> Step 3: 匹配术中时间窗口 (orin_time <= chart_time <= orout_time)...\n")
vitals_intraop <- merge(vitals, ops, by = "op_id", all.x = FALSE)
vitals_intraop <- vitals_intraop[chart_time >= orin_time & chart_time <= orout_time]
vitals_intraop[, min_from_entry := chart_time - orin_time]

rm(vitals)
gc()

cat(">>> Step 4: 生成时序宽表...\n")
final_wide <- dcast(
  vitals_intraop,
  subject_id + op_id + surgery_number + chart_time + min_from_entry ~ item_name,
  value.var = "value",
  fun.aggregate = mean,
  na.rm = TRUE
)

setorder(final_wide, subject_id, surgery_number, chart_time)
output_file_ts <- file.path(processed_path, "vital_intraop_full_complete.csv")
fwrite(final_wide, output_file_ts)
cat(sprintf("   时序数据已保存: %s\n", output_file_ts))

# ==============================================================================
# 4. 生成术中汇总表（改良版）
# ==============================================================================
cat("\n>>> Step 5: 生成术中汇总表 (additive=sum, non-additive=mean+any_use)...\n")

existing_additive <- intersect(additive_vars, names(final_wide))
existing_non_additive <- intersect(non_additive_vars, names(final_wide))

sum_dt <- final_wide[, lapply(.SD, sum, na.rm = TRUE), by = op_id, .SDcols = existing_additive]
if (length(existing_additive) > 0L) {
  sum_new_names <- vapply(
    existing_additive,
    function(v) paste0(v, "_sum_", get_unit(v)),
    character(1)
  )
  setnames(sum_dt, old = existing_additive, new = sum_new_names)
}

mean_dt <- final_wide[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = op_id, .SDcols = existing_non_additive]
if (length(existing_non_additive) > 0L) {
  mean_new_names <- vapply(
    existing_non_additive,
    function(v) paste0(v, "_mean_", get_unit(v)),
    character(1)
  )
  setnames(mean_dt, old = existing_non_additive, new = mean_new_names)
}

any_dt <- final_wide[, lapply(.SD, function(x) as.integer(any(!is.na(x) & x > 0))), by = op_id, .SDcols = existing_non_additive]
if (length(existing_non_additive) > 0L) {
  any_new_names <- vapply(
    existing_non_additive,
    function(v) paste0(v, "_any_use_flag"),
    character(1)
  )
  setnames(any_dt, old = existing_non_additive, new = any_new_names)
}

meta_info <- unique(final_wide[, .(op_id, subject_id, surgery_number)])

final_output_sum <- copy(meta_info)
final_output_sum <- merge(final_output_sum, sum_dt, by = "op_id", all.x = TRUE)
final_output_sum <- merge(final_output_sum, mean_dt, by = "op_id", all.x = TRUE)
final_output_sum <- merge(final_output_sum, any_dt, by = "op_id", all.x = TRUE)
setorder(final_output_sum, subject_id, surgery_number, op_id)

output_file_sum <- file.path(processed_path, "drugs_fluids_total_sum.csv")
fwrite(final_output_sum, output_file_sum)

# 聚合规则清单，便于审计和数据字典
agg_contract <- rbindlist(list(
  data.table(
    source_var = existing_additive,
    output_var = vapply(existing_additive, function(v) paste0(v, "_sum_", get_unit(v)), character(1)),
    aggregation = "sum",
    unit = vapply(existing_additive, get_unit, character(1))
  ),
  data.table(
    source_var = existing_non_additive,
    output_var = vapply(existing_non_additive, function(v) paste0(v, "_mean_", get_unit(v)), character(1)),
    aggregation = "mean",
    unit = vapply(existing_non_additive, get_unit, character(1))
  ),
  data.table(
    source_var = existing_non_additive,
    output_var = vapply(existing_non_additive, function(v) paste0(v, "_any_use_flag"), character(1)),
    aggregation = "any_use",
    unit = "flag"
  )
), use.names = TRUE, fill = TRUE)

contract_file <- file.path(processed_path, "drugs_fluids_aggregation_contract.csv")
fwrite(agg_contract, contract_file)

cat("=======================================================\n")
cat("术中处理完成！\n")
cat("1. 时序宽表: op_id 严格绑定，时间窗为 orin_time~orout_time。\n")
cat("2. 汇总表: 可加总变量用 sum；浓度/速率变量改为 mean + any_use。\n")
cat(sprintf("3. 汇总输出已保存至: %s\n", output_file_sum))
cat(sprintf("4. 聚合契约表已保存至: %s\n", contract_file))
cat("=======================================================\n")
