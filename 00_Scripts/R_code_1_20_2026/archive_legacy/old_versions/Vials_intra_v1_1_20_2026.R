library(data.table)
library(tidyverse)

# ==============================================================================
# 1. 环境设置与路径
# ==============================================================================
# 输入数据路径
raw_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/raw/"
# 输出数据路径
processed_path <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Vials_intra_1_20_2026"

# 如果输出目录不存在，自动创建
if (!dir.exists(processed_path)) {
  dir.create(processed_path, recursive = TRUE)
}

# ==============================================================================
# 2. 定义完整的变量列表 (带详细中文注释)
# ==============================================================================
# 这是一个包含所有目标变量名的向量，用于过滤 vitals 表
target_items <- c(
  # --- A. 基础生命体征 (Basic Vitals) ---
  "hr",       # 心率 (Heart Rate)
  "rr",       # 呼吸频率 (Respiratory Rate)
  "spo2",     # 外周血氧饱和度 (SpO2)
  "etco2",    # 呼气末二氧化碳 (ETCO2)
  "bt",       # 体温 (Body Temperature)
  
  # --- B. 血压监测 (Blood Pressure) ---
  "nibp_sbp", # 无创收缩压 (NIBP Systolic)
  "nibp_dbp", # 无创舒张压 (NIBP Diastolic)
  "nibp_mbp", # 无创平均压 (NIBP Mean)
  "art_sbp",  # 动脉收缩压 (Arterial Systolic)
  "art_dbp",  # 动脉舒张压 (Arterial Diastolic)
  "art_mbp",  # 动脉平均压 (Arterial Mean)
  
  # --- C. 呼吸机与气体参数 (Ventilation & Gases) ---
  "fio2",     # 吸入氧浓度 (FiO2)
  "vt",       # 潮气量 (Tidal Volume)
  "minvol",   # 每分钟通气量 (Minute Volume)
  "pip",      # 吸气峰压 (Peak Inspiratory Pressure)
  "peep",     # 呼气末正压 (PEEP)
  "pplat",    # 平台压 (Plateau Pressure)
  "pmean",    # 平均气道压 (Mean Airway Pressure)
  "etgas",    # 呼气末麻醉气体浓度 (End-tidal Anesthetic Gas)
  "cpat",     # 呼吸综合参数 (CPAT)
  "o2",       # 氧气流量 (Oxygen Flow)
  "air",      # 空气流量 (Air Flow)
  "n2o",      # 一氧化二氮流量 (Nitrous Oxide Flow)
  
  # --- D. 循环动力学与深度监测 (Hemodynamics & Depth) ---
  "cvp",      # 中心静脉压 (CVP)
  "pap_sbp",  # 肺动脉收缩压 (PAP Systolic)
  "pap_dbp",  # 肺动脉舒张压 (PAP Diastolic)
  "pap_mbp",  # 肺动脉平均压 (PAP Mean)
  "ci",       # 心脏指数 (Cardiac Index)
  "svi",      # 每搏指数 (Stroke Volume Index)
  "bis",      # 麻醉深度双频指数 (BIS)
  "cbro2",    # 脑区氧饱和度 (rSO2)
  
  # --- E. 心电图 ST 段分析 (ECG ST Segment) ---
  "stii",     # II导 ST段
  "stiii",    # III导 ST段
  "sti",      # I导 ST段
  "stv5",     # V5导联 ST段
  
  # --- F. 吸入麻醉药 (Inhalational Anesthetics) ---
  "etsevo",   # 呼气末七氟醚浓度 (Sevo)
  "etdes",    # 呼气末地氟醚浓度 (Des)
  "etiso",    # 呼气末异氟醚浓度 (Iso)
  
  # --- G. 静脉药物：血管活性药 (Vasoactive Drugs) ---
  "eph",      # 麻黄碱注射 (Ephedrine Bolus)
  "phe",      # 去氧肾上腺素注射 (Phenylephrine Bolus)
  "pepi",     # 去氧肾上腺素泵注 (Phenylephrine Infusion)
  "nepi",     # 去甲肾上腺素泵注 (Norepinephrine Infusion)
  "epi",      # 肾上腺素注射 (Epinephrine Bolus)
  "epii",     # 肾上腺素泵注 (Epinephrine Infusion)
  "dopai",    # 多巴胺泵注 (Dopamine Infusion)
  "dobui",    # 多巴酚丁胺泵注 (Dobutamine Infusion)
  "ntgi",     # 硝酸甘油泵注 (Nitroglycerin Infusion)
  "mlni",     # 米力农泵注 (Milrinone Infusion)
  "vaso",     # 加压素注射 (Vasopressin)
  
  # --- H. 静脉药物：麻醉与镇痛 (Anesthetics & Analgesics) ---
  "ppf",      # 异丙酚注射量 (Propofol Bolus/Amount)
  "ppfi",     # 异丙酚靶控目标浓度 (Propofol TCI Target)
  "rfti",     # 瑞芬太尼靶控目标浓度 (Remifentanil TCI Target)
  "ftn",      # 芬太尼注射 (Fentanyl)
  "sft",      # 舒芬太尼注射 (Sufentanil)
  "aft",      # 阿芬太尼泵注 (Alfentanil)
  "mdz",      # 咪达唑仑注射 (Midazolam)
  
  # --- I. 液体治疗 (Fluids) ---
  "ns",       # 生理盐水 (Normal Saline)
  "hs",       # 哈特曼液/乳酸林格 (Hartmann's/LR)
  "psa",      # 血浆代用品 (Plasma-Lyte)
  "hns",      # 半浓度盐水 (0.45% NaCl)
  "hes",      # 羟乙基淀粉 (HES)
  "d5w",      # 5% 葡萄糖 (D5W)
  "d10w",     # 10% 葡萄糖 (D10W)
  "d50w",     # 50% 葡萄糖 (D50W)
  "alb5",     # 5% 白蛋白 (Albumin 5%)
  "alb20",    # 20% 白蛋白 (Albumin 20%)
  
  # --- J. 血液制品 (Blood Products) ---
  "rbc",      # 红细胞悬液 (RBC)
  "ffp",      # 新鲜冰冻血浆 (FFP)
  "pc",       # 血小板浓缩物 (Platelets)
  "pheresis", # 单采血小板 (Platelet Pheresis)
  "cryo",     # 冷沉淀 (Cryoprecipitate)
  
  # --- K. 出入量与其他 (Outputs & Others) ---
  "ebl",      # 估计失血量 (Estimated Blood Loss)
  "uo",       # 尿量 (Urine Output)
  "ds"        # DS指标
)

# ==============================================================================
# 3. 读取并处理 Operations (手术表)
# ==============================================================================
cat("Step 1: 读取手术时间信息...\n")
ops <- fread(file.path(raw_path, "operations.csv"), 
             select = c("op_id", "subject_id", "orin_time", "orout_time"))

# 转换时间格式为 numeric
ops[, `:=`(orin_time = as.numeric(orin_time), orout_time = as.numeric(orout_time))]

# 计算 surgery_number (按 subject_id 分组，根据入室时间排序)
ops[, surgery_number := rowid(subject_id)] 

# ==============================================================================
# 4. 读取并处理 Vitals (生命体征表)
# ==============================================================================
cat("Step 2: 读取并过滤生命体征数据 (数据量较大，请稍候)...\n")

# 使用 fread 的 select 参数只读需要的列，最大限度节省内存
vitals <- fread(file.path(raw_path, "vitals.csv"), 
                select = c("op_id", "chart_time", "item_name", "value"))

# 4.1 过滤：只保留我们在 target_items 中定义的变量
vitals <- vitals[item_name %in% target_items]

# 4.2 类型转换
# value 转 numeric (原始数据中的空字符串或非数值字符会变为 NA)
vitals[, value := as.numeric(value)]
vitals[, chart_time := as.numeric(chart_time)]

# 4.3 去除无效值
vitals <- vitals[!is.na(value)] 

# ==============================================================================
# 5. 连接与术中时间过滤 (Intraop Window Filtering)
# ==============================================================================
cat("Step 3: 匹配术中时间窗口 (入室 -> 出室)...\n")

# 将 vitals 与 ops 连接 (Inner Join)
vitals_intraop <- merge(vitals, ops, by = "op_id", all.x = FALSE)

# 核心过滤：只保留 chart_time 在 [orin_time, orout_time] 之间的数据
vitals_intraop <- vitals_intraop[chart_time >= orin_time & chart_time <= orout_time]

# 计算相对时间 (min_from_entry)：相对于入室的分钟数
vitals_intraop[, min_from_entry := chart_time - orin_time]

# 清理内存 (删除原始大表)
rm(vitals, ops)
gc()

# ==============================================================================
# 6. 转宽表 (Pivoting) - 保留原始精度
# ==============================================================================
cat("Step 4: 生成宽表 (Pivoting)...\n")

# 使用 dcast 将 item_name 转为列名
# fun.aggregate = mean: 如果同一分钟有多个记录，取平均值。
# 对于绝大多数只有一条记录的情况，mean 不会改变原始数值的精度。
# na.rm = TRUE: 忽略计算过程中的 NA
final_wide <- dcast(
  vitals_intraop,
  subject_id + op_id + surgery_number + chart_time + min_from_entry ~ item_name,
  value.var = "value",
  fun.aggregate = mean, 
  na.rm = TRUE
)

# ==============================================================================
# 7. 排序与保存
# ==============================================================================
cat("Step 5: 排序并保存...\n")

# 按 病人 -> 手术次序 -> 时间 排序
setorder(final_wide, subject_id, surgery_number, chart_time)

# 定义输出文件名
output_file <- file.path(processed_path, "vital_intraop_full_complete.csv")

# 保存 (fwrite 默认保留高精度浮点数，不做截断)
fwrite(final_wide, output_file)

cat("=======================================================\n")
cat("处理完成！\n")
cat(sprintf("提取变量总数: %d 个\n", length(target_items)))
cat(sprintf("结果已保存至: %s\n", output_file))
cat("=======================================================\n")