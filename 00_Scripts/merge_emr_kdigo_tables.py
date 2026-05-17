import pandas as pd
import numpy as np
import os

# 1. 路径定义
p_emr = "/N/project/analgesia_perioperation/data/VitalDB_1.0.0/processed/emr_data/emr_data_table.csv"
p_kdigo = "/N/project/analgesia_perioperation/data/VitalDB_1.0.0/processed/emr_data/clinical_data_preop_filled_with_kdigo_aki_status.csv"
out_path = "/N/project/analgesia_perioperation/data/VitalDB_1.0.0/processed/emr_data/final_emr_kdigo_merged.csv"

# 2. 读取两张表
df_emr = pd.read_csv(p_emr, low_memory=False)
df_kdigo = pd.read_csv(p_kdigo, low_memory=False)

# 统一全小写
df_emr.columns = df_emr.columns.str.strip().str.lower()
df_kdigo.columns = df_kdigo.columns.str.strip().str.lower()

# 3. 在原先 EMR 提取出错误的 AKI 及化验被丢弃，全用 KDIGO 里的
drop_from_emr = ["aki", "aki_cat"]
emr_to_merge = df_emr.drop(columns=[col for col in drop_from_emr if col in df_emr.columns])

# 找到只存在于 emr 表中的新列
kdigo_cols = set(df_kdigo.columns)
emr_cols = set(emr_to_merge.columns)
unique_to_emr = list(emr_cols - kdigo_cols)

# 把它们连带 caseid 提取出来，准备拼接
emr_sub = emr_to_merge[["caseid"] + unique_to_emr]

# 4. 执行 Left Join 合并 (以你的 KDIGO 为主干)
df_final = df_kdigo.merge(emr_sub, on="caseid", how="left")

# ==========================================
# 5. [核心优化] 基于您的填补化验，重算 eGFR 等级！
# ==========================================

# 提取你完美填补后的肌酐和原本计算好的性别年龄
if all(col in df_final.columns for col in ["preop_cr", "age", "sex"]):
    # eGFR CKD-EPI 2021
    def calc_egfr(scr_mg_dl, age, female):
        kappa = np.where(female == 1, 0.7, 0.9)
        alpha = np.where(female == 1, -0.241, -0.302)
        sex_factor = np.where(female == 1, 1.012, 1.0)
        scr_k = scr_mg_dl / kappa
        egfr = (142 * (np.minimum(scr_k, 1) ** alpha) * (np.maximum(scr_k, 1) ** -1.200) * (0.9938 ** age) * sex_factor)
        return egfr
    
    # Sex: emr提取时将 F 转为 0, M 转为 1. 这里 female flag 为 sex == 0
    df_final['preop_cr'] = pd.to_numeric(df_final['preop_cr'], errors='coerce')
    df_final['age'] = pd.to_numeric(df_final['age'], errors='coerce')
    female_flag = np.where(df_final["sex"] == 0, 1, 0)
    
    # 重写覆盖 egfr 列
    df_final["egfr"] = calc_egfr(df_final["preop_cr"], df_final["age"], female_flag)
    
    # 重新分级
    df_final["renal_disease_category"] = np.select(
        [
            df_final["egfr"] >= 90,
            (df_final["egfr"] >= 60) & (df_final["egfr"] < 90),
            (df_final["egfr"] >= 30) & (df_final["egfr"] < 60),
            (df_final["egfr"] >= 15) & (df_final["egfr"] < 30),
            df_final["egfr"] < 15
        ],
        [1, 2, 3, 4, 5],
        default=np.nan
    )

    print(f"✅ eGFR 的缺失人数已由原理的 379 锐减到: {df_final['egfr'].isna().sum()} 人！")

# 贫血的重算 (hb < 13 for male, <12 for female)
if all(col in df_final.columns for col in ["preop_hb", "sex"]):
    df_final['preop_hb'] = pd.to_numeric(df_final['preop_hb'], errors='coerce')
    male_anemia = (df_final['sex'] == 1) & (df_final['preop_hb'] < 13)
    female_anemia = (df_final['sex'] == 0) & (df_final['preop_hb'] < 12)
    df_final["anemia_preop"] = np.where(male_anemia | female_anemia, 1, 0)
    df_final["anemia_preop"] = np.where(df_final["preop_hb"].isna() | df_final["sex"].isna(), np.nan, df_final["anemia_preop"])
    
    print(f"✅ 贫血特征的缺失人数已由原来的 341 锐减到: {df_final['anemia_preop'].isna().sum()} 人！")

# 6. 保存最强总表
df_final.to_csv(out_path, index=False)
print(f"🔥 大表重构完毕！包含列数: {len(df_final.columns)}，已保存至:\n{out_path}")

