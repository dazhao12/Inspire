#!/usr/bin/env python3
import os
from pathlib import Path

import numpy as np
import pandas as pd

INPUT_FILES = [
    Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/demographic_operation_latest.csv'),
    Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/demographic_subject_latest.csv'),
]

OUTLIER_DIR = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_outlier_removed')
IMPUTED_DIR = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputed')
SUMMARY_PATH = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputation_summary.csv')

HEIGHT_MIN, HEIGHT_MAX = 100, 250
WEIGHT_MIN, WEIGHT_MAX = 30, 300
BMI_MIN, BMI_MAX = 10, 100

ID_COLUMNS = {'subject_id', 'hadm_id', 'op_id'}


def find_col(df: pd.DataFrame, target: str):
    for c in df.columns:
        if c.lower() == target.lower():
            return c
    return None


def apply_outlier_rules(df: pd.DataFrame):
    df2 = df.copy()
    stats = {}

    height_col = find_col(df2, 'height')
    weight_col = find_col(df2, 'weight')
    bmi_col = find_col(df2, 'bmi')

    if height_col is not None:
        h = pd.to_numeric(df2[height_col], errors='coerce')
        out_mask = (h < HEIGHT_MIN) | (h > HEIGHT_MAX)
        stats['height_outlier_to_na'] = int(out_mask.sum())
        h[out_mask] = np.nan
        df2[height_col] = h

    if weight_col is not None:
        w = pd.to_numeric(df2[weight_col], errors='coerce')
        out_mask = (w < WEIGHT_MIN) | (w > WEIGHT_MAX)
        stats['weight_outlier_to_na'] = int(out_mask.sum())
        w[out_mask] = np.nan
        df2[weight_col] = w

    if bmi_col is not None:
        b = pd.to_numeric(df2[bmi_col], errors='coerce')
        out_mask = (b < BMI_MIN) | (b > BMI_MAX)
        stats['bmi_outlier_to_na'] = int(out_mask.sum())
        b[out_mask] = np.nan
        df2[bmi_col] = b

    return df2, stats


def infer_column_types(df: pd.DataFrame):
    numeric_cols = list(df.select_dtypes(include=[np.number]).columns)
    object_cols = list(df.select_dtypes(include=['object', 'category', 'bool']).columns)

    categorical_cols = set(object_cols)
    for c in numeric_cols:
        lc = c.lower()
        nunique = df[c].nunique(dropna=True)
        if lc == 'asa' or 'asa' in lc:
            categorical_cols.add(c)
            continue
        # Numeric low-cardinality columns are usually categorical flags/codes.
        if nunique <= 10:
            categorical_cols.add(c)

    continuous_cols = [
        c for c in numeric_cols
        if c not in categorical_cols and c.lower() not in ID_COLUMNS and c.lower() != 'bmi'
    ]

    categorical_cols = [c for c in df.columns if c in categorical_cols and c.lower() not in ID_COLUMNS and c.lower() != 'bmi']
    return continuous_cols, categorical_cols


def impute_dataframe(df_outlier: pd.DataFrame):
    df_imp = df_outlier.copy()
    summary = []

    height_col = find_col(df_imp, 'height')
    weight_col = find_col(df_imp, 'weight')
    bmi_col = find_col(df_imp, 'bmi')

    continuous_cols, categorical_cols = infer_column_types(df_imp)

    # Ensure Height/Weight are imputed as continuous with global median.
    for c in [height_col, weight_col]:
        if c is not None and c not in continuous_cols:
            continuous_cols.append(c)
        if c is not None and c in categorical_cols:
            categorical_cols.remove(c)

    for c in categorical_cols:
        miss_mask = df_imp[c].isna()
        miss_n = int(miss_mask.sum())
        mode_series = df_imp[c].mode(dropna=True)
        fill_value = mode_series.iloc[0] if not mode_series.empty else np.nan
        if pd.notna(fill_value):
            df_imp[c] = df_imp[c].fillna(fill_value)
        df_imp[f'{c}_imputed'] = miss_mask.astype(np.int8)
        summary.append({
            'column': c,
            'variable_type': 'categorical',
            'impute_method': 'mode',
            'filled_n': miss_n,
            'fill_value': fill_value,
        })

    for c in continuous_cols:
        s = pd.to_numeric(df_imp[c], errors='coerce')
        miss_mask = s.isna()
        miss_n = int(miss_mask.sum())
        median_val = s.median(skipna=True)
        if pd.notna(median_val):
            s = s.fillna(median_val)
        df_imp[c] = s
        df_imp[f'{c}_imputed'] = miss_mask.astype(np.int8)
        summary.append({
            'column': c,
            'variable_type': 'continuous',
            'impute_method': 'median',
            'filled_n': miss_n,
            'fill_value': median_val,
        })

    # Recalculate BMI from (possibly imputed) height/weight after cleaning.
    if height_col is not None and weight_col is not None and bmi_col is not None:
        h = pd.to_numeric(df_imp[height_col], errors='coerce')
        w = pd.to_numeric(df_imp[weight_col], errors='coerce')
        bmi_new = w / ((h / 100.0) ** 2)

        bmi_missing_before = pd.to_numeric(df_outlier[bmi_col], errors='coerce').isna()
        df_imp[bmi_col] = bmi_new
        df_imp['BMI_recalculated'] = (~h.isna() & ~w.isna()).astype(np.int8)
        if f'{height_col}_imputed' in df_imp.columns and f'{weight_col}_imputed' in df_imp.columns:
            df_imp['BMI_from_imputed_hw'] = (
                (df_imp[f'{height_col}_imputed'] == 1) |
                (df_imp[f'{weight_col}_imputed'] == 1)
            ).astype(np.int8)
        df_imp[f'{bmi_col}_imputed'] = bmi_missing_before.astype(np.int8)
        summary.append({
            'column': bmi_col,
            'variable_type': 'derived',
            'impute_method': 'recalculated_from_height_weight',
            'filled_n': int(bmi_missing_before.sum()),
            'fill_value': 'derived',
        })

    return df_imp, pd.DataFrame(summary)


def main():
    OUTLIER_DIR.mkdir(parents=True, exist_ok=True)
    IMPUTED_DIR.mkdir(parents=True, exist_ok=True)

    all_summary = []

    for in_file in INPUT_FILES:
        df = pd.read_csv(in_file, low_memory=False)

        df_out, out_stats = apply_outlier_rules(df)
        out_path = OUTLIER_DIR / in_file.name
        df_out.to_csv(out_path, index=False)

        df_imp, imp_summary = impute_dataframe(df_out)
        imp_path = IMPUTED_DIR / in_file.name
        df_imp.to_csv(imp_path, index=False)

        for k, v in out_stats.items():
            all_summary.append({
                'file': in_file.name,
                'column': k,
                'variable_type': 'rule_based_cleaning',
                'impute_method': 'set_outlier_to_na',
                'filled_n': v,
                'fill_value': np.nan,
            })

        if not imp_summary.empty:
            imp_summary.insert(0, 'file', in_file.name)
            all_summary.extend(imp_summary.to_dict(orient='records'))

        print(f'Processed: {in_file.name}')
        print(f'  Outlier-cleaned: {out_path}')
        print(f'  Imputed:         {imp_path}')

    if all_summary:
        pd.DataFrame(all_summary).to_csv(SUMMARY_PATH, index=False)
        print(f'Summary: {SUMMARY_PATH}')


if __name__ == '__main__':
    main()
