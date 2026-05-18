#!/usr/bin/env python3
from pathlib import Path
import pandas as pd
import numpy as np

IN_FILE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_baseline_final_median_no_imputation_latest.csv')

OUT_NO_IMPUTE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_baseline_final_median_outlier_removed_no_imputation_latest.csv')
OUT_IMPUTED = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputed/preop_baseline_final_median_outlier_removed_imputed_latest.csv')
OUT_SUMMARY = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/preop_baseline_final_median_outlier_impute_summary_20260416.csv')

ID_COLS = ['subject_id', 'hadm_id', 'op_id']
CONTINUOUS_OTHERS = ['preop_sbp', 'preop_dbp', 'preop_hr', 'preop_spo2', 'preop_rr', 'preop_bt']


def main():
    OUT_NO_IMPUTE.parent.mkdir(parents=True, exist_ok=True)
    OUT_IMPUTED.parent.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(IN_FILE, low_memory=False)

    # Ensure numeric parsing for vital columns.
    for c in ['preop_sbp', 'preop_dbp', 'preop_mbp', 'preop_hr', 'preop_spo2', 'preop_rr', 'preop_bt']:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors='coerce')

    # 1) Outlier-to-NA for no-imputation base version.
    mbp_outlier_mask = df['preop_mbp'] < 25
    bt_outlier_mask = df['preop_bt'] < 30

    df_no = df.copy()
    df_no.loc[mbp_outlier_mask, 'preop_mbp'] = np.nan
    df_no.loc[bt_outlier_mask, 'preop_bt'] = np.nan

    df_no.to_csv(OUT_NO_IMPUTE, index=False)

    # 2) Imputed version.
    df_imp = df_no.copy()

    # Median imputation for other continuous variables.
    median_fill_values = {}
    median_imputed_counts = {}
    for c in CONTINUOUS_OTHERS:
        s = pd.to_numeric(df_imp[c], errors='coerce')
        miss_mask = s.isna()
        med = s.median(skipna=True)
        if pd.notna(med):
            s = s.fillna(med)
        df_imp[c] = s
        median_fill_values[c] = med
        median_imputed_counts[c] = int(miss_mask.sum())

    # MBP from SBP/DBP where MBP is missing.
    mbp_missing_before = df_imp['preop_mbp'].isna()
    mbp_calc = (df_imp['preop_sbp'] + 2.0 * df_imp['preop_dbp']) / 3.0
    df_imp.loc[mbp_missing_before, 'preop_mbp'] = mbp_calc[mbp_missing_before]

    # Optional provenance update for MBP source.
    if 'source_mbp' in df_imp.columns:
        df_imp.loc[mbp_missing_before & df_imp['preop_mbp'].notna(), 'source_mbp'] = 'Calculated_from_SBP_DBP'

    df_imp.to_csv(OUT_IMPUTED, index=False)

    summary_rows = []
    summary_rows.append({
        'step': 'outlier_to_na',
        'variable': 'preop_mbp',
        'count': int(mbp_outlier_mask.sum()),
        'detail': 'preop_mbp < 25 -> NA',
    })
    summary_rows.append({
        'step': 'outlier_to_na',
        'variable': 'preop_bt',
        'count': int(bt_outlier_mask.sum()),
        'detail': 'preop_bt < 30 -> NA',
    })

    for c in CONTINUOUS_OTHERS:
        summary_rows.append({
            'step': 'median_imputation',
            'variable': c,
            'count': median_imputed_counts[c],
            'detail': f'fill_value={median_fill_values[c]}',
        })

    summary_rows.append({
        'step': 'mbp_from_formula',
        'variable': 'preop_mbp',
        'count': int(mbp_missing_before.sum()),
        'detail': 'filled with (SBP + 2*DBP)/3 where MBP missing',
    })

    summary_df = pd.DataFrame(summary_rows)
    summary_df.to_csv(OUT_SUMMARY, index=False)

    # Console summary
    print(f'Input: {IN_FILE}')
    print(f'No-impute output: {OUT_NO_IMPUTE}')
    print(f'Imputed output:   {OUT_IMPUTED}')
    print(f'Summary:          {OUT_SUMMARY}')
    print(f'Rows/Cols:        {df.shape}')
    print('No-impute missing:')
    print(df_no[['preop_sbp','preop_dbp','preop_mbp','preop_hr','preop_spo2','preop_rr','preop_bt']].isna().sum().to_string())
    print('Imputed missing:')
    print(df_imp[['preop_sbp','preop_dbp','preop_mbp','preop_hr','preop_spo2','preop_rr','preop_bt']].isna().sum().to_string())


if __name__ == '__main__':
    main()
