#!/usr/bin/env python3
from pathlib import Path
import numpy as np
import pandas as pd

IN_PATH = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_labs_attributable_90d_latest.csv')
OUT_IMPUTED = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputed/preop_labs_attributable_90d_latest.csv')
OUT_OUTLIER_REPORT = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_labs_attributable_90d_outlier_check.csv')
OUT_IMPUTE_SUMMARY = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputation_summary_preop_labs_90d.csv')

ID_COLS = ['op_id', 'subject_id', 'hadm_id']


def iqr_outlier_stats(series: pd.Series):
    s = pd.to_numeric(series, errors='coerce')
    s = s[s.notna()]
    if s.empty:
        return (np.nan, np.nan, np.nan, np.nan, 0)
    q1 = s.quantile(0.25)
    q3 = s.quantile(0.75)
    iqr = q3 - q1
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    n_out = int(((s < lower) | (s > upper)).sum())
    return (q1, q3, lower, upper, n_out)


def build_outlier_report(df: pd.DataFrame):
    rows = []
    feature_cols = [c for c in df.columns if c not in ID_COLS]

    for c in feature_cols:
        s = pd.to_numeric(df[c], errors='coerce')
        non_missing = int(s.notna().sum())
        if non_missing == 0:
            rows.append({
                'column': c,
                'n_non_missing': 0,
                'min': np.nan,
                'max': np.nan,
                'n_negative': 0,
                'n_zero': 0,
                'q1': np.nan,
                'q3': np.nan,
                'iqr_lower': np.nan,
                'iqr_upper': np.nan,
                'n_iqr_outlier': 0,
            })
            continue

        q1, q3, lower, upper, n_iqr_out = iqr_outlier_stats(s)

        # Basic plausibility check:
        # most lab metrics should be non-negative; Base Excess (be) can be negative.
        allow_negative = ('_be_' in c)
        n_negative = int((s < 0).sum()) if not allow_negative else 0

        rows.append({
            'column': c,
            'n_non_missing': non_missing,
            'min': float(s.min()),
            'max': float(s.max()),
            'n_negative': n_negative,
            'n_zero': int((s == 0).sum()),
            'q1': float(q1) if pd.notna(q1) else np.nan,
            'q3': float(q3) if pd.notna(q3) else np.nan,
            'iqr_lower': float(lower) if pd.notna(lower) else np.nan,
            'iqr_upper': float(upper) if pd.notna(upper) else np.nan,
            'n_iqr_outlier': n_iqr_out,
        })

    rep = pd.DataFrame(rows)
    rep = rep.sort_values(['n_negative', 'n_iqr_outlier', 'n_non_missing'], ascending=[False, False, False])
    return rep


def build_imputed(df: pd.DataFrame):
    out = df.copy()
    summary_rows = []

    feature_cols = [c for c in out.columns if c not in ID_COLS]
    for c in feature_cols:
        s = pd.to_numeric(out[c], errors='coerce')
        miss_mask = s.isna()
        miss_n = int(miss_mask.sum())
        med = s.median(skipna=True)
        if pd.notna(med):
            s = s.fillna(med)
        out[c] = s
        summary_rows.append({
            'file': OUT_IMPUTED.name,
            'column': c,
            'impute_method': 'median',
            'filled_n': miss_n,
            'fill_value': med,
        })

    return out, pd.DataFrame(summary_rows)


def main():
    OUT_IMPUTED.parent.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(IN_PATH, low_memory=False)

    report = build_outlier_report(df)
    report.to_csv(OUT_OUTLIER_REPORT, index=False)

    df_imp, summary = build_imputed(df)
    df_imp.to_csv(OUT_IMPUTED, index=False)
    summary.to_csv(OUT_IMPUTE_SUMMARY, index=False)

    print(f'Input: {IN_PATH}')
    print(f'Outlier report: {OUT_OUTLIER_REPORT}')
    print(f'Imputed output: {OUT_IMPUTED}')
    print(f'Impute summary: {OUT_IMPUTE_SUMMARY}')
    print(f'Shape input: {df.shape}, shape imputed: {df_imp.shape}')
    print(f'Remaining NA in imputed (excluding IDs): {int(df_imp[[c for c in df_imp.columns if c not in ID_COLS]].isna().sum().sum())}')


if __name__ == '__main__':
    main()
