#!/usr/bin/env python3
from pathlib import Path
import numpy as np
import pandas as pd

NO_IMPUTE_PATH = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_labs_attributable_90d_latest.csv')
IMPUTED_PATH = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputed/preop_labs_attributable_90d_latest.csv')
OUTLIER_REPORT = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/preop_labs_attributable_90d_outlier_check.csv')
IMPUTE_SUMMARY = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputation_summary_preop_labs_90d.csv')

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


def main():
    df = pd.read_csv(NO_IMPUTE_PATH, low_memory=False)
    nearest_cols = [c for c in df.columns if c.startswith('preop_') and c.endswith('_nearest')]
    nearest_cols = sorted(nearest_cols)

    out_no = df[ID_COLS + nearest_cols].copy()

    # Overwrite no-imputation file with nearest-only version.
    out_no.to_csv(NO_IMPUTE_PATH, index=False)

    # Outlier report on nearest-only columns.
    rows = []
    for c in nearest_cols:
        s = pd.to_numeric(out_no[c], errors='coerce')
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

    rep = pd.DataFrame(rows).sort_values(['n_negative', 'n_iqr_outlier', 'n_non_missing'], ascending=[False, False, False])
    rep.to_csv(OUTLIER_REPORT, index=False)

    # Median imputation nearest-only columns.
    imp = out_no.copy()
    summary_rows = []
    for c in nearest_cols:
        s = pd.to_numeric(imp[c], errors='coerce')
        miss = s.isna()
        miss_n = int(miss.sum())
        med = s.median(skipna=True)
        if pd.notna(med):
            s = s.fillna(med)
        imp[c] = s
        summary_rows.append({
            'file': IMPUTED_PATH.name,
            'column': c,
            'impute_method': 'median',
            'filled_n': miss_n,
            'fill_value': med,
        })

    imp.to_csv(IMPUTED_PATH, index=False)
    pd.DataFrame(summary_rows).to_csv(IMPUTE_SUMMARY, index=False)

    print('Nearest-only no-impute:', NO_IMPUTE_PATH)
    print('Nearest-only imputed:', IMPUTED_PATH)
    print('Outlier report:', OUTLIER_REPORT)
    print('Impute summary:', IMPUTE_SUMMARY)
    print('Shape no-impute:', out_no.shape)
    print('Shape imputed:', imp.shape)
    print('Remaining NA imputed (non-ID):', int(imp[nearest_cols].isna().sum().sum()))


if __name__ == '__main__':
    main()
