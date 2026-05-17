#!/usr/bin/env python3
from pathlib import Path
import numpy as np
import pandas as pd

IN_COMORB = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/comorbidity_defined_latest.csv')
IN_ACUTE_PREOP = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_no_imputation/acute_status_preop_unrestricted_latest.csv')

OUT_CLEAN = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_outlier_removed/comorbidity_defined_latest.csv')
OUT_IMPUTED = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputed/comorbidity_defined_latest.csv')
OUT_SUMMARY = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/cleaned_rule_imputation_summary_comorbidity.csv')

ID_COLS = ['subject_id', 'hadm_id', 'op_id']
ACUTE_FLAGS = [
    'acute_myocardial_infarction', 'cerebral_infarction', 'cardiac_arrest', 'ards',
    'pulmonary_embolism', 'sepsis', 'pneumonia', 'shock',
    'ventilation', 'iabp', 'ecmo', 'oxygen_therapy'
]


def infer_types(df: pd.DataFrame):
    categorical = []
    continuous = []

    for c in df.columns:
        if c in ID_COLS:
            continue

        s = df[c]
        lc = c.lower()

        if s.dtype == 'O' or str(s.dtype).startswith('category') or s.dtype == bool:
            categorical.append(c)
            continue

        if np.issubdtype(s.dtype, np.number):
            nunique = s.nunique(dropna=True)
            # Explicit categorical name hints.
            if any(k in lc for k in ['category', 'stage', 'sex']) or nunique <= 10:
                categorical.append(c)
            else:
                continuous.append(c)
            continue

        categorical.append(c)

    return continuous, categorical


def impute(df: pd.DataFrame):
    out = df.copy()
    continuous, categorical = infer_types(out)
    summary = []

    for c in categorical:
        miss = out[c].isna()
        miss_n = int(miss.sum())
        mode = out[c].mode(dropna=True)
        fill = mode.iloc[0] if not mode.empty else np.nan
        if pd.notna(fill):
            out[c] = out[c].fillna(fill)
        out[f'{c}_imputed'] = miss.astype(np.int8)
        summary.append({
            'column': c,
            'variable_type': 'categorical',
            'impute_method': 'mode',
            'filled_n': miss_n,
            'fill_value': fill,
        })

    for c in continuous:
        s = pd.to_numeric(out[c], errors='coerce')
        miss = s.isna()
        miss_n = int(miss.sum())
        med = s.median(skipna=True)
        if pd.notna(med):
            s = s.fillna(med)
        out[c] = s
        out[f'{c}_imputed'] = miss.astype(np.int8)
        summary.append({
            'column': c,
            'variable_type': 'continuous',
            'impute_method': 'median',
            'filled_n': miss_n,
            'fill_value': med,
        })

    return out, pd.DataFrame(summary)


def main():
    OUT_CLEAN.parent.mkdir(parents=True, exist_ok=True)
    OUT_IMPUTED.parent.mkdir(parents=True, exist_ok=True)

    comorb = pd.read_csv(IN_COMORB, low_memory=False)
    acute = pd.read_csv(IN_ACUTE_PREOP, low_memory=False)

    missing_cols = [c for c in ACUTE_FLAGS if c not in acute.columns]
    if missing_cols:
        raise ValueError(f'Missing acute columns: {missing_cols}')

    acute_small = acute[ID_COLS + ACUTE_FLAGS].copy()
    merged = comorb.merge(acute_small, on=ID_COLS, how='left', validate='one_to_one')

    # Missing acute rows (e.g., missing/invalid timing in source) are treated as no event.
    for c in ACUTE_FLAGS:
        merged[c] = pd.to_numeric(merged[c], errors='coerce').fillna(0).astype(np.int8)

    merged['history_any_acute'] = merged[ACUTE_FLAGS].max(axis=1).astype(np.int8)

    merged.to_csv(OUT_CLEAN, index=False)

    merged_imp, summary = impute(merged)
    merged_imp.to_csv(OUT_IMPUTED, index=False)

    summary.insert(0, 'file', OUT_IMPUTED.name)
    summary.to_csv(OUT_SUMMARY, index=False)

    print('Wrote clean:', OUT_CLEAN)
    print('Wrote imputed:', OUT_IMPUTED)
    print('Wrote summary:', OUT_SUMMARY)
    print('Rows:', len(merged), 'Cols(clean):', merged.shape[1], 'Cols(imputed):', merged_imp.shape[1])


if __name__ == '__main__':
    main()
