#!/usr/bin/env python3
"""
Timestamp: 2026-05-18
Clean and impute INSPIRE all-op 5-minute intraoperative vitals.

Input is the raw extracted 5-min wide table. This script does not modify the raw
extracted or before-clean/before-impute input table.

Outputs:
  1. clean_before_impute: outliers/artifacts set to NA, no imputation
  2. cleaning rule and QC tables
  3. clean_imputed_with_flags: no-NA clinical variables plus per-variable flags

Blood pressure follows the MOVER-style extraction/merge approach:
  - ART/IBP and NIBP are cleaned separately.
  - SBP valid range 30-300, MBP/MAP 20-250, DBP 10-200.
  - A valid triplet must satisfy SBP >= MBP >= DBP.
  - Merged BP uses valid ART triplet first; if no valid ART, use valid NIBP.
  - No calibrated NIBP is created.
"""

from __future__ import print_function
from pathlib import Path
from collections import defaultdict
import math

import duckdb
import numpy as np
import pandas as pd

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed')
INFILE = BASE / 'intraop_vitals_wide_5min_grid_before_impute_no_calibration_all_ops.csv'
CLEAN_OUT = BASE / 'intraop_vitals_wide_5min_grid_clean_before_impute_all_ops.csv'
IMPUTED_OUT = BASE / 'intraop_vitals_wide_5min_grid_clean_imputed_with_flags_all_ops.csv'
RULES_OUT = BASE / 'intraop_vitals_cleaning_imputation_rules_20260518.csv'
QC_OUT = BASE / 'intraop_vitals_clean_before_impute_qc_20260518.csv'
IMPUTE_SUMMARY_OUT = BASE / 'intraop_vitals_imputation_summary_20260518.csv'
GLOBAL_MEDIANS_OUT = BASE / 'intraop_vitals_imputation_global_medians_20260518.csv'

ID_COLS = [
    'subject_id', 'hadm_id', 'op_id', 'case_id', 'chart_time', 'grid_min_from_entry',
    'vital_extract_start_time', 'vital_extract_end_time', 'time_qc_flag'
]
ITEMS = [
    'aft','air','alb20','alb5','art_dbp','art_mbp','art_sbp','bis','bt','cbro2','ci','cpat','cryo','cvp',
    'd10w','d50w','d5w','dobui','dopai','ds','ebl','eph','epi','epii','etco2','etdes','etgas','etiso','etsevo',
    'ffp','fio2','ftn','hes','hns','hr','hs','mdz','minvol','mlni','n2o','nepi','nibp_dbp','nibp_mbp','nibp_sbp',
    'ns','ntgi','o2','pap_dbp','pap_mbp','pap_sbp','pc','peep','pepi','phe','pheresis','pip','pmean','ppf',
    'ppfi','pplat','psa','rbc','rfti','rr','sft','spo2','sti','stii','stiii','stv5','svi','uo','vaso','vt'
]
MERGED_BP = ['sbp_merged', 'mbp_merged', 'dbp_merged']
MERGED_SOURCE = 'bp_merged_source'
VALUE_COLS = ITEMS + MERGED_BP
FLAG_COLS = [v + '_imputed_flag' for v in VALUE_COLS]
CLEAN_COLS = ID_COLS + ITEMS + MERGED_BP + [MERGED_SOURCE]
IMPUTED_COLS = ID_COLS + ITEMS + MERGED_BP + [MERGED_SOURCE] + FLAG_COLS

# variable, group, strategy, unit, lower, upper, keep_zero, note
RULE_ROWS = [
    ('art_sbp','hemodynamics','art_triplet_monitor','mmHg',30,300,False,'MOVER BP SBP range; valid ART triplet requires SBP>=MBP>=DBP'),
    ('art_mbp','hemodynamics','art_triplet_monitor','mmHg',20,250,False,'MOVER BP MAP/MBP range; valid ART triplet requires SBP>=MBP>=DBP'),
    ('art_dbp','hemodynamics','art_triplet_monitor','mmHg',10,200,False,'MOVER BP DBP range; valid ART triplet requires SBP>=MBP>=DBP'),
    ('nibp_sbp','hemodynamics','nibp_triplet_monitor','mmHg',30,300,False,'MOVER BP SBP range; valid NIBP triplet requires SBP>=MBP>=DBP'),
    ('nibp_mbp','hemodynamics','nibp_triplet_monitor','mmHg',20,250,False,'MOVER BP MAP/MBP range; valid NIBP triplet requires SBP>=MBP>=DBP'),
    ('nibp_dbp','hemodynamics','nibp_triplet_monitor','mmHg',10,200,False,'MOVER BP DBP range; valid NIBP triplet requires SBP>=MBP>=DBP'),
    ('pap_sbp','hemodynamics','pap_triplet_monitor','mmHg',30,300,False,'MOVER BP SBP range; valid PAP triplet requires SBP>=MBP>=DBP'),
    ('pap_mbp','hemodynamics','pap_triplet_monitor','mmHg',20,250,False,'MOVER BP MAP/MBP range; valid PAP triplet requires SBP>=MBP>=DBP'),
    ('pap_dbp','hemodynamics','pap_triplet_monitor','mmHg',10,200,False,'MOVER BP DBP range; valid PAP triplet requires SBP>=MBP>=DBP'),
    ('hr','hemodynamics','continuous_monitor','/min',20,220,False,''),
    ('cvp','hemodynamics','continuous_monitor','mmHg',-5,40,True,''),
    ('ci','hemodynamics','continuous_monitor','L/min/m2',0.5,10,False,''),
    ('svi','hemodynamics','continuous_monitor','mL/m2',5,150,False,''),
    ('bt','hemodynamics','continuous_monitor','Celsius',30,43,False,'No CPB exception in this all-op script'),
    ('rr','respiratory_ventilation','continuous_monitor','/min',2,60,False,''),
    ('spo2','respiratory_ventilation','continuous_monitor','%',30,100,False,''),
    ('etco2','respiratory_ventilation','continuous_monitor','mmHg',5,100,False,''),
    ('fio2','respiratory_ventilation','continuous_monitor','%',21,100,False,''),
    ('vt','respiratory_ventilation','continuous_monitor','mL',10,3000,False,''),
    ('minvol','respiratory_ventilation','continuous_monitor','L/min',0.1,60,False,''),
    ('o2','respiratory_ventilation','continuous_monitor','L/min',0,15,True,''),
    ('air','respiratory_ventilation','continuous_monitor','L/min',0,15,True,''),
    ('peep','respiratory_ventilation','continuous_monitor','cmH2O',0,30,True,''),
    ('pip','respiratory_ventilation','continuous_monitor','cmH2O',0,60,True,'pip=1 set NA as artifact'),
    ('pmean','respiratory_ventilation','continuous_monitor','cmH2O',0,60,True,'pmean=0/1/3 set NA as artifact'),
    ('pplat','respiratory_ventilation','continuous_monitor','cmH2O',0,60,True,''),
    ('cbro2','respiratory_ventilation','continuous_monitor','%',15,100,False,''),
    ('bis','anesthesia_sedation','continuous_monitor','',0,100,True,''),
    ('etgas','anesthesia_sedation','continuous_monitor','vol%',0,10,True,''),
    ('etdes','anesthesia_sedation','continuous_monitor','vol%',0,18,True,''),
    ('etiso','anesthesia_sedation','continuous_monitor','vol%',0,5,True,''),
    ('etsevo','anesthesia_sedation','continuous_monitor','vol%',0,8,True,''),
    ('n2o','anesthesia_sedation','continuous_monitor','L/min',0,15,True,''),
    ('ppfi','anesthesia_sedation','tci_state','ug/mL',2.5,10,True,''),
    ('rfti','anesthesia_sedation','tci_state','ng/mL',0.5,20,True,''),
    ('ppf','anesthesia_sedation','event_zero_fill','mg',0,1000,True,''),
    ('ftn','anesthesia_sedation','event_zero_fill','ug',0,5000,True,''),
    ('aft','anesthesia_sedation','event_zero_fill','ug',0,10000,True,''),
    ('sft','anesthesia_sedation','event_zero_fill','ug',0,1000,True,''),
    ('mdz','anesthesia_sedation','event_zero_fill','mg',0,50,True,''),
    ('epi','vasoactive_drugs','event_zero_fill','ug',0,5000,True,''),
    ('phe','vasoactive_drugs','event_zero_fill','ug',0,5000,True,''),
    ('eph','vasoactive_drugs','event_zero_fill','mg',0,200,True,''),
    ('vaso','vasoactive_drugs','event_zero_fill','Unit',0,100,True,''),
    ('epii','vasoactive_drugs','infusion_state_cap60','ug/kg/min',0.01,5,True,''),
    ('nepi','vasoactive_drugs','infusion_state_cap60','ug/kg/min',0.005,3,True,''),
    ('pepi','vasoactive_drugs','infusion_state_cap60','ug/h',10,3000,True,'pepi=0.25 or 10 set NA as artifact unless explicit 0 stop'),
    ('dopai','vasoactive_drugs','infusion_state_cap60','ug/kg/min',1.5,30,True,''),
    ('dobui','vasoactive_drugs','infusion_state_cap60','ug/kg/min',2,30,True,''),
    ('mlni','vasoactive_drugs','infusion_state_cap60','ug/kg/min',0.25,10,True,'mlni=9.93 or 39.325 set NA as artifact'),
    ('ntgi','vasoactive_drugs','infusion_state_cap60','ug/kg/min',0.05,10,True,''),
    ('ebl','fluids_output','event_zero_fill','mL',0,10000,True,''),
    ('uo','fluids_output','event_zero_fill','mL',0,10000,True,''),
    ('ns','fluids_output','event_zero_fill','mL',0,10000,True,''),
    ('hns','fluids_output','event_zero_fill','mL',0,10000,True,''),
    ('hs','fluids_output','event_zero_fill','mL',0,10000,True,''),
    ('psa','fluids_output','event_zero_fill','mL',0,10000,True,''),
    ('d5w','fluids_output','event_zero_fill','mL',0,5000,True,''),
    ('d10w','fluids_output','event_zero_fill','mL',0,5000,True,''),
    ('d50w','fluids_output','event_zero_fill','mL',0,5000,True,''),
    ('hes','fluids_output','event_zero_fill','mL',0,10000,True,''),
    ('alb5','fluids_output','event_zero_fill','mL',0,5000,True,''),
    ('alb20','fluids_output','event_zero_fill','mL',0,5000,True,''),
    ('rbc','blood_products','event_zero_fill','Unit',0,20,True,''),
    ('ffp','blood_products','event_zero_fill','Unit',0,20,True,''),
    ('pc','blood_products','event_zero_fill','Unit',0,20,True,''),
    ('pheresis','blood_products','event_zero_fill','Unit',0,20,True,''),
    ('cryo','blood_products','event_zero_fill','Unit',0,20,True,''),
    ('sti','ecg_st_segment','continuous_monitor','mV',-10,10,True,''),
    ('stii','ecg_st_segment','continuous_monitor','mV',-10,10,True,''),
    ('stiii','ecg_st_segment','continuous_monitor','mV',-10,10,True,''),
    ('stv5','ecg_st_segment','continuous_monitor','mV',-10,10,True,''),
    ('cpat','review_unmapped','global_median_fill','raw_source_unit',0,1000,True,''),
    ('ds','review_unmapped','event_zero_fill','mL',0,10000,True,''),
]
RULES = {}
for r in RULE_ROWS:
    RULES[r[0]] = dict(variable=r[0], column_group=r[1], strategy=r[2], unit=r[3], lower=float(r[4]), upper=float(r[5]), keep_zero=bool(r[6]), note=r[7])
for v in MERGED_BP:
    RULES[v] = dict(variable=v, column_group='hemodynamics', strategy='merged_bp_triplet_monitor', unit='mmHg', lower=30.0 if v == 'sbp_merged' else 20.0 if v == 'mbp_merged' else 10.0, upper=300.0 if v == 'sbp_merged' else 250.0 if v == 'mbp_merged' else 200.0, keep_zero=False, note='ART-first valid triplet, NIBP fallback; no calibrated NIBP')

BP_GROUPS = {
    'art': ['art_sbp','art_mbp','art_dbp'],
    'nibp': ['nibp_sbp','nibp_mbp','nibp_dbp'],
    'pap': ['pap_sbp','pap_mbp','pap_dbp'],
    'merged': ['sbp_merged','mbp_merged','dbp_merged'],
}
BP_VALUE_COLS = set(['art_sbp','art_mbp','art_dbp','nibp_sbp','nibp_mbp','nibp_dbp','pap_sbp','pap_mbp','pap_dbp','sbp_merged','mbp_merged','dbp_merged'])


def to_num(s):
    return pd.to_numeric(s, errors='coerce')


def valid_triplet(df, cols):
    sbp, mbp, dbp = cols
    return df[sbp].notna() & df[mbp].notna() & df[dbp].notna() & (df[sbp] >= df[mbp]) & (df[mbp] >= df[dbp])


def imputation_rule_text(strategy):
    if strategy in ['continuous_monitor', 'art_triplet_monitor', 'nibp_triplet_monitor', 'pap_triplet_monitor', 'merged_bp_triplet_monitor']:
        return 'Within op_id: before first observed use global median; after first observed use LOCF only; no backward fill/future interpolation.'
    if strategy == 'event_zero_fill':
        return 'After threshold cleaning, missing values filled with 0.'
    if strategy == 'infusion_state_cap60':
        return 'Before first positive fill 0; observed 0 stops infusion; positive value carried forward up to 60 min, then 0.'
    if strategy == 'tci_state':
        return 'Before first positive fill 0; observed 0 stays 0; positive/zero state carried forward to end of case.'
    if strategy == 'global_median_fill':
        return 'Missing values filled with global median.'
    return ''


def write_rules():
    rows = []
    for v in VALUE_COLS:
        r = RULES[v]
        rows.append(dict(
            variable=v, column_group=r['column_group'], strategy=r['strategy'], unit=r['unit'],
            lower=r['lower'], upper=r['upper'], keep_zero=r['keep_zero'],
            cleaning_rule='keep explicit 0 or values in range' if r['keep_zero'] else 'keep values in range only',
            imputation_rule=imputation_rule_text(r['strategy']), note=r['note']
        ))
    pd.DataFrame(rows).to_csv(RULES_OUT, index=False)


def clean_chunk(df, qc):
    for c in ITEMS:
        if c not in df.columns:
            df[c] = np.nan
        df[c] = to_num(df[c])
    n_rows = len(df)
    for v in ITEMS:
        r = RULES[v]
        x = df[v]
        nonmiss = x.notna()
        low = nonmiss & (x < r['lower']) if not r['keep_zero'] else nonmiss & (x != 0) & (x < r['lower'])
        high = nonmiss & (x > r['upper'])
        special = pd.Series(False, index=df.index)
        if v == 'pip':
            special = nonmiss & (x == 1)
        elif v == 'pmean':
            special = nonmiss & (x.isin([0, 1, 3]))
        elif v == 'pepi':
            special = nonmiss & (x.isin([0.25, 10]))
        elif v == 'mlni':
            special = nonmiss & (x.isin([9.93, 39.325]))
        invalid = low | high | special
        if invalid.any():
            df.loc[invalid, v] = np.nan
        qc[v]['n_rows'] += n_rows
        qc[v]['n_nonmissing_raw'] += int(nonmiss.sum())
        qc[v]['n_below_range_to_na'] += int(low.sum())
        qc[v]['n_above_range_to_na'] += int(high.sum())
        qc[v]['n_special_artifact_to_na'] += int(special.sum())
        qc[v]['n_total_to_na_before_triplet'] += int(invalid.sum())

    for prefix in ['art', 'nibp', 'pap']:
        cols = BP_GROUPS[prefix]
        full = df[cols[0]].notna() & df[cols[1]].notna() & df[cols[2]].notna()
        conflict = full & (~valid_triplet(df, cols))
        if conflict.any():
            df.loc[conflict, cols] = np.nan
        for v in cols:
            qc[v]['n_triplet_conflict_to_na'] += int(conflict.sum())

    art_valid = valid_triplet(df, BP_GROUPS['art'])
    nibp_valid = valid_triplet(df, BP_GROUPS['nibp'])
    df['sbp_merged'] = np.where(art_valid, df['art_sbp'], np.where(nibp_valid, df['nibp_sbp'], np.nan))
    df['mbp_merged'] = np.where(art_valid, df['art_mbp'], np.where(nibp_valid, df['nibp_mbp'], np.nan))
    df['dbp_merged'] = np.where(art_valid, df['art_dbp'], np.where(nibp_valid, df['nibp_dbp'], np.nan))
    df[MERGED_SOURCE] = np.where(art_valid, 'ART', np.where(nibp_valid, 'NIBP', ''))
    return df[CLEAN_COLS]


def build_clean(chunksize=200000):
    if CLEAN_OUT.exists():
        CLEAN_OUT.unlink()
    qc = defaultdict(lambda: defaultdict(int))
    first = True
    total = 0
    for i, chunk in enumerate(pd.read_csv(INFILE, chunksize=chunksize, low_memory=False), start=1):
        cleaned = clean_chunk(chunk, qc)
        cleaned.to_csv(CLEAN_OUT, mode='w' if first else 'a', header=first, index=False)
        first = False
        total += len(cleaned)
        print('clean chunk {} rows={} total={}'.format(i, len(cleaned), total), flush=True)
    rows = []
    for v in ITEMS:
        q = qc[v]
        total_to_na = q['n_total_to_na_before_triplet'] + q['n_triplet_conflict_to_na']
        r = RULES[v]
        rows.append(dict(
            variable=v, n_rows=q['n_rows'], n_nonmissing_raw=q['n_nonmissing_raw'],
            n_below_range_to_na=q['n_below_range_to_na'], n_above_range_to_na=q['n_above_range_to_na'],
            n_special_artifact_to_na=q['n_special_artifact_to_na'], n_triplet_conflict_to_na=q['n_triplet_conflict_to_na'],
            n_total_to_na=total_to_na,
            pct_nonmissing_set_to_na=round(100.0 * total_to_na / q['n_nonmissing_raw'], 6) if q['n_nonmissing_raw'] else 0.0,
            strategy=r['strategy'], lower=r['lower'], upper=r['upper'], keep_zero=r['keep_zero']
        ))
    pd.DataFrame(rows).to_csv(QC_OUT, index=False)


def qident(c):
    return '"' + c.replace('"', '""') + '"'


def compute_medians():
    con = duckdb.connect()
    con.execute('PRAGMA threads=8')
    exprs = ['median(try_cast({} as double)) as {}'.format(qident(c), qident(c)) for c in VALUE_COLS]
    row = con.execute("SELECT {} FROM read_csv_auto('{}', header=true)".format(', '.join(exprs), CLEAN_OUT)).fetchone()
    con.close()
    med = {}
    for c, val in zip(VALUE_COLS, row):
        if val is None or (isinstance(val, float) and math.isnan(val)):
            val = 0.0
        med[c] = float(val)
    for cols in BP_GROUPS.values():
        if all(c in med for c in cols):
            sbp, mbp, dbp = [med[c] for c in cols]
            sbp2 = max(sbp, mbp, dbp)
            dbp2 = min(sbp, mbp, dbp)
            mbp2 = min(sbp2, max(mbp, dbp2))
            med[cols[0]], med[cols[1]], med[cols[2]] = sbp2, mbp2, dbp2
    pd.DataFrame([dict(variable=c, global_median_after_cleaning=med[c]) for c in VALUE_COLS]).to_csv(GLOBAL_MEDIANS_OUT, index=False)
    return med


def continuous_fill(x, fill_value):
    arr = np.asarray(x, dtype=float)
    flag = pd.isna(arr).astype(int)
    out = arr.copy()
    obs = np.where(~pd.isna(out))[0]
    if len(obs) == 0:
        return np.full(len(out), fill_value), np.ones(len(out), dtype=int)
    first = obs[0]
    if first > 0:
        out[:first] = fill_value
    last = out[first]
    for i in range(first + 1, len(out)):
        if pd.isna(out[i]):
            out[i] = last
        else:
            last = out[i]
    return out, flag


def event_zero_fill(x):
    arr = np.asarray(x, dtype=float)
    flag = pd.isna(arr).astype(int)
    out = arr.copy()
    out[pd.isna(out)] = 0.0
    return out, flag


def tci_fill(x):
    arr = np.asarray(x, dtype=float)
    out = np.zeros(len(arr), dtype=float)
    flag = np.ones(len(arr), dtype=int)
    started = False
    last = 0.0
    for i, val in enumerate(arr):
        if not pd.isna(val):
            out[i] = val
            flag[i] = 0
            if val > 0:
                started = True
            last = val
        elif not started:
            out[i] = 0.0
        else:
            out[i] = last
    return out, flag


def infusion_cap60_fill(x, t):
    arr = np.asarray(x, dtype=float)
    time = np.asarray(t, dtype=float)
    out = np.zeros(len(arr), dtype=float)
    flag = np.ones(len(arr), dtype=int)
    started = False
    last = 0.0
    last_time = np.nan
    for i, val in enumerate(arr):
        ti = time[i]
        if not pd.isna(val):
            out[i] = val
            flag[i] = 0
            if val > 0:
                started = True
            last = val
            last_time = ti
        elif not started:
            out[i] = 0.0
        elif last == 0:
            out[i] = 0.0
        elif (not pd.isna(last_time)) and (not pd.isna(ti)) and ((ti - last_time) <= 60.0):
            out[i] = last
        else:
            out[i] = 0.0
    return out, flag


def triplet_fill(g, cols, med):
    n = len(g)
    sbp, mbp, dbp = cols
    valid = g[sbp].notna() & g[mbp].notna() & g[dbp].notna() & (g[sbp] >= g[mbp]) & (g[mbp] >= g[dbp])
    out = pd.DataFrame(index=g.index)
    flag = np.ones(n, dtype=int)
    global_vals = [med[sbp], med[mbp], med[dbp]]
    obs_idx = np.where(valid.to_numpy())[0]
    if len(obs_idx) == 0:
        out[sbp] = global_vals[0]; out[mbp] = global_vals[1]; out[dbp] = global_vals[2]
        return out, flag
    first = obs_idx[0]
    current = list(global_vals)
    for pos in range(n):
        if valid.iloc[pos]:
            current = [float(g[sbp].iloc[pos]), float(g[mbp].iloc[pos]), float(g[dbp].iloc[pos])]
            flag[pos] = 0
        elif pos < first:
            current = list(global_vals)
        out.loc[g.index[pos], sbp] = current[0]
        out.loc[g.index[pos], mbp] = current[1]
        out.loc[g.index[pos], dbp] = current[2]
    return out, flag


def impute_group(g, med, summary):
    g = g.sort_values('grid_min_from_entry').copy()
    for c in VALUE_COLS:
        g[c] = to_num(g[c])
    processed = set()
    for prefix, cols in BP_GROUPS.items():
        out, flag = triplet_fill(g, cols, med)
        for c in cols:
            g[c] = out[c].to_numpy(dtype=float)
            g[c + '_imputed_flag'] = flag
            processed.add(c)
        if prefix == 'merged':
            raw_src = list(g[MERGED_SOURCE].fillna('').astype(str))
            out_src = ['GLOBAL_MEDIAN'] * len(g)
            obs = np.where(flag == 0)[0]
            if len(obs) > 0:
                first = obs[0]
                last_src = 'GLOBAL_MEDIAN'
                for i in range(len(g)):
                    if flag[i] == 0:
                        last_src = raw_src[i] if raw_src[i] else 'OBSERVED'
                        out_src[i] = last_src
                    elif i < first:
                        out_src[i] = 'GLOBAL_MEDIAN'
                    else:
                        out_src[i] = 'LOCF'
            g[MERGED_SOURCE] = out_src
    time = to_num(g['grid_min_from_entry']).to_numpy(dtype=float)
    for c in VALUE_COLS:
        if c in processed:
            continue
        strat = RULES[c]['strategy']
        x = to_num(g[c]).to_numpy(dtype=float)
        if strat == 'continuous_monitor':
            out, flag = continuous_fill(x, med[c])
        elif strat == 'event_zero_fill':
            out, flag = event_zero_fill(x)
        elif strat == 'infusion_state_cap60':
            out, flag = infusion_cap60_fill(x, time)
        elif strat == 'tci_state':
            out, flag = tci_fill(x)
        elif strat == 'global_median_fill':
            out, flag = continuous_fill(x, med[c])
        else:
            out, flag = continuous_fill(x, med[c])
        g[c] = out
        g[c + '_imputed_flag'] = flag
    for c in VALUE_COLS:
        f = c + '_imputed_flag'
        summary[c]['n_rows'] += len(g)
        summary[c]['n_observed_after_clean'] += int((g[f] == 0).sum())
        summary[c]['n_imputed'] += int(g[f].sum())
        summary[c]['n_na_after_impute'] += int(pd.isna(g[c]).sum())
    return g[IMPUTED_COLS]


def iter_groups(csv_path, chunksize=120000):
    buf = None
    for chunk in pd.read_csv(csv_path, chunksize=chunksize, low_memory=False):
        if buf is not None:
            chunk = pd.concat([buf, chunk], ignore_index=True)
        last_op = chunk['op_id'].iloc[-1]
        complete = chunk[chunk['op_id'] != last_op]
        buf = chunk[chunk['op_id'] == last_op]
        if not complete.empty:
            for _, g in complete.groupby('op_id', sort=False):
                yield g
    if buf is not None and not buf.empty:
        for _, g in buf.groupby('op_id', sort=False):
            yield g


def build_imputed(med):
    if IMPUTED_OUT.exists():
        IMPUTED_OUT.unlink()
    first = True
    total = 0
    summary = defaultdict(lambda: defaultdict(int))
    batch = []
    for i, g in enumerate(iter_groups(CLEAN_OUT), start=1):
        batch.append(impute_group(g, med, summary))
        if len(batch) >= 250:
            out = pd.concat(batch, ignore_index=True)
            out.to_csv(IMPUTED_OUT, mode='w' if first else 'a', header=first, index=False)
            first = False
            total += len(out)
            batch = []
            if i % 5000 == 0:
                print('impute groups={} rows_written={}'.format(i, total), flush=True)
    if batch:
        out = pd.concat(batch, ignore_index=True)
        out.to_csv(IMPUTED_OUT, mode='w' if first else 'a', header=first, index=False)
        total += len(out)
    rows = []
    for c in VALUE_COLS:
        s = summary[c]
        rows.append(dict(variable=c, n_rows=s['n_rows'], n_observed_after_clean=s['n_observed_after_clean'], n_imputed=s['n_imputed'], pct_imputed=round(100.0 * s['n_imputed'] / s['n_rows'], 6) if s['n_rows'] else 0.0, n_na_after_impute=s['n_na_after_impute'], strategy=RULES[c]['strategy']))
    pd.DataFrame(rows).to_csv(IMPUTE_SUMMARY_OUT, index=False)
    print('imputed rows_written={}'.format(total), flush=True)


def validate():
    con = duckdb.connect()
    clean_rows = con.execute("SELECT COUNT(*) FROM read_csv_auto('{}', header=true)".format(CLEAN_OUT)).fetchone()[0]
    imp_rows = con.execute("SELECT COUNT(*) FROM read_csv_auto('{}', header=true)".format(IMPUTED_OUT)).fetchone()[0]
    na_expr = ' + '.join(['sum(case when try_cast({} as double) is null then 1 else 0 end)'.format(qident(c)) for c in VALUE_COLS])
    final_na = con.execute("SELECT {} FROM read_csv_auto('{}', header=true)".format(na_expr, IMPUTED_OUT)).fetchone()[0]
    con.close()
    print('validation clean_rows={}'.format(clean_rows), flush=True)
    print('validation imputed_rows={}'.format(imp_rows), flush=True)
    print('validation final_clinical_na_count={}'.format(final_na), flush=True)


def main():
    if not INFILE.exists():
        raise SystemExit('Input file not found: {}'.format(INFILE))
    write_rules()
    print('rules: {}'.format(RULES_OUT), flush=True)
    build_clean()
    med = compute_medians()
    print('global medians: {}'.format(GLOBAL_MEDIANS_OUT), flush=True)
    # The original row-wise Python imputer is retained above for transparency, but
    # the production path uses the vectorized DuckDB implementation. It is much
    # faster and avoids long-running per-operation loops.
    import subprocess
    import sys
    fast_script = Path('/N/project/analgesia_perioperation/projects/data_process_ZZ/Inspire/final_code/build_intraop_vitals_imputed_duckdb_fast_20260518.py')
    subprocess.run([sys.executable, str(fast_script)], check=True)
    print('clean output: {}'.format(CLEAN_OUT), flush=True)
    print('imputed output: {}'.format(BASE / 'intraop_vitals_wide_5min_grid_clean_imputed_with_flags_all_ops_duckdb_fast.csv'), flush=True)
    print('clean QC: {}'.format(QC_OUT), flush=True)
    print('imputation summary: {}'.format(BASE / 'intraop_vitals_imputation_summary_duckdb_fast_20260518.csv'), flush=True)


if __name__ == '__main__':
    main()
