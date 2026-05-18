#!/usr/bin/env python3
from pathlib import Path
import sys
import duckdb
import pandas as pd

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
DB = BASE / 'intraop_vitals_wide_observed_20260517.duckdb'
ITEMS = [
    'aft','air','alb20','alb5','art_dbp','art_mbp','art_sbp','bis','bt','cbro2','ci','cpat','cryo','cvp',
    'd10w','d50w','d5w','dobui','dopai','ds','ebl','eph','epi','epii','etco2','etdes','etgas','etiso','etsevo',
    'ffp','fio2','ftn','hes','hns','hr','hs','mdz','minvol','mlni','n2o','nepi','nibp_dbp','nibp_mbp','nibp_sbp',
    'ns','ntgi','o2','pap_dbp','pap_mbp','pap_sbp','pc','peep','pepi','phe','pheresis','pip','pmean','ppf',
    'ppfi','pplat','psa','rbc','rfti','rr','sft','spo2','sti','stii','stiii','stv5','svi','uo','vaso','vt'
]
BASE_COLS = [
    'subject_id','hadm_id','op_id','case_id','chart_time','min_from_entry',
    'vital_extract_start_time','vital_extract_end_time','time_qc_flag'
]

lo = int(sys.argv[1])
hi = int(sys.argv[2])
out = Path(sys.argv[3])
con = duckdb.connect(str(DB), read_only=True)
con.execute("PRAGMA threads=2")
con.execute("PRAGMA memory_limit='8GB'")
con.execute("PRAGMA temp_directory='/N/scratch/zz86/tmp'")

times = con.execute("""
SELECT subject_id, hadm_id, op_id, case_id, chart_time, min_from_entry,
       vital_extract_start_time, vital_extract_end_time, time_qc_flag
FROM obs_time
WHERE op_id BETWEEN {lo} AND {hi}
""".format(lo=lo, hi=hi)).fetchdf()

agg = con.execute("""
SELECT op_id, chart_time, item_name, value
FROM obs_agg
WHERE op_id BETWEEN {lo} AND {hi}
""".format(lo=lo, hi=hi)).fetchdf()
con.close()

if times.empty:
    pd.DataFrame(columns=BASE_COLS + ITEMS).to_csv(out, index=False)
    raise SystemExit(0)

if agg.empty:
    wide = times.copy()
    for item in ITEMS:
        wide[item] = pd.NA
else:
    vals = agg.pivot(index=['op_id', 'chart_time'], columns='item_name', values='value').reset_index()
    vals.columns.name = None
    for item in ITEMS:
        if item not in vals.columns:
            vals[item] = pd.NA
    vals = vals[['op_id', 'chart_time'] + ITEMS]
    wide = times.merge(vals, on=['op_id', 'chart_time'], how='left')

wide = wide[BASE_COLS + ITEMS].sort_values(['subject_id', 'hadm_id', 'op_id', 'chart_time'])
wide.to_csv(out, index=False)
