#!/usr/bin/env python3
from pathlib import Path
import sys
import duckdb

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
DB = BASE / 'intraop_vitals_wide_5min_grid_20260517.duckdb'
ITEMS = [
    'aft','air','alb20','alb5','art_dbp','art_mbp','art_sbp','bis','bt','cbro2','ci','cpat','cryo','cvp',
    'd10w','d50w','d5w','dobui','dopai','ds','ebl','eph','epi','epii','etco2','etdes','etgas','etiso','etsevo',
    'ffp','fio2','ftn','hes','hns','hr','hs','mdz','minvol','mlni','n2o','nepi','nibp_dbp','nibp_mbp','nibp_sbp',
    'ns','ntgi','o2','pap_dbp','pap_mbp','pap_sbp','pc','peep','pepi','phe','pheresis','pip','pmean','ppf',
    'ppfi','pplat','psa','rbc','rfti','rr','sft','spo2','sti','stii','stiii','stv5','svi','uo','vaso','vt'
]

def qident(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'

lo = int(sys.argv[1]); hi = int(sys.argv[2]); out = Path(sys.argv[3])
pivot_exprs = ',\n       '.join(f"max(CASE WHEN item_name = '{item}' THEN value END) AS {qident(item)}" for item in ITEMS)
select_items = ', '.join('o.' + qident(item) for item in ITEMS)
query = f"""
WITH wide_obs AS (
  SELECT op_id, grid_min_from_entry, {pivot_exprs}
  FROM agg
  WHERE op_id BETWEEN {lo} AND {hi}
  GROUP BY op_id, grid_min_from_entry
)
SELECT
  g.subject_id, g.hadm_id, g.op_id, g.case_id, g.chart_time, g.grid_min_from_entry,
  g.vital_extract_start_time, g.vital_extract_end_time, g.time_qc_flag,
  {select_items}
FROM grid g
LEFT JOIN wide_obs o
  ON g.op_id = o.op_id AND g.grid_min_from_entry = o.grid_min_from_entry
WHERE g.op_id BETWEEN {lo} AND {hi}
ORDER BY g.subject_id, g.hadm_id, g.op_id, g.grid_min_from_entry
"""
con = duckdb.connect(str(DB), read_only=True)
con.execute("PRAGMA threads=2")
con.execute("PRAGMA memory_limit='4GB'")
con.execute(f"COPY ({query}) TO '{out}' (HEADER, DELIMITER ',')")
con.close()
