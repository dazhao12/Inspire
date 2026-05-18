#!/usr/bin/env python3
from pathlib import Path
import sys
import duckdb

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
DB = BASE / 'intraop_vitals_wide_observed_20260517.duckdb'
ITEMS = [
    'aft','air','alb20','alb5','art_dbp','art_mbp','art_sbp','bis','bt','cbro2','ci','cpat','cryo','cvp',
    'd10w','d50w','d5w','dobui','dopai','ds','ebl','eph','epi','epii','etco2','etdes','etgas','etiso','etsevo',
    'ffp','fio2','ftn','hes','hns','hr','hs','mdz','minvol','mlni','n2o','nepi','nibp_dbp','nibp_mbp','nibp_sbp',
    'ns','ntgi','o2','pap_dbp','pap_mbp','pap_sbp','pc','peep','pepi','phe','pheresis','pip','pmean','ppf',
    'ppfi','pplat','psa','rbc','rfti','rr','sft','spo2','sti','stii','stiii','stv5','svi','uo','vaso','vt'
]

def qident(name):
    return '"' + name.replace('"', '""') + '"'

lo = int(sys.argv[1])
hi = int(sys.argv[2])
out = Path(sys.argv[3])
pivot_exprs = ',\n       '.join("max(CASE WHEN item_name = '{}' THEN value END) AS {}".format(item, qident(item)) for item in ITEMS)
select_items = ', '.join('o.' + qident(item) for item in ITEMS)
query = """
WITH wide_obs AS (
  SELECT op_id, chart_time, {pivot_exprs}
  FROM obs_agg
  WHERE op_id BETWEEN {lo} AND {hi}
  GROUP BY op_id, chart_time
)
SELECT
  t.subject_id, t.hadm_id, t.op_id, t.case_id, t.chart_time, t.min_from_entry,
  t.vital_extract_start_time, t.vital_extract_end_time, t.time_qc_flag,
  {select_items}
FROM obs_time t
LEFT JOIN wide_obs o
  ON t.op_id = o.op_id AND t.chart_time = o.chart_time
WHERE t.op_id BETWEEN {lo} AND {hi}
ORDER BY t.subject_id, t.hadm_id, t.op_id, t.chart_time
""".format(pivot_exprs=pivot_exprs, lo=lo, hi=hi, select_items=select_items)
con = duckdb.connect(str(DB), read_only=True)
con.execute("PRAGMA threads=2")
con.execute("PRAGMA memory_limit='16GB'")
con.execute("PRAGMA temp_directory='/N/scratch/zz86/tmp'")
con.execute("COPY ({}) TO '{}' (HEADER, DELIMITER ',')".format(query, out))
con.close()
