#!/usr/bin/env python3
"""Resume-safe chunked export of 5-min grid wide intraop vitals from DuckDB staging tables."""
from pathlib import Path
import gc
import duckdb
import pandas as pd

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
DB = BASE / 'intraop_vitals_wide_5min_grid_20260517.duckdb'
OUT = BASE / 'intraop_vitals_wide_5min_grid_before_impute_no_calibration_all_ops.csv'
SUMMARY = BASE / 'intraop_vitals_wide_5min_grid_summary_20260517.csv'
CHUNK_OPS = 500
ITEMS = [
    'aft','air','alb20','alb5','art_dbp','art_mbp','art_sbp','bis','bt','cbro2','ci','cpat','cryo','cvp',
    'd10w','d50w','d5w','dobui','dopai','ds','ebl','eph','epi','epii','etco2','etdes','etgas','etiso','etsevo',
    'ffp','fio2','ftn','hes','hns','hr','hs','mdz','minvol','mlni','n2o','nepi','nibp_dbp','nibp_mbp','nibp_sbp',
    'ns','ntgi','o2','pap_dbp','pap_mbp','pap_sbp','pc','peep','pepi','phe','pheresis','pip','pmean','ppf',
    'ppfi','pplat','psa','rbc','rfti','rr','sft','spo2','sti','stii','stiii','stv5','svi','uo','vaso','vt'
]

def qident(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'

pivot_exprs = ',\n       '.join(f"max(CASE WHEN item_name = '{item}' THEN value END) AS {qident(item)}" for item in ITEMS)
select_items = ', '.join('o.' + qident(item) for item in ITEMS)

def connect():
    con = duckdb.connect(str(DB), read_only=True)
    con.execute("PRAGMA threads=4")
    con.execute("PRAGMA memory_limit='8GB'")
    return con

# Resume if a partial CSV exists.
resume_after = None
header = True
if OUT.exists() and OUT.stat().st_size > 0:
    try:
        con0 = duckdb.connect()
        resume_after = con0.execute(f"SELECT max(op_id) FROM read_csv_auto('{OUT}', header=true, columns={{'op_id':'BIGINT'}})").fetchone()[0]
        con0.close()
        header = False
        print(f'Resuming after op_id {resume_after}', flush=True)
    except Exception as e:
        print(f'Could not read partial output for resume ({e}); starting from scratch.', flush=True)
        OUT.unlink()
        resume_after = None
        header = True

con = connect()
if resume_after is None:
    opids = [r[0] for r in con.execute('SELECT op_id FROM win ORDER BY op_id').fetchall()]
else:
    opids = [r[0] for r in con.execute(f'SELECT op_id FROM win WHERE op_id > {int(resume_after)} ORDER BY op_id').fetchall()]
con.close()
print(f'Exporting remaining {len(opids)} ops in chunks of {CHUNK_OPS} ...', flush=True)

n_written = 0
for i in range(0, len(opids), CHUNK_OPS):
    lo = opids[i]
    hi = opids[min(i + CHUNK_OPS, len(opids)) - 1]
    con = connect()
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
    df = con.execute(query).fetch_df()
    con.close()
    df.to_csv(OUT, mode='a', header=header, index=False)
    header = False
    n_written += len(df)
    print(f'chunk {i//CHUNK_OPS + 1}: op_id {lo}-{hi}, rows {len(df)}, resumed_rows {n_written}', flush=True)
    del df
    gc.collect()

con = connect()
summary_query = """
WITH per_op AS (SELECT op_id, COUNT(*) AS n FROM grid GROUP BY op_id),
observed AS (SELECT COUNT(*) AS n_observed FROM (SELECT DISTINCT op_id, grid_min_from_entry FROM agg))
SELECT 'grid_step_min' AS metric, 5::DOUBLE AS value
UNION ALL SELECT 'n_rows', COUNT(*)::DOUBLE FROM grid
UNION ALL SELECT 'n_ops', COUNT(DISTINCT op_id)::DOUBLE FROM grid
UNION ALL SELECT 'n_value_columns', 74::DOUBLE
UNION ALL SELECT 'n_rows_with_any_observed_value', n_observed::DOUBLE FROM observed
UNION ALL SELECT 'median_grid_rows_per_op', median(n)::DOUBLE FROM per_op
UNION ALL SELECT 'p25_grid_rows_per_op', quantile_cont(n, 0.25)::DOUBLE FROM per_op
UNION ALL SELECT 'p75_grid_rows_per_op', quantile_cont(n, 0.75)::DOUBLE FROM per_op
"""
pd.DataFrame(con.execute(summary_query).fetchall(), columns=['metric','value']).to_csv(SUMMARY, index=False)
expected = con.execute('SELECT COUNT(*) FROM grid').fetchone()[0]
con.close()
print(f'Done. Output: {OUT}', flush=True)
print(f'Expected grid rows: {expected}', flush=True)
print(f'Summary: {SUMMARY}', flush=True)
