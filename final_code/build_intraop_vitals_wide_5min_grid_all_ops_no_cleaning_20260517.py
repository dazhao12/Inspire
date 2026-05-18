#!/usr/bin/env python3
"""
Timestamp: 2026-05-17T17:25:00Z
Build intraoperative vitals as a complete 5-minute relative-time wide grid.
Keep only operations with at least one raw vital observation inside the extraction window.
Reshape/resampling only: no outlier cleaning, calibration, or imputation.
"""
from pathlib import Path
import duckdb
import pandas as pd

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed')
LONG = BASE / 'intraop_vitals_clean_before_impute_no_calibration_all_op_extracted.csv'
QC = BASE / 'intraop_vitals_extract_window_qc_20260517.csv'
OUT = BASE / 'intraop_vitals_wide_5min_grid_before_impute_no_calibration_all_ops.csv'
SUMMARY = BASE / 'intraop_vitals_wide_5min_grid_summary_20260517.csv'
TMPDB = BASE / 'intraop_vitals_wide_5min_grid_20260517.duckdb'
GRID_STEP = 5

# Fixed item order from the extracted long table summary / legacy target_items.
ITEMS = [
    'aft','air','alb20','alb5','art_dbp','art_mbp','art_sbp','bis','bt','cbro2','ci','cpat','cryo','cvp',
    'd10w','d50w','d5w','dobui','dopai','ds','ebl','eph','epi','epii','etco2','etdes','etgas','etiso','etsevo',
    'ffp','fio2','ftn','hes','hns','hr','hs','mdz','minvol','mlni','n2o','nepi','nibp_dbp','nibp_mbp','nibp_sbp',
    'ns','ntgi','o2','pap_dbp','pap_mbp','pap_sbp','pc','peep','pepi','phe','pheresis','pip','pmean','ppf',
    'ppfi','pplat','psa','rbc','rfti','rr','sft','spo2','sti','stii','stiii','stv5','svi','uo','vaso','vt'
]

def qident(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'

pivot_exprs = ',\n       '.join(
    f"max(CASE WHEN item_name = '{item}' THEN value END) AS {qident(item)}" for item in ITEMS
)
observed_expr = ' + '.join(f"CASE WHEN {qident(item)} IS NOT NULL THEN 1 ELSE 0 END" for item in ITEMS)

if TMPDB.exists():
    TMPDB.unlink()

con = duckdb.connect(str(TMPDB))
con.execute("PRAGMA threads=8")
con.execute("PRAGMA memory_limit='24GB'")
con.execute("PRAGMA temp_directory='/N/scratch/zz86/tmp'")

print('Loading window QC into DuckDB ...', flush=True)
con.execute(f"""
CREATE TABLE win AS
SELECT
  CAST(op_id AS BIGINT) AS op_id,
  CAST(subject_id AS BIGINT) AS subject_id,
  CAST(hadm_id AS BIGINT) AS hadm_id,
  TRY_CAST(case_id AS DOUBLE) AS case_id,
  CAST(vital_extract_start_time AS DOUBLE) AS vital_extract_start_time,
  CAST(vital_extract_end_time AS DOUBLE) AS vital_extract_end_time,
  time_qc_flag,
  FLOOR((CAST(vital_extract_end_time AS DOUBLE) - CAST(vital_extract_start_time AS DOUBLE)) / {GRID_STEP}) * {GRID_STEP} AS max_grid_min_from_entry
FROM read_csv_auto('{QC}', header=true)
WHERE extract_window_valid = true
  AND vital_extract_start_time IS NOT NULL
  AND vital_extract_end_time IS NOT NULL
  AND CAST(vital_extract_end_time AS DOUBLE) >= CAST(vital_extract_start_time AS DOUBLE)
""")

print('Building complete 5-minute grid ...', flush=True)
max_grid = con.execute('SELECT CAST(MAX(max_grid_min_from_entry) AS BIGINT) FROM win').fetchone()[0]
con.execute(f"""
CREATE TABLE nums AS
SELECT CAST(range * {GRID_STEP} AS DOUBLE) AS grid_min_from_entry
FROM range(0, CAST(({max_grid} / {GRID_STEP}) + 1 AS BIGINT))
""")
con.execute("""
CREATE TABLE grid AS
SELECT
  w.subject_id,
  w.hadm_id,
  w.op_id,
  w.case_id,
  CAST(w.vital_extract_start_time + n.grid_min_from_entry AS DOUBLE) AS chart_time,
  CAST(n.grid_min_from_entry AS DOUBLE) AS grid_min_from_entry,
  w.vital_extract_start_time,
  w.vital_extract_end_time,
  w.time_qc_flag
FROM win w
JOIN nums n ON n.grid_min_from_entry <= w.max_grid_min_from_entry
""")

print('Reading long vitals, assigning nearest 5-minute bins, and aggregating ...', flush=True)
con.execute(f"""
CREATE TABLE agg AS
SELECT
  l.op_id,
  LEAST(
    GREATEST(ROUND((CAST(l.chart_time AS DOUBLE) - w.vital_extract_start_time) / {GRID_STEP}) * {GRID_STEP}, 0),
    w.max_grid_min_from_entry
  ) AS grid_min_from_entry,
  l.item_name,
  median(CAST(l.value AS DOUBLE)) AS value
FROM read_csv_auto('{LONG}', header=true) l
JOIN win w ON CAST(l.op_id AS BIGINT) = w.op_id
WHERE l.chart_time IS NOT NULL
  AND l.item_name IS NOT NULL
  AND l.value IS NOT NULL
  AND CAST(l.chart_time AS DOUBLE) >= w.vital_extract_start_time
  AND CAST(l.chart_time AS DOUBLE) <= w.vital_extract_end_time
GROUP BY 1, 2, 3
""")

print('Filtering grid to operations with at least one raw vital observation ...', flush=True)
con.execute("""
CREATE TABLE grid_all_timeline_ops_before_vital_filter AS
SELECT * FROM grid
""")
con.execute("""
CREATE TABLE vital_ops AS
SELECT DISTINCT op_id FROM agg
""")
con.execute("""
CREATE OR REPLACE TABLE grid AS
SELECT g.*
FROM grid_all_timeline_ops_before_vital_filter g
JOIN vital_ops v USING (op_id)
""")

print('Pivoting aggregated bins to wide format ...', flush=True)
con.execute(f"""
CREATE TABLE wide_obs AS
SELECT
  op_id,
  grid_min_from_entry,
  {pivot_exprs}
FROM agg
GROUP BY op_id, grid_min_from_entry
""")

print('Merging onto complete grid ...', flush=True)
con.execute(f"""
CREATE TABLE wide AS
SELECT
  g.subject_id,
  g.hadm_id,
  g.op_id,
  g.case_id,
  g.chart_time,
  g.grid_min_from_entry,
  g.vital_extract_start_time,
  g.vital_extract_end_time,
  g.time_qc_flag,
  {', '.join('o.' + qident(item) for item in ITEMS)}
FROM grid g
LEFT JOIN wide_obs o
  ON g.op_id = o.op_id AND g.grid_min_from_entry = o.grid_min_from_entry
ORDER BY g.subject_id, g.hadm_id, g.op_id, g.grid_min_from_entry
""")

print('Writing CSV ...', flush=True)
con.execute(f"COPY wide TO '{OUT}' (HEADER, DELIMITER ',')")

print('Writing summary ...', flush=True)
summary_rows = con.execute(f"""
SELECT 'grid_step_min' AS metric, {GRID_STEP}::DOUBLE AS value
UNION ALL SELECT 'n_rows', COUNT(*)::DOUBLE FROM wide
UNION ALL SELECT 'n_ops', COUNT(DISTINCT op_id)::DOUBLE FROM wide
UNION ALL SELECT 'n_empty_vital_ops_removed',
  (SELECT COUNT(*)::DOUBLE FROM (
    SELECT DISTINCT op_id FROM grid_all_timeline_ops_before_vital_filter
    EXCEPT
    SELECT op_id FROM vital_ops
  ))
UNION ALL SELECT 'n_empty_vital_grid_rows_removed',
  (SELECT COUNT(*)::DOUBLE
   FROM grid_all_timeline_ops_before_vital_filter
   WHERE op_id NOT IN (SELECT op_id FROM vital_ops))
UNION ALL SELECT 'n_value_columns', {len(ITEMS)}::DOUBLE
UNION ALL SELECT 'n_rows_with_any_observed_value', SUM(CASE WHEN ({observed_expr}) > 0 THEN 1 ELSE 0 END)::DOUBLE FROM wide
UNION ALL SELECT 'median_grid_rows_per_op', median(n)::DOUBLE FROM (SELECT op_id, COUNT(*) n FROM wide GROUP BY op_id)
UNION ALL SELECT 'p25_grid_rows_per_op', quantile_cont(n, 0.25)::DOUBLE FROM (SELECT op_id, COUNT(*) n FROM wide GROUP BY op_id)
UNION ALL SELECT 'p75_grid_rows_per_op', quantile_cont(n, 0.75)::DOUBLE FROM (SELECT op_id, COUNT(*) n FROM wide GROUP BY op_id)
""").fetchall()
pd.DataFrame(summary_rows, columns=['metric', 'value']).to_csv(SUMMARY, index=False)

n_rows, n_ops = con.execute('SELECT COUNT(*), COUNT(DISTINCT op_id) FROM wide').fetchone()
print(f'Done. Output: {OUT}', flush=True)
print(f'Rows: {n_rows}, Unique op_id: {n_ops}, Cols: {9 + len(ITEMS)}', flush=True)
print(f'Summary: {SUMMARY}', flush=True)
con.close()
