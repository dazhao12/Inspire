#!/usr/bin/env python3
"""
Timestamp: 2026-05-17T18:12:00Z
Build DuckDB staging tables for all-op intraoperative vitals observed-time wide export.

Observed-time wide means raw observed chart_time rows only. It does not create a
complete 5-minute grid and does not clean, calibrate, or impute values. Duplicate
op_id + chart_time + item_name values are collapsed with median before pivoting.
"""
from pathlib import Path
import duckdb
import csv

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed')
LONG = BASE / 'intraop_vitals_clean_before_impute_no_calibration_all_op_extracted.csv'
QC = BASE / 'intraop_vitals_extract_window_qc_20260517.csv'
TMPDB = BASE / 'intraop_vitals_wide_observed_20260517.duckdb'
SUMMARY = BASE / 'intraop_vitals_wide_observed_summary_20260517.csv'

ITEMS = [
    'aft','air','alb20','alb5','art_dbp','art_mbp','art_sbp','bis','bt','cbro2','ci','cpat','cryo','cvp',
    'd10w','d50w','d5w','dobui','dopai','ds','ebl','eph','epi','epii','etco2','etdes','etgas','etiso','etsevo',
    'ffp','fio2','ftn','hes','hns','hr','hs','mdz','minvol','mlni','n2o','nepi','nibp_dbp','nibp_mbp','nibp_sbp',
    'ns','ntgi','o2','pap_dbp','pap_mbp','pap_sbp','pc','peep','pepi','phe','pheresis','pip','pmean','ppf',
    'ppfi','pplat','psa','rbc','rfti','rr','sft','spo2','sti','stii','stiii','stv5','svi','uo','vaso','vt'
]

if TMPDB.exists():
    TMPDB.unlink()

con = duckdb.connect(str(TMPDB))
con.execute("PRAGMA threads=8")
con.execute("PRAGMA memory_limit='24GB'")
con.execute("PRAGMA temp_directory='/N/scratch/zz86/tmp'")

print('Loading valid extraction windows ...', flush=True)
con.execute("""
CREATE TABLE win AS
SELECT
  CAST(op_id AS BIGINT) AS op_id,
  CAST(subject_id AS BIGINT) AS subject_id,
  CAST(hadm_id AS BIGINT) AS hadm_id,
  TRY_CAST(case_id AS DOUBLE) AS case_id,
  CAST(vital_extract_start_time AS DOUBLE) AS vital_extract_start_time,
  CAST(vital_extract_end_time AS DOUBLE) AS vital_extract_end_time,
  time_qc_flag
FROM read_csv_auto('{qc}', header=true)
WHERE extract_window_valid = true
  AND vital_extract_start_time IS NOT NULL
  AND vital_extract_end_time IS NOT NULL
  AND CAST(vital_extract_end_time AS DOUBLE) >= CAST(vital_extract_start_time AS DOUBLE)
""".format(qc=QC))

print('Aggregating duplicate raw observed values by median ...', flush=True)
con.execute("""
CREATE TABLE obs_agg AS
SELECT
  CAST(l.op_id AS BIGINT) AS op_id,
  CAST(l.chart_time AS DOUBLE) AS chart_time,
  l.item_name,
  median(CAST(l.value AS DOUBLE)) AS value,
  COUNT(*) AS n_raw_values
FROM read_csv_auto('{long}', header=true) l
JOIN win w ON CAST(l.op_id AS BIGINT) = w.op_id
WHERE l.chart_time IS NOT NULL
  AND l.item_name IS NOT NULL
  AND l.value IS NOT NULL
  AND CAST(l.chart_time AS DOUBLE) >= w.vital_extract_start_time
  AND CAST(l.chart_time AS DOUBLE) <= w.vital_extract_end_time
GROUP BY 1, 2, 3
""".format(long=LONG))

print('Building observed-time row index ...', flush=True)
con.execute("""
CREATE TABLE obs_time AS
SELECT
  w.subject_id,
  w.hadm_id,
  a.op_id,
  w.case_id,
  a.chart_time,
  CAST(a.chart_time - w.vital_extract_start_time AS DOUBLE) AS min_from_entry,
  w.vital_extract_start_time,
  w.vital_extract_end_time,
  w.time_qc_flag
FROM (SELECT DISTINCT op_id, chart_time FROM obs_agg) a
JOIN win w ON a.op_id = w.op_id
""")

print('Writing summary ...', flush=True)
rows = con.execute("""
WITH per_op AS (SELECT op_id, COUNT(*) AS n FROM obs_time GROUP BY op_id),
dup AS (SELECT SUM(n_raw_values - 1) AS duplicate_raw_values_collapsed FROM obs_agg)
SELECT 'n_rows' AS metric, COUNT(*)::DOUBLE AS value FROM obs_time
UNION ALL SELECT 'n_ops', COUNT(DISTINCT op_id)::DOUBLE FROM obs_time
UNION ALL SELECT 'n_value_columns', {n_items}::DOUBLE
UNION ALL SELECT 'n_aggregated_op_time_item_rows', COUNT(*)::DOUBLE FROM obs_agg
UNION ALL SELECT 'duplicate_raw_values_collapsed', COALESCE(duplicate_raw_values_collapsed, 0)::DOUBLE FROM dup
UNION ALL SELECT 'median_observed_rows_per_op', median(n)::DOUBLE FROM per_op
UNION ALL SELECT 'p25_observed_rows_per_op', quantile_cont(n, 0.25)::DOUBLE FROM per_op
UNION ALL SELECT 'p75_observed_rows_per_op', quantile_cont(n, 0.75)::DOUBLE FROM per_op
""".format(n_items=len(ITEMS))).fetchall()
with SUMMARY.open('w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['metric', 'value'])
    writer.writerows(rows)

n_rows, n_ops, n_agg = con.execute('SELECT (SELECT COUNT(*) FROM obs_time), (SELECT COUNT(DISTINCT op_id) FROM obs_time), (SELECT COUNT(*) FROM obs_agg)').fetchone()
print('Done staging: {}'.format(TMPDB), flush=True)
print('Observed rows: {}, Unique op_id: {}, Aggregated op-time-item rows: {}'.format(n_rows, n_ops, n_agg), flush=True)
print('Summary: {}'.format(SUMMARY), flush=True)
con.close()
