#!/usr/bin/env python3
"""Filter 5-min intraop vital grid to operations with at least one raw vital observation.

Operations with no raw vital observations produce grid rows where all 74 vital
columns are NA. Those rows are not useful for vital imputation/model input, so
this script removes them from the formal 5-min wide output while keeping a debug
backup of the previous all-timeline-op file.
"""
import csv
from pathlib import Path
from datetime import datetime
import duckdb

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed')
DB = BASE / 'intraop_vitals_wide_5min_grid_20260517.duckdb'
OUT = BASE / 'intraop_vitals_wide_5min_grid_before_impute_no_calibration_all_ops.csv'
SUMMARY = BASE / 'intraop_vitals_wide_5min_grid_summary_20260517.csv'
DEBUG = BASE / 'debug_backups_20260518'
DEBUG.mkdir(parents=True, exist_ok=True)

stamp = datetime.now().strftime('%Y%m%d_%H%M%S')
BACKUP = DEBUG / (OUT.stem + '_including_empty_vital_ops_backup_' + stamp + OUT.suffix)
TMP = OUT.with_name(OUT.stem + '_vital_ops_tmp_' + stamp + OUT.suffix)

con = duckdb.connect(str(DB))
tables = set(r[0] for r in con.execute('SHOW TABLES').fetchall())
if 'grid_all_timeline_ops_before_vital_filter' not in tables:
    con.execute('CREATE TABLE grid_all_timeline_ops_before_vital_filter AS SELECT * FROM grid')
con.execute('CREATE OR REPLACE TABLE vital_ops AS SELECT DISTINCT op_id FROM agg')
con.execute('CREATE OR REPLACE TABLE grid_vital_ops AS SELECT g.* FROM grid_all_timeline_ops_before_vital_filter g JOIN vital_ops v USING (op_id)')
con.execute('CREATE OR REPLACE TABLE grid AS SELECT * FROM grid_vital_ops')
expected_rows, expected_ops = con.execute('SELECT COUNT(*), COUNT(DISTINCT op_id) FROM grid').fetchone()
empty_ops = con.execute('SELECT COUNT(*) FROM (SELECT DISTINCT op_id FROM grid_all_timeline_ops_before_vital_filter EXCEPT SELECT op_id FROM vital_ops)').fetchone()[0]
empty_grid_rows = con.execute('SELECT COUNT(*) FROM grid_all_timeline_ops_before_vital_filter WHERE op_id NOT IN (SELECT op_id FROM vital_ops)').fetchone()[0]
keep_ops = set(str(r[0]) for r in con.execute('SELECT op_id FROM vital_ops').fetchall())
summary_rows = con.execute("""
WITH per_op AS (SELECT op_id, COUNT(*) AS n FROM grid GROUP BY op_id),
observed AS (SELECT COUNT(*) AS n_observed FROM (SELECT DISTINCT op_id, grid_min_from_entry FROM agg))
SELECT 'grid_step_min' AS metric, 5::DOUBLE AS value
UNION ALL SELECT 'n_rows', COUNT(*)::DOUBLE FROM grid
UNION ALL SELECT 'n_ops', COUNT(DISTINCT op_id)::DOUBLE FROM grid
UNION ALL SELECT 'n_empty_vital_ops_removed', {empty_ops}::DOUBLE
UNION ALL SELECT 'n_empty_vital_grid_rows_removed', {empty_grid_rows}::DOUBLE
UNION ALL SELECT 'n_value_columns', 74::DOUBLE
UNION ALL SELECT 'n_rows_with_any_observed_value', n_observed::DOUBLE FROM observed
UNION ALL SELECT 'median_grid_rows_per_op', median(n)::DOUBLE FROM per_op
UNION ALL SELECT 'p25_grid_rows_per_op', quantile_cont(n, 0.25)::DOUBLE FROM per_op
UNION ALL SELECT 'p75_grid_rows_per_op', quantile_cont(n, 0.75)::DOUBLE FROM per_op
""".format(empty_ops=empty_ops, empty_grid_rows=empty_grid_rows)).fetchall()
con.close()

OUT.rename(BACKUP)
rows_in = rows_out = 0
ops_out = set()
with BACKUP.open(newline='') as f_in, TMP.open('w', newline='') as f_out:
    reader = csv.reader(f_in)
    writer = csv.writer(f_out, lineterminator='\n')
    header = next(reader)
    writer.writerow(header)
    op_idx = header.index('op_id')
    for row in reader:
        rows_in += 1
        if row[op_idx] not in keep_ops:
            continue
        writer.writerow(row)
        rows_out += 1
        ops_out.add(row[op_idx])

if rows_out != expected_rows or len(ops_out) != expected_ops:
    TMP.unlink()
    BACKUP.rename(OUT)
    raise SystemExit('Validation failed: rows_out={}, expected_rows={}, ops_out={}, expected_ops={}; restored original file'.format(rows_out, expected_rows, len(ops_out), expected_ops))

TMP.rename(OUT)
with SUMMARY.open('w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['metric', 'value'])
    writer.writerows(summary_rows)

print('backup={}'.format(BACKUP))
print('rows_in={}'.format(rows_in))
print('rows_out={}'.format(rows_out))
print('ops_out={}'.format(len(ops_out)))
print('empty_ops_removed={}'.format(empty_ops))
print('empty_grid_rows_removed={}'.format(empty_grid_rows))
print('final={}'.format(OUT))
print('summary={}'.format(SUMMARY))
