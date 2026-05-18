#!/usr/bin/env python3
from pathlib import Path
import subprocess
import duckdb
import pandas as pd

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
DB = BASE / 'intraop_vitals_wide_5min_grid_20260517.duckdb'
OUT = BASE / 'intraop_vitals_wide_5min_grid_before_impute_no_calibration_all_ops.csv'
SUMMARY = BASE / 'intraop_vitals_wide_5min_grid_summary_20260517.csv'
WORKER = Path('/N/project/analgesia_perioperation/projects/data_process_ZZ/Inspire/00_Scripts/final_code/export_intraop_vitals_wide_5min_grid_one_chunk_20260517.py')
TMPDIR = Path('/N/scratch/zz86/tmp/inspire_5min_chunks')
CHUNK_OPS = 100
TMPDIR.mkdir(parents=True, exist_ok=True)

def get_last_op_id(csv_path: Path):
    if not csv_path.exists() or csv_path.stat().st_size == 0:
        return None, 0
    max_op = None
    n_rows = 0
    with csv_path.open('r', newline='') as f:
        import csv
        reader = csv.reader(f)
        header = next(reader, None)
        if not header:
            return None, 0
        op_idx = header.index('op_id')
        for row in reader:
            if not row:
                continue
            n_rows += 1
            try:
                op = int(float(row[op_idx]))
            except Exception:
                continue
            if max_op is None or op > max_op:
                max_op = op
    return max_op, n_rows

last_op, existing_rows = get_last_op_id(OUT)
con = duckdb.connect(str(DB), read_only=True)
if last_op is None:
    opids = [r[0] for r in con.execute('SELECT op_id FROM win ORDER BY op_id').fetchall()]
    header_written = False
    rows_written = 0
    print(f'Exporting {len(opids)} ops to {OUT}', flush=True)
else:
    opids = [r[0] for r in con.execute(f'SELECT op_id FROM win WHERE op_id > {last_op} ORDER BY op_id').fetchall()]
    header_written = True
    rows_written = existing_rows
    print(f'Resuming after op_id {last_op}; remaining ops {len(opids)}; existing rows {existing_rows}', flush=True)
expected = con.execute('SELECT COUNT(*) FROM grid').fetchone()[0]
con.close()
for idx, start in enumerate(range(0, len(opids), CHUNK_OPS), start=1):
    lo = opids[start]
    hi = opids[min(start + CHUNK_OPS, len(opids)) - 1]
    tmp = TMPDIR / f'chunk_{idx:04d}_{lo}_{hi}.csv'
    if tmp.exists():
        tmp.unlink()
    cmd = ['python3', str(WORKER), str(lo), str(hi), str(tmp)]
    subprocess.run(cmd, check=True)
    with tmp.open('r') as fsrc, OUT.open('a') as fdst:
        for line_no, line in enumerate(fsrc):
            if line_no == 0 and header_written:
                continue
            fdst.write(line)
    header_written = True
    # Count rows minus header cheaply.
    with tmp.open('r') as f:
        n = sum(1 for _ in f) - 1
    rows_written += n
    tmp.unlink()
    print(f'chunk {idx}: op_id {lo}-{hi}, rows {n}, total {rows_written}', flush=True)

con = duckdb.connect(str(DB), read_only=True)
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
con.close()
print(f'Done. rows_written={rows_written}, expected={expected}', flush=True)
print(f'Summary: {SUMMARY}', flush=True)
