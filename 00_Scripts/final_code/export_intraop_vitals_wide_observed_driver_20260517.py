#!/usr/bin/env python3
from pathlib import Path
import subprocess
import duckdb
import csv

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
DB = BASE / 'intraop_vitals_wide_observed_20260517.duckdb'
OUT = BASE / 'intraop_vitals_wide_observed_before_impute_no_calibration_all_ops.csv'
WORKER = Path('/N/project/analgesia_perioperation/projects/data_process_ZZ/Inspire/00_Scripts/final_code/export_intraop_vitals_wide_observed_one_chunk_pandas_20260517.py')
TMPDIR = Path('/N/scratch/zz86/tmp/inspire_observed_chunks')
CHUNK_OPS = 500
TMPDIR.mkdir(parents=True, exist_ok=True)

def get_last_op_id(csv_path):
    if not csv_path.exists() or csv_path.stat().st_size == 0:
        return None, 0
    max_op = None
    n_rows = 0
    with csv_path.open(newline='') as f:
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
expected = con.execute('SELECT COUNT(*) FROM obs_time').fetchone()[0]
if last_op is None:
    opids = [r[0] for r in con.execute('SELECT op_id FROM win ORDER BY op_id').fetchall()]
    header_written = False
    rows_written = 0
    print('Exporting {} ops to {}'.format(len(opids), OUT), flush=True)
else:
    opids = [r[0] for r in con.execute('SELECT op_id FROM win WHERE op_id > {} ORDER BY op_id'.format(last_op)).fetchall()]
    header_written = True
    rows_written = existing_rows
    print('Resuming after op_id {}; remaining ops {}; existing rows {}'.format(last_op, len(opids), existing_rows), flush=True)
con.close()

for idx, start in enumerate(range(0, len(opids), CHUNK_OPS), start=1):
    lo = opids[start]
    hi = opids[min(start + CHUNK_OPS, len(opids)) - 1]
    tmp = TMPDIR / 'chunk_{:04d}_{}_{}.csv'.format(idx, lo, hi)
    if tmp.exists():
        tmp.unlink()
    subprocess.run(['python3', str(WORKER), str(lo), str(hi), str(tmp)], check=True)
    with tmp.open('r') as fsrc, OUT.open('a') as fdst:
        for line_no, line in enumerate(fsrc):
            if line_no == 0 and header_written:
                continue
            fdst.write(line)
    header_written = True
    with tmp.open('r') as f:
        n = sum(1 for _ in f) - 1
    rows_written += n
    tmp.unlink()
    print('chunk {}: op_id {}-{}, rows {}, total {}'.format(idx, lo, hi, n, rows_written), flush=True)

print('Done. rows_written={}, expected={}'.format(rows_written, expected), flush=True)
if rows_written != expected:
    print('WARNING: row count mismatch; inspect for interrupted duplicate/partial chunks before using output.', flush=True)
