#!/usr/bin/env python3
"""Repair interrupted 5-min wide export by removing duplicate op_id/grid rows.

The chunked exporter is append/resume based. If a process is interrupted around a
chunk boundary, a small already-appended chunk can be appended again on resume.
This repair keeps the first row for each op_id + grid_min_from_entry and preserves
all original content in a timestamped backup file.
"""
import csv
from pathlib import Path
from datetime import datetime
import duckdb

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
OUT = BASE / 'intraop_vitals_wide_5min_grid_before_impute_no_calibration_all_ops.csv'
DB = BASE / 'intraop_vitals_wide_5min_grid_20260517.duckdb'

stamp = datetime.now().strftime('%Y%m%d_%H%M%S')
BACKUP = OUT.with_name(OUT.stem + f'_with_duplicate_rows_backup_{stamp}' + OUT.suffix)
TMP = OUT.with_name(OUT.stem + f'_deduplicated_tmp_{stamp}' + OUT.suffix)

con = duckdb.connect(str(DB), read_only=True)
expected = con.execute('SELECT COUNT(*) FROM grid').fetchone()[0]
con.close()

OUT.rename(BACKUP)
seen = set()
rows_in = rows_out = duplicate_rows = 0

with BACKUP.open(newline='') as f_in, TMP.open('w', newline='') as f_out:
    reader = csv.reader(f_in)
    writer = csv.writer(f_out, lineterminator='\n')
    header = next(reader)
    writer.writerow(header)
    op_idx = header.index('op_id')
    grid_idx = header.index('grid_min_from_entry')
    for row in reader:
        rows_in += 1
        key = (row[op_idx], row[grid_idx])
        if key in seen:
            duplicate_rows += 1
            continue
        seen.add(key)
        writer.writerow(row)
        rows_out += 1

if rows_out != expected:
    # Restore original file if validation fails.
    TMP.unlink(missing_ok=True)
    BACKUP.rename(OUT)
    raise SystemExit(f'Validation failed: rows_out={rows_out}, expected={expected}; restored original file')

TMP.rename(OUT)
print(f'backup={BACKUP}')
print(f'rows_in={rows_in}')
print(f'duplicate_rows_removed={duplicate_rows}')
print(f'rows_out={rows_out}')
print(f'expected={expected}')
print(f'final={OUT}')
