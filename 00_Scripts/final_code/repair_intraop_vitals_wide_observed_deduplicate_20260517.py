#!/usr/bin/env python3
"""Repair interrupted observed-time wide export by removing duplicate op_id/chart_time rows."""
import csv
from pathlib import Path
from datetime import datetime
import duckdb

BASE = Path('/N/project/analgesia_perioperation/data/INSPIRE_1.3/02_extracted_unprocessed/version_groups/new_all_ops_20260517')
OUT = BASE / 'intraop_vitals_wide_observed_before_impute_no_calibration_all_ops.csv'
DB = BASE / 'intraop_vitals_wide_observed_20260517.duckdb'

stamp = datetime.now().strftime('%Y%m%d_%H%M%S')
BACKUP = OUT.with_name(OUT.stem + '_with_duplicate_rows_backup_' + stamp + OUT.suffix)
TMP = OUT.with_name(OUT.stem + '_deduplicated_tmp_' + stamp + OUT.suffix)

con = duckdb.connect(str(DB), read_only=True)
expected = con.execute('SELECT COUNT(*) FROM obs_time').fetchone()[0]
con.close()

OUT.rename(BACKUP)
seen = set()
rows_in = 0
rows_out = 0
duplicate_rows = 0

with BACKUP.open(newline='') as f_in, TMP.open('w', newline='') as f_out:
    reader = csv.reader(f_in)
    writer = csv.writer(f_out, lineterminator='\n')
    header = next(reader)
    writer.writerow(header)
    op_idx = header.index('op_id')
    time_idx = header.index('chart_time')
    for row in reader:
        rows_in += 1
        key = (row[op_idx], row[time_idx])
        if key in seen:
            duplicate_rows += 1
            continue
        seen.add(key)
        writer.writerow(row)
        rows_out += 1

if rows_out != expected:
    TMP.unlink()
    BACKUP.rename(OUT)
    raise SystemExit('Validation failed: rows_out={}, expected={}; restored original file'.format(rows_out, expected))

TMP.rename(OUT)
print('backup={}'.format(BACKUP))
print('rows_in={}'.format(rows_in))
print('duplicate_rows_removed={}'.format(duplicate_rows))
print('rows_out={}'.format(rows_out))
print('expected={}'.format(expected))
print('final={}'.format(OUT))
