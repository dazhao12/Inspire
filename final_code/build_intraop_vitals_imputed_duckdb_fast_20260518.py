#!/usr/bin/env python3
"""Vectorized DuckDB imputation for the cleaned INSPIRE 5-min intraop vital table."""
from pathlib import Path
import csv
import importlib.util
import duckdb
import pandas as pd

CODE = Path('/N/project/analgesia_perioperation/projects/data_process_ZZ/Inspire/final_code/build_intraop_vitals_clean_impute_5min_all_ops_20260518.py')
spec = importlib.util.spec_from_file_location('clean_impute_rules', str(CODE))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

BASE = mod.BASE
CLEAN_IN = mod.CLEAN_OUT
FAST_OUT = BASE / 'intraop_vitals_wide_5min_grid_clean_imputed_with_flags_all_ops_duckdb_fast.csv'
FAST_SUMMARY = BASE / 'intraop_vitals_imputation_summary_duckdb_fast_20260518.csv'
FAST_QC = BASE / 'intraop_vitals_imputed_duckdb_fast_validation_20260518.csv'
MEDIANS = mod.GLOBAL_MEDIANS_OUT
ID_COLS = mod.ID_COLS
ITEMS = mod.ITEMS
MERGED_BP = mod.MERGED_BP
VALUE_COLS = mod.VALUE_COLS
FLAG_COLS = mod.FLAG_COLS
MERGED_SOURCE = mod.MERGED_SOURCE
RULES = mod.RULES
BP_GROUPS = mod.BP_GROUPS

def q(c):
    return '"' + c.replace('"', '""') + '"'

def lit(v):
    return str(float(v))

def load_medians():
    med = {}
    with open(MEDIANS, newline='') as f:
        r = csv.DictReader(f)
        for row in r:
            med[row['variable']] = float(row['global_median_after_cleaning'])
    return med

def win(expr):
    return expr + ' OVER (PARTITION BY op_id ORDER BY grid_min_from_entry ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)'

def valid_expr(cols):
    sbp, mbp, dbp = cols
    return '({s} IS NOT NULL AND {m} IS NOT NULL AND {d} IS NOT NULL AND {s} >= {m} AND {m} >= {d})'.format(s=q(sbp), m=q(mbp), d=q(dbp))

def build_sql(med):
    id_select = [q(c) for c in ID_COLS]
    value_exprs = []
    flag_exprs = []
    processed = set()

    for prefix in ['art', 'nibp', 'pap', 'merged']:
        cols = BP_GROUPS[prefix]
        vld = valid_expr(cols)
        for c in cols:
            value_exprs.append('COALESCE({last}, {med}) AS {col}'.format(
                last=win('last_value(CASE WHEN {vld} THEN {col} ELSE NULL END IGNORE NULLS)'.format(vld=vld, col=q(c))),
                med=lit(med[c]), col=q(c)))
            flag_exprs.append('CASE WHEN {vld} THEN 0 ELSE 1 END AS {flag}'.format(vld=vld, flag=q(c + '_imputed_flag')))
            processed.add(c)

    for c in ITEMS:
        if c in processed:
            continue
        strat = RULES[c]['strategy']
        col = q(c)
        flag_exprs.append('CASE WHEN {col} IS NULL THEN 1 ELSE 0 END AS {flag}'.format(col=col, flag=q(c + '_imputed_flag')))
        if strat == 'continuous_monitor':
            value_exprs.append('COALESCE({last}, {med}) AS {col}'.format(last=win('last_value({} IGNORE NULLS)'.format(col)), med=lit(med[c]), col=col))
        elif strat == 'event_zero_fill':
            value_exprs.append('COALESCE({col}, 0.0) AS {col}'.format(col=col))
        elif strat == 'global_median_fill':
            value_exprs.append('COALESCE({col}, {med}) AS {col}'.format(col=col, med=lit(med[c])))
        elif strat == 'tci_state':
            lastv = win('last_value({} IGNORE NULLS)'.format(col))
            started = win('max(CASE WHEN {col} > 0 THEN 1 ELSE 0 END)'.format(col=col))
            value_exprs.append('CASE WHEN {col} IS NOT NULL THEN {col} WHEN {started}=0 THEN 0.0 ELSE COALESCE({lastv},0.0) END AS {col}'.format(col=col, started=started, lastv=lastv))
        elif strat == 'infusion_state_cap60':
            lastv = win('last_value({} IGNORE NULLS)'.format(col))
            lastt = win('max(CASE WHEN {col} IS NOT NULL THEN grid_min_from_entry ELSE NULL END)'.format(col=col))
            started = win('max(CASE WHEN {col} > 0 THEN 1 ELSE 0 END)'.format(col=col))
            value_exprs.append('CASE WHEN {col} IS NOT NULL THEN {col} WHEN {started}=0 THEN 0.0 WHEN COALESCE({lastv},0.0)=0 THEN 0.0 WHEN grid_min_from_entry - {lastt} <= 60 THEN {lastv} ELSE 0.0 END AS {col}'.format(col=col, started=started, lastv=lastv, lastt=lastt))
        else:
            value_exprs.append('COALESCE({last}, {med}) AS {col}'.format(last=win('last_value({} IGNORE NULLS)'.format(col)), med=lit(med[c]), col=col))

    mvalid = valid_expr(BP_GROUPS['merged'])
    last_src = win('last_value(CASE WHEN {vld} THEN NULLIF({src}, \'\') ELSE NULL END IGNORE NULLS)'.format(vld=mvalid, src=q(MERGED_SOURCE)))
    source_expr = 'CASE WHEN {vld} THEN NULLIF({src}, \'\') WHEN {last_src} IS NOT NULL THEN \'LOCF\' ELSE \'GLOBAL_MEDIAN\' END AS {src}'.format(vld=mvalid, src=q(MERGED_SOURCE), last_src=last_src)

    ordered_values = []
    value_map = {}
    for expr in value_exprs:
        alias = expr.split(' AS ')[-1].strip('"')
        value_map[alias] = expr
    for c in ITEMS + MERGED_BP:
        ordered_values.append(value_map[c])

    ordered_flags = []
    flag_map = {}
    for expr in flag_exprs:
        alias = expr.split(' AS ')[-1].strip('"')
        flag_map[alias] = expr
    for c in ITEMS + MERGED_BP:
        ordered_flags.append(flag_map[c + '_imputed_flag'])

    select_cols = id_select + ordered_values + [source_expr] + ordered_flags
    cast_cols = []
    for c in ID_COLS:
        if c in ['subject_id', 'hadm_id', 'op_id']:
            cast_cols.append('try_cast({0} as BIGINT) AS {0}'.format(q(c)))
        elif c in ['chart_time', 'grid_min_from_entry', 'vital_extract_start_time', 'vital_extract_end_time', 'case_id']:
            cast_cols.append('try_cast({0} as DOUBLE) AS {0}'.format(q(c)))
        else:
            cast_cols.append('{0} AS {0}'.format(q(c)))
    for c in ITEMS + MERGED_BP:
        cast_cols.append('try_cast({0} as DOUBLE) AS {0}'.format(q(c)))
    cast_cols.append('{0} AS {0}'.format(q(MERGED_SOURCE)))

    return """
WITH base AS (
  SELECT {cast_cols}
  FROM read_csv_auto('{clean}', header=true)
)
SELECT {select_cols}
FROM base
ORDER BY subject_id, hadm_id, op_id, grid_min_from_entry
""".format(cast_cols=',\n         '.join(cast_cols), clean=CLEAN_IN, select_cols=',\n       '.join(select_cols))

def main():
    if not CLEAN_IN.exists():
        raise SystemExit('Missing clean input: {}'.format(CLEAN_IN))
    med = load_medians()
    query = build_sql(med)
    if FAST_OUT.exists():
        FAST_OUT.unlink()
    con = duckdb.connect()
    con.execute("PRAGMA threads=8")
    con.execute("PRAGMA memory_limit='24GB'")
    con.execute("PRAGMA temp_directory='/N/scratch/zz86/tmp'")
    print('writing fast imputed CSV: {}'.format(FAST_OUT), flush=True)
    con.execute("COPY ({}) TO '{}' (HEADER, DELIMITER ',')".format(query, FAST_OUT))
    print('validating...', flush=True)
    n_rows = con.execute("SELECT COUNT(*) FROM read_csv_auto('{}', header=true)".format(FAST_OUT)).fetchone()[0]
    na_expr = ' + '.join(['sum(case when try_cast({} as double) is null then 1 else 0 end)'.format(q(c)) for c in VALUE_COLS])
    final_na = con.execute("SELECT {} FROM read_csv_auto('{}', header=true)".format(na_expr, FAST_OUT)).fetchone()[0]
    rows = []
    for c in VALUE_COLS:
        flag = c + '_imputed_flag'
        n, imp, obs, na = con.execute("SELECT COUNT(*), SUM(try_cast({flag} as BIGINT)), SUM(CASE WHEN try_cast({flag} as BIGINT)=0 THEN 1 ELSE 0 END), SUM(CASE WHEN try_cast({col} as DOUBLE) IS NULL THEN 1 ELSE 0 END) FROM read_csv_auto('{path}', header=true)".format(flag=q(flag), col=q(c), path=FAST_OUT)).fetchone()
        rows.append(dict(variable=c, n_rows=n, n_observed_after_clean=obs, n_imputed=imp, pct_imputed=round(100.0 * imp / n, 6) if n else 0, n_na_after_impute=na, strategy=RULES[c]['strategy']))
    pd.DataFrame(rows).to_csv(FAST_SUMMARY, index=False)
    pd.DataFrame([dict(metric='n_rows', value=n_rows), dict(metric='final_clinical_na_count', value=final_na)]).to_csv(FAST_QC, index=False)
    con.close()
    print('rows={}'.format(n_rows), flush=True)
    print('final_clinical_na_count={}'.format(final_na), flush=True)
    print('summary={}'.format(FAST_SUMMARY), flush=True)
    print('qc={}'.format(FAST_QC), flush=True)

if __name__ == '__main__':
    main()
