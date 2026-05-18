#!/usr/bin/env python3
"""
Build a new intraop vitals table with:
  1) calibrated NIBP columns for SBP/MBP/DBP
  2) ART-first merged BP columns (fallback to calibrated NIBP)

Calibration method (current method):
  - endpoint-wise fixed delta:
      delta_case = median(ART - NIBP) within op_id on paired rows
  - if pair_n >= min_pairs_case: use delta_case
  - else fallback:
      department+antype -> department -> global
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, List

import numpy as np
import pandas as pd


ENDPOINTS = ("sbp", "mbp", "dbp")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build calibrated NIBP + ART-first merged intraop vitals table.")
    parser.add_argument(
        "--input-csv",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/"
            "cleaned_no_imputation/intraop_vitals_clean_before_impute_latest.csv"
        ),
    )
    parser.add_argument(
        "--op-meta-csv",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/"
            "cleaned_no_imputation/demographic_operation_latest.csv"
        ),
    )
    parser.add_argument(
        "--output-csv",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/"
            "cleaned_no_imputation/intraop_vitals_clean_before_impute_with_calibrated_nibp_artfirst_latest.csv"
        ),
    )
    parser.add_argument(
        "--delta-table-out",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/"
            "art_nibp_agreement_20260409/output_bp_suitability/"
            "bp_case_delta_table_for_artfirst_calibrated_table_20260409.csv"
        ),
    )
    parser.add_argument("--min-pairs-case", type=int, default=5)
    parser.add_argument("--chunksize", type=int, default=300000)
    return parser.parse_args()


def build_delta_table(mini: pd.DataFrame, op_meta: pd.DataFrame, min_pairs_case: int) -> pd.DataFrame:
    all_ops = pd.DataFrame({"op_id": mini["op_id"].dropna().drop_duplicates().to_numpy()})
    all_ops = all_ops.merge(op_meta, on="op_id", how="left")
    all_ops["department"] = all_ops["department"].fillna("Unknown")
    all_ops["antype"] = all_ops["antype"].fillna("Unknown")

    endpoint_tables: List[pd.DataFrame] = []
    for ep in ENDPOINTS:
        art_col = f"art_{ep}"
        nibp_col = f"nibp_{ep}"

        pair = mini.loc[mini[art_col].notna() & mini[nibp_col].notna(), ["op_id", art_col, nibp_col]].copy()
        pair["error"] = pair[art_col] - pair[nibp_col]

        case_stats = (
            pair.groupby("op_id", sort=False)["error"]
            .agg(pair_n="size", delta_case="median")
            .reset_index()
        )
        case_stats["pair_n"] = case_stats["pair_n"].astype(int)

        pair_meta = pair.merge(op_meta, on="op_id", how="left")
        pair_meta["department"] = pair_meta["department"].fillna("Unknown")
        pair_meta["antype"] = pair_meta["antype"].fillna("Unknown")

        delta_dept_antype = (
            pair_meta.groupby(["department", "antype"], dropna=False, sort=False)["error"]
            .median()
            .rename("delta_group_dept_antype")
            .reset_index()
        )
        delta_dept = (
            pair_meta.groupby(["department"], dropna=False, sort=False)["error"]
            .median()
            .rename("delta_group_dept")
            .reset_index()
        )
        delta_global = float(pair_meta["error"].median())

        t = all_ops.merge(case_stats, on="op_id", how="left")
        t["pair_n"] = t["pair_n"].fillna(0).astype(int)
        t = t.merge(delta_dept_antype, on=["department", "antype"], how="left")
        t = t.merge(delta_dept, on=["department"], how="left")
        t["delta_global"] = delta_global

        t["delta_used"] = np.nan
        t["source_level"] = ""
        m_case = t["pair_n"] >= min_pairs_case
        t.loc[m_case, "delta_used"] = t.loc[m_case, "delta_case"]
        t.loc[m_case, "source_level"] = "case"

        m_need = t["delta_used"].isna()
        m_da = m_need & t["delta_group_dept_antype"].notna()
        t.loc[m_da, "delta_used"] = t.loc[m_da, "delta_group_dept_antype"]
        t.loc[m_da, "source_level"] = "department_antype"

        m_need = t["delta_used"].isna()
        m_d = m_need & t["delta_group_dept"].notna()
        t.loc[m_d, "delta_used"] = t.loc[m_d, "delta_group_dept"]
        t.loc[m_d, "source_level"] = "department"

        m_need = t["delta_used"].isna()
        t.loc[m_need, "delta_used"] = t.loc[m_need, "delta_global"]
        t.loc[m_need, "source_level"] = "global"

        keep = t[
            [
                "op_id",
                "pair_n",
                "delta_case",
                "delta_used",
                "source_level",
                "department",
                "antype",
                "delta_group_dept_antype",
                "delta_group_dept",
                "delta_global",
            ]
        ].copy()
        keep = keep.rename(
            columns={
                "pair_n": f"pair_n_{ep}",
                "delta_case": f"delta_case_{ep}",
                "delta_used": f"delta_used_{ep}",
                "source_level": f"source_level_{ep}",
                "delta_group_dept_antype": f"delta_group_dept_antype_{ep}",
                "delta_group_dept": f"delta_group_dept_{ep}",
                "delta_global": f"delta_global_{ep}",
            }
        )
        endpoint_tables.append(keep)

    delta = endpoint_tables[0]
    for t in endpoint_tables[1:]:
        delta = delta.merge(
            t,
            on=["op_id", "department", "antype"],
            how="outer",
        )
    return delta


def main() -> None:
    args = parse_args()

    input_csv = Path(args.input_csv)
    output_csv = Path(args.output_csv)
    delta_out = Path(args.delta_table_out)
    delta_out.parent.mkdir(parents=True, exist_ok=True)

    print("[INFO] loading reduced table for calibration deltas...")
    usecols = ["op_id"]
    for ep in ENDPOINTS:
        usecols.extend([f"art_{ep}", f"nibp_{ep}"])
    mini = pd.read_csv(input_csv, usecols=usecols)
    for c in usecols:
        mini[c] = pd.to_numeric(mini[c], errors="coerce")
    mini = mini.dropna(subset=["op_id"])
    print(f"[INFO] rows in reduced table: {len(mini):,}")

    print("[INFO] loading operation metadata...")
    op_meta = pd.read_csv(args.op_meta_csv, usecols=["op_id", "department", "antype"])
    op_meta["op_id"] = pd.to_numeric(op_meta["op_id"], errors="coerce")
    op_meta["department"] = op_meta["department"].fillna("Unknown").astype(str).replace("nan", "Unknown")
    op_meta["antype"] = op_meta["antype"].fillna("Unknown").astype(str).replace("nan", "Unknown")

    print("[INFO] building endpoint-wise delta table...")
    delta = build_delta_table(mini, op_meta, min_pairs_case=args.min_pairs_case)
    delta.to_csv(delta_out, index=False)
    print(f"[INFO] wrote: {delta_out}")

    delta_map_cols = ["op_id"]
    for ep in ENDPOINTS:
        delta_map_cols.extend([f"delta_used_{ep}", f"source_level_{ep}"])
    delta_map = delta[delta_map_cols].copy()

    print("[INFO] generating new calibrated full table (chunked write)...")
    if output_csv.exists():
        output_csv.unlink()

    first = True
    total = 0
    for i, chunk in enumerate(pd.read_csv(input_csv, chunksize=args.chunksize), start=1):
        chunk = chunk.merge(delta_map, on="op_id", how="left")

        for ep in ENDPOINTS:
            art_col = f"art_{ep}"
            nibp_col = f"nibp_{ep}"
            delta_col = f"delta_used_{ep}"
            src_col = f"source_level_{ep}"

            nibp_adj_col = f"nibp_{ep}_calibrated"
            merged_adj_col = f"{ep}_merged_artfirst_calibrated"
            nibp_src_col = f"nibp_{ep}_calibration_source"
            merged_src_col = f"{ep}_merged_artfirst_calibrated_source"

            chunk[nibp_adj_col] = np.where(
                chunk[nibp_col].notna(),
                pd.to_numeric(chunk[nibp_col], errors="coerce") + pd.to_numeric(chunk[delta_col], errors="coerce"),
                np.nan,
            )

            chunk[nibp_src_col] = np.where(chunk[nibp_col].notna(), chunk[src_col].fillna(""), "")

            chunk[merged_adj_col] = np.where(
                chunk[art_col].notna(),
                chunk[art_col],
                chunk[nibp_adj_col],
            )
            chunk[merged_src_col] = np.where(
                chunk[art_col].notna(),
                "art",
                np.where(chunk[nibp_adj_col].notna(), chunk[nibp_src_col], ""),
            )

        chunk = chunk.drop(columns=[c for c in chunk.columns if c.startswith("delta_used_") or c.startswith("source_level_")])
        chunk.to_csv(output_csv, mode="w" if first else "a", header=first, index=False)
        total += len(chunk)
        first = False
        print(f"[INFO] chunk {i} done, cumulative rows={total:,}")

    print(f"[INFO] wrote new calibrated table: {output_csv}")
    print(f"[INFO] total rows written: {total:,}")


if __name__ == "__main__":
    main()

