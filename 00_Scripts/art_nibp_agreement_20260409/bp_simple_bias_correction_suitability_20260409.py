#!/usr/bin/env python3
"""
Suitability check for fixed-delta correction on SBP/MBP/DBP.

For each endpoint (sbp/mbp/dbp):
  1) Build case-level delta: median(ART - NIBP) from paired rows.
  2) Fallback for low-pair cases:
       department+antype -> department -> global.
  3) Evaluate before vs after on paired rows (in-sample screening).
  4) Produce visualization:
       - value distribution (ART / NIBP before / corrected after)
       - error distribution before/after
       - delta-error distribution (after-before, abs improvement)
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, List

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ENDPOINTS = ("sbp", "mbp", "dbp")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="SBP/MBP/DBP suitability under simple fixed-delta correction.")
    parser.add_argument(
        "--vitals",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/"
            "cleaned_no_imputation/intraop_vitals_clean_before_impute_latest.csv"
        ),
    )
    parser.add_argument(
        "--op-meta",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/"
            "cleaned_no_imputation/demographic_operation_latest.csv"
        ),
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/"
            "art_nibp_agreement_20260409/output_bp_suitability"
        ),
    )
    parser.add_argument("--min-pairs-case", type=int, default=5)
    parser.add_argument("--plot-max-points", type=int, default=80000)
    parser.add_argument("--seed", type=int, default=20260409)
    return parser.parse_args()


def compute_metrics(error: np.ndarray, mean_pressure: np.ndarray) -> Dict[str, float]:
    error = np.asarray(error, dtype=float)
    mean_pressure = np.asarray(mean_pressure, dtype=float)
    ok = np.isfinite(error) & np.isfinite(mean_pressure)
    error = error[ok]
    mean_pressure = mean_pressure[ok]
    abs_error = np.abs(error)

    out: Dict[str, float] = {
        "n_pairs": int(error.size),
        "bias": float(np.mean(error)) if error.size else np.nan,
        "sd": float(np.std(error, ddof=1)) if error.size > 1 else np.nan,
        "mae": float(np.mean(abs_error)) if error.size else np.nan,
        "medae": float(np.median(abs_error)) if error.size else np.nan,
        "p90ae": float(np.quantile(abs_error, 0.90)) if error.size else np.nan,
        "prop_abs_le_5": float(np.mean(abs_error <= 5.0)) if error.size else np.nan,
        "prop_abs_le_10": float(np.mean(abs_error <= 10.0)) if error.size else np.nan,
        "prop_abs_le_15": float(np.mean(abs_error <= 15.0)) if error.size else np.nan,
    }
    out["loa_low"] = out["bias"] - 1.96 * out["sd"] if np.isfinite(out["sd"]) else np.nan
    out["loa_high"] = out["bias"] + 1.96 * out["sd"] if np.isfinite(out["sd"]) else np.nan

    if error.size > 1 and np.nanstd(mean_pressure) > 0:
        slope, intercept = np.polyfit(mean_pressure, error, 1)
        corr = float(np.corrcoef(mean_pressure, error)[0, 1])
        out["resid_vs_mean_slope"] = float(slope)
        out["resid_vs_mean_intercept"] = float(intercept)
        out["resid_vs_mean_corr"] = corr
    else:
        out["resid_vs_mean_slope"] = np.nan
        out["resid_vs_mean_intercept"] = np.nan
        out["resid_vs_mean_corr"] = np.nan
    return out


def sample_df(df: pd.DataFrame, max_points: int, seed: int) -> pd.DataFrame:
    if len(df) <= max_points:
        return df
    rng = np.random.default_rng(seed)
    idx = rng.choice(len(df), size=max_points, replace=False)
    return df.iloc[idx].copy()


def plot_endpoint(eval_df: pd.DataFrame, endpoint: str, fig_dir: Path, max_points: int, seed: int) -> None:
    d = sample_df(eval_df, max_points, seed)

    # 1) value distributions
    plt.figure(figsize=(8, 6))
    plt.hist(d["art_value"], bins=80, alpha=0.45, label="ART", color="tab:red")
    plt.hist(d["nibp_before"], bins=80, alpha=0.45, label="NIBP before", color="tab:blue")
    plt.hist(d["nibp_after"], bins=80, alpha=0.45, label="NIBP after correction", color="tab:green")
    plt.xlabel(f"{endpoint.upper()} (mmHg)")
    plt.ylabel("Count")
    plt.title(f"{endpoint.upper()} Value Distribution: Before vs After")
    plt.legend()
    plt.tight_layout()
    plt.savefig(fig_dir / f"{endpoint}_value_distribution_before_after.png", dpi=150)
    plt.close()

    # 2) error distributions
    plt.figure(figsize=(8, 6))
    plt.hist(d["error_before"], bins=80, alpha=0.5, label="error before", color="tab:gray")
    plt.hist(d["error_after"], bins=80, alpha=0.5, label="error after", color="tab:green")
    plt.axvline(d["error_before"].mean(), color="tab:gray", linestyle="--", linewidth=1.2)
    plt.axvline(d["error_after"].mean(), color="tab:green", linestyle="--", linewidth=1.2)
    plt.xlabel(f"Error (ART - {endpoint.upper()} estimate), mmHg")
    plt.ylabel("Count")
    plt.title(f"{endpoint.upper()} Error Distribution: Before vs After")
    plt.legend()
    plt.tight_layout()
    plt.savefig(fig_dir / f"{endpoint}_error_distribution_before_after.png", dpi=150)
    plt.close()

    # 3) difference of errors / absolute improvement
    plt.figure(figsize=(8, 6))
    plt.hist(d["error_after"] - d["error_before"], bins=80, alpha=0.6, label="error_after - error_before", color="tab:purple")
    plt.hist(np.abs(d["error_before"]) - np.abs(d["error_after"]), bins=80, alpha=0.5, label="abs_error improvement", color="tab:orange")
    plt.axvline(0.0, color="black", linestyle="--", linewidth=1.0)
    plt.xlabel("Difference (mmHg)")
    plt.ylabel("Count")
    plt.title(f"{endpoint.upper()} Difference Distribution")
    plt.legend()
    plt.tight_layout()
    plt.savefig(fig_dir / f"{endpoint}_difference_distribution.png", dpi=150)
    plt.close()


def main() -> None:
    args = parse_args()
    out_dir = Path(args.output_dir)
    fig_dir = out_dir / "figures"
    out_dir.mkdir(parents=True, exist_ok=True)
    fig_dir.mkdir(parents=True, exist_ok=True)

    print("[INFO] loading vitals...")
    usecols = ["op_id"]
    for ep in ENDPOINTS:
        usecols.extend((f"art_{ep}", f"nibp_{ep}"))
    vitals = pd.read_csv(args.vitals, usecols=usecols)
    for c in usecols:
        vitals[c] = pd.to_numeric(vitals[c], errors="coerce")
    vitals = vitals.dropna(subset=["op_id"])
    print(f"[INFO] vitals rows: {len(vitals):,}")

    print("[INFO] loading op metadata...")
    op_meta = pd.read_csv(args.op_meta, usecols=["op_id", "department", "antype"])
    op_meta["op_id"] = pd.to_numeric(op_meta["op_id"], errors="coerce")
    op_meta["department"] = op_meta["department"].fillna("Unknown").astype(str).replace("nan", "Unknown")
    op_meta["antype"] = op_meta["antype"].fillna("Unknown").astype(str).replace("nan", "Unknown")

    all_ops = pd.DataFrame({"op_id": vitals["op_id"].drop_duplicates().to_numpy()})
    all_ops = all_ops.merge(op_meta, on="op_id", how="left")
    all_ops["department"] = all_ops["department"].fillna("Unknown")
    all_ops["antype"] = all_ops["antype"].fillna("Unknown")

    case_rows: List[pd.DataFrame] = []
    eval_rows: List[Dict[str, float]] = []
    suitability_rows: List[Dict[str, float]] = []

    for i, ep in enumerate(ENDPOINTS):
        print(f"[INFO] processing endpoint: {ep}")
        art_col = f"art_{ep}"
        nibp_col = f"nibp_{ep}"

        pair = vitals.loc[vitals[art_col].notna() & vitals[nibp_col].notna(), ["op_id", art_col, nibp_col]].copy()
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
            pair_meta.groupby(["department", "antype"], sort=False, dropna=False)["error"]
            .median()
            .rename("delta_group_dept_antype")
            .reset_index()
        )
        delta_dept = (
            pair_meta.groupby(["department"], sort=False, dropna=False)["error"]
            .median()
            .rename("delta_group_dept")
            .reset_index()
        )
        delta_global = float(pair_meta["error"].median())

        case = all_ops.merge(case_stats, on="op_id", how="left")
        case["pair_n"] = case["pair_n"].fillna(0).astype(int)
        case = case.merge(delta_dept_antype, on=["department", "antype"], how="left")
        case = case.merge(delta_dept, on=["department"], how="left")
        case["delta_global"] = delta_global

        case["delta_used"] = np.nan
        case["source_level"] = ""
        m_case = case["pair_n"] >= args.min_pairs_case
        case.loc[m_case, "delta_used"] = case.loc[m_case, "delta_case"]
        case.loc[m_case, "source_level"] = "case"

        m_need = case["delta_used"].isna()
        m_da = m_need & case["delta_group_dept_antype"].notna()
        case.loc[m_da, "delta_used"] = case.loc[m_da, "delta_group_dept_antype"]
        case.loc[m_da, "source_level"] = "department_antype"

        m_need = case["delta_used"].isna()
        m_d = m_need & case["delta_group_dept"].notna()
        case.loc[m_d, "delta_used"] = case.loc[m_d, "delta_group_dept"]
        case.loc[m_d, "source_level"] = "department"

        m_need = case["delta_used"].isna()
        case.loc[m_need, "delta_used"] = case.loc[m_need, "delta_global"]
        case.loc[m_need, "source_level"] = "global"

        case_ep = case[
            [
                "op_id",
                "department",
                "antype",
                "pair_n",
                "delta_case",
                "delta_used",
                "source_level",
                "delta_group_dept_antype",
                "delta_group_dept",
                "delta_global",
            ]
        ].copy()
        case_ep["endpoint"] = ep
        case_rows.append(case_ep)

        eval_df = pair.merge(case_ep[["op_id", "delta_used"]], on="op_id", how="left")
        eval_df["art_value"] = eval_df[art_col]
        eval_df["nibp_before"] = eval_df[nibp_col]
        eval_df["nibp_after"] = eval_df["nibp_before"] + eval_df["delta_used"]
        eval_df["error_before"] = eval_df["art_value"] - eval_df["nibp_before"]
        eval_df["error_after"] = eval_df["art_value"] - eval_df["nibp_after"]
        eval_df["mean_before"] = (eval_df["art_value"] + eval_df["nibp_before"]) / 2.0
        eval_df["mean_after"] = (eval_df["art_value"] + eval_df["nibp_after"]) / 2.0

        before = compute_metrics(eval_df["error_before"].to_numpy(), eval_df["mean_before"].to_numpy())
        after = compute_metrics(eval_df["error_after"].to_numpy(), eval_df["mean_after"].to_numpy())
        eval_rows.append({"endpoint": ep, "stage": "before", **before, "min_pairs_case_threshold": args.min_pairs_case})
        eval_rows.append({"endpoint": ep, "stage": "after", **after, "min_pairs_case_threshold": args.min_pairs_case})

        improve_bias = abs(after["bias"]) < abs(before["bias"])
        improve_mae = after["mae"] < before["mae"]
        improve_p10 = after["prop_abs_le_10"] > before["prop_abs_le_10"]
        improve_slope = abs(after["resid_vs_mean_slope"]) < abs(before["resid_vs_mean_slope"])
        suitability_rows.append(
            {
                "endpoint": ep,
                "bias_abs_before": abs(before["bias"]),
                "bias_abs_after": abs(after["bias"]),
                "mae_before": before["mae"],
                "mae_after": after["mae"],
                "prop_abs_le_10_before": before["prop_abs_le_10"],
                "prop_abs_le_10_after": after["prop_abs_le_10"],
                "abs_slope_before": abs(before["resid_vs_mean_slope"]),
                "abs_slope_after": abs(after["resid_vs_mean_slope"]),
                "improve_bias_abs": int(improve_bias),
                "improve_mae": int(improve_mae),
                "improve_prop_abs_le_10": int(improve_p10),
                "improve_abs_slope": int(improve_slope),
                "suitable_simple_delta": int(improve_bias and improve_mae and improve_p10),
            }
        )

        plot_endpoint(eval_df, ep, fig_dir, args.plot_max_points, args.seed + i * 101)

    case_all = pd.concat(case_rows, ignore_index=True)
    eval_all = pd.DataFrame(eval_rows)
    suit_all = pd.DataFrame(suitability_rows)

    case_all.to_csv(out_dir / "bp_case_delta_table_long.csv", index=False)
    eval_all.to_csv(out_dir / "bp_adjustment_eval_long.csv", index=False)
    suit_all.to_csv(out_dir / "bp_suitability_summary.csv", index=False)

    src_dist = (
        case_all.groupby(["endpoint", "source_level"], dropna=False, sort=False)["op_id"]
        .count()
        .rename("n_ops")
        .reset_index()
    )
    src_dist.to_csv(out_dir / "bp_source_level_distribution.csv", index=False)

    print(f"[INFO] wrote: {out_dir / 'bp_case_delta_table_long.csv'}")
    print(f"[INFO] wrote: {out_dir / 'bp_adjustment_eval_long.csv'}")
    print(f"[INFO] wrote: {out_dir / 'bp_suitability_summary.csv'}")
    print(f"[INFO] wrote: {out_dir / 'bp_source_level_distribution.csv'}")
    print(f"[INFO] figures: {fig_dir}")


if __name__ == "__main__":
    main()

