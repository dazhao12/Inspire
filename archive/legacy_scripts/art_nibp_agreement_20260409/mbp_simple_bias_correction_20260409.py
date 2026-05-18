#!/usr/bin/env python3
"""
MBP simple bias correction (fixed Delta + safeguards).

Rules:
  - Delta_case = median(ART - NIBP) within op_id on paired rows.
  - If n_pair >= min_pairs_case: use Delta_case.
  - Else fallback Delta_group by:
      department+antype -> department -> global.
  - Apply only when ART missing and NIBP available:
      mbp_adj = nibp_mbp + delta_used
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="MBP fixed-delta correction with fallback safeguards.")
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
            "art_nibp_agreement_20260409/output_mbp_simple_correction"
        ),
    )
    parser.add_argument("--min-pairs-case", type=int, default=5)
    parser.add_argument("--plot-max-points", type=int, default=80000)
    parser.add_argument("--seed", type=int, default=20260409)
    parser.add_argument("--audit-sample-n", type=int, default=30)
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
        "prop_abs_le_10": float(np.mean(abs_error <= 10.0)) if error.size else np.nan,
    }
    out["loa_low"] = out["bias"] - 1.96 * out["sd"] if np.isfinite(out["sd"]) else np.nan
    out["loa_high"] = out["bias"] + 1.96 * out["sd"] if np.isfinite(out["sd"]) else np.nan

    if error.size > 1 and np.nanstd(mean_pressure) > 0:
        slope, intercept = np.polyfit(mean_pressure, error, 1)
        pred = slope * mean_pressure + intercept
        ss_res = float(np.sum((error - pred) ** 2))
        ss_tot = float(np.sum((error - np.mean(error)) ** 2))
        r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else np.nan
        corr = float(np.corrcoef(mean_pressure, error)[0, 1])
        out["resid_vs_mean_slope"] = float(slope)
        out["resid_vs_mean_intercept"] = float(intercept)
        out["resid_vs_mean_corr"] = corr
        out["resid_vs_mean_r2"] = float(r2)
    else:
        out["resid_vs_mean_slope"] = np.nan
        out["resid_vs_mean_intercept"] = np.nan
        out["resid_vs_mean_corr"] = np.nan
        out["resid_vs_mean_r2"] = np.nan

    return out


def add_trimmed_metrics(prefix: str, error: np.ndarray, mean_pressure: np.ndarray, row: Dict[str, float]) -> None:
    error = np.asarray(error, dtype=float)
    mean_pressure = np.asarray(mean_pressure, dtype=float)
    ok = np.isfinite(error) & np.isfinite(mean_pressure)
    error = error[ok]
    mean_pressure = mean_pressure[ok]
    if error.size == 0:
        row[f"{prefix}_n_pairs"] = 0
        row[f"{prefix}_bias"] = np.nan
        row[f"{prefix}_mae"] = np.nan
        row[f"{prefix}_prop_abs_le_10"] = np.nan
        return

    lo = float(np.quantile(error, 0.005))
    hi = float(np.quantile(error, 0.995))
    keep = (error >= lo) & (error <= hi)
    err2 = error[keep]
    mean2 = mean_pressure[keep]
    met = compute_metrics(err2, mean2)
    row[f"{prefix}_n_pairs"] = met["n_pairs"]
    row[f"{prefix}_bias"] = met["bias"]
    row[f"{prefix}_mae"] = met["mae"]
    row[f"{prefix}_prop_abs_le_10"] = met["prop_abs_le_10"]


def sample_for_plot(df: pd.DataFrame, max_points: int, seed: int) -> pd.DataFrame:
    if len(df) <= max_points:
        return df
    rng = np.random.default_rng(seed)
    idx = rng.choice(len(df), size=max_points, replace=False)
    return df.iloc[idx].copy()


def main() -> None:
    args = parse_args()
    out_dir = Path(args.output_dir)
    fig_dir = out_dir / "figures"
    out_dir.mkdir(parents=True, exist_ok=True)
    fig_dir.mkdir(parents=True, exist_ok=True)

    print("[INFO] loading vitals...")
    vitals = pd.read_csv(
        args.vitals,
        usecols=["op_id", "subject_id", "min_from_entry", "art_mbp", "nibp_mbp"],
    )
    for c in ["op_id", "subject_id", "min_from_entry", "art_mbp", "nibp_mbp"]:
        vitals[c] = pd.to_numeric(vitals[c], errors="coerce")
    vitals = vitals.dropna(subset=["op_id", "min_from_entry"])
    print(f"[INFO] vitals rows: {len(vitals):,}")

    print("[INFO] loading operation metadata...")
    op_meta = pd.read_csv(args.op_meta, usecols=["op_id", "department", "antype"])
    op_meta["op_id"] = pd.to_numeric(op_meta["op_id"], errors="coerce")
    op_meta["department"] = op_meta["department"].fillna("Unknown").astype(str).replace("nan", "Unknown")
    op_meta["antype"] = op_meta["antype"].fillna("Unknown").astype(str).replace("nan", "Unknown")

    pair = vitals.loc[vitals["art_mbp"].notna() & vitals["nibp_mbp"].notna(), ["op_id", "art_mbp", "nibp_mbp"]].copy()
    pair["error"] = pair["art_mbp"] - pair["nibp_mbp"]
    print(f"[INFO] paired rows (ART+NIBP): {len(pair):,}")

    case_stats = (
        pair.groupby("op_id", sort=False)["error"]
        .agg(pair_n="size", delta_case="median")
        .reset_index()
    )
    case_stats["pair_n"] = case_stats["pair_n"].astype(int)

    all_ops = pd.DataFrame({"op_id": vitals["op_id"].dropna().drop_duplicates().to_numpy()})
    case = all_ops.merge(case_stats, on="op_id", how="left").merge(op_meta, on="op_id", how="left")
    case["pair_n"] = case["pair_n"].fillna(0).astype(int)
    case["department"] = case["department"].fillna("Unknown")
    case["antype"] = case["antype"].fillna("Unknown")

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

    case["delta_used"] = pd.to_numeric(case["delta_used"], errors="coerce")

    case_out = case[
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
    case_out = case_out.sort_values("op_id").reset_index(drop=True)
    case_out.to_csv(out_dir / "mbp_case_delta_table.csv", index=False)
    print(f"[INFO] wrote: {out_dir / 'mbp_case_delta_table.csv'}")

    rng = np.random.default_rng(args.seed)
    audit_n = min(args.audit_sample_n, len(case_out))
    audit = case_out.sample(n=audit_n, random_state=args.seed).sort_values("op_id")
    audit.to_csv(out_dir / "mbp_case_delta_audit_sample30.csv", index=False)

    adj = vitals.merge(case_out[["op_id", "delta_used", "source_level"]], on="op_id", how="left")
    apply_mask = adj["art_mbp"].isna() & adj["nibp_mbp"].notna()
    adj["mbp_adj"] = np.where(apply_mask, adj["nibp_mbp"] + adj["delta_used"], np.nan)
    adj["mbp_adj_flag"] = apply_mask.astype(int)
    adj["mbp_adj_source"] = np.where(apply_mask, adj["source_level"], "")
    adj["mbp_final_art_else_adj"] = np.where(adj["art_mbp"].notna(), adj["art_mbp"], adj["mbp_adj"])

    out_cols = [
        "op_id",
        "subject_id",
        "min_from_entry",
        "art_mbp",
        "nibp_mbp",
        "mbp_adj",
        "mbp_adj_flag",
        "mbp_adj_source",
        "mbp_final_art_else_adj",
    ]
    adj[out_cols].to_csv(out_dir / "mbp_adjusted_timeseries_research.csv", index=False)
    print(f"[INFO] wrote: {out_dir / 'mbp_adjusted_timeseries_research.csv'}")

    eval_df = adj.loc[adj["art_mbp"].notna() & adj["nibp_mbp"].notna(), ["op_id", "art_mbp", "nibp_mbp", "delta_used"]].copy()
    eval_df["error_before"] = eval_df["art_mbp"] - eval_df["nibp_mbp"]
    eval_df["nibp_after"] = eval_df["nibp_mbp"] + eval_df["delta_used"]
    eval_df["error_after"] = eval_df["art_mbp"] - eval_df["nibp_after"]
    eval_df["mean_before"] = (eval_df["art_mbp"] + eval_df["nibp_mbp"]) / 2.0
    eval_df["mean_after"] = (eval_df["art_mbp"] + eval_df["nibp_after"]) / 2.0

    before = compute_metrics(eval_df["error_before"].to_numpy(), eval_df["mean_before"].to_numpy())
    after = compute_metrics(eval_df["error_after"].to_numpy(), eval_df["mean_after"].to_numpy())

    row_before: Dict[str, float] = {"stage": "before_correction", **before}
    row_after: Dict[str, float] = {"stage": "after_correction", **after}
    add_trimmed_metrics("trim_0p5_99p5", eval_df["error_before"].to_numpy(), eval_df["mean_before"].to_numpy(), row_before)
    add_trimmed_metrics("trim_0p5_99p5", eval_df["error_after"].to_numpy(), eval_df["mean_after"].to_numpy(), row_after)

    eval_out = pd.DataFrame([row_before, row_after])
    eval_out["min_pairs_case_threshold"] = args.min_pairs_case
    eval_out["evaluation_note"] = "In-sample paired rows (optimistic); used for baseline screening."
    eval_out.to_csv(out_dir / "mbp_adjustment_eval.csv", index=False)
    print(f"[INFO] wrote: {out_dir / 'mbp_adjustment_eval.csv'}")

    # Plot: error histogram before/after
    plt.figure(figsize=(8, 6))
    plt.hist(eval_df["error_before"], bins=80, alpha=0.5, label="before", color="tab:gray")
    plt.hist(eval_df["error_after"], bins=80, alpha=0.5, label="after", color="tab:green")
    plt.axvline(eval_df["error_before"].mean(), color="tab:gray", linestyle="--", linewidth=1.2)
    plt.axvline(eval_df["error_after"].mean(), color="tab:green", linestyle="--", linewidth=1.2)
    plt.xlabel("Error (ART - MBP estimate), mmHg")
    plt.ylabel("Count")
    plt.title("MBP Error Histogram: Before vs After Correction")
    plt.legend()
    plt.tight_layout()
    plt.savefig(fig_dir / "mbp_error_hist_before_after.png", dpi=150)
    plt.close()

    # Plot: BA before/after
    plot_df = sample_for_plot(eval_df, args.plot_max_points, args.seed)
    fig, axes = plt.subplots(1, 2, figsize=(14, 6), sharey=True)
    for ax, mean_col, err_col, title, color in [
        (axes[0], "mean_before", "error_before", "Before Correction", "tab:gray"),
        (axes[1], "mean_after", "error_after", "After Correction", "tab:green"),
    ]:
        m = plot_df[mean_col].to_numpy(dtype=float)
        e = plot_df[err_col].to_numpy(dtype=float)
        bias = float(np.mean(e))
        sd = float(np.std(e, ddof=1))
        loa_l = bias - 1.96 * sd
        loa_h = bias + 1.96 * sd
        ax.scatter(m, e, s=5, alpha=0.18, linewidths=0, color=color)
        ax.axhline(bias, color="tab:blue", linewidth=1.4)
        ax.axhline(loa_l, color="tab:red", linestyle="--", linewidth=1.0)
        ax.axhline(loa_h, color="tab:red", linestyle="--", linewidth=1.0)
        ax.set_title(title)
        ax.set_xlabel("Mean pressure, mmHg")
    axes[0].set_ylabel("Error, mmHg")
    fig.suptitle("Bland-Altman: Before vs After MBP Correction")
    fig.tight_layout()
    fig.savefig(fig_dir / "mbp_bland_altman_before_after.png", dpi=150)
    plt.close(fig)

    # Plot: residual vs mean before/after
    fig, axes = plt.subplots(1, 2, figsize=(14, 6), sharey=True)
    for ax, mean_col, err_col, title, color in [
        (axes[0], "mean_before", "error_before", "Before Correction", "tab:gray"),
        (axes[1], "mean_after", "error_after", "After Correction", "tab:green"),
    ]:
        m = plot_df[mean_col].to_numpy(dtype=float)
        e = plot_df[err_col].to_numpy(dtype=float)
        slope, intercept = np.polyfit(m, e, 1)
        line_x = np.linspace(np.nanmin(m), np.nanmax(m), 100)
        line_y = slope * line_x + intercept
        ax.scatter(m, e, s=5, alpha=0.18, linewidths=0, color=color)
        ax.plot(line_x, line_y, color="tab:red", linewidth=1.5)
        ax.set_title(f"{title} (slope={slope:.3f})")
        ax.set_xlabel("Mean pressure, mmHg")
    axes[0].set_ylabel("Residual error, mmHg")
    fig.suptitle("Residual vs Mean Pressure: Before vs After MBP Correction")
    fig.tight_layout()
    fig.savefig(fig_dir / "mbp_residual_vs_mean_before_after.png", dpi=150)
    plt.close(fig)

    print(f"[INFO] figures: {fig_dir}")

    # Quick rule-check summary in stdout
    src_ct = case_out["source_level"].value_counts(dropna=False)
    print("[INFO] source_level distribution:")
    for k, v in src_ct.items():
        print(f"  - {k}: {int(v):,}")


if __name__ == "__main__":
    main()

