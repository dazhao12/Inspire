#!/usr/bin/env python3
"""
ART vs NIBP agreement evaluation (SBP/MBP/DBP).

Outputs:
  - agreement_summary.csv
  - clinical_thresholds.csv
  - heterogeneity_by_op.csv
  - figures/*.png

Primary pairing:
  - same-row pairing
Sensitivity pairing:
  - nearest ART to each NIBP within +/- 5 minutes (within op_id)
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ENDPOINTS = ("sbp", "mbp", "dbp")
THRESHOLDS = (5, 10, 15)
SEED_OFFSET = {
    ("same_row", "sbp"): 11,
    ("same_row", "mbp"): 13,
    ("same_row", "dbp"): 17,
    ("nearest_5min", "sbp"): 19,
    ("nearest_5min", "mbp"): 23,
    ("nearest_5min", "dbp"): 29,
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Evaluate ART vs NIBP agreement.")
    parser.add_argument(
        "--input",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/"
            "version_split/cleaned_no_imputation/"
            "intraop_vitals_clean_before_impute_latest.csv"
        ),
        help="Input CSV path.",
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/"
            "art_nibp_agreement_20260409/output"
        ),
        help="Output directory.",
    )
    parser.add_argument(
        "--bootstrap-reps",
        type=int,
        default=400,
        help="Number of cluster-bootstrap repetitions.",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=20260409,
        help="Random seed.",
    )
    parser.add_argument(
        "--window-min",
        type=float,
        default=5.0,
        help="Sensitivity nearest-neighbor time window in minutes.",
    )
    parser.add_argument(
        "--plot-max-points",
        type=int,
        default=80000,
        help="Maximum points to draw in BA scatter.",
    )
    parser.add_argument(
        "--min-op-pairs",
        type=int,
        default=5,
        help="Minimum paired points per op_id for op-level heterogeneity summary.",
    )
    return parser.parse_args()


def _safe_quantile(x: np.ndarray, q: float) -> float:
    if x.size == 0:
        return math.nan
    return float(np.quantile(x, q))


def load_input(path: str) -> pd.DataFrame:
    usecols = ["op_id", "min_from_entry"]
    for ep in ENDPOINTS:
        usecols.extend((f"art_{ep}", f"nibp_{ep}"))

    df = pd.read_csv(path, usecols=usecols)
    df["op_id"] = pd.to_numeric(df["op_id"], errors="coerce")
    df["min_from_entry"] = pd.to_numeric(df["min_from_entry"], errors="coerce")
    for ep in ENDPOINTS:
        df[f"art_{ep}"] = pd.to_numeric(df[f"art_{ep}"], errors="coerce")
        df[f"nibp_{ep}"] = pd.to_numeric(df[f"nibp_{ep}"], errors="coerce")
    return df


def build_same_row_pairs(df: pd.DataFrame, endpoint: str) -> pd.DataFrame:
    art_col = f"art_{endpoint}"
    nibp_col = f"nibp_{endpoint}"
    sub = df.loc[df[art_col].notna() & df[nibp_col].notna(), ["op_id", art_col, nibp_col]].copy()
    if sub.empty:
        return pd.DataFrame(
            columns=[
                "op_id",
                "endpoint",
                "pairing_strategy",
                "art_value",
                "nibp_value",
                "mean_pressure",
                "error",
                "abs_error",
                "time_delta_min",
            ]
        )
    sub = sub.rename(columns={art_col: "art_value", nibp_col: "nibp_value"})
    sub["endpoint"] = endpoint
    sub["pairing_strategy"] = "same_row"
    sub["mean_pressure"] = (sub["art_value"] + sub["nibp_value"]) / 2.0
    sub["error"] = sub["art_value"] - sub["nibp_value"]
    sub["abs_error"] = sub["error"].abs()
    sub["time_delta_min"] = 0.0
    return sub[
        [
            "op_id",
            "endpoint",
            "pairing_strategy",
            "art_value",
            "nibp_value",
            "mean_pressure",
            "error",
            "abs_error",
            "time_delta_min",
        ]
    ]


def build_nearest_pairs_all(df: pd.DataFrame, window_min: float) -> pd.DataFrame:
    cols = ["op_id", "min_from_entry"]
    for ep in ENDPOINTS:
        cols.extend((f"art_{ep}", f"nibp_{ep}"))
    sub = df[cols].dropna(subset=["op_id", "min_from_entry"]).copy()
    sub["op_id"] = pd.to_numeric(sub["op_id"], errors="coerce")
    sub["min_from_entry"] = pd.to_numeric(sub["min_from_entry"], errors="coerce")
    sub = sub.dropna(subset=["op_id", "min_from_entry"])

    out_frames: List[pd.DataFrame] = []
    print(f"[INFO] building nearest pairs with merge_asof (window={window_min} min)")
    for ep in ENDPOINTS:
        art_col = f"art_{ep}"
        nibp_col = f"nibp_{ep}"

        art = (
            sub.loc[sub[art_col].notna(), ["op_id", "min_from_entry", art_col]]
            .rename(columns={"min_from_entry": "art_time", art_col: "art_value"})
            .reset_index(drop=True)
        )
        art["art_time"] = pd.to_numeric(art["art_time"], errors="coerce").astype(float)
        art = art.dropna(subset=["op_id", "art_time"]).sort_values(["art_time", "op_id"]).reset_index(drop=True)
        nibp = (
            sub.loc[sub[nibp_col].notna(), ["op_id", "min_from_entry", nibp_col]]
            .rename(columns={"min_from_entry": "nibp_time", nibp_col: "nibp_value"})
            .reset_index(drop=True)
        )
        nibp["nibp_time"] = pd.to_numeric(nibp["nibp_time"], errors="coerce").astype(float)
        nibp = nibp.dropna(subset=["op_id", "nibp_time"]).sort_values(["nibp_time", "op_id"]).reset_index(drop=True)
        if art.empty or nibp.empty:
            continue

        merged = pd.merge_asof(
            nibp,
            art,
            left_on="nibp_time",
            right_on="art_time",
            by="op_id",
            direction="nearest",
            tolerance=window_min,
            allow_exact_matches=True,
        )
        merged = merged.dropna(subset=["art_value"])
        if merged.empty:
            continue

        merged["endpoint"] = ep
        merged["pairing_strategy"] = "nearest_5min"
        merged["time_delta_min"] = merged["nibp_time"] - merged["art_time"]
        merged["mean_pressure"] = (merged["art_value"] + merged["nibp_value"]) / 2.0
        merged["error"] = merged["art_value"] - merged["nibp_value"]
        merged["abs_error"] = merged["error"].abs()
        out_frames.append(
            merged[
                [
                    "op_id",
                    "endpoint",
                    "pairing_strategy",
                    "art_value",
                    "nibp_value",
                    "mean_pressure",
                    "error",
                    "abs_error",
                    "time_delta_min",
                ]
            ]
        )
        print(f"[INFO] nearest pairs {ep}: {len(merged):,}")

    if not out_frames:
        return pd.DataFrame(
            columns=[
                "op_id",
                "endpoint",
                "pairing_strategy",
                "art_value",
                "nibp_value",
                "mean_pressure",
                "error",
                "abs_error",
                "time_delta_min",
            ]
        )
    out = pd.concat(out_frames, ignore_index=True)
    return out[
        [
            "op_id",
            "endpoint",
            "pairing_strategy",
            "art_value",
            "nibp_value",
            "mean_pressure",
            "error",
            "abs_error",
            "time_delta_min",
        ]
    ]


def cluster_bootstrap_ba_ci(
    pair_df: pd.DataFrame, reps: int, seed: int
) -> Dict[str, float]:
    work = pair_df[["op_id", "error"]].copy()
    work["error_sq"] = work["error"] * work["error"]
    agg = (
        work.groupby("op_id", sort=False)
        .agg(n=("error", "size"), s=("error", "sum"), ss=("error_sq", "sum"))
        .reset_index(drop=True)
    )
    n_i = agg["n"].to_numpy(dtype=np.int64)
    s_i = agg["s"].to_numpy(dtype=float)
    ss_i = agg["ss"].to_numpy(dtype=float)
    m = len(agg)

    rng = np.random.default_rng(seed)
    boot_bias = np.empty(reps, dtype=float)
    boot_sd = np.empty(reps, dtype=float)
    boot_loa_low = np.empty(reps, dtype=float)
    boot_loa_high = np.empty(reps, dtype=float)

    for b in range(reps):
        sampled = rng.integers(0, m, size=m)
        w = np.bincount(sampled, minlength=m).astype(np.int64)
        total_n = int(np.dot(w, n_i))
        if total_n <= 1:
            boot_bias[b] = np.nan
            boot_sd[b] = np.nan
            boot_loa_low[b] = np.nan
            boot_loa_high[b] = np.nan
            continue

        total_s = float(np.dot(w, s_i))
        total_ss = float(np.dot(w, ss_i))
        bias = total_s / total_n
        var = max((total_ss - total_n * bias * bias) / (total_n - 1), 0.0)
        sd = math.sqrt(var)
        boot_bias[b] = bias
        boot_sd[b] = sd
        boot_loa_low[b] = bias - 1.96 * sd
        boot_loa_high[b] = bias + 1.96 * sd

    mask = np.isfinite(boot_bias) & np.isfinite(boot_sd)
    if not np.any(mask):
        return {
            "bias_ci_low": math.nan,
            "bias_ci_high": math.nan,
            "sd_ci_low": math.nan,
            "sd_ci_high": math.nan,
            "loa_low_ci_low": math.nan,
            "loa_low_ci_high": math.nan,
            "loa_high_ci_low": math.nan,
            "loa_high_ci_high": math.nan,
        }

    return {
        "bias_ci_low": _safe_quantile(boot_bias[mask], 0.025),
        "bias_ci_high": _safe_quantile(boot_bias[mask], 0.975),
        "sd_ci_low": _safe_quantile(boot_sd[mask], 0.025),
        "sd_ci_high": _safe_quantile(boot_sd[mask], 0.975),
        "loa_low_ci_low": _safe_quantile(boot_loa_low[mask], 0.025),
        "loa_low_ci_high": _safe_quantile(boot_loa_low[mask], 0.975),
        "loa_high_ci_low": _safe_quantile(boot_loa_high[mask], 0.025),
        "loa_high_ci_high": _safe_quantile(boot_loa_high[mask], 0.975),
    }


def compute_metrics(pair_df: pd.DataFrame) -> Dict[str, float]:
    error = pair_df["error"].to_numpy(dtype=float)
    abs_error = np.abs(error)
    mean_pressure = pair_df["mean_pressure"].to_numpy(dtype=float)

    n = int(error.size)
    out: Dict[str, float] = {
        "n_pairs": n,
        "n_ops": int(pair_df["op_id"].nunique()),
        "bias": float(np.mean(error)) if n else math.nan,
        "sd": float(np.std(error, ddof=1)) if n > 1 else math.nan,
        "mae": float(np.mean(abs_error)) if n else math.nan,
        "medae": float(np.median(abs_error)) if n else math.nan,
        "p90ae": _safe_quantile(abs_error, 0.90),
        "p95ae": _safe_quantile(abs_error, 0.95),
    }
    out["loa_low"] = out["bias"] - 1.96 * out["sd"] if np.isfinite(out["sd"]) else math.nan
    out["loa_high"] = out["bias"] + 1.96 * out["sd"] if np.isfinite(out["sd"]) else math.nan

    for t in THRESHOLDS:
        out[f"prop_abs_le_{t}"] = float(np.mean(abs_error <= t)) if n else math.nan

    if n >= 2 and np.nanstd(mean_pressure) > 0:
        slope, intercept = np.polyfit(mean_pressure, error, 1)
        pred = slope * mean_pressure + intercept
        ss_res = float(np.sum((error - pred) ** 2))
        ss_tot = float(np.sum((error - np.mean(error)) ** 2))
        r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else math.nan
        out["proportional_bias_slope"] = float(slope)
        out["proportional_bias_intercept"] = float(intercept)
        out["proportional_bias_r2"] = float(r2)
    else:
        out["proportional_bias_slope"] = math.nan
        out["proportional_bias_intercept"] = math.nan
        out["proportional_bias_r2"] = math.nan

    return out


def summarize_heterogeneity(
    pair_df: pd.DataFrame, min_pairs: int
) -> Tuple[pd.DataFrame, Dict[str, float]]:
    op = (
        pair_df.groupby("op_id", sort=False)
        .agg(
            n_pairs=("error", "size"),
            op_bias=("error", "mean"),
            op_sd=("error", "std"),
            op_mae=("abs_error", "mean"),
            op_medae=("abs_error", "median"),
        )
        .reset_index()
    )
    op_ge = op.loc[op["n_pairs"] >= min_pairs].copy()
    summary = {
        "op_count_ge_min_pairs": int(len(op_ge)),
        "op_bias_p10": _safe_quantile(op_ge["op_bias"].to_numpy(dtype=float), 0.10),
        "op_bias_p50": _safe_quantile(op_ge["op_bias"].to_numpy(dtype=float), 0.50),
        "op_bias_p90": _safe_quantile(op_ge["op_bias"].to_numpy(dtype=float), 0.90),
        "op_sd_p10": _safe_quantile(op_ge["op_sd"].dropna().to_numpy(dtype=float), 0.10),
        "op_sd_p50": _safe_quantile(op_ge["op_sd"].dropna().to_numpy(dtype=float), 0.50),
        "op_sd_p90": _safe_quantile(op_ge["op_sd"].dropna().to_numpy(dtype=float), 0.90),
    }
    return op, summary


def build_binned_summary(pair_df: pd.DataFrame, bins: int = 5) -> pd.DataFrame:
    temp = pair_df[["mean_pressure", "error"]].copy()
    if temp.empty:
        return pd.DataFrame(columns=["mean_bin", "n", "error_mean", "error_median", "error_p10", "error_p90"])
    try:
        labels = pd.qcut(temp["mean_pressure"], q=bins, duplicates="drop")
    except ValueError:
        return pd.DataFrame(columns=["mean_bin", "n", "error_mean", "error_median", "error_p10", "error_p90"])
    temp["mean_bin"] = labels.astype(str)
    out = (
        temp.groupby("mean_bin", sort=False)["error"]
        .agg(
            n="size",
            error_mean="mean",
            error_median="median",
            error_p10=lambda s: s.quantile(0.10),
            error_p90=lambda s: s.quantile(0.90),
        )
        .reset_index()
    )
    return out


def make_bland_altman_plot(
    pair_df: pd.DataFrame, title: str, out_path: Path, max_points: int, seed: int
) -> None:
    rng = np.random.default_rng(seed)
    draw = pair_df
    if len(draw) > max_points:
        idx = rng.choice(len(draw), size=max_points, replace=False)
        draw = draw.iloc[idx]

    bias = float(pair_df["error"].mean())
    sd = float(pair_df["error"].std(ddof=1))
    loa_low = bias - 1.96 * sd
    loa_high = bias + 1.96 * sd

    plt.figure(figsize=(8, 6))
    plt.scatter(draw["mean_pressure"], draw["error"], s=5, alpha=0.15, linewidths=0)
    plt.axhline(bias, color="tab:blue", linestyle="-", linewidth=1.5, label=f"Bias={bias:.2f}")
    plt.axhline(loa_low, color="tab:red", linestyle="--", linewidth=1.2, label=f"LOA low={loa_low:.2f}")
    plt.axhline(loa_high, color="tab:red", linestyle="--", linewidth=1.2, label=f"LOA high={loa_high:.2f}")
    plt.xlabel("Mean pressure (ART+NIBP)/2, mmHg")
    plt.ylabel("Error (ART-NIBP), mmHg")
    plt.title(title)
    plt.legend(loc="best", fontsize=9)
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def make_error_hist_plot(pair_df: pd.DataFrame, title: str, out_path: Path) -> None:
    plt.figure(figsize=(8, 6))
    plt.hist(pair_df["error"].to_numpy(dtype=float), bins=80, color="tab:gray", alpha=0.85)
    plt.axvline(pair_df["error"].mean(), color="tab:blue", linestyle="-", linewidth=1.4)
    plt.xlabel("Error (ART-NIBP), mmHg")
    plt.ylabel("Count")
    plt.title(title)
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def make_binned_box_plot(pair_df: pd.DataFrame, title: str, out_path: Path, bins: int = 5) -> None:
    temp = pair_df[["mean_pressure", "error"]].copy()
    try:
        temp["bin"] = pd.qcut(temp["mean_pressure"], q=bins, duplicates="drop")
    except ValueError:
        return
    temp["bin_label"] = temp["bin"].astype(str)
    order = list(dict.fromkeys(temp["bin_label"].tolist()))
    data = [temp.loc[temp["bin_label"] == lb, "error"].to_numpy(dtype=float) for lb in order]
    if not data:
        return

    plt.figure(figsize=(10, 6))
    plt.boxplot(data, tick_labels=order, showfliers=False)
    plt.xticks(rotation=30, ha="right")
    plt.xlabel("Mean pressure quantile bin")
    plt.ylabel("Error (ART-NIBP), mmHg")
    plt.title(title)
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def run(args: argparse.Namespace) -> None:
    out_dir = Path(args.output_dir)
    fig_dir = out_dir / "figures"
    out_dir.mkdir(parents=True, exist_ok=True)
    fig_dir.mkdir(parents=True, exist_ok=True)

    print("[INFO] loading input data...")
    df = load_input(args.input)
    print(f"[INFO] rows loaded: {len(df):,}")

    all_pairs: List[pd.DataFrame] = []
    for ep in ENDPOINTS:
        same = build_same_row_pairs(df, ep)
        print(f"[INFO] same-row pairs {ep}: {len(same):,}")
        all_pairs.append(same)

    near_all = build_nearest_pairs_all(df, args.window_min)
    all_pairs.append(near_all)

    all_pairs_df = pd.concat(all_pairs, ignore_index=True)

    agreement_rows: List[Dict[str, float]] = []
    threshold_rows: List[Dict[str, float]] = []
    heter_rows: List[pd.DataFrame] = []
    binned_rows: List[pd.DataFrame] = []

    for strategy in ("same_row", "nearest_5min"):
        for ep in ENDPOINTS:
            pair_df = all_pairs_df.loc[
                (all_pairs_df["pairing_strategy"] == strategy) & (all_pairs_df["endpoint"] == ep)
            ].copy()
            if pair_df.empty:
                continue

            metrics = compute_metrics(pair_df)
            ci_seed = args.seed + SEED_OFFSET[(strategy, ep)]
            ci = cluster_bootstrap_ba_ci(pair_df, reps=args.bootstrap_reps, seed=ci_seed)
            op_detail, heter_summary = summarize_heterogeneity(pair_df, min_pairs=args.min_op_pairs)
            binned = build_binned_summary(pair_df, bins=5)

            trimmed = pair_df.copy()
            lo = trimmed["error"].quantile(0.005)
            hi = trimmed["error"].quantile(0.995)
            trimmed = trimmed.loc[(trimmed["error"] >= lo) & (trimmed["error"] <= hi)]
            trim_metrics = compute_metrics(trimmed)

            agreement_row = {
                "pairing_strategy": strategy,
                "endpoint": ep,
                **metrics,
                **ci,
                **heter_summary,
                "trim_0p5_99p5_bias": trim_metrics["bias"],
                "trim_0p5_99p5_sd": trim_metrics["sd"],
                "trim_0p5_99p5_loa_low": trim_metrics["loa_low"],
                "trim_0p5_99p5_loa_high": trim_metrics["loa_high"],
                "time_window_min": args.window_min if strategy == "nearest_5min" else 0.0,
            }
            agreement_rows.append(agreement_row)

            for t in THRESHOLDS:
                threshold_rows.append(
                    {
                        "pairing_strategy": strategy,
                        "endpoint": ep,
                        "threshold_mmHg": t,
                        "n_pairs": metrics["n_pairs"],
                        "n_within_threshold": int((pair_df["abs_error"] <= t).sum()),
                        "proportion_within_threshold": metrics[f"prop_abs_le_{t}"],
                    }
                )

            op_detail["pairing_strategy"] = strategy
            op_detail["endpoint"] = ep
            heter_rows.append(op_detail)

            if not binned.empty:
                binned["pairing_strategy"] = strategy
                binned["endpoint"] = ep
                binned_rows.append(binned)

            title_prefix = f"{ep.upper()} | {strategy}"
            make_bland_altman_plot(
                pair_df,
                title=f"Bland-Altman: {title_prefix}",
                out_path=fig_dir / f"{strategy}_{ep}_bland_altman.png",
                max_points=args.plot_max_points,
                seed=args.seed,
            )
            make_error_hist_plot(
                pair_df,
                title=f"Error Histogram: {title_prefix}",
                out_path=fig_dir / f"{strategy}_{ep}_error_hist.png",
            )
            make_binned_box_plot(
                pair_df,
                title=f"Error by Mean-Pressure Bin: {title_prefix}",
                out_path=fig_dir / f"{strategy}_{ep}_error_box_by_mean_bin.png",
                bins=5,
            )

    agreement_df = pd.DataFrame(agreement_rows)
    thresholds_df = pd.DataFrame(threshold_rows)
    heter_df = pd.concat(heter_rows, ignore_index=True) if heter_rows else pd.DataFrame()
    binned_df = pd.concat(binned_rows, ignore_index=True) if binned_rows else pd.DataFrame()

    agreement_df = agreement_df.sort_values(["pairing_strategy", "endpoint"]).reset_index(drop=True)
    thresholds_df = thresholds_df.sort_values(["pairing_strategy", "endpoint", "threshold_mmHg"]).reset_index(drop=True)
    if not heter_df.empty:
        heter_df = heter_df.sort_values(["pairing_strategy", "endpoint", "op_id"]).reset_index(drop=True)

    agreement_path = out_dir / "agreement_summary.csv"
    thresholds_path = out_dir / "clinical_thresholds.csv"
    heter_path = out_dir / "heterogeneity_by_op.csv"
    binned_path = out_dir / "proportional_bias_binned_summary.csv"

    agreement_df.to_csv(agreement_path, index=False)
    thresholds_df.to_csv(thresholds_path, index=False)
    heter_df.to_csv(heter_path, index=False)
    binned_df.to_csv(binned_path, index=False)

    print(f"[INFO] wrote: {agreement_path}")
    print(f"[INFO] wrote: {thresholds_path}")
    print(f"[INFO] wrote: {heter_path}")
    print(f"[INFO] wrote: {binned_path}")
    print(f"[INFO] figures dir: {fig_dir}")


def main() -> None:
    args = parse_args()
    run(args)


if __name__ == "__main__":
    main()
