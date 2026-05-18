#!/usr/bin/env python3
"""
Generate PPT-ready figure pack for ART/NIBP calibration.
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
import numpy as np
import pandas as pd


ENDPOINTS = ("sbp", "mbp", "dbp")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate PPT-ready calibration figures.")
    parser.add_argument(
        "--calibrated-table",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/"
            "cleaned_no_imputation/intraop_vitals_clean_before_impute_with_calibrated_nibp_artfirst_latest.csv"
        ),
    )
    parser.add_argument(
        "--bp-suitability-dir",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/"
            "art_nibp_agreement_20260409/output_bp_suitability"
        ),
    )
    parser.add_argument(
        "--out-dir",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/"
            "art_nibp_agreement_20260409/output_bp_suitability/ppt_figures_20260409"
        ),
    )
    parser.add_argument("--max-points-per-endpoint", type=int, default=80000)
    parser.add_argument("--seed", type=int, default=20260409)
    parser.add_argument("--min-pairs-case", type=int, default=5)
    return parser.parse_args()


def draw_workflow(out_path: Path, min_pairs_case: int) -> None:
    fig, ax = plt.subplots(figsize=(14, 7))
    ax.set_axis_off()
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)

    def box(x: float, y: float, w: float, h: float, text: str, fc: str) -> None:
        patch = FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.02,rounding_size=0.03", fc=fc, ec="black", lw=1.2)
        ax.add_patch(patch)
        ax.text(x + w / 2, y + h / 2, text, ha="center", va="center", fontsize=11)

    box(0.05, 0.70, 0.20, 0.18, "Input data\nART + NIBP\n(SBP/MBP/DBP)", "#dbeafe")
    box(0.30, 0.70, 0.22, 0.18, "Paired rows\nerror = ART - NIBP\nby endpoint", "#e0f2fe")
    box(0.58, 0.70, 0.20, 0.18, "Case delta\nmedian(error)\nby op_id", "#dcfce7")
    box(0.80, 0.70, 0.15, 0.18, f"Threshold\nn_pair >= {min_pairs_case} ?", "#fef9c3")

    box(0.58, 0.42, 0.18, 0.16, "Use delta_case\n(if >=5 pairs)", "#bbf7d0")
    box(0.80, 0.42, 0.15, 0.16, "Fallback\nDept+AnType\n-> Dept -> Global", "#fde68a")

    box(0.26, 0.18, 0.30, 0.16, "Calibrate NIBP\nNIBP_cal = NIBP + delta_used", "#f3e8ff")
    box(0.60, 0.18, 0.33, 0.16, "Final merged BP\nART-first, else NIBP_cal", "#ede9fe")

    ax.annotate("", xy=(0.30, 0.79), xytext=(0.25, 0.79), arrowprops=dict(arrowstyle="->", lw=1.6))
    ax.annotate("", xy=(0.58, 0.79), xytext=(0.52, 0.79), arrowprops=dict(arrowstyle="->", lw=1.6))
    ax.annotate("", xy=(0.80, 0.79), xytext=(0.78, 0.79), arrowprops=dict(arrowstyle="->", lw=1.6))
    ax.annotate("", xy=(0.67, 0.58), xytext=(0.87, 0.70), arrowprops=dict(arrowstyle="->", lw=1.4))
    ax.annotate("", xy=(0.87, 0.58), xytext=(0.87, 0.70), arrowprops=dict(arrowstyle="->", lw=1.4))
    ax.annotate("", xy=(0.42, 0.34), xytext=(0.67, 0.42), arrowprops=dict(arrowstyle="->", lw=1.5))
    ax.annotate("", xy=(0.42, 0.34), xytext=(0.87, 0.42), arrowprops=dict(arrowstyle="->", lw=1.5))
    ax.annotate("", xy=(0.60, 0.26), xytext=(0.56, 0.26), arrowprops=dict(arrowstyle="->", lw=1.5))

    ax.set_title(
        f"ART/NIBP Error Calibration Workflow (Fixed-Delta + Safeguards, min_pairs={min_pairs_case})",
        fontsize=16,
        pad=18,
    )
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)


def plot_metric_comparison(suit_df: pd.DataFrame, out_path: Path) -> None:
    eps = [x.upper() for x in suit_df["endpoint"].tolist()]
    x = np.arange(len(eps))
    width = 0.36

    fig, axes = plt.subplots(2, 2, figsize=(14, 9))
    axes = axes.flatten()
    metrics = [
        ("bias_abs_before", "bias_abs_after", "|Bias| (mmHg)"),
        ("mae_before", "mae_after", "MAE (mmHg)"),
        ("prop_abs_le_10_before", "prop_abs_le_10_after", "Proportion |error| <= 10"),
        ("abs_slope_before", "abs_slope_after", "|Residual~Mean Slope|"),
    ]
    for ax, (b, a, title) in zip(axes, metrics):
        ax.bar(x - width / 2, suit_df[b], width, label="Before", color="#94a3b8")
        ax.bar(x + width / 2, suit_df[a], width, label="After", color="#22c55e")
        ax.set_xticks(x)
        ax.set_xticklabels(eps)
        ax.set_title(title)
        ax.grid(axis="y", alpha=0.25)
    axes[0].legend(loc="best")
    fig.suptitle("Calibration Impact by Endpoint", fontsize=16)
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)


def plot_source_distribution(src_df: pd.DataFrame, out_path: Path) -> None:
    piv = src_df.pivot(index="endpoint", columns="source_level", values="n_ops").fillna(0)
    piv = piv.reindex(index=["sbp", "mbp", "dbp"])
    order = ["case", "department_antype", "department", "global"]
    for c in order:
        if c not in piv.columns:
            piv[c] = 0
    piv = piv[order]

    fig, ax = plt.subplots(figsize=(10, 7))
    bottom = np.zeros(len(piv))
    colors = {
        "case": "#2563eb",
        "department_antype": "#16a34a",
        "department": "#f59e0b",
        "global": "#ef4444",
    }
    x = np.arange(len(piv))
    for c in order:
        y = piv[c].to_numpy(dtype=float)
        ax.bar(x, y, bottom=bottom, label=c, color=colors[c])
        bottom += y

    ax.set_xticks(x)
    ax.set_xticklabels([i.upper() for i in piv.index.tolist()])
    ax.set_ylabel("Number of operations")
    ax.set_title("Delta Source Level Distribution")
    ax.legend(loc="best")
    ax.grid(axis="y", alpha=0.25)
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)


def plot_delta_distribution(case_long: pd.DataFrame, out_path: Path) -> None:
    fig, axes = plt.subplots(1, 3, figsize=(15, 4.8), sharey=True)
    for ax, ep in zip(axes, ENDPOINTS):
        d = case_long.loc[case_long["endpoint"] == ep, "delta_used"].dropna()
        ax.hist(d, bins=80, color="#6366f1", alpha=0.8)
        ax.axvline(d.median(), color="black", linestyle="--", linewidth=1.2)
        ax.set_title(ep.upper())
        ax.set_xlabel("delta_used (mmHg)")
        ax.grid(axis="y", alpha=0.2)
    axes[0].set_ylabel("Operations")
    fig.suptitle("Distribution of Applied Delta by Endpoint", fontsize=14)
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)


def combine_existing_distribution_panels(existing_dir: Path, out_path: Path) -> None:
    files = []
    for ep in ENDPOINTS:
        files.extend(
            [
                existing_dir / f"{ep}_value_distribution_before_after.png",
                existing_dir / f"{ep}_error_distribution_before_after.png",
                existing_dir / f"{ep}_difference_distribution.png",
            ]
        )

    fig, axes = plt.subplots(3, 3, figsize=(16, 13))
    for ax, fp in zip(axes.flatten(), files):
        img = plt.imread(fp)
        ax.imshow(img)
        ax.set_axis_off()
        ax.set_title(fp.stem, fontsize=10)
    fig.suptitle("Per-endpoint Distribution Panels", fontsize=16)
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)


def collect_sample_for_scatter(cal_table: Path, max_points_per_ep: int, seed: int) -> Dict[str, pd.DataFrame]:
    cols = []
    for ep in ENDPOINTS:
        cols.extend([f"art_{ep}", f"nibp_{ep}", f"nibp_{ep}_calibrated"])
    rng = np.random.default_rng(seed)
    out: Dict[str, List[pd.DataFrame]] = {ep: [] for ep in ENDPOINTS}

    # Use random sampling per chunk; target >= max_points then trim.
    for chunk in pd.read_csv(cal_table, usecols=cols, chunksize=500000):
        for ep in ENDPOINTS:
            art = chunk[f"art_{ep}"]
            nibp = chunk[f"nibp_{ep}"]
            cal = chunk[f"nibp_{ep}_calibrated"]
            m = art.notna() & nibp.notna() & cal.notna()
            if not m.any():
                continue
            d = pd.DataFrame(
                {
                    "art": art[m].to_numpy(dtype=float),
                    "before": nibp[m].to_numpy(dtype=float),
                    "after": cal[m].to_numpy(dtype=float),
                }
            )
            # keep approx 25% first, later trim
            keep = rng.random(len(d)) < 0.25
            d = d.loc[keep]
            if not d.empty:
                out[ep].append(d)

    ret: Dict[str, pd.DataFrame] = {}
    for i, ep in enumerate(ENDPOINTS):
        if not out[ep]:
            ret[ep] = pd.DataFrame(columns=["art", "before", "after"])
            continue
        d = pd.concat(out[ep], ignore_index=True)
        if len(d) > max_points_per_ep:
            rng2 = np.random.default_rng(seed + i * 131 + 7)
            idx = rng2.choice(len(d), size=max_points_per_ep, replace=False)
            d = d.iloc[idx].copy()
        ret[ep] = d
    return ret


def plot_bland_altman_grid(samples: Dict[str, pd.DataFrame], eval_df: pd.DataFrame, out_path: Path) -> None:
    fig, axes = plt.subplots(3, 2, figsize=(13, 16), sharex=False, sharey=False)
    for r, ep in enumerate(ENDPOINTS):
        d = samples[ep]
        if d.empty:
            continue

        d["err_before"] = d["art"] - d["before"]
        d["err_after"] = d["art"] - d["after"]
        d["mean_before"] = (d["art"] + d["before"]) / 2.0
        d["mean_after"] = (d["art"] + d["after"]) / 2.0

        for c, stage in enumerate(["before", "after"]):
            ax = axes[r, c]
            rr = eval_df[(eval_df["endpoint"] == ep) & (eval_df["stage"] == stage)].iloc[0]
            if stage == "before":
                mean_x = d["mean_before"].to_numpy()
                err = d["err_before"].to_numpy()
                color = "#64748b"
            else:
                mean_x = d["mean_after"].to_numpy()
                err = d["err_after"].to_numpy()
                color = "#16a34a"
            ax.scatter(mean_x, err, s=5, alpha=0.15, linewidths=0, color=color)
            ax.axhline(rr["bias"], color="tab:blue", linewidth=1.4)
            ax.axhline(rr["loa_low"], color="tab:red", linestyle="--", linewidth=1.0)
            ax.axhline(rr["loa_high"], color="tab:red", linestyle="--", linewidth=1.0)
            ax.set_title(f"{ep.upper()} - {stage}")
            ax.set_xlabel("Mean pressure (mmHg)")
            ax.set_ylabel("Error (ART - estimate)")
            ax.grid(alpha=0.15)

    fig.suptitle("Bland-Altman Before vs After (SBP/MBP/DBP)", fontsize=16)
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)


def plot_residual_mean_grid(samples: Dict[str, pd.DataFrame], eval_df: pd.DataFrame, out_path: Path) -> None:
    fig, axes = plt.subplots(3, 2, figsize=(13, 16), sharex=False, sharey=False)
    for r, ep in enumerate(ENDPOINTS):
        d = samples[ep]
        if d.empty:
            continue

        d["err_before"] = d["art"] - d["before"]
        d["err_after"] = d["art"] - d["after"]
        d["mean_before"] = (d["art"] + d["before"]) / 2.0
        d["mean_after"] = (d["art"] + d["after"]) / 2.0

        for c, stage in enumerate(["before", "after"]):
            ax = axes[r, c]
            if stage == "before":
                x = d["mean_before"].to_numpy()
                y = d["err_before"].to_numpy()
                color = "#64748b"
            else:
                x = d["mean_after"].to_numpy()
                y = d["err_after"].to_numpy()
                color = "#16a34a"
            slope, intercept = np.polyfit(x, y, 1)
            xl = np.linspace(np.nanmin(x), np.nanmax(x), 100)
            yl = slope * xl + intercept
            corr = float(np.corrcoef(x, y)[0, 1])
            ax.scatter(x, y, s=5, alpha=0.15, linewidths=0, color=color)
            ax.plot(xl, yl, color="tab:red", linewidth=1.4)
            ax.set_title(f"{ep.upper()} - {stage} (slope={slope:.3f}, corr={corr:.3f})")
            ax.set_xlabel("Mean pressure (mmHg)")
            ax.set_ylabel("Residual error")
            ax.grid(alpha=0.15)

    fig.suptitle("Residual vs Mean Before vs After (SBP/MBP/DBP)", fontsize=16)
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)


def write_index(out_dir: Path) -> None:
    rows = [
        ("01_workflow_fixed_delta.png", "Method workflow", "How calibration is built and applied"),
        ("02_metric_comparison_before_after.png", "Core metric improvement", "Bias/MAE/|error|<=10/slope comparison"),
        ("03_source_level_distribution.png", "Delta source structure", "How often case-level vs fallback is used"),
        ("04_delta_distribution_by_endpoint.png", "Applied delta distribution", "Range and center of delta by endpoint"),
        ("05_distribution_panels_3x3.png", "Distribution panels", "Value/error/difference before vs after"),
        ("06_bland_altman_grid.png", "Bland-Altman grid", "Agreement before vs after for SBP/MBP/DBP"),
        ("07_residual_vs_mean_grid.png", "Residual-mean trend", "Whether proportional bias is reduced"),
    ]
    df = pd.DataFrame(rows, columns=["file", "figure_title", "ppt_message"])
    df.to_csv(out_dir / "figure_index_for_ppt.csv", index=False)


def main() -> None:
    args = parse_args()
    bp_dir = Path(args.bp_suitability_dir)
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    eval_df = pd.read_csv(bp_dir / "bp_adjustment_eval_long.csv")
    suit_df = pd.read_csv(bp_dir / "bp_suitability_summary.csv")
    src_df = pd.read_csv(bp_dir / "bp_source_level_distribution.csv")
    case_long = pd.read_csv(bp_dir / "bp_case_delta_table_long.csv", usecols=["endpoint", "delta_used"])

    draw_workflow(out_dir / "01_workflow_fixed_delta.png", args.min_pairs_case)
    plot_metric_comparison(suit_df, out_dir / "02_metric_comparison_before_after.png")
    plot_source_distribution(src_df, out_dir / "03_source_level_distribution.png")
    plot_delta_distribution(case_long, out_dir / "04_delta_distribution_by_endpoint.png")
    combine_existing_distribution_panels(bp_dir / "figures", out_dir / "05_distribution_panels_3x3.png")

    samples = collect_sample_for_scatter(Path(args.calibrated_table), args.max_points_per_endpoint, args.seed)
    plot_bland_altman_grid(samples, eval_df, out_dir / "06_bland_altman_grid.png")
    plot_residual_mean_grid(samples, eval_df, out_dir / "07_residual_vs_mean_grid.png")

    write_index(out_dir)
    print(f"[INFO] wrote ppt figure pack: {out_dir}")


if __name__ == "__main__":
    main()
