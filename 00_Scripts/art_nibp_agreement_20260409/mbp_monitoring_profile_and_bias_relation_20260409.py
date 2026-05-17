#!/usr/bin/env python3
"""
MBP-focused profile:
1) NIBP-only (no ART MBP across case) vs any-ART groups: department/type/duration
2) Case-level mean error (ART-NIBP) relationship with case-level mean ART/NIBP MBP
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, Tuple

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="MBP monitoring profile and bias relation.")
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
        "--time-meta",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/version_split/"
            "cleaned_no_imputation/time_related_data_latest.csv"
        ),
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default=(
            "/N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ/"
            "art_nibp_agreement_20260409/output_mbp_profile"
        ),
    )
    parser.add_argument("--min-pairs-for-case-bias", type=int, default=5)
    parser.add_argument("--plot-max-points", type=int, default=30000)
    parser.add_argument("--seed", type=int, default=20260409)
    return parser.parse_args()


def iqr_text(x: pd.Series) -> str:
    x = pd.to_numeric(x, errors="coerce").dropna()
    if x.empty:
        return "NA"
    return f"{x.median():.1f} [{x.quantile(0.25):.1f}, {x.quantile(0.75):.1f}]"


def numeric_summary(x: pd.Series, prefix: str) -> Dict[str, float]:
    x = pd.to_numeric(x, errors="coerce").dropna()
    return {
        f"{prefix}_n_non_missing": int(len(x)),
        f"{prefix}_mean": float(x.mean()) if len(x) else np.nan,
        f"{prefix}_sd": float(x.std(ddof=1)) if len(x) > 1 else np.nan,
        f"{prefix}_median": float(x.median()) if len(x) else np.nan,
        f"{prefix}_p25": float(x.quantile(0.25)) if len(x) else np.nan,
        f"{prefix}_p75": float(x.quantile(0.75)) if len(x) else np.nan,
    }


def build_case_level(vitals_path: str, op_meta_path: str, time_meta_path: str) -> Tuple[pd.DataFrame, pd.DataFrame]:
    usecols = ["op_id", "subject_id", "min_from_entry", "art_mbp", "nibp_mbp"]
    print("[INFO] loading vitals subset...")
    df = pd.read_csv(vitals_path, usecols=usecols)
    df["op_id"] = pd.to_numeric(df["op_id"], errors="coerce")
    df["subject_id"] = pd.to_numeric(df["subject_id"], errors="coerce")
    df["min_from_entry"] = pd.to_numeric(df["min_from_entry"], errors="coerce")
    df["art_mbp"] = pd.to_numeric(df["art_mbp"], errors="coerce")
    df["nibp_mbp"] = pd.to_numeric(df["nibp_mbp"], errors="coerce")
    df = df.dropna(subset=["op_id", "min_from_entry"])
    print(f"[INFO] rows after basic cleaning: {len(df):,}")

    case = (
        df.groupby("op_id", sort=False)
        .agg(
            subject_id=("subject_id", "first"),
            row_n=("op_id", "size"),
            case_start_min=("min_from_entry", "min"),
            case_end_min=("min_from_entry", "max"),
        )
        .reset_index()
    )
    case["case_span_from_vitals_min"] = case["case_end_min"] - case["case_start_min"]

    art_present = df["art_mbp"].notna()
    nibp_present = df["nibp_mbp"].notna()

    art_counts = df.loc[art_present].groupby("op_id", sort=False).size().rename("n_art_mbp")
    nibp_counts = df.loc[nibp_present].groupby("op_id", sort=False).size().rename("n_nibp_mbp")
    case = case.merge(art_counts, on="op_id", how="left").merge(nibp_counts, on="op_id", how="left")
    case["n_art_mbp"] = case["n_art_mbp"].fillna(0).astype(int)
    case["n_nibp_mbp"] = case["n_nibp_mbp"].fillna(0).astype(int)

    art_span = (
        df.loc[art_present]
        .groupby("op_id", sort=False)["min_from_entry"]
        .agg(art_start_min="min", art_end_min="max")
        .reset_index()
    )
    art_span["art_span_min"] = art_span["art_end_min"] - art_span["art_start_min"]

    nibp_span = (
        df.loc[nibp_present]
        .groupby("op_id", sort=False)["min_from_entry"]
        .agg(nibp_start_min="min", nibp_end_min="max")
        .reset_index()
    )
    nibp_span["nibp_span_min"] = nibp_span["nibp_end_min"] - nibp_span["nibp_start_min"]

    case = case.merge(art_span, on="op_id", how="left").merge(nibp_span, on="op_id", how="left")

    case["has_art_mbp"] = case["n_art_mbp"] > 0
    case["has_nibp_mbp"] = case["n_nibp_mbp"] > 0
    case["nibp_only_full_case"] = case["has_nibp_mbp"] & (~case["has_art_mbp"])
    case["any_art_case"] = case["has_art_mbp"]
    case["group_mbp"] = np.where(case["nibp_only_full_case"], "nibp_only_full_case", np.where(case["any_art_case"], "any_art_case", "no_mbp"))

    op_meta = pd.read_csv(op_meta_path, usecols=["op_id", "department", "antype", "icd10_pcs"])
    op_meta["op_id"] = pd.to_numeric(op_meta["op_id"], errors="coerce")
    op_meta["department"] = op_meta["department"].astype(str).replace("nan", "Unknown")
    op_meta["antype"] = op_meta["antype"].astype(str).replace("nan", "Unknown")
    op_meta["icd10_pcs"] = op_meta["icd10_pcs"].astype(str).replace("nan", "Unknown")

    time_meta = pd.read_csv(
        time_meta_path,
        usecols=["op_id", "op_duration_min", "anesthesia_duration_min", "or_room_time_min"],
    )
    time_meta["op_id"] = pd.to_numeric(time_meta["op_id"], errors="coerce")

    case = case.merge(op_meta, on="op_id", how="left").merge(time_meta, on="op_id", how="left")
    case["department"] = case["department"].fillna("Unknown")
    case["antype"] = case["antype"].fillna("Unknown")
    case["icd10_pcs"] = case["icd10_pcs"].fillna("Unknown")

    pair = df.loc[art_present & nibp_present, ["op_id", "art_mbp", "nibp_mbp"]].copy()
    pair["error"] = pair["art_mbp"] - pair["nibp_mbp"]
    pair_case = (
        pair.groupby("op_id", sort=False)
        .agg(
            pair_n=("error", "size"),
            mean_error=("error", "mean"),
            sd_error=("error", "std"),
            median_error=("error", "median"),
            mean_art_mbp=("art_mbp", "mean"),
            mean_nibp_mbp=("nibp_mbp", "mean"),
        )
        .reset_index()
    )
    case = case.merge(pair_case, on="op_id", how="left")
    return case, pair


def group_overview(case: pd.DataFrame) -> pd.DataFrame:
    groups = []
    for gname, g in case.groupby("group_mbp", dropna=False):
        row: Dict[str, float] = {
            "group_mbp": gname,
            "n_cases": int(len(g)),
            "n_subjects": int(g["subject_id"].nunique()),
        }
        row.update(numeric_summary(g["op_duration_min"], "op_duration_min"))
        row.update(numeric_summary(g["anesthesia_duration_min"], "anesthesia_duration_min"))
        row.update(numeric_summary(g["case_span_from_vitals_min"], "case_span_from_vitals_min"))
        row.update(numeric_summary(g["nibp_span_min"], "nibp_span_min"))
        row.update(numeric_summary(g["art_span_min"], "art_span_min"))
        row.update(numeric_summary(g["n_nibp_mbp"], "n_nibp_mbp"))
        row.update(numeric_summary(g["n_art_mbp"], "n_art_mbp"))
        groups.append(row)
    return pd.DataFrame(groups).sort_values("group_mbp").reset_index(drop=True)


def top_distribution(case: pd.DataFrame, by: str, group_col: str, top_n: int = 15) -> pd.DataFrame:
    rows = []
    for gname, g in case.groupby(group_col, dropna=False):
        s = g[by].fillna("Unknown").astype(str)
        vc = s.value_counts(dropna=False).head(top_n)
        total = len(g)
        for k, n in vc.items():
            rows.append(
                {
                    "group_mbp": gname,
                    by: k,
                    "n_cases": int(n),
                    "pct_in_group": float(n / total),
                }
            )
    return pd.DataFrame(rows)


def case_error_relation(case: pd.DataFrame, min_pairs: int) -> Tuple[pd.DataFrame, pd.DataFrame]:
    rel = case.loc[case["pair_n"].fillna(0) >= min_pairs].copy()
    if rel.empty:
        return rel, pd.DataFrame()

    x_art = rel["mean_art_mbp"].to_numpy(dtype=float)
    x_nibp = rel["mean_nibp_mbp"].to_numpy(dtype=float)
    y = rel["mean_error"].to_numpy(dtype=float)
    art_slope, art_intercept = np.polyfit(x_art, y, 1)
    nibp_slope, nibp_intercept = np.polyfit(x_nibp, y, 1)
    art_corr = np.corrcoef(x_art, y)[0, 1]
    nibp_corr = np.corrcoef(x_nibp, y)[0, 1]

    stats = pd.DataFrame(
        [
            {
                "population": "case_mean_bias_relation",
                "n_cases_min_pairs": int(len(rel)),
                "min_pairs_required": int(min_pairs),
                "mean_error_median_iqr": iqr_text(rel["mean_error"]),
                "mean_art_mbp_median_iqr": iqr_text(rel["mean_art_mbp"]),
                "mean_nibp_mbp_median_iqr": iqr_text(rel["mean_nibp_mbp"]),
                "corr_mean_error_vs_mean_art_mbp": float(art_corr),
                "slope_mean_error_vs_mean_art_mbp": float(art_slope),
                "intercept_mean_error_vs_mean_art_mbp": float(art_intercept),
                "corr_mean_error_vs_mean_nibp_mbp": float(nibp_corr),
                "slope_mean_error_vs_mean_nibp_mbp": float(nibp_slope),
                "intercept_mean_error_vs_mean_nibp_mbp": float(nibp_intercept),
            }
        ]
    )
    return rel, stats


def subject_error_relation(rel_case: pd.DataFrame, min_total_pairs: int) -> Tuple[pd.DataFrame, pd.DataFrame]:
    req = ["subject_id", "pair_n", "mean_error", "mean_art_mbp", "mean_nibp_mbp"]
    work = rel_case[req].dropna(subset=req).copy()
    work["subject_id"] = pd.to_numeric(work["subject_id"], errors="coerce")
    work = work.dropna(subset=["subject_id"])
    work["subject_id"] = work["subject_id"].astype("int64")
    work = work.loc[work["pair_n"] > 0].copy()
    if work.empty:
        return work, pd.DataFrame()

    rows = []
    for sid, g in work.groupby("subject_id", sort=False):
        w = g["pair_n"].to_numpy(dtype=float)
        rows.append(
            {
                "subject_id": sid,
                "n_cases": int(len(g)),
                "total_pairs": int(g["pair_n"].sum()),
                "mean_error": float(np.average(g["mean_error"], weights=w)),
                "mean_art_mbp": float(np.average(g["mean_art_mbp"], weights=w)),
                "mean_nibp_mbp": float(np.average(g["mean_nibp_mbp"], weights=w)),
            }
        )
    sub = pd.DataFrame(rows)
    sub = sub.loc[sub["total_pairs"] >= min_total_pairs].copy()
    if sub.empty:
        return sub, pd.DataFrame()

    x1 = sub["mean_art_mbp"].to_numpy(dtype=float)
    x2 = sub["mean_nibp_mbp"].to_numpy(dtype=float)
    y = sub["mean_error"].to_numpy(dtype=float)
    sl1, it1 = np.polyfit(x1, y, 1)
    sl2, it2 = np.polyfit(x2, y, 1)
    co1 = float(np.corrcoef(x1, y)[0, 1])
    co2 = float(np.corrcoef(x2, y)[0, 1])

    stats = pd.DataFrame(
        [
            {
                "population": "subject_weighted_mean_bias_relation",
                "n_subjects_min_total_pairs": int(len(sub)),
                "min_total_pairs_required": int(min_total_pairs),
                "mean_error_median_iqr": iqr_text(sub["mean_error"]),
                "mean_art_mbp_median_iqr": iqr_text(sub["mean_art_mbp"]),
                "mean_nibp_mbp_median_iqr": iqr_text(sub["mean_nibp_mbp"]),
                "corr_mean_error_vs_mean_art_mbp": co1,
                "slope_mean_error_vs_mean_art_mbp": float(sl1),
                "intercept_mean_error_vs_mean_art_mbp": float(it1),
                "corr_mean_error_vs_mean_nibp_mbp": co2,
                "slope_mean_error_vs_mean_nibp_mbp": float(sl2),
                "intercept_mean_error_vs_mean_nibp_mbp": float(it2),
            }
        ]
    )
    return sub, stats


def plot_relation(rel: pd.DataFrame, out_dir: Path, max_points: int, seed: int) -> None:
    if rel.empty:
        return

    rng = np.random.default_rng(seed)
    draw = rel
    if len(rel) > max_points:
        idx = rng.choice(len(rel), size=max_points, replace=False)
        draw = rel.iloc[idx]

    plt.figure(figsize=(8, 6))
    plt.hist(rel["mean_error"].dropna(), bins=60, color="tab:gray", alpha=0.85)
    plt.axvline(rel["mean_error"].mean(), color="tab:blue", linewidth=1.4)
    plt.xlabel("Case mean error (ART-NIBP), mmHg")
    plt.ylabel("Cases")
    plt.title("Distribution of Case Mean MBP Error")
    plt.tight_layout()
    plt.savefig(out_dir / "mbp_case_mean_error_hist.png", dpi=150)
    plt.close()

    def scatter_fit(x: pd.Series, y: pd.Series, xlabel: str, title: str, out_name: str) -> None:
        xv = x.to_numpy(dtype=float)
        yv = y.to_numpy(dtype=float)
        slope, intercept = np.polyfit(xv, yv, 1)
        xline = np.linspace(np.nanmin(xv), np.nanmax(xv), 100)
        yline = slope * xline + intercept

        plt.figure(figsize=(8, 6))
        plt.scatter(
            draw[x.name].to_numpy(dtype=float),
            draw[y.name].to_numpy(dtype=float),
            s=10,
            alpha=0.25,
            linewidths=0,
        )
        plt.plot(xline, yline, color="tab:red", linewidth=1.6)
        plt.xlabel(xlabel)
        plt.ylabel("Case mean error (ART-NIBP), mmHg")
        plt.title(title)
        plt.tight_layout()
        plt.savefig(out_dir / out_name, dpi=150)
        plt.close()

    scatter_fit(
        rel["mean_art_mbp"],
        rel["mean_error"],
        xlabel="Case mean ART MBP, mmHg",
        title="Case Mean Error vs Case Mean ART MBP",
        out_name="mbp_case_mean_error_vs_mean_art.png",
    )
    scatter_fit(
        rel["mean_nibp_mbp"],
        rel["mean_error"],
        xlabel="Case mean NIBP MBP, mmHg",
        title="Case Mean Error vs Case Mean NIBP MBP",
        out_name="mbp_case_mean_error_vs_mean_nibp.png",
    )

    plt.figure(figsize=(8, 6))
    plt.hist(rel["mean_art_mbp"].dropna(), bins=50, alpha=0.5, label="mean ART MBP", color="tab:red")
    plt.hist(rel["mean_nibp_mbp"].dropna(), bins=50, alpha=0.5, label="mean NIBP MBP", color="tab:blue")
    plt.xlabel("Case mean MBP, mmHg")
    plt.ylabel("Cases")
    plt.title("Distribution: Case Mean ART vs NIBP MBP")
    plt.legend()
    plt.tight_layout()
    plt.savefig(out_dir / "mbp_case_mean_art_vs_nibp_distribution.png", dpi=150)
    plt.close()


def plot_subject_relation(sub: pd.DataFrame, out_dir: Path) -> None:
    if sub.empty:
        return

    y = sub["mean_error"].to_numpy(dtype=float)
    x_art = sub["mean_art_mbp"].to_numpy(dtype=float)
    x_nibp = sub["mean_nibp_mbp"].to_numpy(dtype=float)

    plt.figure(figsize=(8, 6))
    plt.hist(sub["mean_error"], bins=50, color="tab:gray", alpha=0.85)
    plt.axvline(sub["mean_error"].mean(), color="tab:blue", lw=1.4)
    plt.xlabel("Subject weighted mean error (ART-NIBP), mmHg")
    plt.ylabel("Subjects")
    plt.title("Distribution of Subject Mean MBP Error")
    plt.tight_layout()
    plt.savefig(out_dir / "mbp_subject_mean_error_hist.png", dpi=150)
    plt.close()

    for x, col, xlabel, title, out_name in [
        (
            x_art,
            "mean_art_mbp",
            "Subject weighted mean ART MBP, mmHg",
            "Subject Mean Error vs Subject Mean ART MBP",
            "mbp_subject_mean_error_vs_mean_art.png",
        ),
        (
            x_nibp,
            "mean_nibp_mbp",
            "Subject weighted mean NIBP MBP, mmHg",
            "Subject Mean Error vs Subject Mean NIBP MBP",
            "mbp_subject_mean_error_vs_mean_nibp.png",
        ),
    ]:
        slope, intercept = np.polyfit(x, y, 1)
        xline = np.linspace(np.nanmin(x), np.nanmax(x), 100)
        yline = slope * xline + intercept

        plt.figure(figsize=(8, 6))
        plt.scatter(sub[col], sub["mean_error"], s=10, alpha=0.25, linewidths=0)
        plt.plot(xline, yline, color="tab:red", linewidth=1.6)
        plt.xlabel(xlabel)
        plt.ylabel("Subject weighted mean error (ART-NIBP), mmHg")
        plt.title(title)
        plt.tight_layout()
        plt.savefig(out_dir / out_name, dpi=150)
        plt.close()


def main() -> None:
    args = parse_args()
    out_dir = Path(args.output_dir)
    fig_dir = out_dir / "figures"
    out_dir.mkdir(parents=True, exist_ok=True)
    fig_dir.mkdir(parents=True, exist_ok=True)

    case, pair = build_case_level(args.vitals, args.op_meta, args.time_meta)
    print(f"[INFO] case rows: {len(case):,}, pair rows: {len(pair):,}")

    overview = group_overview(case)
    dept_top = top_distribution(case, by="department", group_col="group_mbp", top_n=20)
    antype_top = top_distribution(case, by="antype", group_col="group_mbp", top_n=20)
    icd_top = top_distribution(case, by="icd10_pcs", group_col="group_mbp", top_n=20)
    rel_case, rel_stats = case_error_relation(case, min_pairs=args.min_pairs_for_case_bias)
    rel_subject, rel_subject_stats = subject_error_relation(rel_case, min_total_pairs=args.min_pairs_for_case_bias)

    plot_relation(rel_case, fig_dir, args.plot_max_points, args.seed)
    plot_subject_relation(rel_subject, fig_dir)

    case.to_csv(out_dir / "mbp_case_level_profile.csv", index=False)
    overview.to_csv(out_dir / "mbp_group_overview.csv", index=False)
    dept_top.to_csv(out_dir / "mbp_department_top20_by_group.csv", index=False)
    antype_top.to_csv(out_dir / "mbp_antype_top20_by_group.csv", index=False)
    icd_top.to_csv(out_dir / "mbp_icd10_pcs_top20_by_group.csv", index=False)
    rel_case.to_csv(out_dir / "mbp_case_mean_bias_relation_cases.csv", index=False)
    rel_stats.to_csv(out_dir / "mbp_case_mean_bias_relation_stats.csv", index=False)
    rel_subject.to_csv(out_dir / "mbp_subject_mean_bias_relation_subjects.csv", index=False)
    rel_subject_stats.to_csv(out_dir / "mbp_subject_mean_bias_relation_stats.csv", index=False)

    print(f"[INFO] wrote: {out_dir / 'mbp_case_level_profile.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_group_overview.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_department_top20_by_group.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_antype_top20_by_group.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_icd10_pcs_top20_by_group.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_case_mean_bias_relation_cases.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_case_mean_bias_relation_stats.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_subject_mean_bias_relation_subjects.csv'}")
    print(f"[INFO] wrote: {out_dir / 'mbp_subject_mean_bias_relation_stats.csv'}")
    print(f"[INFO] figures: {fig_dir}")


if __name__ == "__main__":
    main()
