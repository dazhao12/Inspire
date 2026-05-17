#!/usr/bin/env python3
"""
Rebuild INSPIRE_1.3/03_cleaned_outlier snapshot files.

Why this exists:
- Some legacy generation modules are no longer present in current repo layout.
- Most 03 files can be sourced from archived cleaned_no_imputation outputs.
- A few files (timeline/comorbidity variants) currently only exist in 03 itself.

This script provides a reproducible, explicit sync plan.
"""

import argparse
import hashlib
import shutil
from pathlib import Path
from typing import NamedTuple


DATA_ROOT = Path("/N/project/analgesia_perioperation/data/INSPIRE_1.3")
DEFAULT_OUTPUT_03 = DATA_ROOT / "03_cleaned_outlier"
DEFAULT_ARCHIVE_NO_IMPUTE = (
    DATA_ROOT
    / "processed/archive/archived_from_processed_20260417_204755/version_split/cleaned_no_imputation"
)
DEFAULT_LEGACY_03 = DATA_ROOT / "03_cleaned_outlier"


class FilePlan(NamedTuple):
    name: str
    source_group: str


FILE_PLANS = [
    FilePlan("anesthesia_timeline_first_nonMAC_latest.csv", "legacy_03_only"),
    FilePlan("comorbidity_defined_latest.csv", "legacy_03_only"),
    FilePlan(
        "comorbidity_defined_latest_bak_before_egfr_all_people_stage_2026-04-18.csv",
        "legacy_03_only",
    ),
    FilePlan("demographic_operation_latest.csv", "archive_no_impute"),
    FilePlan("demographic_subject_latest.csv", "archive_no_impute"),
    FilePlan(
        "intraop_vitals_clean_before_impute_with_calibrated_nibp_artfirst_min2_latest.csv",
        "archive_no_impute",
    ),
    FilePlan("postop_complications_defined_latest.csv", "archive_no_impute"),
    FilePlan("postop_complications_summary_latest.csv", "archive_no_impute"),
    FilePlan("postop_labs_attributable_7d_latest.csv", "archive_no_impute"),
    FilePlan(
        "preop_baseline_final_median_outlier_removed_no_imputation_latest.csv",
        "archive_no_impute",
    ),
    FilePlan("preop_labs_attributable_90d_latest.csv", "archive_no_impute"),
    FilePlan("preop_meds_defined_preop_unrestricted_latest.csv", "archive_no_impute"),
]


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def choose_source(plan: FilePlan, archive_no_impute: Path, legacy_03: Path) -> Path:
    fname = plan.name
    if plan.source_group == "archive_no_impute":
        primary = archive_no_impute / fname
        fallback = legacy_03 / fname
        if primary.exists():
            return primary
        if fallback.exists():
            return fallback
        raise FileNotFoundError(f"Missing source for {fname}: tried {primary} and {fallback}")

    if plan.source_group == "legacy_03_only":
        src = legacy_03 / fname
        if src.exists():
            return src
        raise FileNotFoundError(f"Missing legacy-only source for {fname}: {src}")

    raise ValueError(f"Unknown source_group: {plan.source_group}")


def format_size(n: int) -> str:
    units = ["B", "KB", "MB", "GB", "TB"]
    x = float(n)
    for u in units:
        if x < 1024 or u == units[-1]:
            return f"{x:.2f}{u}"
        x /= 1024
    return f"{n}B"


def iter_rows(output_03, archive_no_impute, legacy_03, dry_run):
    output_03.mkdir(parents=True, exist_ok=True)
    for plan in FILE_PLANS:
        src = choose_source(plan, archive_no_impute=archive_no_impute, legacy_03=legacy_03)
        dst = output_03 / plan.name
        action = "SKIP"
        if src.resolve() != dst.resolve():
            if dry_run:
                action = "COPY"
            else:
                shutil.copy2(src, dst)
                action = "COPY"
        src_hash = sha256(src)
        dst_hash = sha256(dst) if dst.exists() else "-"
        status = "OK" if dst.exists() and src_hash == dst_hash else "HASH_MISMATCH"
        yield (plan.name, action, status, format_size(src.stat().st_size))


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Rebuild INSPIRE_1.3/03_cleaned_outlier snapshot files from archived sources."
    )
    parser.add_argument("--output-03", type=Path, default=DEFAULT_OUTPUT_03)
    parser.add_argument("--archive-no-impute", type=Path, default=DEFAULT_ARCHIVE_NO_IMPUTE)
    parser.add_argument("--legacy-03", type=Path, default=DEFAULT_LEGACY_03)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    print("Rebuild plan")
    print(f"  output_03        : {args.output_03}")
    print(f"  archive_no_impute: {args.archive_no_impute}")
    print(f"  legacy_03        : {args.legacy_03}")
    print(f"  dry_run          : {args.dry_run}")
    print("")

    rows = list(
        iter_rows(
            output_03=args.output_03,
            archive_no_impute=args.archive_no_impute,
            legacy_03=args.legacy_03,
            dry_run=args.dry_run,
        )
    )

    ok = 0
    for name, action, status, size in rows:
        print(f"{action:4s}  {status:12s}  {size:>10s}  {name}")
        ok += int(status == "OK")

    print("")
    print(f"Completed: {ok}/{len(rows)} files OK.")
    if ok != len(rows):
        raise SystemExit(2)


if __name__ == "__main__":
    main()
