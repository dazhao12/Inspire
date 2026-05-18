"""
vitalDB_3_26_2026.py
批量将 .vital 文件转换为 .csv 文件
每行 = 1秒（可调整 INTERVAL）

v1.2 改动：
  - SKIP_EXISTING 通过 args 传入，避免全局变量在子进程中的依赖问题
  - 使用 None 自动获取所有轨道，兼容不同版本 vitaldb API
  - 新增可配置聚合：
      numeric 轨道支持 last / mean / median（默认 median）
      wave 轨道支持 last / mean（默认 mean）
  - 保持 CSV 输出格式（若需 Excel 请将 to_csv 改为 to_excel）

使用方式（HPC）：
    python vitalDB_3_26_2026.py
    nohup python vitalDB_3_26_2026.py > convert.log 2>&1 &   # 后台运行
"""

import os
import sys
import time
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor, as_completed

import vitaldb
import pandas as pd
import numpy as np

# ============================================================
# ⚙️  配置区 — 根据你的实际路径修改
# ============================================================

# vital 文件所在文件夹
INPUT_DIR = "/N/project/analgesia_perioperation/data/VitalDB_1.0.0/raw/vital_waveform_files"

# 输出 CSV 的文件夹（不存在会自动创建）
OUTPUT_DIR = "/N/project/analgesia_perioperation/data/VitalDB_1.0.0/csv_output"

# 采样间隔（秒）：1 = 每行1秒，1/100 = 100Hz高频
INTERVAL = 1

# 聚合方式：
# - NUMERIC_AGG: "last" / "mean" / "median"
# - WAVE_AGG: "last" / "mean"
# 推荐：
# - 秒级（INTERVAL=1）可用 mean 或 median（numeric）
# - 分钟级（INTERVAL=60）建议 numeric=median, wave=mean
NUMERIC_AGG = "median"
WAVE_AGG = "mean"

# 是否跳过已存在的 CSV（True=断点续传，False=全部重新转）
SKIP_EXISTING = True

# 并行进程数（HPC 可调大，建议 4~16；本地设为 1）
N_WORKERS = 4

# ============================================================
# 主程序
# ============================================================


def _nret_from_vf(vf, interval):
    if interval <= 0:
        raise ValueError("interval must be > 0")
    if vf.dtend <= vf.dtstart:
        return 0
    return int(np.ceil((vf.dtend - vf.dtstart) / interval))


def _aggregate_numeric_track(trk, dtstart, nret, interval, method):
    out = np.full(nret, np.nan, dtype=np.float32)
    idx_list = []
    val_list = []
    for rec in trk.recs:
        if "dt" not in rec or "val" not in rec:
            continue
        idx = int((rec["dt"] - dtstart) / interval)
        if idx < 0:
            idx = 0
        elif idx >= nret:
            idx = nret - 1
        idx_list.append(idx)
        val_list.append(float(rec["val"]))

    if not idx_list:
        return out

    idx_arr = np.asarray(idx_list, dtype=np.int64)
    val_arr = np.asarray(val_list, dtype=np.float64)
    valid = np.isfinite(val_arr)
    idx_arr = idx_arr[valid]
    val_arr = val_arr[valid]
    if len(idx_arr) == 0:
        return out

    if method == "last":
        out[idx_arr] = val_arr.astype(np.float32)
        return out
    if method == "mean":
        sums = np.bincount(idx_arr, weights=val_arr, minlength=nret)
        cnts = np.bincount(idx_arr, minlength=nret)
        nz = cnts > 0
        out[nz] = (sums[nz] / cnts[nz]).astype(np.float32)
        return out
    if method == "median":
        df = pd.DataFrame({"idx": idx_arr, "val": val_arr})
        med = df.groupby("idx", sort=False)["val"].median()
        out[med.index.to_numpy(dtype=np.int64)] = med.to_numpy(dtype=np.float32)
        return out
    raise ValueError(f"Unsupported NUMERIC_AGG: {method}")


def _aggregate_string_track(trk, dtstart, nret, interval):
    out = np.full(nret, np.nan, dtype=object)
    for rec in trk.recs:
        if "dt" not in rec or "val" not in rec:
            continue
        idx = int((rec["dt"] - dtstart) / interval)
        if idx < 0:
            idx = 0
        elif idx >= nret:
            idx = nret - 1
        out[idx] = rec["val"]
    return out


def _aggregate_wave_track(trk, dtstart, nret, interval, method):
    out = np.full(nret, np.nan, dtype=np.float32)
    if method == "median":
        # wave 数据量通常很大，median 聚合开销高，这里回退到 mean
        method = "mean"

    if method == "mean":
        sums = np.zeros(nret, dtype=np.float64)
        cnts = np.zeros(nret, dtype=np.int64)
    elif method != "last":
        raise ValueError(f"Unsupported WAVE_AGG: {method}")

    srate = float(getattr(trk, "srate", 0.0) or 0.0)
    if srate <= 0:
        return out

    for rec in trk.recs:
        if "dt" not in rec or "val" not in rec:
            continue
        vals = np.asarray(rec["val"], dtype=np.float32)
        if vals.size == 0:
            continue

        if getattr(trk, "fmt", 0) > 2:
            vals = vals * float(getattr(trk, "gain", 1.0)) + float(getattr(trk, "offset", 0.0))

        rel_t0 = rec["dt"] - dtstart
        idx = np.floor((rel_t0 + (np.arange(vals.size, dtype=np.float64) / srate)) / interval).astype(np.int64)
        valid = (idx >= 0) & (idx < nret) & np.isfinite(vals)
        if not np.any(valid):
            continue
        idx = idx[valid]
        vals = vals[valid]

        if method == "last":
            out[idx] = vals
        else:
            sums += np.bincount(idx, weights=vals, minlength=nret)
            cnts += np.bincount(idx, minlength=nret)

    if method == "mean":
        nz = cnts > 0
        out[nz] = (sums[nz] / cnts[nz]).astype(np.float32)

    out[np.isinf(out) | (out > 4e9)] = np.nan
    return out


def _to_pandas_binned(vf, track_names, interval, numeric_agg, wave_agg):
    nret = _nret_from_vf(vf, interval)
    if nret == 0:
        return pd.DataFrame(columns=track_names)

    data = {}
    dtstart = vf.dtstart
    for name in track_names:
        trk = vf.find_track(name) if hasattr(vf, "find_track") else vf.trks.get(name)
        if trk is None:
            data[name] = np.full(nret, np.nan, dtype=np.float32)
            continue

        if trk.type == 2:  # numeric
            data[name] = _aggregate_numeric_track(trk, dtstart, nret, interval, numeric_agg)
        elif trk.type == 5:  # str
            data[name] = _aggregate_string_track(trk, dtstart, nret, interval)
        elif trk.type == 1:  # wave
            data[name] = _aggregate_wave_track(trk, dtstart, nret, interval, wave_agg)
        else:
            data[name] = np.full(nret, np.nan, dtype=np.float32)

    return pd.DataFrame(data, columns=track_names)


def convert_one(args):
    """将单个 vital 文件转换为 csv（供并行调用）"""
    vital_path, csv_path, interval, skip_existing, numeric_agg, wave_agg = args
    vital_path = Path(vital_path)
    csv_path = Path(csv_path)

    if skip_existing and csv_path.exists():
        return 'skip', vital_path.name, 0, 0, 0

    try:
        vf = vitaldb.VitalFile(str(vital_path))

        # 获取所有轨道名（兼容 vitaldb 不同版本）
        if hasattr(vf, 'get_track_names'):
            tnames = vf.get_track_names()
        else:
            tnames = [t for t in vf.trks.keys() if t != 'EVENT']

        # 使用可控聚合，避免 numeric 轨道“同 bin 默认取最后值”造成偏差
        df = _to_pandas_binned(vf, tnames, interval=interval, numeric_agg=numeric_agg, wave_agg=wave_agg)

        df.index.name = 'time_sec'
        csv_path.parent.mkdir(parents=True, exist_ok=True)
        df.to_csv(str(csv_path))   # HPC 无需 utf-8-sig
        size_kb = csv_path.stat().st_size / 1024
        return 'ok', vital_path.name, len(df), len(df.columns), size_kb
    except Exception as e:
        return 'fail', vital_path.name, 0, 0, str(e)


def main():
    input_dir = Path(INPUT_DIR)
    output_dir = Path(OUTPUT_DIR)
    output_dir.mkdir(parents=True, exist_ok=True)

    # 过滤掉 macOS 隐藏元数据文件（以 ._ 开头）
    vital_files = sorted(f for f in input_dir.glob("**/*.vital") if not f.name.startswith("._"))
    total = len(vital_files)

    if total == 0:
        print(f"[ERROR] 未找到 .vital 文件: {INPUT_DIR}")
        sys.exit(1)

    print(f"[INFO] 输入目录 : {INPUT_DIR}")
    print(f"[INFO] 输出目录 : {OUTPUT_DIR}")
    print(f"[INFO] 采样间隔 : {INTERVAL} 秒")
    print(f"[INFO] NUMERIC_AGG : {NUMERIC_AGG}")
    print(f"[INFO] WAVE_AGG    : {WAVE_AGG}")
    print(f"[INFO] 并行进程 : {N_WORKERS}")
    print(f"[INFO] 共 {total} 个 .vital 文件")
    print("-" * 70)

    # 构造任务列表（✅ 将 SKIP_EXISTING 作为参数传入，避免子进程全局变量依赖）
    tasks = []
    for vital_path in vital_files:
        rel = vital_path.relative_to(input_dir)
        csv_path = output_dir / rel.with_suffix('.csv')
        tasks.append((str(vital_path), str(csv_path), INTERVAL, SKIP_EXISTING, NUMERIC_AGG, WAVE_AGG))

    success, skipped, failed = 0, 0, 0
    t0 = time.time()

    with ProcessPoolExecutor(max_workers=N_WORKERS) as executor:
        futures = {executor.submit(convert_one, t): t for t in tasks}
        for i, future in enumerate(as_completed(futures), 1):
            status, name, rows, cols, extra = future.result()
            prefix = f"[{i:5d}/{total}]"
            if status == 'ok':
                print(f"{prefix} OK    {name}  {rows}行x{cols}列  {extra:.1f}KB")
                success += 1
            elif status == 'skip':
                print(f"{prefix} SKIP  {name}")
                skipped += 1
            else:
                print(f"{prefix} FAIL  {name}  原因: {extra}")
                failed += 1

    elapsed = time.time() - t0
    print("-" * 70)
    print(f"[DONE] 成功:{success}  跳过:{skipped}  失败:{failed}  耗时:{elapsed:.1f}s")
    print(f"[DONE] 输出目录: {output_dir}")


if __name__ == "__main__":
    main()
