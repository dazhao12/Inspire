#!/bin/bash
#SBATCH --job-name=vitaldb_convert
#SBATCH --output=convert.log
#SBATCH --error=convert.log
#SBATCH --partition=general          # 不需要 GPU，用 general 节点即可
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8            # N_WORKERS=4，留双倍余量
#SBATCH --mem=32G
#SBATCH --time=12:00:00              # 6388个文件预计需要数小时
#SBATCH -A r00209

# 激活环境
module load python/gpu
source /N/project/waveform_mortality/ZhaoZhang/timesfm311/bin/activate

cd /N/project/analgesia_perioperation/projects/Inspire_data_process_ZZ

python -u vitalDB_3_26_2026.py
