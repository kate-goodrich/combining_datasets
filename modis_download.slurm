#!/bin/bash
#SBATCH --job-name=modis_download
#SBATCH --cpus-per-task=45
#SBATCH --ntasks=10
#SBATCH --mem=30G
#SBATCH --time=02:00:00
#SBATCH --gres=gpu:2
#SBATCH --mail-user=kate.pogue@nih.gov
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=job_%j.out
#SBATCH --error=job_%j.err



Rscript /ddn/gs1/group/set/chords/combining_datasets/modis_download.r



### View jobs

#### `squeue`
Lists jobs in the queue (running or waiting), showing job IDs, users, state, and more.

```bash
squeue             # list all jobs on the cluster
squeue -u $poguekl    # list only your jobs