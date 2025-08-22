#!/bin/bash
#SBATCH --job-name=calc_nlcd
#SBATCH --output=slurm_messages/container_output_%j.txt
#SBATCH --error=slurm_messages/container_error_%j.txt
#SBATCH --time=40:00:00
#SBATCH --partition=highmem
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --mem=600G
#SBATCH --mail-user=kate.pogue@nih.gov
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --hint=nomultithread

# Paths
CONTAINER_PATH="/ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif"
SCRIPT_PATH="/ddn/gs1/group/set/chords/combining_datasets/orginal_scripts/calc_nlcd.R"

# Threading: let BLAS/GDAL/terra honor Slurm's CPU allocation
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export OPENBLAS_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export MKL_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export NUMEXPR_NUM_THREADS=${SLURM_CPUS_PER_TASK}

# Faster temp + cache on node-local storage (falls back to /tmp)
SCRATCH="${TMPDIR:-/tmp}/$SLURM_JOB_ID"
mkdir -p "$SCRATCH"/{tmp,gdal,tmp_r}
export TMPDIR="$SCRATCH/tmp"
export TEMP="$SCRATCH/tmp"
export TMP="$SCRATCH/tmp"
export R_TMPDIR="$SCRATCH/tmp_r"

# GDAL/terra tuning
export CPL_VSIL_CURL_ALLOWED_EXTENSIONS=".tif,.tiff,.zip"
export GDAL_CACHEMAX=4096                 # MB; increase if plenty of RAM
export VSI_CACHE=TRUE
export VSI_CACHE_SIZE=134217728           # 128 MB

# Optional: more helpful diagnostics if something goes wrong
ulimit -n 1048576  # allow many open files (lots of small TIFFs)
ulimit -Sn 1048576

# Run inside the container; inherit env; pin threads via srun
srun --cpu-bind=cores apptainer exec \
  --env OMP_NUM_THREADS=${OMP_NUM_THREADS},OPENBLAS_NUM_THREADS=${OPENBLAS_NUM_THREADS},MKL_NUM_THREADS=${MKL_NUM_THREADS},NUMEXPR_NUM_THREADS=${NUMEXPR_NUM_THREADS},TMPDIR=${TMPDIR},R_TMPDIR=${R_TMPDIR},GDAL_CACHEMAX=${GDAL_CACHEMAX},VSI_CACHE=${VSI_CACHE},VSI_CACHE_SIZE=${VSI_CACHE_SIZE} \
  "$CONTAINER_PATH" bash -c "Rscript $SCRIPT_PATH"
