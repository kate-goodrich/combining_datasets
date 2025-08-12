#!/bin/bash
#SBATCH --job-name=run_targets
#SBATCH --output=slurm_messages/container_output_%j.txt
#SBATCH --error=slurm_messages/container_error_%j.txt
#SBATCH --time=75:00:00
#SBATCH --partition=highmem
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=300G
#SBATCH --mail-user=kate.pogue@nih.gov
#SBATCH --mail-type=BEGIN,END,FAIL



# Paths
CONTAINER_PATH="/ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif"

# Run pipeline inside container
apptainer exec "$CONTAINER_PATH" bash -c "Rscript -e 'targets::tar_make()'"
