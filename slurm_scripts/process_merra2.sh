#!/bin/bash
#SBATCH --job-name=process_merra2
#SBATCH --output=slurm_messages/container_output_%j.txt
#SBATCH --error=slurm_messages/container_error_%j.txt
#SBATCH --time=75:00:00
#SBATCH --partition=highmem
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=100G
#SBATCH --mail-user=kate.pogue@nih.gov
#SBATCH --mail-type=BEGIN,END,FAIL

# Paths
CONTAINER_PATH="/ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif"
SCRIPT_PATH="/ddn/gs1/group/set/chords/combining_datasets/original_scripts/process_merra.R"

# Run the R script inside the container with an isolated shell
apptainer exec "$CONTAINER_PATH" bash -c "Rscript $SCRIPT_PATH"