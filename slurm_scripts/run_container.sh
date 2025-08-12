#!/bin/bash
#SBATCH --job-name=container_combining_datasets
#SBATCH --output=container_output_%j.txt
#SBATCH --error=container_error_%j.txt
#SBATCH --time=01:00:00
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mail-user=kate.pogue@nih.gov
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --cpus-per-task=4
#SBATCH --mem=8G

# Load Apptainer module 
module load apptainer

apptainer exec \
  --bind /ddn/gs1/group/set/chords/combining_datasets/raw_data:/raw_data \
  /ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif \
  ls /raw_data 


