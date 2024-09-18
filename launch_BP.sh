#!/bin/bash
#
# ****************************************
# Confounder Handling Simulation Study
#
# BluePebble Launch Bash Script
# Defines and runs the R simulation procedure on the BluePebble HPC
#
# Emma Tarmey
#
# Started:          16/09/2024
# Most Recent Edit: 18/09/2024
# ****************************************
#
#SBATCH --partition=default
#SBATCH --job-name=conf_sim_study
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=0-00:10:00
#SBATCH --mem-per-cpu=10G
#SBATCH --account=MATH033344


# Change into working directory
cd R

# Define executable
export EXE=/bin/hostname

# Change into working directory
cd ${SLURM_SUBMIT_DIR}

# Do some stuff
echo "\n"
echo "***** START *****"
echo "***** Confounder Handling Simulation Study - Simulation *****"
echo JOB ID: ${SLURM_JOBID}
echo SLURM ARRAY ID: ${SLURM_ARRAY_TASK_ID}
echo Working Directory: $(pwd)
echo Start Time: $(date)

# Import R
module load languages/R/4.4.1

# Execute code
Rscript launch_isolated.R
${EXE}
echo End Time: $(date)
echo "***** END *****"

