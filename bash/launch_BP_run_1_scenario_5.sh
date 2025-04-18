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
# Started:          08/01/2025
# Most Recent Edit: 08/01/2025
# ****************************************
#
#SBATCH --partition=compute
#SBATCH --job-name=conf_sim_study_run_1_scenario_5
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --time=2-00:00:00
#SBATCH --mem-per-cpu=10G
#SBATCH --account=MATH033344
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aa22294@bristol.ac.uk

# Runs
RUNS="1"

# Scenarios
SCENARIOS="5"

# Change into working directory
cd ${SLURM_SUBMIT_DIR}
cd ..
cd R

# Record info
echo ""
echo "***** START *****"
echo "***** Confounder Handling Simulation Study - Simulation *****"
echo Simulation Runs:   $RUNS
echo Scenarios:         $SCENARIOS
echo Start Time:        $(date)
echo Working Directory: $(pwd)
echo JOB ID:            ${SLURM_JOBID}
echo SLURM ARRAY ID:    ${SLURM_ARRAY_TASK_ID}
echo ""

# Import R
module load languages/R/4.4.1

# Execute code
Rscript launch_isolated.R $RUNS $SCENARIOS
echo ""
echo End Time: $(date)
echo "***** END *****"
echo ""

