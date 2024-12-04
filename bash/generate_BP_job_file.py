# ****************************************
# Confounder Handling Simulation Study
#
# BluePebble Launch Bash Script
# Defines and runs the R simulation procedure on the BluePebble HPC
#
# Emma Tarmey
#
# Started:          02/12/2024
# Most Recent Edit: 02/12/2024
# ****************************************

from datetime import datetime
import sys

# sim_runs      = list(map(int, sys.argv[1].split(',')))
# sim_scenarios = list(map(int, sys.argv[2].split(',')))

sim_runs      = sys.argv[1]
sim_scenarios = sys.argv[2]

sim_runs_no_commas      = sys.argv[1].replace(",", "_")
sim_scenarios_no_commas = sys.argv[2].replace(",", "_")

date_string     = datetime.today().strftime('%d/%m/%Y')
job_name_string = f"conf_sim_study_run_{sim_runs_no_commas}_scenario_{sim_scenarios_no_commas}"

job_file_output = f"""
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
# Started:          {date_string}
# Most Recent Edit: {date_string}
# ****************************************
#
#SBATCH --partition=compute
#SBATCH --job-name={job_name_string}
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --time=2-00:00:00
#SBATCH --mem-per-cpu=10G
#SBATCH --account=MATH033344
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aa22294@bristol.ac.uk

# Runs
RUNS="{sim_runs}"

# Scenarios
SCENARIOS="{sim_scenarios}"

# Change into working directory
cd ${{SLURM_SUBMIT_DIR}}
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
echo JOB ID:            ${{SLURM_JOBID}}
echo SLURM ARRAY ID:    ${{SLURM_ARRAY_TASK_ID}}
echo ""

# Import R
module load languages/R/4.4.1

# Execute code
Rscript launch_isolated.R $RUNS $SCENARIOS
echo ""
echo End Time: $(date)
echo "***** END *****"
echo ""

"""

# check output
# print(sim_runs)
# print(sim_scenarios)
# print(job_file_output)

# save job file
f = open(f"launch_BP_run_{ sim_runs_no_commas }_scenario_{ sim_scenarios_no_commas }.sh", "w")
f.write(job_file_output)
f.close()
