#!/bin/bash
#
# ****************************************
# Confounder Handling Simulation Study
#
# BluePebble Automation Script
# This bash scripts automates updating to the most recent
# code version from GitHub and submitting the job to BP
#
# Emma Tarmey
#
# Started:          18/09/2024
# Most Recent Edit: 11/12/2024
# ****************************************

echo ""

# delete older version
rm -f -r Confounder-Handling-Sim-Study

# clone most recent version
git clone https://github.com/RaspberryEmma/Confounder-Handling-Sim-Study

# change wd
cd Confounder-Handling-Sim-Study
cd bash

# import python
module load languages/python/3.12.3

# submit simulation to BP HPC
#sbatch launch_BP.sh
for ((i=1;i<5;i++));do
	for ((j=5;j<7;j++));do
		python generate_BP_job_file.py $i $j
		echo "Submitting job: launch_BP_run_"$i"_scenario_"$j".sh"
		sbatch "launch_BP_run_"$i"_scenario_"$j".sh"
	done
done

# check jobs submitted correctly
sleep 5.0
echo ""
sacct -X
echo ""

