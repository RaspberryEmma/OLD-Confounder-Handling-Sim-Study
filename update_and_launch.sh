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
# Most Recent Edit: 18/09/2024
# ****************************************

# delete older version
rm -f -r Confounder-Handling-Sim-Study

# clone most recent version
git clone https://github.com/RaspberryEmma/Confounder-Handling-Sim-Study

# change wd
cd Confounder-Handling-Sim-Study

# submit simulation to BP HPC
sbatch launch_BP.sh

# check job submitted correctly
sleep 5.0
echo ""
sacct -X
echo ""

