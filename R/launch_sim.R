# ****************************************
# Confounder Handling Simulation Study
# 
# Launch the shiny app containing the simulation
# 
# Emma Tarmey
#
# Started:          31/01/2024
# Most Recent Edit: 08/02/2024
# ****************************************

# fix wd issue
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# launch app
shiny::runApp("sim_frontend")

