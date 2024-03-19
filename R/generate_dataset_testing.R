# ****************************************
# Confounder Handling Simulation Study
# 
# Test Suite for Dataset generation algorithm
# 
# Emma Tarmey
#
# Started:          19/03/2024
# Most Recent Edit: 19/03/2024
# ****************************************


# fix wd issue
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# simulation-proper code
source("simulation.R")

# test case
coef_data <- data.frame(
  cause     = c(1, 1, 0, 0, 0),
  intercept = c(1, 1, NA, NA, NA),
  Y         = c(NA, NA, NA, NA, NA),
  X         = c(1, NA, NA, NA, NA),
  Z1        = c(1, 1, NA, NA, NA),
  Z2        = c(1, 1, NA, NA, NA),
  Z3        = c(1, 1, NA, NA, NA)
)
n_obs  <- 50
labels <- c("Y", "X", "Z1", "Z2", "Z3")
print(coef_data)

# running test
example <- generate_dataset(coef_data = coef_data, n_obs = n_obs, labels = labels)
print(head(example))


