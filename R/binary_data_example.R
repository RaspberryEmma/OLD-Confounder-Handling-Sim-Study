# ****************************************
# Confounder Handling Simulation Study
# 
# Binary Data Generation example
# 
# Emma Tarmey
#
# Started:          15/01/2025
# Most Recent Edit: 15/01/2025
# ****************************************

# fix wd issue
# forces wd to be the location of this file
if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

inverse_logit <- function(real_values = NULL) {
  probabilities <- (1)/(1 + exp(-1 * real_values))
  return (probabilities)
}


# Generate all data

dataset       <- read.csv(paste("../data/sim-1-scenario-5-dataset.csv", sep = ""))
logit_prob_X  <- dataset[, 'X']
prob_X        <- inverse_logit(logit_prob_X)
binary_vals_X <- rbinom(n = 1000, size = 1, prob = prob_X)


# Starting with continuous data

message("\nLogit(Prob_X)")
print(head(logit_prob_X))
print(summary(logit_prob_X))
print(var(logit_prob_X))

message("\nProb_X")
print(head( prob_X ))
print(summary(prob_X))
print(var(prob_X))

message("\nBinary from continuous X")
print(head( true_binary_X ))
print(summary(true_binary_X))
print(var(true_binary_X))




