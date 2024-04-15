# ****************************************
# Confounder Handling Simulation Study
# 
# Testing Suite for main and helper functions
# 
# Emma Tarmey
#
# Started:          26/03/2024
# Most Recent Edit: 15/04/2024
# ****************************************


# clear R memory
rm(list=ls())

# all external libraries
library(chest)
library(dplyr)
library(DT)
library(ggdag)
library(ggplot2)
library(glmnet)
library(igraph)
library(microbenchmark)
library(shiny)
library(shinycssloaders)
library(sjmisc)

# fix wd issue
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# simulation-proper code
source("simulation.R")

# top-level simulation parameters
n_obs_init  <- 50
n_rep_init  <- 10
SE_req_init <- 0.05

# initialise DAG
DAG_file       <- "../data/key-input-coef-data-c2.csv"
coef_data      <- read.csv(DAG_file)
DAG_adj_matrix <- adjacency_matrix_from_coef_data(coef_data = coef_data)
DAG_labels     <- colnames(DAG_adj_matrix)
DAG_graph      <- graph_from_adjacency_matrix(DAG_adj_matrix, mode = "directed")


# # TESTING dagitty_from_adjacency_matrix
# DAG_dagitty <- dagitty_from_adjacency_matrix(adj_DAG  = DAG_adj_matrix,
#                                            exposure = "X",
#                                            outcome  = "Y")#
# 
# plot(DAG_graph,
#      layout = layout_as_tree(DAG_graph),
#      main   = paste("DAG of Interest \n", sep = ""))
# 
# plot(DAG_dagitty,
#      layout = layout_as_tree(DAG_graph),
#      main   = paste("DAG of Interest \n", sep = ""))


# TESTING 

# generate training data
data   <- generate_dataset(coef_data = coef_data, n_obs = 200, labels = DAG_labels)
data_X <- data[, -1]
data_y <- data[,  1]

model_lasso <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                    y         = data_y,            # outcome
                    type      = "lasso",
                    intercept = TRUE)

model_least_angle <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                          y         = data_y,            # outcome
                          type      = "lar",
                          intercept = TRUE)

model_fwd_stage <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                        y         = data_y,            # outcome
                        type      = "forward.stagewise",
                        intercept = TRUE)

View(model_lasso)
View(model_least_angle)
View(model_fwd_stage)

