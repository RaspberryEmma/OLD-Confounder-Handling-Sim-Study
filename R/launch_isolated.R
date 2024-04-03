# ****************************************
# Confounder Handling Simulation Study
#
# Isolated form of more flexible wider-sim
# Provides the following:
#    (1) Pre-set conditions, such as the DAG of interest and n_obs
#    (2) Call to simulation procedure
#    (3) Interpretation of results and plotting
#
# Emma Tarmey
#
# Started:          19/03/2024
# Most Recent Edit: 03/04/2024
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

# intialise DAG
coef_data      <- read.csv("../data/key-input-coef-data.csv")
DAG_adj_matrix <- read.csv("../data/key-input-adjacency-matrix.csv") %>% as.matrix()

DAG_labels               <- DAG_adj_matrix[, 1]
rownames(DAG_adj_matrix) <- DAG_labels
DAG_adj_matrix           <- DAG_adj_matrix[, -1]
DAG_graph                <- graph_from_adjacency_matrix(DAG_adj_matrix, mode = "directed")

# models to fit and results metrics to measure
model_methods   <- c("stepwise", "LASSO")
results_methods <- c("r_squared", "param_bias", "blocked_paths", "benchmark")

# simulation procedure call
run(graph           = DAG_graph,
    coef_data       = coef_data,
    n_obs           = n_obs_init,
    n_rep           = n_rep_init,
    labels          = DAG_labels,
    model_methods   = model_methods,
    results_methods = results_methods)


