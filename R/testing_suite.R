# ****************************************
# Confounder Handling Simulation Study
# 
# Testing Suite for All Main and Helper Functions
# 
# Emma Tarmey
#
# Started:          26/03/2024
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


# TESTING dagitty_from_adjacency_matrix
DAG_dagitty <- dagitty_from_adjacency_matrix(adj_DAG  = DAG_adj_matrix,
                                           exposure = "X",
                                           outcome  = "Y")#

plot(DAG_graph,
     layout = layout_as_tree(DAG_graph),
     main   = paste("DAG of Interest \n", sep = ""))

plot(DAG_dagitty,
     layout = layout_as_tree(DAG_graph),
     main   = paste("DAG of Interest \n", sep = ""))

