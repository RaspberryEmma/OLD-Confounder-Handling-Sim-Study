# ****************************************
# Confounder Handling Simulation Study
# 
# Examining Results and Plotting
# 
# Emma Tarmey
#
# Started:          19/03/2024
# Most Recent Edit: 19/03/2024
# ****************************************

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
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# Record current date time
date_string <- Sys.time()

# import
data    <- read.csv("../data/2024-03-19 13:42:17-dataset.csv")
DAG     <- read.csv("../data/2024-03-19 13:42:17-adjacency-matrix.csv")
results <- read.csv("../data/2024-03-19 13:42:17-results-table.csv")

labels        <- DAG[, 1]
DAG           <- DAG[, -1]
rownames(DAG) <- labels
print(DAG)
print(cor(data))
print(results)

png(paste("../plots/", date_string, "-DAG-plot.png"))
gd <- graph_from_adjacency_matrix( adjmatrix = as.matrix(DAG), mode = c("directed"))
plot(gd, layout = layout_as_tree(gd))
dev.off()
