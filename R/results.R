# ****************************************
# Confounder Handling Simulation Study
#
# Examining Results and Plotting
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
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )


detect_most_recent_timestamp <- function() {
  timestamp <- 0.0
  
  # extract all file names from data directory
  files <- list.files("../data")
  
  # look only at timestamps
  files <- substr(files, start = 1, stop = 19)
  
  # filter invalid POSIX timestamps with regex
  # pattern: "YYYY-MM-DD HH:MM:SS" 
  files <- grep(pattern = "[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}",
                x       = files,
                value   = TRUE)
  
  # find most recent timestamp (highest epoch)
  filestamp <- NULL
  for (file in files) {
    filestamp <- as.integer(as.POSIXct( file ))
    if (filestamp > timestamp) { timestamp <- filestamp }
  }
  
  # reconvert to string, cut off timezone marker
  timestamp <- as.POSIXct(timestamp, origin = '1970-01-01') %>% substr(., start = 1, stop = 19)
  
  return (timestamp)
}


# detect most recent time string
date_string <- detect_most_recent_timestamp()


# import
data    <- read.csv(paste("../data/", recent_date_string, "-dataset.csv", sep = ""))
DAG     <- read.csv(paste("../data/", recent_date_string, "-adjacency-matrix.csv", sep = ""))
results <- read.csv(paste("../data/", recent_date_string, "-results-table.csv", sep = ""))


# data pre-processing
labels        <- DAG[,  1]
DAG           <- DAG[, -1]
rownames(DAG) <- labels


# convert wide to long
results_long <- results %>%
  pivot_longer(
    cols      = colnames(results)[2]:colnames(results)[ncol(results)],
    names_to  = "Metric",
    values_to = "Value"
)


# plot DAG
png(paste("../plots/", date_string, "_DAG_plot.png"))
gd <- graph_from_adjacency_matrix( adjmatrix = as.matrix(DAG), mode = c("directed"))
plot(gd,
     layout = layout_as_tree(gd),
     main   = paste("DAG of Interest \n", date_string, sep = ""))
dev.off()


# plot data correlation plot
png( paste("../plots/", date_string, "_synthetic_data_correlation_plot.png", sep = "") )
p <- data %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle( paste("Synthetic Data Correlation Plot \n", date_string, sep = "") )
p
dev.off()


# plot results heat-map
#png( paste("../plots/", date_string, "_results_plot.png", sep = "") )
#p <- ggplot(results_long, aes(x = X, y = Metric, fill = Value)) +
#  geom_tile() +
#  ggtitle( paste("Results Plot \n", date_string, sep = "") )
#p
#dev.off()


