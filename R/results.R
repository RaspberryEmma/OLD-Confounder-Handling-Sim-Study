# ****************************************
# Confounder Handling Simulation Study
#
# Examining Results and Plotting
#
# Emma Tarmey
#
# Started:          19/03/2024
# Most Recent Edit: 23/08/2024
# ****************************************


# all external libraries
library(dagitty)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(glmnet)
library(igraph)
library(microbenchmark)
library(sjmisc)
library(tidyr)


# fix wd issue
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )


detect_most_recent_timestamp <- function() {
  timestamp <- 0.0
  
  # extract all file names from data directory
  files <- list.files("../data")
  
  # look only at timestamps
  files <- substr(files, start = 1, stop = 19)
  
  # filter invalid POSIX timestamps with regex
  # pattern: "YYYY-MM-DD_HH-MM-SS" 
  files <- grep(pattern = "[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}_[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}",
                x       = files,
                value   = TRUE)
  
  # find most recent timestamp (highest epoch)
  filestamp <- NULL
  for (file in files) {
    filestamp <- as.integer(as.POSIXct( file, format = "%Y-%m-%d_%H-%M-%S" ))
    if (filestamp > timestamp) { timestamp <- filestamp }
  }
  
  # reconvert to string, cut off timezone marker
  timestamp <- as.POSIXct( timestamp, origin = '1970-01-01') %>% substr(., start = 1, stop = 19)
  
  timestamp <- gsub(":", "-", timestamp)
  timestamp <- gsub(" ", "_", timestamp)
  
  return (timestamp)
}


generate_all_plots <- function(case = NULL) {
  case_string <- paste("c", case, sep = "")
  
  # import
  data    <- read.csv(paste("../data/", case_string, "-dataset.csv", sep = ""))
  coefs   <- read.csv(paste("../data/", case_string, "-coef-data.csv", sep = ""))
  results <- read.csv(paste("../data/", case_string, "-results-table.csv", sep = ""))
  
  
  # data pre-processing
  DAG           <- adjacency_matrix_from_coef_data(coefs)
  labels        <- colnames(DAG)
  rownames(DAG) <- labels
  data          <- data[, -1]
  
  
  # convert wide to long
  results_long <- results %>%
    pivot_longer(
      cols      = colnames(results)[2]:colnames(results)[ncol(results)],
      names_to  = "Metric",
      values_to = "Value"
  )
  
  
  # plot DAG
  png(filename = paste("../plots/", case_string, "_DAG_plot.png", sep = ""),
      width = 540, height = 540, units = "px")
  gd <- graph_from_adjacency_matrix( adjmatrix = as.matrix(DAG), mode = c("directed"))
  plot(gd,
       layout = layout_as_tree(gd),
       main   = paste("DAG of Interest \n", case_string, sep = "")) %>% print()
  dev.off()
  
  
  # plot data correlation plot
  png(filename = paste("../plots/", case_string, "_synthetic_data_correlation_plot.png", sep = ""),
      width = 540, height = 540, units = "px")
  p <- data %>% cor() %>% ggcorrplot::ggcorrplot() +
    ggtitle( paste("Synthetic Data Correlation Plot \n", case_string, sep = "") )
  p %>% print()
  dev.off()
  
}

