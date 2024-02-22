# ****************************************
# Confounder Handling Simulation Study
# 
# Controls the GUI of the Shiny app, interfaces with the simulation code
# Provides three things:
#    (1) UI Code
#    (2) Server Code
#    (3) Call to 'shinyApp' function
#
# TODO: implement data-set generation, have "run simulation" button do that
# 
# Emma Tarmey
#
# Started:          31/01/2024
# Most Recent Edit: 14/02/2024
# ****************************************

# simulation-proper
source("../simulation.R")

library(dplyr)
library(ggdag)
library(ggplot2)
library(igraph)
library(shiny)
library(shinycssloaders)


# initial conditions
n_node_init <- 3
n_obs_init  <- 50
n_rep_init  <- 100
SE_req_init <- 0.05


ui <- fluidPage(
  # App title ----
  titlePanel("Confounder Handling Simulation Study"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
        
      helpText("Simulation Parameters"),
      
      checkboxGroupInput(inputId = "model_methods",
                         label   = "Statistical Methods",
                         choices = list("Stepwide Regression" = "stepwise",
                                        "Change-in-Estimate"  = "change_in_est",
                                        "TMLEs"               = "TMLEs"),
                         selected = "stepwise"),
      
      numericInput(inputId = "n_node",
                   label   = "n_node",
                   value   = n_node_init),
      
      numericInput(inputId = "n_obs",
                   label   = "n_obs",
                   value   = n_obs_init),
      
      numericInput(inputId = "n_rep",
                   label   = "n_rep",
                   value   = n_rep_init),
      
      numericInput(inputId = "SE_req",
                   label   = "SE_req",
                   value   = SE_req_init),
      
      actionButton("run_sim", label = "Run Simulation")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # DAG Plot
      p("Causal Structure"),
      plotOutput(outputId = "distPlot") %>% withSpinner(color="#0dc5c1"),
      
      # DAG table
      headerPanel(""),
      p("Adjacency Matrix"),
      DT::dataTableOutput("DAG_table"),
      
      # Working Directory
      headerPanel(""),
      p("Working Directory (testing)"),
      verbatimTextOutput("wd"),
      
      # Node Labels
      headerPanel(""),
      p("Node Labels (testing)"),
      verbatimTextOutput("labels"),
    )
  )
)


server <- function(input, output) {
  
  # Initialise DAG input data table
  DAG_data      <- reactiveValues(data = NULL)
  DAG_data$data <- data.frame( matrix(0, nrow = n_node_init, ncol = n_node_init) )
  
  # Initialise DAG node labels (wlog, 'y' is always first)
  DAG_data$labels <- reactiveValues(data = NULL)
  DAG_data$labels <- c("y", paste("X", 1:(n_node_init-1), sep = ""))
  
  # Dynamic DAG node labels
  DAG_labels <- reactive({
    labels <- c("y")
    if (input$n_node == 2) { labels <- c("y", "X") }
    if (input$n_node  > 2) { labels <- c("y", "X", paste("Z", 1:( input$n_node - 2 ), sep = "")) }
    labels
  })
  
  # Edit DAG input data table dimensions
  observeEvent(
    eventExpr   = {input$n_node},
    
    handlerExpr = {
      old_n_node <- nrow( DAG_data$data )
      new_n_node <- input$n_node
      diff       <- abs(new_n_node - old_n_node)
      DAG_matrix <- as.matrix( DAG_data$data )
      
      if (new_n_node > old_n_node) {
        # add diff-many columns of size old_n_node
        for (i in 1:diff) {
          DAG_matrix <- cbind(DAG_matrix, rep(0, old_n_node))
        }
        
        # add diff-many rows of size new_n_node
        for (i in 1:diff) {
          DAG_matrix <- rbind(DAG_matrix, rep(0, new_n_node))
        }
      }
      else if (old_n_node > new_n_node) {
        # remove diff-many columns of size old_n_node and diff-many rows of size new_n_node
        DAG_matrix <- DAG_matrix[1:new_n_node, 1:new_n_node]
      }
      else { # old_n_node = new_n_node
        # make no changes
      }
      
      # update DAG data
      DAG_data$data <- ( DAG_matrix %>% data.frame() %>% setNames( DAG_labels() ) )

    }
  )
  
  # Edit DAG input data table contents
  observeEvent(input$DAG_table_cell_edit, {
    row <- input$DAG_table_cell_edit$row
    col <- input$DAG_table_cell_edit$col
    val <- input$DAG_table_cell_edit$value
    
    DAG_data$data[row, col] <- as.numeric(val)
  })
  
  # Reactive DAG input table
  output$DAG_table = DT::renderDT(
    expr     = {
      display           <- DAG_data$data
      rownames(display) <- DAG_labels()
      display
    },
    server   = FALSE,
    editable = TRUE,
    options  = list(dom = "t") # see "https://datatables.net/reference/option/dom"
  )
  
  # Reactive DAG generation using input table
  outputDAG <- reactive({
    gd <- graph_from_adjacency_matrix( adjmatrix = as.matrix(DAG_data$data),
                                       mode      = c("directed"))
    gd
  })
  
  # Reactive plot
  output$distPlot <- renderPlot({
    # demonstrate progress spinner
    Sys.sleep(0.2)
    
    gd <- outputDAG()
    plot(gd, layout = layout_as_tree(gd))
  })
  
  # Run simulation when button is pressed
  observeEvent(
    eventExpr   = {input$run_sim},
    handlerExpr = {
      run(graph         = outputDAG(),
          n_obs         = input$n_obs,
          labels        = DAG_labels(),
          model_methods = input$model_methods)
    }
  )
  
  # Testing!
  output$wd     <- renderPrint({ getwd() })
  output$labels <- renderPrint({ DAG_labels() })

}

shinyApp(ui = ui, server = server)


