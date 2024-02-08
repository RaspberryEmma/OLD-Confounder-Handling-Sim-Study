# ****************************************
# Confounder Handling Simulation Study
# 
# Controls the GUI of the Shiny app, interfaces with the simulation code
# Provides three things:
#    (1) UI Code
#    (2) Server Code
#    (3) Call to 'shinyApp' function
# 
# Emma Tarmey
#
# Started:          31/01/2024
# Most Recent Edit: 08/02/2024
# ****************************************


library(dplyr)
library(ggdag)
library(ggplot2)
library(igraph)
library(shiny)
library(shinycssloaders)


ui <- fluidPage(
  # App title ----
  titlePanel("Confounder Handling Simulation Study"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
        
      helpText("Do something."),
      
      checkboxGroupInput(inputId = "checkGroup",
                         label   = "Statistical Methods",
                         choices = list("Stepwide Regression" = 1,
                                        "Change-in-Estimate"  = 2,
                                        "TMLEs"               = 3),
                         selected = 1),
      
      numericInput(inputId = "n_node",
                   label   = "n_node",
                   value   = 1),
      
      numericInput(inputId = "n_obs",
                   label   = "n_obs",
                   value   = 100),
      
      numericInput(inputId = "SE_req",
                   label   = "SE_req",
                   value   = 0.05),
      
      
      actionButton("action", label = "Action")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram
      plotOutput(outputId = "distPlot") %>% withSpinner(color="#0dc5c1"),
      
      # Output: Extracting all variables
      fluidRow(column(1, verbatimTextOutput("checkGroup")),
               column(2, verbatimTextOutput("n_node")),
               column(3, verbatimTextOutput("n_obs")),
               column(4, verbatimTextOutput("SE_req")),
               column(5, verbatimTextOutput("value")),
               column(6, verbatimTextOutput("wd"))
      ),
    )
  )
)


server <- function(input, output) {
  # Perform computations here
  outputDAG <- reactive({
    ?graph
    gd <- graph( n = input$n_node, edges = c() )
    gd
  })
  
  # Reactive plot
  output$distPlot <- renderPlot({
    # demonstrate progress spinner
    Sys.sleep(0.2)
    
    gd <- outputDAG()
    plot(gd)
  })
  
  # Reactive outputs
  output$checkGroup <- renderPrint({ input$checkGroup })
  output$n_node     <- renderPrint({ input$n_node })
  output$n_obs      <- renderPrint({ input$n_obs })
  output$SE_req     <- renderPrint({ input$SE_req })
  output$value      <- renderPrint({ as.logical(input$action %% 2) })
  output$wd         <- renderPrint({ getwd() })

}

shinyApp(ui = ui, server = server)


