library(dplyr)
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
      
      checkboxGroupInput("checkGroup",
                         label = "Checkbox group",
                         choices = list("Choice 1" = 1,
                                        "Choice 2" = 2,
                                        "Choice 3" = 3),
                         selected = 1),
      
      radioButtons("radio",
                   label = "Radio buttons",
                   choices = list("Choice 1" = 1, "Choice 2" = 2,
                                  "Choice 3" = 3),selected = 1),
      
      numericInput("numeric",
                   label = "Numeric input",
                   value = 1),
      
      selectInput("dropdown",
                  label = "Dropdown menu",
                  choices = list("A",
                                 "B",
                                 "C",
                                 "D"),
                  selected = "A"),
        
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "samples",
                  label = "Slider input",
                  min = 1,
                  max = 100,
                  value = 10),
      
      actionButton("action", label = "Action")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram
      plotOutput(outputId = "distPlot") %>% withSpinner(color="#0dc5c1"),
      
      # Output: Extracting all variables
      fluidRow(column(2, verbatimTextOutput("checkGroup")),
               column(3, verbatimTextOutput("radio")),
               column(4, verbatimTextOutput("numeric")),
               column(5, verbatimTextOutput("dropdown")),
               column(6, verbatimTextOutput("samples")),
               column(7, verbatimTextOutput("value"))
      ),
    )
  )
)


server <- function(input, output) {
  
  # Reactive plot with input variable bins
  output$distPlot <- renderPlot({
    
    # generate some number of normal distribution observations
    x <- seq(from = -5, to = 5, length.out = input$samples)
    y <- dnorm(x, mean = 0, sd = 1)
    
    # demonstrate progress spinner
    Sys.sleep(0.2)
    
    plot(x = x,
         y = y,
         xlab = "Values",
         main = "Normal PDF")
  })
  
  # React
  output$checkGroup <- renderPrint({ input$checkGroup })
  output$radio      <- renderPrint({ input$radio })
  output$numeric    <- renderPrint({ input$numeric })
  output$dropdown   <- renderPrint({ input$dropdown })
  output$bins       <- renderPrint({ input$bins })
  output$value      <- renderPrint({ as.logical(input$action %% 2) })
}

shinyApp(ui = ui, server = server)
