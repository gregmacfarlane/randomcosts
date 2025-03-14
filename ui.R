library(shiny)
# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cash Flow Generator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      htmlOutput("info"),
      # Input: Numeric entry for seed number
      numericInput("seed", "Seed Number:", value = 1),
      actionButton("simulate", "Generate a problem!"),
      # Output: Text for problem
      htmlOutput("problemText"),

      # have a hidden text output with the result
      htmlOutput("solution")
    ),
    
    mainPanel(
      # Output: Plot
      plotOutput("distPlot"),
      
      # Output: Text
      textOutput("text")
    )
  ))
)
