library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hour and the top three group"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      sliderInput('hour',
                  'HOUR: ',
                  0, 24, value=7,
                  animate = T)
      ),
    
    
    # Main panel
    mainPanel(
      plotOutput("Plot")
      )
      )
      ))


