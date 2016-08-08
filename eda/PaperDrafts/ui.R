library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hour and the top three group"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Race", "AgeRange", "Gender")),
    
    
    # Main panel
    mainPanel(
      plotOutput("Plot")
      )
      )
      ))



