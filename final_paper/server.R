
# load other packages
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plyr)

stop_by <- read_csv("stop_data_jenny.csv")

##Problems:
#1. add_legend
#2. scale
#4. change size of the point 

shinyServer(function(input, output) {
  
  output$Plot <- renderPlot({
  
  test <- subset(stop_by, Hour = input$hour) %>% 
    group_by(Race) %>% 
    tally() 

  rrr$Race<-as.factor(rrr$Race)
  
  ggplot(rrr)+
    geom_bar(aes(x=Race, y = n),
             stat = "identity",
             fill = c("#FFCC00","#000000","#330099","#66FFCC","#FFFFFF"))
  
  })
  

})

