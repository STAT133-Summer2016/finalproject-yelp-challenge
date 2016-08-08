
# load other packages
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plyr)

stop_by <- read_csv("stop_data_jenny.csv")

mapdf <- read_csv("mergedf.csv")

berkMap = map = get_map(location = c( lon = mean(lonvals), lat = mean(latvals) ), zoom = 14)


##Problems:
#1. add_legend
#2. scale
#4. change size of the point 

shinyServer(function(input, output) {
  
  output$Plot <- renderPlot({
  
  ggmap(berkMap) +
    stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..),
                   bins = I(5), geom = "polygon", data = mapdf ) +
    scale_fill_gradient2( "Stop Density",
                          low = "white", mid = "orange", high = "red", midpoint = 25) +
    labs(x = "Longitude", y = "Latitude") + facet_wrap(~ input$dataset) +
    scale_alpha(range = c(.2, .55), guide = FALSE) +
    ggtitle("BPD Stop Contour Map of Berkeley by Age Range") +
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
  
  })
  

})

