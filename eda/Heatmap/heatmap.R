# Title: heatmap.R
# Purpose: fix the heat map for the Berkeley sensus data
# Author: Rebecca Reus
# Date: 8-4

library(sp)
library(ggmap)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(rgeos)

# data file names:
data_file <- "../../clean_data/StopData_merged2.rds" # use the cleaned version of StopData_merged.rds for plotting. See StopData_clean_merged_rds.R for more information.

# read in the data to modify:
df <- readRDS( data_file )

# Location info (to set lim values for coordinates):
latmax <- max( df$lat, na.rm = TRUE )
latmin <- min( df$lat, na.rm = TRUE )
lonmax <- max( df$long, na.rm = TRUE )
lonmin <- min( df$long, na.rm = TRUE )
latvals <- c( latmin, latmax )
lonvals <- c( lonmin, lonmax )
######################################

# get the plain Berkeley map from Google:
berkMap = map = get_map(location = c( lon = mean(lonvals), lat = mean(latvals) ), zoom = 14)

# get the census shape files:
##locationCensusFiles <- "C:/Users/Rebecca/Dropbox/School/2016_su_School/stat133/finalproject_angry_ladies/Rebeccas_location_Code2/Census_Block_Group_Polygons2010"
blocks <- readOGR("Census_Block_Group_Polygons2010","Census_blockgroups_2010", verbose = TRUE)

blocks.polygons <- blocks@polygons
blocks.data <- blocks@data
blocks.plot.order <- blocks@plotOrder
b2 <- spTransform(blocks, CRS("+proj=longlat +datum=WGS84"))
b3 <- fortify(b2)

ggmap(berkMap) +
  stat_density2d(aes(x = long, y = lat, fill= ..level.., alpha = .2* ..level..),
                 size = 2, bins = 5, data = b3, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "red") +
  theme (panel.grid.major = element_blank (), # remove major grid
         panel.grid.minor = element_blank ()  # remove minor grid
  ) + 
  ggtitle("Population Density") +
  labs(alpha = element_blank())+
  guides(alpha = FALSE)


### Heat Map 2: All BPD Stops Density, 2015-2016

# a contour plot
ggmap(berkMap) +
  stat_density2d(aes(x = long, y = lat, fill= ..level.., alpha = .2* ..level..),
                 size = 2, bins = 5, data = mergedf, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "red") +
  theme (panel.grid.major = element_blank (), # remove major grid
         panel.grid.minor = element_blank ()  # remove minor grid
  )+ 
  ggtitle("All BPD Stops Density, 2015-2016") +
  labs(alpha = element_blank())+
  guides(alpha = FALSE)
