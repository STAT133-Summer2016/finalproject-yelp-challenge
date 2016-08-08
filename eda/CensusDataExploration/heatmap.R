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
library(rgdal)
library(rgeos)
library(gpclib)       # loads polygon clipping library
library(maptools)     # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables

# data file names:
data_file <- "../../clean_data/StopData_merged2.rds" # use the cleaned version of StopData_merged.rds for plotting. See StopData_clean_merged_rds.R for more information.
census_tract_folder <- "../../clean_data/Census_Tract_Polygons2010"
census_tract_shp <- "Census_tracts_2010"
census_tract_csv <- "../../clean_data/Census_tracts_2010.csv"


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
berkMap = map = get_map(location = c( lon = mean(lonvals), lat = mean(latvals) ), zoom = 13)

# get the census shape files:
##locationCensusFiles <- "C:/Users/Rebecca/Dropbox/School/2016_su_School/stat133/finalproject_angry_ladies/Rebeccas_location_Code2/Census_Block_Group_Polygons2010"
#blocks <- readOGR("Census_Block_Group_Polygons2010","Census_blockgroups_2010", verbose = TRUE)
census <- readOGR(census_tract_folder,census_tract_shp, verbose = TRUE)

# blocks.polygons <- blocks@polygons
# blocks.data <- blocks@data
# blocks.plot.order <- blocks@plotOrder
census2 <- spTransform(census, CRS("+proj=longlat +datum=WGS84"))
census3 <- fortify(census2)
#census4 <- census2@data
#pop <- census4$TotalPop
# census3 <- census3 %>%
#   mutate(id = as.factor(id))
# popdf <- as.vector(data.frame(id = as.character(unique(census3$id)), pop = as.character(pop), stringsAsFactors = FALSE, mode = "list" ))
# census5 <- merge(census3, popdf, by = id)

census4 <- read.csv(file=census_tract_csv, header = TRUE)
census4$id <- 0:32
census5 <- merge(census3, census4, by='id')

#usa <- map_data("usa") # we already did this, but we can do it again
# ggmap(berkMap) + geom_polygon(data = census3, aes(x=long, y = lat, group = group)) + 
#   coord_fixed(1.3)




# mappp <- ggmap(berkMap) + 
#   geom_polygon(data = census5, aes(x=lofill = TotalPop), color = "white") +
#   geom_polygon(color = "black", fill = NA) +
#   theme_bw() + 
#   scale_fill_gradient(trans = "log10")
# mappp

# 
# ca_base <- ggmap(berkMap) + ggplot(data = census5, mapping = aes(x = long, y = lat, group = group)) + 
#   coord_fixed(1.3) + 
#   geom_polygon(color = "black", fill = "gray")
# ca_base + theme_nothing()

elbow_room1 <- ggmap(berkMap) + 
  geom_polygon(data = census5, aes(x = long, y = lat, group = group, fill = TotalPop), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  coord_fixed(1.3) + 
  theme_bw()

elbow_room1 








# ggmap(berkMap) + get_map(census5)
# 
# 
#   stat_density2d(aes(x = long, y = lat, color = TotalPop),
#                  size = 2, bins = 5, data = census5, geom = "polygon") +
#   theme (panel.grid.major = element_blank (), # remove major grid
#          panel.grid.minor = element_blank ()  # remove minor grid
#   ) + 
#   ggtitle("Population Density by Census 2010 Tract Polygons") +
#   labs(alpha = element_blank())


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
