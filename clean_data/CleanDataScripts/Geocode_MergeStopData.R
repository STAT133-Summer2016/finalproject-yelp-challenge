# Geocode_MergeStopData.R

# Author: Rebecca Reus

# Purpose: 
# 1. This script combines the stop data frame containing cleaned disposition variables 
#    with the stop data frame containing cleaned location information. The result will be the cleaned
#    stop data.

###################################################################################################################################
## LIBRARIES:
## Before running, please make sure you have installed ALL of these packages:
###################################################################################################################################

library(sp)
library(ggmap)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)
library(data.table)
library(rworldmap)
library(maps)
library(mapdata)
library(maptools)
library(scales)
library(RgoogleMaps)
library(tmap)
library(sp)
library(rgdal)
library(rgeos)

# define file names:

file_finaldf <- '../../raw_data/StopData_finaldf.rds' # contains the stop data with geolocation information.
file_CleanedDisp <- '../../raw_data/StopData_CleanDisposition.csv'
file_saveResult <-  "../../raw_data/StopData_merged.rds"

# Read in the data:
location_df <- readRDS( file_finaldf ) # dataframe with location coordinates
clean_df <- read_csv( file_CleanedDisp ) # dataframe with cleaned Disposition columns

# Remove the blank column name in cleaned_df:
# colnames(clean_df)[1] <- "first"
# colnames(clean_df)[1]
# clean_df <- clean_df %>%
#   select(-first) 
  
# Select only the coordinates of the location_df:
coords_df <- location_df %>%
  select(lat, long, Incident.Number)
  
# Try to merge:
merged_df <- left_join( x = clean_df, y = coords_df, by = "Incident.Number")

# Save to RDS file:
#saveRDS( merged_df, file = file_saveResult )
  

