# StopData_mergeCleanData.R

# Purpose: this file is for merging the StopData data with location and the cleaned Stop data data frames.
# Author: Rebecca Reus, Crazy Ladies, STAT 133 Summer 2016

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

# Read in the data:
location_df <- readRDS( 'StopData_finaldf.rds' ) # dataframe with location coordinates
clean_df <- read_csv( 'StopData_cleaned.csv') # dataframe with cleaned Disposition columns

# Remove the blank column name in cleaned_df:
colnames(clean_df)[1] <- "first"
colnames(clean_df)[1]
clean_df <- clean_df %>%
  select(-first) 
  
# Select only the coordinates of the location_df:
coords_df <- location_df %>%
  select(lat, long, Incident.Number)
  
# Try to merge:
merged_df <- left_join( x = clean_df, y = coords_df, by = "Incident.Number")

# Save to RDS file:
saveRDS( merged_df, file = "StopData_merged.rds" )
  

