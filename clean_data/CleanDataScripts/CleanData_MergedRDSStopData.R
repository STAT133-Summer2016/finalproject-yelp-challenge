# CleanData_MergedRDSStopData.R

# Author: Rebecca Reus

# Purpose: 
# 1. This script does some minor necessary cleaning to StopData_merged.rds data.

# Notes:
# StopData_merged.rds was created by the location cleaning script followed by the merging script.

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
data_file <- "../../raw_data/StopData_merged.rds" # we will be modifying this file.
new_data_file <- data_file # rewrite the exisiting file.

# read in the data to modify:
df <- readRDS( data_file )

# make the changes necessary for plotting and mapping the data:
df$CarSearch <- factor(df$CarSearch)
levels(df$CarSearch) <- c("No Search", "Search")  
df$Race <- factor(df$Race)
levels(df$Race) <- c("Asian", "Black", "Hispanic", "Other", "White")
df$Enforcement <- factor(df$Enforcement)
levels(df$Enforcement) <- c("Arrest", "Citation", "Other", "Warning")
df$Reason <- factor(df$Reason)
levels(df$Reason) <- c("Other", "Investigation", "Probation/Parole", "Reasonable Suscipcion", "Traffic", "Wanted")
df <- df %>%
  mutate(Arrest = ifelse( 
    as.character(Enforcement) == "Arrest", 
    "Arrested",
    "Not Arrested") ) %>%
  mutate(Arrest = factor(Arrest))
v <- factor(df$Other)
df <- df %>%
  mutate(Emergency.Psych.Eval = ifelse( str_detect(as.character(Other), "MH"), 
                                        yes = "Yes",
                                        no = "No") )
df$Emergency.Psych.Eval <- factor(df$Emergency.Psych.Eval)

# save the changed data as a new RDS (RDS file saves more space than  CVS file):
#saveRDS(object = df, file = new_data_file )

# remove unecessary variables:
rm(v)
rm(data_file)
rm(new_data_file)
rm(df)
