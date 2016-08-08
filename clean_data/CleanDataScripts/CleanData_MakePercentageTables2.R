# CleanData_MakePercentageTables2.R

# Author: Rebecca Reus

# Purpose: 
# 1. Manipulate census and merged location stop data to produce small tables of percentages to display in paper.
# 2. Please note that this code has already been run, so it doesn't need to be run again.
# 3. Look in the clean data folder for the .rds file containing the tables created by this script.

# Notes:
# Please look at the documentation below to understand how to read the tables in from the files.


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
library(rgeos)
library(scales)
library(RgoogleMaps)
library(sp)
library(rgdal)
library(rgeos)
library(classInt)
library(gdata)
library(XLConnect)
library(RColorBrewer)


# data files:

file_stops <- "../../clean_data/StopData_clean.csv" 
file_tract_populations <- "../../clean_data/berk_census2010_clean.csv"
file_saveAs <- "../../clean_data/StopData_tbls.rds" 
mergedf <- read_csv(file = file_stops)
bpop2 <- read_csv(file = file_tract_populations )

# stop data sumamary by race:

tbl_summary <- mergedf %>%  
  group_by(Race) %>%
  summarise(Stopped=n() ) %>%
  mutate(Percent.Stopped = Stopped/nrow(mergedf)) 
temp <- c(sum(bpop2$Asian), sum(bpop2$Black), sum(bpop2$Hispanic), sum(bpop2$Other), sum(bpop2$White) )
tbl_summary$Race.Population = temp
tbl_summary$Percent.Race.Stopped = tbl_summary$Stopped/tbl_summary$Race.Population
tbl_summary <- tbl_summary %>%
  select(-Stopped, -Race.Population)
tbl_summary

# percent reason:

tbl_reason <- mergedf %>%  
  group_by( Reason) %>%
  summarise(Stopped=n() ) %>%
  mutate(Percent.Stopped = Stopped/nrow(mergedf)) 

tbl_reason <- tbl_reason[-2,]
tbl_summary$Percent.Race.Stopped = tbl_summary$Stopped/temp
tbl_reason

# percent enforced by race:

tbl_enforcement <- mergedf %>% 
  group_by(Enforcement) %>%
  summarise(Stopped = n()) %>%
  mutate(Percent.Stopped = Stopped/nrow(mergedf))
tbl_enforcement

tbl_enforcement_W <- subset(mergedf, Race == "Asian") %>%
  group_by(Enforcement) %>%
  summarise(Stopped=n() ) %>%
  mutate(Percent.Asian = Stopped/temp[1] )
tbl_enforcement$Percent.Asian <- tbl_enforcement_W$Percent.Asian
tbl_enforcement

tbl_enforcement_W <- subset(mergedf, Race == "Black") %>%
  group_by(Enforcement) %>%
  summarise(Stopped=n() ) %>%
  mutate(Percent.Black = Stopped/temp[2] )
tbl_enforcement$Percent.Black <- tbl_enforcement_W$Percent.Black
tbl_enforcement

tbl_enforcement_W <- subset(mergedf, Race == "Hispanic") %>%
  group_by(Enforcement) %>%
  summarise(Stopped=n() ) %>%
  mutate(Percent.Hispanic = Stopped/temp[3] )
tbl_enforcement$Percent.Hispanic <- tbl_enforcement_W$Percent.Hispanic
tbl_enforcement

tbl_enforcement_W <- subset(mergedf, Race == "Other") %>%
  group_by(Enforcement) %>%
  summarise(Stopped=n() ) %>%
  mutate(Percent.Black = Stopped/temp[4] )
tbl_enforcement$Percent.Other <- tbl_enforcement_W$Percent.Other
tbl_enforcement

tbl_enforcement_W <- subset(mergedf, Race == "White") %>%
  group_by(Enforcement) %>%
  summarise(Stopped=n() ) %>%
  mutate(Percent.White = Stopped/temp[5] )
tbl_enforcement$Percent.White <- tbl_enforcement_W$Percent.White
tbl_enforcement
rm(tbl_enforcement_W)

# save the resulting 3 data frames:
saveRDS(list(tbl_summary, tbl_reason, tbl_enforcement), file = file_saveAs)

# how to read in the data: 

dataframelist <- readRDS(file = file_saveAs)

# how to exract the data frames from the list:
table1 <- data.frame(dataframelist[1])
table2 <- data.frame(dataframelist[2])
table3 <- data.frame(dataframelist[3])

