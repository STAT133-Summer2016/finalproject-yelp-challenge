# CleanData_MakePercentageTables1.R

# Author: Rebecca Reus

# Purpose: 
# 1. Clean the merged location stop data and create and save summary tables to display in paper.
# 2. Clean the jail and arrest data and create and save tables to display in paper.

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
library(tmap)
library(sp)
library(rgdal)
library(rgeos)
library(classInt)
library(gdata)
library(XLConnect)
library(RColorBrewer)

# data file names:
file_stops <- "../StopData_clean.csv" 
file_stopsummary <- "../../clean_data/StopData_summary.csv" 
mergedf <- read_csv(file = file_stops)
census <- read.csv("../../raw_data/Census_Data_2000_And_2010.csv")
census2010 <- census[census$Year==2010,]
totalpop <- census2010[1,]$Amount
stopsummary <- read.csv( file_stopsummary )
stopsummary <- select(stopsummary, -X)

racecensus2010 <- census2010[census2010$Heading=="Not Hispanic or Latino"|census2010$Heading=="HISPANIC OR LATINO AND RACE",]
sexcensus2010 <- census2010[census2010$Heading=="Sex",]
agecensus2010 <- census2010[census2010$Heading=="Age",]


race <- select(racecensus2010, Description, Amount) 
race$Description <- race$Description %>% 
  str_replace_all("Hispanic or Latino.*", "Hispanic") %>% 
  str_replace("Black or African American", "Black") %>% 
  str_replace("American Indian and Alaska Native", "Other") %>% 
  str_replace("Native Hawaiian and Other Pacific Islander", "Other") %>% 
  str_replace("Some other race", "Other") %>% 
  str_replace("Two or more races", "Other")

race <- race[race$Description !="Not Hispanic",]

race <- race %>% 
  group_by(Description) %>% 
  tally(Amount)

names(race) <- c("Description", "Counts")  

sex <- select(sexcensus2010, Description, Amount) 

age <- select(agecensus2010, Description, Amount) 
age <- age[age$Description != "Median age",]
age$Description <- c("0-18", "0-18", "18-64", "65+")

age<- age %>% 
  group_by(Description) %>% 
  tally(Amount)
names(age) <- c("Description", "Counts")  

############################## arrest data ############################## 
arrest <- read.csv("../../raw_data/Berkeley_PD_Log_-_Arrests.csv")

############################## jail data ############################## 
jail <- read.csv("../../raw_data/Berkeley_PD_Log_-_Jail_Bookings.csv")


############################## call for service data ############################## 

callservice<- read.csv("../../raw_data/Berkeley_PD_-_Calls_for_Service.csv") 

callservice<- callservice %>% 
  mutate(latlong= 
           str_replace_all(callservice$Block_Location,"[0-9]* [A-Za-z]*", "") %>%
           str_replace_all("\nBerkeley,\n", "") %>% 
           str_replace("[A-Za-z]*", "") %>% 
           str_replace(";","") %>% 
           str_replace("&","") %>% 
           str_replace("[A-Za-z]*", "")) 

callservice$latlong<- str_replace(callservice$latlong, "\\(", "") %>% 
  str_replace("\\)", "") 

callservice<- callservice %>% 
  separate(latlong, c("lat", "long"), sep=",")

callservice$lat <- as.numeric(callservice$lat)
callservice$long <- as.numeric(callservice$long)

# other:

m <- mergedf %>% 
  group_by(Race) %>% 
  tally()
names(m) <- c("Race", "stop")
names(race) <- c("Race", "census")

pop1 <- left_join(race,m,by = "Race")
pop1 <- pop1 %>% 
  mutate(percentage=(stop/census))
pop1$percentage<- round(pop1$percentage, digits = 2)

pop1 <- pop1 %>% 
  mutate(totalp = (census/totalpop))
pop1$totalp <- round(pop1$totalp, digits =2)



# stop summary table:

ss <- stopsummary %>%
  select(Race, Percent.Stopped, Percent.Race.Stopped)
ss$Percent.Stopped <- percent(ss$Percent.Stopped)
ss$Percent.Race.Stopped <- percent(ss$Percent.Race.Stopped)
names(ss)[2:3] <- c("Percent Stopped", "Percent Population Stopped")


# analyze date and time of stops:

mergedf$Call.Date.Time <- mdy_hm(mergedf$Call.Date.Time)
mergedf$AgeRange <- as.factor(mergedf$AgeRange)

mergedf <- mergedf %>%
  mutate(Hour = as.integer(hour(Call.Date.Time))) %>%
  mutate(Day = as.factor(as.integer(wday(Call.Date.Time))))


# save the clean data:

# saveRDS(callservice, "../CallsForService.rds")
# listOfTables <- list(age, sex, race, pop1, ss)
# saveRDS(listOfTables, "../Tables_AgeSexRacePopStop.rds")
# saveRDS(mergedf, "../StopData_clean.rds")
# write.csv(mergedf, "../clean_data/StopData_clean.csv" )


