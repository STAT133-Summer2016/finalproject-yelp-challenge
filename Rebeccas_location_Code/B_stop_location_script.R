# Uses part of Geocoding script for large list of addresses created by:
# Shane Lynn 10/10/2013 at http://www.r-bloggers.com/batch-geocoding-with-r-and-google-maps/
# Modified by Rebecca Reus for use with Berkeley policing data for year 2016

setwd("~/Desktop/finalproject_angry_ladies/Rebeccas_location_Code")

#load up the ggmap library and data wrangling / string analysis libraries:
library(sp)
library(ggmap)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# row number to finish at (will start at the last spot):
number_of_rows_to_do<-7700
geocodeQueryCheck()

# get the input data for police stops in Berkeley:
infile <- "B_stop_data"
data <- read.csv(paste0('./', infile, '.csv'))

stop_df<- data
stop_df <-stop_df %>% 
  mutate(Location=paste0(Location, ", Berkeley")) %>%
  mutate(Location=as.character(Location)) %>%
  mutate(Incident.Number=as.character(Incident.Number)) %>%
  mutate(Call.Date.Time=as.character(Call.Date.Time)) %>%
  mutate(Dispositions=as.character(Dispositions))

c<-str_replace_all(stop_df$Location, "/"," and ")

stop_df <- stop_df %>%
  mutate(Location = c) %>%
  mutate(index=as.numeric(rownames(stop_df)))

is.character(data$Location)
is.character(stop_df$Location)

c<-mdy_hms(stop_df$Call.Date.Time)
stop_df<-stop_df %>%
  mutate(Call.Date.Time=c)

is.POSIXct(data$Call.Date.Time)
is.POSIXct(stop_df$Call.Date.Time)

stop_df<-stop_df%>%
   mutate(Year=year(Call.Date.Time)) %>%
  mutate(Date=date(Call.Date.Time)) %>%
  mutate(DayOfWeek=weekdays(Call.Date.Time))


# look at the first few addresses in stop_df
address<-head(stop_df$Location,number_of_rows_to_do)

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)+1
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(address))){
  print(paste("Working on index", ii, "of", length(address)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(address[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

#now we add the latitude and longitude to the main data
stop_df<-join(stop_df,geocoded,by="index")

stop_df60<-stop_df %>%
  subset(index <=number_of_rows_to_do)
