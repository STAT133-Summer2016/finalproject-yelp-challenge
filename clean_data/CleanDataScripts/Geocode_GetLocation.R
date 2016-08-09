# Geocode_GetLocation.R

# Author: Rebecca Reus
# Note: I modified and built upon the geocode function written by 
# Shane Lynn, 10/10/2013 at http://www.r-bloggers.com/batch-geocoding-with-r-and-google-maps/

# Purpose:
# 1. Do some preliminary data-tidying of the Berkeley Stop Data.
# 2. Obtain the location coordinates from the string location through geolocation.
# 3. Save temporary results as an RDS file when query limit is exhausted.

# please make sure you have all of these installed before running:

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

###################################################################################################################################
## DATA TIDYING:
###################################################################################################################################

# Get the data from the file in the raw data folder:
infile <- "../../raw_data/StopData"
data <- read.csv(paste0('./', infile, '.csv'))
tempfilename_finaldf <- paste0( infile, '_temp_finaldf')
tempfilename_finaldf_CSV <- "../../raw_data/StopData_w_Locations.csv"

# Filter out the data that is not already in coordinate form (doesn't have longitude and latitude values):
stop_df <- data %>%
  subset( !str_detect( Location, "~-"))

# Since we removed some rows, we need to rename the row names to match the row number (used for indexing):
rownames(stop_df) <- 1:nrow(stop_df)

# We need to adjust the Location data to account for popular abbreviations and mispellings, in addition to some other data tidying:
stop_df <- stop_df %>%
  mutate( Location = as.character( Location ) ) %>%                  # location = character information
  mutate( Location = paste0(Location, " , Berkeley, CA, USA") ) %>%
  mutate( Location = str_replace( Location, "/", " and " ) ) %>%     # change / to and
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "UN " ), 
              yes = str_replace_all( Location, "UN ", "UNIVERSITY AVE " ), 
              no = Location ) ) %>%
  
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "^CO " ), 
              yes = str_replace_all( Location, "^CO ", "COLLEGE AVE " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, " DER " ), 
              yes = str_replace_all( Location, " DER ", " DERBY ST " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "TELE " ), 
              yes = str_replace_all( Location, "TELE ", "TELEGRAPH AVE " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, " 6nsn339" ), 
              yes = str_replace_all( Location, " 6nsn339", "" ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "ASH " ), 
              yes = str_replace_all( Location, "ASH ", "ASHBY AVE " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "DWI " ), 
              yes = str_replace_all( Location, "DWI ", "DWIGHT WAY " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "BANC " ), 
              yes = str_replace_all( Location, "BANC ", "BANCROFT WAY " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "PID " ), 
              yes = str_replace_all( Location, "PID ", "PIEDMONT AVE " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, " 5VNA390" ), 
              yes = str_replace_all( Location, " 5VNA390", "" ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "NOF " ), 
              yes = str_replace_all( Location, "NOF ", "" ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "UNI " ), 
              yes = str_replace_all( Location, "UNI ", "UNIVERSITY AVE " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "SAC " ), 
              yes = str_replace_all( Location, "SAC ", "SACRAMENTO ST " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "ASHY " ), 
              yes = str_replace_all( Location, "ASHY ", "ASHBY AVE " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, " EO " ), 
              yes = str_replace_all( Location, " EO ", " " ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, "DELAWRAE" ), 
              yes = str_replace_all( Location, "DELAWRAE", "DELAWARE" ), 
              no = Location ) ) %>% 
  mutate( Location = 
            ifelse( 
              test = str_detect( Location, " AS " ), 
              yes = str_replace_all( Location, " AS ", " ASHBY AVE " ), 
              no = Location ) ) %>% 
  mutate( Incident.Number = as.character( Incident.Number ) ) %>%    # make sure character information is of the character data type
  mutate( Call.Date.Time = as.character( Call.Date.Time ) ) %>%
  mutate( Dispositions=as.character( Dispositions ) ) %>%
  mutate( index = as.numeric( rownames( stop_df ) ) ) %>%            # this creates an index column, which will come in handy when using the tmp file
  mutate( Call.Date.Time = mdy_hms( Call.Date.Time ) ) %>%           # make sure that the date is of the POSIXt data type
  mutate( Year = year( Call.Date.Time ) ) %>%                        # new variable: year only (info is of 2015 and 2016)
  mutate( Date = date( Call.Date.Time ) ) %>%                        # new variable: date only
  mutate( DayOfWeek = weekdays( Call.Date.Time ) ) %>%               # new variable: day of the week
  mutate( Location = ifelse( 
    test = str_detect( Location, " BLOCK" ), 
    yes = str_replace( Location, " BLOCK", "" ), 
    no = Location))                                                  # "BLOCK" Interferes with Google server info

saveRDS( stop_df, 'StopData_stop_df.rds' ) # save this form of stopdf for later.

####################################################################################################################################
## GETTING LOCATION COORDINATES WITH GOOGLE:
####################################################################################################################################

# Choose a row number to finish at (will start at the last spot using the temp file):
number_of_rows_to_do <- 16255

# Select the addresses from the Location column of stop_df to run through Google:
#address<-head(stop_df$Location,number_of_rows_to_do)
address <- stop_df$Location[ 1:number_of_rows_to_do ]
address

# Define a function that will process Google's server responses:
getGeoDetails <- function( address ) {   
  
  # Use the gecode() function from the ggmap library to query Google's servers:
  geo_reply = geocode( address, output='all', messaging=TRUE, override_limit=TRUE ) 
  
  # Extract the info we need from the returned list:
  answer <- data.frame( lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA )
  answer$status <- geo_reply$status
  
  # If we are over the query limit, pause for an hour:
  while( geo_reply$status == "OVER_QUERY_LIMIT" ) {
    print( "OVER QUERY LIMIT - Pausing for 1 hour at:" ) 
    time <- Sys.time()
    print( as.character( time ) )
    Sys.sleep( 60*60 )
    geo_reply = geocode( address, output='all', messaging=TRUE, override_limit=TRUE )
    answer$status <- geo_reply$status
  }
  
  # If no location match, return NA:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  
  # Else, put what needed info from the Google server reply into our answer dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng  
  
  # Create an accuracy column:
  if ( length( geo_reply$results[[1]]$types ) > 0 ) {
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  
  # Create columns for the other information Google supplies:
  answer$address_type <- paste( geo_reply$results[[1]]$types, collapse=',' )
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  # Return the answer dataframe:
  return( answer )
  
}

# Initialise a dataframe to hold the results when we use the above function:
geocoded <- data.frame()

# Find out where to start in the address list (if there is a temp file in your directory folder). If a temp file exists - load it up and count the rows!
startindex <- 1
tempfilename <- paste0( infile, '_temp_geocoded.rds' )
if ( file.exists( tempfilename ) ) {
  print( "Found temp file - resuming from index:" )
  geocoded <- readRDS( tempfilename )
  startindex <- nrow( geocoded ) + 1
  print( startindex )
}

# Start the geocoding process by using the function we defined above. We'll go address by address. The geocode() function takes care of query speed limit.
for ( ii in seq( startindex, length( address ) ) ) {
  
  # Tell us which row is being processed:
  print( paste( "Working on index", ii, "of", length( address ) ) )
  
  # Query the Google geocoder - this will pause here if we are over the limit:
  result = getGeoDetails( address[ii] ) 
  print( result$status )     
  result$index <- ii
  
  # Append the answer to the results file:
  geocoded <- rbind( geocoded, result )
  
  # Save the results for next time to the temp file:
  saveRDS( geocoded, tempfilename )
  
}

# Finally, add join the info above to the stop_df data frame by index:
stop_df <- join( stop_df, geocoded, by="index" )

####################################################################################################################################
## DEALING DATA THAT ALREADY HAS LOCATION COORDINATE INFORMATION:
####################################################################################################################################

# Remove the data_w_coords (put it back later!):
data_w_coords <- data %>%
  subset( str_detect( Location, "~-" ) )

# Add the appropriate columns to data_w_coords (same variables as finaldf, so that we can use rbind() to combine them together):
data_w_coords <- data_w_coords %>%
  mutate( Location = as.character(Location)) %>%
  mutate( lat = transpose(strsplit(Location,"~-"))[[1]] ) %>%
  mutate( long = transpose(strsplit(Location,"~-"))[[2]] ) %>%
  mutate( lat = str_extract( lat, "[0-9]+\\.[0-9]+" ) ) %>%
  mutate( long = str_extract( long, "[0-9]+\\.[0-9]+" ) ) %>%
  mutate( lat = as.double( lat ) ) %>%
  mutate( long = as.double( long ) ) %>%                
  mutate( Incident.Number = as.character( Incident.Number ) ) %>%    # make sure character information is of the character data type
  mutate( Call.Date.Time = as.character( Call.Date.Time ) ) %>%
  mutate( Dispositions=as.character( Dispositions ) ) %>%
  mutate( index = 
            seq( from = nrow(stop_df)+1, 
                 to = nrow(stop_df) + nrow(data_w_coords), 
                 by = 1  ) ) %>%
  mutate( Call.Date.Time = mdy_hms( Call.Date.Time ) ) %>%           # make sure that the date is of the POSIXt data type
  mutate( Year = year( Call.Date.Time ) ) %>%                        # new variable: year only (info is of 2015 and 2016)
  mutate( Date = date( Call.Date.Time ) ) %>%                        # new variable: date only
  mutate( DayOfWeek = weekdays( Call.Date.Time ) ) %>%               # to use rbind() with finaldf, we must have same # of columns.
  mutate( accuracy = NA ) %>%
  mutate( formatted_address = NA ) %>%
  mutate( address_type = NA ) %>%
  mutate( status = NA ) %>%
  mutate( long = -long )                                             # the split data forgot to put on a negative sign, so add it here.

# Put the data frames back together:
finaldf<-rbind(stop_df,data_w_coords)

####################################################################################################################################
## CHECK FOR OUTLIERS:
####################################################################################################################################

# Get longitude and latitude information for use with MAP 1:
latmax <- max( finaldf$lat, na.rm = TRUE ) 
latmin <- min( finaldf$lat, na.rm = TRUE )
lonmax <- max( finaldf$long, na.rm = TRUE ) 
lonmin <- min( finaldf$long, na.rm = TRUE )
latvals <- c( latmin, latmax )
lonvals <- c( lonmin, lonmax )

#### MAP 1: Use this one for observing outliers (if you can see the shape of California, there is something wrong):
newmap <- getMap( resolution="low" )
plot( newmap, xlim = lonvals, ylim = latvals, asp=1 )
points( finaldf$long, finaldf$lat, col="red", cex=.6 )

#### MAP 2: Use this one for an example of using color-coded factors (much prettier version, but doesn't test for outliers):
# berkMap = map = get_map(location = 'Berkeley', zoom = 14)
# finaldf$Incident.Type.Class <- factor(finaldf$Incident.Type)
# ggmap(berkMap) +
#   geom_point(aes(x = long, y = lat, colour = Incident.Type.Class), data = finaldf, alpha = 0.7)

####################################################################################################################################
## SAVE THE DATA / PLAN FOR LATER:
####################################################################################################################################

# Use this code to see how many rows left Google will allow you for today:
geocodeQueryCheck()

# WRITE finaldf to a file, if you would like to use the Location info at another time:
# write.csv( finaldf, file = tempfilename_finaldf_CSV)
# saveRDS( finaldf, tempfilename_finaldf )



