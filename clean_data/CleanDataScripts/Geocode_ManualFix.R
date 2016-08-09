# Geocode_ManualFix.R

# Author: Rebecca Reus

# Purpose: 
# 1. Use this file to manually change latitude and longitude errors by hand.
# 2. Do NOT run this file unless you know EXACTLY which row you want to change, 
#    since you must manually enter the row you want to change, and this file 
#    updates that row in our temp file (the .rds file in the raw data folder).

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

file_tempGeocoded <- '../../raw_data/StopData_temp_geocoded.rds' # contains geolocation information.
file_finaldf <- '../../raw_data/StopData_finaldf.rds' # contains the stop data with geolocation information.

# Read in the data:
geocoded <- readRDS( file_tempGeocoded )
finaldf <- readRDS( file_finaldf )


####################################################################################################################################
## CREATE SOME FUNCTIONS:
####################################################################################################################################

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
  
####################################################################################################################################
## READ THE DATA IN:
####################################################################################################################################


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
## Look at the text in the row with outlier longitude:
row_lonmax <- finaldf[ which( finaldf$long == lonmax ), ]
rn_lonmax <- which( finaldf$long == lonmax )
row_lonmin <-finaldf[ which( finaldf$long == lonmin ), ]
rn_lonmin <- which( finaldf$long == lonmin )
## Look at the text in the row with outlier latitude:
row_latmax <- finaldf[ which( finaldf$lat == latmax ), ]
rn_latmax <- which( finaldf$lat == latmax )
row_latmin <-finaldf[ which( finaldf$lat == latmin ), ]
rn_latmin <- which( finaldf$lat == latmin )
#### MAP 1: Use this one for observing outliers (if you can see the shape of California, there is something wrong):
newmap <- getMap( resolution="low" )
plot( newmap, xlim = lonvals, ylim = latvals, asp = 1 )
points( finaldf$long, finaldf$lat, col = "red", cex = .6 )
# outliers are:
select(row_latmax, formatted_address )
select(row_latmin, formatted_address )
select(row_lonmax, formatted_address )
select(row_lonmin, formatted_address )

##################################################################################################
## EXAMINE THE DATA VISUALLY AND DECIDE WHICH OUTLIER TO LOOK AT:
##################################################################################################
# set rn to the row name in question:
rn<- rn_latmin
rn
finaldf[ rn, ]

# set the thing to change:
new_text <- "MLK Jr Way and 36th St, Berkeley, CA 94710"
new_text

# change stuff:
finaldf$Location[ rn ] <- new_text
finaldf$Location[ rn ]
result = getGeoDetails( finaldf$Location[ rn ] )   
result$index <- rn
rownames(result) <- rn
geocoded[ rn, ] <- result
finaldf[ rn, 'lat' ] <- geocoded[ rn, 'lat' ]
finaldf[ rn, 'long' ] <- geocoded[ rn, 'long' ]
finaldf[ rn, 'accuracy' ] <- geocoded[ rn, 'accuracy' ]
finaldf[ rn, 'formatted_address' ] <- geocoded[ rn, 'formatted_address' ]
finaldf[ rn, 'address_type' ] <- geocoded[ rn, 'address_type' ]
finaldf[ rn, 'status' ] <- geocoded[ rn, 'status' ]
print(  "finaldf[ rn, ]: ") 
print( finaldf[ rn, ] )

# Save all:
# saveRDS( finaldf, file_finaldf )
# saveRDS( geocoded, file_tempGeocoded )

##################################################################################################
## OTHER PROBLEMS:
##################################################################################################

# NA locations:
finaldf_na <- finaldf[ which(is.na(finaldf$lat)), ]
select( finaldf_na, Location, formatted_address)

# select one of the na latitude rows:
rn <- finaldf_na$index[1]
rn 
finaldf[ rn, ]

# set the thing to change:
new_text <- "3100 Adeline St, Berkeley, CA 94703"
new_text

# change stuff:
finaldf$Location[ rn ] <- new_text
finaldf$Location[ rn ]
result = getGeoDetails( finaldf$Location[ rn ] )   
result$index <- rn
rownames(result) <- rn
geocoded[ rn, ] <- result
finaldf[ rn, 'lat' ] <- geocoded[ rn, 'lat' ]
finaldf[ rn, 'long' ] <- geocoded[ rn, 'long' ]
finaldf[ rn, 'accuracy' ] <- geocoded[ rn, 'accuracy' ]
finaldf[ rn, 'formatted_address' ] <- geocoded[ rn, 'formatted_address' ]
finaldf[ rn, 'address_type' ] <- geocoded[ rn, 'address_type' ]
finaldf[ rn, 'status' ] <- geocoded[ rn, 'status' ]
print(  "finaldf[ rn, ]: ") 
print( finaldf[ rn, ] )

# Save all:
# saveRDS( finaldf, file_finaldf )
# saveRDS( geocoded, file_tempGeocoded )
