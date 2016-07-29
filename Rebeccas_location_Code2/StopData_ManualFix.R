#############################################################################################################################
## FIXING OTHER PROBLEMS:
#############################################################################################################################

#### IMPORTANT: ONLY RUN THIS CODE IF YOU ARE REBECCA: THIS IS FOR MANUAL FIXES! LET REBECCA FIX AT THE END.


## Only run this if you know what exactly to change by hand:
finaldf[ which( finaldf$lat == latmin ),]


## LATITUDE OUTLIER example:
# row name of the point in question:
rn <- which( finaldf$lat == latmin )
rn
# text to replace:
error_text <- "CIVIC CENTER PARK N SIDE , Berkeley, CA, USA"
error_text
# replacement text:
new_text <- "2151 MLK Jr Way, Berkeley, CA 94704"
new_text
v <- str_replace( finaldf$Location[rn], error_text, new_text)
v
result = getGeoDetails( v )   
result
result$index <- rn
result
geocoded[rn, ] <- result
saveRDS( geocoded, tempfilename )

##################################################################################################

## LONGITUDE OUTLIER example:
# row name of the point in question:
rn <- which( finaldf$lat == latmin )
rn
# text to replace:
error_text <- "CIVIC CENTER PARK N SIDE , Berkeley, CA, USA"
# replacement text:
new_text <- "2151 MLK Jr Way, Berkeley, CA 94704"
finaldf[ which( finaldf$long == lonmin ),]

v <- str_replace( finaldf$Location[rn], error_text, new_text  )
v
result = getGeoDetails( v )   
result$index <- rn
result
rownames(result) <- rn
result
geocoded[rn, ]
geocoded[rn, ] <- result
saveRDS( geocoded, tempfilename )
