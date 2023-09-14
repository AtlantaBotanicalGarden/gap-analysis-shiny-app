library(rgbif)

## Perfect example of a Positive match with no coordinate data ##
#species <- "Pseudotorellia crista"

## Perfect example of a Positive match with SOME (but not all) lat/long matches
species <- "Pseudotorellia Florin"

# Fetch GBIF data
# limiting to 100 records for demonstration
gbif_data <- occ_search(scientificName = species, limit = 10000, occurrenceStatus = "PRESENT") # make limit user defined with warnings

# Convert gbif list of lists into dataframe
gbif_df <- do.call(rbind.data.frame, gbif_data[3])

# Extracting only gbif_data with lat/longs
sub_gbif <- subset(gbif_df, !is.na(gbif_df[,85]) & !is.na(gbif_df[,86]))
###

# Converting gbif_df to gbif_gap_df
gbif_gap_df <- sub_gbif[c("decimalLatitude", "decimalLongitude")]

# Add "collection_bool" and "id" to gbif_gap_df
gbif_gap_df <- cbind(gbif_gap_df, collection_bool=FALSE, id=NA)

# Change decimalLatitude and decimalLongitude
names(gbif_gap_df)[names(gbif_gap_df) == 'decimalLatitude'] <- 'y'
names(gbif_gap_df)[names(gbif_gap_df) == 'decimalLongitude'] <- 'x'

# Check if any data is found
# Check if any the data list is null (search found nothing)
if(is.null(gbif_data$data)) {
  # Show a warning message if no data is found
  #print("No occurrence data found for the specified species on GBIF.")
  showNotification("No occurrence data found for the specified species on GBIF.", type = "warning")
  return(NULL)
}

# Check if all coordinate data
if(nrow(gbif_gap_df) == 0) {
  showNotification("Occurrence data found for the specified species on GBIF lacking coordinate data.", type = "warning")
}


# Convert gbif_data to a dataframe and append to uploaded CSV
uploaded_data <- df() 
appended_data <- rbind(uploaded_data, as.data.frame(gbif_data$data))

# Update the reactive value with appended data
df(appended_data)