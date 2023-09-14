library(rgbif)

## Perfect example of a Positive match with no coordinate data ##
#species <- "Pseudotorellia crista"

## Perfect example of a Positive match with SOME (but not all) lat/long matches
species <- "Pseudotorellia Florin"

# Fetch GBIF data
# limiting to 100 records for demonstration
gbif_data <- occ_search(scientificName = species, limit = 10000) # make limit user defined with warnings

# Convert gbif list of lists into dataframe
gbif_df <- do.call(rbind.data.frame, gbif_data[3])

# Extracting only gbif_data with lat/longs
sub_gbif <- subset(gbif_df, !is.na(gbif_df[,85]) & !is.na(gbif_df[,86]))
###

# Check if any data is found
if(nrow(gbif_data$data) == 0) {
  # Show a warning message if no data is found
  showNotification("No occurrence data found for the specified species on GBIF.", type = "warning")
  return(NULL)
}

# Convert gbif_data to a dataframe and append to uploaded CSV
uploaded_data <- df() 
appended_data <- rbind(uploaded_data, as.data.frame(gbif_data$data))

# Update the reactive value with appended data
df(appended_data)