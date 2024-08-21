gbifBackbone <- read_csv("appData/gbifBackBone.csv")
ecoRegions <- terra::vect("appData/ecoregionsSimplified.gpkg")





# Remove ------------------------------------------------------------------

# JSONdata <- fromJSON('appData/data_validation.json')
# valid_data_structure <- JSONdata[[1]]
# 
# 
# expectedNames <- c("Accession Number",	"Taxon Name",	"Current Germplasm Type",	"Collection Date",
#                    "Latitude",	"Longitude",	"Locality",	"Collector")
# tempTable <- read.csv("dataToPreProcess/Magnolia_acuminata_data.csv")|>
#   dplyr::select(-c("genus","species", "issues"))|>
#   sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
# 
# names(tempTable) <- c(expectedNames ,"geometry")
# 
# 
# 
# names(tempTable) <- expectedNames