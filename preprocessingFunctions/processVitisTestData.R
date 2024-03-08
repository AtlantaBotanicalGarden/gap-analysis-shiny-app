library(tidyr)
library(dplyr)
library(sf)

## require attribute data 
names <- c("UID", "taxon", "genus",  "species", "type", "year", "latitude",
           "longitude", "locality", "collector")

data <-  sf::st_read("dataToPreProcess/spatialData.gpkg")|>
  tidyr::extract(geom, c('longitude','latitude', ), '\\((.*), (.*)\\)', convert = TRUE) 
  dplyr::select(
    UID = "sourceUniqueID",
    taxon,
    genus,
    species,
    type,
    year = "yearRecorded",
    latitude,
    longitude,
    locality,
    collector
  )
  
