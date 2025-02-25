###
# 
library(terra)
library(leaflet)
# lakes layer 
lakes <- terra::vect("dataToPreProcess/water/ne_10m_lakes/ne_10m_lakes.shp") 
# land layers 
land <- terra::vect("dataToPreProcess/water/ne_10m_land/ne_10m_land.shp")

# try the erase function 
t2 <- terra::erase(land, lakes) |> terra::makeValid()
leaflet()|>
  addPolygons(
    data = t2
  )

# export 
terra::writeVector(x = t2, filename = "appData/land/landNoLakes.gpkg",
                   overwrite = TRUE)
