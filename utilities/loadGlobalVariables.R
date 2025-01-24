
# gbid data
gbifBackbone <- read_csv("appData/gbifBackBone.csv")
genera <- unique(gbifBackbone$genus)

# eco regions 
ecoRegions <- terra::vect("appData/ecoregionsSimplified.gpkg")

# names for the datasets 
expectedNames <- c("Accession Number",	"Taxon Name",	"Current Germplasm Type",	"Collection Date",
                   "Latitude",	"Longitude",	"Locality",	"Collector")
tempTable <- read.csv("dataToPreProcess/Magnolia_acuminata_data.csv")|>
  dplyr::select(-c("genus","species", "issues"))|>
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

names(tempTable) <- c(expectedNames ,"geometry")

# load in land mask 
land <- terra::vect("appData/water/land.gpkg")

# Color Palettes --- H , G 
uploadColor <-  c("#dfc27d", "#a6611a")
gbifColor <- c("#80cdc1", "#018571")
combinedColor <- c("#f1a340","#542788") 

# define shapes for legend elements 
## from https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
make_shapes <- function(colors, sizes, borders, shapes) {
  shapes <- gsub("circle", "50%", shapes)
  shapes <- gsub("square", "0%", shapes)
  paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
}
  
gbif_legend_colors <- make_shapes(gbifColor, sizes = 20, borders = "white", shapes = "circle")
upload_legend_colors <- make_shapes(uploadColor, sizes = 20, borders = "white", shapes = "circle")

  

