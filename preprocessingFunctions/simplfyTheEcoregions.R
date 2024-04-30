pacman::p_load(rmapshaper, sf, tmap)
tmap_mode("view")
# read in feature 
sp1 <- sf::st_read("dataToPreProcess/official/wwf_terr_ecos.shp")
# select specific counties
admin <- st_as_sf(rnaturalearth::countries110)|>
  dplyr::filter(GU_A3 %in% c("CAN","MEX","USA"))

# filter to objects within those countries 


object.size(sp1)
qtm(head(sp1))
# resample
r1 <- rmapshaper::ms_simplify(sp1,keep_shapes=TRUE)
object.size(r1)
qtm(head(r1))

#export 
st_write(obj = r1, "appData/ecoregionsSimplified.gpkg", delete_layer = TRUE)
