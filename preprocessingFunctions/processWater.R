pacman::p_load(sf, rmapshaper, tmap)
tmap_mode("view")

# read in, simplfy and export 

w1 <- sf::st_read("dataToPreProcess/water/ne_10m_lakes/ne_10m_lakes.shp")|>
  sf::st_make_valid() 

# simplfy features looked too extreme... 
qtm(w1)

sf::st_write(obj = w1, "appData/water/lakes.gpkg")

# coastlines 
w2 <- sf::st_read("dataToPreProcess/water/ne_10m_land/ne_10m_land.shp")

# simplfy features looked too extreme... 
qtm(w2)
sf::st_write(obj = w2, "appData/water/land.gpkg")
