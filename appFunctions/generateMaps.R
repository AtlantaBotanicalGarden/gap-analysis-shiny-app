
# initial map layout for the application 
generateMap1 <- function(){
  # intial map for page one 
  map1 <-
    leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16)) |>
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
    addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
    addProviderTiles("Esri.WorldImagery", group = "Imagery") |>  # layer control groups should not be set at the map proxy level as they will overwrite the existing element.
    addLayersControl(
      position = "topleft",
      overlayGroups = c("records", "Selection"),
      baseGroups = c("OpenStreetMap",
                     "Topography",
                     "Imagery"),
      options = layersControlOptions(collapsed = FALSE)
    )|>
    addDrawToolbar(
      singleFeature = TRUE,
      polylineOptions = FALSE,
      polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'red')),
      rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(color = 'red')),
      circleOptions = FALSE,
      markerOptions = FALSE,
      editOptions = editToolbarOptions(edit = FALSE)
    )
  return(map1)
}

createSpatialObject <- function(data){
  # # define icons 
  # gGbif <- icons(iconUrl = "www/purpleSquare.png", iconWidth = 14,iconHeight = 14)
  # gUp <-  icons(iconUrl = "www/purpleTriangle.png", iconWidth = 14,iconHeight = 14)
  # hGbif <- icons(iconUrl = "www/blueSquare.png",iconWidth = 14,iconHeight = 14)
  # hUp <- icons(iconUrl = "www/blueTriangle.png", iconWidth = 14,iconHeight = 14)
  # remove any empty lat lon values 
  data <- data |>
    dplyr::mutate(
      icon = case_when(
        `Current Germplasm Type` == "G" & source == "GBIF" ~ "www/purpleSquare.png",
        `Current Germplasm Type` == "G" & source == "upload" ~ "www/purpleTriangle.png",
        `Current Germplasm Type` == "H" & source == "GBIF" ~ "www/blueSquare.png",
        `Current Germplasm Type` == "H" & source == "upload" ~ "www/blueTriangle.png" 
      )
    )
  
  # generate spatial object
  points <- data |>
    sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
    dplyr::mutate(
      index = dplyr::row_number(),
      popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
                     "<br/><strong> Type: </strong>", `Current Germplasm Type`,
                     "<br/><b>Collector Name:</b> ", Collector,
                     "<br/><b>Locality Description):</b> ", Locality),
      color = case_when(
        `Current Germplasm Type` == "H" ~ "#1184d4",
        `Current Germplasm Type` == "G" ~ "#6300f0"
      )
    )
  return(points)
}

generateMap2 <- function(){
  # inital map for gap analysis page
  map2 <- leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16))|>
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
    addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
    addProviderTiles("Esri.WorldImagery", group = "Imagery") |>
    addMapPane("points", zIndex = 215) |>
    addMapPane("buffers", zIndex = 210)|>
    addMapPane("gaps", zIndex = 205) |> 
    addMapPane("ecoregions", zIndex = 205) |> 
    addLayersControl( 
      position = "topleft",
      overlayGroups = c("Reference Records",
                        "Germplasm Records",
                        "Buffers",
                        "GRS gaps",
                        "ERS gaps"),
      baseGroups = c(
        "OpenStreetMap",
        "Topography",
        "Imagery"
      ),
      options = layersControlOptions(collapsed = FALSE))
  return(map2)
}