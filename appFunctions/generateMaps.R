
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
      overlayGroups = c("Upload",
                        "Upload Selection",
                        "GBIF",
                        "GBIF Selection"),
      baseGroups = c("OpenStreetMap",
                     "Topography",
                     "Imagery"),
      options = layersControlOptions(collapsed = FALSE)
    )

  return(map1)
}

createSpatialObject <- function(table){
  # convert from rhandson to r 
  # remove any empty rows 
  # data <- hot_to_r(table) |>
  #   dplyr::filter(!is.na(Longitude))|>
  #   dplyr::filter(!is.na(Latitude))
  
  # testing 
  data <- table |>
    dplyr::filter(!is.na(Longitude))|>
    dplyr::filter(!is.na(Latitude))
  
  
  # generate spatial object
  points <- data |>
    sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
    dplyr::mutate(
      index = dplyr::row_number(),
      popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
                     "<br/><strong> Type: </strong>", `Current Germplasm Type`,
                     "<br/><b>Collector Name:</b> ", Collector,
                     "<br/><b>Locality Description):</b> ", Locality)
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



# Logic for applying all user-selected filters

indicatorData_active <- reactive({
  filtered_data <- indicatorData_raw()
  filtered_data
  
  # Point Type (targeted or random)
  if (!is.null(input$pointType_filter)) {
    filtered_data <- filtered_data %>% filter(PointSelectionType %in% input$pointType_filter)
  }
  
  filtered_data
})







