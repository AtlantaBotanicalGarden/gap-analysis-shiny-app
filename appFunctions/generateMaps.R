

generateMap1 <- function(){
  # intial map for page one 
  map1 <-
    leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16)) |>
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
    addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
    addProviderTiles("Esri.WorldImagery", group = "Imagery") |>  # layer control groups should not be set at the map proxy level as they will overwrite the existing element.
    addLayersControl(
      position = "topleft",
      overlayGroups = c("GBIF", "Upload Dataset"),
      baseGroups = c("OpenStreetMap",
                     "Topography",
                     "Imagery"),
      options = layersControlOptions(collapsed = FALSE)
    )|>
    addDrawToolbar(
      position = "topleft",
      polylineOptions = FALSE,
      circleOptions = FALSE,
      rectangleOptions = FALSE,
      markerOptions = FALSE,
    )
  return(map1)
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