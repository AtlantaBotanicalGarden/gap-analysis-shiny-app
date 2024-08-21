
getMaps <- function(){
  # gbif selector map -------------------------------------------------------
  gbifMap <- leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16))|>
    addTiles()|> 
    # layer control groups should not be set at the map proxy level as they will overwrite the existing element.
    addLayersControl( 
      position = "topleft",
      overlayGroups = c("GBIF", "Upload Dataset"),
      options = layersControlOptions(collapsed = TRUE)
    )
  
  
  
  
  # gap analysis map --------------------------------------------------------
  gapMap <- leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16))|>
    addTiles()|>
    addMapPane("points", zIndex = 420) |>
    addMapPane("buffers", zIndex = 410)|>
    addMapPane("gaps", zIndex = 405) |> # shown below ames_circles%>% # shown below ames_circles
    addLayersControl( 
      position = "topleft",
      overlayGroups = c("Reference Records",
                        "Germplasm Records",
                        "Buffers",
                        "GRS gaps",
                        "ERS gaps"),
      options = layersControlOptions(collapsed = FALSE))
  
  return(list(
    gbifMap = gbifMap,
    gapMap = gapMap
  ))
}

