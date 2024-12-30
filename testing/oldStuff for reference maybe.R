



### current working version
# generate a reactive spatail data object
# points <- reactiveVal(NULL)
# 
# 
# ## This is for table upload 
# observeEvent(input$mapTableUpload, {
#   # generate spatial data 
#   tempPoints1 <-createSpatialObject(input$mapTableUpload) |>
#     dplyr::mutate(
#       color = case_when(
#         `Current Germplasm Type` == "H" ~ uploadColor[1],
#         `Current Germplasm Type` == "G" ~ uploadColor[2]
#       )
#     )
#   points(tempPoints1)
# })
# 
# # updata point object base on button click 
# observeEvent(input$updateCombinedTable, {
#   uploadData <- points()
#   if(!is.null(uploadData)){
#     # generate spatial data 
#     tempPoints1 <-createSpatialObject(input$mapTableUpload) |>
#       dplyr::mutate(
#         color = case_when(
#           `Current Germplasm Type` == "H" ~ uploadColor[1],
#           `Current Germplasm Type` == "G" ~ uploadColor[2]
#         )
#       )
#     points(tempPoints1)
#   }
# })
# 
# 
# # Observe changes to the spatial object and update the map
# observe({
#   uploadPoints <- points()
#   if (!is.null(uploadPoints)) {
#     # ideally this would be within the create SpatialObject call. 
#     labels <- lapply(uploadPoints$popup, htmltools::HTML)
#     # produce map
#     leafletProxy("map1")|>
#       setView(lng = mean(uploadPoints$Longitude), lat = mean(uploadPoints$Latitude), zoom = 6)|>
#       addCircleMarkers(
#         data = uploadPoints, 
#         layerId = ~`Accession Number`,
#         group = "Upload",
#         radius = 4,
#         color = "white",
#         fillColor = ~color,
#         stroke = TRUE,
#         weight = 1,
#         fillOpacity = 1,
#         label = labels) 
#   }
# })
# 
# # Reactive values to store selected markers
# selectedUpload <- reactiveValues(markers = NULL)
# 
# # INDIVIDUAL POINT SELECTION
# # Observe marker clicks
# observeEvent(input$map1_marker_click, {
#   click <- input$map1_marker_click
#   
#   print(click$id)
#   print(click$group)
#   # define local varaible 
#   uploadPoints <- points()
#   if(click$group == "Upload"){
#     # add it to selected markers
#     selectedUpload$markers <- c(selectedUpload$markers, click$id)
#     # edit map  
#     leafletProxy("map1") %>%
#         addCircleMarkers(
#           data = uploadPoints |>
#             dplyr::filter(`Accession Number` == click$id) ,
#           layerId = ~`Accession Number`,
#           radius = 4,
#           color = "red",
#           fillOpacity = 0.8,
#           stroke = FALSE,
#           group = "Upload Selection"
#         )
#   }
#   if(click$group == "Upload Selection"){
#     # remove the item from selection
#     selectedUpload$markers <- selectedUpload$markers[selectedUpload$markers != click$id]
#     # update the map 
#     leafletProxy("map1") %>%
#       leaflet::clearGroup("Upload Selection") |>
#       addCircleMarkers(
#         data = uploadPoints[uploadPoints$`Accession Number` %in% selectedUpload$markers, ],
#         layerId = ~`Accession Number`,
#         radius = 4,
#         color = "red",
#         fillOpacity = 0.8,
#         stroke = FALSE,
#         group = "Upload Selection"
#       )
#   }
# })
