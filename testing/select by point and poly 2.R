library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)

# Sample data: Randomly generate some points
set.seed(123)
points <- data.frame(
  tempID = paste("temp-", 1:1000, sep = ""),
  EvaluationID = paste("point-", 1:1000, sep = ""),
  selectionID = NA,# Add an ID column to uniquely identify each point
  lng = runif(1000, 93.65, 94.55) *-1,
  lat = runif(1000, 41.00, 43.10)
)

pointssf <- points %>% st_as_sf(coords = c("lng", "lat"), crs = 4326)

ui <- fluidPage(
  titlePanel("Interactive Point Selection"),
  leafletOutput("map", width = "100%", height = "600px"),
  actionButton("reset", "Reset Selection"),
  verbatimTextOutput("selected_points")
)

server <- function(input, output, session) {
  # Initialize selected points with the same structure as `points`
  selected_points <- reactiveVal(pointssf[0, ])
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(pointssf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        layerId = ~EvaluationID,
        group = "allPts",
        color = "blue", radius = 5, fillOpacity = 0.7
      ) %>%
      addDrawToolbar(
        singleFeature = TRUE,
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'red')),
        rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(color = 'red')),
        circleOptions = FALSE,
        markerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE)
      )
  })
  
  # Function to update the color of selected points
  update_selected_points <- function(data) {
    leafletProxy("map") %>%
      clearGroup("selectedPts") %>%
      addCircleMarkers(
        data = data,
        group = "selectedPts",  
        layerId = ~selectionID,
        color = "red",
        radius = 5,
        fillOpacity = 0.7
      )
  }

  # # Event handler for clicks on points
  # observeEvent(input$map_marker_click, {
  #   clickID <- input$map_marker_click$id
  #   clickGroup <- input$map_marker_click$group
  #   
  #   selected <- selected_points()
  #   
  #   if (clickGroup == "allPts") {
  #     clickEvalID <- clickID
  #   } else if (clickGroup == "selectedPts") {
  #     clickEvalID <- selected %>% filter(selectionID == clickID) %>% pull(EvaluationID)
  #   }
  # 
  #   # Check if the point is already selected
  #   if (clickEvalID %in% selected$EvaluationID) {
  #     # Remove the point from selected_points
  #     new_selection <- selected %>% filter(EvaluationID != clickEvalID)
  #   } else {
  #     # Add the point to selected_points
  #     new_selection <- bind_rows(selected, pointssf %>% filter(EvaluationID == clickEvalID))
  #   }
  # 
  #   # Make new selectionID.  This is required because you cant have duplicate layerIDs.
  #   new_selection$selectionID <- seq_len(nrow(new_selection))
  #   
  #   selected_points(new_selection) 
  #   
  #   print(selected_points())
  #   
  #   update_selected_points(selected_points())  # Update the map to reflect the color change
  # })

  # Event handler for polygon selection
  observeEvent(input$map_draw_new_feature, {
    selected <- selected_points()
    
    # Extract polygon feature 
    feature <- input$map_draw_new_feature
    coords <- feature$geometry$coordinates[[1]]
    selectionPoly <- st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))

    # Find points within the polygon
    pointsInPoly <- st_filter(pointssf, selectionPoly)

    # Identify only the new points.  Do not include points already selected.
    new_points <- pointsInPoly %>% filter(!(EvaluationID %in% selected$EvaluationID))

    # Combined previous and new selection
    new_selection <- bind_rows(selected, pointssf %>% filter(EvaluationID %in% new_points$EvaluationID))
    
    # Make new selectionID.  This is required because you cant have duplicate layerIDs.
    new_selection$selectionID <- seq_len(nrow(new_selection))
    
    selected_points(new_selection)
    print(selected_points())
    
    update_selected_points(selected_points())
    
    # Optional workaround to immediately remove the drawn feature once completed.  Toolbar may quickly flash.
    leafletProxy('map')%>%
      removeDrawToolbar(clearFeatures = TRUE) %>%
      addDrawToolbar(
      polylineOptions = FALSE,
      polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'red')),
      rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(color = 'red')),
      circleOptions = FALSE,
      markerOptions = FALSE,
      editOptions = editToolbarOptions(edit = FALSE)
    )
      
  })

  # Display selected points
  output$selected_points <- renderPrint({
    selected_points()
  })

  # Reset button to clear selections and reset colors
  observeEvent(input$reset, {
    selected_points(pointssf[0, ])  # Reset to empty data frame with correct structure
    update_selected_points(selected_points())  # Update the map to reflect the reset
  })
}

shinyApp(ui, server)
