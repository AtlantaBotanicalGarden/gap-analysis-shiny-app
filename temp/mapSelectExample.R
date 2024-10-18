library(leaflet.extras)
library(shiny)
library(jsonlite)
library(geojsonio)
library(geojsonsf)
library(sf)
library(rhandsontable)

dat = data.frame(a = 1:50, 
                 b = rnorm(50, 2, 1), 
                 x = (runif(50, 113, 116)) * -1,
                 y = runif(50, 42, 45))

datsf = st_as_sf(dat, coords = c("x", "y"), crs = 4326)

dat2 = data.frame(a = 1:50, 
                  b = rnorm(50, 2, 1), 
                  x = (runif(50, 113, 116)) * -1,
                  y = runif(50, 42, 45))

datsf2 = st_as_sf(dat, coords = c("x", "y"), crs = 4326)



ui <- fluidPage(
  actionButton("delete","Remove the Draw Toolbar"),
  actionButton("deleteandclear","Remove the Draw Toolbar and cleanFeatures=T"),
  leafletOutput("leafmap")
)

server <- function(input, output, session) {
  
  
  selectionPolygon <- reactiveVal()
  
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = datsf) |> 
      leaflet.extras::addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        rectangleOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions())
      # addDrawToolbar(
      #   singleFeature = TRUE,
      #   toolbar = toolbarOptions(
      #     actions = list(title = "Cancel", text = "Cancel"),
      #     finish = list(title = "Done", text = "Done"),
      #     undo = list(title = "Delete last vertex", text = "Undo"),
      #     buttons = list(polygon      = "Select by polygon",
      #                    rectangle    = "Select by rectangle",
      #                    circle       = "Select by circle")
      #   ),
      #   handlers =  handlersOptions(
      #     polygon = list(tooltipStart  = "Start drawing.  Click first point to complete shape")
      #   ),
        # editOptions = editToolbarOptions(),
        # polylineOptions = F, rectangleOptions = T, circleOptions = T,
        # polygonOptions = T, markerOptions = F, circleMarkerOptions = F)
  })
  
  
  observeEvent(input$delete, {
    leafletProxy("leafmap") %>%
      removeDrawToolbar(clearFeatures = FALSE)
  })
  
  observeEvent(input$deleteandclear, {
    leafletProxy("leafmap") %>%
      removeDrawToolbar(clearFeatures = TRUE)
  })
  
  
  observeEvent(input$leafmap_draw_all_features, {
    
    # Raw data from leaflet map (a geogjson in an R-style list, apparently)
    selectionPoly <- input$leafmap_draw_all_features
    selectionPoly <- jsonlite::toJSON(selectionPoly, auto_unbox = TRUE) # convert to json
    selectionPoly <- geojsonsf::geojson_sf(selectionPoly) # conver to sf object
    print(input$leafmap$features)
    # Save polygon to previously defined reactive value.
    selectionPolygon(selectionPoly)
    
    # Find points within the polygon (Intersection).
    pointsWithinPolygon <- st_filter(selectionPoly, datsf)
    print(pointsWithinPolygon)
    
    
  })
}

shinyApp(ui, server)

