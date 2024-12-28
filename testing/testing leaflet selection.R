library(shiny)
library(leaflet)



data <- read_csv("temp/Upload Example.csv", )
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


library(shiny)
library(leaflet)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                h2("Selected Markers"),
                verbatimTextOutput("selected_markers"),
                h2("Click Information"),
                verbatimTextOutput("click_info")
  )
)

server <- function(input, output, session) {
  
  # Sample data (replace with your own)
  df <- data.frame(
    lat = c(37.7749, 34.0522, 40.7128),
    lng = c(-122.4194, -118.2437, -74.0060),
    city = c("San Francisco", "Los Angeles", "New York City")
  )
  
  # Reactive values to store selected markers
  selected <- reactiveValues(markers = NULL)
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    leaflet(points) %>%
      addTiles() %>%
      addCircleMarkers(
        layerId = ~`Accession Number`,
        radius = 8,
        color = "blue",
        fillOpacity = 0.6,
        stroke = FALSE,
        group = "upload"
      )
  })
  
  # Observe marker clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    # If marker is already selected, deselect it
    if(click$id %in% selected$markers) {
      selected$markers <- selected$markers[selected$markers != click$id]
      leafletProxy("map") %>%
        leaflet::clearGroup("selection") |> 
        addCircleMarkers(
          data = points[points$`Accession Number` %in% selected$markers, ],
          layerId = ~`Accession Number`,
          radius = 8,
          color = "red", 
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "selection"
        )
    } else {
      # Otherwise, add it to selected markers
      selected$markers <- c(selected$markers, click$id)
      leafletProxy("map") %>%
        addCircleMarkers(
          data = points[points$`Accession Number` == click$id, ],
          layerId = ~`Accession Number`,
          radius = 8,
          color = "red", 
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "selection"
        )
    }
    
    # # Update the click_info output
    output$click_info <- renderPrint({
      click
    })
  })
  
  # Display selected markers
  output$selected_markers <- renderPrint({
    selected$markers
  })
}

shinyApp(ui, server)