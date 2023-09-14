library(shiny)
library(tidyverse)
library(rsconnect)
library(leaflet)
library(shinyjs)
library(rgbif)

# Define UI for data upload app ----
ui <- fluidPage(
  
  useShinyjs(),
  
  # App title ----
  titlePanel("ABG Gap Analysis Tool"),
  
  # Add custom CSS to make validation text red
  tags$style("#dataWarning { color: red; }"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", "space")),
      
      # Display warning messages ----
      verbatimTextOutput("dataWarning"),
      
      # Add button to toggle advanced settings
      actionButton("toggleAdvanced", "Show Advanced Settings"),
      
      # Collapsible panel with advanced settings
      div(id = "advancedSettings",
          style = "display: none;",  # Hidden by default
          
          # Horizontal line ----
          tags$hr(),
          
          # Input: Checkbox if file has header ----
          checkboxInput("header", "Header", TRUE),
          
          # Input: Select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t",
                                   Space = " "),
                       selected = ","),
          
          # Input: Select quotes ----
          radioButtons("quote", "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"'),
          
          # Horizontal line ----
          tags$hr(),
          
          # Input: Select number of rows to display ----
          radioButtons("disp", "Display",
                       choices = c(Head = "head",
                                   All = "all"),
                       selected = "head")
      ),
      
      # Species search input and button
      textInput("speciesName", "Enter Species Name"),
      actionButton("searchGBIF", "Search GBIF"),
      
      # Add "Load Map" and "Run Analysis" buttons with padding
      div(style = "padding-top: 20px; padding-bottom: 10px;", 
          actionButton("loadMap", "Load Map"),
          actionButton("runAnalysis", "Run Analysis", style = "background-color: #339CFF; color: white;")
      ),
      
      # Add slider input for buffer size with padding
      div(style = "padding-top: 10px; padding-bottom: 10px;", 
          sliderInput("buffer", "Buffer Radius (km):", 
                      min = 1, max = 100, value = 1)
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Use fluidRow and column to display tables side by side
      fluidRow(
        column(6,
               h3("Submitted Occurrences"),  # Title for user data
               tableOutput("contents")
        ),
        column(6,
               h3(textOutput("gbifTitle")),  # Title for GBIF data that updates based on user input
               tableOutput("gbifContents")
        )
      ),
      
      # Output: Display map ----
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  shinyjs::disable("runAnalysis")
  
  # Toggle visibility of advanced settings on button click
  observeEvent(input$toggleAdvanced, {
    toggle("advancedSettings")
  })
  
  df = reactiveVal()
  gbif_df = reactiveVal()  # For storing GBIF data
  
  observe({
    req(input$file1)
    df_data <- read.csv(input$file1$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
    df(df_data)
  })
  
  output$dataWarning <- renderText({
    df_data <- df()
    if(is.null(df_data)) return(NULL)
    
    missing_cols <- setdiff(c("x", "y", "collection_bool", "id"), colnames(df_data))
    if(length(missing_cols) > 0) {
      # Disable the other buttons
      shinyjs::disable("speciesName")
      shinyjs::disable("searchGBIF")
      shinyjs::disable("loadMap")
      shinyjs::disable("runAnalysis")
      shinyjs::disable("buffer")
      paste("WARNING | The uploaded CSV does not contain the following required columns:", paste(missing_cols, collapse = ", "))
    } else {
      # Enable the buttons
      shinyjs::enable("speciesName")
      shinyjs::enable("searchGBIF")
      shinyjs::enable("loadMap")
      shinyjs::enable("runAnalysis")
      shinyjs::enable("buffer")
      ""
    }
  })
  
  # GBIF Search and Append Data
  observeEvent(input$searchGBIF, {
    species <- input$speciesName
    
    # Fetch GBIF data
    gbif_data <- occ_search(scientificName = species, limit = 100) # limiting to 100 records for demonstration
    
    # Check if any the data list is null (search found nothing)
    if(is.null(gbif_data$data)) {
      # Show a warning message if no data is found
      showNotification("No occurrence data found for the specified species on GBIF.", type = "warning")
      return(NULL)
    }
    
    # Convert gbif list of lists into dataframe
    gbif_data_df <- do.call(rbind.data.frame, gbif_data[3])
    
    # Extracting only gbif_data with lat/longs
    sub_gbif <- subset(gbif_data_df, !is.na(gbif_data_df[,85]) & !is.na(gbif_data_df[,86]))
    
    # Converting gbif_df to gbif_gap_df
    gbif_gap_df <- sub_gbif[c("decimalLatitude", "decimalLongitude")]
    
    # Add "collection_bool" and "id" to gbif_gap_df
    gbif_gap_df <- cbind(gbif_gap_df, collection_bool=FALSE, id=NA)
    
    # Change decimalLatitude and decimalLongitude
    names(gbif_gap_df)[names(gbif_gap_df) == 'decimalLatitude'] <- 'y'
    names(gbif_gap_df)[names(gbif_gap_df) == 'decimalLongitude'] <- 'x'
    
    # Check if all coordinate data
    if(nrow(gbif_gap_df) == 0) {
      showNotification("Occurrence data found for the specified species on GBIF lacking coordinate data.", type = "warning")
      return(NULL)
    }
    
    gbif_df(gbif_gap_df)  # Store the GBIF data in the reactive value
  })
  
  output$contents <- renderTable({
    df_data <- df()
    if(is.null(df_data)) return(NULL)
    
    if(input$disp == "head") {
      return(head(df_data))
    } else {
      return(df_data)
    }
  }, max.cols = NULL)  # Display all columns
  
  output$gbifContents <- renderTable({
    gbif_data <- gbif_df()
    if(is.null(gbif_data)) return(NULL)
    
    if(input$disp == "head") {
      return(head(gbif_data))
    } else {
      return(gbif_data)
    }
  }, max.cols = NULL)  # Display all columns
  
  # Create a reactive title for the GBIF table
  output$gbifTitle <- renderText({
    if(!is.null(input$speciesName) && input$speciesName != "") {
      paste("GBIF Occurrences for", input$speciesName)
    } else {
      "GBIF Occurrences"
    }
  })
  
  # Initialize the map when the app starts
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
  })
  
  # Add circles when the "Run Analysis" button is pressed
  observeEvent(input$loadMap, {
    df_temp <- df()
    gbif_data_temp <- gbif_df()
    radius <- input$buffer * 1000
    
    map <- leafletProxy("map", session, data = df_temp)
    clearShapes(map)
    
    if(!is.null(df_temp) && all(c("x", "y") %in% colnames(df_temp))) {
      map <- addCircles(map, lng = df_temp$x, lat = df_temp$y, popup = ~paste(df_temp$x, df_temp$y), radius = radius, color = "blue")
    }
    
    if(!is.null(gbif_data_temp) && all(c("x", "y") %in% colnames(gbif_data_temp))) {
      map <- addCircles(map, lng = gbif_data_temp$x, lat = gbif_data_temp$y, popup = ~paste(gbif_data_temp$x, gbif_data_temp$y), radius = radius, color = "red")
    }
  })
  
  # Update circles when the buffer size changes
  observeEvent(input$buffer, {
    df_temp <- df()
    gbif_data_temp <- gbif_df()
    radius <- input$buffer * 1000
    
    map <- leafletProxy("map", session, data = df_temp)
    clearShapes(map)
    
    if(!is.null(df_temp) && all(c("x", "y") %in% colnames(df_temp))) {
      map <- addCircles(map, lng = df_temp$x, lat = df_temp$y, popup = ~paste(df_temp$x, df_temp$y), radius = radius, color = "blue")
    }
    
    if(!is.null(gbif_data_temp) && all(c("x", "y") %in% colnames(gbif_data_temp))) {
      map <- addCircles(map, lng = gbif_data_temp$x, lat = gbif_data_temp$y, popup = ~paste(gbif_data_temp$x, gbif_data_temp$y), radius = radius, color = "red")
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
