library(shiny)
library(tidyverse)
library(rsconnect)
library(leaflet)
library(shinyjs)
library(rgbif)

map <- leaflet() %>%
  addTiles()

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
          sliderInput("buffer", "Buffer Radius:", 
                      min = 1, max = 50, value = 10)
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      tableOutput("contents"),
      
      # Output: Display map ----
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  # Toggle visibility of advanced settings on button click
  observeEvent(input$toggleAdvanced, {
    toggle("advancedSettings")
  })
  
  df = reactiveVal()
  
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
    
    # Check if any data is found
    if(nrow(gbif_data$data) == 0) {
      # Show a warning message if no data is found
      showNotification("No occurrence data found for the specified species on GBIF.", type = "warning")
      return(NULL)
    }
    
    # Convert gbif_data to a dataframe and append to uploaded CSV
    uploaded_data <- df() 
    appended_data <- rbind(uploaded_data, as.data.frame(gbif_data$data))
    df(appended_data)  # Update the reactive value with appended data
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
  
  # Initialize the map when the app starts
  output$map <- renderLeaflet({
    map
  })
  
  # Add circles when the "Load Map" button is pressed
  observeEvent(input$loadMap, {
    df_temp <- df()
    if(is.null(df_temp)) return(NULL)
    
    radius <- input$buffer * 1000
    
    if(all(c("x", "y") %in% colnames(df_temp))) {
      leafletProxy("map", session, data = df_temp) %>%
        clearShapes() %>%
        addCircles(lng = ~x, lat = ~y, popup = ~paste(x, y), radius = radius)  # Convert km to meters
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
