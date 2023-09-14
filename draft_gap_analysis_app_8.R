library(shiny)
library(tidyverse)
library(rsconnect)
library(leaflet)
library(shinyjs)

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
      
      # Add "Run Analysis" button with padding
      div(style = "padding-top: 20px; padding-bottom: 10px;", 
          actionButton("loadMap", "Load Map")
      ),
      
      # Add slider input for buffer size with padding
      div(style = "padding-top: 10px; padding-bottom: 10px;", 
          sliderInput("buffer", "Buffer Radius:", 
                      min = 1, max = 50, value = 10)
      ),
      
      # Add "Run Analysis" button with padding
      div(style = "padding-top: 10px; padding-bottom: 10px;", 
          actionButton("runAnalysis", "Run Analysis", style = "background-color: #007BFF; color: white;")
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
  
  # Disable the "Run Analysis" button initially
  # 'shinyjs::enable("runAnalysis")' to be activated with a certain condition in future
  shinyjs::disable("runAnalysis")
  
  # Toggle visibility of advanced settings on button click
  observeEvent(input$toggleAdvanced, {
    toggle("advancedSettings")
  })
  
  df = reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  output$dataWarning <- renderText({
    df_data <- df()
    missing_cols <- setdiff(c("x", "y", "collected", "identifier"), colnames(df_data))
    
    if(length(missing_cols) > 0) {
      paste("WARNING | The uploaded CSV does not contain the following required columns:", paste(missing_cols, collapse = ", "))
    } else {
      ""
    }
  })
  
  output$contents <- renderTable({
    df_data <- df()
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
  
  # Add circles when the "Run Analysis" button is pressed
  observeEvent(input$loadMap, {
    df_temp <- df()
    radius <- input$buffer * 1000
    
    if(all(c("x", "y") %in% colnames(df_temp))) {
      leafletProxy("map", session, data = df_temp) %>%
        clearShapes() %>%
        addCircles(lng = ~x, lat = ~y, popup = ~paste(x, y), radius = radius)  # Convert km to meters
    }
  })
  
  # Update circles when the buffer size changes
  observeEvent(input$buffer, {
    radius <- input$buffer * 1000
    df_temp <- df()
    
    if(all(c("x", "y") %in% colnames(df_temp))) {
      leafletProxy("map", session, data = df_temp) %>%
        clearShapes() %>%
        addCircles(lng = ~x, lat = ~y, popup = ~paste(x, y), radius = radius)  # Convert km to meters
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
