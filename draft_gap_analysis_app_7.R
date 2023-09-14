library(shiny)
library(tidyverse)
library(rsconnect)
library(leaflet)

map <- leaflet() %>%
  addTiles()

# Define UI for data upload app ----
ui <- fluidPage(
  
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
                   selected = "head"),
      
      # Add "Run Analysis" button ----
      actionButton("runAnalysis", "Run Analysis"),
      
      # Add slider input for buffer size ----
      sliderInput("buffer", "Buffer Radius:", 
                  min = 1, max = 50, value = 10)
      
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
  observeEvent(input$runAnalysis, {
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
