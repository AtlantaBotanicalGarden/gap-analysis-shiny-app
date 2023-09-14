library(shiny)
library(tidyverse)
library(rsconnect)
library(leaflet) # added leaflet for map visualization

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("ABG Gap Analysis Tool"),
  
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
      leafletOutput("map") # added this output for map
      
    )
    
  )
)

server <- function(input, output) {
  
  df = reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    return(df)
  })
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  # Updated code to display map with buffer in kilometers
  output$map <- renderLeaflet({
    req(input$runAnalysis) # Ensure "Run Analysis" button is clicked
    df_temp <- df()
    
    # Check if df_temp has columns "x" and "y"
    if(all(c("x", "y") %in% colnames(df_temp))) {
      leaflet(df_temp) %>%
        addTiles() %>%
        addCircles(lng = ~x, lat = ~y, popup = ~paste(x, y), radius = input$buffer * 1000) # Multiply by 1000 to convert km to meters
    }
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
