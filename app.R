###
# draft material of the Gap Analysis and Metacollection Management tool
# carverd@colostate.edu
# 20240225
### 
library(shiny)
library(bslib)
library(ggplot2)
library(shinipsum)
library(leaflet)
library(DT)
library(readr)
library(dplyr)
library(tools)
library(vroom)
library(rgbif)
library(spocc)
library(jsonlite)
library(rgbif)
library(stringr)
library(shinycssloaders) # not working for the full page 
library(shinyalert)
library(plotly)
library(sf)
library(tidyterra)

# source modules --------------------------------------------------------
lapply(list.files(
  path = "shinyModules",
  pattern = ".R",
  full.names = TRUE),
  source)

# source UI or Server only functions ------------------------------------
lapply(list.files(
  path = "appFunctions",
  pattern = ".r",
  full.names = TRUE),
  source)

# initialize maps ----------------------------------------------------------
maps <- getMaps()


# UI ----------------------------------------------------------------------
# testing starting with a fluid page. 
ui <- fluidPage(
  ## Banner section ----------------------------------------------------------
  tags$div(
    # HTML(tags$span(style="color:red","This application is under active development. If you experience a loss of connection while using the application please refresh your page or try again at a latter time. We apologize for any inconvience."))
    tags$h6(style="color:red; background-color: #f5e642;    width: 100%;text-align: center; padding:10px ",
            "This application is under active development. If you experience a loss of connection while using the application please refresh your page or try again at a different time. We apologize for any inconvience.")
  ),
  # Set styling for the app -------------------------------------------------
  theme = bslib::bs_theme(
    version = "5",
    bootswatch = "yeti",
    # bg = "#948",
    # fg = "#655560",
    primary = "#52b788",
    secondary = "#f6bd60",
    base_font = font_google("Nunito Sans"),
    heading_font =  font_google("Merriweather")
  ),
  includeCSS("www/style.css"), 

  # Establish primary navigation page structure  ----------------------------
  page_navbar(
    title = "Gap Analysis and Metacollection Management",
    bg ="#52b788",
    window_title = "GAMMA Tool",
    position = "static-top",
    # set some space between the Application title and the tab selectors 
    nav_spacer(),
    nav_panel(
      title = "Data Evaluation",
      dataEvaluation(id = "dataEval")
    ),
    nav_panel(
      title = "Gap Analysis"
    ),
    nav_panel(
      title = "About",
      aboutPage()
    ),
    nav_spacer(),
    
    ## Links -------------------------------------------------------------------
    nav_menu(
      title = "External Links",
      align = "right",
      nav_item(tags$a("Source Code", href = "https://github.com/Jonathan-Gore/gap-analysis-shiny-app"), target="_blank"),
      nav_item(tags$a("Alanta Botanical Garden", href = "https://atlantabg.org/"), target="_blank")
    )
  )
)
  
# server ----------------------------------------------------------------
server <- function(input, output) {
   
  dataEvaluation_server(id = "dataEval",
                        map = maps$gbifMap,
                        gbifBackbone = gbifBackbone)
  }



# call the app ------------------------------------------------------------


shinyApp(ui, server)
