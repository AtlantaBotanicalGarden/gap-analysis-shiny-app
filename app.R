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
library(palmerpenguins)
# source modules --------------------------------------------------------
lapply(list.files(
  path = "modules/",
  pattern = ".R",
  full.names = TRUE),
  source)

# source UI or Server only functions ------------------------------------
lapply(list.files(
  path = "appFunctions/",
  pattern = ".R",
  full.names = TRUE),
  source)

# global variables -----------------------------------------

# spatial Data 
spData <- sf::st_read("appData/spatialData.gpkg")

## this content will likely be replaced by module elements
cards <- list(
  card(
    full_screen = TRUE,
    card_header("Map Element"),
    layout_sidebar(
      sidebar = sidebar("Sidebar"),
      "Main contents",
      leaflet::leafletOutput("map1"),
      "Data Tables",
      DTOutput("mapTable")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Gap Analysis"),
    layout_columns(
      col_widths = c(12,8,4),
      row_heights = c(2,2),
      card(leaflet::leafletOutput("map2")),
      card(DTOutput("mapTable2")),
      card(p("Some options for changing parameters for the analysis"))
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Body Mass"),
    plotOutput("body_mass")
  )
)

map1 <- leaflet::leaflet()|>
  addTiles()

map2 <- map1




# define the theme elements  ----------------------------------------------
theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "yeti",
  base_font = font_google("Nunito Sans"),
  heading_font =  font_google("Merriweather")
  )



# UI ----------------------------------------------------------------------


ui <- page_navbar(
  title = "Gap Analysis and Metacollection Management",
  # set theme elements 
  ## settting the colors here is not working well at all... might just need to rely on the scss file but wait on that development wokr. 
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "yeti",
    base_font = font_google("Nunito Sans"),
    heading_font =  font_google("Merriweather")
  ),
  # header = 
  bg ="#52b788",
  window_title = "GAMMA Tool",
  
  br(),
  underline = TRUE,
  nav_spacer(),
  nav_panel("About",
            h3("Project Summary"),
            p(shinipsum::random_text(nwords = 100)),
            br(),
            tags$blockquote("The GAMMA tool will allow users to quantify and assess the completeness of recent collections made, as well as enable meta collection communities to assess the current ex situ conservation status and collection gaps across all participating collections.", cite = "Hadley Wickham"),
            br(),
            p(shinipsum::random_text(nwords = 50)),
            br(),
            plotOutput("image",height = "20px"),
            br(),
            p(random_text(nwords = 300))
            ),
  nav_panel("1. Gather and Clean Data ", 
            cards[[1]],),
  nav_panel("2. Run a Gap Analysis", 
            cards[[2]]),
  nav_spacer(),
  nav_menu(
    title = "External Links",
    align = "right",
    nav_item(tags$a("Source Code", href = "https://github.com/Jonathan-Gore/gap-analysis-shiny-app"), target="_blank"),
    nav_item(tags$a("Alanta Botanical Garden", href = "https://atlantabg.org/"), target="_blank")
  ),
)


# server ----------------------------------------------------------------
server <- function(input, output) {
  
  output$map1 <- leaflet::renderLeaflet(map1)
  output$mapTable <- renderDT(shinipsum::random_DT(nrow = 5,ncol = 4))
  output$image <- renderImage(random_image())
  output$map2<- leaflet::renderLeaflet(map2)
  output$mapTable2 <- renderDT(shinipsum::random_DT(nrow = 5,ncol = 4))

}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

