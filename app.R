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

data(penguins, package = "palmerpenguins")

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

color_by <- varSelectInput(
  "color_by", "Color by",
  penguins[c("species", "island", "sex")],
  selected = "species"
)

map1 <- leaflet::leaflet()|>
  addTiles()

map2 <- map1


# UI ----------------------------------------------------------------------


ui <- page_navbar(
  # set theme elements 
  theme = bs_theme(
    bootswatch = "yeti",
    base_font = font_google("Inter"),
    version = "5",
    primary = "#1b998b",
    secondary = "#f6bd60"
    
  ),
  title = "Gap Analysis and Metacollection Management",
  br(),
  underline = TRUE,
  nav_spacer(),
  nav_panel("Landing Page",
            h3("project summary"),
            p(shinipsum::random_text(nwords = 100)),
            br(),
            plotOutput("image", height = "300px"),
            br(),
            p(random_text(nwords = 300))
            ),
  nav_panel("Map Example 1", 
            cards[[1]],),
  nav_panel("Map Example 2", 
            cards[[2]]),
  nav_menu(
    title = "External Links",
    align = "right",
    nav_item(tags$a("Source Code", href = "https://github.com/Jonathan-Gore/gap-analysis-shiny-app"), target="_blank"),
    nav_item(tags$a("Alanta Botanical Garden", href = "https://atlantabg.org/"), target="_blank")
  ),
)


# server ----------------------------------------------------------------
server <- function(input, output) {
  gg_plot <- reactive({
    ggplot(penguins) +
      geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
      theme_bw(base_size = 16) +
      theme(axis.title = element_blank())
  })
  
  output$map1 <- leaflet::renderLeaflet(map1)
  output$mapTable <- renderDT(shinipsum::random_DT(nrow = 5,ncol = 4))
  output$image <- renderImage(random_image())
  output$map2<- leaflet::renderLeaflet(map2)
  output$mapTable2 <- renderDT(shinipsum::random_DT(nrow = 5,ncol = 4))

}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

