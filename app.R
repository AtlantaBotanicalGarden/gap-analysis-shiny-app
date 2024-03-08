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
mafa <- read_csv("appData/magnolia_fraseri.csv")
qubr <- read_csv("appData/Quercus_brandegeei.csv")
allData <- bind_rows(mafa,qubr) |> as.data.frame()
# temp data




## this content will likely be replaced by module elements
# cards <- list(
#   card(
#     full_screen = TRUE,
#     card_header("Map Element"),
#     layout_sidebar(
#       sidebar = sidebar("Sidebar"),
#       "Main contents",
#       leaflet::leafletOutput("map1"),
#       "Data Tables",
#       DTOutput("mapTable")
#     )
#   ),
#   card(
#     full_screen = TRUE,
#     card_header("Gap Analysis"),
#     layout_columns(
#       col_widths = c(12,8,4),
#       row_heights = c(2,2),
#       card(leaflet::leafletOutput("map2")),
#       card(DTOutput("mapTable2")),
#       card(p("Some options for changing parameters for the analysis"))
#     )
#   ),
#   card(
#     full_screen = TRUE,
#     card_header("Body Mass"),
#     plotOutput("body_mass")
#   )
# )
# 
# map1 <- leaflet::leaflet()|>
#   addTiles()
# 
# map2 <- map1




# define accodian selection options 
genus_by <- shiny::selectInput(
  inputId = "genus",
  label = "Genus: ",
  choices = unique(allData$genus)
  )
species_by <- varSelectInput(
  inputId = "Species",
  label = "Species",
  data =  c("fraseri", "brandegeei"),
  selected = "fraseri"
)


# UI ----------------------------------------------------------------------
# testing starting with a fluid page. 
ui <- fluidPage(
  # css class
  class = "container-all",
  theme = bslib::bs_theme(
    version = "5",
    bootswatch = "yeti",
    bg = "#cce3de",
    fg = "#655560",
    primary = "#52b788",
    secondary = "#f6bd60",
    base_font = font_google("Nunito Sans"),
    heading_font =  font_google("Merriweather")
  ),
  includeCSS("www/style.css"),

  # navbarPage --------------------------------------------------------------
  ### primary container for the application. 
  page_navbar(
    title = "Gap Analysis and Metacollection Management",
    bg ="#52b788",
    window_title = "GAMMA Tool",
    underline = TRUE,
    position = "static-top",
    # footer = p("this is footer content? Not very good at the moment"),
    # set some space between the Application title and the tab selectors 
    nav_spacer(),
  # landing page panel ------------------------------------------------------
  nav_panel(
    title = "About",
    card(full_screen = TRUE,
         h3("Project Summary"),
         p(shinipsum::random_text(nwords = 100))
         ),
    br(),
    tags$blockquote("The GAMMA tool will allow users to quantify and assess the completeness of recent collections made, as well as enable meta collection communities to assess the current ex situ conservation status and collection gaps across all participating collections.", cite = "Hadley Wickham"),
    br(),
    fluidRow(
      column(12,
             div(em("For any questions or feedback please reach out to Maintainer of the Website @ there email.com"),
                 style="text-align:left"))
    ),
    h2("Project Background"),
    p("This project was led by Alanta Botanical Garden in collaboration with the IMLS GCC Growing
      Metacollections team which includes individuals from ",
      tags$a(href = "https://mortonarb.org/", "The Morton Arboretum", target = "_blank"),
      ", ",
      tags$a(href = "https://sdbg.org/", "San Diego Botanic Garden", target = "_blank"),
      ", ",
      tags$a(href = "https://www.montgomerybotanicalgardens.com/", "Montgomery Botanical Gardens at Oak Park", target = "_blank"),
      ", ",
      tags$a(href = "https://www.smith.edu/", "Smith College", target = "_blank"),
      ", ",
      tags$a(href = "https://www.auburn.edu/", "Auburn University", target = "_blank"),
      ", ",
      tags$a(href = "https://www.lotusland.org/", "Lotus Land", target = "_blank"),
      ". The project was primarily funded by the ",
      tags$a(href = "https://www.imls.gov/", "Institute of Museum and Library Services", target = "_blank"),
      " (IMLS) "
    ),
    fluidRow(
      column(3,
             div(img(src="temp.png", alt="Logo 2", align="center", width="60%"), style="text-align:center")),
      column(3,
             div(img(src="temp.png", alt="Logo 2", align="center", width="60%"), style="text-align:center")),
      column(3,
             div(img(src="temp.png", alt="Logo 3", align="center", width="50%"), style="text-align:center")),
      column(3,
             div(img(src="temp.png", alt="Logo 4", align="center", width="40%"), style="text-align:center"))
    ),
    br(),
    card(full_screen = TRUE,
         h3("Learn More"),
         p(random_text(nwords = 300)),
         br()
         ),
    hr(),
    h2("Who We Are"),
    p("This website is the result of a collaboration among the following individuals and institutions:"),
    fluidRow(
      column(3,
             strong("Alanta Botanical Garden"),
             p(tags$a(href = "https://atlantabg.org/article/emily-e-d-coffey-ph-d/", "Emily E. D. Coffey, Ph.D", target = "_blank")),
             p(tags$a(href = "https://atlantabg.org/article/jean-linsky-m-sc/", "Jean Linsky, M.Sc.", target = "_blank")),
            p("etc...")
      ),
      column(3,
             strong("Other organizations and people as needed"),
             p("Superstar 1"),
             p("Superstar 2")
      ),
    ),
  ),
  # Data Evaluation ---------------------------------------------------------
  nav_panel(
    title = "Data Evaluation",
    # because of the fixed header this need to be pushed down
    h2("Description of Page"),
    p(shinipsum::random_text(nwords = 50)),
    # define row for containing the map feaut
    page_fluid(
      # defeine the sidebar element 
      layout_sidebar(
        border = TRUE,
        border_color = "#f6bd60",
        padding = "10px",
        sidebar = sidebar(
          position = "left",
          accordion(
            accordion_panel(
              "S",
              genus_by
            ),
            accordion_panel(
              "Other controls",
              "Other controls go here"
            )
          )
        )
      )
    )
  ),

  # Gap Analysis ------------------------------------------------------------
  nav_panel(
    title = "Gap Analysis",
    
  ),
  nav_spacer(),

  # Links -------------------------------------------------------------------
    nav_menu(
      title = "External Links",
      align = "right",
      nav_item(tags$a("Source Code", href = "https://github.com/Jonathan-Gore/gap-analysis-shiny-app"), target="_blank"),
      nav_item(tags$a("Alanta Botanical Garden", href = "https://atlantabg.org/"), target="_blank")
    )
  )
  
)
  
# 
# ui <- page_navbar(
#   title = "Gap Analysis and Metacollection Management",
#   # set theme elements 
#   ## settting the colors here is not working well at all... might just need to rely on the scss file but wait on that development wokr. 
#   theme = bslib::bs_theme(
#     version = 5,
#     bootswatch = "yeti",
#     base_font = font_google("Nunito Sans"),
#     heading_font =  font_google("Merriweather")
#   ),
#   # header = 
#   bg ="#52b788",
#   window_title = "GAMMA Tool",
#   
#   br(),
#   underline = TRUE,
#   nav_spacer(),
#   nav_panel("About",
#             h3("Project Summary"),
#             p(shinipsum::random_text(nwords = 100)),
#             br(),
#             tags$blockquote("The GAMMA tool will allow users to quantify and assess the completeness of recent collections made, as well as enable meta collection communities to assess the current ex situ conservation status and collection gaps across all participating collections.", cite = "Hadley Wickham"),
#             br(),
#             p(shinipsum::random_text(nwords = 50)),
#             br(),
#             plotOutput("image",height = "20px"),
#             br(),
#             p(random_text(nwords = 300))
#             ),
#   nav_panel("1. Gather and Clean Data ", 
#             cards[[1]],),
#   nav_panel("2. Run a Gap Analysis", 
#             cards[[2]]),
#   
# )
# 
# 
# # server ----------------------------------------------------------------
server <- function(input, output) {

  output$map1 <- leaflet::renderLeaflet(map1)
  output$mapTable <- renderDT(shinipsum::random_DT(nrow = 5,ncol = 4))
  output$image <- renderImage(random_image())
  output$map2<- leaflet::renderLeaflet(map2)
  output$mapTable2 <- renderDT(shinipsum::random_DT(nrow = 5,ncol = 4))

}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

