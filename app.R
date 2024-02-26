###
# draft material of the Gap Analysis and Metacollection Management tool
# carverd@colostate.edu
# 20240225
### 

library(shiny)
# library(shinyWidgets)
# library(leaflet)
library(bslib)
# library(terra)
# library(sf)
# library(leaflet.extras)
# library(plotly)
# library(rmarkdown)
# library(tmap)
# library(tidyverse)
# library(shinyalert)
# library(DT)


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


# UI section --------------------------------------------------------------
ui <- fluidPage(
  class = "container-all",
  navbarPage(
    # required as reference for the button selection process. 
    id = "pages",
    theme = bs_theme(version = 5,
                     bootswatch = "yeti",
                     primary = "#1b4332",
                     secondary = "#f6bd60",
                     base_font = "Merriweather ") |>
      bslib::bs_add_rules(sass::sass_file("www/style.scss")),
    # 
    # the text that appears next to the pages
    title =  HTML("Gap Analysis and Metacollection Management tool <em class = beta-text> Beta Version </em>"),
    # the text in the browser tab
    windowTitle = "GAMMMA",
    # means of applying data to all pages -- could be useful in footer section
    # header = h5("This content appears at the top of every page "),
    # footer = "This content appears at the bottom of every page",
    
    ## Home page --------------------------------------------------------------- 
    tabPanel(title = "Home"),
    ## About Page -------------------------------------------------------------------
    tabPanel(title = "About", class = "about-tab",
          #    p(
          #    "This website was designed to let you visually explore the future of
          # climate change and how it may impact Kenya’s forests. The purpose is to
          # provide a useful source of information (using maps, graphs, and tables)
          # about the potential impacts of climate change, and possible outcomes 
          # that could arise for different types of trees, across Kenya."
          #      ),
          #    p(
          #     "On this page, you can find information regarding the origin of the
          #   project, the team of people that were involved, publications, and other
          #   relevant information."
          #      ),
          #    br(),
          #    fluidRow(
          #      # column(1, 
          #      #        div(img(src="email-icon.png", alt="Email icon", width="30px"),style="text-align:left")),
          #      column(12,
          #             div(em("For any questions or feedback please reach out to Professor Patrick Keys at patrick.keys@colostate.edu"),
          #                 style="text-align:left;color:grey"))
          #    ),
          #    hr(),
          #    h2("Project Background"),
          #    p("This project was led by Colorado State University in collaboration with ",
          #      tags$a(href = "https://www.sei.org/centres/africa/", "SEI-Africa", target = "_blank"),
          #      " and the ",
          #      tags$a(href = "https://www.kefri.org/home.html", "Kenya Forestry Research Institute", target = "_blank"),
          #      " (KEFRI). The project was primarily funded by the ",
          #      tags$a(href = "https://www.nasa.gov/", "National Aeronautics and Space Administration", target = "_blank"),
          #      " (NASA) ",
          #      tags$a(href = "https://cce.nasa.gov/biodiversity/index.html", "Biological Diversity and Ecological Conservation program", target = "_blank"),
          #      " (Project# 80NSSC19K0182)."
          #    ),
          #    fluidRow(
          #      column(3, 
          #             div(img(src="ram.png", alt="CSU Logo", align="center", width="50%"), style="text-align:center")),
          #      column(3,
          #             div(img(src="NASA_logo.svg", alt="NASA Logo", align="center", width="60%"), style="text-align:center")),
          #      column(3,
          #             div(img(src="kefri_logo.png", alt="KEFRI Logo", align="center", width="50%"), style="text-align:center")),
          #      column(3,
          #             div(img(src="sei_logo.png", alt="SEI Logo", align="center", width="40%"), style="text-align:center"))
          #    ),
          #    hr(),
          #    h2("Who We Are"),
          #    p("This website is the result of a collaboration among the following individuals and institutions:"),
          #    fluidRow(
          #      column(3,
          #             strong("SEI-Africa"),
          #             p(tags$a(href = "https://www.sei.org/people/anderson-kehbila/", "Dr. Anderson Kehbila", target = "_blank"))
          #      ),
          #      column(3,
          #             strong("KEFRI"),
          #             p("Dr. Vincent Oeba")
          #      ),
          #      column(
          #        3,
          #        strong("Colorado State University"),
          #        p(
          #          tags$a(href = "https://www.atmos.colostate.edu/people/faculty/keys/", "Professor Patrick Keys", target = "_blank"),
          #          br(),
          #          tags$a(href = "http://sites.warnercnr.colostate.edu/jdsal/people/rekha/", "Dr. Rekha Warrier", target = "_blank"),
          #          br(),
          #          tags$a(href = "https://www.nrel.colostate.edu/investigator/randall-boone-homepage/", "Professor Randall Boone", target = "_blank"),
          #          br(),
          #          tags$a(
          #            href = "https://www.libarts.colostate.edu/people/kgalvin/",
          #            "Professor Kathleen Galvin",
          #            target = "_blank"
          #          )
          #        )
          #      ),
          #      column(3,
          #             strong(tags$a(href = "https://gis.colostate.edu/", "Geospatial Centroid", target = "_blank")),
          #             p(tags$a(href = "https://caitlinmothes.com/", "Dr. Caitlin Mothes", target = "_blank"),
          #               br(),
          #               tags$a(href = "https://carverd.com/", "Dan Carver", target = "_blank"))
          #      )
          #      
          #    )
          #    # hr(),
          #    # h2("Publications (In-progress)"),
          #    # p("A variety of published work stemmed from this project, including reports, guidance documents, and academic journal articles"),
          #    # tags$ol(
          #    #   tags$li("Report on forest cover change scenarios"), 
          #    #   tags$li("Model documentation for SPIRALL L-Range"), 
          #    #   tags$li("Ecology & Society article"),
          #    #   tags$li("Earth Interactions")
          #    # )
    ),
  ),
    # combine scenarios into navbar menu
    navbarMenu(
      title = "temp",
      # ## Optimistic --------------------------------------------------------------
      # tabPanel(title = "Optimistic",
      #          tabsetPanel(
      #            type = "pills",
      #            tabPanel(
      #              "Climate change in Kenya",
      #              map_UI(id = "ssp126", panelName = panelNames[1])
      #            ),
      #            tabPanel(
      #              "Climate and disturbance effects on tree cover",
      #              map2_UI(
      #                id = "ssp126_2",
      #                panelName = panelNames[1],
      #                county_names = county_names
      #              )
      #            )
      #          )
      #          ),
      # ## Middle of the road ------------------------------------------------------
      # tabPanel(title = "Middle of the Road",
      #          tabsetPanel(
      #            type = "pills",
      #            tabPanel(
      #              "Climate change in Kenya",
      #              map_UI(id = "ssp245", panelName = panelNames[2])
      #            ),
      #            tabPanel(
      #              "Climate and disturbance effects on tree cover",
      #              map2_UI(
      #                id = "ssp245_2",
      #                panelName = panelNames[2],
      #                county_names = county_names
      #              )
      #            )
      #          )),
      # ## Pessimistic -------------------------------------------------------------
      # tabPanel(title = "Pessimistic",
      #          tabsetPanel(
      #            type = "pills",
      #            tabPanel(
      #              "Climate change in Kenya",
      #              map_UI(id = "ssp370", panelName = panelNames[3])
      #            ),
      #            tabPanel(
      #              "Climate and disturbance effects on tree cover",
      #              map2_UI(
      #                id = "ssp370_2",
      #                panelName = panelNames[3],
      #                county_names = county_names
      #              )
      #            )
      #          )),
      # ## Extreme Heat ------------------------------------------------------------
      # tabPanel(title = "Extreme",
      #          tabsetPanel(
      #            type = "pills",
      #            tabPanel(
      #              "Climate change in Kenya",
      #              map_UI(id = "ssp585", panelName = panelNames[4])
      #            ),
      #            tabPanel(
      #              "Climate and disturbance effects on tree cover",
      #              map2_UI(
      #                id = "ssp585_2",
      #                panelName = panelNames[4],
      #                county_names = county_names
      #              )
      #            )
      #          )
      # )
    ),
    # validation_UI(id = "val"),
  # tags$footer(includeHTML("www/footer.html"))
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  # # set button functionality ------------------------------------------------
  # pageButtonServer("optimistic", parentSession = session,pageName = "Optimistic" )
  # pageButtonServer("middle", parentSession = session,pageName = "Middle of the Road" )
  # pageButtonServer("pessimistic", parentSession = session,pageName = "Pessimistic" )
  # pageButtonServer("extreme", parentSession = session,pageName = "Extreme" )
  # 
  # 
  # 
  # 
  # # ssp126 data -------------------------------------------------------------
  # map_server(id = "ssp126", histRasters = allRasters_abs$hist, 
  #            sspRasters =  allRasters_abs$`126`,
  #            changeRasters = allRasters_change$`126`,
  #            ssp = "126",
  #            pals1 = pal_abs,
  #            pals2 = pal_change,
  #            countyFeat = county,
  #            county_avg = county_avg)
  # map2_server(id = "ssp126_2",
  #             histRaster = climateManagementInputs$existingForest,
  #             futureRaster = climateManagementInputs$expandedForest,
  #             managementRasters = climateManagementInputs$forestChangeRasters,
  #             countryDF = climateManagementInputs$areaCountry,
  #             countyDF = climateManagementInputs$areaCounty,
  #             pal1 = pal_management,
  #             ssp = "126",
  #             countyFeat = county,
  #             decid_report = decid,
  #             ever_report = ever,
  #             proj_report = projection,
  #             pop_report = population,
  #             ecosystem_data = ecosystem_data)
  # # ssp245 data -------------------------------------------------------------
  # map_server(id = "ssp245",
  #            histRasters = allRasters_abs$hist,
  #            sspRasters =  allRasters_abs$`245`,
  #            changeRasters = allRasters_change$`245`,
  #            ssp = "245",
  #            pals1 = pal_abs,
  #            pals2 = pal_change,
  #            countyFeat = county,
  #            county_avg = county_avg)
  # map2_server(id = "ssp245_2",
  #             histRaster = climateManagementInputs$existingForest,
  #             futureRaster = climateManagementInputs$expandedForest,
  #             managementRasters = climateManagementInputs$forestChangeRasters,
  #             countryDF = climateManagementInputs$areaCountry,
  #             countyDF = climateManagementInputs$areaCounty,
  #             pal1 = pal_management,
  #             ssp = "245",
  #             countyFeat = county,
  #             decid_report = decid,
  #             ever_report = ever,
  #             proj_report = projection,
  #             pop_report = population,
  #             ecosystem_data = ecosystem_data)  
  # # ssp370 data -------------------------------------------------------------
  # map_server(id = "ssp370",
  #            histRasters = allRasters_abs$hist,
  #            sspRasters =  allRasters_abs$`370`,
  #            changeRasters = allRasters_change$`370`,
  #            ssp = "370",
  #            pals1 = pal_abs,
  #            pals2 = pal_change,
  #            countyFeat = county,
  #            county_avg = county_avg)
  # map2_server(id = "ssp370_2",
  #             histRaster = climateManagementInputs$existingForest,
  #             futureRaster = climateManagementInputs$expandedForest,
  #             managementRasters = climateManagementInputs$forestChangeRasters,
  #             countryDF = climateManagementInputs$areaCountry,
  #             countyDF = climateManagementInputs$areaCounty,
  #             pal1 = pal_management,
  #             ssp = "370",
  #             countyFeat = county,
  #             decid_report = decid,
  #             ever_report = ever,
  #             proj_report = projection,
  #             pop_report = population,
  #             ecosystem_data = ecosystem_data)  
  # 
  # # ssp585 data ------------------------------------------------------------
  # map_server(id = "ssp585",
  #            histRasters = allRasters_abs$hist,
  #            sspRasters =  allRasters_abs$`585`,
  #            changeRasters = allRasters_change$`585`,
  #            ssp = "585",
  #            pals1 = pal_abs,
  #            pals2 = pal_change,
  #            countyFeat = county,
  #            county_avg = county_avg)
  # map2_server(id = "ssp585_2",
  #             histRaster = climateManagementInputs$existingForest,
  #             futureRaster = climateManagementInputs$expandedForest,
  #             managementRasters = climateManagementInputs$forestChangeRasters,
  #             countryDF = climateManagementInputs$areaCountry,
  #             countyDF = climateManagementInputs$areaCounty,
  #             pal1 = pal_management,
  #             ssp = "585",
  #             countyFeat = county,
  #             decid_report = decid,
  #             ever_report = ever,
  #             proj_report = projection,
  #             pop_report = population,
  #             ecosystem_data = ecosystem_data)
  # 
  # # validation maps ------------------------------------------------------
  # validation_server(id = "val", 
  #                   npp_val = npp_val,
  #                   carbon_val = carbon_val,
  #                   county = county)
  # 
  # 
}

shinyApp(ui, server)

