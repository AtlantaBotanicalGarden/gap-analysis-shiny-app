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
# source modules --------------------------------------------------------
lapply(list.files(
  path = "modules/",
  pattern = ".R",
  full.names = TRUE),
  source)

# source UI or Server only functions ------------------------------------
lapply(list.files(
  path = "appFunctions",
  pattern = ".r",
  full.names = TRUE),
  source)

# global variables -----------------------------------------
gbifBackbone <- read_csv("appData/gbifBackBone.csv")

# column name structure 
## use this once we have the file 
JSONdata <- fromJSON('appData/data_validation.json')
valid_data_structure <- JSONdata[[1]]

# tempTable <- allData |>
#   dplyr::filter(taxon=="Magnolia fraseri")

# 
map1 <- leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16))|>
  addTiles()
# 
map2 <- map1

# expectedNames <- c("UID",	"taxon",	"genus",	"species",	"type",	"year",	"latitude",	"longitude",	"locality",	"collector")



# UI ----------------------------------------------------------------------
# testing starting with a fluid page. 
ui <- fluidPage(
  # css class
  class = "container-all",
  theme = bslib::bs_theme(
    version = "5",
    bootswatch = "yeti",
    bg = "#FFFFFF",
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
    p(style="text-align: center;",
      "This project was led by Alanta Botanical Garden in collaboration with the IMLS GCC Growing
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
      ".",
      tags$br(),
       tags$strong("This work is made possible by the Institute of Museum and Library Services ",
      tags$a(href = "https://www.imls.gov/", "(MG-252894-OMS-23).", target = "_blank"),
      "." ),
    ),
    fluidRow(
      p("Multiple logos?"),
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
    fluidRow(
      p("Or a single logo?"),
      column(2),
      column(8,
             div(img(src="Metacollections project logo_color.png", alt="Logo 2", align="center", width="60%"), style="text-align:center")),
      column(2),
    ),
    
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
    h2("Data Evaluation"),
    p("On this page individuals will be able to "),
    br(),
    tags$ul(
      tags$li("Select the Gensus and species being evaluated"),
      tags$li("Grab data on the species from GBIF"),
      tags$li("Upload data from a local environment into the tool"),
      tags$li("Evaluate the data from GBIF and the local environment on the map"),
      tags$li("future : remove points from the analys based on the data onmap")
    ),
   
      # define row for containing the map feaut
      # defeine the sidebar element
      layout_sidebar(
        height = "1000px",
      # sidebar feature
        sidebar = sidebar(
          position = "left",
          accordion(
            accordion_panel(
              # user selects the geneus 
              "1. Select Taxon",
              shiny::selectInput(
                inputId = "genusSelect",
                label = "Genus: ",
                choices = unique(gbifBackbone$genus),
                selected = "Magnolia"),
              # list of subspecies is generate for selection 
              uiOutput("speciesSelect"),
              # when possible the option for var subsp is listed
              uiOutput("taxonRank"),
              # final filter of sub species it provided 
              uiOutput("speciesIntrfraspecific"),
              # print the current taxon name 
              textOutput("currentSpecies")
            ),
            accordion_panel(
              "2. Add GBIF data to the map ",
              tags$br(),
              tags$strong("GBIF Taxon ID:"),  textOutput("gbiftaxonid"),
              checkboxInput("allowSyn", "Allow Synonyms in Data", TRUE),
              # p(tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit#gid=1735583299", "View GBIF Issue Codes ", target = "_blank")),
              actionButton("gbifPull", "Download Data from Gbif"),
              textInput("issueCodes", 
                        label = tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit#gid=1735583299",
                                       "View GBIF Issue Codes ", target = "_blank"), "see link below for options"),
              tags$br(),
              tags$strong("Download Details:"),  textOutput("gbifDownloadSpecifics"),
              tags$br(),
              actionButton("gbifToMap", "Add GBIF to map")
            ),
            accordion_panel(
              "3. Upload Your own data",
              p(tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit?usp=sharing", "View Data Format Example", target = "_blank")),
              fileInput("upload", "Upload a file"),
              tags$p("Testing for expected column names"),
              textOutput("validateColNames"),
              actionButton("uploadToMap", "add uploaded to map")
            ),
            accordion_panel(
              "4. Remove Data from Map ",
              "Eventually this might expand to have some more data cleaning options.",
              actionButton("pointsToMap", "Remove all GBIF data"),
              actionButton("pointsToMap", "Remove uploaded data")
            ),
            
            
          )
        ),
        # main panel features
        card(
          card_header("Map Element 1"),
          leaflet::leafletOutput("map1"),
          card_footer("Description of the map? ")
        )
      ),
    card(
      card_header("Information on the Records"),
      ### add a tab section here for evaluating GBIF or uploaded record 
      tags$p("**note**: currently this auto populates once a genus species is selected. Once we transistion to uploading data or grabing data from gbif and uploading users will have to 
             use a button to add content to this table"),
      DTOutput("mapTable"),
      downloadButton("download1", "Download the current table")
    )
    
  ), ## end data analysis page

  # Gap Analysis ------------------------------------------------------------
  nav_panel(
    title = "Gap Analysis",
    h2("Gap Analysis"),
    p("On this page individuals will be able to "),
    br(),
    tags$ul(
      tags$li("Utilize the dataset generated on the first Data Analysis page"),
      tags$li("Select a specific buffer size"),
      tags$li("Evaluate data on the map"),
      tags$li("Generate a statistical summary of the gap analysis"),
      tags$li("Export the results of the gap analysis")
    ),
    layout_sidebar(
      # sidebar feature
      height="600px", # does not seem to actively effect this 
      sidebar = sidebar(
        position = "right",
        accordion(
          accordion_panel(
            "Select Buffer Size",
            selectInput("bufferSize", "Select Buffer Distances in KM", choices = c("1", "5", "10","20","50"), selected = "50")
          ),
          accordion_panel(
            "Run Gap Analysis",
            actionButton("runGapAnalysis", "Generate Results")
          ),
          accordion_panel(
            "Export Map",
            actionButton("exportGapMap", "Download the current map")
          )
        )
      ),
      # main panel features
      card(
        card_header("Map Element 1"),
        leaflet::leafletOutput("map2"),
        card_footer("Description of the map? ")
      )
    ),
    card(
      card_header("Results of the Gap Analysis"),
      DTOutput("mapTable2"),
      card_footer(
        actionButton("exportGapTabke", "Download the current table")
      )
    )
  ),## end gap analysis page
  nav_spacer(),

  # Links -------------------------------------------------------------------
    nav_menu(
      title = "External Links",
      align = "right",
      nav_item(tags$a("Source Code", href = "https://github.com/Jonathan-Gore/gap-analysis-shiny-app"), target="_blank"),
      nav_item(tags$a("Alanta Botanical Garden", href = "https://atlantabg.org/"), target="_blank")
      ) # edd nav bar links
  )## end navbar page
)## end ui
  

# server ----------------------------------------------------------------
server <- function(input, output) {
  # UI select Subspecies 
  output$speciesSelect = renderUI({
    # grab the selection
    genusPick = input$genusSelect
    # filter the data 
    genusData <- gbifBackbone |>
      dplyr::filter(genus == as.character(genusPick))
    # define selector 
    selectInput("speciesSelect", "Select a species", choices = sort(genusData$specificEpithet), selected = )
  })
  # UI select variaty/subspec
  output$taxonRank = renderUI({
    # grab the selection
    genusPick = input$genusSelect
    speciesPick = input$speciesSelect
    # filter the data
    filteredData <- gbifBackbone |>
      dplyr::filter(genus == as.character(genusPick)) |>
      dplyr::filter(specificEpithet == as.character(speciesPick))
    # define selector
    selectInput("taxonRank", "Select a taxon rank", choices = filteredData$taxonRank, selected = )
  })
  # UI select sub species feature
  output$speciesIntrfraspecific = renderUI({
    # grab the selection
    genusPick = input$genusSelect
    speciesPick = input$speciesSelect
    intraPick = input$taxonRank
    # filter the data
    filteredData2 <- gbifBackbone |>
      dplyr::filter(genus == as.character(genusPick)) |>
      dplyr::filter(specificEpithet == as.character(speciesPick))|>
      dplyr::filter(taxonRank == as.character(intraPick))
      
    # define selector
    selectInput("speciesIntrfraspecific", "Select a infraspecific epithet", choices = filteredData2$infraspecificEpithet, selected = )
  })
  output$currentSpecies = renderText({
    name <- paste0("Current Taxon: ", input$genusSelect, " ",tolower(input$speciesSelect), " ")
    if(input$taxonRank != "species"){
      name <- paste0(name, tolower(input$taxonRank), " ", tolower(input$speciesIntrfraspecific))
    }
    # pass the object to render
    name
  })
  
  # pull the gbif id from the selection 
  output$gbiftaxonid <- renderText({
    f1 <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect)) |>
      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
    
    if(as.character(input$taxonRank) == "species"){
      f1$taxonID[1]
    }else{
      f1 |> 
        dplyr::filter(taxonRank == as.character(input$taxonRank))|>
        dplyr::filter(taxonRank == as.character(input$speciesIntrfraspecific))|>
        dplyr::select(taxonID)|>
        dplyr::pull()
      
    }
      
  })

  # generate the GBIF table data 
  # df1 <- reactive({
  #   f1 <- gbifData() |>
  #     dplyr::filter(genus == as.character(input$genusSelect)) |>
  #     dplyr::filter(specificEpithet == as.character(input$speciesSelect))
  #   
  #   if(as.character(input$taxonRank) == "species"){
  #     f1
  #   }else{
  #     f1 |> 
  #       dplyr::filter(taxonRank == as.character(input$taxonRank))|>
  #       dplyr::filter(taxonRank == as.character(input$speciesIntrfraspecific))
  #   }
  # })

  # Download data from GBIF -------------------------------------------------
  gbifData <- eventReactive(input$gbifPull, {
    # define params 
    initialPull <- query_gbif_occ(taxonkey = 2878373,
                   allow_synonyms_bool = TRUE)
    
    # filter to specific layers 
    structureData <- etl_gbif_occ_data(gbif_occurrence_df = initialPull,
                                       valid_data_structure = valid_data_structure)
    
    structureData
  })
  # 
  output$gbifDownloadSpecifics <- renderText({
    if(is.null(gbifData)){
      "There is no data available on GBIF for this species"
    }else{
      paste0("The query returned ", nrow(gbifData), " records.")
    }
  })


  # create spatial data for example/GBIF feature  -----------------------------------
  sp1 <- observeEvent(input$gbifToMap, {
    # generate spatial object 
    pointsVals <- gbifData() |>
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)|>
      dplyr::mutate(
        popup = paste0("<strong>", as.character(taxon),"</strong>", # needs to be text
                       "<br/><strong> Type: </strong>", type,
                       "<br/><b>Collector Name:</b> ", collector,
                       "<br/><b>Locality Description):</b> ", locality),
        color = case_when(
          type == "H" ~ "#1184d4",
          type == "G" ~ "#6300f0"
        )
      )
    
    # update the map
    leafletProxy("map1")|>
      setView(lng = pointsVals$longitude[1], lat = pointsVals$latitude[1], zoom = 6)|>
      clearGroup(group = "GBIF") |>
      addCircleMarkers(
        data = pointsVals,
        color = ~color,
        stroke = FALSE,
        fillOpacity = 0.9,
        popup = ~popup,
        group = "GBIF"
      ) |>
      # single legend for the GBIF features
      addLegend(
        position = "topright",
        colors = c("#1184d4","#6300f0"),
        labels = c("H","G"),
        title = "GBIF Data",
        opacity = 1,
        group = "GBIF"
      )|>
      addLayersControl(
        position = "topleft",
        overlayGroups = c("GBIF"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # create spatial data for uploaded data  -----------------------------------
  sp2 <- observeEvent(input$uploadToMap, {
    # generate spatial object 
    pointsVals2 <- dataUpload()|>
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)|>
      dplyr::mutate(
        popup = paste0("<strong>", as.character(taxon),"</strong>", # needs to be text
                       "<br/><strong> Type: </strong>", type,
                       "<br/><b>Collector Name:</b> ", collector,
                       "<br/><b>Locality Description):</b> ", locality),
        color = case_when(
          type == "H" ~ "#1184d4",
          type == "G" ~ "#6300f0"
        )
      )
    # update the map 
    leafletProxy("map1")|>
      setView(lng = pointsVals2$longitude[1], lat = pointsVals2$latitude[1], zoom = 6)|>
      clearGroup(group = "Upload Dataset") |>
      addCircleMarkers(
        data = pointsVals2,
        color = ~color,
        stroke = TRUE,
        fillOpacity = 0.9,
        popup = ~popup,
        group = "Upload Dataset"
      ) |>
      # single legend for the GBIF features
      addLegend(
        position = "topright",
        colors = c("#1184d4","#6300f0"),
        labels = c("H","G"),
        title = "Upload Dataset",
        opacity = 1,
        group = "Upload Dataset"
      )|>
      addLayersControl(
        position = "topleft",
        overlayGroups = c("Upload Dataset"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  

  # Export the table from data processing page  -----------------------------------------------
  # The requested dataset
  output$download1 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$genusSelect,"_",input$speciesSelect, "_data.csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(df1(), file, row.names = FALSE)
    }
  )
  

  # read in the uploaded data -----------------------------------------------
  dataUpload <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
 
  })
  output$validateColNames <- renderText({
    colNames <- names(dataUpload())
    vals <- c()
    for(i in seq_along(expectedNames)){
      test1 <-TRUE %in% grepl(pattern = expectedNames[i], x = colNames)
      if(test1 == FALSE){
        vals[i] <- expectedNames[i]
      }
    }
    nonMatchedNames <- vals[!is.na(vals)]
    if(is.null(nonMatchedNames)){
      print("All column names match")
    }else{
      print(paste0("The following columns are not present in the uploaded data ", nonMatchedNames))
    }
    
    
  })
  
  
  output$map1 <- leaflet::renderLeaflet(map1)
  output$mapTable <- renderDT(gbifData())
  output$image <- renderImage(random_image())
  output$map2<- leaflet::renderLeaflet(map2)
  output$mapTable2 <- renderDT(shinipsum::random_DT(nrow = 5,ncol = 4))

}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

