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
ecoRegions <- terra::vect("appData/ecoregionsSimplified.gpkg")
# column name structure 
## use this once we have the file 
JSONdata <- fromJSON('appData/data_validation.json')
valid_data_structure <- JSONdata[[1]]


expectedNames <- c("Accession Number",	"Taxon Name",	"Current Germplasm Type",	"Collection Date",
                   "Latitude",	"Longitude",	"Locality",	"Collector")
tempTable <- read.csv("dataToPreProcess/Magnolia_acuminata_data.csv")|>
  dplyr::select(-c("genus","species", "issues"))|>
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

names(tempTable) <- c(expectedNames ,"geometry")



names(tempTable) <- expectedNames
# 
# move this to a function !!!
map1 <- leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16))|>
  addTiles()|> 
  # layer control groups should not be set at the map proxy level as they will overwrite the existing element.
  addLayersControl( 
    position = "topleft",
    overlayGroups = c("GBIF", "Upload Dataset"),
    options = layersControlOptions(collapsed = TRUE)
  )
# 
map2 <- leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16))|>
  addTiles()|>
  addMapPane("points", zIndex = 420) |>
  addMapPane("buffers", zIndex = 410)|>
  addMapPane("gaps", zIndex = 405) |> # shown below ames_circles%>% # shown below ames_circles
  addLayersControl( 
    position = "topleft",
    overlayGroups = c("Reference Records",
                      "Germplasm Records",
                      "Buffers",
                      "GRS gaps",
                      "ERS gaps"),
    options = layersControlOptions(collapsed = FALSE))
# |>
#   leaflet::hideGroup(c("GRS gaps", "ERS gaps"))





# UI ----------------------------------------------------------------------
# testing starting with a fluid page. 
ui <- fluidPage(
  # css class
  class = "container-all",
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

  ## Banner section ----------------------------------------------------------
  tags$div(
    # HTML(tags$span(style="color:red","This application is under active development. If you experience a loss of connection while using the application please refresh your page or try again at a latter time. We apologize for any inconvience."))
    tags$h6(style="color:red; background-color: #f5e642;    width: 100%;text-align: center; padding:10px ",
              "This application is under active development. If you experience a loss of connection while using the application please refresh your page or try again at a different time. We apologize for any inconvience.")
    ),
  ## navbarPage --------------------------------------------------------------
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
  ## Data Evaluation ---------------------------------------------------------
  nav_panel(
    title = "Data Evaluation",
      # define row for containing the map feaut
      # defeine the sidebar element
      layout_sidebar(
        height = "700px",
      # sidebar feature
        sidebar = sidebar(
          position = "left",
          accordion(
            open = FALSE,
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
              uiOutput("speciesInfraspecific"),
              # print the current taxon name 
              textOutput("currentSpecies")
            ),
            accordion_panel(
              "2. Download and Display GBIF data ",
              tags$br(),
              tags$strong("GBIF Taxon ID:"),  textOutput("gbiftaxonid"),
              checkboxInput("allowSyn", "Allow Synonyms in Data", TRUE),
              # p(tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit#gid=1735583299", "View GBIF Issue Codes ", target = "_blank")),
              actionButton("gbifPull", "Download Data from Gbif"),
              textInput("issueCodes", 
                        label = tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit#gid=1735583299",
                                       "View GBIF Issue Codes ", target = "_blank"), "see link below for options"),
              tags$br(),
              # shinycssloaders::withSpinner(
                tags$strong("Download Details:"),  textOutput("gbifDownloadSpecifics"),
              # ),
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
              "Selecting a button will remove a specific layer from the map. The toggle buttons in the top left corner of the map can be uses to hide specific layers.",
              actionButton("removeGBIF", "Remove all GBIF data"),
              actionButton("removeUpload", "Remove uploaded data")
            ),
            accordion_panel(
              "5. Move data to Gap Analysis",
              "Select the button below with gather all GBIF and uploaded datasets and add them to the Gap Analysis Map.",
              actionButton("compileDatasets", "Compile Data For Gap Analysis")
              ),
            
            
          )
        ),
        # main panel features
        card(
          card_header("Map of Downloaded and Uploaded Datasets"),
          leaflet::leafletOutput("map1"),
          # card_footer("Description of the map? ")
        )
      ),
    navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Tabular Data of the Records",
        nav_panel(
          "GBIF_table",
          downloadButton("download1", "Download the GBIF data"),
          card_title("GBIF Records"),
          shinycssloaders::withSpinner(
          DTOutput("mapTableGBIF")),
        ),
        nav_panel(
          "Uploaded Data",
          downloadButton("download2", "Download the Uploaded data"),
          card_title("Uploaded Records"),
          DTOutput("mapTableUpload"),
        )
      )
      ### add a tab section here for evaluating GBIF or uploaded record 
      # tags$p("**note**: currently this auto populates once a genus species is selected. Once we transistion to uploading data or grabing data from gbif and uploading users will have to 
      #        use a button to add content to this table"),
      # DTOutput("mapTable"),
      # downloadButton("download1", "Download the current table")
    ), ## end data analysis page

  ## Gap Analysis ------------------------------------------------------------
  nav_panel(
    title = "Gap Analysis",
    h2("Gap Analysis"),
    # p("On this page individuals will be able to "),
    # br(),
    # tags$ul(
    #   tags$li("Utilize the dataset generated on the first Data Analysis page"),
    #   tags$li("Select a specific buffer size"),
    #   tags$li("Evaluate data on the map"),
    #   tags$li("Generate a statistical summary of the gap analysis"),
    #   tags$li("Export the results of the gap analysis")
    # ),
    layout_sidebar(
      # sidebar feature
      height="600px", # does not seem to actively effect this
      sidebar = sidebar(
        position = "right",
        accordion(
          accordion_panel(
            "Add Points to Map",
            actionButton("addGapPoints", "Add records to the map")
          ),
          accordion_panel(
            "Select Buffer Size",
            selectInput("bufferSize",
                        "Select Buffer Distances in KM",
                        choices = c(1, 5, 10,20,50),
                        selected = 50)
          ),
          accordion_panel(
            "Run Gap Analysis",
            actionButton("createBuffersGap", "1. Create Buffers"),
            tags$br(),
            actionButton("generateGapMaps", "2. Create Gap Maps Layers"),
            tags$br(),
            actionButton("generateGapSummary", "3. Generate Summary Figure")
          ),
          accordion_panel(
            "Export Map",
            p("placeholder for future functionality"),
            actionButton("exportGapMap", "Download the current map")
          )
        )
      ),
      # main panel features
      card(
        card_header("Map of Gap Analysis Results"),
        leaflet::leafletOutput("map2"),
        # card_footer("Description of the map? ")
      )
    ),
    navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Results of the Gap Analysis",
        nav_panel(
          "Gap Analysis Results",
          plotlyOutput('gapAnalysisResults')
        ),
        nav_panel(
          "Input Data",
          DTOutput("mapTable2"),
        ),

    )
  ),## end gap analysis page
  ## landing page panel ------------------------------------------------------
  nav_panel(
    title = "About",
    # card(full_screen = TRUE,
    #      h3("Project Summary"),
    #      p(shinipsum::random_text(nwords = 100))
    # ),
    br(),
    tags$blockquote("The GAMMA tool will allow users to quantify and assess the completeness of recent collections made, as well as enable meta collection communities to assess the current ex situ conservation status and collection gaps across all participating collections.", cite = "Hadley Wickham"),
    br(),
    card(full_screen = TRUE,
         h2("Gap Analysis Method"),
         p("This version of the Gap Analysis conservation assessment evaluates the ex situ conservation status of a taxon, combines these metrics into an integrated assessment, and calculates an indicator metric that can be compared across taxa.",
         "The quantitative and spatial outputs demonstrate the state of conservation by highlighting where gaps in protection exist. The methods are fully described in Carver et al. (2021). Articles by Ramirez-Villegas et al. (2010), Castañeda-Álvarez and Khoury et al. (2016), and Khoury et al. (2019a, b; 2020))"),
        
         h3("Definitions of occurrence data categories"),
         p(strong("Germplasm Records (G)")," : Occurrences in which a living sample (via plant or seed) is present in an ",em("ex situ")," conservation system (i.e., botanical garden, seed bank, genebank, etc.). "),
         p(strong("Reference Records (H)")," : Occurrences that have a supporting herbarium or other reference record."),
         
         
         h3("Definitions of conservation gap analysis scores"),
        p(strong("The Sampling Representativeness Score ") ,em("ex situ"), "(SRS ex) calculates the ratio of germplasm accessions (G) available in ", em("ex situ")," repositories to reference (H) records for each taxon, making use of all compiled records irrespective of whether they include coordinates."),
        p(strong("The Geographic Representativeness Score ") ,em("ex situ"), "(GRS ex situ) uses 50-km-radius buffers created around each G collection coordinate point to estimate geographic areas already well collected within the potential distribution models of each taxon and then calculates the proportion of the potential distribution model covered by these buffers."),
        p(strong("The Ecological Representativeness Score ") ,em("ex situ"), "(ERS ex situ) calculates the proportion of terrestrial ecoregions (25) represented within the G buffered areas out of the total number of ecoregions occupied by the potential distribution model."),
        p(strong("The  Final Conservation Score "), em("ex situ")," (FCS ex situ) was derived by calculating the average of the three ",em("ex situ")," conservation metrics."),
    ),
         
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
    # fluidRow(
    #   p("Multiple logos?"),
    #   column(3,
    #          div(img(src="temp.png", alt="Logo 2", align="center", width="60%"), style="text-align:center")),
    #   column(3,
    #          div(img(src="temp.png", alt="Logo 2", align="center", width="60%"), style="text-align:center")),
    #   column(3,
    #          div(img(src="temp.png", alt="Logo 3", align="center", width="50%"), style="text-align:center")),
    #   column(3,
    #          div(img(src="temp.png", alt="Logo 4", align="center", width="40%"), style="text-align:center"))
    # ),
    br(),
    fluidRow(
      # p("Or a single logo?"),
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
    # fluidRow(
    #   column(3,
    #          strong("Alanta Botanical Garden"),
    #          p(tags$a(href = "https://atlantabg.org/article/emily-e-d-coffey-ph-d/", "Emily E. D. Coffey, Ph.D", target = "_blank")),
    #          p(tags$a(href = "https://atlantabg.org/article/jean-linsky-m-sc/", "Jean Linsky, M.Sc.", target = "_blank")),
    #          p("etc...")
    #   ),
    #   column(3,
    #          strong("Other organizations and people as needed"),
    #          p("Superstar 1"),
    #          p("Superstar 2")
    #   ),
    # ),
  ),
  nav_spacer(),

  ## Links -------------------------------------------------------------------
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
    # filter the data 
    genusData <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect))
    # define selector 
    selectInput("speciesSelect", "Select a species", choices = sort(genusData$specificEpithet), selected = )
  })
  # UI select variaty/subspec
  output$taxonRank = renderUI({
    # filter the data
    filteredData <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect)) |>
      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
    # define selector
    selectInput("taxonRank", "Select a taxon rank", choices = filteredData$taxonRank, selected = )
  })
  # UI select sub species feature
  output$speciesInfraspecific = renderUI({
    # filter the data
    filteredData2 <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect)) |>
      dplyr::filter(specificEpithet == as.character(input$speciesSelect))|>
      dplyr::filter(taxonRank == as.character(input$taxonRank))
      
    # define selector
    selectInput("speciesInfraspecific", "Select a infraspecific epithet", choices = filteredData2$infraspecificEpithet, selected = )
  })
  output$currentSpecies = renderText({
    name <- paste0("Current Taxon: ", input$genusSelect, " ",tolower(input$speciesSelect), " ")
    if(input$taxonRank != "species"){
      name <- paste0(name, tolower(input$taxonRank), " ", tolower(input$speciesInfraspecific))
    }
    # pass the object to render
    name
  })
  
  # pull the gbif id from the selection 
  ### might be nice to have a single generic function that takes input parameters for all selections. 
  output$gbiftaxonid <- renderText({
    f1 <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect)) |>
      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
    
    if(as.character(input$taxonRank) == "species"){
      f1 <- gbifBackbone |>
        dplyr::filter(genus == as.character(input$genusSelect)) |>
        dplyr::filter(specificEpithet == as.character(input$speciesSelect)) |>
        dplyr::filter(taxonRank == as.character(input$taxonRank))
      f1$taxonID[1]
    }else{
      f1 <- gbifBackbone |> 
        dplyr::filter(taxonRank == as.character(input$taxonRank))|>
        dplyr::filter(infraspecificEpithet == as.character(input$speciesInfraspecific)) 
      f1$taxonID[1]
    }
    
  })

  # Download data from GBIF -------------------------------------------------
  gbifData <- eventReactive(input$gbifPull, {

    if(as.character(input$taxonRank) == "species"){
      f1 <- gbifBackbone |>
        dplyr::filter(genus == as.character(input$genusSelect)) |>
        dplyr::filter(specificEpithet == as.character(input$speciesSelect)) |>
        dplyr::filter(taxonRank == as.character(input$taxonRank))
      taxonID <- f1$taxonID[1]
    }else{
      f1 <- gbifBackbone |> 
        dplyr::filter(taxonRank == as.character(input$taxonRank))|>
        dplyr::filter(infraspecificEpithet == as.character(input$speciesInfraspecific)) 
      taxonID <- f1$taxonID[1]
    }

    # define params
    initialPull <- query_gbif_occ(taxonkey = taxonID,
                   allow_synonyms_bool = TRUE)

    # filter to specific layers
    structureData <- etl_gbif_occ_data(gbif_occurrence_df = initialPull,
                                       valid_data_structure = valid_data_structure)

    structureData
  })
  
  # 
  output$gbifDownloadSpecifics <- renderText({
    if(is.null(gbifData())){
      "There is no data available on GBIF for this species"
    }else{
      paste0("The query returned ", nrow(gbifData()), " records.")
    }
  })


  # create spatial data for example/GBIF feature  -----------------------------------
  sp1 <- observeEvent(input$gbifToMap, {
    # generate spatial object 
    pointsVals <- gbifData() |>
      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
      dplyr::mutate(
        popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
                       "<br/><strong> Current Germplasm Type: </strong>", `Current Germplasm Type`,
                       "<br/><b>Collector Name:</b> ", Collector,
                       "<br/><b>Locality Description):</b> ", Locality),
        color = case_when(
          `Current Germplasm Type` == "H" ~ "#1184d4",
          `Current Germplasm Type` == "G" ~ "#6300f0"
        )
      )
    
    # update the map
    leafletProxy("map1")|>
      setView(lng = mean(pointsVals$Longitude), lat = mean(pointsVals$Latitude), zoom = 6)|>
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
        layerId = "GBIFlegend",
        colors = c("#1184d4","#6300f0"),
        labels = c("H","G"),
        title = "GBIF Data",
        opacity = 1,
        group = "GBIF"
      )
  })
  
  # create spatial data for uploaded data  -----------------------------------
 observeEvent(input$uploadToMap, {
    # generate spatial object 
    pointsVals2 <- dataUpload()|>
      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
      dplyr::mutate(
        popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
                       "<br/><strong> Type: </strong>", `Current Germplasm Type`,
                       "<br/><b>Collector Name:</b> ", Collector,
                       "<br/><b>Locality Description):</b> ", Locality),
        color = case_when(
          `Current Germplasm Type` == "H" ~ "#1184d4",
          `Current Germplasm Type` == "G" ~ "#6300f0"
        )
      )
    # spilt out the G and H points 
    gVals2 <- pointsVals2 |>
      dplyr::filter(`Current Germplasm Type` == "G")
    hVals2 <- pointsVals2 |>
      dplyr::filter(`Current Germplasm Type` == "H")
    # update the map 
    leafletProxy("map1")|>
      setView(lng = pointsVals2$Longitude[1], lat = pointsVals2$Latitude[1], zoom = 6)|>
      clearGroup(group = "Upload Dataset") |>
      addCircleMarkers(
        data = hVals2,
        color = ~color,
        stroke = TRUE,
        fillOpacity = 0.9,
        popup = ~popup,
        group = "Upload Dataset"
      ) |>
      addCircleMarkers(
        data = gVals2,
        color = ~color,
        stroke = TRUE,
        fillOpacity = 0.9,
        popup = ~popup,
        group = "Upload Dataset"
      ) |>
      # single legend for the GBIF features
      addLegend(
        position = "topright",
        layerId = "uploadlegend",
        colors = c("#1184d4","#6300f0"),
        labels = c("H","G"),
        title = "Upload Dataset",
        opacity = 1,
        group = "Upload Dataset"
      ) 
  })
  

  # Remove layers from map  -------------------------------------------------
  ## GBIF 
  observeEvent(input$removeGBIF, {
    leafletProxy("map1")|>
      clearGroup(group = "GBIF")|>
      removeControl(layerId = "GBIFlegend") # doesn't seem to be working at the moment. Mugh
      # not sure if I can remove the layer control element once it is added 
  })
  ## upload data
  observeEvent(input$removeUpload, {
    leafletProxy("map1")|>
      clearGroup(group = "Upload Dataset")|>
      removeControl(layerId = "uploadlegend") # doesn't seem to be working at the moment. Mugh
    # not sure if I can remove the layer control element once it is added 
  })

  # Export the table from data processing page  -----------------------------------------------
  # GBIF Data
  output$download1 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$genusSelect,"_",input$speciesSelect, "_data.csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(gbifData(), file, row.names = FALSE)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$genusSelect,"_",input$speciesSelect, "_data.csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(dataUpload(), file, row.names = FALSE)
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
  
  # compile dataset for the gap analysis page  ------------------------------
  ## popup to show that the compile dataset is working. ----
  observeEvent(input$compileDatasets, {
    # Show a modal when the button is pressed
    shinyalert(title = "Data is Moving",
               text = "Data is being gather and organized. Please continue your evaluation on the Gap Analysis page.",
               type = "success")
  })
  ## generate the gapAnalysus input dataset ----
  gapAnalysisInput <- reactive({
    d1 <- NA
    d2 <- NA
    d3 <- NA
    req(input$compileDatasets)
    d1 <- try(gbifData()|>
                dplyr::mutate(source = "GBIF",
                              "Accession Number" = as.character(`Accession Number`),
                              "Collection Date" = as.character(`Collection Date`)))
    d2 <- try(dataUpload()|>
                dplyr::mutate(source = "upload",
                              "Accession Number" = as.character(`Accession Number`),
                              "Collection Date" = as.character(`Collection Date`)))
    d3 <- try(dplyr::bind_rows(d1,d2))
    # both datasets 
    if("data.frame" %in% class(d3)){
      outputTable <- d3
    }
    # upload olny
    if("data.frame" %in% class(d2) && !"data.frame" %in% class(d1)){
      outputTable <- d2
    }
    #gbif only 
    if(!"data.frame" %in% class(d2) && "data.frame" %in% class(d1)){
      outputTable <- d1
    }
    outputTable
  })
  

  # intial map on gap analysis page -----------------------------------------
  
  gapPoints <-  reactive({
    # req(input$compileDatasets) ### transistion from using this as the que to sequentail outputs. 
    gapAnalysisInput()|>
      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
      dplyr::mutate(
        popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
                       "<br/><strong> Current Germplasm Type: </strong>", `Current Germplasm Type`,
                       "<br/><b>Collector Name:</b> ", Collector,
                       "<br/><b>Locality Description):</b> ", Locality,
                       "<br/><b>Data Source:</b> ", source),
        color = case_when(
          `Current Germplasm Type` == "H" ~ "#1184d4",
          `Current Germplasm Type` == "G" ~ "#6300f0"
        )
      )
  })
  ## add points to map ---
  observeEvent(input$addGapPoints, {
    # # generate spatial object 
    # spilt out the G and H points 
    gGap <- gapPoints() |>
      dplyr::filter(`Current Germplasm Type` == "G")
    hGap <- gapPoints() |>
      dplyr::filter(`Current Germplasm Type` == "H")
    if(nrow(gGap)==0){
      # update the map 
      leafletProxy("map2")|>
        setView(lng = mean(gapPoints()$Longitude), lat = mean(gapPoints()$Latitude), zoom = 6)|>
        addCircleMarkers(
          data = hGap,
          color = ~color,
          stroke = TRUE,
          radius = 5,
          fillOpacity = 0.9,
          popup = ~popup,
          group = "Reference Records"
        )|>
        # single legend for the GBIF features
        addLegend(
          position = "topright",
          layerId = "referencelegend",
          colors = c("#1184d4"),
          labels = c("H"),
          title = "Reference Records",
          opacity = 1,
          group = "Reference Records"
        )
    }else{
      # update the map 
      leafletProxy("map2")|>
        setView(lng = mean(gapPoints()$Longitude), lat = mean(gapPoints()$Latitude), zoom = 6)|>
        clearGroup("Reference Records")|>
        clearGroup("Germplasm Records")|>      
        addCircleMarkers(
          data = hGap,
          color = ~color,
          stroke = FALSE,
          radius = 5,
          fillOpacity = 0.9,
          popup = ~popup,
          group = "Reference Records"
        ) |>
        addCircleMarkers(
          data = gGap,
          color = ~color,
          stroke = FALSE,
          radius = 5,
          fillOpacity = 0.9,
          popup = ~popup,
          group = "Germplasm Records"
        ) |>
        # single legend for the GBIF features
        addLegend(
          position = "topright",
          layerId = "referencelegend",
          colors = c("#1184d4"),
          labels = c("H"),
          title = "Reference Records",
          opacity = 1,
          group = "Reference Records"
        ) |>
        addLegend(
          position = "topright",
          layerId = "germplasmalegend",
          colors = c("#6300f0"),
          labels = c("G"),
          title = "Germplasm Records",
          opacity = 1,
          group = "Germplasm Records"
        ) 
    }

  })

  # Gap analysis method  ----------------------------------------------------
  ## calculate the SRSex -- number of G points / total number of points 
  ##buffer the points 
  ## display the buffered objects on the map
  ## calculate the GRSex -- total G buffer area / total area 
  ## calculate the ERSex -- total number of ecorgions within the g buffer / total number of eco regions 
  ## display data on a chart 
  



  ## createBuffersGap --- 
  ## buffer points -----------------------------------------------------------
  pointsBuffer <- eventReactive(input$createBuffersGap, {
    gapPoints()|>
      terra::vect()|>
      terra::buffer(width = as.numeric(input$bufferSize) * 1000)
    })
  # aggregate buffers 
  aggregateBuffers <-  eventReactive(input$createBuffersGap, {
    pointsBuffer() |>
      terra::aggregate()
    })
  
  # g buffer object 
  ## sf objects because they are being added to the map 
  gGapBuffer <- eventReactive(input$createBuffersGap, {
    # g points 
    pointsBuffer()|>
      sf::st_as_sf()|>
      dplyr::filter(`Current Germplasm Type` == "G")
  })
  # h buffer object
  ## sf objects because they are being added to the map 
  hGapBuffer <- eventReactive(input$createBuffersGap, {
    pointsBuffer()|>
      sf::st_as_sf()|>
      dplyr::filter(`Current Germplasm Type` == "H")
    })
  
  ### update map with buffer features --- 
  observeEvent(input$createBuffersGap, {
    # aggregate buffer object to reduce complexity 
    hbuf <- hGapBuffer() |> 
      terra::vect() |>
      terra::aggregate()|>
      st_as_sf()
    gbuf <- gGapBuffer() |> 
      terra::vect() |>
      terra::aggregate()|>
      st_as_sf()
    
    
    if(nrow(gbuf)!=0){
      # update the map 
      leafletProxy("map2")|>        
        clearGroup("Buffers")|>
        removeControl(layerId = "bufferLegend") |>
        addPolygons(
          data = hbuf,
          group = "Buffers",
          color = "blue",
          fillOpacity = 0.9) |>
        addPolygons(
          data = gbuf,
          group = "Buffers",
          color = "purple",
          fillOpacity = 0.9)|>
        addLegend(
          position = "topright",
          layerId = "bufferLegend",
          colors = c("blue", "purple"),
          labels = c("Reference", "Germplasm"),
          title = "Buffers",
          opacity = 1,
          group = "Buffers")
    }else{
      leafletProxy("map2")|>        
        clearGroup("Buffers")|>
        removeControl(layerId = "bufferLegend") |>
        addPolygons(
          data = hbuf,
          group = "Buffers",
          color = "blue",
          fillOpacity = 0.9) |>
        addLegend(
          position = "topright",
          layerId = "bufferLegend",
          colors = c("blue"),
          labels = c("Reference"),
          title = "Buffers",
          opacity = 1,
          group = "Buffers")
    }
  })
  
  
  
  ## trimed ecoregion ---
  # crop the ecoregions to full buffer object
  allEcos <-  eventReactive(input$generateGapMaps, {
    ecoRegions |>
      terra::crop(aggregateBuffers())
  })
  # g area ecoregions 
  gEcos <- eventReactive(input$generateGapMaps, {
    if(nrow(gGapBuffer())>0){
      gVals <- terra::vect(gGapBuffer())|>
        terra::aggregate()
      # crop to g buffers 
      gEco <- allEcos() |>
        terra::crop(gVals)
    }else{
      gEco <- allEcos()[0,]
      }
    gEco
  })

  ## generete gap maps -------------------------------------------------------
  
  ## ers gap Map ----  
  ## return SF object because it's on the map 
  ersexGap <- eventReactive(input$generateGapMaps, {
    # get a unique list of values from eco buffers 
    allEco1 <- unique(allEcos()$ECO_ID)
    gEco1 <- unique(gEcos()$ECO_ID)
    
    # select ecos not in gEcos
    gEco1 <- allEco1[!allEco1 %in% gEco1]
    
    # select eco not in the gEcoLayer 
    gapEcos <- ecoRegions |>
      tidyterra::filter(ECO_ID %in% gEco1)
    # spatail object showing the ecoregions outside of the 
    gapEcos |> st_as_sf()
  })
  
  ## gap map ---  
  grsexGap <- eventReactive(input$generateGapMaps, {
    # format the buffer feature
    gGapBuffer1 <-  terra::vect(gGapBuffer()) |>
      terra::aggregate()
    # difference 
    gapBuffers2 <- aggregateBuffers() - gGapBuffer1
    # set to sf for display on map 
    gapBuffers2 |> st_as_sf()

  })


  ## gap analysis data to the map ----------------------------------------------------------
  ### update map with gap map features --- 
  observeEvent(input$generateGapMaps, {
        # update the map
        leafletProxy("map2")|>
          clearGroup("ERS gaps")|>
          clearGroup("GRS gaps")|>
          removeControl(layerId = "ersGapLegend") |>
          removeControl(layerId = "bufferLegend") |>
          # ers gap map layer
          addPolygons(
            data = ersexGap(),
            color = "grey",
            opacity = 0,
            popup = ~ECO_NAME,
            group = "ERS gaps",
            fillOpacity = 0.9,
            highlight = highlightOptions(
                        weight = 3,
                        fillOpacity = 0.4,
                        color = "black",
                        fillColor = "yellow",
                        opacity = 0.4,
                        bringToFront = TRUE,
                        sendToBack = TRUE)
          ) |>
          # grsex gap layer
          addPolygons(
            data = grsexGap(),
            color = "#d8b365",
            opacity = 0.2,
            group = "GRS gaps",
            fillOpacity = 0.4)|>
      addLegend(
            position = "topright",
            layerId = "ersGapLegend",
            labels = c("Uncollected Ecoregions"),
            title = "ERSex Gap",
            colors = c("grey"),
            opacity = 1,
            group = "ERS gaps"
          )|>
      addLegend(
        position = "topright",
        layerId = "grsGapLegend",
        labels = c("Areas outside of Gerplasm Buffer Range"),
        title = "GRSex Gap",
        colors = c("#d8b365"),
        opacity = 1,
        group = "GRS gaps"
      )
    #     # |>
    #       # single legend for the GBIF features
    #       # addLegend(
    #       #   position = "topright",
    #       #   colors = c("purple"),
    #       #   labels = c("G"),
    #       #   title = "Germplasm Records",
    #       #   opacity = 1,
    #       #   group = "G Buffers"
    #       # )
  })
 
  
  
  # observeEvent(input$createBuffersGap, {
  #   
  #   # ersmap 
  #   ersMap <- ersexGap()
  #   grsMap <- grsexGap()
  #   
  #   
  #   # pull points for an sf condition statement
  #   gGap <- gapPoints() |>
  #     dplyr::filter(`Current Germplasm Type` == "G")
  #   # gapAreas <- hGapBuffer - gGapBuffer
  #   if(nrow(gGap)==0){
  #     # update the map 
  #     leafletProxy("map2")|>        
  #       clearGroup("Buffers")|>
  #       clearGroup("ERS gaps")|>
  #       clearGroup("GRS gaps")|>
  #       removeControl(layerId = "bufferLegend") |>
  #       addPolygons(
  #         data = hGapBuffer,
  #         group = "Buffers",
  #         color = "blue",
  #         fillOpacity = 0.9
  #       )|>
  #       # ers gap map layer
  #       addPolygons(
  #         data = ersMap,
  #         color = "grey",
  #         opacity = 0.4,
  #         popup = ~ECO_NAME, 
  #         group = "ERS gaps",
  #         fillOpacity = 0.9
  #       ) |>
  #       # grsex gap layer 
  #       addPolygons(
  #         data = grsMap,
  #         color = "grey",
  #         opacity = 0.2, 
  #         group = "GRS gaps",
  #         fillOpacity = 0.4) |>
  #       # single legend for the GBIF features
  #       addLegend(
  #         position = "topright",
  #         layerId = "bufferLegend",
  #         colors = c("blue"),
  #         labels = c("H"),
  #         title = "Buffers",
  #         opacity = 1,
  #         group = "Buffers"
  #       )
  #   }else{
  #     # update the map 
  #     leafletProxy("map2")|>
  #       clearGroup("Buffers")|>
  #       clearGroup("ERS gaps")|>
  #       clearGroup("GRS gaps")|>
  #       removeControl(layerId = "bufferLegend") |>
  #       removeControl(layerId = "ersGapLegend") |>
  #       addPolygons(
  #         data = hGapBuffer,
  #         group = "Buffers",
  #         color = "blue",
  #         fillOpacity = 0.9
  #       )|>
  #       addPolygons(
  #         data = gGapBuffer,
  #         group = "Buffers",
  #         color = "purple",
  #         fillOpacity = 0.9
  #       )|>
  #       # ers gap map layer
  #       addPolygons(
  #         data = ersMap,
  #         color = "grey",
  #         opacity = 0.4,
  #         popup = ~ECO_NAME, 
  #         group = "ERS gaps",
  #         fillOpacity = 0.4,
  #         highlight = highlightOptions(
  #           weight = 3,
  #           fillOpacity = 0.4,
  #           color = "black",
  #           fillColor = "yellow",
  #           opacity = 0.4,
  #           bringToFront = TRUE,
  #           sendToBack = TRUE),)|>
  #       # grs gap map layer
  #       addPolygons(
  #         data = grsMap,
  #         color = "grey",
  #         opacity = 0.4, 
  #         group = "GRS gaps",
  #         fillOpacity = 0.8) |>
  #       # legend
  #       addLegend(
  #         position = "topright",
  #         layerId = "bufferLegend",
  #         colors = c("blue", "purple"),
  #         labels = c("Reference", "Germplasm"),
  #         title = "Buffers",
  #         opacity = 1,
  #         group = "Buffers"
  #       )|>
  #       addLegend(
  #         position = "topright",
  #         layerId = "ersGapLegend",
  #         labels = c("Uncollected Ecoregions"),
  #         title = "ERSex Gap",
  #         colors = c("grey"),
  #         opacity = 1,
  #         group = "ERS gaps"
  #       )
  #     # |>
  #       # single legend for the GBIF features
  #       # addLegend(
  #       #   position = "topright",
  #       #   colors = c("purple"),
  #       #   labels = c("G"),
  #       #   title = "Germplasm Records",
  #       #   opacity = 1,
  #       #   group = "G Buffers"
  #       # )
  #   }
  # })
  
  
  ## generateGapSummary --- 
  ## srs ex ------------------------------------------------------------------
  srsex <- eventReactive(input$generateGapSummary,{
    # select g points
    gGap <- gapPoints() |>
      dplyr::filter(`Current Germplasm Type` == "G")
    # calculate score 
    gs <- nrow(gGap)
    if(gs == 0){
      srsScore <- 0
    }else(
      srsScore <- (nrow(gGap)/ nrow(gapPoints()))*100
    )
    # return score
    srsScore
  })
  ## GRSex  ------------------------------------------------------------------
  grsex <- eventReactive(input$generateGapSummary, {
    # total area
    totalArea <- aggregateBuffers() |>
      terra::expanse(unit="km")
    # calculate areas outside of gbuffer 
    gapArea <- grsexGap()  |>
      terra::vect() |>
      terra::expanse(unit="km")
    #calculate GRSex score 
    ## total - gap area give the area covered by G buffers. 
    difference <- totalArea - gapArea
    if(difference == 0){
      grsScore <- 0
    }else{
      grsScore <- ((totalArea - gapArea)/totalArea)*100   
    }
    grsScore
    
  })
  
  ## ERSex -------------------------------------------------------------------
  ersex <- eventReactive(input$generateGapSummary, {
    
    #calculate ersex score 
    gs <- nrow(gEcos())
    if(gs == 0){
      ersScore <- 0
    }else(
      ersScore <- (nrow(gEcos())/nrow(allEcos()))*100
    )
    ersScore
  })
  ### render gap analysis plot ------------------------------------------------
    gapAnalysisResultsFigure<- eventReactive(input$generateGapSummary,{
      # define the base table 
      df <- data.frame(class = c(
        "Sampling Representativeness Score",
        "Ecological Representativeness Score",
        "Geographic Representativeness Score", 
        "Final Representativeness Score"
      ),
      score = c(0,0,0,0))
      # assign values based on the presence of specific output values 
      df$score[1] <- try(srsex())
      df$score[2] <- try(ersex())
      df$score[3] <- try(grsex())
      # assign the fcsex score 
      df$score[4] <- try(mean(df[1:3, "score"], na.rm = TRUE))
      
      # assign color based on the score 
      df <- df|>
        dplyr::mutate(rank = case_when(
          score <= 25 ~ "Urgent Priority",
          score > 25 & score <= 50 ~ "High Priority", 
          score > 50 & score <= 75 ~"Medium Priority",
          score > 75 ~ "Low Priority"
        ), 
        colors = case_when(
          rank == "Urgent Priority" ~ "#ffb4b3",
          rank ==  "High Priority" ~ "#ffd380",
          rank == "Medium Priority"~ "#ffff80",
          rank == "Low Priority" ~ "#a8d2a8"
        )
      )
      #define the display order of the plot
      xform <- list(categoryorder = "array",
                    categoryarray = c("Sampling Representativeness Score",
                                      "Ecological Representativeness Score",
                                      "Geographic Representativeness Score", 
                                      "Final Representativeness Score"))
      
      # generate a plotly figure 
      fig <- plot_ly(
        data = df, 
        x = ~class,
        y = ~score,
        marker = list(color = c(df$colors)),
        type = "bar"
      )|>
        layout(title = "Gap Analysis Ex Situ Conservation Summary",
              xaxis = xform,
              yaxis = list(title = "", 
                           range = c(0,100))
              )
    # print figure 
      fig
    }
  )
  output$gapAnalysisResults <- renderPlotly(gapAnalysisResultsFigure())

# poorly ordered renders --------------------------------------------------
  output$map1 <- leaflet::renderLeaflet(map1)
  output$mapTableGBIF <- renderDT(gbifData())
  output$mapTableUpload <- renderDT(dataUpload())
  
  output$image <- renderImage(random_image())
  output$map2<- leaflet::renderLeaflet(map2)
  output$mapTable2 <- renderDT(gapAnalysisInput()) # shinipsum::random_DT(nrow = 10, ncol = 10))

}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

