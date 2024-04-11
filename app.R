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
library(shinycssloaders)
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
map2 <- map1

expectedNames <- c("UID",	"taxon",	"genus",	"species",	"type",	"year",	"latitude",	"longitude",	"locality",	"collector")



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
              uiOutput("speciesInfraspecific"),
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
              "Select the botton below with gather all GBIF and uploaded datasets and add them to the Gap Analysis Map.",
              actionButton("compileDatasets", "Compile Data For Gap Analysis")
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
    navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Tabular Data of the Records",
        nav_panel(
          "GBIF_table",
          downloadButton("download1", "Download the GBIF data"),
          card_title("GBIF Records"),
          DTOutput("mapTableGBIF"),
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
        actionButton("exportGapTable", "Download the current table")
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
    ####genusPick = input$genusSelect
    ####speciesPick = input$speciesSelect
    
    # filter the data
    filteredData <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect)) |>
      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
    # define selector
    selectInput("taxonRank", "Select a taxon rank", choices = filteredData$taxonRank, selected = )
  })
  # UI select sub species feature
  output$speciesInfraspecific = renderUI({
    
    # grab the selection
    ###genusPick = input$genusSelect
    ###speciesPick = input$genusSelect
    ###infraPick = input$genusSelect
    
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
        dplyr::filter(infraspecificEpithet == as.character(input$speciesInfraspecific)) #|>
        #dplyr::select(taxonID)|>
        #dplyr::pull()
      f1$taxonID[1]
    }
      
  })

  # Download data from GBIF -------------------------------------------------
  gbifData <- eventReactive(input$gbifPull, {
    ### repeated method from print render 
    f1 <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect)) |>
      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
    
    if(as.character(input$taxonRank) == "species"){
      f2 <- f1$taxonID[1] # issue 
    }else{
      f2 <- f1 |> 
        dplyr::filter(taxonRank == as.character(input$taxonRank))|>
        dplyr::filter(taxonRank == as.character(input$speciesIntrfraspecific))|>
        dplyr::select(taxonID)|>
        dplyr::pull()
      
    }
    
    # define params 
    initialPull <- query_gbif_occ(taxonkey = f2,
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
    # spilt out the G and H points 
    gVals2 <- pointsVals2 |>
      dplyr::filter(type == "G")
    hVals2 <- pointsVals2 |>
      dplyr::filter(type == "H")
    # update the map 
    leafletProxy("map1")|>
      setView(lng = pointsVals2$longitude[1], lat = pointsVals2$latitude[1], zoom = 6)|>
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
  mapData <- data.frame(matrix(nrow = 1, ncol = length(expectedNames)+1))
  names(mapData) <- c(expectedNames, "source")
  
  gapAnalysisInput <- eventReactive(input$compileDatasets, {
    # test for presences of data
    if(!is.null(gbifData())){
      d1 <- gbifData()|>
        dplyr::mutate(source = "GBIF")
      mapData <- bind_rows(mapData, d1) # I don't think this is going to work super well but let's run with it. 
    }
    #upload 
    if(!is.null(gbifData())){
      d2 <- dataUpload()|>
        dplyr::mutate(source = "upload")
      mapData <- bind_rows(mapData, d2)
    }
    mapData
  })
  
  # 
  output$map1 <- leaflet::renderLeaflet(map1)
  output$mapTableGBIF <- renderDT(gbifData())
  output$mapTableUpload <- renderDT(dataUpload())
  
  output$image <- renderImage(random_image())
  output$map2<- leaflet::renderLeaflet(map2)
  output$mapTable2 <- renderDT(gapAnalysisInput())

}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

