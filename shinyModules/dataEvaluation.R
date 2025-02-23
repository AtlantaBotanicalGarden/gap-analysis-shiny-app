
# define ui ---------------------------------------------------------------
dataEvaluation <- function(id){
  ns <- NS(id)
  shiny::tagList( # ensures that correct html returns. Functions as a list. 
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
              inputId = ns("genusSelect"),
              label = "Genus: ",
              choices = unique(gbifBackbone$genus),
              selected = "Magnolia"),
            # list of subspecies is generate for selection 
            uiOutput(ns("speciesSelect")),
            # when possible the option for var subsp is listed
            uiOutput(ns("taxonRank")),
            # final filter of sub species it provided 
            uiOutput(ns("speciesInfraspecific")),
            # print the current taxon name 
            textOutput(ns("currentSpecies"))
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
            fileInput(inputId = "upload",
                      label = "Upload a file",
                      multiple = FALSE,
                      accept = ".csv"),
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
      navset_card_tab(
        height = 600,
        full_screen = TRUE,
        title = "Map and Table",
        nav_panel(
          "Map",
          card_title("Map of Downloaded and Uploaded Datasets"),
          leafletOutput(ns("map1"))
        ),
        nav_panel(
          "Table",
          card_title("Table of Downloaded and Upload Datasets"),
        ),
        nav_panel(
          shiny::icon("circle-info"),
          markdown("A space for some information about these plots")
        )
      )
    )
    #   # main panel features
    #   card(
    #     card_header("Map of Downloaded and Uploaded Datasets"),
    #     leaflet::leafletOutput("map"),
    #     # card_footer("Description of the map? ")
    #   )
    # ),
  )
}


# define server  ---------------------------------------------------------- 
dataEvaluation_server <- function(id, map, gbifBackbone){
  
  moduleServer(id,function(input,output,session){
    
    # UI select Subspecies 
    output$speciesSelect = renderUI({
      # filter the data 
      genusData <- gbifBackbone |>
        dplyr::filter(genus == as.character(input$genusSelect))
      # define selector 
      selectInput(inputId = "speciesSelect", 
                  label = "Select a species",
                  choices = sort(genusData$specificEpithet), 
                  selected = )
    })
    # UI select variaty/subspec
    output$taxonRank = renderUI({
      # filter the data
      filteredData <- gbifBackbone |>
        dplyr::filter(genus == as.character(input$genusSelect)) |>
        dplyr::filter(specificEpithet == as.character(input$speciesSelect))
      # define selector
      selectInput(inputId = "taxonRank", 
                  label = "Select a taxon rank", 
                  choices = filteredData$taxonRank,
                  selected = )
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
    
    
    # render the initial map 
    output$map1 <- leaflet::renderLeaflet({map})
    }
  )
}