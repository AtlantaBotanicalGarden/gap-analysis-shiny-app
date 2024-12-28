
dataEvaluationPage <- function(){
  ## Data Evaluation ---------------------------------------------------------
    
    
    # define row for containing the map feaut
    # defeine the sidebar element
    layout_sidebar(
      height = "700px",
      # sidebar feature
      sidebar = sidebar(
        position = "left",
        width = "20%",
        # primary accordion structure
        accordion(
          open = FALSE,
          # GBIF panel set
          accordion_panel(
            title = "Download GBIF data ",
            accordion_panel(
              multiple = FALSE,
              # user selects the geneus
              title = "Select Taxon",
              # print the current taxon name
              textOutput("currentSpecies"),
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
              uiOutput("speciesInfraspecific")
            ),
            tags$br(),
            accordion_panel(
              multiple = FALSE,
              title = "Default Download",
              tags$strong("GBIF Taxon ID:"),  textOutput("gbiftaxonid"),
              checkboxInput("allowSyn", "Allow Synonyms in Data", TRUE),
              # p(tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit#gid=1735583299", "View GBIF Issue Codes ", target = "_blank")),
              actionButton("gbifPull", "Download Data from Gbif"),
              "Once download the details of your request will be shown below.",
              tags$strong("Download Details:"),  textOutput("gbifDownloadSpecifics")
              ),
            accordion_panel(
              multiple = FALSE,
              title = "Advanced Settings - optional",
              "Select the number of GBIF records you wish to consider. A maxium of 1000 can be used.",
              numericInput("numberGBIFDownload",
                           "Number of records:", 200, min = 1, max = 1000),
              tags$br(),
              "Data can be filter by the collection year as reported by the observer.",
              ## add a selector to uses as a filter for the year range 
              checkboxInput(
                inputId = "useYear",
                label = strong("Toggle box to use year range"),
                value = FALSE
              ),
              "The starting year must be equal or less than the ending year",
              # data range selector 
              numericInput( 
                "startYear", 
                "Starting Year", 
                value = 2020, 
                min = 1900, 
                max = 2024 
              ), 
              numericInput( 
                "endYear", 
                "Ending Year", 
                value = 2020, 
                min = 1900, 
                max = 2024 
              ), 
            )
          ),
          accordion_panel(
            "Upload Your own data",
            p(tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit?usp=sharing", "View Data Format Example", target = "_blank")),
            fileInput("upload", "Upload a file"),
            tags$p("Please check that follow the column names in your uploaded dataset match the example dataset"),
            textOutput("validateColNames"),
          ),
          accordion_panel(
            "Update the data on the map",
            actionButton("updateCombinedTable", "Update")
          ),
          accordion_panel(
            "Remove Data from Map",
            "Select button below to remove all point from within your drawn area",
            actionButton("removeSelection", "Remove selection")
          ),
          accordion_panel(
            "Finalize data for gap analysis",
            "Select the button below with gather all GBIF and uploaded datasets and add them to the Gap Analysis Map.",
            actionButton("compileDatasets", "Compile Data For Gap Analysis")
          ),
        )# end of accordion 
      ), # end of sidebar 
      # main panel features

    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Map and Data",
      layout_column_wrap(
        height = 600,
        card(
          title = "Map",
          leaflet::leafletOutput("map1"),
        ),
        navset_card_tab( # Editable data format, this is what will be displayed on the map and transfered to the gap analysis 
          nav_panel(
            title = "Editable/Map Data",
            shinycssloaders::withSpinner(
              rHandsontableOutput("mapTableUploadEdit")),
          downloadButton("downloadCombined", "Download the full/edited data")
          ),
          nav_panel( # this provides a chance to view the material specifically from GBIF
            "Original Data from GBIF",
            shinycssloaders::withSpinner(
              DTOutput("mapTableGBIF")),
            downloadButton("download1", "Download the GBIF data")
          ),
          nav_panel( # view the material from the upload GBIF
            "Original Uploaded Records",
            shinycssloaders::withSpinner(
              rHandsontableOutput("mapTableUpload")),
            downloadButton("downloadUpload", "Download the Uploaded data")
          ),
          nav_panel( # Editable data format, this is what will be displayed on the map and transfered to the gap analysis
            title = "test print",
            verbatimTextOutput("testPrint")
          )
        )
      ),
    ),
  ) # end of side bar

  
}


