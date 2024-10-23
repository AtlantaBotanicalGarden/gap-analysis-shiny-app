
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
              textInput("issueCodes",
                        label = tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit#gid=1735583299",
                                       "View GBIF Issue Codes ", target = "_blank"), "see link below for options"),
              tags$br(),
            )
          ),
          accordion_panel(
            "Upload Your own data",
            p(tags$a(href = "https://docs.google.com/spreadsheets/d/1BeDUBCg2BJ1DYpW47bxYH8p8CJAVqgM5rafyz81RoOc/edit?usp=sharing", "View Data Format Example", target = "_blank")),
            fileInput("upload", "Upload a file"),
            tags$p("Testing for expected column names"),
            textOutput("validateColNames"),
          ),
          accordion_panel(
            "Add data to the map",
            actionButton("uploadToMap", "Add to map")
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
      title = "Tabular Data of the Records",
      nav_panel(
        title = "Map",
        leaflet::leafletOutput("map1"),
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
        DTOutput("mapTableUpload")),
        downloadButton("download2", "Download the Uploaded data")
      ),
      nav_panel( # Editable data format, this is what will be displayed on the map and transfered to the gap analysis 
        title = "Editable/Map Data",
        shinycssloaders::withSpinner(
        rHandsontableOutput("mapTableUploadEdit")),
        downloadButton("download3", "Download the full/edited data")
      )
    )
    
    # navset_card_tab(
    #   height = 450,
    #   full_screen = TRUE,
    #   title = "Tabular Data of the Records",
    #   nav_panel(
    #     "GBIF_table",
    #     downloadButton("download1", "Download the GBIF data"),
    #     card_title("GBIF Records"),
    #     shinycssloaders::withSpinner(
    #       DTOutput("mapTableGBIF")),
    #   ),
    #   nav_panel(
    #     "Uploaded Data",
    #     downloadButton("download2", "Download the Uploaded data"),
    #     card_title("Uploaded Records"),
    #     DTOutput("mapTableUpload"),
    #   ),
    #   nav_panel(
    #     "example editable table",
    #     downloadButton("download3", "Download the Uploaded data"),
    #     card_title("Edit Uploaded Records"),
    #     rHandsontableOutput("mapTableUploadEdit"),
    #   )
    )
}


