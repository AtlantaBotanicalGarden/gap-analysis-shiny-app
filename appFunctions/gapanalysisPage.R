

gapAnalysisPage <- function(){
  fluidPage(
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
      ),
    ),
    navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Results of the Gap Analysis",
        nav_panel(
          "Input Data",
          DTOutput("mapTable2"),
        ),
        nav_panel(
          "Gap Analysis Results",
          plotlyOutput('gapAnalysisResults')
        ),

      )
  )
}