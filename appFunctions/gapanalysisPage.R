

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
            actionButton("createBuffersGap", "1. Run Gap Analysis"),
          ),
          accordion_panel(
            "Export Map",
            p("Once gap analysis is complete, select link to download report. Reports will appear in computers download folder as a .html file."),
            downloadLink("downloadReport", "Download Report")
          )
        )
      ),
      # main panel features
      card(
        card_header("Map of Gap Analysis Results"),
        leaflet::leafletOutput("map2"),
        # card_footer("Description of the map? ")
        # Text output area below the map
        htmlOutput("map_text_output")
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