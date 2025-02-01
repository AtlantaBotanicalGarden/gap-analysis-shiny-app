###
# draft material of the Gap Analysis and Metacollection Management tool
# carverd@colostate.edu
# 20240225
### 
library(shiny)
library(bslib)
library(ggplot2)
# library(shinipsum)
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
library(leaflet.extras)
library(rhandsontable)


# source functions  -------------------------------------------------------
lapply(list.files(
  path = "appFunctions/",
  pattern = ".R",
  full.names = TRUE),
  source)

source("utilities/loadGlobalVariables.R")

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
    id = "main_navbar",
    title = "Gap Analysis and Metacollection Management",
    bg ="#52b788",
    window_title = "GAMMA Tool",
    position = "static-top",
    # set some space between the Application title and the tab selectors
    nav_spacer(),
  # ## Data Evaluation ---------------------------------------------------------
    nav_panel( # have to define the 'nav' elements outside of the function
      title = "Data Evaluation",
      # all the ui elements store in appFunction 
      dataEvaluationPage(),
    ),
  ## Gap Analysis -------- 
    nav_panel(
      title = "Gap Analysis",
      gapAnalysisPage()
    ),
    nav_panel(
      title = "About",
      aboutPage(),
    ),
    nav_spacer(),
    # reference links
    links()
  )## end page_navbar
)## end ui
  

# server ----------------------------------------------------------------
server <- function(input, output, session) {
  #
  # the server elements for fit as well inside the functions. This probably has
  # to do with the namespace and modules... leave it be for now and look to organize 
  # code that works into functions for simplicity when possible. 
  #

  # initalize the map -------------------------------------------------------
  output$map1 <- leaflet::renderLeaflet(generateMap1())
  
  # GBIF accodion and data processing -----------------------------------------------------------
  ## taxon select --- 
  ### UI select Subspecies
  output$speciesSelect = renderUI({
    # filter the data
    genusData <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect))
    # define selector
    selectInput("speciesSelect", "Select a species", 
                choices = sort(genusData$specificEpithet),
                selected = )
  })
  ### UI select variaty/subspec
  output$taxonRank = renderUI({
    # filter the data
    filteredData <- gbifBackbone |>
      dplyr::filter(genus == as.character(input$genusSelect)) |>
      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
    # define selector
    selectInput("taxonRank", "Select a taxon rank",
                choices = filteredData$taxonRank, selected = )
  })
  #### UI select sub species feature
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
  ## pull the gbif id from the selection
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
  
  
  # Reactive value to store the uploaded data
  gbifData <- reactiveVal(NULL)

  ## Download data from GBIF
  observeEvent(input$gbifPull, {
   # define all input variables
   ## taxon key
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
   # synonym
   synonym <- input$allowSyn
   # download limit
   downloadLimit <- input$numberGBIFDownload
   # issues
   issueCodes <- input$issueCodes
   # year
   if(input$useYear){
     year <- c(input$startYear,input$endYear)
   }else{
     year <- NULL
   }
 #   
 #   
 #   # define params
     initialPull <- query_gbif_occ(taxonkey = taxonID,
                    allow_synonyms_bool = synonym,
                    limit = downloadLimit,
                    year = year,
                    # issues_not_allowed = issueCodes
                    )
     # force character to joins later on.
     initialPull$`Accession Number` <- as.character(initialPull$`Accession Number`)
     initialPull$source <- "GBIF"
     initialPull$index <- seq(from = 20000, to = 20000+nrow(initialPull)-1, by = 1 )

     gbifData(initialPull)
   })
 # 
 #  ## Produce a text output to show the results 
  output$gbifDownloadSpecifics <- renderText({
    if(is.null(gbifData())){
      "There is no data available on GBIF for this species"
    }else{
      paste0("The query returned ", nrow(gbifData()), " records.")
    }
  })
 # 
 # 
 #  ## GBIF Data
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
 #  
 #  ## display in table
 #  # Render the rhandsontable (only if data is available)
  output$mapTableGBIF <- renderRHandsontable({
    req(gbifData()) # Require data before rendering
    rhandsontable(gbifData()) # dropping the source and index from the display?
  })
 #  
 #  # Render the map (only if data is available)
  observe({
    req(gbifData()) # Require data before rendering
    # generate point objects
    gbifPoints <- createSpatialObject(gbifData())|>
      dplyr::mutate(
        color = case_when(
          `Current Germplasm Type` == "H" ~ gbifColor[1],
          `Current Germplasm Type` == "G" ~ gbifColor[2]
        )
      )
    labels <- lapply(gbifPoints$popup, htmltools::HTML)

    # update map
    leafletProxy("map1")|>
      setView(lng = mean(gbifPoints$Longitude), lat = mean(gbifPoints$Latitude), zoom = 6)|>
      addCircleMarkers(
        data = gbifPoints,
        layerId = ~index,
        group = "GBIF",
        radius = 4,
        color = "white",
        fillColor = ~color,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = labels
      )
  })
 #  # 
 #  # Observe changes in the table
  observeEvent(input$mapTableGBIF, {
    gbifData(hot_to_r(input$mapTableGBIF))

    gbifPoints <- createSpatialObject(gbifData())|>
      dplyr::mutate(
        color = case_when(
          `Current Germplasm Type` == "H" ~ gbifColor[1],
          `Current Germplasm Type` == "G" ~ gbifColor[2]
        )
      )
    labels <- lapply(gbifPoints$popup, htmltools::HTML)

    # update map
    leafletProxy("map1")|>
      setView(lng = mean(gbifPoints$Longitude), lat = mean(gbifPoints$Latitude), zoom = 6)|>
      addCircleMarkers(
        data = gbifPoints,
        layerId = ~index,
        group = "GBIF",
        radius = 4,
        color = "white",
        fillColor = ~color,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = labels
      )
  })
 #  # Reactive values to store selected markers
  selectedGBIF <- reactiveValues(markers = NULL)

  # INDIVIDUAL POINT SELECTION
  # Observe marker clicks
  observeEvent(input$map1_marker_click, {
    click <- input$map1_marker_click
    print(click)
    # need a condition to prevent triggering from the upload selection
    if(click$group == "GBIF" || click$group == "GBIF Selection"){
      # # regenerateing the spatial data to try to resolve the removal of the inital layer on select
      gbifData(hot_to_r(input$mapTableGBIF))
      
      gbifPoints <- createSpatialObject(gbifData())|>
        dplyr::mutate(
          color = case_when(
            `Current Germplasm Type` == "H" ~ gbifColor[1],
            `Current Germplasm Type` == "G" ~ gbifColor[2]
          )
        )
      labels <- lapply(gbifPoints$popup, htmltools::HTML)
    }
 

    if(click$group == "GBIF"){
      # add it to selected markers
      selectedGBIF$markers <- c(selectedGBIF$markers, click$id)
      # edit map
      leafletProxy("map1") %>%
        addCircleMarkers(
          data = gbifPoints[gbifPoints$index == click$id, ],
          layerId = ~index,
          radius = 4,
          color = "red",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "GBIF Selection"
        )
    }
    if(click$group == "GBIF Selection"){
      # remove the item from selection
      selectedGBIF$markers <- selectedGBIF$markers[selectedGBIF$markers != click$id]
      # update the map
      leafletProxy("map1") %>%
        leaflet::clearGroup("GBIF Selection") |>
        addCircleMarkers(
          data = gbifPoints[gbifPoints$index %in% selectedGBIF$markers, ],
          layerId = ~index,
          radius = 4,
          color = "red",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "GBIF Selection"
        )
    }
  })
  
  
  # uploaded dataset processing -----------------------------------------------
  
  ## add a download functionality
  output$downloadUpload <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$genusSelect,"_",input$speciesSelect, "_uploaded_data.csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(dataUpload(), file, row.names = FALSE)
    }
  )
  ## testing column headers 
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
      print(nonMatchedNames)
    }
  })
  # Reactive value to store the uploaded data
  dataUpload <- reactiveVal(NULL)
  
  observeEvent(input$upload, {
    inFile <- input$upload
    if(is.null(inFile)){
      return(NULL)
    }
    # Read the uploaded CSV file
    uploaded_data <- vroom::vroom(inFile$datapath, delim = ",")
    
    # assign id as character 
    uploaded_data$`Accession Number` <- as.character( uploaded_data$`Accession Number`)
    uploaded_data$index <- seq(from = 1000, to = 1000 +nrow(uploaded_data) - 1, by = 1)
    uploaded_data$source <- "upload"
    # Update the reactive value with the uploaded data
    dataUpload(uploaded_data)    
  })
  
  ## display in table
  # Render the rhandsontable (only if data is available)
  output$mapTableUpload <- renderRHandsontable({
    req(dataUpload()) # Require data before rendering
    rhandsontable(dataUpload())
  })
  
  # Render the map (only if data is available)
  observe({
    req(dataUpload()) # Require data before rendering
    # generate point objects 
    uploadPoints <- createSpatialObject(dataUpload())|>
          dplyr::mutate(
            color = case_when(
              `Current Germplasm Type` == "H" ~ uploadColor[1],
              `Current Germplasm Type` == "G" ~ uploadColor[2]
            )
          )
    labels <- lapply(uploadPoints$popup, htmltools::HTML)
    
    # update map 
    leafletProxy("map1")|>
      setView(lng = mean(uploadPoints$Longitude), lat = mean(uploadPoints$Latitude), zoom = 6)|>
      addCircleMarkers(
        data = uploadPoints,
        layerId = ~index,
        group = "Upload",
        radius = 4,
        color = "white",
        fillColor = ~color,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = labels
        )
  })
  
  # Observe changes in the table
  observeEvent(input$mapTableUpload, {
    dataUpload(hot_to_r(input$mapTableUpload))
    
    uploadPoints <- createSpatialObject(dataUpload())|>
      dplyr::mutate(
        color = case_when(
          `Current Germplasm Type` == "H" ~ uploadColor[1],
          `Current Germplasm Type` == "G" ~ uploadColor[2]
        )
      )
    labels <- lapply(uploadPoints$popup, htmltools::HTML)
    
    # update map 
    leafletProxy("map1")|>
      setView(lng = mean(uploadPoints$Longitude), lat = mean(uploadPoints$Latitude), zoom = 6)|>
      addCircleMarkers(
        data = uploadPoints,
        layerId = ~index,
        group = "Upload",
        radius = 4,
        color = "white",
        fillColor = ~color,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = labels
      )
  })
  # Reactive values to store selected markers
  selectedUpload <- reactiveValues(markers = NULL)

  # INDIVIDUAL POINT SELECTION
  # Observe marker clicks
  observeEvent(input$map1_marker_click, {
    click <- input$map1_marker_click

    print(click$id)
    print(click$group)
    # define local varaible
    uploadPoints <- dataUpload()
    if(click$group == "Upload"){
      # add it to selected markers
      selectedUpload$markers <- c(selectedUpload$markers, click$id)
      # edit map
      leafletProxy("map1") %>%
          addCircleMarkers(
            data = uploadPoints |>
              dplyr::filter(index == uploadPoints$index[click$id]) ,
            layerId = ~index,
            radius = 4,
            color = "red",
            fillOpacity = 0.8,
            stroke = FALSE,
            group = "Upload Selection"
          )
    }
    if(click$group == "Upload Selection"){
      # remove the item from selection
      selectedUpload$markers <- selectedUpload$markers[selectedUpload$markers != click$id]
      # update the map
      leafletProxy("map1") %>%
        leaflet::clearGroup("Upload Selection") |>
        addCircleMarkers(
          data = uploadPoints[uploadPoints$index %in% selectedUpload$markers, ],
          layerId = ~index,
          radius = 4,
          color = "red",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "Upload Selection"
        )
    }
  })

  
  
  # combined table---------------------------------------------
  ## take the selected data from both groups and use that to filter the two dataframes 
  
  combined_data <- reactiveVal(NULL)
  
  
  # Combine the data sets only when the button is clicked
  observeEvent(input$compileDatasets, {
    if (!is.null(gbifData()) && !is.null(dataUpload())) {
      # Both exist: combine
        combined <- bind_rows(gbifData(), dataUpload())
        print("both datasets 1")
      # }, error = function(e) {
      #   showNotification(paste("Error combining data:", e$message), type = "error")
      #   NULL
      # })
    } else if (!is.null(gbifData())) {
      # Only data1 exists
      combined <- gbifData()
      print("gbif only 1")
    } else if (!is.null(dataUpload())) {
      # Only data2 exists
      combined <- dataUpload()
      print("upload only 1")
    } else {
      # Neither exists
      NULL
    }
    # returned a filtered dataset based on the map selection 
    if(!is.null(combined)){
      ## add the filtering data set back in 
      uploadSelection <- NULL
      gbifSelection <- NULL
      print("filter on selection ")
      if(!is.null(dataUpload())){
        uploadSelection <- dataUpload()$index[selectedUpload$markers]
        }
      if(!is.null(gbifData())){
        # g1 <- gbifData()
        gbifSelection <- gbifData()$index[selectedGBIF$markers]
        print(gbifSelection)
      }
      
      if(!is.null(uploadSelection) && !is.null(gbifSelection)){
        print("Both selection 2")
        
        # drop values before combining 
        selectAccession <- c(uploadSelection,gbifSelection )
        # drop accessions  
        combined2 <- combined[!combined$index%in% selectAccession, ]
        # generate object 
        combined_data(combined2)
      }else if(is.null(uploadSelection) && !is.null(gbifSelection)){
        print("gbif selection 2 ")
        # drop accessions  
        combined2 <- combined[!combined$index%in% gbifSelection, ]
        # generate object 
        combined_data(combined2)
      }else if(!is.null(uploadSelection) && is.null(gbifSelection)){
        print( "upload selection 2")
        # drop accessions  
        combined2 <- combined[!combined$index%in% uploadSelection, ]
        # generate object 
        combined_data(combined2)
      }else{
        print("no selection 2")
        # no selection 
        combined_data(combined)
      }
      # generate object 
      # print(combined_data)
    }else{
      combined_data(combined)
    }
  })
  # develop the popup 
  observeEvent(input$compileDatasets, {
    # print(combined_data())
    Sys.sleep(1)
    ## show the popup selection
    showModal(
      modalDialog(
        title = "Choose an Action",
        "What would you like to do?",
        footer = tagList(
          downloadButton("download_csv", "Download your data"),
          actionButton("continue", "Continue to gap analysis"),
          actionButton("return", "Return to data evaluation")
        ),
        size = "l",
        easyClose = FALSE
      )
    )
  })
  
  
  # Switch to Page 2 when "Continue" is clicked
  observeEvent(input$continue, {
    updateNavbarPage(session, "main_navbar", selected = "Gap Analysis")
    removeModal() # Remove the modal dialog
  })
  # return to Page 1 when "Return" is clicked
  observeEvent(input$return, {
    removeModal() # Remove the modal dialog
  })
  
  # download the combined data 
  output$download_csv <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      taxon <- combined_data()$`Taxon Name`
      paste0(taxon,"_combined_data.csv")
    },
    content = function(file2) {
      export <- combined_data()
      # print(nrow(export))
      # Write the dataset to the `file` that will be downloaded
      write.csv(export, file2, row.names = FALSE)
    }
  )
  
  
  
  # Gap Analysis Page  ------------------------------------------------------
  ## generate the gapAnalysus input dataset ----

  # Gap analysis method  ----------------------------------------------------
  ## initialize map  
  output$map2 <- leaflet::renderLeaflet(generateMap2())
  ## populated table with the data 
  observeEvent(input$continue, {
    d1 <- combined_data()
    # visualize table
    output$mapTable2 <- renderDT(d1)
  })
  
  
  # reactive Values are effective list objects... 
  # gapPoints <- reactiveVal(NULL)
  # generate gap points on input select 
  gapPoints <- eventReactive(input$addGapPoints, {
    print("gapPoints")
    req(combined_data())
    # generate spatial feature
    createSpatialObject(combined_data())|>
      dplyr::mutate(
        color = case_when(
          `Current Germplasm Type` == "H" ~ combinedColor[1],
          `Current Germplasm Type` == "G" ~ combinedColor[2]
        )
      )
  })
  
  # Observer to add points to the map when the button is clicked
  ## for some reason this does not work if on the 'continue' button so leaving the add to maps button for now. 
  observeEvent(gapPoints(),{
    print("gapPoints to map")
    req(gapPoints())
    # assign vect object to a local variable 
    points <- gapPoints() 
    # spilt out the G and H points
    # assign icon for visualization 
    points <- points |>
      dplyr::mutate(
        icon = dplyr::case_when(
          `Current Germplasm Type` == "G" & source == "GBIF" ~ icon_paths$g_gbif, # Use & for vectorized AND
          `Current Germplasm Type` == "H" & source == "GBIF" ~ icon_paths$h_gbif, # Use & for vectorized AND
          `Current Germplasm Type` == "G" & source == "upload" ~ icon_paths$g_upload, # Corrected source name
          `Current Germplasm Type` == "H" & source == "upload" ~ icon_paths$h_upload, # Corrected source name
          TRUE ~ "default_icon" # Optional: Default icon if no conditions match
        )
      )
    View(points)
    
    ## G
    gGap <- points |>
        dplyr::filter(`Current Germplasm Type` == "G")
    gLabels <- lapply(gGap$popup, htmltools::HTML)
    # assign color/shape 
  
    
    ## H
    hGap <- points |>
        dplyr::filter(`Current Germplasm Type` == "H")
    hLabels <- lapply(hGap$popup, htmltools::HTML)
    # test for presence and add features to the map 
    if(nrow(gGap)!=0){
      # add g points 
      leafletProxy("map2") |>
            setView(lng = mean(gGap$Longitude), lat = mean(gGap$Latitude), zoom = 6)|>
            clearGroup("Germplasm Records") |>
            addMarkers(data = gGap,
                             layerId = ~index,
                             # radius = 4,
                             group = "Germplasm Records",
                             icon = ~makeIcon( # Create the icon *here*
                               iconUrl = icon, # Use the path column
                               iconWidth = 10,
                               iconHeight = 10
                             ),
                             label = gLabels,
                             options = pathOptions(pane = "pointsG"))
              # addLegend(
              #   position = "topright",
              #   layerId = "germplasmalegend",
              #   colors = c(combinedColor[2]),
              #   labels = c("G"),
              #   title = "Germplasm Records",
              #   opacity = 1,
              #   group = "Germplasm Records"
              # )
    }
    if(nrow(hGap)!=0){
        # add h points 
        leafletProxy("map2") |>
          setView(lng = mean(hGap$Longitude), lat = mean(hGap$Latitude), zoom = 6)|>
          clearGroup("Reference Records") |>
          addMarkers(data = hGap,
                   layerId = ~index,
                   # radius = 4,
                   group = "Reference Records",
                   icon = ~makeIcon( # Create the icon *here*
                     iconUrl = icon, # Use the path column
                     iconWidth = 10,
                     iconHeight = 10
                   ),
                   label = hLabels,
                   options = pathOptions(pane = "pointsH"))
          # addCircleMarkers(data = hGap,
          #                  layerId = ~index,
          #                  radius = 4,
          #                  group = "Reference Records",
          #                  color = "white",
          #                  fillColor = ~color,
          #                  stroke = TRUE,
          #                  weight = 1,
          #                  fillOpacity = 1,
          #                  label = hLabels,
          #                  options = pathOptions(pane = "pointsH")) |>
          # addLegend(
          #   position = "topright",
          #   layerId = "referencelegend",
          #   colors = c(combinedColor[1]),
          #   labels = c("H"),
          #   title = "Reference Records",
          #   opacity = 1,
          #   group = "Reference Records"
          # )
      }   
  })
  
  ## createBuffersGap --- 
  ## buffer points -----------------------------------------------------------
  pointsBuffer <- eventReactive(input$renderGapAnalaysis, {
    print("pointsBuffer")
    
    ## so this will break if this is selected before the add to map... not worrying about it now 
    gapPoints()|>
      terra::vect()|>
      terra::buffer(width = as.numeric(input$bufferSize) * 1000)|>
      terra::intersect(land)
    })
  
  # Reactive expressions for H and G buffers
  h_buffer <- reactive({
    print("hBuffer")
    
    req(pointsBuffer()) # Ensure pointsBuffer exists
    # select h points and aggregate for visulization 
    terra::aggregate(
      pointsBuffer()[pointsBuffer()$`Current Germplasm Type` == "H", ]
    )
  })
  
  g_buffer <- reactive({
    print("g buffer")
    
    req(pointsBuffer()) # Ensure pointsBuffer exists
    # select h points and aggregate for visulation 
    terra::aggregate(
      pointsBuffer()[pointsBuffer()$`Current Germplasm Type` == "G", ]
    )
  })


  ### update map with buffer features ---
  observeEvent(pointsBuffer(), {
    print("buffers to map")
    
    ## will need some error handling here. but for now just 
    leafletProxy("map2")|>
      clearGroup("Buffers")|>
      addPolygons(data = h_buffer(),
                  group = "Buffers",
                  color = combinedColor[1],
                  fillOpacity = 0.5,
                  options = pathOptions(pane = "allBuffers"))|>
      addPolygons(data = g_buffer(),
                  group = "Buffers",
                  color = combinedColor[2],
                  fillOpacity = 0.5,
                  options = pathOptions(pane = "allBuffers"))
  })
  
  ## srs ex ------------------------------------------------------------------
  srsex <- reactive({
    require(gapPoints())
    print("srsex")
    p1 <- gapPoints()
    # select g points
    gGap <- p1[p1$`Current Germplasm Type` == "G", ]
    # calculate score
    gs <- nrow(gGap)
    if(gs == 0){
      srsScore <- 0
    }else(
      srsScore <- (nrow(gGap)/ nrow(p1))*100
    )
    # return score
    srsScore
  })
  # print statement to test reactivity 
  observeEvent(srsex(), {
    req(srsex())
    print("SRSEX Score:")
    print(head(srsex())) # Or whatever you want to print
  })
  
  ## GRSex  ------------------------------------------------------------------
  # Reactive expressions for generating GRSex gap buffer object 
  grsex_buffers <- eventReactive(input$renderGapAnalaysis, {   
    
    req(h_buffer()) # Ensure pointsBuffer exists
    print("grsex_buffers")
    # Sys.sleep(1)
    # calculate areas outside of gbuffer
    grsexVals <- terra::erase(x = h_buffer(),
                            y = g_buffer())
    print(head(grsexVals))
    grsexVals
  })
  # generate the score 
  grsex_score <- eventReactive(input$renderGapAnalaysis, { 
    req(pointsBuffer()) # Ensure pointsBuffer exists
    req(grsex_buffers())
    print("grsex score Calculations")
    p1 <- pointsBuffer()
    g1 <- grsex_buffers()
    # total area
    totalArea <- p1 |>
      terra::aggregate() |> 
      terra::expanse(unit="km")
    print("total area")
    print(totalArea)
    # calculate areas outside of gbuffer
    gapArea <- g1 |>
      terra::aggregate() |>
      terra::expanse(unit="km")
    print("gap area")
    print(gapArea)
    #calculate GRSex score
    ## total - gap area give the area covered by G buffers.
    difference <- totalArea - gapArea
    if(difference == 0){
      grsScore <- 0
    }else{
      grsScore <- ((totalArea - gapArea)/totalArea)*100
    }
    print(paste0("The grsex score :", grsScore))
    grsScore
  })
  
  
  ### update map with buffer features ---
  observeEvent(grsex_buffers(), {
    print("grsex_buffers to map")
    
    leafletProxy("map2")|>
      clearGroup("GRS gaps")|>
      addPolygons(data = grsex_buffers(),
                  group = "GRS gaps",
                  color = grsexColor,
                  fillOpacity = 0.5,
                  options = pathOptions(pane = "grsGap"))
  })
  

  # ERSex -------------------------------------------------------------------
  ## determine a all eco regions of interest 
  ersex_Ecos <- eventReactive(input$renderGapAnalaysis, { 
    req(gapPoints())
    # define point objects
    d1 <- gapPoints() |>
      terra::vect()
    print("d1")
    # print(head(d1))
    # determine the eco regions present in the
    inter <- terra::intersect(x = d1, y = ecoRegions) 
    print("inter")
    # print(head(inter))
    
    # select ecoregions of interest
    ecoCodes <- unique(inter$ECO_NAME)
    # select ecoregions of interest
    ecos <- ecoRegions[ecoRegions$ECO_NAME %in% ecoCodes, ]
    print("ecos")
    # print(head(ecos))
    ecos
  })
  # determine missing ecos 
  ersex_missingEcos <- eventReactive(input$renderGapAnalaysis, { 
    req(ersex_Ecos())
    currentEcos <- ersex_Ecos()
    print("current Ecos")
    # print(currentEcos$ECO_NAME)
    # determine ecoregions with G buffers  
    gs <- terra::aggregate(g_buffer())
    gEco <- terra::intersect(x = gs, y = currentEcos)
    print("gEcos")
    print(gEco$ECO_NAME)
    
    # Pull unique Ids and subset the G values 
    ecoIDs <- unique(currentEcos$ECO_NAME[!currentEcos$ECO_NAME %in% gEco$ECO_NAME])
    print("ecoIDs")
    # print(ecoIDs)
    # select from 
    missingEco <- currentEcos[currentEcos$ECO_NAME %in% ecoIDs]
    print("totalmissing")
    # print(missingEco$ECO_NAME)
    # return  
    missingEco
  })
  
  # generate the score 
  ersex_score <- eventReactive(input$renderGapAnalaysis, {
    req(ersex_missingEcos())
    ecos1 <- ersex_missingEcos()
    # 
    allEcos <- ersex_Ecos()
    print("missing ecos -- score")
    # print(ecos1)


    nEco <- nrow(allEcos)
    gEco <- nEco - nrow(ecos1)
    ers <- min(c(100, (gEco/nEco)*100))
    print("ERS")
    print(paste0("ers score ",ers))
    ers
  })
  # update the map 
  observeEvent(ersex_missingEcos(), {
    print("ers map")
    leafletProxy("map2")|>
      clearGroup("ERS gaps")|>
      addPolygons(data = ersex_missingEcos(),
                  group = "ERS gaps",
                  color = ersexColor,
                  fillOpacity = 0.5,
                  popup = ~ECO_NAME,
                  options = pathOptions(pane = "ersGap"))
  })


  ### render gap analysis plot ------------------------------------------------
    gapAnalysisResultsFigure<- eventReactive(input$renderGapAnalaysis,{
      # define the base table
      df <- data.frame(class = c(
        "Sampling Representativeness Score",
        "Ecological Representativeness Score",
        "Geographic Representativeness Score",
        "Final Representativeness Score"
      ),
      score = c(0,0,0,0))
      # assign values based on the presence of specific output values
      df$score[1] <- as.numeric(try(srsex()))
      df$score[2] <- as.numeric(try(ersex_score()))
      df$score[3] <- as.numeric(try(grsex_score()))
      # assign the fcsex score
      df$score[4] <- as.numeric(try(mean(df[1:3, "score"], na.rm = TRUE)))

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
  
  # Text summary of the gap analysis 
  # Example of updating the text output
  observeEvent(input$renderGapAnalaysis, { # Or any other event
    # Generate the text you want to display
    srs <- as.numeric(try(srsex()))|> round(digits = 2)
    grs <- as.numeric(try(grsex_score()))|> round(digits = 2)
    ers <- as.numeric(try(ersex_score())) |> round(digits = 2)
    points <- gapPoints()
    totalPoints <- nrow(points)
    referncePoints <- nrow(points[points$`Current Germplasm Type` == "H",])
    germplasmPoints <- nrow(points[points$`Current Germplasm Type` == "G",])
    species <- points$`Taxon Name`[1]
    output$map_text_output <- renderPrint({
      HTML(paste(
        "<p>Gap Analysis summary results for :", species, "</p>", # Paragraphs for formatting
        "<p>Reference points: ", referncePoints, "          Germplasma points: ", germplasmPoints, "<br>",
        " SRSex : ", srs, " -- ", " GRSex : ", grs, " -- "," ERSex : ", ers , "</p>" 
      ))
    })
  })
   
  # develop the popup 
  observeEvent(input$createBuffersGap, {
    # print(combined_data())
    Sys.sleep(1)
    ## show the popup selection
    showModal(
      modalDialog(
        title = "Run the gap analysis",
        "The following step is going to take a minute to run. Maybe more depending on the number of locations. If your ready press yes.",
        footer = tagList(
          actionButton( "renderGapAnalaysis","Yes"),
          actionButton("return2", "Return to Gap Analysis Page"),
        ),
        size = "l",
        easyClose = TRUE
      )
    )
  })
  # return to Page 1 when "Return" is clicked
  observeEvent(input$return2, {
    removeModal() # Remove the modal dialog
  })
  
  
  # test print  --------------------------------------------------------------

  # output$testPrint <- renderText({ input$txt })
}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

