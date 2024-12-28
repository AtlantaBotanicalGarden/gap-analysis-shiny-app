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
server <- function(input, output) {
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
  ## Download data from GBIF 
  gbifData <- eventReactive(input$gbifPull, {
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
     year <- c(input$startYear, input$endYear)
   }else{
     year <- NULL
   }
   
   
   # define params
   initialPull <- query_gbif_occ(taxonkey = taxonID,
                  allow_synonyms_bool = synonym,
                  limit = downloadLimit,
                  year = year,
                  # issues_not_allowed = issueCodes
                  )
   initialPull
   
 })

  ## Produce a text output to show the results 
  output$gbifDownloadSpecifics <- renderText({
    if(is.null(gbifData())){
      "There is no data available on GBIF for this species"
    }else{
      paste0("The query returned ", nrow(gbifData()), " records.")
    }
  })
 
  ## display GBIF data in the table 
  output$mapTableGBIF <- renderDT(gbifData())

  ## GBIF Data
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
  # add the data to the gbif table
  output$mapTableGBIF <- renderRHandsontable(gbifData() |> rhandsontable())
  
  
  # generate a reactive spatail data object
  points1 <- reactiveVal(NULL)
  
  observeEvent(input$mapTableGBIF, {
    # generate spatial data 
    tempPoints2 <-createSpatialObject(input$mapTableGBIF) |>
      dplyr::mutate(
        color = case_when(
          `Current Germplasm Type` == "H" ~ gbifColor[1],
          `Current Germplasm Type` == "G" ~ gbifColor[2]
        )
      )
    points1(tempPoints2)
  })
  
  
  
  
    # Observe changes to the spatial object and update the map
  observe({
    gbifPoints <- points1()
    if (!is.null(gbifPoints)){
      # ideally this would be within the create SpatialObject call. 
      labels <- lapply(gbifPoints$popup, htmltools::HTML)
    # produce map
    leafletProxy("map1")|>
      setView(lng = mean(gbifPoints$Longitude), lat = mean(gbifPoints$Latitude), zoom = 6)|>
      addCircleMarkers(
        data = gbifPoints, 
        layerId = ~`Accession Number`,
        group = "GBIF",
        radius = 4,
        color = "white",
        fillColor = ~color,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = labels) 
    }
  })
  # Reactive values to store selected markers
  selectedGBIF <- reactiveValues(markers = NULL)
  
  
  # INDIVIDUAL POINT SELECTION
  # Observe marker clicks
  observeEvent(input$map1_marker_click, {
    click <- input$map1_marker_click
    
    # define local varaible 
    gbifPoints <- points1()

    # If marker is already selected, deselect it
    if(click$group == "GBIF"){
      # add it to selected markers
      selectedGBIF$markers <- c(selectedGBIF$markers, click$id)
      # update the map 
      leafletProxy("map1") %>%
        addCircleMarkers(
          data = gbifPoints |>
            dplyr::filter(`Accession Number` == click$id),
          layerId = ~`Accession Number`,
          radius = 4,
          color = "red",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "GBIF Selection"
        )
    }
    
    if(click$group == "GBIF Selection"){
      # remove from selection list
      selectedGBIF$markers <- selectedGBIF$markers[selectedGBIF$markers != click$id]
      # update the map
      leafletProxy("map1") %>%
        leaflet::clearGroup("GBIF Selection") |>
        addCircleMarkers(
          data = gbifPoints[gbifPoints$`Accession Number` %in% selectedGBIF$markers, ],
          layerId = ~`Accession Number`,
          radius = 4,
          color = "red",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "GBIF Selection"
        )
    }
  })
  
  
  # uploaded dataset processing -----------------------------------------------
  dataUpload <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })

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
  
  ## display in table
  output$mapTableUpload <- renderRHandsontable(dataUpload() |> rhandsontable())
  
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
  # generate a reactive spatail data object
  points <- reactiveVal(NULL)
  
  observeEvent(input$mapTableUpload, {
    # generate spatial data 
    tempPoints1 <-createSpatialObject(input$mapTableUpload) |>
      dplyr::mutate(
        color = case_when(
          `Current Germplasm Type` == "H" ~ uploadColor[1],
          `Current Germplasm Type` == "G" ~ uploadColor[2]
        )
      )
    points(tempPoints1)
  })
  
  # Observe changes to the spatial object and update the map
  observe({
    uploadPoints <- points()
    if (!is.null(uploadPoints)) {
      # ideally this would be within the create SpatialObject call. 
      labels <- lapply(uploadPoints$popup, htmltools::HTML)
      # produce map
      leafletProxy("map1")|>
        setView(lng = mean(uploadPoints$Longitude), lat = mean(uploadPoints$Latitude), zoom = 6)|>
        addCircleMarkers(
          data = uploadPoints, 
          layerId = ~`Accession Number`,
          group = "Upload",
          radius = 4,
          color = "white",
          fillColor = ~color,
          stroke = TRUE,
          weight = 1,
          fillOpacity = 1,
          label = labels) 
    }
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
    uploadPoints <- points()
    if(click$group == "Upload"){
      # add it to selected markers
      selectedUpload$markers <- c(selectedUpload$markers, click$id)
      # edit map  
      leafletProxy("map1") %>%
          addCircleMarkers(
            data = uploadPoints |>
              dplyr::filter(`Accession Number` == click$id) ,
            layerId = ~`Accession Number`,
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
          data = uploadPoints[uploadPoints$`Accession Number` %in% selectedUpload$markers, ],
          layerId = ~`Accession Number`,
          radius = 4,
          color = "red",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "Upload Selection"
        )
    }
  })
  
  
  
  
  # combined table---------------------------------------------
  # combined_data <- reactive({
  #     d1 <- NA
  #     d2 <- NA
  #     d3 <- NA
  #     outputTable <- data.frame()
  #   # organize the gbif data
  #   d1 <- try(gbifData()|>
  #               dplyr::mutate(source = "GBIF",
  #                             "Accession Number" = as.character(`Accession Number`),
  #                             "Collection Date" = as.character(`Collection Date`)))
  #   d2 <- try(dataUpload()|>
  #                 dplyr::mutate(source = "upload",
  #                               "Accession Number" = as.character(`Accession Number`),
  #                               "Collection Date" = as.character(`Collection Date`)))
  #   d3 <- try(dplyr::bind_rows(d1,d2))
  #   # both datasets
  #   if("data.frame" %in% class(d3)){
  #     outputTable <- d3
  #   }
  #   # upload olny
  #   if("data.frame" %in% class(d2) && !"data.frame" %in% class(d1)){
  #     outputTable <- d2
  #   }
  #   #gbif only
  #   if(!"data.frame" %in% class(d2) && "data.frame" %in% class(d1)){
  #     outputTable <- d1
  #   }
  #   outputTable
  # })


  # Gap Analysis Page  ------------------------------------------------------
  ## generate the gapAnalysus input dataset ----

  # Gap analysis method  ----------------------------------------------------
  ## initialize map  
  output$map2 <- leaflet::renderLeaflet(generateMap2())
  ## 
  # generate dataframe object 
  observeEvent(input$compileDatasets, {
    # visualize table 
    output$mapTable2 <- renderDT( hot_to_r(input$mapTableUploadEdit)) 
  })
  
  
  # generate point object 
  gapPoints <- eventReactive(input$compileDatasets, {
    d1 <- hot_to_r(input$mapTableUploadEdit)
    # visualize table 
    output$mapTable2 <- renderDT(d1) 
    # output the spatial object
    createSpatialObject(d1)
  })
  
  
  ## add points to the map 
  observeEvent(input$addGapPoints, {

    # spilt out the G and H points
    gGap <- gapPoints() |>
      dplyr::filter(`Current Germplasm Type` == "G")
    hGap <- gapPoints() |>
      dplyr::filter(`Current Germplasm Type` == "H")
    
    # add points to the map 
    if(nrow(gGap)==0){
      # update the map
      leafletProxy("map2")|>
        setView(lng = mean(gapPoints()$Longitude), lat = mean(gapPoints()$Latitude), zoom = 6)|>
        # addCircleMarkers(
        #   data = hGap,
        #   color = ~color,
        #   stroke = TRUE,
        #   radius = 5,
        #   fillOpacity = 0.9,
        #   popup = ~popup,
        #   group = "Reference Records"
        # )|>
        addMarkers(
          data = hGap,
          group = "Reference Records",
          popup = ~popup,
          icon = leaflet::icons(~icon, iconWidth = 10, iconHeight = 10),
          options = pathOptions(pane = "points")
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
        addMarkers(
          data = hGap,
          group = "Reference Records",
          popup = ~popup,
          icon = leaflet::icons(~icon, iconWidth = 10, iconHeight = 10),
          options = pathOptions(pane = "points")
        )|>
        addMarkers(
          data = gGap,
          group = "Germplasm Records",
          popup = ~popup,
          icon = leaflet::icons(~icon, iconWidth = 10, iconHeight = 10),
          options = pathOptions(pane = "points")
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


  
  
## add points to map ---
  
      
  
    ## createBuffersGap --- 
  ## buffer points -----------------------------------------------------------
  pointsBuffer <- eventReactive(input$createBuffersGap, {
    gapPoints()|>
      terra::vect()|>
      terra::buffer(width = as.numeric(input$bufferSize) * 1000)
    })
  # aggregate buffers and crop 
  aggregateBuffers <-  eventReactive(input$createBuffersGap, {
    pointsBuffer() |>
      terra::aggregate()|>
      terra::mask(mask = land)
    })

  # # g buffer object 
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
  # 
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
          fillOpacity = 0.9,
          options = pathOptions(pane = "buffers")) |>
        addPolygons(
          data = gbuf,
          group = "Buffers",
          color = "purple",
          fillOpacity = 0.9,
          options = pathOptions(pane = "buffers"))|>
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
          fillOpacity = 0.9,
          options = pathOptions(pane = "buffers")) |>
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
  # 
  # 
  # 
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
  # 
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

  # 
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
                        sendToBack = TRUE),
            options = pathOptions(pane = "ecoregions")
          ) |>
          # grsex gap layer
          addPolygons(
            data = grsexGap(),
            color = "#d8b365",
            opacity = 0.2,
            group = "GRS gaps",
            fillOpacity = 0.4,
            options = pathOptions(pane = "gaps"))|>
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

  })

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

  # test print  --------------------------------------------------------------

  # output$testPrint <- renderText({ input$txt })
}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

