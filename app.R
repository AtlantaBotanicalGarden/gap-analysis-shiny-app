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
library(rhandsontable)
library(leaflet.extras)

# source UI or Server only functions ------------------------------------
lapply(list.files(
  path = "appFunctions",
  pattern = ".r",
  ignore.case = TRUE,
  full.names = TRUE),
  source)

# global variables -----------------------------------------
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
    nav_panel(
      title = "Gap Analysis",
    
    ),
    nav_panel(
      title = "About",
      
    ),
    nav_spacer(),
    # reference links
    links()
  )## end page_navbar
)## end ui
  

# server ----------------------------------------------------------------
server <- function(input, output) {
  # UI select Subspecies 
 #  output$speciesSelect = renderUI({
 #    # filter the data 
 #    genusData <- gbifBackbone |>
 #      dplyr::filter(genus == as.character(input$genusSelect))
 #    # define selector 
 #    selectInput("speciesSelect", "Select a species", choices = sort(genusData$specificEpithet), selected = )
 #  })
 #  # UI select variaty/subspec
 #  output$taxonRank = renderUI({
 #    # filter the data
 #    filteredData <- gbifBackbone |>
 #      dplyr::filter(genus == as.character(input$genusSelect)) |>
 #      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
 #    # define selector
 #    selectInput("taxonRank", "Select a taxon rank", choices = filteredData$taxonRank, selected = )
 #  })
 #  # UI select sub species feature
 #  output$speciesInfraspecific = renderUI({
 #    # filter the data
 #    filteredData2 <- gbifBackbone |>
 #      dplyr::filter(genus == as.character(input$genusSelect)) |>
 #      dplyr::filter(specificEpithet == as.character(input$speciesSelect))|>
 #      dplyr::filter(taxonRank == as.character(input$taxonRank))
 #      
 #    # define selector
 #    selectInput("speciesInfraspecific", "Select a infraspecific epithet", choices = filteredData2$infraspecificEpithet, selected = )
 #  })
 #  output$currentSpecies = renderText({
 #    name <- paste0("Current Taxon: ", input$genusSelect, " ",tolower(input$speciesSelect), " ")
 #    if(input$taxonRank != "species"){
 #      name <- paste0(name, tolower(input$taxonRank), " ", tolower(input$speciesInfraspecific))
 #    }
 #    # pass the object to render
 #    name
 #  })
 #  
 #  # pull the gbif id from the selection 
 #  ### might be nice to have a single generic function that takes input parameters for all selections. 
 #  output$gbiftaxonid <- renderText({
 #    f1 <- gbifBackbone |>
 #      dplyr::filter(genus == as.character(input$genusSelect)) |>
 #      dplyr::filter(specificEpithet == as.character(input$speciesSelect))
 #    
 #    if(as.character(input$taxonRank) == "species"){
 #      f1 <- gbifBackbone |>
 #        dplyr::filter(genus == as.character(input$genusSelect)) |>
 #        dplyr::filter(specificEpithet == as.character(input$speciesSelect)) |>
 #        dplyr::filter(taxonRank == as.character(input$taxonRank))
 #      f1$taxonID[1]
 #    }else{
 #      f1 <- gbifBackbone |> 
 #        dplyr::filter(taxonRank == as.character(input$taxonRank))|>
 #        dplyr::filter(infraspecificEpithet == as.character(input$speciesInfraspecific)) 
 #      f1$taxonID[1]
 #    }
 #    
 #  })
 # 
 #  # Download data from GBIF -------------------------------------------------
 #  gbifData <- eventReactive(input$gbifPull, {
 # 
 #    if(as.character(input$taxonRank) == "species"){
 #      f1 <- gbifBackbone |>
 #        dplyr::filter(genus == as.character(input$genusSelect)) |>
 #        dplyr::filter(specificEpithet == as.character(input$speciesSelect)) |>
 #        dplyr::filter(taxonRank == as.character(input$taxonRank))
 #      taxonID <- f1$taxonID[1]
 #    }else{
 #      f1 <- gbifBackbone |> 
 #        dplyr::filter(taxonRank == as.character(input$taxonRank))|>
 #        dplyr::filter(infraspecificEpithet == as.character(input$speciesInfraspecific)) 
 #      taxonID <- f1$taxonID[1]
 #    }
 # 
 #    # define params
 #    initialPull <- query_gbif_occ(taxonkey = taxonID,
 #                   allow_synonyms_bool = TRUE)
 # 
 #    # filter to specific layers
 #    structureData <- etl_gbif_occ_data(gbif_occurrence_df = initialPull,
 #                                       valid_data_structure = valid_data_structure)
 # 
 #    structureData
 #  })
 #  
 #  # 
 #  output$gbifDownloadSpecifics <- renderText({
 #    if(is.null(gbifData())){
 #      "There is no data available on GBIF for this species"
 #    }else{
 #      paste0("The query returned ", nrow(gbifData()), " records.")
 #    }
 #  })
 # 
 # 
 #  # create spatial data for example/GBIF feature  -----------------------------------
 #  sp1 <- observeEvent(input$gbifToMap, {
 #    # generate spatial object 
 #    pointsVals <- gbifData() |>
 #      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
 #      dplyr::mutate(
 #        index = dplyr::row_number(),
 #        popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
 #                       "<br/><strong> Current Germplasm Type: </strong>", `Current Germplasm Type`,
 #                       "<br/><b>Collector Name:</b> ", Collector,
 #                       "<br/><b>Locality Description):</b> ", Locality),
 #        color = case_when(
 #          `Current Germplasm Type` == "H" ~ "#1184d4",
 #          `Current Germplasm Type` == "G" ~ "#6300f0"
 #        )
 #      )
 #    
 #    # update the map
 #    leafletProxy("map1")|>
 #      setView(lng = mean(pointsVals$Longitude), lat = mean(pointsVals$Latitude), zoom = 6)|>
 #      addCircleMarkers(
 #        data = pointsVals,
 #        color = ~color,
 #        stroke = FALSE,
 #        fillOpacity = 0.9,
 #        popup = ~popup,
 #        group = "GBIF",
 #        
 #      ) |>
 #      # single legend for the GBIF features
 #      addLegend(
 #        position = "topright",
 #        layerId = "GBIFlegend",
 #        colors = c("#1184d4","#6300f0"),
 #        labels = c("H","G"),
 #        title = "GBIF Data",
 #        opacity = 1,
 #        group = "GBIF"
 #      )
 #  })
 #  
 #  # create spatial data for uploaded data  -----------------------------------
 # observeEvent(input$uploadToMap, {
 #    # generate spatial object 
 #    pointsVals2 <- dataUpload()|>
 #      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
 #      dplyr::mutate(
 #        index = dplyr::row_number(),
 #        popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
 #                       "<br/><strong> Type: </strong>", `Current Germplasm Type`,
 #                       "<br/><b>Collector Name:</b> ", Collector,
 #                       "<br/><b>Locality Description):</b> ", Locality),
 #        color = case_when(
 #          `Current Germplasm Type` == "H" ~ "#1184d4",
 #          `Current Germplasm Type` == "G" ~ "#6300f0"
 #        )
 #      )
 #    # update the map 
 #    leafletProxy("map1")|>
 #      setView(lng = mean(pointsVals2$Longitude), lat = mean(pointsVals2$Latitude), zoom = 6)|>
 #      addCircleMarkers(
 #        data = pointsVals2,
 #        color = ~color,
 #        stroke = FALSE,
 #        fillOpacity = 0.9,
 #        popup = ~popup,
 #        group = "Upload Dataset"
 #      ) |>
 #      # single legend for the GBIF features
 #      addLegend(
 #        position = "topright",
 #        layerId = "uploadlegend",
 #        colors = c("#1184d4","#6300f0"),
 #        labels = c("H","G"),
 #        title = "Upload Dataset",
 #        opacity = 1,
 #        group = "Upload Dataset"
 #      ) 
 #  })
 #  
 # 
 #  # Remove layers from map  -------------------------------------------------
 #  ## GBIF --- remove all features 
 #  observeEvent(input$removeGBIF, {
 #    leafletProxy("map1")|>
 #      clearGroup(group = "GBIF")|>
 #      removeControl(layerId = "GBIFlegend") # doesn't seem to be working at the moment. Mugh
 #      # not sure if I can remove the layer control element once it is added 
 #  })
 #  ## upload data -- remove all features  
 #  observeEvent(input$removeUpload, {
 #    leafletProxy("map1")|>
 #      clearGroup(group = "Upload Dataset")|>
 #      removeControl(layerId = "uploadlegend") # doesn't seem to be working at the moment. Mugh
 #    # not sure if I can remove the layer control element once it is added 
 #  })
 #  ###
 #  # for the polygon selection a few things need to happen 
 #  # 1. create a template layer to 
 #  
 #  
 #  # Create an empty reactive value to store selected points
 #  selected_points <- reactiveVal()
 #  
 #  # Observe the drawn polygon and update selected points
 #  observeEvent(input$map_draw_new_feature, {
 #    if (input$map_draw_new_feature$type == "polygon") {
 #      polygon <- input$map_draw_new_feature$geometry$coordinates[[1]]
 #      
 #      # Check if any initial points are within the polygon
 #      selected_points(c(selected_points(), polygon))
 #    }
 #  })
 #  # Observe the "Remove Selected Points" button and clear selected points
 #  observeEvent(input$removeSelected, {
 #    
 #    d1 <- selected_points()
 #    if("GBIF" %in%  d1$source){
 #      gbifOnly <- d1 |>
 #        dplyr::filter(source == "GBIF")|>
 #        dplyr::select(index)|>
 #        dplyr::pull()
 #      
 #      gbifData <- gbifData()|>
 #        fitler(-gbifOlny)
 #      
 #      # update the map
 #      leafletProxy("map1")|>
 #        setView(lng = mean(gbifData$Longitude), lat = mean(gbifData$Latitude), zoom = 6)|>
 #        addCircleMarkers(
 #          data = gbifData,
 #          color = ~color,
 #          stroke = FALSE,
 #          fillOpacity = 0.9,
 #          popup = ~popup,
 #          group = "GBIF") |>
 #        # single legend for the GBIF features
 #        addLegend(
 #          position = "topright",
 #          layerId = "GBIFlegend",
 #          colors = c("#1184d4","#6300f0"),
 #          labels = c("H","G"),
 #          title = "GBIF Data",
 #          opacity = 1,
 #          group = "GBIF"
 #        )
 #      
 #    }
 #    if("upload" %in%  d1$source){
 #      uploadOnly <- d1 |>
 #        dplyr::filter(source == "upload")|>
 #        dplyr::select(index)|>
 #        dplyr::pull()
 #      
 #      dataUpload <- dataUpload()|>
 #        fitler(-uploadOnly)
 #      
 #      
 #      leafletProxy("map1")|>
 #        setView(lng = mean(dataUpload$Longitude), lat = mean(dataUpload$Latitude), zoom = 6)|>
 #        addCircleMarkers(
 #          data = dataUpload,
 #          color = ~color,
 #          stroke = FALSE,
 #          fillOpacity = 0.9,
 #          popup = ~popup,
 #          group = "Upload Dataset") |>
 #        # single legend for the GBIF features
 #        addLegend(
 #          position = "topright",
 #          layerId = "uploadlegend",
 #          colors = c("#1184d4","#6300f0"),
 #          labels = c("H","G"),
 #          title = "Upload Dataset",
 #          opacity = 1,
 #          group = "Upload Dataset"
 #        ) 
 #      
 #    }
 #  })
  
  
  
  # Export the table from data processing page  -----------------------------------------------
  # 
  # 
  # 
  # # GBIF Data
  # output$download1 <- downloadHandler(
  #   filename = function() {
  #     # Use the selected dataset as the suggested file name
  #     paste0(input$genusSelect,"_",input$speciesSelect, "_data.csv")
  #   },
  #   content = function(file) {
  #     # Write the dataset to the `file` that will be downloaded
  #     write.csv(gbifData(), file, row.names = FALSE)
  #   }
  # )
  # 
  # output$download2 <- downloadHandler(
  #   filename = function() {
  #     # Use the selected dataset as the suggested file name
  #     paste0(input$genusSelect,"_",input$speciesSelect, "_data.csv")
  #   },
  #   content = function(file) {
  #     # Write the dataset to the `file` that will be downloaded
  #     write.csv(dataUpload(), file, row.names = FALSE)
  #   }
  # )
  # 
  # output$download3 <- downloadHandler(
  #   filename = function() {
  #     # Use the selected dataset as the suggested file name
  #     paste0(input$genusSelect,"_",input$speciesSelect, "_data.csv")
  #   },
  #   content = function(file) {
  #     #Convert to R object before export
  #     write.csv(hot_to_r(input$mapTableUploadEdit), file, row.names = FALSE,quote = TRUE)
  #     }
  # )
  # 
  # # read in the uploaded data -----------------------------------------------
  # dataUpload <- reactive({
  #   req(input$upload)
  #   
  #   ext <- tools::file_ext(input$upload$name)
  #   switch(ext,
  #          csv = vroom::vroom(input$upload$datapath, delim = ","),
  #          validate("Invalid file; Please upload a .csv file")
  #   )
  # })
  # # handson Table object 
  # reactTableUpload <- reactive({   
  #   dataUpload() |>
  #     rhandsontable(width = 1800, height = 200)|>
  #     hot_cols(fixedColumnsLeft = 1, columnSorting = TRUE) |>
  #     hot_rows(fixedRowsTop = 1)|>
  #     hot_col("Current Germplasm Type", type = "dropdown", source = c("G","H"))|>
  #     hot_col("Collection Date", type = "date")|>
  #     hot_table(highlightCol = TRUE, 
  #               highlightRow = TRUE)})
  # 
  # 
  # 
  # 
  # output$mapTableUploadEdit <- renderRHandsontable({reactTableUpload()})
  # 
  # 
  # 
  # output$validateColNames <- renderText({
  #   colNames <- names(dataUpload())
  #   vals <- c()
  #   for(i in seq_along(expectedNames)){
  #     test1 <-TRUE %in% grepl(pattern = expectedNames[i], x = colNames)
  #     if(test1 == FALSE){
  #       vals[i] <- expectedNames[i]
  #     }
  #   }
  #   nonMatchedNames <- vals[!is.na(vals)]
  #   if(is.null(nonMatchedNames)){
  #     print("All column names match")
  #   }else{
  #     print(paste0("The following columns are not present in the uploaded data ", nonMatchedNames))
  #   }
  #   
  #   
  # })
  # 
  # # compile dataset for the gap analysis page  ------------------------------
  # ## popup to show that the compile dataset is working. ----
  # observeEvent(input$compileDatasets, {
  #   # Show a modal when the button is pressed
  #   shinyalert(title = "Data is Moving",
  #              text = "Data is being gather and organized. Please continue your evaluation on the Gap Analysis page.",
  #              type = "success")
  # })
  # ## generate the gapAnalysus input dataset ----
  # gapAnalysisInput <- reactive({
  #   d1 <- NA
  #   d2 <- NA
  #   d3 <- NA
  #   req(input$compileDatasets)
  #   d1 <- try(gbifData()|>
  #               dplyr::mutate(source = "GBIF",
  #                             "Accession Number" = as.character(`Accession Number`),
  #                             "Collection Date" = as.character(`Collection Date`)))
  #   d2 <- try(rhandsontable::hot_to_r(input$mapTableUploadEdit)|>
  #               dplyr::mutate(source = "upload",
  #                             "Accession Number" = as.character(`Accession Number`),
  #                             "Collection Date" = as.character(`Collection Date`)))
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
  

  # intial map on gap analysis page -----------------------------------------
  
  # gapPoints <-  reactive({
  #   # req(input$compileDatasets) ### transistion from using this as the que to sequentail outputs. 
  #   gapAnalysisInput()|>
  #     sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)|>
  #     dplyr::mutate(
  #       popup = paste0("<strong>", as.character(`Taxon Name`),"</strong>", # needs to be text
  #                      "<br/><strong> Current Germplasm Type: </strong>", `Current Germplasm Type`,
  #                      "<br/><b>Collector Name:</b> ", Collector,
  #                      "<br/><b>Locality Description):</b> ", Locality,
  #                      "<br/><b>Data Source:</b> ", source),
  #       color = case_when(
  #         `Current Germplasm Type` == "H" ~ "#1184d4",
  #         `Current Germplasm Type` == "G" ~ "#6300f0"
  #       )
  #     )
  # })
  ## add points to map ---
  # observeEvent(input$addGapPoints, {
  #   # # generate spatial object 
  #   # spilt out the G and H points 
  #   gGap <- gapPoints() |>
  #     dplyr::filter(`Current Germplasm Type` == "G")
  #   hGap <- gapPoints() |>
  #     dplyr::filter(`Current Germplasm Type` == "H")
  #   
  #   # icons for the specific data source 
  #   gIcons <- icons(
  #     iconUrl = ifelse(gGap$source == "GBIF",
  #                      "www/purpleSquare.png",
  #                      "www/purpleTriangle.png"),
  #     iconWidth = 14,
  #     iconHeight = 14)
  #   hIcons <- icons(
  #     iconUrl = ifelse(hGap$source == "GBIF" ,
  #                      "www/blueSquare.png",
  #                      "www/blueTriangle.png"),
  #     iconWidth = 14,
  #     iconHeight = 14)
  #   
  #   
  #   if(nrow(gGap)==0){
  #     # update the map 
  #     leafletProxy("map2")|>
  #       setView(lng = mean(gapPoints()$Longitude), lat = mean(gapPoints()$Latitude), zoom = 6)|>
  #       addMarkers(
  #         data = hGap,
  #         # color = ~color,
  #         # stroke = TRUE,
  #         # radius = 5,
  #         # fillOpacity = 0.9,
  #         popup = ~popup,
  #         icon = hIcons,
  #         group = "Reference Records",
  #         options = pathOptions(pane = "points")
  #       )|>
  #       # single legend for the GBIF features
  #       addLegend(
  #         position = "topright",
  #         layerId = "referencelegend",
  #         colors = c("#1184d4"),
  #         labels = c("H"),
  #         title = "Reference Records",
  #         opacity = 1,
  #         group = "Reference Records"
  #       )
  #   }else{
  #     # update the map 
  #     leafletProxy("map2")|>
  #       setView(lng = mean(gapPoints()$Longitude), lat = mean(gapPoints()$Latitude), zoom = 6)|>
  #       clearGroup("Reference Records")|>
  #       clearGroup("Germplasm Records")|>      
  #       addMarkers(
  #         data = hGap,
  #         # color = ~color,
  #         # stroke = FALSE,
  #         # radius = 5,
  #         # fillOpacity = 0.9,
  #         popup = ~popup,
  #         icon = hIcons,
  #         group = "Reference Records",
  #         options = pathOptions(pane = "points")
  #       ) |>
  #   addMarkers(
  #         data = gGap,
  #         # color = ~color,
  #         # stroke = FALSE,
  #         # radius = 5,
  #         # fillOpacity = 0.9,
  #         popup = ~popup,
  #         icon = gIcons,
  #         group = "Germplasm Records",
  #         options = pathOptions(pane = "points")
  #       ) |>
  #       # single legend for the GBIF features
  #       addLegend(
  #         position = "topright",
  #         layerId = "referencelegend",
  #         colors = c("#1184d4"),
  #         labels = c("H"),
  #         title = "Reference Records",
  #         opacity = 1,
  #         group = "Reference Records"
  #       ) |>
  #       addLegend(
  #         position = "topright",
  #         layerId = "germplasmalegend",
  #         colors = c("#6300f0"),
  #         labels = c("G"),
  #         title = "Germplasm Records",
  #         opacity = 1,
  #         group = "Germplasm Records"
  #       ) 
  #   }
  # 
  # })

  # Gap analysis method  ----------------------------------------------------
  ## calculate the SRSex -- number of G points / total number of points 
  ##buffer the points 
  ## display the buffered objects on the map
  ## calculate the GRSex -- total G buffer area / total area 
  ## calculate the ERSex -- total number of ecorgions within the g buffer / total number of eco regions 
  ## display data on a chart 
  



  ## createBuffersGap --- 
  ## buffer points -----------------------------------------------------------
  # pointsBuffer <- eventReactive(input$createBuffersGap, {
  #   gapPoints()|>
  #     terra::vect()|>
  #     terra::buffer(width = as.numeric(input$bufferSize) * 1000)
  #   })
  # # aggregate buffers 
  # aggregateBuffers <-  eventReactive(input$createBuffersGap, {
  #   pointsBuffer() |>
  #     terra::aggregate()
  #   })
  # 
  # # g buffer object 
  # ## sf objects because they are being added to the map 
  # gGapBuffer <- eventReactive(input$createBuffersGap, {
  #   # g points 
  #   pointsBuffer()|>
  #     sf::st_as_sf()|>
  #     dplyr::filter(`Current Germplasm Type` == "G")
  # })
  # # h buffer object
  # ## sf objects because they are being added to the map 
  # hGapBuffer <- eventReactive(input$createBuffersGap, {
  #   pointsBuffer()|>
  #     sf::st_as_sf()|>
  #     dplyr::filter(`Current Germplasm Type` == "H")
  #   })
  # 
  # ### update map with buffer features --- 
  # observeEvent(input$createBuffersGap, {
  #   # aggregate buffer object to reduce complexity 
  #   hbuf <- hGapBuffer() |> 
  #     terra::vect() |>
  #     terra::aggregate()|>
  #     st_as_sf()
  #   gbuf <- gGapBuffer() |> 
  #     terra::vect() |>
  #     terra::aggregate()|>
  #     st_as_sf()
  #   
  #   
  #   if(nrow(gbuf)!=0){
  #     # update the map 
  #     leafletProxy("map2")|>        
  #       clearGroup("Buffers")|>
  #       removeControl(layerId = "bufferLegend") |>
  #       addPolygons(
  #         data = hbuf,
  #         group = "Buffers",
  #         color = "blue",
  #         fillOpacity = 0.9,
  #         options = pathOptions(pane = "buffers")) |>
  #       addPolygons(
  #         data = gbuf,
  #         group = "Buffers",
  #         color = "purple",
  #         fillOpacity = 0.9,
  #         options = pathOptions(pane = "buffers"))|>
  #       addLegend(
  #         position = "topright",
  #         layerId = "bufferLegend",
  #         colors = c("blue", "purple"),
  #         labels = c("Reference", "Germplasm"),
  #         title = "Buffers",
  #         opacity = 1,
  #         group = "Buffers")
  #   }else{
  #     leafletProxy("map2")|>        
  #       clearGroup("Buffers")|>
  #       removeControl(layerId = "bufferLegend") |>
  #       addPolygons(
  #         data = hbuf,
  #         group = "Buffers",
  #         color = "blue",
  #         fillOpacity = 0.9,
  #         options = pathOptions(pane = "buffers")) |>
  #       addLegend(
  #         position = "topright",
  #         layerId = "bufferLegend",
  #         colors = c("blue"),
  #         labels = c("Reference"),
  #         title = "Buffers",
  #         opacity = 1,
  #         group = "Buffers")
  #   }
  # })
  # 
  # 
  # 
  # ## trimed ecoregion ---
  # # crop the ecoregions to full buffer object
  # allEcos <-  eventReactive(input$generateGapMaps, {
  #   ecoRegions |>
  #     terra::crop(aggregateBuffers())
  # })
  # # g area ecoregions 
  # gEcos <- eventReactive(input$generateGapMaps, {
  #   if(nrow(gGapBuffer())>0){
  #     gVals <- terra::vect(gGapBuffer())|>
  #       terra::aggregate()
  #     # crop to g buffers 
  #     gEco <- allEcos() |>
  #       terra::crop(gVals)
  #   }else{
  #     gEco <- allEcos()[0,]
  #     }
  #   gEco
  # })
  # 
  # ## generete gap maps -------------------------------------------------------
  # 
  # ## ers gap Map ----  
  # ## return SF object because it's on the map 
  # ersexGap <- eventReactive(input$generateGapMaps, {
  #   # get a unique list of values from eco buffers 
  #   allEco1 <- unique(allEcos()$ECO_ID)
  #   gEco1 <- unique(gEcos()$ECO_ID)
  #   
  #   # select ecos not in gEcos
  #   gEco1 <- allEco1[!allEco1 %in% gEco1]
  #   
  #   # select eco not in the gEcoLayer 
  #   gapEcos <- ecoRegions |>
  #     tidyterra::filter(ECO_ID %in% gEco1)
  #   # spatail object showing the ecoregions outside of the 
  #   gapEcos |> st_as_sf()
  # })
  # 
  # ## gap map ---  
  # grsexGap <- eventReactive(input$generateGapMaps, {
  #   # format the buffer feature
  #   gGapBuffer1 <-  terra::vect(gGapBuffer()) |>
  #     terra::aggregate()
  #   # difference 
  #   gapBuffers2 <- aggregateBuffers() - gGapBuffer1
  #   # set to sf for display on map 
  #   gapBuffers2 |> st_as_sf()
  # 
  # })
  # 
  # 
  # ## gap analysis data to the map ----------------------------------------------------------
  # ### update map with gap map features --- 
  # observeEvent(input$generateGapMaps, {
  #       # update the map
  #       leafletProxy("map2")|>
  #         clearGroup("ERS gaps")|>
  #         clearGroup("GRS gaps")|>
  #         removeControl(layerId = "ersGapLegend") |>
  #         removeControl(layerId = "bufferLegend") |>
  #         # ers gap map layer
  #         addPolygons(
  #           data = ersexGap(),
  #           color = "grey",
  #           opacity = 0,
  #           popup = ~ECO_NAME,
  #           group = "ERS gaps",
  #           fillOpacity = 0.9,
  #           highlight = highlightOptions(
  #                       weight = 3,
  #                       fillOpacity = 0.4,
  #                       color = "black",
  #                       fillColor = "yellow",
  #                       opacity = 0.4,
  #                       bringToFront = TRUE,
  #                       sendToBack = TRUE),
  #           options = pathOptions(pane = "ecoregions")
  #         ) |>
  #         # grsex gap layer
  #         addPolygons(
  #           data = grsexGap(),
  #           color = "#d8b365",
  #           opacity = 0.2,
  #           group = "GRS gaps",
  #           fillOpacity = 0.4,
  #           options = pathOptions(pane = "gaps"))|>
  #     addLegend(
  #           position = "topright",
  #           layerId = "ersGapLegend",
  #           labels = c("Uncollected Ecoregions"),
  #           title = "ERSex Gap",
  #           colors = c("grey"),
  #           opacity = 1,
  #           group = "ERS gaps"
  #         )|>
  #     addLegend(
  #       position = "topright",
  #       layerId = "grsGapLegend",
  #       labels = c("Areas outside of Gerplasm Buffer Range"),
  #       title = "GRSex Gap",
  #       colors = c("#d8b365"),
  #       opacity = 1,
  #       group = "GRS gaps"
  #     )
  # 
  # })
  # 
  # 
  # 
  # 
  # 
  # ## generateGapSummary --- 
  # ## srs ex ------------------------------------------------------------------
  # srsex <- eventReactive(input$generateGapSummary,{
  #   # select g points
  #   gGap <- gapPoints() |>
  #     dplyr::filter(`Current Germplasm Type` == "G")
  #   # calculate score 
  #   gs <- nrow(gGap)
  #   if(gs == 0){
  #     srsScore <- 0
  #   }else(
  #     srsScore <- (nrow(gGap)/ nrow(gapPoints()))*100
  #   )
  #   # return score
  #   srsScore
  # })
  # ## GRSex  ------------------------------------------------------------------
  # grsex <- eventReactive(input$generateGapSummary, {
  #   # total area
  #   totalArea <- aggregateBuffers() |>
  #     terra::expanse(unit="km")
  #   # calculate areas outside of gbuffer 
  #   gapArea <- grsexGap()  |>
  #     terra::vect() |>
  #     terra::expanse(unit="km")
  #   #calculate GRSex score 
  #   ## total - gap area give the area covered by G buffers. 
  #   difference <- totalArea - gapArea
  #   if(difference == 0){
  #     grsScore <- 0
  #   }else{
  #     grsScore <- ((totalArea - gapArea)/totalArea)*100   
  #   }
  #   grsScore
  #   
  # })
  # 
  # ## ERSex -------------------------------------------------------------------
  # ersex <- eventReactive(input$generateGapSummary, {
  #   
  #   #calculate ersex score 
  #   gs <- nrow(gEcos())
  #   if(gs == 0){
  #     ersScore <- 0
  #   }else(
  #     ersScore <- (nrow(gEcos())/nrow(allEcos()))*100
  #   )
  #   ersScore
  # })
  # ### render gap analysis plot ------------------------------------------------
  #   gapAnalysisResultsFigure<- eventReactive(input$generateGapSummary,{
  #     # define the base table 
  #     df <- data.frame(class = c(
  #       "Sampling Representativeness Score",
  #       "Ecological Representativeness Score",
  #       "Geographic Representativeness Score", 
  #       "Final Representativeness Score"
  #     ),
  #     score = c(0,0,0,0))
  #     # assign values based on the presence of specific output values 
  #     df$score[1] <- try(srsex())
  #     df$score[2] <- try(ersex())
  #     df$score[3] <- try(grsex())
  #     # assign the fcsex score 
  #     df$score[4] <- try(mean(df[1:3, "score"], na.rm = TRUE))
  #     
  #     # assign color based on the score 
  #     df <- df|>
  #       dplyr::mutate(rank = case_when(
  #         score <= 25 ~ "Urgent Priority",
  #         score > 25 & score <= 50 ~ "High Priority", 
  #         score > 50 & score <= 75 ~"Medium Priority",
  #         score > 75 ~ "Low Priority"
  #       ), 
  #       colors = case_when(
  #         rank == "Urgent Priority" ~ "#ffb4b3",
  #         rank ==  "High Priority" ~ "#ffd380",
  #         rank == "Medium Priority"~ "#ffff80",
  #         rank == "Low Priority" ~ "#a8d2a8"
  #       )
  #     )
  #     #define the display order of the plot
  #     xform <- list(categoryorder = "array",
  #                   categoryarray = c("Sampling Representativeness Score",
  #                                     "Ecological Representativeness Score",
  #                                     "Geographic Representativeness Score", 
  #                                     "Final Representativeness Score"))
  #     
  #     # generate a plotly figure 
  #     fig <- plot_ly(
  #       data = df, 
  #       x = ~class,
  #       y = ~score,
  #       marker = list(color = c(df$colors)),
  #       type = "bar"
  #     )|>
  #       layout(title = "Gap Analysis Ex Situ Conservation Summary",
  #             xaxis = xform,
  #             yaxis = list(title = "", 
  #                          range = c(0,100))
  #             )
  #   # print figure 
  #     fig
  #   }
  # )
  # output$gapAnalysisResults <- renderPlotly(gapAnalysisResultsFigure())

# poorly ordered renders --------------------------------------------------
  # output$map1 <- leaflet::renderLeaflet(map1)
  # output$mapTableGBIF <- renderDT(gbifData())
  # output$mapTableUpload <- renderDT(dataUpload())
  # 
  # output$image <- renderImage(random_image())
  # output$map2<- leaflet::renderLeaflet(map2)
  # output$mapTable2 <- renderDT(gapAnalysisInput()) # shinipsum::random_DT(nrow = 10, ncol = 10))

}



# call the app ------------------------------------------------------------


shinyApp(ui, server)

