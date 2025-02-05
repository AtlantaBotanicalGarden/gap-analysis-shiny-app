## GBIF Querying Function for Populating Taxa Selects ##
# Author: Jonathan Gore
# Create Date: 20240321
# Edit Date: 20240327
##

# library("rgbif")
# library("dplyr")
# library("stringr")

# used functions ----------------------------------------------------------


# testing 
## Magnolia acuminata
# taxonkey <- 3153837
# testing species
# taxonkey <- 5939154
# limit <- 400
# year <- NULL
# issues_not_allowed = NULL
# allow_synonyms_bool = TRUE
## Getting GBIF Occurrence Data ##
# Allows for optional filtering out multiple issues
# reference issue codes can be found in issues.csv [https://github.com/AtlantaBotanicalGarden/gap-analysis-shiny-app/issues/2]
query_gbif_occ <- function(taxonkey,
                           allow_synonyms_bool = TRUE, 
                           issues_not_allowed = NULL,
                           limit = 200,
                           year = NULL
                           ){

  # no year specified 
  if(is.null(year)){
    # gather all basis of record == living specimen 
    gbif_response1 <- rgbif::occ_data(taxonKey = taxonkey,
                                      hasCoordinate = TRUE,
                                      hasGeospatialIssue = FALSE,
                                      basisOfRecord = "LIVING_SPECIMEN",
                                      limit = limit)
    
    summary1 <- gbif_response1$data
    # Set the total number to be less then or equal to user set limit
    pull <- ifelse(is.null(nrow(summary1)), yes = pull <- limit, no = pull <- limit - nrow(summary1))
    # gathers all basis Of records 
    gbif_response2 <- rgbif::occ_data(taxonKey = taxonkey,
                                      hasCoordinate = TRUE,
                                      hasGeospatialIssue = FALSE,
                                      limit = pull)
    
    # combine the datasets
    parsed_response  <- dplyr::bind_rows(summary1, gbif_response2$data)
  }else{
    # assign this outside of the forloop to intilize 
    summary1 <- NULL
    years <- year[1]:year[2]
    print(years)
    for(i in years){
      print(i)
      gbif_response1 <- rgbif::occ_data(taxonKey = taxonkey,
                                        hasCoordinate = TRUE,
                                        hasGeospatialIssue = FALSE,
                                        basisOfRecord = "LIVING_SPECIMEN",
                                        limit = limit,
                                        year = i)
      # bind data from all years
      d1 <- gbif_response1$data
      print(d1)
      if(!is.null(d1)){
        if(class(summary1)[1]== "NULL"){
          summary1 <- d1
          pull <- limit - nrow(summary1)
        }else{
          summary1 <- bind_rows(summary1, d1)
          pull <- limit - nrow(summary1)
        }
      }
    }
    parsed_response <- summary1
  }
  
  if(is.null(parsed_response) || (is.data.frame(parsed_response) && nrow(parsed_response) == 0)){
    # empty output 
    output <- data.frame(matrix(nrow=1, ncol = 9))
    names(output) <- c("Accession Number","Taxon Name","Current Germplasm Type",
                       "Collection Date","Latitude","Longitude","Locality",
                       "Collector","issues")    
    # add some data for map doesn't fail 
    output[1, 5:6] <- c(0,0) 
    return(output)     # need to add a print statement to the UI 
    # if succesful show the total number of records gathers
    # if unsucessful print not records were found
  }else{
    # parsed_response <- gbif_response$data
    pattern <- paste(issues_not_allowed, collapse = "|")
    
    if(pattern != ""){
      parsed_response <- filter(parsed_reponse, !str_detect(parsed_reponse$issues, pattern))
    }
    # remove synonyms from the search 
    if(allow_synonyms_bool == FALSE){
      parsed_response <- parsed_response[parsed_response$taxonomicStatus == "ACCEPTED",]
    }
    
    
    ## need a test here that checks for all expect columns in the data and if they are not present adds them wiht NA values assign
    add_missing_columns <- function(df, col_names) {
      for (col in col_names) {
        if (!(col %in% names(df))) {
          print(col)
          df[[col]] <- NA  # Add missing column with NA values
        }
      }
      return(df)
    }
    # need to test against the original names from the gbif pull
    col_names <- c("key","scientificName","basisOfRecord",
                   "year","decimalLatitude","decimalLongitude","locality",
                   "recordedBy","issues")   
    
    parsed_response <- add_missing_columns(parsed_response, col_names)
    
    # rename and assign type 
    ## locality column was not being pulled with low values.. adding condition to test 
    # if("locality" %in% names(parsed_response)){
      output <- parsed_response |> 
        dplyr::select(
          "Accession Number" = key,
          "Taxon Name" = scientificName,
          "Current Germplasm Type" = basisOfRecord, 
          "Collection Date" = year,
          "Latitude" = decimalLatitude,
          "Longitude" = decimalLongitude,
          "Locality" = locality,
          "Collector" = recordedBy,
          issues
        )|>
        dplyr::mutate(`Current Germplasm Type` = case_when(
          `Current Germplasm Type` != "LIVING_SPECIMEN" ~ "H",
          `Current Germplasm Type` == "LIVING_SPECIMEN" ~ "G"
        ))
    # }else{
    #   output <- parsed_response |> 
    #     dplyr::select(
    #       "Accession Number" = key,
    #       "Taxon Name" = scientificName,
    #       "Current Germplasm Type" = basisOfRecord, 
    #       "Collection Date" = year,
    #       "Latitude" = decimalLatitude,
    #       "Longitude" = decimalLongitude,
    #       "Locality" = locality,
    #       "Collector" = recordedBy,
    #       issues
    #     )|>
    #     dplyr::mutate(`Current Germplasm Type` = case_when(
    #       `Current Germplasm Type` != "LIVING_SPECIMEN" ~ "H",
    #       `Current Germplasm Type` == "LIVING_SPECIMEN" ~ "G"
    #     ),
    #     "Locality" = NA
    #     )
    # }
    
    output$`Accession Number` <- as.character(output$`Accession Number`)
    return(output)
  }
}


















# old 
# Set the total number to be less then or equal to user set limit
# pull <- ifelse(is.null(nrow(summary1)), yes = pull <- limit, no = pull <- limit - nrow(summary1))
# for(i in 1:length(year)){
#   # repeat for all basis of records 
#   gbif_response2 <- rgbif::occ_data(taxonKey = taxonkey,
#                                     hasCoordinate = TRUE,
#                                     hasGeospatialIssue = FALSE,
#                                     limit = pull,
#                                     year = year[i]
#                                     )
#   d2 <- gbif_response2$data
#   print(d2)
#     if(i == 1){
#       summary2 <- d2
#     }else{
#       summary2 <- bind_rows(summary2, d2)
#     }
#   }
