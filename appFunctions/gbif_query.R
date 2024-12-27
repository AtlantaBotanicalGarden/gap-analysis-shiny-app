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
# taxonkey <- 3153837
# limit <- 20
# year <- c(2019:2024)
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
    summary2 <- gbif_response2$data

      
  }else{
    for(i in 1:length(year)){
      gbif_response1 <- rgbif::occ_data(taxonKey = taxonkey,
                                        hasCoordinate = TRUE,
                                        hasGeospatialIssue = FALSE,
                                        basisOfRecord = "LIVING_SPECIMEN",
                                        limit = limit,
                                        year = year[i])
      # bind data from all years
        d1 <- gbif_response1$data
        if(i == 1){
          summary1 <- d1
        }else{
          summary1 <- bind_rows(summary1, d1)
        }
    }
    # Set the total number to be less then or equal to user set limit
    pull <- ifelse(is.null(nrow(summary1)), yes = pull <- limit, no = pull <- limit - nrow(summary1))
    for(i in 1:length(year)){
      # repeat for all basis of records 
      gbif_response2 <- rgbif::occ_data(taxonKey = taxonkey,
                                        hasCoordinate = TRUE,
                                        hasGeospatialIssue = FALSE,
                                        limit = pull,
                                        year = year[i]
                                        )
      d2 <- gbif_response2$data
      print(d2)
        if(i == 1){
          summary2 <- d2
        }else{
          summary2 <- bind_rows(summary2, d2)
        }
      }
  }
  
  

  
  # bind the datasets
  if(is.null(summary1)){
    parsed_response <- summary2
  }else{
    parsed_response <- dplyr::bind_rows(summary1,summary2)
  }
  
  # parsed_response <- gbif_response$data
  pattern <- paste(issues_not_allowed, collapse = "|")
  
  if(pattern != ""){
    parsed_response <- filter(parsed_reponse, !str_detect(parsed_reponse$issues, pattern))
  }
  # remove synonyms from the search 
  if(allow_synonyms_bool == FALSE){
    parsed_response <- parsed_response[parsed_response$taxonomicStatus == "ACCEPTED",]
  }
  
  # rename and assign type 
  ## locality column was not being pulled with low values.. adding condition to test 
  if("locality" %in% names(parsed_response)){
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
  }else{
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
      ),
      "Locality" = NA
      )
  }
  return(output)
}
