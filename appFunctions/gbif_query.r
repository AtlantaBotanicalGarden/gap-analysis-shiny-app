## GBIF Querying Function for Populating Taxa Selects ##
# Author: Jonathan Gore
# Create Date: 20240321
# Edit Date: 20240327
##

library("rgbif")
library("dplyr")
library("stringr")



### Likely defunct with a pre-assembled Genus list being the base for TaxonID ###
## Core function ##
query_gbif_taxonomy <- function(qstring, qrank) {
  api_result <- name_suggest(q = qstring, rank = qrank, limit = 100)
  return (unique(api_result$data))
}

## Elaborated Function to handle pagination ##
fuzzy_search_gbif_taxonomy <- function(qstring, qrank) {
  limit <- 100 # The max number of records per page
  start <- 0   # Start at the beginning
  all_data <- tibble() # Initialize an empty tibble to store results
  
  repeat {
    # Fetch a page of results
    api_result <- name_suggest(q = qstring, rank = qrank, limit = limit, start = start)
    # Combine with previous results
    all_data <- bind_rows(all_data, api_result$data)
    
    # Check if there are more results to fetch
    if (nrow(api_result$data) < limit) {
      break # Exit loop if we've fetched all results
    }
    
    # Otherwise, prepare to fetch the next page
    start <- start + limit
  }
  
  # Return unique results
  return(unique(all_data))
}

## Query Children of GBIF Taxon Key ##
# Current arbitrary limit set to 10000 because it lets me
# could implement pagination in future if required if a taxa has more than 10000 children
query_gbif_taxon_key_child <- function(qkey, qrank) {
  api_result <- name_usage(qkey, data = "children", rank = qrank, limit = 10000)
  childdata <- api_result$data
  return(childdata[childdata$taxonomicStatus == "ACCEPTED",])
}
### End of Likely defunct ###

## Getting GBIF Occurrence Data ##
# Allows for optional filtering out multiple issues
# reference issue codes can be found in issues.csv [https://github.com/AtlantaBotanicalGarden/gap-analysis-shiny-app/issues/2]
query_gbif_occ <- function(taxonkey, allow_synonyms_bool = TRUE, issues_not_allowed = NULL) {
  print("gbif response")
  gbif_response <- occ_data(taxonKey = taxonkey,
                            hasCoordinate = TRUE,
                            hasGeospatialIssue = FALSE)

  parsed_response <- gbif_response$data
  pattern <- paste(issues_not_allowed, collapse = "|")
  
  if(pattern != ""){
    parsed_response <- filter(parsed_reponse, !str_detect(parsed_reponse$issues, pattern))
  }
  
  if(allow_synonyms_bool == FALSE){
    parsed_response <- parsed_response[parsed_response$taxonomicStatus == "ACCEPTED",]
  }
  
  return(parsed_response)
}
