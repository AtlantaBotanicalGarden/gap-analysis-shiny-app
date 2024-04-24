## GBIF Querying Function for Populating Taxa Selects ##
# Author: Jonathan Gore
# Create Date: 20240321
# Edit Date: 20240327
##

library("rgbif")
library("dplyr")
library("stringr")

# used functions ----------------------------------------------------------


## Getting GBIF Occurrence Data ##
# Allows for optional filtering out multiple issues
# reference issue codes can be found in issues.csv [https://github.com/AtlantaBotanicalGarden/gap-analysis-shiny-app/issues/2]
query_gbif_occ <- function(taxonkey, allow_synonyms_bool = TRUE, issues_not_allowed = NULL) {
  # call all G points 
  gbif_response1 <- rgbif::occ_data(taxonKey = taxonkey,
                            hasCoordinate = TRUE,
                            hasGeospatialIssue = FALSE,
                            basisOfRecord = "LIVING_SPECIMEN",
                            limit = 200
                            )
  # Set the total number to be less then or equal to 500
  if(is.null(nrow(gbif_response1$data))){
    pull <- 200
  }else{
    pull <- 200 - nrow(gbif_response1$data)
  }
  # call H points with the amount 
  gbif_response2 <- rgbif::occ_data(taxonKey = taxonkey,
                                   hasCoordinate = TRUE,
                                   hasGeospatialIssue = FALSE,
                                   limit = pull)
  # bind the datasets
  if(is.null(nrow(gbif_response1$data))){
    parsed_response <- gbif_response2$data
  }else{
    parsed_response <- dplyr::bind_rows(gbif_response1$data, gbif_response2$data)
  }
  
  # parsed_response <- gbif_response$data
  pattern <- paste(issues_not_allowed, collapse = "|")
  
  if(pattern != ""){
    parsed_response <- filter(parsed_reponse, !str_detect(parsed_reponse$issues, pattern))
  }
  # I think we would want to do this in the inital query not filter after the fact because we are setting the 500 features limit. 
  if(allow_synonyms_bool == FALSE){
    parsed_response <- parsed_response[parsed_response$taxonomicStatus == "ACCEPTED",]
  }
  parsed_response
}


# currently not used ------------------------------------------------------

# ## Core function ##
# query_gbif_taxonomy <- function(qstring, qrank) {
#   api_result <- name_suggest(q = qstring, rank = qrank, limit = 100)
#   taxon_df <- api_result$data
#   taxon_df <- taxon_df[taxon_df$canonicalName == qstring,]
#   return (taxon_df$key)
# }
# 
# ## Elaborated Function to handle pagination ##
# fuzzy_search_gbif_taxonomy <- function(qstring, qrank) {
#   limit <- 100 # The max number of records per page
#   start <- 0   # Start at the beginning
#   all_data <- tibble() # Initialize an empty tibble to store results
#   
#   repeat {
#     # Fetch a page of results
#     api_result <- name_suggest(q = qstring, rank = qrank, limit = limit, start = start)
#     # Combine with previous results
#     all_data <- bind_rows(all_data, api_result$data)
#     
#     # Check if there are more results to fetch
#     if (nrow(api_result$data) < limit) {
#       break # Exit loop if we've fetched all results
#     }
#     
#     # Otherwise, prepare to fetch the next page
#     start <- start + limit
#   }
#   
#   taxon_df <- api_result$data
#   taxon_df <- taxon_df[taxon_df$canonicalName == qstring,]
#   
#   # Return unique results
#   return(taxon_df)
# }
# 
# ## Query Children of GBIF Taxon Key ##
# # Current arbitrary limit set to 10000 because it lets me
# # could implement pagination in future if required if a taxa has more than 10000 children
# query_gbif_taxon_key_child <- function(qkey, qgenus, qrank) {
#   
#   api_result <- name_usage(qkey, data = "children", rank = qrank, limit = 10000)
#   
#   childdata <- api_result$data
#   childdata <- childdata[childdata$taxonomicStatus == "ACCEPTED",]
#   childdata <- filter(childdata, sapply(str_split(childdata$species, " "), function(x) x[1]) == qgenus)
#   
#   return(childdata)
# }




