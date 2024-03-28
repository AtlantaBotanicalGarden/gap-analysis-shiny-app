library("jsonlite")

## ETL of User Supplied Occurrence Data into GAMMA data-type ##
etl_occ_data <- function(occurrence_df) {
  
  JSONdata <- fromJSON('G:/Projects/gap-analysis-shiny-app/appData/data_validation.json')
  valid_data_structure <- JSONdata[[1]]
  
  valid_columns <- colnames(valid_data_structure)
  target_columns <- colnames(occurrence_df)
  
  missing_columns <- c()
  
  ## This is the current logic for matching column names ##
  # is currently an exact string match, we could do some fuzzy matching in future
  for (x in valid_columns){
    if (x %in% target_columns) {
      next
    } else {
      missing_columns <- append(missing_columns, x)
    }
  }
  
  if (is.null(missing_columns)){
    formatted_occurrence_df <- occurrence_df[names(valid_data_structure)]
    return(formatted_occurrence_df)
  } else {
    return(missing_columns)
  }
  
}


## ETL of GBIF Data into GAMMA data-type ##
etl_gbif_occ_data <- function(gbif_occurrence_df) {
  
  JSONdata <- fromJSON('G:/Projects/gap-analysis-shiny-app/appData/data_validation.json')
  valid_data_structure <- JSONdata[[1]]
  
  formatted_occurrence_df <- gbif_occurrence_df[unlist(valid_data_structure, use.names = FALSE)]
  
  return(formatted_occurrence_df)
}
