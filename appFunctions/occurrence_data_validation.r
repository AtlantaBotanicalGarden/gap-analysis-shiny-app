

## 
#' ETL of User Supplied Occurrence Data into GAMMA data-type 
#'
#' @param occurrence_df " 
#' @param valid_data_structure 
#'
#' @return
#' @export
#'
#' @examples
etl_occ_data <- function(occurrence_df, valid_data_structure) {

  
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
etl_gbif_occ_data <- function(gbif_occurrence_df, valid_data_structure) {
  
  # this is selecting column but not renaming the feature. 
  formatted_occurrence_df <- gbif_occurrence_df[unlist(valid_data_structure, use.names = FALSE)]
  # assigning names 
  names(formatted_occurrence_df) <- c(names(valid_data_structure), "geometry")
  
  # run a classification on the basic of records for defining the type 
  formatted_occurrence_df <- formatted_occurrence_df |>
    mutate(type = case_when(
      type != "LIVING_SPECIMEN" ~ "H",
      type == "LIVING_SPECIMEN" ~ "G"
    ))
  
  return(formatted_occurrence_df)
}
