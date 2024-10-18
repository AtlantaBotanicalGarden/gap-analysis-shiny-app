library(rhandsontable)
DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = TRUE
              )

df1 <- DF |>
  dplyr::mutate(source = case_when(
    val <= 5 ~ "group1",
    val > 5 ~ "group2"
  )) |> 
  dplyr::group_by(source)

rhandsontable(df1, rowHeaders = NULL, useTypes  = TRUE)

## table questions 
# - would change to the table then reflect the datasets (lat long values)
# - on the table upload ;;; 
#   - highlight the issues 
#   - then they can go in and edit within the application to make the changes 
# - !!add a specific column for text annotation -- this saves with the tables and within the applicaiton 
# 
