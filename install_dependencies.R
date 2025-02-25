# Script to install all required packages for the application
packages <- c(
  "shiny", "bslib", "ggplot2", "shinipsum", "leaflet", "DT", 
  "readr", "dplyr", "tools", "vroom", "rgbif", "spocc", 
  "jsonlite", "stringr", "shinycssloaders", "shinyalert", 
  "plotly", "sf", "tidyterra", "leaflet.extras", 
  "rhandsontable", "geojsonio", "geojsonsf", "tidyr", 
  "googledrive", "googlesheets4", "terra"
)

# Check which packages need to be installed
missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse=", "))
  install.packages(missing_packages, repos="https://cran.rstudio.com/")
} else {
  message("All required packages are already installed.")
}
