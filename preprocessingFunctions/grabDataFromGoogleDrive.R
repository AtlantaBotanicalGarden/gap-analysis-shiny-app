# pull stuff to the server through google drive 
library(googledrive)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(readr)


## require attribute data 
names <- c("UID", "taxon", "genus",  "species", "type", "year", "latitude",
           "longitude", "locality", "collector")

# Magnolia fraseri existu 
d1 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1ub67ylXEeSX6czRRqlEqBLysYCaWviK31T9l3ta2Gb0/edit?usp=drive_link"),
                                sheet = "Sheet1")
d1a <- d1 |>
  dplyr::mutate(
    type = "Exsitu"
  )|>
  dplyr::select(
    UID = "UID",
    taxon = "species_name_acc",
    type,
    year = "submission_year",
    latitude = "lat_dd",
    longitude = "long_dd",
    locality = "locality" ,
    collector =  "coll_name"
  )|> 
  tidyr::separate(
    col = taxon,
    into = c("genus","species"),
    remove = FALSE,
    sep = " "
  )


# magnolia fraseri insitu 
d2 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1YLvj1ya4V9KQHSgbiky0kL4-8ly1YWtJ_dhxfg99B4w/edit?usp=drive_link"))

d2a <- d2 |>
  dplyr::mutate(
    type = "Insitu",
    Specimen_ID = as.character(Specimen_ID)
  )|>
  dplyr::select(
    UID = "Specimen_ID",
    taxon = "FullName",
    type,
    year = "Year",
    latitude = "dec_lat",
    longitude = "dec_long",
    locality = "Locality" ,
    collector =  "recordedBy"
  )|> 
  tidyr::separate(
    col = taxon,
    into = c("genus","species"),
    remove = FALSE,
    sep = " "
  )

## bind dataset and export 
d1d2 <- dplyr::bind_rows(d1a,d2a)
write_csv(x = d1d2, file = "appData/magnolia_fraseri.csv")
# Quercus brandegeei 
d3 <- googlesheets4::read_sheet(as_id("https://docs.google.com/spreadsheets/d/1EOyV15R4rKsxuZQ8tCywnOcmY6Csv6YCr1I7x74hFZ4/edit?usp=sharing"))

d3a <- d3 |>
  dplyr::mutate(
    type = case_when(
      basisOfRecord == "PRESERVED_SPECIMEN" ~ "Exsitu",
      TRUE ~ "Insitu"
    )
  )|>
  dplyr::select(
    UID = "UID",
    taxon = "taxon_name",
    genus = "genus",
    species = "specificEpithet",
    type,
    year = "year",
    latitude = "decimalLatitude",
    longitude = "decimalLongitude",
    locality = "locality" ,
    collector =  "recordedBy"
  )
# export data
write_csv(x = d3a, file = "appData/Quercus_brandegeei.csv")
