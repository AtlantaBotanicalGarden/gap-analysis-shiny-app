pacman::p_load(dplyr, readr)
# read in data
data <- read_csv("dataToPreProcess/Plantae_Taxon.csv") 
# filter ot genus of interest
filterData <- data |>
  dplyr::filter(genus %in% c(
    "Acer",
    "Pinus",
    "Cycas",
    "Dipterocarpus",
    "Diospyros",
    "Erica",
    "Magnolia",
    "Nothofagus",
    "Quercus",
    "Rhododendron"))|>
  dplyr::select(-1) # drop the row index column
# export 
write_csv(x = filterData, file = "appData/gbifBackBone.csv")
