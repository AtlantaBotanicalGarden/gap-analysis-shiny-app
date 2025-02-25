pacman::p_load(dplyr, readr)
# read in data
data <- read_csv("~/trueNAS/work/extraData/Plantae_Taxon.csv") 
# filter ot genus of interest
filterData <- data |>
  dplyr::filter(genus %in% c(
    "Acer",
    "Pinus",
    "Cycas",
    "Ceratozamia",
    "Hamamelis",
    "Dipterocarpus",
    "Diospyros",
    "Erica",
    "Magnolia",
    "Nothofagus",
    "Quercus",
    "Rhododendron",
    "Abies",
    "Artocarpus",
    "Attalea",
    "Crataegus",
    "Fraxinus",
    "Gymnocladus",
    "Ilex",
    "Juglans",
    "Prunus",
    "Stewartia",
    "Torreya",
    "Ulmus",
    "Zanthoxylum"
    ))|>
  dplyr::select(-1) # drop the row index column
# export 
write_csv(x = filterData, file = "appData/gbifBackBone.csv")
head(filterData)
