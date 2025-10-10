# GAMMA 

##  Project Description 

*This project is under active development, please expect frequent and potential significant changes between now and the planned release in 2026*

The **Gap Analysis and Meta Collection Management** (GAMMA) was original funding under an IMLS agreement. The funding for the work was redacted by the Federal Government in March 2025. 
To evaluated the significant and initating work that was support by the IMLS agreement please see the oct205 branch within this repository 

Going forward the project is funded through a coorperative agreement between Atlanta Botanical Garden, Botanical Garden Conservation International and the US Botanical Garden. 


## Expected functionality 

- Provide an open access portal with a user-friendly interface that lowers the technical bar for accessing and visualizing on a map the metacollection of a given plant species. 


The tool will reach this aim by 

- Allowing users to upload their own species occurrence data (*Note:* no uploaded data will persist in the system)
- Query and input species occurrence data from online repositories (GBIF, PlantSearch)
- View all the these occurrences on a map and remove erroneous records 
- Generate a geographic [Gap Analysis](https://github.com/CIAT-DAPA/GapAnalysis) to visualize and quantify gap in the meta collection of a species 
- Export the tabular data, a map, and a summary report of the gap analysis results. 


### Project timeline 
- October 2025 : initial planing and goal setting with interal project team
- Winter 2025 to Spring 2026 : Continue development by the core product team 
- Summer 2025 : Full Product release with public outreach event 


## Organization of the Repository

**gap-analysis-shiny-app** (*name will change )   
├── 0_preprocessingData.R :  *script for preparing any raw input required by the application*    
├── appData : *folder containing datasets required by the application*     
├── appFunctions : *folder containing functions used within the application*     
├── app.R : *script for defining the application*     
├── dataToPreProcess : *folder containing raw data sets called by the 0_preprocessingData.R file*     
├── preprocessingFunctions : *folder containing functions used by the 0_preprocessingData.R file *     
├── README.md : *script for preparing any raw input required by the application*     
├── shinyModules :  *folder containing modules used within the shiny application*     
├── utilities :  *folder containing helper functions that may be used in both the preprocessing and shiny application* 
|── www : *folder containing css and image files*     

