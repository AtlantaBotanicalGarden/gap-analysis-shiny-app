# GAMMA 

##  Project Description 

*This project is under active development, please expect frequent and potential significant changes between now and the planned release in 2026*

The **Gap Analysis and Meta Collection Management** (GAMMA) tool aims to  

- Provide an open access portal with a user-friendly interface that lowers the technical bar for accessing and visualizing on a map the metacollection of a given plant species. 


The GAMMA tool will reach this aim by 

- Allowing users to upload thier own species occurrance data (*Note:* no uploaded data will persist in the system)
- Query and input species occurrance data from online repositories (GBIF, PlantSearch)
- View all the these occurrences on a map and remove or flag errorous records 
- Generate a geographic Gap Analysis to visualize and quantify gap in the meta collection of a species 
- Export the tabular data, a map, and a summary report of the gap analysis results. 


### Project timeline 
- May 2024 : initial testing of a minimum viable product at a partner meeting 
- Fall 2024 to Summer 2025 : Continue development by the core product team 
- Summer 2025 : Full product testing with multiple end users at a in person training event 
- Fall and Winter 2025 : Integration of feedback from in person training event 
- Spring 2026 : Release of the stable version of the application at a in person traning event 


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


## Viewing or Running the GAMMA tool  

A hosted version of this application can be found [here]*enter URL when it exists*

To run locally. 

- Download or clone this repo. 
- open the `.Rproj` file
- run the `0_preprocessingData.R` script
- call the `app.R` file or run via the rstudio interface 

## How to Use the Project

**will add**

## Credits 

**will add**

## License

**will add**


# How to Contribute to the project

# Examples 

**will add**
