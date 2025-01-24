# Change log 

Versioning Nomenclature

1.x = the version presented at the April 2024 training 

2.x = the version developed for the March 2025 training 

*Added* for new features 

*Changed* for changes in existing functionality.

*Deprecated* for soon-to-be removed features.

*Removed* for now removed features.

*Fixed* for any bug fixes. 

## Gamma 2.1.1
Updates from 2025 01 24 

Goals 
data eval page
- remove index column from the tables 

gap analysis page 
- add a wait spinner for buffer point generation 
- add lakes to water filter dataset 
- add unique symbols to data based on source 
- add the complete gap anlaysis workflow back in 
- start development of the report 
- 

### added 
- legend to data evaluation page 

### change 
- ensure the year seaching  gbif data is working as expected
- added a stop gap error catch measure for cases when no records are pulled 


## Gamma 2.1 

### added 
- new point visualization on the data analysis page - upload and gbif data are seperate 
- new popup element that enables a data download before moving to gap analysis page 
- added new color/layout legend elements to the buffer map page 

### changed 

- alter map/table layout to be a side by side features 
- simplified the number of button pushes require for map visualizations 
- greatly improved the stability of the download gbif options 


### removed 

- eliminated the "basis of record" filter within advanced gbif filter 
- removed the filter on 'issue' functionality until we get more generate interest in the process