# Data Notes
The GIS data associated with the maps here are available in other folders,
as follows:

## Geospatial Data
Point locations were taken from "Presumpscot_WQ_Locations_Data.xlsx", in the 
`Original_Data` folder.

Geospatial data was imported into ArcGIS, as latitude  and longitude pairs 
(WGS 1984) and exported as the SHAPEFILE "Sample_Locations" in the 
`Derived_Data/GIS` folder. 

## Data on *E. coli* levels 
From `Analysis` Folder, "E_coli_Results.csv".  Our preferred summary of water
quality conditions is the "Clss" data column.

## Data on Dissolved Oxygen
From `Analysis` Folder, "DO_Results.csv".   Our preferred summary of water
quality conditions are the "All_Meets" or "Both_Meets" data columns (which here 
contain the same values).

## Combining the Data
The *E. coli* and dissolved oxygen CSV data were joined to the geospatial data 
in the shapefile for display.



