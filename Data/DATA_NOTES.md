# Data Preparation

## Sample Preparation
After data QA/QC, we reformatted the underlying Excel to CSV files using an R 
script.

In reorganizing the data, we also addressed some QA/QC concerns, especially from
2017, 2018, and 2019.  In particular, some of the qualitative data fields were
incomplete, so we had to drop variables.  In addition, duplicate samples were
not always consistently flagged, resulting in erroneously duplicated  *E.coli*
data, and data where it was not always clear whether they represented the same
sampling event or not. We arbitrarily assigned one of some pairs of data from
the same sampling date, time and location as "duplicates".  Data flags
indicating right censoring of the *E. coli* data were not always shown with 
consistent symbolism. We harmonized those differences, and generated a 
separate indicator that shows censored vaues.

We also dropped unused data, recoded attribute names, and otherwise looked
for ways to simplify the data.

## Data Contents
**presumpscot_CORRECTED.csv**
Missing data is coded in this data file with "NA".

Column Name | Contents                                  | Units / Values 
------------|-------------------------------------------|-------------- 
(Blank)     | Row Number                                | Integer 
Site        | Site Code                                 | Text 
Name        | Site Name                                 | Text 
Year        | Year of data collection                   | 2009 - 2019 
Date        | Date of sample collection                 | "%m/%d/%Y" 
Time        | Time of sample collection                 | "%H:%M:%S" 
QC          | QC flags, especially "D" for duplicates   | "NA"" or "D"         
Depth       | Depth of sample (rare)                    | meters
Temp        | Temperature                               | Celsius
DO          | Dissolved oxygen                          | mg/l
PctSat      | Percent saturation (of oxygen)            | percent
Ecoli       | E. Coli, MPN estimated colonies per 100ml | Float
Flag        | Flags indicating  right censoring of *E. coli* data |  TRUE/FALSE
------------|-------------------------------------------|-------------- 

## Geospatial Data Contents
Geospatial data was assembled from latitudes and longitudes in Excel files.
The Shapefile includes the following attributes:

File attributes:

Column Name | Contents                                  | Units / Values 
------------|-------------------------------------------|-------------- 
Site_Name   | Site code (NOT a user-readable name)      | Text
lat         | Latitide                                  | WGS 1984
long        | Longitude                                 | WGS 1984
Water_Body	| Name of river or stream
Class       | Statutory class of river of stream        | "AA", "A", "B", "C"	
Location    | narrative description of sampling location| Text
Water_Recr  | Narrative of known recreational use of nearby waters | Text
------------|-------------------------------------------|-------------- 

## Derived Data
### Data on *E. coli* levels 
From `Analysis` Folder, "E_coli_Results.csv".  Our preferred summary of water
quality conditions is the "Clss" data column.

### Data on Dissolved Oxygen
From `Analysis` Folder, "DO_Results.csv".   Our preferred summary of water
quality conditions are the "All_Meets" or "Both_Meets" data columns (which here 
contain the same values).


