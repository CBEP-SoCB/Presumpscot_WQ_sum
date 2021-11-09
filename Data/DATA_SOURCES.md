# Data Source
Data was received from Toby Jacobs from Presumpscot Regional Land Trust, 
sent to Curtis C. Bohlen March 13, 2020. The data was actually in a
Google Sheets document. A corrected version was forwarded in a follow-up e-mail 
on March 20, 2020 and downloaded by Curtis Bohlen as an Excel file March 25, 
2020. `Historical Data Master.xlsx`

Data QA/QC revealed observations with likely errors.  In particular,
observations in which recorded DO and Pct Saturation values were not consistent.
We shared our observations with Toby Jacobs, who passed them on to Kristen 
Feindel at DEP, who provided corrections for eight samples from 2018.

Preliminary analysis also demonstrated that pairs of observations taken in 2019
had identical *E. coli* data, but different DO values.  After consulting with
Toby Jacobs, we interpreted these as transcription errors, in which the 
*E. coli* data were incorrectly duplicated. While editing the file, we removed
*E. coli* data from one of each pair of replicate DO samples.  We arbitrarily 
removed the data from the FIRST matching observation in the data set.

All corrections were entered BY HAND into a copy of the data by Curtis Bohlen
on 4/10/2020.  That data was then processed by R scripts to produce the
corrected data included here.

# Geospatial Information
Mapping data on the locations of sample sites was assembled directly from the
Google Sheets document. That was done by making a copy of the "Historical Data
Master", converting all formulae on the "Mapping" tab to values, and deleting
other tabs.  That file was saved as an Excel file, `Presumpscot WQ Locations
Data.xlsx`  That was then loaded into ArcGIS to produce the shapefiles in the 
"GIS" folder. 
