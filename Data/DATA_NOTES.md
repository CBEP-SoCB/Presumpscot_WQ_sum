# Data Source
Data received as a Google Sheets document.  E-mail from Toby Jacobs from
Presumpscot Regional Land Trust sent to Curtis C. Bohlen March 13, 2020.
Original data, `Historical Data Master (1).xlsx`, contained several minor
errors.  Revised data forwarded in a follow-up e-mail on March 20, 2020 and
downloaded by Curtis Bohlen as an Excel file March 25, 2020. `Historical Data
Master.xlsx`

Preliminary analysis revealed observations with likely errors.  In particular,
observations in which recorded DO and Pct Saturation values were not consistent.
See Data QA/QC R Notebook for details.  I shared my observations with Toby
Jacobs, who passed them on to "Kristen" at DEP, who provided corrections for
eight samples from 2018, via another excel file, `2018Corrections.xlsx`.

Preliminary analysis also demonstrated that pairs of observations taken in 2019
had identical *E. coli* data, but different DO values.  After consulting with
Toby Jacobs, we interpreted these as transcription errors, in which the *E.
coli* data were incorrectly duplicated.  While editing the file, we removed
*E. coli* data from one of
each pair of replicate DO samples.  We arbitrarily removed the data from the
FIRST matching observation in the data set.

All corrections were entered BY HAND into a copy of the data by Curtis Bohlen
on 4/10/2020.  
*  Samples with all data deleted are highlighted in Yellow.
*  Samples with corrected values are highlighted in Orange, with corrected values
   (including some dates -- differing from the original data by one day) in red
   text.  
*  2019 samples with *E. coli* values deleted are highlighted in Green. File
   was saved as `Historical Data Master CORRECTED.xlsx`.

# Geospatial Information
Mapping data on the locations of sample sites was assembled directly from the
Google Sheets document.  That was done by making a copy of the "Historical Data
Master", converting all formulae on the "Mapping" tab to values, and deleting
other tabs.  That file was saved as an excel file, `Presumpscot WQ Locations
Data.xlsx`

# Data Notes
The file still has some quirks.  Perhaps because it was downloaded from Google
Sheets, Excel reports an error when loading the file, but all data appears to be
intact on the primary data sheet.  Other sheets, with lookup tables and pivot
table, did not transfer correctly.

Historical data also has some quirks with how right censored data was recorded.
In particular, some early observations are recorded as ">", while in years data
they were reported as ">2419.6".  These observations are treated as equivalent.
This interpretation was confirmed in an e-mail From Toby to Curtis on 3.23.2020.
