# Presumpscot Water Quality Data

Analysis of volunteer water quality monitoring data from Presumpscot River Watch 
and Presumpscot Land Trust.

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our State of the Bay data
archives ensure the science underlying the 2020/2021 State of Casco Bay report
is documented and reproducible by others. The purpose of these archives is to
release  data and data analysis code whenever possible to allow others to
review, critique, learn from, and build upon CBEP science.

# Archive Structure
CBEP 2020/2021 State of the Bay data analysis summaries contain a selection of 
data,  data analysis code, and visualization code as used to produce 
results shared via our most recent State of Casco Bay report. Usually, these
archives are organized int otwo or three folders, including the following:

- `Data`  folder.  Contains data in simplified or derived form as used in our
data  analysis.  Associated metadata is contained in related Markdown documents,
usually `DATA_SOURCES.md` and `DATA_NOTES.md`.

- Analysis.  Contains one or more R Notebooks proceeding through the principal
data analysis steps that underpin SoCB reporting. To simplify the archives,
much preliminary analysis, and many analysis "dead ends" have been omitted. 

- Graphics.  Contains R Notebooks stepping through development of graphics, and
also copies of resulting graphics, usually in \*.png and \*.pdf formats.  These
graphics may differ from graphics as they appear in final State of the Bay
graphical layouts. Again, most draft versions of graphics have been omitted for 
clarity.

This repository includes no graphics code, as site-by-site variation in 
conditions strongly dominate the observed patterns, and those differences are
most easily shown via maps. The "Analysis" folder includes code that explores
ways to analyze these data to evaluate long-term trends (none were found), and
export data that could be used in GIS to produce informative maps.

# Summary of Data Sources
Presumpscot River Watch and Presumpscot Land Trust have led volunteer-based 
monitoring of water quality in the Presumpscot watershed for a couple of
decades.  More recently, the program has added monitoring of the Stroudwater
watersheds.

Data collected focuses on bacteria (*E. coli* ) and dissolved oxygen levels.
History of monitoring in the region is complex, with some sites monitored for 
just a few years, while other sites have been monitored pretty much every year 
for 20 years or so.
