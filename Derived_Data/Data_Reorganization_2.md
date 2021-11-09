Data Organization for Presumpscot WQ Monitoring Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
12/19/2020

  - [Inroduction](#inroduction)
  - [Import Libraries](#import-libraries)
  - [Import Data](#import-data)
      - [Load data](#load-data)
  - [Clean the Data](#clean-the-data)
  - [Prepare Data](#prepare-data)
  - [Check the *E. coli* data](#check-the-e.-coli-data)
  - [Data Completeness](#data-completeness)
  - [Exploring Field Duplicates](#exploring-field-duplicates)
      - [Identify Unflagged Duplicates](#identify-unflagged-duplicates)
      - [Flag 2012 Unflagged Duplicate](#flag-2012-unflagged-duplicate)
      - [Deal with 2017 and 2019.](#deal-with-2017-and-2019.)
          - [Generate a Vector Flagging Even
            Numbers](#generate-a-vector-flagging-even-numbers)
          - [Calculate a Selection of
            Duplicates](#calculate-a-selection-of-duplicates)
          - [Combine Selection Vectors](#combine-selection-vectors)
  - [Correcting the QC Flags](#correcting-the-qc-flags)
  - [Final Cleanup](#final-cleanup)
  - [Save Data as CSV](#save-data-as-csv)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

# Inroduction

This Notebook documents how raw data was converted to the working data.
Preliminary analysis reveals that 2018 and 2019 data were delivered
incomplete, without all supplementary data. While for the most part,
that simply reduces analytic options, it also drops the flags indicating
QA/QC duplicates. Here I have added arbitrary flags to identify one of
each replicate sample as as QA/QC sample.

# Import Libraries

``` r
library(readxl)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.2     v purrr   0.3.4
#> v tibble  3.0.4     v dplyr   1.0.2
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.0
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
```

# Import Data

``` r
sibfldnm <- 'Original_Data'
parent <- dirname(getwd())
sibling <- file.path(parent,sibfldnm)
fn <- 'Historical_Data_Master_CORRECTED.xlsx'
```

## Load data

We load the data selectively, eliminating data we will not or can not
analyze. Note that the QC Code, in the corrected Excel spreadsheet
contains “NA” as a string when there was no other indication of contents
of the QC flag, but only up through 2017. For 2018 and 2019, this
columna is empty.

``` r
suppressWarnings(Prelim_presumpscot_data2 <- read_excel(file.path(sibling, fn),
                                      col_types = c("skip", "skip",
                                                    "text", "text",
                                                    "skip", "numeric",  #Year
                                                    "date","date",
                                                    "text", "numeric",
                                                    "skip", "text",
                                                    "numeric", "numeric",
                                                    "numeric", "numeric", #spcond
                                                    "skip", "skip",
                                                    "skip", "skip",
                                                    "skip", "text",       #Ecoli
                                                    "skip", "skip", 
                                                    "skip", "skip",
                                                    "skip", "skip",
                                                    "skip", "skip",   
                                                    "skip", "text",   #Sampled by
                                                    "text", "text",
                                                    "numeric", "text",
                                                    "text", "skip",
                                                    "text", "text",
                                                    "skip", "skip",
                                                    "text", "text")))
```

``` r
names(Prelim_presumpscot_data2)
#>  [1] "Organization Site Code"          "VRMP Site ID"                   
#>  [3] "Year"                            "Date"                           
#>  [5] "Time"                            "QC Type"                        
#>  [7] "Sample Depth"                    "Flow"                           
#>  [9] "Water Temperature (DEG C)"       "DissolvedOxygen"                
#> [11] "Dissolved Oxygen Saturation (%)" "Specific Conductance (US/CM)"   
#> [13] "Ecoli"                           "Sampled By"                     
#> [15] "Current Weather"                 "Past 24HR Weather"              
#> [17] "Air Temperature (DEG C)"         "Air Condition"                  
#> [19] "Sample Location"                 "Stage"                          
#> [21] "Habitat"                         "Water Appearance"               
#> [23] "Comments"
```

# Clean the Data

We rename variables and convert many to factors (without checking
levels).

``` r
Prelim_presumpscot_data2 <- Prelim_presumpscot_data2 %>%
  rename(Site = `Organization Site Code`,
         Name = `VRMP Site ID`,
         Year = Year,
         Date = Date,
         Time = Time,
         QC = `QC Type`,
         Depth = `Sample Depth`,
         Flow = Flow,
         Temp = `Water Temperature (DEG C)`,
         DO = DissolvedOxygen,
         PctSat = `Dissolved Oxygen Saturation (%)`,
         SpCond = `Specific Conductance (US/CM)`,
         Ecoli = Ecoli,
         Sampled_By = `Sampled By`,
         Weather = `Current Weather`,
         Recent_Weather = `Past 24HR Weather`,
         AirTemp = `Air Temperature (DEG C)`,
         Condition = `Air Condition`,
         Location = `Sample Location`,
         Stage = Stage,
         Habitat = Habitat,
         Appearance = `Water Appearance`,
         Comments = Comments
  )


Prelim_presumpscot_data2 <- Prelim_presumpscot_data2 %>%
  mutate_at(c('Site', 'Flow', 'Sampled_By', 'Weather', 'Recent_Weather',
             'Condition', 'Location', 'Stage', 'Habitat', 'Appearance'), ~ factor(.)) %>%
  mutate(Year = as.numeric(format(Date, '%Y')))

summary(Prelim_presumpscot_data2)
#>       Site          Name                Year           Date                    
#>  P030   : 169   Length:2582        Min.   :2009   Min.   :2009-05-16 00:00:00  
#>  P020   : 161   Class :character   1st Qu.:2012   1st Qu.:2012-05-19 00:00:00  
#>  PI010  :  97   Mode  :character   Median :2015   Median :2015-07-11 00:00:00  
#>  OB010  :  96                      Mean   :2015   Mean   :2015-03-11 02:28:21  
#>  PI020  :  93                      3rd Qu.:2018   3rd Qu.:2018-06-17 00:00:00  
#>  M010   :  91                      Max.   :2019   Max.   :2019-09-21 00:00:00  
#>  (Other):1875                                                                  
#>       Time                          QC                Depth      
#>  Min.   :1899-12-31 05:05:00   Length:2582        Min.   :0.000  
#>  1st Qu.:1899-12-31 06:35:00   Class :character   1st Qu.:0.000  
#>  Median :1899-12-31 07:06:00   Mode  :character   Median :1.000  
#>  Mean   :1899-12-31 07:06:44                      Mean   :1.275  
#>  3rd Qu.:1899-12-31 07:35:00                      3rd Qu.:2.000  
#>  Max.   :1899-12-31 10:20:00                      Max.   :5.000  
#>  NA's   :756                                      NA's   :2440   
#>         Flow           Temp             DO             PctSat      
#>  BASEFLOW :1238   Min.   : 8.00   Min.   : 0.500   Min.   :  5.30  
#>  STORMFLOW: 236   1st Qu.:15.90   1st Qu.: 7.250   1st Qu.: 81.50  
#>  NA's     :1108   Median :18.30   Median : 8.085   Median : 88.30  
#>                   Mean   :18.48   Mean   : 7.990   Mean   : 85.53  
#>                   3rd Qu.:21.30   3rd Qu.: 8.870   3rd Qu.: 93.60  
#>                   Max.   :27.70   Max.   :18.300   Max.   :127.70  
#>                   NA's   :1029    NA's   :342      NA's   :330     
#>      SpCond         Ecoli                   Sampled_By            Weather    
#>  Min.   :  7.8   Length:2582        LYNDA REED   : 224   CLEAR        : 752  
#>  1st Qu.: 60.5   Class :character   FRED DILLON  : 131   CLOUDY       : 218  
#>  Median :127.2   Mode  :character   TOM TERO     : 103   PARTLY CLOUDY: 129  
#>  Mean   :147.4                      RICK COPELAND:  89   FOGGY        :  55  
#>  3rd Qu.:210.0                      LANCE GURNEY :  79   MOSTLY CLOUDY:  48  
#>  Max.   :538.0                      (Other)      :1206   (Other)      : 293  
#>  NA's   :1811                       NA's         : 750   NA's         :1087  
#>               Recent_Weather    AirTemp             Condition       Location  
#>  CLEAR               : 435   Min.   : 4.444   BREEZE     : 108   BANK   :497  
#>  PARTLY CLOUDY       : 132   1st Qu.:12.780   CALM       :1112   BOAT   : 35  
#>  CLEAR, PARTLY CLOUDY: 103   Median :15.560   STRONG WIND:   3   BRIDGE :292  
#>  CLOUDY              :  59   Mean   :15.739   NA's       :1359   CULVERT:155  
#>  CLOUDY, LIGHT RAIN  :  57   3rd Qu.:18.330                      DOCK   :  7  
#>  (Other)             : 740   Max.   :70.000                      WADING :786  
#>  NA's                :1056   NA's   :1405                        NA's   :810  
#>     Stage         Habitat              Appearance     Comments        
#>  HIGH  : 258   CASCADE:   9   CLEAR         : 783   Length:2582       
#>  LOW   : 403   RIFFLE : 426   MEDIUM STAINED: 326   Class :character  
#>  MEDIUM: 831   RUN    :1050   TURBID        : 199   Mode  :character  
#>  NA's  :1090   NA's   :1097   DARKLY STAINED:  78                     
#>                               MILKY         :  58                     
#>                               (Other)       :  53                     
#>                               NA's          :1085
```

Note that many of the descriptive data fields are incomplete, with many
NAs. While not evident from the summaries, most were not reported for
the 2018 and 2019 data. (Presumably that is because these data were hand
entered into the spreadsheet by PRLT, not accessed from the complete
records in DEP’s EGAD data management system.) Because those data are
incomplete, we functionally can not analyze them, so we will remove them
from further consideration.

# Prepare Data

We remove qualitative data we can not analyze, and split the *E. coli*
data into data censoring flags and numerical values.

``` r
presumpscot_data <- Prelim_presumpscot_data2 %>%
  select(-Flow, -SpCond, -Sampled_By, -Weather, -Recent_Weather, -AirTemp,
         -Condition, -Location, - Stage, -Habitat, -Appearance, -Comments) %>%
  mutate(qualifier = if_else(substr(Ecoli,1,1) %in% c('<', '>'),
                            substr(Ecoli,1,1), "" ))  %>%
  mutate(Ecoli = if_else(Ecoli=='>', '>2419.6', Ecoli)) %>%
  mutate(value = if_else(nchar(qualifier)>0,
                       as.numeric(substr(Ecoli,2,nchar(Ecoli))),
                       as.numeric(Ecoli))) %>%
  arrange(Date, Site)
#> Warning: Problem with `mutate()` input `value`.
#> i NAs introduced by coercion
#> i Input `value` is `if_else(...)`.
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

The warning appears to be generated inside the `if_else()` call, if the
value of Ecoli is just a single digit. Those values are coerced to NA by
the `TRUE` branch of the `if_else()` call, but then not used, as none of
those are either left or right censored.

# Check the *E. coli* data

1.  Did we generate any new NAs?
2.  Did left censored values get converted appropriately?
3.  Did right censored vlaues get converted appropriately?
4.  What do high uncensored values look like?

<!-- end list -->

``` r
presumpscot_data %>% select(Ecoli, qualifier, value) %>% filter( is.na(value) & ! is.na(Ecoli))
#> # A tibble: 0 x 3
#> # ... with 3 variables: Ecoli <chr>, qualifier <chr>, value <dbl>
presumpscot_data %>% select(Ecoli, qualifier, value) %>% filter( qualifier == '<')
#> # A tibble: 1 x 3
#>   Ecoli qualifier value
#>   <chr> <chr>     <dbl>
#> 1 <1    <             1
presumpscot_data %>% select(Ecoli, qualifier, value) %>% filter( qualifier == '>')
#> # A tibble: 70 x 3
#>    Ecoli   qualifier value
#>    <chr>   <chr>     <dbl>
#>  1 >2419.6 >         2420.
#>  2 >2419.6 >         2420.
#>  3 >2419.6 >         2420.
#>  4 >2419.6 >         2420.
#>  5 >2419.6 >         2420.
#>  6 >2419.6 >         2420.
#>  7 >2419.6 >         2420.
#>  8 >2419.6 >         2420.
#>  9 >2419.6 >         2420.
#> 10 >2419.6 >         2420.
#> # ... with 60 more rows
presumpscot_data %>% select(Ecoli, qualifier, value) %>% filter( qualifier == '' & value >2410)
#> # A tibble: 42 x 3
#>    Ecoli              qualifier value
#>    <chr>              <chr>     <dbl>
#>  1 2419.17            ""        2419.
#>  2 2419.17            ""        2419.
#>  3 2419.17            ""        2419.
#>  4 2419.1999999999998 ""        2419.
#>  5 2419.17            ""        2419.
#>  6 2419.17            ""        2419.
#>  7 2419.17            ""        2419.
#>  8 2419.6             ""        2420.
#>  9 2419.6             ""        2420.
#> 10 2419.6             ""        2420.
#> # ... with 32 more rows
```

So, we’ve addressed all the right censored observations (no new NAs) and
there is formally only a single left censored observation\! That’s
pretty remarkable. And it means we can ignore it for any practical
purpose. In effect, we can treat these data as only right censored, not
left censored.

Note that some right censored values appear to have been inconsistently
coded as 2419.2 instead of 2419.6. I suspect those are errors in coding,
but the error is so small that they won’t matter in any analysis, so we
leave them unchanged.

Similarly, there are (uncensored) values recorded at 2419.17, 2419.2,
and 2419.6. Again, my guess is these all represent maximum observable
values, coded differently. But differences won’t matter, so we leave
them unaltered.

# Data Completeness

We note that some data fields are absent for recent years. We believe
that’s because the data file was partially downloaded from DEP, and
partially entered directly by PRLT.

``` r
presumpscot_data %>%
  group_by(Year) %>%
  mutate(QC = if_else(QC == "NA", NA_character_, QC)) %>%  # replace "NA"s
  summarize_at(c("Time", "QC", "Depth", "Temp", "DO", "PctSat", "Ecoli"), 
               function(x) sum(! is.na(x)))
#> # A tibble: 11 x 8
#>     Year  Time    QC Depth  Temp    DO PctSat Ecoli
#>    <dbl> <int> <int> <int> <int> <int>  <int> <int>
#>  1  2009   198    18     0   174   175    172   177
#>  2  2010   178    20    31   149   151    148   153
#>  3  2011   243     6    61   135   219    134   154
#>  4  2012   204    12    50   185   172    184   153
#>  5  2013   187    22     0   168   169    169   184
#>  6  2014   192    25     0   181   178    181   188
#>  7  2015   178    31     0   149   141    149   171
#>  8  2016   102     5     0    95    93     96   102
#>  9  2017   338    13     0   311   255    312   318
#> 10  2018     6     0     0     6   336    343   360
#> 11  2019     0     0     0     0   351    364   368
```

The biggest issue here is the QC flag, which marks field duplicate
samples.  
That was not reported in 2019. And it appears the QC flag was used
inconsistently at other times.

Lets see if there are any field duplicates in those years.

# Exploring Field Duplicates

Check for field duplicates in *E. coli* and DO data.

``` r
tmp <- presumpscot_data %>%
  mutate(QC = replace_na(QC, "NA")) %>%  # replace NAs so they can be counted
  group_by(Date, Site) %>%
  summarize(nColi= sum(!is.na(Ecoli) ),
            nDO = sum(!is.na(DO)),
            is_D = any(QC == 'D'),
            Year = min(Year),
            .groups = 'drop') %>%
  filter(nColi>1 | nDO>1) %>%
  arrange(Year, Date)
tmp
#> # A tibble: 214 x 6
#>    Date                Site  nColi   nDO is_D   Year
#>    <dttm>              <fct> <int> <int> <lgl> <dbl>
#>  1 2009-05-16 00:00:00 M030      2     1 TRUE   2009
#>  2 2009-05-16 00:00:00 OB010     2     1 TRUE   2009
#>  3 2009-05-30 00:00:00 N010      2     1 TRUE   2009
#>  4 2009-05-30 00:00:00 P170      2     1 TRUE   2009
#>  5 2009-06-13 00:00:00 N010      2     1 TRUE   2009
#>  6 2009-06-13 00:00:00 P020      2     1 TRUE   2009
#>  7 2009-06-13 00:00:00 PI020     2     1 TRUE   2009
#>  8 2009-06-27 00:00:00 P089      2     1 TRUE   2009
#>  9 2009-06-27 00:00:00 PI020     2     1 TRUE   2009
#> 10 2009-06-27 00:00:00 PL030     2     0 TRUE   2009
#> # ... with 204 more rows
```

That shows many field duplicates in 2017 (including 22 for *E coli*),
none in 2018, but eight (all for Dissolved oxygen) in 2019.

## Identify Unflagged Duplicates

``` r
tmp  %>%
  filter(nColi > 1) %>%
  filter(! is_D)
#> # A tibble: 8 x 6
#>   Date                Site  nColi   nDO is_D   Year
#>   <dttm>              <fct> <int> <int> <lgl> <dbl>
#> 1 2012-08-25 00:00:00 BL010     2     2 FALSE  2012
#> 2 2017-05-20 00:00:00 P135      2     2 FALSE  2017
#> 3 2017-06-03 00:00:00 DG010     2     2 FALSE  2017
#> 4 2017-06-03 00:00:00 P135      2     2 FALSE  2017
#> 5 2017-06-17 00:00:00 P060      2     2 FALSE  2017
#> 6 2017-07-15 00:00:00 P060      2     1 FALSE  2017
#> 7 2017-08-12 00:00:00 P060      2     2 FALSE  2017
#> 8 2017-08-26 00:00:00 P060      2     2 FALSE  2017
```

There appears to be one duplicate sample in 2012 that was not coded as a
duplicate, and duplicates are inconsistently coded in 2017. Duplicates
are absent in 2018, and never coded in 2019.

``` r
tmp  %>%
  filter(nDO > 1) %>%
  filter(! is_D)
#> # A tibble: 61 x 6
#>    Date                Site  nColi   nDO is_D   Year
#>    <dttm>              <fct> <int> <int> <lgl> <dbl>
#>  1 2010-07-03 00:00:00 P020      1     3 FALSE  2010
#>  2 2010-07-03 00:00:00 P030      1     2 FALSE  2010
#>  3 2010-07-17 00:00:00 P020      1     3 FALSE  2010
#>  4 2010-07-17 00:00:00 P030      1     4 FALSE  2010
#>  5 2010-07-31 00:00:00 P020      1     3 FALSE  2010
#>  6 2010-07-31 00:00:00 P030      1     3 FALSE  2010
#>  7 2010-08-14 00:00:00 P020      0     2 FALSE  2010
#>  8 2010-08-28 00:00:00 P020      1     2 FALSE  2010
#>  9 2010-08-28 00:00:00 P030      1     2 FALSE  2010
#> 10 2010-09-11 00:00:00 P020      1     2 FALSE  2010
#> # ... with 51 more rows
```

Many of these are not a problem, as they were collected at different
depths, especially in 2010, 2011, and 2012. However, we have corrections
to make in 2017 and 2019.

## Flag 2012 Unflagged Duplicate

The 2012 mis-coded sample is for Site = BL010, Date = 2012-08-25. The
two samples from that year certainly look like field duplicates. All
parameters are similar. This can be corrected by recoding the one with
Temp == 17.2 as a Duplicate.

``` r
index2012 <- with(presumpscot_data, (Date == as.Date("2012-08-25") &
                       Site == "BL010" &
                       Temp == 17.2) )
presumpscot_data[index2012,]
#> # A tibble: 1 x 13
#>   Site  Name   Year Date                Time                QC    Depth  Temp
#>   <fct> <chr> <dbl> <dttm>              <dttm>              <chr> <dbl> <dbl>
#> 1 BL010 BLAC~  2012 2012-08-25 00:00:00 1899-12-31 07:21:00 NA       NA  17.2
#> # ... with 5 more variables: DO <dbl>, PctSat <dbl>, Ecoli <chr>,
#> #   qualifier <chr>, value <dbl>
```

## Deal with 2017 and 2019.

It’s a little more complicated for 2017 and 2019. Our goal is to
randomly flag half of the observations as duplicates, in case we chose
to drop replicates for some reason. We note that in these data, QA/QC
duplicates are always present as sequential pairs (because of how we
sorted the data). We arbitrarily decide that the one occupying an
even-numbered row is the QA/QC replicate, while the other is the
“original” observation.

### Generate a Vector Flagging Even Numbers

``` r
len = length(presumpscot_data$Ecoli)
evens <- rep(c(FALSE, TRUE),len%/% 2)
if (length(evens) < len) append(evens, TRUE)
```

### Calculate a Selection of Duplicates

``` r
selection <- presumpscot_data %>%
  mutate(QC = replace_na(QC, "NA")) %>%  # replace NAs so they can be counted
  group_by(Site, Date) %>%
  mutate(nColi= sum(!is.na(Ecoli)),
         nDO = sum(!is.na(DO)),
         D = any(QC == "D")) %>%
  ungroup(Site, Date) %>%
  mutate(test = (nColi > 1 | nDO > 1) & Year==2017 & !D)
```

``` r
selection <- selection %>%
  pull(test)
```

### Combine Selection Vectors

Demonstrate that the method works.

``` r
index2017 <- selection & ! evens
sum(index2017)
#> [1] 9
```

Here are the rows it selects. These are one half of each pair of
duplicates in 2017, selected to get the value `QC <- 'D'`.

``` r
presumpscot_data[index2017,] %>%
  select(Site, Year, Date, QC, Temp, DO, PctSat) %>%
  arrange(Site, Date)
#> # A tibble: 9 x 7
#>   Site   Year Date                QC     Temp    DO PctSat
#>   <fct> <dbl> <dttm>              <chr> <dbl> <dbl>  <dbl>
#> 1 DG010  2017 2017-05-20 00:00:00 NA     15.8  7.99   80.4
#> 2 DG010  2017 2017-06-03 00:00:00 NA     12.4  9.09   84.5
#> 3 P060   2017 2017-06-17 00:00:00 NA     14.8  8.37   82.5
#> 4 P060   2017 2017-07-15 00:00:00 NA     16.2  8.2    82.5
#> 5 P060   2017 2017-08-12 00:00:00 NA     18.5  7.57   80.8
#> 6 P060   2017 2017-08-26 00:00:00 NA     22.6  7.85   91.2
#> 7 P060   2017 2017-09-23 00:00:00 NA     15.3  7.5    74.8
#> 8 P135   2017 2017-05-20 00:00:00 NA     13.3  9.77   93  
#> 9 P135   2017 2017-06-03 00:00:00 NA     13.8 10.4   101.
```

It works for 2019 too. Here it flags all the 2019 DO duplicates, and
selects one of each pair to be labeled wit the ‘D’.

``` r
selection <- presumpscot_data %>%
  mutate(QC = replace_na(QC, "NA")) %>%  # replace NAs so they can be counted
  group_by(Site, Date) %>%
  mutate(nColi= sum(! is.na(Ecoli)),
         nDO = sum(! is.na(DO)),
         D = any(QC == "D")) %>%
  ungroup(Site, Date) %>%
  mutate(test = (nColi > 1 | nDO > 1) & Year==2019) %>% # & !D) %>%
  pull(test)

sum(selection)
#> [1] 16

index2019 <- selection & evens
sum(index2019)
#> [1] 8

presumpscot_data[index2019,] %>%
  select(Site, Year, Date, QC, Temp, DO, PctSat) %>%
  arrange(Site, Date)
#> # A tibble: 8 x 7
#>   Site   Year Date                QC     Temp    DO PctSat
#>   <fct> <dbl> <dttm>              <chr> <dbl> <dbl>  <dbl>
#> 1 BB010  2019 2019-06-01 00:00:00 <NA>     NA  9.97   97.4
#> 2 BB010  2019 2019-06-15 00:00:00 <NA>     NA  8.7    85  
#> 3 OB010  2019 2019-08-10 00:00:00 <NA>     NA  5.02   48.6
#> 4 OB010  2019 2019-08-24 00:00:00 <NA>     NA  5.03   47.7
#> 5 OB010  2019 2019-09-07 00:00:00 <NA>     NA  6.32   83.2
#> 6 OB010  2019 2019-09-21 00:00:00 <NA>     NA  6.85   61.3
#> 7 P030   2019 2019-07-27 00:00:00 <NA>     NA  7.82   96  
#> 8 SW010  2019 2019-07-27 00:00:00 <NA>     NA  6.54   73.5
```

# Correcting the QC Flags

We finally alter the data set to flag the field duplicates we have
identified.

``` r
presumpscot_data2 <- presumpscot_data%>%
  mutate(QC = ifelse(index2012 | index2017 | index2019,
                     "D", as.character(QC))) %>%
  mutate(QC=factor(QC))
```

# Final Cleanup

``` r
presumpscot_data2 <- presumpscot_data2 %>%
  mutate(Flag= (qualifier == '>')) %>%
  select(-Ecoli, -qualifier) %>%
  rename(Ecoli = value)
```

# Save Data as CSV

``` r
tmp <- presumpscot_data2 %>%
  mutate(Time=strftime(Time, format = "%H:%M:%S", tz="GMT"))
write.csv(tmp, 'presumpscot_CORRECTED.csv')
```
