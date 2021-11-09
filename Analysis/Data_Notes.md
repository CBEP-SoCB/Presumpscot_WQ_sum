# Data Notes

CSV Files produced by the R Notebooks included here are complex enough so that 
it is worth providing limited metadata on the meaning of each column.

## Metadata for `DO_Results.csv`
*  Site:  Presumpscot Regional Land Trust / Presumpscot River Watch Site Code.   
*  DO_Sample:  Number of dissolved oxygen samples in last five years.  
*  DO_AB:      Number of dissolved oxygen samples (in last five years) that met
   Class B instantaneous dissolved oxygen standards.  
*  DO_C:       Number of dissolved oxygen samples (in last five years) that met
   Class C (but not class B) instantaneous dissolved oxygen standards.  
*  DO_NA:      Number of dissolved oxygen samples (in last five years) that did
   not meet Class C instantaneous dissolved oxygen standards.  
*  DO_Avg:     Mean dissolved oxygen (in mg/l) from the last five years.  
*  DO_SD:      Standard deviation of dissolved oxygen (in mg/l) from the last
   five years.   
*  PS_Sample:  Number of percent Saturation samples (in last five years).  
*  PS_AB:      Number of percent Saturation samples (in last five years) that
   met class B standards.  
*  PS_C:       Number of percent Saturation samples (in last five years) that
   met Class c (but not Class B) instantaneous Percent Saturation standards.  
*  PS_NA:      Number of percent Saturation samples (in last five years) that
   did not meet Class C instantaneous Percent Saturation standards.  
*  PS_Avg:     Mean percent saturation from the past five years.  
*  PS_SD:      Standard Deviation of percent saturation from the past five year.  
*  DO_Meets:   Allowing one exceptional observation, instantaneous 
   standard this site meets, based on dissolved oxygen data.  
*  PS_Meets:   Allowing one exceptional observation, instantaneous
   standard this site meets, based on percent saturation data.  
*  Both_Meets: Allowing one exceptional observation in each category, 
   instantaneous standard this site meets, based on both dissolved oxygen
   and percent saturation data.  
*  All_Meets:  Add the criterion that mean dissolved oxygen must be over 6.5
   mg/l to meet the Class C or better standards (this is a slight
   misapplication of state standards, which apply over a thirty day period; 
   has no effect on results in our data).  
   
## Metadata for `E_coli_Results.csv`
*  Site:  Presumpscot Regional Land Trust / Presumpscot River Watch Site Code  
*  Avg_Log:  The arithmetic mean of the natural log of the *E. coli* values.  
*  SD_Log:   The standard deviation of the of the natural log of the *E. coli*
   values.
*  gm:    Geometric Mean *E. coli* value over the most recent five years
   (2015 through 2019) in Colony  Forming Units per 100 ml, according to
   the Most Probable Number method.  
*  gm_LC: Lower 95% confidence limit for the geometric mean (based on pooled 
   estimate of variance).  
*  gm_UC: Upper 95% confidence limit for the geometric mean (based on pooled 
   estimate of variance).  
*  pFailIns:  Probability that an individual observation, over the past 5 years,
   fails the instantaneous *E. coli* limit of 236 CFU/100ml.  
*  pFail_LC:  Lower 95% confidence limit for that probability.  
*  pFail_UC:  Upper 95% confidence limit for that probability.  
*  Clss:  Water quality class this site meets based on the geometric
   mean *E. coli* values.  
*  Clss_inst_10:  Water quality class this site meets based on the geometric
   mean *E. coli* values, modified so any site with greater than a 10% 
   probability of failing instantaneous water quality criteria is classified as 
   having "Poor" water quality. Many sites get downgraded by this test.  
*  Clss_inst_20:  Water quality class this site meets based on the geometric
   mean *E. coli* values, modified so any site with greater than a 20% chance
   of failing instantaneous water quality criteria is classified as having 
   "Poor" water quality. A small number of sites get downgraded by this test.  
