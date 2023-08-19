# NAMCS Project general
Descriptive summaries
 - Age check for extension: checking distribution of ages for each of the 4 types of physicians from 2012 to 2016, to check if we can use age as a proxy variable to classify physicians in the 2018 and 2019 data sets
 - Visit records summary: Descriptive summary of number of nonsurgical visits to physicians in NAMCS data from 1993 to 2019, for children (under 18) and infants (under 1)

General time series
 - Under 1: Time series for infants (under 1), from 1993 to 2016 (original) and 1993 to 2019 (extended)
 - Under 18: Time series for children (under 18), from 1993 to 2016 (original) and 1993 to 2019 (extended)
* Note: I perform the analysis for both children and infants from 1993 to 2016 in the “original” R script. I then save the data from 1993 to 2016 as a csv file, and import this csv in the “extended” R script when creating the extended time series from 1993 to 2019.

Time series by demographic variables
 - Payment type: Time series for children (under 18), by payment type (private insurance OR Medicaid), from 1997 to 2016
 - Race and ethnicity:  Time series for children (under 18), by race (white OR black) and ethnicity (hispanic), from 1993 to 2016
 - MSA: Time series for children (under 18), by MSA (urban OR rural), from 1993 to 2016

Binary response models
 - Binary response models: Linear probability, logit and probit models using pooled data from 1997 to 2016 NAMCS. probability of visit to pediatrician ~ race + ethnicity + payment type + MSA + year dummy variables. Output tables in RMarkdown file.
