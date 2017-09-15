
######################################
## Problem Set 1. 
## Data Science for Economists 
## C.U. 91621
## Armando Enriquez Z.
## Date: September 14, 2017.
#####################################


## Data
install.packages("AER")

library(AER)
data("GrowthDJ")

names(GrowthDJ)

library(dplyr)

## Changing variables names
GrowthDJ <- GrowthDJ %>% rename(oil_producer = oil, data_factor = inter,
                                oecd_member = oecd, gdpgrowth_av = gdpgrowth, 
                                popgrowth_av = popgrowth, invest_av = invest, 
                                school_av = school)

## Dummies
install.packages("dummies")
library(dummies)
GrowthDJ$oil_producer <- dummy(GrowthDJ$oil_producer)
