
######################################
## Problem Set 1. 
## Data Science for Economists 
## C.U. 91621
## Armando Enriquez Z.
## Date: September 14, 2017.
#####################################


### ------------------------------------------------------
### Part 1. Data
### ------------------------------------------------------

library(AER)
data("GrowthDJ")

### ------------------------------------------------------
## Part 2.0 Rename variables names
### ------------------------------------------------------

## Some variables names are uninformative.
## Let's change some of them.

names(GrowthDJ)
library(dplyr) ## Tools for data manipulation

## Changing variables names
growth_data <- GrowthDJ %>% rename(oil_producer = oil, data_factor = inter,
                                oecd_member = oecd, gdpgrowth_av = gdpgrowth, 
                                popgrowth_av = popgrowth, invest_av = invest, 
                                school_av = school)

### -------------------------------------------------------
## Part 2.1 Create variables
### -------------------------------------------------------

## Dummies
library(dummies)

growth_data[, 1] <- dummy(growth_data[, 1]) ## For oil_producer
growth_data[, 2] <- dummy(growth_data[, 2]) ## For data_factor
growth_data[, 3] <- dummy(growth_data[, 3]) ## For oecd_member

## Interaction
growth_data$interact_gdp60_oilfactor <- growth_data$gdp60*growth_data$oil_producer

### -------------------------------------------------------
## Part 2.2 Correlation Matrix
### -------------------------------------------------------

## Selecting variables and new data frame
new_vars <- c("gdp60", "gdp85", "popgrowth_av", "invest_av", 
              "school_av", "literacy60")
growth_data_bis <- growth_data[new_vars]

## Correlation matrix
corr_mat <- cor(growth_data_bis, use = "pairwise.complete.obs")
corr_mat

## Linear regression 
mod1 <- lm(gdpgrowth_av ~ gdp60 + popgrowth_av + invest_av,
           data = growth_data)


## Another linear regression, now including school_av 
mod2 <- lm(gdpgrowth_av ~ school_av + gdp60 + popgrowth_av + invest_av,
           data = growth_data)


