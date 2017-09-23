
#######################################################################
## Problem Set 1. 
## Data Science for Economists 
## C.U. 91621
## Armando Enriquez Z.
## Date: September 22, 2017.
#######################################################################

## Session info
# R version 3.3.1 
# Platform x86_64-w64 (64-bit) 

rm(list = ls())

### ------------------------------------------------------------------
### Part 1.1 and 1.2 Data structure and loading of AER package
### ------------------------------------------------------------------

library(AER)
data("GrowthDJ")
str(GrowthDJ)

### ------------------------------------------------------------------
## Part 2.0 Rename variables names
### ------------------------------------------------------------------

## Some variables names are uninformative.
## Let's change some of them.

names(GrowthDJ)
library(dplyr) ## Tools for data manipulation

## Changing variables names
growth_data <- GrowthDJ %>% rename(oil_producer = oil, data_factor = inter,
                                oecd_member = oecd, gdpgrowth_av = gdpgrowth, 
                                popgrowth_av = popgrowth, invest_av = invest, 
                                school_av = school)

### ------------------------------------------------------------------
## Part 2.1 Create variables
### ------------------------------------------------------------------

## a) Dummies
library(dummies)

growth_data[, 1] <- dummy(growth_data[, 1]) ## For oil_producer
growth_data[, 2] <- dummy(growth_data[, 2]) ## For data_factor
growth_data[, 3] <- dummy(growth_data[, 3]) ## For oecd_member

## b) Interaction
growth_data$interact_gdp60_oilfactor <- growth_data$gdp60*growth_data$oil_producer

### -----------------------------------------------------------------
## Part 2.2 Correlation Matrix
### -----------------------------------------------------------------

## a) Selecting variables and new data frame
## We will call the new data frame as growth_data_subset
new_vars <- c("gdp60", "gdp85", "popgrowth_av", "invest_av", 
              "school_av", "literacy60")
growth_data_subset <- growth_data[new_vars]

## b) Construction of Correlation matrix
corr_mat <- cor(growth_data_subset, use = "pairwise.complete.obs")

## c) Corrleation matrix report
corr_mat 
# Let's make a matrix correlation plot to better visualize correlation
pairs(~gdp60 + gdp85 + popgrowth_av + invest_av + school_av + literacy60,
      data = growth_data_subset, main = "Growth Correlation Matrix")


## d)Linear regression 
mod1 <- lm(gdpgrowth_av ~ gdp60 + popgrowth_av + invest_av,
           data = growth_data)
## The estimated beta coefficients of the three explanatory variables 
## (gdp60, popgrowht_av and invest_av) indicate a change on response
## (important: no causation) in the outcome variable (in this case gdpgrowth_av)
## related with a change in the explanatory variabes.
## The beta coefficient formula involves the covariance between the explanatory 
## and outcome variables wheighted by the explanatory variable variance, 
## whereas the correlation coefficient involves the same covariance weighted
## by the product of the standard deviations of both the explanatory and 
## the outcome variables

## Interpretaciones:
## 1. gdp60: Caeteris paribus, un aumento de 1 unidad del producto interno bruto 
## per cápita en 1960 conlleva una disminución promedio de 0.00007835 puntos 
## porcentuales en la tasa de crecimiento promedio del GDP entre 1960 y 1985.

## 2. popgrowth_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## tasa de crecimiento pormedio de la población entre 1960 y 1985 conlleva un
## aumento promedio de 0.9973 puntos porcentuales en la tasa de crecimiento 
## promedio del GDP entre 1960 y 1985.

## 3. invest_av: Caeteris paribus, un aumento de de 1 punto porcentual en el 
## ratio de inverstion a GDP de 1960 a 1985 conlleva un aumento promedio de
## 0.1425 puntos porcentuales en la tasa de crecimiento promedio del GDP 
## enre 1960 y 1985.

## All these coefficients are statistically significat.

## e) Another linear regression, now including school_av 
mod2 <- lm(gdpgrowth_av ~ school_av + gdp60 + popgrowth_av + invest_av,
           data = growth_data)
## Interpretaciones:

## 1. school_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## proporción de gente en edad de trabajar inscrita en escuela secundaria de 
## 1960 a 1985 conlleva un aumento promedio de 0.09074 puntos porcentuales 
## en la tasa de crecimiento promedio del GDP entre 196 y 1985. 

## 1. gdp60: Caeteris paribus, un aumento de 1 unidad del producto interno bruto 
## per cápita en 1960 conlleva una disminución promedio de 0.00009189 puntos 
## porcentuales en la tasa de crecimiento promedio del GDP entre 1960 y 1985.

## 2. popgrowth_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## tasa de crecimiento pormedio de la población entre 1960 y 1985 conlleva un
## aumento promedio de 1.030 puntos porcentuales en la tasa de crecimiento 
## promedio del GDP entre 1960 y 1985.

## 3. invest_av: Caeteris paribus, un aumento de de 1 punto porcentual en el 
## ratio de inverstion a GDP de 1960 a 1985 conlleva un aumento promedio de
## 0.120 puntos porcentuales en la tasa de crecimiento promedio del GDP 
## enre 1960 y 1985.

## All these coefficients are statistically significat, except school_av. 

## f) Incluirías la variable school_av en la regresión? 

## h) analiza las R^2 de incisos e) y f) Contrastalas. Con cual te quedas? 

### -----------------------------------------------------------------
## Part 2.3 Dummy variables and interactions
### -----------------------------------------------------------------

## a) regresión del inciso f) pero con oil_factor
## b) agregar interacción


### -----------------------------------------------------------------
## Part 2.4 Plot
### -----------------------------------------------------------------

## Plot between GDP60 (y-axis) and oil_factor

### -----------------------------------------------------------------
## Part 3. Linear Relationships
### -----------------------------------------------------------------

## 
