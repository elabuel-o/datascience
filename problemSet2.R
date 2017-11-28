
#######################################################################
## Problem Set 2. 
## Data Science for Economists 
## C.U. 91621
## Armando Enriquez Z.
## Date: November 27, 2017.
#######################################################################

## Session info
# R version 3.3.1 
# Platform x86_64-w64 (64-bit) 


### ------------------------------------------------------------------
### Exercise 1.
### ------------------------------------------------------------------

## Loading dataset seatbelts
data("Seatbelts") ## Notice that this dataset is a time-series.

### (a) Convert the dataset to a data frame
Seatbelts <- as.data.frame(Seatbelts)
str(Seatbelts)

### (b) First regression model
lm1 <- lm(DriversKilled ~ kms + PetrolPrice, data = Seatbelts)
coefficients(lm1)

## Interpretation
## kms: Caeteris paribus, un cambio de 1 kilómetro conducido en vehículo, 
## conlleva un cambio de -0.0017 muertes de tránsito. 
## PetrolPrice: Caeteris paribus, un cambio de 1 unidad monetaria en el 
## precio de la gasolina, conlleva un cambio de -643.79 muertes de tránsito.

### (c) Predict the drivers Killed at the average kms and Petrol Prices levels. 

yhat <- predict(lm1, newdata = data.frame(kms = mean(Seatbelts$kms), 
                                  PetrolPrice = mean(Seatbelts$PetrolPrice)))


### (d) Residuals 

### Residuals 1.
### -----------------------------------------------------------
lm2 <- lm(DriversKilled ~ PetrolPrice, data = Seatbelts)
lm3 <- lm(kms ~ PetrolPrice, data = Seatbelts)

r2 <- residuals(lm2)
r3 <- residuals(lm3)

lm4 <- lm(r2 ~ r3 - 1) ## Regression without intercept of both residuals

## Comparing both coefficients. Both are the same! 
summary(lm4)$coefficients[1]
summary(lm1)$coefficients[2]
### -----------------------------------------------------------


### Residuals 2.
### -----------------------------------------------------------
lm5 <- lm(DriversKilled ~ kms, data = Seatbelts)
lm6 <- lm(PetrolPrice ~ kms, data = Seatbelts)

r5 <- residuals(lm5)
r6 <- residuals(lm6)

lm7 <- lm(r5 ~ r6 - 1) ## Regression without intercept of both residuals

## Comparing both coefficients. Both are the same!
summary(lm7)$coefficients[1]
summary(lm1)$coefficients[3]
### -----------------------------------------------------------

### (e) Now logs
lm8 <- lm(I(log(DriversKilled)) ~ kms + PetrolPrice, data = Seatbelts)
coefficients(lm8)

## Interpretation
## kms: Caeteris paribus, un cambio de 1 kilómetro conducido en vehiculo,
## conlleva un cambio de -0.0014 por ciento en las muertes de tránsito.
## PetrolPrice: Caeteris paribus, un cambio de 1 unidad monetaria en el precio 
## de la gasolina conlleva un cambio de -526.66 por ciento en las muertes de tránsito.

### (f) Adding the dummy variable law
lm9 <- lm(DriversKilled ~ kms + PetrolPrice + I(factor(law)), data = Seatbelts)
summary(lm9)

## Interpretation
## kms: Caeteris paribus, un cambio de 1 kilómetro conducido en vehiculo,
## conlleva un cambio de -0.001 en las muertes de tránsito.
## PetrolPrice: Caeteris paribus, un cambio de 1 unidad monetaria en el 
## precio de la gasolina, conlleva un cambio de -568.33 muertes de tránsito.
## law: Caeteris Paribus, en los meses con la Ley de Tránsito en vigencia 
## ocurren, en promedio, 11.89 muertes de tránsito menos que en los meses
## en los que la Ley de Tránsito no estuvo en vigencia.

## We notice that the reference is law = 0, so law = 1 is included.

## Change the reference in factor law
Seatbelts$law <- as.factor(Seatbelts$law)
lm10 <- lm(DriversKilled ~ kms + PetrolPrice + C(law, contr.treatment(2, base = 2)), 
   data = Seatbelts)
summary(lm10)
## Now the interpretation changes, because we are using law = 1 as our reference:
## Law: Caeteris paribus, en los meses con la Ley de Tránsito en vigencia 
## ocurren, en promedio, 11.89 muertes de tránsito más que en los meses 
## en los que la Ley de Tránsito estuvo en vigencia. 

### (g) Plot

library(ggplot2)
library(reshape2)
library(zoo)

mydata_ts <- Seatbelts
mydata <- data.frame(year = index(mydata_ts),
                     value = melt(mydata_ts)$value)

driver.plot <- ggplot(mydata, aes(year, value)) + 
        geom_line() + xlab("Year") + ylab("Drivers killed") + 
        labs(title = "Drivers fatalities in UK by month 1969-1984")

driver.plot
