
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
summary.lm(mod1)
## The estimated beta coefficients of the three explanatory variables 
## (gdp60, popgrowht_av and invest_av) indicate a change on response
## (important: no causation) in the outcome variable (in this case gdpgrowth_av)
## related with a change in the explanatory variabes.
## The beta coefficient formula involves the covariance between the explanatory 
## and outcome variables wheighted by the explanatory variable variance, 
## whereas the correlation coefficient involves the same covariance weighted
## by the product of the standard deviations of both the explanatory and 
## the outcome variables.
## Both have the same numerator (and both have positive denominators, hence the
## signs are the same!). One key diference in terms of interpretation 
## is that the beta coefficient has units, 
## whereas the correlation coefficient does not. 

## Interpretaciones:
## 1. gdp60: Caeteris paribus, un aumento de 1 unidad del producto interno bruto 
## per c�pita en 1960 conlleva una disminuci�n promedio de 0.00007835 puntos 
## porcentuales en la tasa de crecimiento promedio del GDP per capita entre 1960 y 1985.

## 2. popgrowth_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## tasa de crecimiento pormedio de la poblaci�n entre 1960 y 1985 conlleva un
## aumento promedio de 0.9973 puntos porcentuales en la tasa de crecimiento 
## promedio del GDP per capita entre 1960 y 1985.

## 3. invest_av: Caeteris paribus, un aumento de de 1 punto porcentual en el 
## ratio de inverstion a GDP de 1960 a 1985 conlleva un aumento promedio de
## 0.1425 puntos porcentuales en la tasa de crecimiento promedio del GDP per capita 
## entre 1960 y 1985.

## All these coefficients are statistically significat.

## e) Another linear regression, now including school_av 
mod2 <- lm(gdpgrowth_av ~ school_av + gdp60 + popgrowth_av + invest_av,
           data = growth_data)
summary.lm(mod2)
## Interpretaciones:
## 1. school_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## proporci�n de gente en edad de trabajar inscrita en escuela secundaria de 
## 1960 a 1985 conlleva un aumento promedio de 0.09074 puntos porcentuales 
## en la tasa de crecimiento promedio del GDP per capita entre 196 y 1985. 

## 2. gdp60: Caeteris paribus, un aumento de 1 unidad del producto interno bruto 
## per c�pita en 1960 conlleva una disminuci�n promedio de 0.00009189 puntos 
## porcentuales en la tasa de crecimiento promedio del GDP per capita entre 1960 y 1985.

## 3. popgrowth_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## tasa de crecimiento pormedio de la poblaci�n entre 1960 y 1985 conlleva un
## aumento promedio de 1.030 puntos porcentuales en la tasa de crecimiento 
## promedio del GDP per capita entre 1960 y 1985.

## 4. invest_av: Caeteris paribus, un aumento de de 1 punto porcentual en el 
## ratio de inversion a GDP de 1960 a 1985 conlleva un aumento promedio de
## 0.120 puntos porcentuales en la tasa de crecimiento promedio del GDP per capita 
## enre 1960 y 1985.

## All these coefficients are statistically significat, except school_av. 

## f) �Incluir�as la variable school_av en la regresi�n? 
## En caso afirmativo, especifica el modelo que correr�as.
## En caso contrario, �qu� variable sustituir�as?
## RESPUESTA: S� agregar�a la variable school_av. La teor�a econ�mica, en
## espec�fico la del crecimiento y el capital humano, predice que un mayor
## nivel de capital humano es consistente con mayores tasas y niveles de 
## crecimiento de las econom�as.
## El modelo que especificar�a es uno que incluya las variables del inciso (e),
## incluyendo adem�s gdp85.

## New model fit:
mod3 <- lm(gdpgrowth_av ~ school_av + gdp60 + gdp85 + popgrowth_av + invest_av,
                   data = growth_data)
summary.lm(mod3)

## h) analiza las R^2 de incisos e) y f) Contrastalas.
## �Con cual te quedas? 
## �Cual regresi�n explica m�s la variabilidad? 
summary(mod2)$adj.r.squared
summary(mod3)$adj.r.squared

## Siempre que a�adamos variables incrementamos la R^2, misma que es una 
## medida de bondad de ajuste. Por lo tanto nos fijamos en las adjusted R^2:
## model2: 0.38
## model3: 0.43
## El modelo que explica m�s la variablidad de gdpgrowth_av es aquel que 
## incluye a la variable gdp85. Me quedo con este �ltimo modelo.

### -----------------------------------------------------------------
## Part 2.3 Dummy variables and interactions
### -----------------------------------------------------------------

## a) regresi�n del inciso f) pero con oil_factor

mod4 <- lm(gdpgrowth_av ~ school_av + gdp60 + gdp85 + popgrowth_av + 
                           invest_av + oil_producer, data = growth_data)
summary.lm(mod4)

## Interpretaciones:
## 1. school_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## proporci�n de gente en edad de trabajar inscrita en escuela secundaria de 
## 1960 a 1985 conlleva un aumento promedio de 0.02785 puntos porcentuales 
## en la tasa de crecimiento promedio del GDP per capita entre 196 y 1985. 

## 2. gdp60: Caeteris paribus, un aumento de 1 unidad del producto interno bruto 
## per c�pita en 1960 conlleva una disminuci�n promedio de 0.0001752 puntos 
## porcentuales en la tasa de crecimiento promedio del GDP per capita entre 1960 y 1985.

## 3. gdp85: gdp60: Caeteris paribus, un aumento de 1 unidad del producto interno bruto 
## per c�pita en 1960 conlleva un aumento promedio de 0.0001577 puntos 
## porcentuales en la tasa de crecimiento promedio del GDP per capita entre 1960 y 1985.

## 4. popgrowth_av: Caeteris paribus, un aumento de 1 punto porcentual en la 
## tasa de crecimiento pormedio de la poblaci�n entre 1960 y 1985 conlleva un
## aumento promedio de 1.194 puntos porcentuales en la tasa de crecimiento 
## promedio del GDP per capita entre 1960 y 1985.

## 5. invest_av: Caeteris paribus, un aumento de de 1 punto porcentual en el 
## ratio de inversion a GDP de 1960 a 1985 conlleva un aumento promedio de
## 0.08901 puntos porcentuales en la tasa de crecimiento promedio del GDP per capita 
## enre 1960 y 1985.

## 6. oil_producer: Caeteris paribus, la diferencia promedio de tasas de 
## crecimiento promedio del GDP per capita entre 1960 y 1985 entre pa�ses productores y 
## pa�ses no productores de petr�leo es de 1.71 puntos porcentuales.

## All coefficients are statistically significant, except school_av.

## b) agregar interacci�n
## �C�mo afecta a los coeficientes? 
mod5 <- lm(gdpgrowth_av ~ school_av + gdp60 + gdp85 + popgrowth_av + 
                           invest_av + oil_producer + 
                   interact_gdp60_oilfactor, data = growth_data)
summary.lm(mod5)
## Al a�adir la interacci�n, los coeficientes no tuvieron 
## cambios ni en signo, ni en significancia, a excepci�n de 
## oil_producer, mismo que cambio de signo y cambi� a ser
## no significativo estad�sticamente.

## Esto se podr�a deber a lo siguiente, de acuerdo a las
## siguientes interpretaciones:

## Cuando a�adimos la interacci�n, entonces tenemos dos ajustes lineales, 
## uno para productores de petr�leo y otra para no productores. 
## el intercepto de la l�nea de no productores es b_0, en este caso 0.2015,
## mientras que el intercepto de la l�nea de productores es b_0 + b_6 
## que es el coeff de oil_producer, en este caso 0.2015 + (-0.1345) = 0.067.
## Es decir, los pa�ses productores de pet�leo parten de una base m�s baja 
## en cuanto a sus niveles de crecimiento.

## Interpretamos el coeficiente de la interacci�n como cu�n mayor es el aumento
## en la tasa de crecimiento promedio del GDP entre 1960 y 1985 para 
## los pa�ses productores de petr�leo respecto de los no productores 
## de petr�leo si es que aumenta el nivel de GDP per capita que ambos ten�an en 1960. 

### -----------------------------------------------------------------
## Part 2.4 Plot
### -----------------------------------------------------------------

## Gr�fica con GDP60 en eje-y y oil_producer en eje-x

## Plot between GDP60 (y-axis) and oil_factor
library(ggplot2) ## more plotting capabilities than Base R.
ggplot(GrowthDJ, aes(x = oil, y = gdp60)) + 
        geom_point() + 
        xlab("Oil producers") + 
        ylab("GDP per capita in 1960") + 
        
### -----------------------------------------------------------------
## Part 3. Linear Relationships
### -----------------------------------------------------------------

## a) Como identificar�as si la variable school_av tiene una relaci�n lineal?
## Tendr�amos que incluir una variable de school_av^2. 
## El signo del coeficiente estimado de esta variable nos dir� si existe
## una relaci�n no lineal (esto debido a que 2 veces el coefficiente estimado 
## es igual a la segunda derivada). Si el signo es negativo, 
## querr� decir que existe una relaci�n no lineal c�ncava, y convexa 
## en caso de que el signo sea positivo, ello en caso de que el coeff. sea
## estad�sticamente distinto de cero. 

## b) Agregar este t�rmino e interpretar. 
growth_data$school_av2 <- growth_data$school_av^2

mod6 <- lm(gdpgrowth_av ~ school_av + gdp60 + gdp85 + popgrowth_av + 
                   invest_av + oil_producer + 
                   interact_gdp60_oilfactor + 
                   school_av2, data = growth_data)
summary.lm(mod6)

## Encontramos que la variable school_av tiene una relaci�n no lineal 
## con la variable gdpgrowth_av de tipo c�ncava, ya que el coeficiente 
## de school_av2 es negativo y estad�sticamente significativo a un 
## nivel de signficancia del 5%. 

