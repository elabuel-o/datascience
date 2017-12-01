
#######################################################################
## Problem Set 3. 
## Data Science for Economists 
## C.U. 91621
## Armando Enriquez Z.
## Date: December 1st, 2017.
#######################################################################

## Session info
# R version 3.3.1 
# Platform x86_64-w64 (64-bit) 

### ------------------------------------------------------------------
### Question 1.
### ------------------------------------------------------------------
## Loading dataset USArrests
mydata <- USArrests
str(mydata)

### ------------------------------------------------------------------
### Question 2.
### ------------------------------------------------------------------
## Media y varianza muestral de cada variable, en una sola linea. 
apply(mydata, 2, mean) ## sample mean
apply(mydata, 2, var) ## sample variance

### Que problema surge si aplicamos directamente PCA?
## Para este caso observamos que mientras que para una variable la 
## varianza es de menos de 20, para otras es cercana a 7000. 
## No seria adecuado utilizar PCA directamente. Es adecuado 
## reescalar las varianzas previo al analisis.

### ------------------------------------------------------------------
### Question 3.
### ------------------------------------------------------------------
## Utilizar prcomp()
USArrests_pca <- prcomp(USArrests, scale = TRUE) ##scaling variance

### ------------------------------------------------------------------
### Question 4.
### ------------------------------------------------------------------
## Obtaining eigenvalues
eigen_0 <- USArrests_pca$sdev^2
eigen_0 ## eigenvalues with variance scaling

## Notice that, because of scaling the variances in the PC Analysis, these
## eigenvalues differ from the manually computed eigenvalues
USArrests_cov <- cov(USArrests)
eigen_1 <- eigen(USArrests_cov)$values
eigen_1

## If we do not scale, both procedures (manual and sdev^2 name attribute)
## yield the same eigenvalues:
USArrests_pca2 <- prcomp(USArrests, scale = FALSE) #w/o scaling variance
eigen_2 <- USArrests_pca2$sdev^2

all.equal(eigen_1, eigen_2) ## TRUE! 

rm(USArrests_pca2)
rm(eigen_1)
rm(eigen_2)

### ------------------------------------------------------------------
### Question 5.
### ------------------------------------------------------------------
## Cuantos componentes principales se generaron?
## Que relacion tiene el numero de componentes con el numero
## de observaciones y variables?
str(USArrests_pca)

USArrests_pca$rotation
## Se generaron 4 componentes principales.
## Estos componentes corresponden a cada una de las variables de nuestro
## dataset. Esto es debido a que el atributo "rotation" corresponde a una
## matriz de kxk (cuadrada).

### ------------------------------------------------------------------
### Question 6.
### ------------------------------------------------------------------
## Utiliza el componente sdev^2.
## Computar la proporción de varianza de cada componente principal. 
## Que proporcion de varianza tiene el PC1?

## Proporciones de varianza para cada PC
prop_var <- (USArrests_pca$sdev^2)/4
## Verificamos que la suma de las proporciones de cada PC sea 1
sum(prop_var)

## El PC1 tiene la mayor porporcion de varianza de todos los PC.
prop_var[1] ## 0.6200604

### ------------------------------------------------------------------
### Question 7.
### ------------------------------------------------------------------
## Utiliza el attribute x para obtener los scores
PCscores <- USArrests_pca$x
PCscores

### ------------------------------------------------------------------
### Question 8.
### ------------------------------------------------------------------
library(ggfortify)
autoplot(USArrests_pca)


### ------------------------------------------------------------------
### Question 9.
### ------------------------------------------------------------------

## PCA for my data:
my_pca <- prcomp(my_data)
summary(my_pca) 

## Getting the matrix of KxK:
my_pca$rotation

## for each observation of my dataset, getting all computed principal 
## components:
predict(my_pca)[, 1]

### ------------------------------------------------------------------
### Question 10.
### ------------------------------------------------------------------
## Este paquete provee funciones (metodos en la terminologia de R)
## para Regresion Multivariada, Regresiones parciales, y regresion 
## de componente principal.

## pcr es una funcion del paquete pls que sirve para correr 
## regresiones de componente principal (principal component regression)
## La podemos utilizar para correr regresiones para componentes, mediante
## algoritmos como kernel, o bien como clasificador de categorías. 









