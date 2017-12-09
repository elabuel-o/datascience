
#######################################################################
## Problem Set 4. 
## Data Science for Economists 
## C.U. 91621
## Armando Enriquez Z.
## Date: December 8th, 2017.
#######################################################################

## Session info
# R version 3.3.1 
# Platform x86_64-w64 (64-bit) 

### ------------------------------------------------------------------
### Question 1.
### ------------------------------------------------------------------
# Reading into R the database 
library(randomForest)
mydata <- read.csv("german_credit.csv", header = TRUE)

### ------------------------------------------------------------------
### Question 2.
### ------------------------------------------------------------------
## Crear nuevo dataframe con solo las primeras 16 variables
FraudData <- mydata[, 1:16]

### ------------------------------------------------------------------
### Question 3.
### ------------------------------------------------------------------
FraudData$label <- ifelse(FraudData$default == 1, "default", 0)
FraudData$label <- as.factor(FraudData$label)

### ------------------------------------------------------------------
### Question 4.
### ------------------------------------------------------------------
## Training set con 80% de la muestra y validation set con 20%
set.seed(123) # Reproducibility
index.train <- sample(1:nrow(FraudData), 800)
index.valid <- sample(1:nrow(FraudData), 200)

train_set <- FraudData[index.train, ]
valid_set <- FraudData[index.valid, ]

### ------------------------------------------------------------------
### Question 5.
### ------------------------------------------------------------------
rf1 <- randomForest(label ~ duration_in_month + credit_amount + age + 
                     present_res_since + installment_as_income_perc, 
             data = FraudData)
## OBB es el out-of-bag error estimate, y nos dice los casos que no 
## fueron seleccionados en la consturccion del arbol. 
## Report OBB
length(rf1$oob.times)

### ------------------------------------------------------------------
### Question 6.
### ------------------------------------------------------------------
fit.forest <- predict(valid_set)

### ------------------------------------------------------------------
### Question 7.
### ------------------------------------------------------------------
library(caret)


