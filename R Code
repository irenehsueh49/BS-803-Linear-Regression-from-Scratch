---
title: "Irene Hsueh's BS 803 Project"
author: "Irene Hsueh"
date: "12/3/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

### My Function 
```{r}
linear_regression <- function(data, outcome, predictor, ...)
  {
  #Creating Variables
  y <- as.matrix(data[outcome])
  predictors_variables <- c(predictor, ...)
  x <- as.matrix(data.frame(intercept=1, data[predictors_variables]))
  
  #Parameter Estimate
  beta <- solve(t(x)%*%x) %*% (t(x)%*%y)
  
  #Standard Error of Coefficients
  RSS <- t(y)%*%y - t(beta)%*%t(x)%*%y
  sigma_squared <- RSS/ (nrow(x) - ncol(x))
  variance <- as.numeric(sigma_squared) * solve(t(x)%*%x)
  standard_error <- round(sqrt(diag(variance)), digits=6)
  
  #R-Squared Goodness of Fit 
  SYY <- t(y)%*%y - length(y)*mean(y)^2
  r_squared <- round((1 - RSS/SYY), digits=4)
  
  #Renaming and Rounding Output
  dimnames(r_squared)[1] <- ""
  dimnames(r_squared)[2] <- "R-Squared Goodness of Fit"
  beta <- round(beta, 6)
  dimnames(beta)[2] <- "Parameter Estimates"
  
  return(list(linear_model_estimates=cbind(beta, standard_error), r_squared=r_squared))
}
```


### Turtles Test Dataset
```{r}
#Reading in Text File
turtles <- read.table("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 806 - Multivariate Analysis for Biostatisticians/Class 2 - Multiple Linear Regression/2. Exercise/galapagos_turtles.txt") %>% 
  dplyr::select(species = Species, 
                endemics = Endemics,
                area = Area, 
                elevation = Elevation, 
                nearest = Nearest, 
                scruz = Scruz, 
                adjacent = Adjacent)
head(turtles, 20)

#Built in R Function 
turtles_linear_model <- lm(species ~ area + elevation + nearest + scruz + adjacent, data=turtles)
summary(turtles_linear_model)

#My Linear Regression Function
linear_model_turtles <- linear_regression(data=turtles, outcome="species", predictor="area", "elevation", "nearest", "scruz", "adjacent")
linear_model_turtles
```


### Savings Rate Test Dataset
```{r}
#Reading in Text File
load("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 806 - Multivariate Analysis for Biostatisticians/Class 3 - Model Fitting Inference/Homework 3/savings.rda") 
savings <- data.frame(savings) %>% 
  dplyr::select(population_under15 = pop15, 
                population_under75 = pop75, 
                disposable_income = dpi, 
                disposable_income_growth_rate = ddpi, 
                savings_rate = sr)
head(savings, 20)

#Built in R Function 
savings_linear_model <- lm(savings_rate ~ population_under15 + population_under75 + disposable_income + disposable_income_growth_rate, data=savings)
summary(savings_linear_model)

#My Linear Regression Function
linear_model_savings <- linear_regression(data=savings, outcome="savings_rate", predictor="population_under15", "population_under75", "disposable_income", "disposable_income_growth_rate")
linear_model_savings
```

#Parameter Estimates 
β = (X'X)^(-1) X'Y

#Standard Error of Coefficients
SE(β) = sqrt(variance)
Var(β) = σ^2 * (X'X)^(-1)
σ^2 = RSS/(n-p-1)
RSS = Y'Y - beta'X'Y

#R^2
R^2 = 1 - RSS/Syy
RSS = Y'Y - beta'X'Y
Syy = Y'Y - n*(y_mean)^2

