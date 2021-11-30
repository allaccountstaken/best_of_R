# title: "Homework10"
# author: "Dima Mikhaylov"
# date: "11/15/2021"

# Load and inspect the dataset
library(datasets) 
library(tidyverse)
data(swiss)
head(swiss)

# Fit the model
mlr <- lm(Fertility ~ Education + Catholic + Infant.Mortality, data = swiss)
summary(mlr)

# Critical value using Bonferroni procedure at 95% confidence level
n <- dim(swiss)[1]
p <- 4 #3 slopes and 1 for intercept
critical <- qt(1-0.05/(2*n), n-1-p)
cat("Critical value is", critical)

##Number of outliers predictors based on studentized residuals
student.res<-rstandard(mlr)
student.res[abs(student.res)>critical]

# Number of outliers predictors based on externally studentized residuals
ext.student.res <- rstudent(mlr)
ext.student.res[abs(ext.student.res)>critical]

# High leverage points above the cutoff
leverage <- lm.influence(mlr)$hat
leverage[leverage > 2* p/n]


# Influential observations in terms of y_hat_i
DFFITS <- dffits(mlr)
DFFITS[abs(DFFITS) >  2*sqrt(p/n)]

# Influential observations in terms of least-squares
COOKS <- cooks.distance(mlr)
COOKS[COOKS>qf(0.5,p,n-p)]

