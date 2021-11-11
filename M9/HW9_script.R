# title: "Homework9"
# author: "Dima Mikhaylov"
# date: "11/5/2021"



# Part 1

library(MASS)
#data(package = 'birthwt')
head(birthwt)

sapply(birthwt, class)

birthwt$race <- as.factor(birthwt$race)
class(birthwt$race)

data <- within(birthwt, rm('low')) 

library(leaps)
allregs <- regsubsets(bwt ~ ., data=data, nbest=1)
summary(allregs)

coef(allregs, which.max(summary(allregs)$adjr2))

coef(allregs, which.min(summary(allregs)$cp))

coef(allregs, which.min(summary(allregs)$bic))

regnull <- lm(bwt ~ 1, data=data)
regfull <- lm(bwt ~ ., data=data)
step(regfull, scope=list(lower=regnull, upper=regfull), direction = 'backward')



#Custom function to calculate PRESS statistics:
PRESS <- function(model) {
  i <- residuals(model)/(1 - lm.influence(model)$hat)
  sum(i^2)
}