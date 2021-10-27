# title: "Homework7"
# author: "Dima Mikhaylov"
#date: "10/21/2021"


library(datasets)
library(tidyverse)
library(corrplot)
library(GGally)
data(swiss)
#head(swiss)

full_lm.fit <- lm(Fertility ~ ., data=swiss)
summary(full_lm.fit)

reduced_lm.fit <- lm(Fertility ~ Education + Catholic + Infant.Mortality, data=swiss)
summary(reduced_lm.fit)

anova(reduced_lm.fit, full_lm.fit)

test_lm.fit <- lm(Fertility ~ Education + Catholic + Infant.Mortality + ., data=swiss)
anova(test_lm.fit)

F0_stat = ((264.2+53)/2)/(2105/41)
1-pf(F0_stat, 2, 41)

par(mfrow=c(2,2))
plot(reduced_lm.fit)


