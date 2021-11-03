
library(MASS)
data(package = 'birthwt')
head(birthwt)

class(birthwt$race)

birthwt$race <- factor(birthwt$race)
class(birthwt$race)

library(tidyverse)
ggplot(birthwt, aes(x=age, y=bwt, color=race))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Age (years)", 
       y="Birth weight (grams)",
       title="Scatter plot of birth weight against age")


model = lm(bwt ~ age*race, data=birthwt)
summary(model)


par(mfrow=c(2,2))
plot(model)

boxcox(model)