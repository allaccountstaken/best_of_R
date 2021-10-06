#title: "Homework5"
#author: "Dima Mikhaylov"
#date: "10/5/2021"

library(faraway)
data(cornnit)
head(cornnit)

library(tidyverse)
ggplot(cornnit, aes(x=nitrogen, y=yield))+
  geom_point()+
  labs(x="Nitrogen fertilizer (pounds per acre) fertilizer", 
       y="Corn yield (bushels per acre)",
       title="Scatter plot of corn yield against nitrogen fertilizer usage")


ggplot(cornnit, aes(x=nitrogen, y=yield))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  labs(x="Nitrogen fertilizer (pounds per acre) fertilizer", 
       y="Corn yield (bushels per acre)",
       title="Fitted linear model for yield against nitrogen fertilizer usage")

base_model = lm(yield ~ nitrogen, data=cornnit)
summary(base_model)

y_fitted <- base_model$fitted.values
residuals <- base_model$residuals
cornnit <- data.frame(cornnit, y_fitted, residuals)

ggplot(cornnit, aes(x=y_fitted, y=residuals))+
  geom_point()+
  geom_hline(yintercept = 0, color='red')+
  labs(x="Fitted values", y="Residuals", title= "Residual plot before any transformations")


acf(residuals, main="ACF Plot of Residuals with original x")

boxcox(base_model)

summary(lm(yield**1.4 ~ nitrogen, data = cornnit))


x_star<-sqrt(cornnit$nitrogen)
cornnit<-data.frame(cornnit,x_star)

x_star_model <- lm(yield ~ x_star, data=cornnit)
summary(x_star_model)


y_fitted_x_star <- x_star_model$fitted.values
residuals_x_star <- x_star_model$residuals
cornnit <- data.frame(cornnit, y_fitted_x_star, residuals_x_star)

ggplot(cornnit, aes(x=y_fitted_x_star, y=residuals_x_star))+
  geom_point()+
  geom_hline(yintercept = 0, color='red')+
  labs(x="Fitted values", y="Residuals", title= "Residual plot after log(x) transformation")


acf(residuals_x_star, main="ACF Plot of Residuals with sqrt x")

ggplot(cornnit, aes(x=x_star, y=yield))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  labs(x="Log nitrogen fertilizer (pounds per acre) fertilizer", 
       y="Corn yield (bushels per acre)",
       title="Fitted linear model for yield against log nitrogen fertilizer usage")