# title: "project_q4"
# author: "Dima Mikhaylov"
# date: "10/9/2021"

library(tidyverse)
library(MASS)
df <- read_csv("diamonds4.csv", show_col_types = FALSE)

# Scatterplot analysis - nonlinear relationship between y=Price and x=Carat:
ggplot(df, aes(x=carat, y=price))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  labs(x="Carat", y="Price",
       title="Regression line Price against Carat")

# Fitting a `base_model` with no transformations:
base_model = lm(price ~ carat, data=df)
summary(base_model)
# Meaningless intercept but a significant predictor as measured by F-statistic, only moderate R-squared of 68%. 

y_hat <- base_model$fitted.values
residuals_0 <- base_model$residuals
df <- data.frame(df, y_hat, residuals_0)

# From the Residual plot, at least 2 assumptions seem to be violated: 
# 1) mean of errors is not zero as there is a # cluster below the horizontal line, 
# 2) variance also tends to increase with larger values.

ggplot(df, aes(x=y_hat, y=residuals_0))+
  geom_point()+
  geom_hline(yintercept = 0, color='red')+
  labs(x="Fitted y", y="Residuals", title= "Residual plot from the `base_model` before any transformation")

# Check for a good transformation of the response variable is used to remedy non-constant variance.
boxcox(price ~ carat, data = df)


# Next check for possible auto-correlations in predictor with ACF. 
# Consider possible x transformation to correct for lag 3 correlation:
acf(residuals_0, main="ACF Plot of Residuals for the original x")

# Finally check for normality of residuals from the `base_model`, not looking normal at all:
qqnorm(residuals_0)

# Start with y transformation - use a convex function, sqrt() or log(). Log transform first, residuals_1
price_log <- log(df$price)
df <- data.frame(df, price_log)

model.price_log <- lm(price_log ~ carat, data=df)

price_log_hat1 <- model.price_log$fitted.values
residuals_1 <- model.price_log$residuals
df <- data.frame(df, price_log_hat1, residuals_1)

## Residual plot for log y-transformation
ggplot(df, aes(x=price_log_hat1, y=residuals_1))+
  geom_point()+
  geom_hline(yintercept=0, color="red")+
  labs(x="Fitted y", y="Residuals", title="Residual Plot after log y transformation")

# Residuals are looking better at least in terms of variance, the mean is still not zero. Check 
ggplot(df, aes(x=carat, y=price_log))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  labs(x="Carat", y="Sqrt of Price",
       title="Regression line Sqrt of Price against Carat")

# As shown below, the log model is much stronger in terms of R-squared (78%.)
summary(model.price_log)

# Still need to consider changes to the predictor. 
# Possibly log transformation of x will produce a better set of residuals_2
carat_log <- log(df$carat)
df <- data.frame(df,carat_log)

result.log_log <- lm(price_log ~ carat_log, data=df)

price_log_hat2 <- result.log_log$fitted.values
residuals_2 <- result.log_log$residuals
df <- data.frame(df,price_log_hat2,residuals_2)

##residual plot for x-transformation
ggplot(df, aes(x=price_log_hat2,y=residuals_2))+
  geom_point()+
  geom_hline(yintercept=0, color="red")+
  labs(x="Fitted y", y="Residuals", title="Residual Plot after log x and log y transormations")

# Checking the summary of the resulting model:
summary(result.log_log)

# R-squared of almost 96% recorded. 
# Checking residuals after the second transformation - very close to "normality" now:
qqnorm(residuals_2)



