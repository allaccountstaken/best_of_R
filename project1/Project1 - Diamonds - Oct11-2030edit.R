# Title : Rscript for Project 1 - SLR Diamonds
# Authors : Alex Bass, Andy Ortiz, Seth Harrison, Dima Mikhaylow

######################
# Alex's Section - Correlations/Correlation Matrix
#####################

#loading packages
library(tidyverse)
library(corrplot)
library(scales)

df <- read.csv("diamonds4.csv")

# looks like there are only two continuous variablies: price and carat
# .... but clarity, color, and cut can be made into factors for a correlation 
# matrix

df_for_cmatrix <- df

df_for_cmatrix$clarity <- as.numeric(factor(df$clarity, 
                                            levels = c("SI2", "SI1", "VS2", "VS1", "VVS2",
                                                       "VVS1", "IF", "FL")))

df_for_cmatrix$cut <- as.numeric(factor(df$cut, 
                                        levels = c("Good", "Very Good", "Ideal",
                                                   "Astor Ideal")))



df_for_cmatrix$color <- as.numeric(factor(df$color,
                                          levels = rev(names(table(df$color)))))

# I obtained the levels above from BlueNile.com

corrplot(cor(df_for_cmatrix), type = "upper", method = 'color', diag = F,
         addCoef.col = "black", tl.col = "Black")


#############
# Andy's Section - Visualize Price vs Clarity and Carat
#############

df_cat <- df
# Data$clarity<-factor(Data$clarity, levels=c("FL","IF","VVS1","VVS2","VS1","VS2","SI1","SI2"))
df_cat$clarity<-factor(df_cat$clarity, levels=c("SI2","SI1","VS2","VS1","VVS2","VVS1","IF","FL"))

df_cat$color <- factor(df_cat$color, levels=c("J","I","H","G","F","E","D"))

# Data$cut<-factor(Data$cut, levels=c("Astor Ideal","Ideal","Very Good","Good"))
df_cat$cut<-factor(df_cat$cut, levels=c("Good","Very Good","Ideal","Astor Ideal"))


ggplot(df_for_cmatrix, aes(x=carat,y=price))+
  geom_point(size = 1.5,alpha=0.2, color='darkblue')+
  labs(x="Carat", y="Price", title="Scatterplot of Carat against Price")+
  scale_y_continuous(labels = dollar)

ggplot(df_for_cmatrix, aes(x=clarity,y=price))+
  geom_point(size = 1.5, alpha=0.2, color='darkblue')+
  labs(x="Clarity", y="Price", title="Scatterplot of Clarity against Price")+
  scale_y_continuous(labels = dollar)

ggplot(df_cat, aes(x=carat,y=price, color=clarity))+
  geom_point(size = 1.5)+
  facet_wrap(~clarity)+
  labs(x="Carat", y="Price", title="Scatterplot of Carat against Price, by Clarity")+
  scale_y_continuous(labels = dollar)

#############
# Seth's Section - Visualize Price vs Color and Cut
#############
library(ggthemes)
ggplot(df_for_cmatrix, aes(x=color,y=price))+
  geom_point(size = 1.5,alpha=0.2, color='darkblue')+
  labs(x="Color", y="Price", title="Scatterplot of Color against Price")+
  scale_y_continuous(labels = dollar) #+ theme_classic()

ggplot(df_for_cmatrix, aes(x=cut,y=price))+
  geom_point(size = 1.5,alpha=0.2, color='darkblue')+
  labs(x="Color", y="Price", title="Scatterplot of Cut against Price")+
  scale_y_continuous(labels = dollar) #+ theme_classic()

ggplot(df_cat, aes(x=cut,y=price, color = color)) +
  facet_wrap(~color) +
  geom_point(size = 1.5)+
  labs(x="Color", y="Price", title="Scatterplot of Cut against Price, by Color")+
  scale_y_continuous(labels = dollar) #+ theme_classic()

## Extra graphs for looking at variable interactions, may not be used

ggplot(df_cat, aes(x=carat,y=price, color = cut)) +
  facet_wrap(~cut) +
  geom_point(size = 1.5)+
  labs(x="Color", y="Price", title="Scatterplot of Carat against Price, by Cut")+
  scale_y_continuous(labels = dollar)

ggplot(df_cat, aes(x=color,y=price, color = clarity)) +
  facet_wrap(~clarity) +
  geom_point(size = 1.5)+
  labs(x="Color", y="Price", title="Scatterplot of Color against Price, by Clarity")+
  scale_y_continuous(labels = dollar)

#############
## Histograms for average price based on different variables that can be easily grouped
#############

# Histogram for average price based on color

df_histCol <- data.frame(df["color"], df["price"])
df_histCol <- df_histCol %>% group_by(color) %>%
  summarize(mean_price = mean(price))

ggplot(df_histCol, aes(x = color, y = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title="Average price based on color")

# Histogram for average price based on cut

df_histCut <- data.frame(df["cut"], df["price"])
df_histCut <- df_histCut %>% group_by(cut) %>%
  summarize(mean_price = mean(price))

ggplot(df_histCut, aes(x = cut, y = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title="Average price based on cut")

# Histogram for average price based on clarity

df_histClar <- data.frame(df["clarity"], df["price"])
df_histClar <- df_histClar %>% group_by(clarity) %>%
  summarize(mean_price = mean(price))

ggplot(df_histClar, aes(x = clarity, y = mean_price)) +
  geom_bar(stat = "identity")+
  labs(title="Average price based on clarity")


#############
# Dima's Section - Fitting SLR model
#############


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
# Possibly, a log transformation of x will produce a better set of residuals_2
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




