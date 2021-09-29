
#---
# title: "Homework4"
# author: "Dima Mikhaylow"
# date: "9/27/2021"
# output: R script
#---


Data = read.table("copier.txt", header=TRUE)
#head(Data)


result <- lm(Minutes ~ Serviced, data=Data)

library(tidyverse)
ggplot(Data, aes(x=Serviced, y=Minutes))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90))+
  labs(x="Serviced", 
       y="Minutes", 
       title="Scatterplot of time spent and number of serviced devices")

cor(Data$Minutes, Data$Serviced)


confint(result, level = 0.95)


new_data <- data.frame(Serviced=5)
predict(result, new_data, level=0.95, interval="prediction")


summary(result)$residuals[1]

mean(result$residuals)

