# title: "Homework3"
# author: "Dima Mikhaylow"
# date: "9/21/2021"
# output: html_document


Data = read.table("copier.txt", header=TRUE)
head(Data)


library(tidyverse)
ggplot(Data, aes(x=Minutes, y=Serviced))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90))+
  labs(x="Minutes", 
       y="Serviced", 
       title="Scatterplot of number of serviced devices and time spent")


result <- lm(Serviced ~ Minutes, data=Data)
summary(result)

anova.tab <- anova(result)
anova.tab