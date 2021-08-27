library(tidyverse)
library(ggplot2)
Data <- read.csv('data/ClassDataPrevious.csv', header=TRUE)
table(Data$Year)
ggplot(Data, aes(x=Year)) +
  geom_bar(fill='blue', color='black') +
  theme(axis.text.x = element_text(angle=360), 
        plot.title=element_text(hjust=0.5)) + 
  labs(x="Year", y="Number of Students", title="Distribution of Students")

library(dplyr)


newData<-Data%>% # Not sure what this "%>%" suppose to mean????
  group_by(Year)%>%
  summarize(Counts=n())%>%
  mutate(Percent=Counts/nrow(Data))


ggplot(newData, aes(x=Year, y=Percent))+
  geom_bar(stat = "identity", fill='green')+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x="Year", y="Percent of Students", title="Dist of Years")
