library(tidyverse)
library(ggplot2)

Data <- read.csv('data/ClassDataPrevious.csv', header=TRUE)
table(Data$Year)

# Categorical variables
ggplot(Data, aes(x=Year)) +
  geom_bar(fill='blue', color='black') +
  theme(axis.text.x = element_text(angle=360), 
        plot.title=element_text(hjust=0.5)) + 
  labs(x="Year", y="Number of Students", title="Distribution of Students")

library(dplyr)


newData<-Data%>% 
  group_by(Year)%>%
  summarize(Counts=n())%>%
  mutate(Percent=Counts/nrow(Data))


ggplot(newData, aes(x=Year, y=Percent))+
  geom_bar(stat = "identity", fill='green')+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x="Year", y="Percent of Students", title="Dist of Years")

# Quantitative variables
summary(Data$Age)


ggplot(Data, aes(y=Age))+
  geom_boxplot()+
  coord_flip()


ggplot(Data, aes(y=Age))+
  geom_boxplot(color="blue", outlier.colour = "orange")


Data$Year <- factor(Data$Year,
                    levels=c("First","Second","Third","Fourth"))
levels(Data$Year)

ggplot(Data, aes(x=Year, y=Age))+
  geom_boxplot(color="blue", outlier.color="orange")

ggplot(Data, aes(x=Year, y=Age, fill=Computer))+
  geom_boxplot(color="blue", outlier.color="orange")


ggplot(Data, aes(x=Year, y=Age))+
  geom_violin(color="blue")

sqrt(nrow(Data))

ggplot(Data, aes(x=Age))+
  geom_histogram(bins = 17,fill="blue",color="orange")

ggplot(Data, aes(x=Age))+
  geom_density()
