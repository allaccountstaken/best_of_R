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


# Bivariate data visualization 
library(gapminder)
gapminder[1:5, ]

Data<-gapminder%>%
  filter(year==2007)

ggplot(Data, aes(x=continent, y=lifeExp))+
  geom_boxplot(fill="Blue")+
  labs(x="Continent", y="Life Exp",
       title="Dist of Life Expectancies by Continent")

       
ggplot(gapminder, aes(x=continent, y=lifeExp, fill=as.factor(year)))+
  geom_boxplot()+
  labs(x="Continent", y="Life Exp",
       title="Dist of Life Expectancies by Continent & Year")

Data <- Data%>%
  mutate(expectancy=ifelse(lifeExp<70, "Low", "High"))

mytab2 <- table(Data$continent, Data$expectancy)
mytab2

prop.table(mytab2, 1)       

round(prop.table(mytab2, 1) * 100, 2)


ggplot(Data, aes(x=continent, fill=expectancy))+
  geom_bar(position="stack")+
  labs(x="Continent", y="Count", title="Life Expectancies by Continent")


ggplot(Data, aes(x=continent, fill=expectancy))+
  geom_bar(position="dodge")


ggplot(Data, aes(x=continent, fill=expectancy))+
  geom_bar(position="fill")+
  labs(x="Continent", y="Proportion",
       title="Proportion of Life Expectancies by Continent")


ggplot(Data, aes(x=gdpPercap, y=lifeExp))+
  geom_point(alpha=0.2)

# Multivariate
Data <- gapminder%>%
  mutate(expectancy=ifelse(lifeExp<70, "Low", "High"))%>%
  filter(year==2007)

Data.all <- gapminder%>%
  mutate(expectancy=ifelse(lifeExp<70, "Low", "High"))


ggplot(Data, aes(x=continent, fill=expectancy))+
  geom_bar(position = "fill")

ggplot(Data.all, aes(x=continent, fill=expectancy))+
  geom_bar(postion = "fill") +
  facet_wrap(~ year)


ggplot(Data, aes(x=gdpPercap, y=lifeExp, size=pop))+
  geom_point()+
  scale_size(range = c (0.1, 12))


ggplot(Data, aes(x=gdpPercap, y=lifeExp, size=pop, color=continent))+
  geom_point()+
  scale_size(range = c (0.1, 12))



ggplot(Data, aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent))+
  geom_point(shape=21, alpha=0.5)+
  scale_size(range = c(0.1,12))+
  labs(x="GDP", y="Life Exp", title="Scatterplot of Life Exp against GDP")
