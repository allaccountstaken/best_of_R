# title: "Homework2"
# author: "Dima Mikhaylov"
# date: "9/11/2021"

# As per homework guidelines:
# 9. Please also upload your R script containing the code you used to answer any of the
# questions. This allows me to refer to your work and give credit if needed.

library(tidyverse)

# Part 1

PoliceKillings <- read.csv('PoliceKillings.csv', header=TRUE)

round(prop.table(table(PoliceKillings$raceethnicity), ) *100, 2)

raceethnicity_prop <- PoliceKillings %>%
  group_by(raceethnicity)%>%
  summarise(Counts=n())%>%
  mutate(Percent=Counts/nrow(PoliceKillings))

ggplot(raceethnicity_prop, aes(x=raceethnicity, y=Percent))+
  geom_bar(fill="brown3", color='black', stat="identity")+
  theme(axis.text.x=element_text(angle=90),
        plot.title=element_text(hjust=0.5))+
  labs(x="Race/Ethnicity", y="Prop. of killings", title="Dist of variable `raceethnicity`")

PoliceKillings$age.num <- as.numeric(PoliceKillings$age)
is.numeric(PoliceKillings$age.num)

ggplot(PoliceKillings, aes(x=age.num))+
  geom_density(fill="darkorange", color='black')+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Victim' Age", y="Density", title="Probability density of variable `age.num`")

ggplot(PoliceKillings, aes(x=raceethnicity, y=age.num))+
  geom_violin(fill="darkorange", color='black')+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Race/Ethnicity", y="Victim' Age", title="Race specific probability density at different ages")

round(prop.table(table(PoliceKillings$cause))*100, 2)

ggplot(PoliceKillings, aes(x=raceethnicity, fill=cause))+
  geom_bar(color='black', position="fill")+
  theme(axis.text.x=element_text(angle=90),
        plot.title=element_text(hjust=0.5))+
  labs(x="Race", y="Deaths", title="Causes of death by race")

round(prop.table(table(PoliceKillings$raceethnicity, PoliceKillings$gender), ) *100, 2)

ggplot(PoliceKillings, aes(x=raceethnicity, y=age.num, fill=as.factor(gender)))+
  geom_boxplot(color='black')+
  theme(axis.text.x=element_text(angle=90),
        plot.title=element_text(hjust=0.5))+
  labs(x="Race", y="Deaths", title="Age and gender of deaths by race")


# Part 2

Covid <- read.csv('stateCovid.csv', header=TRUE) #, row.names=1)

Election <- read.csv('State_pop_election.csv', header=TRUE)
Merged <- merge( Covid, Election, by.x="state", by.y = "State")
head(Merged)

ggplot(Merged[!is.na(Merged$Election),], aes(x=Election, y=state_rate))+
  geom_boxplot(fill="darkgreen", color='black')+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Voting preferences", y="State death rate", title="State COVID death rate by election results")

