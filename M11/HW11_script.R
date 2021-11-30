#title: "Homework11"
#author: "Dima Mikhaylov"
#date: "11/22/2021"


library(palmerpenguins)
Data<-penguins
##remove penguins with gender missing
Data<-Data[complete.cases(Data[ , 7]),-c(2,8)]
##80-20 split
set.seed(1)
sample<-sample.int(nrow(Data), floor(.80*nrow(Data)), replace = F)
train<-Data[sample, ]
test<-Data[-sample, ]
head(train)


# Convert sex to 1 and 0
train$sex <- ifelse(train$sex=="male",1,0)
train$sex <- factor(train$sex)
head(train)

# Create 4 boxplot objects for respective numeric predictors:
library(ggplot2)
bp1<-ggplot(train, aes(x=sex, y=bill_length_mm, color=species))+
  geom_boxplot()+
  labs(x="sex", y="bill_length_mm", title="bill_length_mm by sex")

bp2<-ggplot(train, aes(x=sex, y=bill_depth_mm, color=species))+
  geom_boxplot()+
  labs(x="sex", y="bill_depth_mm", title="bill_depth_mm by sex")

bp3<-ggplot(train, aes(x=sex, y=flipper_length_mm, color=species))+
  geom_boxplot()+
  labs(x="sex", y="flipper_length_mm", title="flipper_length_mm by sex")

bp4<-ggplot(train, aes(x=sex, y=body_mass_g, color=species))+
  geom_boxplot()+
  labs(x="sex", y="body_mass_g", title="body_mass_g by sex")

## Produce the 4 boxplots in a 2 by 2 matrix
library(gridExtra)
library(grid)
grid.arrange(bp1, bp2, bp3, bp4, ncol = 2, nrow = 2)


# Create full logistoc model
full_model<-glm(sex ~ ., family="binomial", data=train)
summary(full_model)


# Create reduced logistoc model
reduced_model<-glm(sex ~ species + bill_length_mm + bill_depth_mm + body_mass_g, family="binomial", data=train)
summary(reduced_model)

# Create a vector with new data using species + bill_length_mm + bill_depth_mm + body_mass_g,
newdata <- data.frame(species="Gentoo", bill_length_mm=49, bill_depth_mm=15, flipper_length_mm=220, body_mass_g=5700)

# Make prediction for log odds
predict(full_model, newdata)

## Convert to odds
odds<-exp(predict(full_model,newdata))
odds


## Convert odds to probability
prob<-odds/(1+odds)
prob


# Compute delat G2
deltaG2<-full_model$null.deviance - full_model$deviance
cat("Delta G2 statistic is", deltaG2)

# Test the stats for significance
cat("Corresponding p-value is", 1-pchisq(deltaG2,5))

