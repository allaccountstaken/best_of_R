#title: "Homework12"
#author: "Dima Mikhaylov"
#date: "12/4/2021"

# Load and inspect the data
library(palmerpenguins)
Data<-penguins
str(Data)

## Remove penguins with gender missing
Data<-Data[complete.cases(Data[ , 7]),-c(2,8)]

# Drop flipper length
Data <- subset( Data, select = -c(flipper_length_mm))

# Convert sex to 0 and 1 factor for output variable
Data$sex <- factor(ifelse(Data$sex=="male",1,0))
head(Data)


##80-20 split
set.seed(1)
sample<-sample.int(nrow(Data), floor(.80*nrow(Data)), replace = F)
train<-Data[sample, ]
test<-Data[-sample, ]
head(train)


# Fit the model
full_model<-glm(sex ~ ., family="binomial", data=train)
summary(full_model)


library(ROCR)
# Get predictions based on test holdout set
preds <- predict(full_model, newdata=test, type='response')
rates <- prediction(preds, test$sex)
roc_result <- performance(rates, measure = 'tpr', x.measure = 'fpr')
roc_result


# Plot the ROC curve and random guess 50% line
plot(roc_result, title="The ROC curve")+
  lines(x = c(0,1), y = c(0, 1), col="red")


# Get AUC from test performance
auc <- performance(rates, measure="auc")
auc@y.values


# Confusion matrix
prop.table(table(test$sex))
cf <- table(test$sex, preds>0.5)
cf


# Report errors
cat("False positive (type 1 error):", cf[3]/(cf[3]+cf[1]))
cat("False negative (type 2 error):", cf[2]/(cf[2]+cf[4]))
cat("Error rate:", (cf[3]+cf[2])/(cf[1]+cf[2]+cf[3]+cf[4]))



