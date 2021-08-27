library(tidyverse)

df = read.table("data/students.txt", header=TRUE)
head(df)

# Remove column Student ID or index?

# How many students are there in the dataset?
dim(df)

# How many students are missing entry in the last 2 columns?
#df[!complete.cases(Data), -2]

# Report the median values of the numeric variables.
head(df)
apply(df[, c(6,7,8,9)], 2, median)

#Report the mean and standard deviation of StudyHrs for female and male students
tapply(df$StudyHrs, df$Gender, median, na.rm=T)
tapply(df$StudyHrs, df$Gender, sd, na.rm=T)




#library(ggplot2)
Data <- read.csv('data/ClassDataPrevious.csv', header=TRUE)

dim(Data)

colnames(Data)

Data[1, 8]

Data[c(1, 3, 4), c(1, 5, 8)]

Data$Year

Data[c(1, 3), ]

which(Data$Sport=="Soccer")
Data[3, ]

SoccerFans <- Data[which(Data$Sport=="Soccer"), ]

dim(SoccerFans)



SoccerFans_2nd <- Data[which(Data$Sport=="Soccer" & Data$Year=="Second"), ]


dim(SoccerFans_2nd)


Sleepy_or_Soccer <- Data[which(Data$Sport=="Soccer" | Data$Sleep>8) , ]
dim(Sleepy_or_Soccer)


names(Data)[7] <- "Comp"
names(Data)
dim(Data)

is.na(Data)

Data[!complete.cases(Data), ]


apply(Data[,c(2,4,6,8)], 2, mean, na.rm=T)


apply(Data[,c(2, 4, 6, 8)], 2, median, na.rm=T)

tapply(Data$Sleep, Data$Year, median, na.rm=T)
Data$Yr <- factor(Data$Year, levels=c("First", "Second", "Third", "Fourth"))
levels(Data$Yr)


tapply(Data$Sleep, Data$Yr, median, na.rm=T)


deprived <- ifelse(Data$Sleep<7, "yes", "no")

length(deprived)

CourseLoad <- cut(Data$Courses, breaks = c(-Inf, 3, 5, Inf),
                  labels = c("light", "regular", "heavy"))

Data <- data.frame(Data, Sleep_mins, deprived, CourseLoad)
head(Data)

write.csv(Data, file="data/newdata.csv", row.names = TRUE)
