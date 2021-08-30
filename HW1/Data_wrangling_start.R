library(tidyverse)

df = read.table("data/students.txt", header=TRUE)
head(df)

# 1. Variable that is not a part of any meaningful analysis? 
#Remove column Student ID or index?

# 2. How many students are there in the dataset? 
# Number of rows is given by the first dimension of dim()
dim(df)[1]

# 3. How many students are missing entry in the last 2 columns?
df[!complete.cases(Data), -2]

# 4. Report the median values of the numeric variables.
# First, check where numeric columns are. Second, use apply() to get the medians
head(df)
apply(df[, c(6,7,8,9)], 2, median)

# 5. Report the mean and standard deviation of StudyHrs for female and male students
# Use tapply() with group by df$Gender first for median and then for std.
mean_females <- tapply(df$StudyHrs, df$Gender, median, na.rm=T)[1]
mean_males <- tapply(df$StudyHrs, df$Gender, median, na.rm=T)[2]
sd_females <- tapply(df$StudyHrs, df$Gender, sd, na.rm=T)[1]
sd_males <- tapply(df$StudyHrs, df$Gender, sd, na.rm=T)[2]
print(paste("Female mean:", mean_females, "-- Female std:", sd_females))
print(paste("Male mean:", mean_males, "-- Male std:", sd_males))

# 6. Construct a 95% confidence interval for the mean StudyHrs for female/male students
# Is there evidence that the means are different? 
#First, get CI for females, i.e sample mean +/- margin of error, z*sigma/n
n_females <- count(filter(df, Gender=="female"))
CI_females_upper = mean_females + (qnorm(1-((1-0.95)/2))*sd_females)/sqrt(n_females)
CI_females_lower = mean_females - (qnorm(1-((1-0.95)/2))*sd_females)/sqrt(n_females)
print(paste("Female CI: [", CI_females_lower,",", CI_females_upper, "]"))

#Same for males:
n_males <- count(filter(df, Gender=="male"))
CI_males_upper = mean_males + (qnorm(1-((1-0.95)/2))*sd_males)/sqrt(n_males)
CI_males_lower = mean_males - (qnorm(1-((1-0.95)/2))*sd_males)/sqrt(n_males)
print(paste("Male CI: [", CI_males_lower,",", CI_males_upper, "]"))

#Second, look if the difference of the means is statistically significant.

qnorm(1-((1-0.95)/2))
qnorm(0.975)

n_females <- count(filter(df, Gender=="female"))

n_males <- count(filter(df, Gender=="male"))

n_males

#library(ggplot2)sw 
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
