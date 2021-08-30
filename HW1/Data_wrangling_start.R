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
mean_females <- tapply(df$StudyHrs, df$Gender, mean, na.rm=T)[1]
mean_males <- tapply(df$StudyHrs, df$Gender, mean, na.rm=T)[2]
sd_females <- tapply(df$StudyHrs, df$Gender, sd, na.rm=T)[1]
sd_males <- tapply(df$StudyHrs, df$Gender, sd, na.rm=T)[2]
print(paste("Female mean:", mean_females, "-- Female std:", sd_females))
print(paste("Male mean:", mean_males, "-- Male std:", sd_males))

# 6. Construct a 95% confidence interval for the mean StudyHrs for female/male students
# Is there evidence that the means are different? -- Probably, not, as males are in females CI
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

#Second, look if the difference of the means is statistically significant, 
#assuming independent samples, and NOT a matched pair interval:

#First, compute critical t and check if we need to pool variances
t_critical <- qt(0.975, 247)
F_stat <- sd_males**2 / sd_females**2
F_stat < t_critical # TRUE => pool variances

# Compute pooled variance
s_pooled <- sqrt(((n_females-1)*(sd_females)**2 + (n_males-1)*(sd_males))/247)
mean_diff <- mean_females-mean_males
CI_diff_lower <- mean_diff - t_critical * s_pooled* sqrt(1/n_females + 1/n_males)
CI_diff_upper <- mean_diff + t_critical * s_pooled* sqrt(1/n_females + 1/n_males)
print(paste("Difference of mean CI contains zero: [", CI_diff_lower,",", CI_diff_upper, "]"))


