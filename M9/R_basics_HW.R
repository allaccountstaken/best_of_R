# Name of activity: R Basics
# Your name: Dima Mikhaylov
# UVA computing ID: agp7dp


# QUESTION 1

# Changing directory and checking available files
setwd("/Users/dmitrymikhaylov/Documents/learn/uva/fall2021/STAT6021/best_of_R/M0")
list.files()

# Reading file new_students.txt and storing in variable data
file <- "new_students.txt"
data <- read.table(file, header = TRUE, sep = "", dec = ".")

# QUESTION 2

# Creting vectors with the data
Animal <- c('Lion', 'Dog', 'Fish', 'Elephant', 'Mole')
Rating <- c(5, 10, 4, 7, 3)

# Storing vectors in a frame and naming columns
mydata <- data.frame(Animal, Rating)
names(mydata) <- c("Animal","Rating") # variable columns

# Returning the frame with print
print(mydata)

# QUESTION 3:

#Simple subseting - only animals with a rating of 5 or higher are shown
mydata[which(mydata$Rating > 5), ]

#Order descending - the ratings are in descending order
subset(mydata[order(mydata$Rating, decreasing = TRUE),], Rating > 5)


# QUESTION 4:

# Assign value to a variable and check datatypes
x <- '5'
print(class(x))
x <- 5
print(class(x))

# Basic If-Else statement
var_1 <- 10
var_2 <- 7
if (var_1 >= var_2) {
  cat('var_1 is greater than or equal to var_2.')
} else {
  cat('var_1 is lesser than var_2.')
}

# While statement
i <- 1
while (i <= 5)
{
  print(i)
  i <- i+1
}

