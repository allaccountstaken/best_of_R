
#title: 'STAT 6021: Project 2'
#author: "Dima Mikhaylov"
#date: "12/13/2021"


library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(Hmisc)
library(miscset)
library(dplyr)
library(gridExtra)

# loading the data
house <- read.csv("house_data.csv")

# converting to factors
house$view <- factor(house$view, ordered = TRUE, levels = c(0, 1, 2, 3, 4))
house$condition <- factor(house$condition, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
house$grade <- factor(house$grade, ordered = TRUE, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
house$waterfront <- factor(house$waterfront, ordered = TRUE, levels = c(0, 1))

# dropping geo and date variables
house <- subset(house, select=-c(id,num, date, zipcode, lat, long))


# plotting original categorical variables
sp1 <- ggplot(house, aes(x=sqft_living, y=price, color=waterfront))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Possible 'waterfront' interactions")


sp2 <- ggplot(house, aes(x=sqft_living, y=price, color=view))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Possible 'view' interactions")


sp3 <- ggplot(house, aes(x=sqft_living, y=price, color=condition))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Possible 'condition' interactions")


sp4 <- ggplot(house, aes(x=sqft_living, y=price, color=grade))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Possible 'grade' interactions")

##produce the 4 density plots in a 2 by 2 matrix
grid.arrange(sp1+scale_color_grey(), 
             sp2+scale_color_grey(), 
             sp3+scale_color_grey(), 
             sp4+scale_color_grey(), 
             ncol = 2, 
             nrow = 2)



# plotting distribution of original categorical variables
ggplotGrid(ncol = 2, lapply(c("view", "waterfront", "condition", "grade"),
                            function(col) {
                              ggplot(house, aes_string(col)) + geom_bar() + coord_flip()
                            }))+ scale_color_grey()


# plotting box plots of original categorical variables
bp1 <- ggplot(house, aes(x=waterfront, y=price))+
  geom_boxplot()+
  labs(x="waterfront", y="price", title="Price by 'waterfront'")

bp2 <- ggplot(house, aes(x=view, y=price))+
  geom_boxplot()+
  labs(x="view", y="price", title="Price by 'view'")

bp3 <- ggplot(house, aes(x=condition, y=price))+
  geom_boxplot()+
  labs(x="condition", y="price", title="Price by 'condition'")

bp4 <- ggplot(house, aes(x=grade, y=price))+
  geom_boxplot()+
  labs(x="grade", y="price", title="Price by 'grade'")

##produce the 4 plots in a 2 by 2 matrix
grid.arrange(bp1, bp2, bp3, bp4, ncol = 2, nrow = 2)

#changing `view` to 0 for regular view and 1 for every other view
house$view <- factor(ifelse(house$view!=0, 1, 0))
#changing `condition` to 0 for everything below 3 and 1 otherwise
house$condition <- factor(ifelse(house$condition==1 | house$condition==2 | house$condition==3, 0, 1))
#changing `grade` to 0 for everything below 7 and 1 otherwise
house$grade <- factor(ifelse(house$grade==1 | house$grade==2 | house$grade==3 |
                               house$grade==4 | house$grade==5 | house$grade==7 , 0, 1))


#plotting distributions of remapped categorical variables
ggplotGrid(ncol = 2,
           lapply(c("view", "waterfront", "condition", "grade"),
                  function(col) {
                    ggplot(house, aes_string(col)) + geom_bar() + coord_flip()
                  }))


#plotting box plots of remapped categorical variables
bp1 <- ggplot(house, aes(x=waterfront, y=price))+
  geom_boxplot()+
  labs(x="waterfront", y="price", title="Price by 'waterfront'")

bp2 <- ggplot(house, aes(x=view, y=price))+
  geom_boxplot()+
  labs(x="view", y="price", title="Price by 'view'")

bp3 <- ggplot(house, aes(x=condition, y=price))+
  geom_boxplot()+
  labs(x="condition", y="price", title="Price by 'condition'")

bp4 <- ggplot(house, aes(x=grade, y=price))+
  geom_boxplot()+
  labs(x="grade", y="price", title="Price by 'grade'")

##produce the 4 plots in a 2 by 2 matrix
grid.arrange(bp1, bp2, bp3, bp4, ncol = 2, nrow = 2)




#interactions of remapped categorical variables
sp1 <- ggplot(house, aes(x=sqft_living, y=price, color=waterfront))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Remaining 'waterfront' interactions")

sp2 <- ggplot(house, aes(x=sqft_living, y=price, color=view))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Remaining 'view' interactions")

sp3 <- ggplot(house, aes(x=sqft_living, y=price, color=condition))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Remaining 'condition' interactions")

sp4 <- ggplot(house, aes(x=sqft_living, y=price, color=grade))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="price",
       title="Remaining 'grade' interactions")

##produce the 4 plots in a 2 by 2 matrix
grid.arrange(sp1+scale_color_grey(), 
             sp2+scale_color_grey(), 
             sp3+scale_color_grey(), 
             sp4+scale_color_grey(), 
             ncol = 2, 
             nrow = 2)



#final check, same scatter plots but with log(price) - no visiable interaction with log price.
sp1 <- ggplot(house, aes(x=sqft_living, y=log(price), color=waterfront))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="log(price)",
       title="Additive 'waterfront' indicator")

sp2 <- ggplot(house, aes(x=sqft_living, y=log(price), color=view))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="sqft_living", 
       y="log(price)",
       title="Additive 'view' indicator")

sp3 <- ggplot(house, aes(x=sqft_living, y=log(price), color=condition))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  scale_fill_manual(values=c("red", "blue", "green"))+
  labs(x="sqft_living", 
       y="log(price)",
       title="Additive 'condition' indicator")

sp4 <- ggplot(house, aes(x=sqft_living, y=log(price), color=grade))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  scale_fill_manual(values=c("red", "blue", "green"))+
  labs(x="sqft_living", 
       y="log(price)",
       title="Additive 'grade' indicator")


##produce the 4 plots in a 2 by 2 matrix
grid.arrange(sp1+scale_color_grey(), 
             sp2+scale_color_grey(), 
             sp3+scale_color_grey(), 
             sp4+scale_color_grey(), 
             ncol = 2, 
             nrow = 2)




#selecting quantitative predictors
quant_vars = c("yr_built", "yr_renovated",
               "floors", "bedrooms", "bathrooms", 
               "sqft_living", "sqft_lot", 
               "sqft_above", "sqft_basement", 
               "sqft_living15", "sqft_lot15")

hist.data.frame(house[,quant_vars])


#filtering out 0 bedrooms and bathrooms
house <- filter(house, bathrooms != 0, bedrooms != 0)

#dropping sqft_basement
house <- subset(house, select=-sqft_basement)

#creating 'age' feature 
house$age = ifelse(2021-house$yr_renovated >= 2021-house$yr_built, 2021-house$yr_built, 2021-house$yr_renovated)

#dropping yr_renovated and yr_built
house <- subset(house, select=-c(yr_renovated, yr_built))


#final quantitative predictors histogram
quant_vars = c("age", "bedrooms", "bathrooms", "floors",
               "sqft_living", "sqft_lot", "sqft_above",  
               "sqft_living15", "sqft_lot15")

hist.data.frame(house[,quant_vars])

#plotting correlations of price with quatitative predictors
grey_shades <- c('#DCDCDC','white',	'#696969')
corr <- round(cor(house[,c("price",quant_vars)]), 1)
ggcorrplot(corr, 
           method = "circle", 
           lab = TRUE,
           # type = "lower", 
           outline.color = "white", 
           ggtheme = ggplot2::theme_gray,
           colors = grey_shades)#c("#6D9EC1", "white", "#E46726"))


#final set of predictors
house <- subset(house, select=-sqft_above)
str(house)


#saving to file - done!
#write_csv(house, 'house_data_cleaned.csv')











