#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
iris

#you may work through each section or choose one challenge to focus on
#and work through as many examples as possible


#####################################
##### Review: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
iris_height <- left_join(iris, height, by="Species")

#####################################
##### Challenge: learn apply    #####
#####################################
#take the average of all sepal and petal widths & lengths 
#for iris setosa
#use the apply function to take this average
help(apply)
apply(iris[iris$Species == "setosa", 1:4], 2, "mean")
# to refer to columns
iris[ ,"Species"]


#####################################
##### Greater Challenge:        #####
##### learn for loops           #####
#####################################
#take the average of all sepal and petal widths & lengths 
#for iris setosa
#using a for loop
#read writing a for loop in R  stop at the next portion)
#https://www.r-bloggers.com/2015/12/how-to-write-the-first-for-loop-in-r/
avgs <- numeric()
for(i in 1:4){
  avgs[i] <- mean(iris[iris$Species == "setosa", i])
}

#####################################
#####################################
##### Extra challenge: for loops ####
##### and regression             ####
#####################################
#read writing a for loop in R  stop at the next portion)
#https://www.r-bloggers.com/2015/12/how-to-write-the-first-for-loop-in-r/

#Using only data for iris versicolor
#write a for loop
#that produces 3 regression tables
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length
#make a versicolor dataframe
versicolor <- iris[iris$Species == "versicolor",]
#variables in each regression
x <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y <- c("Sepal.Width", "Petal.Width", "Petal.Length")


