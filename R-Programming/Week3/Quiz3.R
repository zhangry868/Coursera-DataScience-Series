#Question1 
#Take a look at the 'iris' dataset that comes with R. The data can be loaded with the code:
library(datasets)
data(iris)
#A description of the dataset can be found by running
?iris
#There will be an object called 'iris' in your workspace. In this dataset,what is the mean of 'Sepal.Length' for the species virginica? (Please only enter the numeric result and nothing else.)

sapply(split(iris$Sepal.Length, iris$Species), mean)

# setosa versicolor  virginica 
# 5.006      5.936      6.588 


#Question2
#Continuing with the 'iris' dataset from the previous Question, what R code returns a vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean)
#1 row 2 column


#Question3
#Load the 'mtcars' dataset in R with the following code
library(datasets)
data(mtcars)
#There will be an object names 'mtcars' in your workspace. You can find some information about the dataset by running
?mtcars
#How can one calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?
sapply(split(mtcars$mpg, mtcars$cyl), mean)

#Question4
#Continuing with the 'mtcars' dataset from the previous Question, what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
sapply(split(mtcars$hp, mtcars$cyl), mean)
#        4         6         8 
# 82.63636 122.28571 209.21429 

#Question5
#If you run
debug(ls)
#Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.