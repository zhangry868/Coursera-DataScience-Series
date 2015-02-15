#Question 1
#Review the lecture Note

#Question 2
library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)

#Question 3
#Review the lecture Note
#panel.abline()

#Question 4
library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)

#Question 5
?trellis.par.set
#Functions used to query, display and modify graphical parameters for fine control of Trellis displays. Modifications are made to the settings for the currently active device only.
?print.trellis
#This is the default print method for objects of class "trellis", produced by calls to functions like xyplot, bwplot etc.
?splom
#Draw Conditional Scatter Plot Matrices and Parallel Coordinate Plots
?lines
#A generic function taking coordinates given in various ways and joining the corresponding points with line segments.

#Questin 6
#ggplot2: An  implementa:on	of	the	Grammar	of	Graphics by	Leland Wilkinson	
#Writen	by	Hadley	Wickham	(while	he	was	a	graduate	student	at	Iowa	State)

#Question 7
airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

#Question 8
#Review the Lecture Note
#Plots are	made up of aesthetics(size, shape, color)	and	geoms	(points,	lines)

#Question 9
#library(ggplot2)
#g <- ggplot(movies, aes(votes, rating))
#print(g)
#Error: No layers in plot

#Question 10
#qplot(votes, rating, data = movies) + geom_smooth()