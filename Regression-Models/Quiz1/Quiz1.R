attach(mtcars)
cor(mpg, wt) * sd(mpg)/sd(wt)
## [1] -5.344
detach(mtcars)