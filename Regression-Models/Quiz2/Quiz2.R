# Problem 1.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y~x)
summary(fit)$coefficients
# Problem 2.
n<-9
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
e<-y-beta0-beta1*x
sigma<-sqrt(sum(e^2)/(n-2))
# Problem 3.
fit2 <- lm(mpg~I(wt),data = mtcars)
predict(fit2,data.frame(x=mean(x)), interval="confidence")

# Problem 5.
data(mtcars)
predict(fit, data.frame(x=mean(3)), interval="prediction")
# upr: 27.57355

# Problem 6.
fit2 <- lm(y ~ I(x / 2))
tbl2 <- summary(fit2)$coefficients
mean <- tbl2[2,1]      
se <- tbl2[2,2] 
df <- fit2$df
#Two sides T-Tests
mean + c(-1,1) * qt(0.975, df=df) * se
# -12.97262  -8.40527

# Problem 7.
summary(fit)$coefficients[2, 1]
fit3 <- lm(y ~ I(x / 100))
summary(fit3)$coefficients[2, 1]
# It would get multiplied by 100.

# Problem 8. 
# Y = beta0 + beta1 * X + epsilon = 
# beta1 * (X + c) + (beta0 - beta1 * c)
# New intercept: beta0 - c*beta1

# Problem 9.
fitRes <- fit$residuals ^ 2
fitIntercept <- lm(mpg ~ 1, mtcars)
fitInterceptRes <- fitIntercept$residuals ^ 2
sum(fitRes) /sum(fitInterceptRes) # 0.2471672

# Problem 10.
sum(resid(fit))
# If an intercept is included, then they will sum to 0.