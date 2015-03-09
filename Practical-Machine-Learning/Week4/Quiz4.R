# Problem 1.
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
# fit rf predictor relating the factor variable y
fitRf <- train(y ~ ., data=vowel.train, method="rf")
fitGBM <- train(y ~ ., data=vowel.train, method="gbm")
predRf <- predict(fitRf, vowel.test)
predGBM <- predict(fitGBM, vowel.test)
# RF Accuracy: 0.6060606
confusionMatrix(predRf, vowel.test$y)$overall[1]
# GBM Accuracy: 0.530303
confusionMatrix(predGBM, vowel.test$y)$overall[1]
pred <- data.frame(predRf, predGBM, y=vowel.test$y, agree=predRf == predGBM)
head(pred)
accuracy <- sum(predRf[pred$agree] == pred$y[pred$agree]) / sum(pred$agree)
accuracy # Agreement Accuracy: 0.6569579

# Problem 2.
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p=3/4)[[1]]
training <- adData[inTrain, ]
testing <- adData[-inTrain, ]
dim(adData) # 333 131
# head(adData)
set.seed(62433)
fitRf <- train(diagnosis ~ ., data=training, method="rf")
fitGBM <- train(diagnosis ~ ., data=training, method="gbm")
fitLDA <- train(diagnosis ~ ., data=training, method="lda")
predRf <- predict(fitRf, testing)
predGBM <- predict(fitGBM, testing)
predLDA <- predict(fitLDA, testing)
pred <- data.frame(predRf, predGBM, predLDA, diagnosis=testing$diagnosis)
# Stack the predictions together using random forests ("rf")
fit <- train(diagnosis ~., data=pred, method="rf")
predFit <- predict(fit, testing)
c1 <- confusionMatrix(predRf, testing$diagnosis)$overall[1]
c2 <- confusionMatrix(predGBM, testing$diagnosis)$overall[1]
c3 <- confusionMatrix(predLDA, testing$diagnosis)$overall[1]
c4 <- confusionMatrix(predFit, testing$diagnosis)$overall[1]
print(paste(c1, c2, c3, c4)) 
# Stacked Accuracy: 0.79 is better than random forests and lda 
# and the same as boosting.

# Problem 3.
set.seed(3523)
library(AppliedPredictiveModeling)
library(elasticnet)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength, 
                               p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]
set.seed(233)
fit <- train(CompressiveStrength ~ ., data=training, method="lasso")
fit
plot.enet(fit$finalModel, xvar="penalty", use.color=T) # Cement

# Problem 4.
library(lubridate)  # For year() function below
library(forecast)
dat <- read.csv("./data/gaData.csv")
training <- dat[year(dat$date) < 2012, ]
testing <- dat[(year(dat$date)) > 2011, ]
tstrain <- ts(training$visitsTumblr)
fit <- bats(tstrain)
fit
pred <- forecast(fit, level=95, h=dim(testing)[1])
names(data.frame(pred))
predComb <- cbind(testing, data.frame(pred))
names(testing)
names(predComb)
predComb$in95 <- (predComb$Lo.95 < predComb$visitsTumblr) & 
  (predComb$visitsTumblr < predComb$Hi.95)
# How many of the testing points is the true value within the 
# 95% prediction interval bounds?
prop.table(table(predComb$in95))[2] # 0.9617021

# Problem 5.
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]
set.seed(325)
fit <- svm(CompressiveStrength ~., data=training)
# OR another way
# fit <- train(CompressiveStrength ~. data=training, method="svmRadial")
pred <- predict(fit, testing)
acc <- accuracy(pred, testing$CompressiveStrength)
acc
acc[2] # RMSE 6.715009