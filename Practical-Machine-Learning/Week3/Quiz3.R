#Quiz3
#Problem 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
data <- segmentationOriginal
set.seed(125)
inTrain <- data$Case == "Train"
trainData <- data[inTrain, ]
testData <- data[!inTrain, ]
cartModel <- train(Class ~ ., data=trainData, method="rpart")
cartModel$finalModel

# n= 1009 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 1) root 1009 373 PS (0.63032706 0.36967294)  
#   2) TotalIntenCh2< 45323.5 454  34 PS (0.92511013 0.07488987) *
#   3) TotalIntenCh2>=45323.5 555 216 WS (0.38918919 0.61081081)  
#     6) FiberWidthCh1< 9.673245 154  47 PS (0.69480519 0.30519481) *
#     7) FiberWidthCh1>=9.673245 401 109 WS (0.27182045 0.72817955) *

plot(cartModel$finalModel, uniform=T)
text(cartModel$finalModel, cex=0.8)
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 => PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 => WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 => PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 => Not possible to predict 

# Problem 2

# Q: If K is small in a K-fold cross validation is the bias in the 
# estimate of out-of-sample (test set) accuracy smaller or bigger?
# A: bias is larger
# If K is small is the variance in the estimate of out-of-sample
# (test set) accuracy smaller or bigger. 
# A: variance is smaller
# Is K large or small in leave one out cross validation?
# A: Under leave one out cross validation K is equal to the sample 
# size.

# Program 3

library(pgmm)
data(olive)
olive = olive[,-1]
treeModel <- train(Area ~ ., data=olive, method="rpart2")
newdata <- as.data.frame(t(colMeans(olive)))
predict(treeModel, newdata)

#2.875. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

# Program 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train <- sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA <- SAheart[train,]
testSA <- SAheart[-train,]
set.seed(13234)
logitModel <- train(chd ~ age + alcohol + obesity + tobacco + 
                      typea + ldl, data=trainSA, method="glm", 
                    family="binomial")
missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
predictTrain <- predict(logitModel, trainSA)
predictTest <- predict(logitModel, testSA)
# Training Set Misclassification rate
missClass(trainSA$chd, predictTrain) # 0.2727273
# Test Set Misclassification rate
missClass(testSA$chd, predictTest) # 0.3116883

#Problem 5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
head(vowel.test)
dim(vowel.train) # 528  11
dim(vowel.test) # 462  11
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modelRf <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
order(varImp(modelRf), decreasing=T)
# The order of the variables is:
#  x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10