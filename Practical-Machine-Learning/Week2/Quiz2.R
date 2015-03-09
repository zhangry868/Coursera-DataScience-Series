# Problem 1.
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)
testIndex = createDataPartition(diagnosis, p=0.50, list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]
## OR
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Problem 2.
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training = mixtures[inTrain,]
testing = mixtures[-inTrain,]
xnames <- colnames(concrete)[1:8]
featurePlot(x=training[, xnames], y=training$CompressiveStrength, plot="pairs")
# No relation between the outcome and other variables
index <- seq_along(1:nrow(training))
ggplot(data=training, aes(x=index, y=CompressiveStrength)) + geom_point() + 
  theme_bw()
# Step-like pattern -> 4 categories
library(Hmisc)
cutCompressiveStrength <- cut2(training$CompressiveStrength, g=4)
summary(cutCompressiveStrength)
ggplot(data=training, aes(y=index, x=cutCompressiveStrength)) + 
  geom_boxplot() + geom_jitter(col="blue") + theme_bw()
# Another way
library(plyr)
splitOn <- cut2(training$Age, g=4)
splitOn <- mapvalues(splitOn, 
                     from=levels(factor(splitOn)), 
                     to=c("red", "blue", "yellow", "green"))
plot(training$CompressiveStrength, col=splitOn)
# There is a step-like pattern in the plot of outcome versus index 
# in the training set that isn't explained by any of the predictor 
# variables so there may be a variable missing.

# Problem 3.
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(Superplasticizer, data=training) # OR
ggplot(data=training, aes(x=Superplasticizer)) + geom_histogram() + theme_bw()
# There are a large number of values that are the same and even if 
# you took the log(SuperPlasticizer + 1) they would still all be 
# identical so the distribution would not be symmetric.
# There are values of zero so when you take the log() transform 
# those values will be -Inf.

# Problem 4.
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p=3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
ss <- training[,grep('^IL', x = names(training) )]
preProc <- preProcess(ss, method='pca', thresh=0.9, 
                      outcome=training$diagnosis)
preProc$rotation # 9

# Problem 5.
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p=3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
ILpredictors <- predictors[, IL]
df <- data.frame(diagnosis, ILpredictors)
inTrain <- createDataPartition(df$diagnosis, p=3/4)[[1]]
training <- df[inTrain, ]
testing <- df[-inTrain, ]
modelFit <- train(diagnosis ~ ., method="glm", data=training)
predictions <- predict(modelFit, newdata=testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)
acc1 <- C1$overall[1]
acc1 # Non-PCA Accuracy: 0.65 

modelFit <- train(training$diagnosis ~ ., 
                  method="glm", 
                  preProcess="pca", 
                  data=training, 
                  trControl=trainControl(preProcOptions=list(thresh=0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)
acc2 <- C2$overall[1]
acc2 # PCA Accuracy: 0.72