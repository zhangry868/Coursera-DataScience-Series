library(caret)
library(randomForest)
library(ggplot2)

setwd('C:/Users/YourDream/Documents/GitHub/Coursera-DataScience-Series/Practical-Machine-Learning')

download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', 'training.csv') #, method='curl')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', 'testing.csv') #, method='curl')

raw_testing <- read.csv('testing.csv', na.strings=c("#DIV/0!"))
data <- read.csv('training.csv', na.strings=c("#DIV/0!") )

cTest <- raw_testing
for(i in c(8:ncol(cTest)-1)) {cTest[,i] = as.numeric(as.character(cTest[,i]))}
cData <- data
for(i in c(8:ncol(cData)-1)) {cData[,i] = as.numeric(as.character(cData[,i]))}

featuresnames <- colnames(cData[colSums(is.na(cData)) == 0])[-(1:7)]
features <- cData[featuresnames]

submit_Testing <- cTest[featuresnames[1:52]]
set.seed(5188)
xdata <- createDataPartition(y=features$classe, p=3/4, list=FALSE )
training <- features[xdata,]
testing <- features[-xdata,]

num_features_idx = which(lapply(training,class) %in% c('numeric'))

#model <- foreach(ntree=rep(150, 4), .combine=randomForest::combine) %dopar% randomForest(training[-ncol(training)], training$classe, ntree=ntree)

rf_model  <- randomForest(classe ~ ., training, ntree=500, mtry=32)

training_pred <- predict(rf_model, training) 
print(confusionMatrix(training_pred, training$classe))

testing_pred <- predict(rf_model, testing) 
print(confusionMatrix(testing_pred, testing$classe))

answers <- predict(rf_model, submit_Testing) 

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(as.character(answers))