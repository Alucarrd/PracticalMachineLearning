setwd("/Users/petinatan/data/DataScience/")


#load the necessary library
library(RCurl)
library(caret)
library(randomForest)
library(ggplot2)
library(gbm)
library(rpart)



#Import the training data set
trainingURL <- getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
trainingData <- read.csv(text = trainingURL , na.strings=c("NA","#DIV/0!", ""))

#Import the final testing data set
testingURL <- getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
testingData <- read.csv(text = testingURL, na.strings=c("NA","#DIV/0!", ""))

#Replace all the NA value to 0
trainingData[is.na(trainingData)] <- 0
testingData[is.na(testingData)] <- 0

#Remove the non-feature columns from the data frame
trainingData <- trainingData[, -c(1:7)]
testingData <- testingData[, -c(1:7)]


##Split data set into training and testing to train our model

inTrain <- createDataPartition(trainingData$classe, p=.70, list=FALSE)
training <- trainingData[inTrain,]
testing <- trainingData[-inTrain,]

plot(training$classe, col="blue", main="Classe with Counts", xlab="classe", ylab="Count")



#Train our decision tree model
model.rpart <- rpart(classe ~ ., data=training, method="class")

# Predicting 
predict.rpart <- predict(model.rpart, testing, type = "class")
#Review the accuracy
confusionMatrix(predict.rpart, testing$classe)

model.gbm <- train(classe ~ ., method="gbm", data=training, verbose=FALSE)
predict.gbm<- predict(model.gbm, testing)
confusionMatrix(predict.gbm, testing$classe)


model.rf <- train(classe ~ ., method="rf", data=training, type="class")
predict.rf<- predict(model.rf, testing)
confusionMatrix(predict.rf, testing$classe)


prediction <- predict(model.rf, testingData)
prediction



product_createFile = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

product_createFile(prediction)




