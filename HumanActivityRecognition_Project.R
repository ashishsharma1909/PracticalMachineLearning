# Load the caret package
library(caret)
# Read the data
training<-read.csv("pml-training.csv",header = T)
testing<-read.csv("pml-testing.csv",header = T)
# Exploratory analysis
dim(training)
sum(complete.cases(training))
dim(testing)
sum(complete.cases(testing))
trainMissing <- is.na.data.frame(training)
trainMissing2 <- apply(trainMissing, 2, sum) 
testMissing <- is.na.data.frame(testing)
testMissing2 <- apply(testMissing, 2, sum)
par(mfrow=c(1,2))
plot(trainMissing2, xlab = "Variables", ylab = "Number of NAs") 
plot(testMissing2, xlab = "Variables", ylab = "Number of NAs")
mtext("Columns with Missing Values in Training and Testing Sets",side = 3,line = -3,outer = T,font = 2)
# Cleaning data
trainingClean <- training[, colSums(is.na(training))<nrow(training)*0.60]
dim(trainingClean)
testingClean <- testing[, colSums(is.na(testing))<nrow(testing)*0.60]
dim(testingClean)
trainingClean2 <- trainingClean[,names(trainingClean) %in% names(testingClean)]
trainingClean2$classe <- trainingClean$classe
trainingClean2 <- trainingClean2[,8:60]
testingClean <- testingClean[,8:60]
# Apply Random Forest
inTrain <- createDataPartition(trainingClean2$classe, p=0.6, list = F)
myTraining <- trainingClean2[inTrain,]
myTesting <- trainingClean2[-inTrain,]
set.seed(38)
controlRF <- trainControl(method = "cv", 3)
modRF <- train(classe~., data=myTraining, method="rf", trControl=controlRF)
predRF <- predict(modRF, myTesting)
ConfMatRF <- confusionMatrix(predRF, myTesting$classe)
ConfMatRF$table
ConfMatRF$overall
par(mfrow=c(1,1))
plot(modRF$finalModel, main="RF Model Error vs Number of Trees")
# Apply Generalised Boosting
modGBM <- train(classe~., data = myTraining, method="gbm", trControl=controlRF, verbose=F)
predGBM <- predict(modGBM, myTesting)
ConfMatGBM <- confusionMatrix(predGBM, myTesting$classe)
ConfMatGBM$table
ConfMatGBM$overall
# Predict the TEST data
predTEST <- predict(modRF, testingClean)
predTEST
