# SVM-classifier
#This classifier is used to classify the test data labels based on the knowledge gained by trained data set.It was coded in R.

library(e1071)
library(class)
sensorTagData <- read.csv("D:/Data/Book3.csv", header=FALSE)
head(sensorTagData)
# Assign names for the data vectors
names(sensorTagData) <- c("Time.Stamp", "Lux", "Ambient.Temperature", "Object.Temperature", "Pressure", "Temperature.Humidity", "Humidity", "Label")

#Preparing Training and Testing Datasets
index <- createDataPartition(sensorTagData$Label,p=0.67, list=FALSE)
# subset training set with index
sensorData.train <- sensorTagData[index,]
# Subset test set with index
sensorData.test <- sensorTagData[-index,]

model_svm <- svm(sensorData.train$Label~.,data = sensorData.train[2:3], kernel='linear', type='C-classification',gamma=0.2,cost=100)
summary(model_svm)
pred = predict(model_svm,sensorData.test[2:3])
plot(pred)
table(pred,sensorData.test$Label)
mean(pred==sensorData.test$Label)
confusionMatrix(pred,sensorData.test[,8])
library(gmodels)
CrossTable(x = sensorData.test[,8],y = pred, prop.chisq=FALSE)
