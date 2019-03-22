rm(list=ls())
# library(doMC)
# registerDoMC(cores = 6)
library(doParallel)
library(foreach)
library(plyr)
cl <- makeCluster(8)  # Use 8 cores
registerDoParallel(cl) # register these 8 cores with the "foreach" package
final<-read.csv("final.csv", sep = ",")
#View(final) #Optional keep comented to save RAM space
set.seed(1000)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(final), 0.8*nrow(final))  # row indices for training data
trainingData <- final[trainingRowIndex, ]  # model training data
testData  <- final[-trainingRowIndex, ]   # test data
dim(trainingData)
dim(testData)
anyNA(trainingData)
anyNA(testData)#it return's TRUE, if there exists any NA's and returned TRUE
dim(trainingData)
dim(testData)
summary(trainingData)# this gives aggregate function on each column and NA's if exists
summary(testData)
trctrl<-trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)
svmLinear1 <-train(as.numeric(BOROUGH) ~ ., data = trainingData, method = "svmLinear",
                   trControl =trctrl,
                   preProcess= c("center","scale"),
                   tuneLength = 10)
#save(svmLinear1, file = "/home/scientist/Documents/DataScience/R/RProjects/RealEstateSalePrices/model2BOROUGH.rda")
svmPredict<-predict(svmLinear1, newdata = testData)

# library(e1071)
# test<-e1071::svm(SALE.PRICE ~ ., data = trainingData)
# table(predict(test), trainingData$SALE.PRICE, dnn=c("Prediction", "Actual"))
# confusionMatrix(trainingData$SALE.PRICE, predict(test))
x1<-subset(trainingData, select = -BOROUGH)
y1<-as.factor(trainingData$BOROUGH)
svmMod<-svm(as.factor(BOROUGH) ~ ., data = trainingData)
predict1<-predict(svmMod2,x1)
table(pr1,y1)

