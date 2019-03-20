rm(list=ls())
library(Amelia)
library(tidyverse)
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(formattable)
library(knitr)
library(modelr)
library(plotly)
library(naniar)
library(dplyr)
library(caret)

#Loading the data in to a variable
dataset = read.csv("data.csv",sep = ",",stringsAsFactors = TRUE)
dim(dataset)
head(dataset)
#Handelling the missing Values
dataset$LAND.SQUARE.FEET<-gsub("-",NA,dataset$LAND.SQUARE.FEET)
dataset$GROSS.SQUARE.FEET<-gsub("-",NA,dataset$GROSS.SQUARE.FEET)
dataset$SALE.PRICE<-gsub("-",NA,dataset$SALE.PRICE)
mean(is.na(dataset))
dataset <- na.omit(dataset)
dim(dataset)
# final$newAge <- final %>% 
#   mutate(`BUILDING AGE` = 2017 - `YEAR.BUILT`)

dataset <- dataset %>%
  separate(col = "BUILDING.CLASS.CATEGORY", into = c("BUILDING CLASS CATEGORY NUMBER",
                                                     "BUILDING CLASS CATEGORY"), sep = 3) %>%
  separate(col = "SALE.DATE", into = c("SALE DATE", "TIME"), sep = " ")
remove_these<-c(-1,-8)#SerialNo.s,Ease-̥Ment,
dataset <-as_tibble(dataset[,remove_these])
View(dataset)
#dataset
#names(nyc_property)EVACALCULATIONS$NOPAT[is.na(EVACALCULATIONS$NOPAT)] = median(EVACALCULATIONS$NOPAT,na.rm = TRUE)
#class(dataset$LAND.SQUARE.FEET[20])
dataset$LAND.SQUARE.FEET[is.na(as.numeric(dataset$LAND.SQUARE.FEET))] = median(as.numeric(dataset$LAND.SQUARE.FEET), na.rm =
                                                                     TRUE)
dataset$GROSS.SQUARE.FEET[is.na(as.numeric(dataset$GROSS.SQUARE.FEET))] = median(as.numeric(dataset$GROSS.SQUARE.FEET), na.rm =
                                                                       TRUE)

dataset %>% filter(duplicated(dataset) == TRUE) %>% nrow()

#View(dataset)
fac <- c(1,3,4,5,8,11,18,19)
dataset %<>% mutate_at(fac, funs(factor(.)))
levels(dataset$BOROUGH) = c("Western New Yourk",
                          "Finger Lakes",
                          "Southern Tire",
                          "Central New York",
                          "North Country")
num <- c(15,16,17,20)
dataset %<>% mutate_at(num, funs(as.numeric(.)))
chr <- c(6,7)
dataset %<>% mutate_at(chr, funs(as.character(.)))
#dataset$`SALE DATE`<- ymd(dataset$`SALE DATE`)
# library("randomForest")
# saleNA%<>%mutate_at(fac, funs(na.omit(dataset)))#  na.omit(dataset$SALE.PRICE)
# rf <-randomForest(SALE.PRICE~.,data=saleNA, ntree=500) 
final <-
  subset(dataset,SALE.PRICE>100000,select = c(BOROUGH,
    LAND.SQUARE.FEET,
    GROSS.SQUARE.FEET,
    YEAR.BUILT,
    RESIDENTIAL.UNITS,
    COMMERCIAL.UNITS,
    TOTAL.UNITS,
    YEAR.BUILT,
    SALE.PRICE))
#final2 %>% filter(as.numeric(final.SALE.PRICE)<=100000) %>% nrow()
set.seed(1000)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(final), 0.8*nrow(final))  # row indices for training data
trainingData <- final[trainingRowIndex, ]  # model training data
testData  <- final[-trainingRowIndex, ]   # test data
dim(trainingData)
dim(testData)
anyNA(final)#it return's TRUE, if there exists any NA's and returned TRUE
summary(final)# this gives aggregate function on each column and NA's if exists
trctrl<-trainControl(method = "repeatedcv", number = 10, repeats = 3)
svmLinear1 <-train(as.numeric(SALE.PRICE) ~ ., data = trainingData, method = "svmLinear",
                  trControl =trctrl,
                  preProcess= c("center","scale"),
                  tuneLength = 10)


set.seed(1000)  # setting seed to reproduce results of random sampling
trainingRowIndex2 <- sample(1:nrow(trainingData), 0.1*nrow(trainingData))  # row indices for training data
trainingData2 <- trainingData[trainingRowIndex2, ]  # model training data
testData2  <- trainingData[-trainingRowIndex2, ]   # test data
dim(trainingData2)
dim(testData2)
anyNA(trainingData2)#it return's TRUE, if there exists any NA's and returned TRUE
summary(trainingData2)# this gives aggregate function on each column and NA's if exists
trctrl2<-trainControl(method = "repeatedcv", number = 10, repeats = 3)
svmLinear <-train(as.numeric(SALE.PRICE) ~ ., data = trainingData2, method = "svmLinear",
                  trControl =trctrl2,
                  preProcess= c("center","scale"),
                  tuneLength = 10)
#save(svmLinear, file = "C:/Users/Scientist/Desktop/svmLinear.rda")
