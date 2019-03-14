
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

#Loading the data in to a variable
dataset = read.csv("data.csv",sep = ",",stringsAsFactors = TRUE)
dim(dataset)
final <- na.omit(dataset)
dim(dataset)

#Handelling the missing Values
dataset$LAND.SQUARE.FEET<-gsub("-",NA,dataset$LAND.SQUARE.FEET)
dataset$GROSS.SQUARE.FEET<-gsub("-",NA,dataset$GROSS.SQUARE.FEET)
dataset$SALE.PRICE<-gsub("-",NA,dataset$LAND.SQUARE.FEET)
mean(is.na(dataset))

final <- na.omit(dataset)
dim(final)
dataset$newAge <- dataset %>% 
  mutate(`BUILDING AGE` = 2017 - `YEAR.BUILT`)
dataset <- dataset %>%
  separate(col = "BUILDING.CLASS.CATEGORY", into = c("BUILDING CLASS CATEGORY NUMBER",
                                                     "BUILDING CLASS CATEGORY"), sep = 3) %>%
  separate(col = "SALE.DATE", into = c("SALE DATE", "TIME"), sep = " ")
remove_these<-c(-1,-8,-23)#SerialNo.s,Ease-̥Ment,
dataset <-as_tibble(dataset[,remove_these])
dataset
levels(dataset$BOROUGH) = c("Western New Yourk",
                                 "Finger Lakes",
                                 "Southern Tire",
                                 "Central New York",
                                 "North Country")
#names(nyc_property)EVACALCULATIONS$NOPAT[is.na(EVACALCULATIONS$NOPAT)] = median(EVACALCULATIONS$NOPAT,na.rm = TRUE)
class(dataset$LAND.SQUARE.FEET[20])
dataset$LAND.SQUARE.FEET[is.na(as.numeric(dataset$LAND.SQUARE.FEET))] = median(as.numeric(dataset$LAND.SQUARE.FEET), na.rm =
                                                                     TRUE)
dataset$GROSS.SQUARE.FEET[is.na(as.numeric(dataset$GROSS.SQUARE.FEET))] = median(as.numeric(dataset$GROSS.SQUARE.FEET), na.rm =
                                                                       TRUE)

dataset %>% filter(duplicated(dataset) == TRUE) %>% nrow()

View(dataset)
fac <- c(1,3,4,5,8,11,18,19)
dataset %<>% mutate_at(fac, funs(factor(.)))
num <- c(15,16,17,20)
dataset %<>% mutate_at(num, funs(as.numeric(.)))
chr <- c(6,7)
dataset %<>% mutate_at(chr, funs(as.character(.)))
dataset$`SALE DATE` <- ymd(dataset$`SALE DATE`)
library("randomForest")
saleNA%<>%mutate_at(fac, funs(na.omit(dataset)))#  na.omit(dataset$SALE.PRICE)
rf <-randomForest(SALE.PRICE~.,data=saleNA, ntree=500) 


