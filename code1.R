init<-read.csv("E:/DataScience/nyc-rolling-sales.csv", sep = ",")
replaceWithNa<-lapply( init$LAND.SQUARE.FEET, function(col) as.numeric( gsub("-", "NA", col) ) )
#init[is.na(init)] <- "NA"
library(Amelia)
library(tidyverse)
#colnames(aq)<-gsub(".","_",colnames(aq),fixed = TRUE)
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(formattable)
library(knitr)
library(modelr)
library(plotly)
nyc_property <-as_tibble(init[,-1])
#̥nyc_property$BUILDING.CLASS.CATEGORY %>% mutate()
nyc_property <- nyc_property %>%
  separate(col = "BUILDING.CLASS.CATEGORY", into = c("BUILDING CLASS CATEGORY NUMBER",
                                                     "BUILDING CLASS CATEGORY"), sep = 3) %>%
  separate(col = "SALE.DATE", into = c("SALE DATE", "TIME"), sep = " ")
nyc_property <- nyc_property[,c(-8,-23)]
dim(nyc_property)
nyc_property<-nyc_property %>% separate(col = `YEAR.BUILT`,into = "YEAR BUILT")

nyc_property <- nyc_property %>%
  mutate(`BUILDING AGE` = 2017 - `YEAR BUILT`)
nyc_property %>% filter(duplicated(nyc_property) == TRUE) %>% nrow()
levels(nyc_property$BOROUGH) = c("Western New Yourk",
                                 "Finger Lakes",
                                 "Southern Tire",
                                 "Central New York",
                                 "North Country")
num <- c(15,16,17,20)
nyc_property %<>% mutate_at(num, funs(as.numeric(.)))


