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
init<-read.csv("E:/DataScience/nyc-rolling-sales.csv", sep = ",")
remove_these<-c(-1,-8,-23)#SerialNo.s,Ease-̥Ment,
nyc_property <-as_tibble(init[,remove_these])
levels(nyc_property$BOROUGH) = c("Western New Yourk",
                                 "Finger Lakes",
                                 "Southern Tire",
                                 "Central New York",
                                 "North Country")
#names(nyc_property)
nyc_property <- nyc_property %>%
  separate(col = "BUILDING.CLASS.CATEGORY", into = c("BUILDING CLASS CATEGORY NUMBER",
                                                     "BUILDING CLASS CATEGORY"), sep = 3) %>%
  separate(col = "SALE.DATE", into = c("SALE DATE", "TIME"), sep = " ")
nyc_property <- nyc_property %>%
  
  
  
  
  
  
  
  
  
  
  



















































































  mutate(`BUILDING AGE` = 2017 - `YEAR.BUILT`)
nyc_property %>% filter(duplicated(nyc_property) == TRUE) %>% nrow()
nyc_property <- unique(nyc_property)
nyc_property %>% replace_with_na

