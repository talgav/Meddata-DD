library(tidyverse)

crete_data <- read.csv("~/Lab stuff/Crete/crete2019/UVC_crete_2019.csv")

crete_data %>% 
glimpse(crete_data)

crete_data_mod <- crete_data %>%
  filter(Distance <= 2.5)

## choosing one observer in random

crete = data.frame()

for(i in unique(crete_data_mod$TransID)) {
  trans = crete_data_mod %>%
    filter(crete_data_mod$TransID == i)
  
  observer=unique(trans$observer)
  random_observer=sample(observer,1)
  
  trans=trans%>%
    filter(observer==random_observer)
  
  biob=rbind.data.frame(biob,trans)
  
}