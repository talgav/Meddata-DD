library(tidyverse)
library(sf)

medata <- read.csv("med_raw.csv")
med_mat <- medata %>%
  select(lon, lat, species, sp.n) %>% 
  group_by(lon, lat, species) %>% 
  summarise(n()) %>% 
  spread(species, "n()")
  # med_mat[, 4:ncol(med_mat)][is.na(med_mat[, 4:ncol(med_mat)])] <- 0
View(med_mat)

med_map <- read_sf("C:/Users/shira/Documents/1Masters/Research/maps/med/iho.shp")
chromis_locations <- med_mat %>% 
  select(lon, lat, Chromis.chromis)

ggplot(chromis_locations) + 
  geom_map(data = med_map, map = med_map, lon = longitude, lat = latitude) + 
  geom_jitter(aes(x = lon, y = lat))
