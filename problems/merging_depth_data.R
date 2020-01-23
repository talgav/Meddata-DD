library(tidyverse)

# Data from Ori (from which I'll extract depth)
new_data <- read_csv("data/all_uvc_data_201119.csv", na = c("", "NA"), col_types = cols(country = "c"))
(depth_data <- new_data %>% select(site, lon, lat, trans, depth) %>% distinct())
str(new_data)

# MEData (data I want to update with depths)
old_data <- read_csv("data/medata_nov_2019.csv", na = c("", "NA"), col_types = cols(depth = "d")) %>%
  mutate(depth.x = depth) %>% select(-depth) %>% mutate(id = rownames(.))
str(old_data)
summary(old_data)

# Join the 2 together to get the depth
new_old_data <- old_data %>% left_join(depth_data)

new_old_data %>% count(id) %>% arrange(desc(n))

colnames(new_old_data)
new_old_data <- new_old_data %>%
  mutate(depth = if_else(is.na(depth), depth.x, depth)) %>%
  select(data.origin, country, site, lon, lat, trans, species, sp.length, sp.n, season,
         protection, enforcement, total.mpa.ha, size.notake, yr.creation, age.reserve.yr,
         depth, a, b, tmean, trange, sal_mean, pp_mean, pp_range)

summary(new_old_data)

old_data %>% 
  sample_n(1000) %>% 
  left_join(depth_data) %>% 
  count(id) %>% 
  arrange(desc(n))

new_data %>% 
  filter(site == "ASINARA_add", trans == "48") %>% 
  select(lon, lat, site, trans, depth) %>% 
  distinct()

