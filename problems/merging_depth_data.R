library(tidyverse)

# Data from Ori (from which I'll extract depth)
new_data <- read_csv("data/all_uvc_data_201119.csv", na = c("", "NA"), col_types = cols(country = "c"))
(depth_data <- new_data %>% select(site, lon, lat, trans, depth) %>% distinct())
str(new_data)

# MEData (data I want to update with depths)
old_data <- read_csv("data/all_uvc_data_101119.csv", na = c("", "NA"), col_types = cols(depth = "d")) %>%
  mutate(id = rownames(.), depth.x = depth) %>% select(-depth)
str(old_data)

summary(old_data)
# Join the 2 together to get the depth
new_old_data <- old_data %>% left_join(new_data)

colnames(new_old_data)
new_old_data <- new_old_data %>%
  mutate(depth = if_else(is.na(depth), depth.x, depth)) %>%
  select(data.origin, country, site, lon, lat, trans, species, sp.length, sp.n, season,
         protection, enforcement, total.mpa.ha, size.notake, yr.creation, age.reserve.yr,
         depth, a, b, tmean, trange, sal_mean, pp_mean, pp_range)

summary(new_old_data)

depth_miss <- new_old_data %>% filter(is.na(depth)) %>% distinct(site) %>% print(n = Inf)
new_depth_miss <- new_data %>% filter(is.na(depth)) %>% distinct(site) %>% print(n = Inf)
old_depth_miss <- old_data %>% filter(is.na(depth.x)) %>% distinct(site) %>% print(n = Inf)

setdiff(old_depth_miss, new_depth_miss) %>% print(n = Inf)


new_old_data <- new_old_data %>% mutate(id = row.names(.))

crete_data <- read_csv("data/crete_uvc_full.csv")
# crete_depth <- crete_data %>% select(site, lon, lat, trans, depth) %>% distinct()

full_data <- old_data %>% left_join(crete_data)
summary(full_data)

check <- full_data %>% count(id) %>% arrange(desc(n)) %>% distinct() %>% filter(n > 1) %>% .$id
check_full <- full_data[check,]
View(check_full)

