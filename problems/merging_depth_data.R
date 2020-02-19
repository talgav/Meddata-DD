library(tidyverse)

medata <- read_csv("data/uvc_data_200120.csv", col_types = cols(total.mpa.ha = col_double(), size.notake = col_double()))
summary(medata)
medata %>% filter(site != "Cret12", site != "Cret13", site != "Cret14", 
                  site != "Cret16", site != "Cret18", site != "Cret19",
                  site != "Cret20", site != "Cret22") %>% count(is.na(depth))

# medata %>% filter(country == "Greece") %>% select(site) %>% distinct()

## Data from Ori (from which I'll extract depth)
new_data <- read_csv("data/all_uvc_data_201119.csv", na = c("", "NA"), col_types = cols(country = "c"))
depth_data <- new_data %>% select(site, lon, lat, trans, depth) %>% distinct()
summary(new_data)

# new_data %>% count(is.na(depth))

## MEData (data I want to update with depths)
old_data <- read_csv("data/medata_nov_2019.csv", na = c("", "NA"), col_types = cols(depth = "d")) %>%
  mutate(depth.x = depth) %>% select(-depth) %>% mutate(id = rownames(.))
str(old_data)
summary(old_data)

## No duplicated depth values for `new_data` or`old_data` or `depth_data` by themselves (only when merged)


## Join the 2 together to get the depth
new_old_data <- old_data %>% left_join(depth_data)

## Check for duplicate depths (same point with > 1 depth value)
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

