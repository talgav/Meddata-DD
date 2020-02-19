library(tidyverse)

## Data from Ori (from which I'll extract depth)
new_data <- read_csv("data/all_uvc_data_201119.csv", na = c("", "NA"), col_types = cols(country = "c"))
depth_data <- new_data %>% select(site, lon, lat, trans, depth) %>% filter() %>% distinct()
summary(new_data)

# new_data %>% count(is.na(depth))

## MEData (data I want to update with depths)
old_data <- read_csv("data/medata_nov_2019.csv", na = c("", "NA"), col_types = cols(depth = "d")) %>%
  mutate(depth.x = depth) %>% select(-depth) %>% mutate(id = rownames(.))
str(old_data)
summary(old_data)

## No duplicated depth values for `new_data` or`old_data` or `depth_data` by themselves (only when merged)

## When merged, duplicates created, so which are the points with duplicates?
dups <- old_data %>% left_join(depth_data) %>% count(id) %>% arrange(desc(n)) %>% filter(n > 1) %>% select("id")
diff(dups) # Consecutive 1141-1281
## See the duplicated data
old_data %>% filter(id %in% dups$id) %>% distinct(site)
old_data %>% filter(id %in% dups$id) %>% distinct(lon, lat)

## Check which data rows are duplicates according to site, lon, lat:
old_data %>% filter(lon == 8.217 & lat == 41.121 & site == "ASINARA_add")
problematic_trans <- depth_data %>%
  filter(lon == 8.217 & lat == 41.121 & site == "ASINARA_add") %>% 
  count(trans) %>% filter(n > 1)
## These are 4 transects with problematic data.
## I can fix it by averaging (maybe, need to check) but ATM I'm just going to get rid of them.

new_data <- read_csv("data/all_uvc_data_201119.csv", na = c("", "NA"), col_types = cols(country = "c")) %>% 
  filter(lon != 8.217 & lat != 41.121 & site != "ASINARA_add" & trans != 48:51)
## Now the problematic Asinara_add transects are removed (35 rows total)
depth_data <- new_data %>% select(site, lon, lat, trans, depth) %>% filter() %>% distinct()
summary(new_data)
str(depth_data)

## Join the 2 together to get the depth
new_old_data <- old_data %>% left_join(depth_data) # 2 depth columns: `depth` (new data) and `depth.x` (old data)

## Create 1 column of depth and if it's unavailable from the new data - fetch it from the old data:
colnames(new_old_data)
new_old_data <- new_old_data %>%
  mutate(depth = if_else(is.na(depth), depth.x, depth)) %>%
  # And tidy everything up:
  select(data.origin, country, site, lon, lat, trans, species, sp.length, sp.n, season,
         protection, enforcement, total.mpa.ha, size.notake, yr.creation, age.reserve.yr,
         depth, a, b, tmean, trange, sal_mean, pp_mean, pp_range)

summary(new_old_data)

# write_csv(new_old_data, "data/uvc_data_19022020.csv")
