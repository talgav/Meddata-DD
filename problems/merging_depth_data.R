library(tidyverse)

missing_sites <- setdiff(old_data$site, new_data$site)



new_data <- read_csv("data/all_uvc_data_201119.csv", na = c("", "NA"), col_types = cols(country = "c")) %>% 
  select(-X1)
new_data$species <- str_replace(string = new_data$species, pattern = "\\ ", replacement = "\\.")
depth_data <- new_data %>% select(site, lon, lat, trans, depth)
depth_data
str(new_data)

old_data <- read_csv("data/all_uvc_data_101119.csv", na = c("", "NA"), col_types = cols(depth = "d")) %>% 
  select(-depth)

str(old_data)

colnames(old_data)
colnames(depth_data)


new_old_data <- old_data %>% inner_join(depth_data)

updated <- new_old_data %>% select(data.origin, country, site, lon, lat, trans, species, sp.length, sp.n,
                                    season, protection, enforcement, total.mpa.ha, size.notake,
                                    yr.creation, age.reserve.yr, depth.y, a, b, tmean, trange,
                                    sal_mean, pp_mean, pp_range)

colnames(updated) <- c("data.origin", "country", "site", "lon", "lat", "trans", "species", "sp.length",
                       "sp.n", "season", "protection", "enforcement", "total.mpa.ha", "size.notake",
                       "yr.creation", "age.reserve.yr", "depth", "a", "b", "tmean", "trange",
                       "sal_mean", "pp_mean", "pp_range")
updated$depth
summary(updated)

updated %>% select(depth) %>% na.omit()

