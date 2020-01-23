
# Load mediterranean data
medata <- read_csv("data/all_uvc_data_101119.csv", na = c("", "NA"), col_types = cols(depth = "d"))
count(medata)

# remove the old, irrelevant, crete data
uvc_data <- read_csv("data/all_uvc_data_101119.csv", na = c("", "NA"), col_types = cols(depth = "d")) %>%
  filter(!str_detect(.$site, "Cret"))

count(medata) - count(uvc_data)

a <- uvc_data %>% dplyr::select(site, lon, lat, trans, depth) %>% 
  distinct()

a %>% count(site,lon,lat,trans) %>% arrange(desc(n))

crete <- read_csv("data/crete_uvc.csv")

# check there's no depth duplicates
a <- crete %>% select(site, lon, lat, trans, depth) %>% 
  distinct()
a %>% count(site,lon,lat,trans) %>% arrange(desc(n))
a %>% filter(site == "ASSECret2210191MLSC") %>% count(site, lon, lat, trans)
a %>% filter(site == "ASSECret2210191MLSC") %>% filter(trans == 1)
# shit

crete_raw <- read_csv("~/MSc/Lab/crete2019/UVC_crete_2019.csv", col_types = cols(Notes = "c"))
colnames(crete_raw)
crete_raw %>%
  filter(SiteID == "ASSECret2210191MLSC") %>%
  distinct(SiteID, Transect, Mean_Depth)



# check there's no differences in columns before I merge them
cols_crete <- colnames(crete_full)
cols_uvc <- colnames(uvc_data)

setdiff(cols_crete, cols_uvc)

uvc_full <- bind_rows(uvc_data, crete_full)
uvc_full %>% distinct(country)
count(uvc_full)

summary(uvc_full)

# write_csv(uvc_full, "data/medata_nov_2019.csv")
