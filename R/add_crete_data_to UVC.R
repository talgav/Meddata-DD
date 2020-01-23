
# Load mediterranean data
medata <- read_csv("data/all_uvc_data_101119.csv", na = c("", "NA"), col_types = cols(depth = "d"))
count(medata)

# remove the old, irrelevant, crete data
uvc_data <- read_csv("data/all_uvc_data_101119.csv", na = c("", "NA"), col_types = cols(depth = "d")) %>%
  filter(!str_detect(.$site, "Cret"))

count(uvc_data)

a <- uvc_data %>% select(site, lon, lat, trans, depth) %>% 
  distinct()

a %>% count(site,lon,lat,trans) %>% arrange(desc(n))

crete <- read_csv("data/crete_uvc.csv")

# check there's no differences in columns before I merge them
cols_crete <- colnames(crete)
cols_uvc <- colnames(uvc_data)

setdiff(cols_crete, cols_uvc)

uvc_full <- bind_rows(uvc_data, crete)
uvc_full %>% distinct(country)
count(uvc_full)
