# This is a code to modify data from Crete underwater census surveys into the format of all UVC surveys

library(tidyverse)

uvc_colnames <- c("data.origin", "country", "site", "lon", "lat", "trans", "species",
                  "sp.length", "sp.n", "season", "protection", "enforcement", "total.mpa.ha",
                  "size.notake", "yr.creation", "age.reserve.yr", "depth") # missing a, b ,temp, pp, sal

crete_data <- read_csv("~/Lab stuff/Crete/crete2019/UVC_crete_2019.csv",
                       col_types = cols(Notes = "c"))
colnames(crete_data)

crete_data <- crete_data %>%
  filter(Distance <= 2.5) %>% # limit belt transect to 2.5 m distance
  filter(Date != 2019-10-12 & Observer_Name != "Shira Salingre",
         Date != 2019-10-13 & Observer_Name != "Shira Salingre") # remove inexperienced observer from 2 1st days of diving

# choose one observer at random
crete <- data.frame()
for (i in unique(crete_data$TransID)) {
  trans <- crete_data %>% filter(crete_data$TransID == i) # select a TransID from crete_data
  observer <- unique(trans$Observer_Name) # create a variable of the observers of this TransID
  random_observer <- sample(observer, 1) # choose 1 observer, randomly, from this TransID
  trans <- trans %>% filter(Observer_Name == random_observer) # select that random observer from the TransID
  crete <- rbind.data.frame(crete, trans) # add this TransID to a df called 'crete's
  }

# match crete data to uvc format
# 1. change transect names from letters to numbers
crete$trans <- ifelse(crete$Transect == "A", 1,
                          ifelse(crete$Transect == "B", 2,
                                 ifelse(crete$Transect == "C", 3, 4)))
# 2. clean and organise: remove unnecessary rows and cols, create missing ones and modify as needed
crete_mod <- crete %>% mutate(data.origin = "Belmaker", country = "Greece", # add columns of UVC format
                              protection = "NO", enforcement = 0, total.mpa.ha	= 0,
                              size.notake = 0, yr.creation	= NA, age.reserve.yr = NA,
                              site = str_extract(SiteID, "Cret\\d\\d")) %>%   # site name is 'Cret' followed by day of dive date
                      select(data.origin, country, site, lon, lat, trans, Species, Length, Amount,
                             Season, protection, enforcement, total.mpa.ha, size.notake, yr.creation,
                             age.reserve.yr, Mean_Depth)
# 3. match colnames to the uvc colnames (missing: a, b, temp, pp, sal)
colnames(crete_mod) <- c(uvc_colnames)

# check it out
glimpse(crete_mod)

# write_csv(crete_mod, "data/crete_uvc.csv")
