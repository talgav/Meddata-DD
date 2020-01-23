library(todor) # not required

# This is a code to modify data from Crete underwater census surveys into the format of all UVC surveys

library(tidyverse)

uvc_colnames <- c("data.origin", "country", "site", "lon", "lat", "trans", "species",
                  "sp.length", "sp.n", "season", "protection", "enforcement", "total.mpa.ha",
                  "size.notake", "yr.creation", "age.reserve.yr", "depth") # missing a, b ,temp, pp, sal

crete_data <- read_csv("~/MSc/Lab/crete2019/UVC_crete_2019.csv",
                       col_types = cols(Notes = "c"))
# correct mistakes:
crete_data$Species <- str_replace(crete_data$Species, "ThalASSoma pavo", "Thalassoma pavo")
crete_data$Species <- str_replace(crete_data$Species, "Epinephelus costea", "Epinephelus costae")
crete_data$Species <- str_replace(crete_data$Species, "Atherina sp.", "Atherina spp")
crete_data$Species <- str_replace(crete_data$Species, "Gobius panellus", "Gobius paganellus")
crete_data$Species <- str_replace(crete_data$Species, "Gobiidae spp", "Gobiidae")
crete_data$Species <- str_replace(crete_data$Species, "Belonidae spp.", "Belonidae")
crete_data$Species <- str_replace(crete_data$Species, "Pagrus coeruleostrictus", "Pagrus coeruleostictus")

str(crete_data)

crete_data <- crete_data %>%
  filter(Distance <= 2.5) %>% # limit belt transect to 2.5 m distance
  filter(Confidence == 1) %>% # only species that we're sure about their ID
  filter(Date != 2019-10-12 & Observer_Name != "Shira Salingre",
         Date != 2019-10-13 & Observer_Name != "Shira Salingre") # remove inexperienced observer from 2 1st days of diving
# ---------
# Check there are no duplicates with depths (2 depths for the same transect)
crete_data %>%
  select(lon, lat, SiteID, Transect, Mean_Depth) %>%
  distinct() %>%
  count(lon, lat, SiteID, Transect) %>%
  arrange(desc(n))

# There are 2 depths for transect ASSECret2210191MLSC_A:
# when I check it in the raw data (google sheet) I find that there are 2 transects with the same name
crete_data %>%
  select(lon, lat, SiteID, Transect, Mean_Depth) %>%
  filter(SiteID == "ASSECret2210191MLSC") %>%
  distinct()

# change the transect ID
crete_data[crete_data$SiteID == "ASSECret2210191MLSC" & crete_data$`Time Start` > hms::hms(hours = 16),]$SiteID <- "ASSECret2210192MLSC"



# ---------

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
                              site = TransID) %>%
                       select(data.origin, country, site, lon, lat, trans, Species, Length, Amount,
                             Season, protection, enforcement, total.mpa.ha, size.notake, yr.creation,
                             age.reserve.yr, Mean_Depth)
# 3. match colnames to the uvc colnames (missing: a, b, temp, pp, sal)
colnames(crete_mod) <- c(uvc_colnames)
crete_mod$species <- gsub("\\s", "\\.", x = crete_mod$species) # replace space with '.' in species names

# Check there are no duplicates with depths (2 depths for the same transect)
crete_mod %>%
  select(site, lon, lat, trans, depth) %>%
  distinct() %>%
  count(site,lon,lat,trans) %>%
  arrange(desc(n))

# check it out
glimpse(crete_mod)

# write_csv(crete_mod, "data/crete_uvc.csv")

# add to medata:
medata <- read_csv("data/uvc_data_200120.csv")
colnames(medata)

# check for new species:
crete_spp <- unique(crete_mod$species)
med_spp <- unique(medata$species)
base::setdiff(crete_spp, med_spp)

# add biomass coefficients:
ab <- read_csv("~/MEData/Lenght_weight.csv")
crete_full <- crete_mod %>% dplyr::left_join(ab)

# add environmental variables (explanations in `bio-oracle extraction code.R`)
library(sdmpredictors)
unique_coords <- crete_mod %>% 
  dplyr::distinct(lon, lat)
temperature <- load_layers(c("BO_sstmean", "BO_sstrange"), datadir = "~/MEData/Bio ORACLE data")
salinity <- load_layers("BO2_salinitymean_bdmin", datadir = "~/MEData/Bio ORACLE data")
productivity <- load_layers(c("BO2_ppmean_bdmin", "BO2_pprange_bdmin"), datadir = "~/MEData/Bio ORACLE data")
temp_mean <- raster::extract(temperature[[1]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
temp_mean <- temp_mean[, "BO_sstmean"]
temp_range <- raster::extract(temperature[[2]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
temp_range <- temp_range[, "BO_sstrange"]
sal_mean <- raster::extract(salinity, unique_coords[c("lon","lat")], fun = mean, df = TRUE)
sal_mean <- sal_mean[, "BO2_salinitymean_bdmin"]
pp_mean <- raster::extract(productivity[[1]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
pp_mean <- pp_mean[, "BO2_ppmean_bdmin"]
pp_range <- raster::extract(productivity[[2]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
pp_range <- pp_range[, "BO2_pprange_bdmin"]
env_data <- cbind.data.frame(unique_coords, tmean = temp_mean, trange = temp_range, sal_mean, pp_mean, pp_range) # put together all enviromental data with coordiantes

# add data to crete_full file:
crete_full <- crete_mod %>%
  left_join(env_data, by = c("lon", "lat"))

View(crete_full)
# write_csv(crete_full, "data/crete_uvc_full.csv")

# FIXME salinity not filled for some reason?

# add crete data to all uvc:
all <- rbind(medata, crete_full)
View(all)
# write_csv(all, "data/all_uvc_data_101119.csv")
