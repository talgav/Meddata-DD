
# Load package 
library(sdmpredictors)
library(tidyverse)
# library(leaflet) 

# load point data (I only need their lon and lat) - only unique coordinates:
unique_coords <- read_csv("~/MEData/medata/data/all_uvc_data_101119.csv",
                          col_types = cols(depth = col_double())) %>%
  dplyr::distinct(lon, lat)

# explore layers from Bio ORACLE
all_bio_layers <- list_layers(datasets = "Bio-ORACLE", marine = TRUE) %>%
  dplyr::select(layer_code, name) # to view layers names and code

# to search for a layer code by a specific string - change the 2nd arg in str_detect:
all_bio_layers %>% filter(stringr::str_detect(.$name, "salinity"))

######################### temperature #########################

# Download temperature raster files from Bio ORACLE:
temperature <- load_layers(c("BO_sstmean", "BO_sstrange"), datadir = "~/MEData/Bio ORACLE data") # download temperature data from bio ORACLE to the appointed directory

temp_mean <- raster::extract(temperature[[1]], # raster layer (the 2 raters are stacked so here we choose the relavent one)
                     unique_coords, # SPDF with coords as centroids for buffer
                     buffer = 15000,  # buffer size, units depend on CRS (here it's degrees)
                     fun = mean, # what to value to extract
                     df = TRUE) %>% # create data frame
              dplyr::select(BO_sstmean) # select only the temperature column

temp_range <- raster::extract(temperature[[2]], unique_coords, buffer = 15000, fun = mean, df = TRUE) %>% 
              dplyr::select(BO_sstrange)

######################### salinity #########################

# Download salinity raster files from Bio ORACLE:
salinity <- load_layers("BO2_salinitymean_ss", datadir = "~/MEData/Bio ORACLE data")
sal_mean <- raster::extract(salinity, unique_coords, fun = mean, df = TRUE) %>% 
            dplyr::select("BO2_salinitymean_ss")

######################### primary production #########################

# Download pp raster files from Bio ORACLE:
productivity <- load_layers(c("BO2_ppmean_ss", "BO2_pprange_ss"), datadir = "~/MEData/Bio ORACLE data")
pp_mean <- raster::extract(productivity[[1]], unique_coords, buffer = 15000, fun = mean, df = TRUE) %>% 
           dplyr::select("BO2_ppmean_ss")
pp_range <- raster::extract(productivity[[2]], unique_coords, buffer = 15000, fun = mean, df = TRUE) %>% 
            dplyr::select("BO2_pprange_ss")

# ######################### depth #########################
# 
# # Download bathymertry raster files from Bio ORACLE:
# bathymetry <- load_layers("BO_bathymean", datadir = "~/MEData/Bio ORACLE data")
# bathy_mean <- raster::extract(bathymetry, unique_coords, fun = mean, df = TRUE) %>% 
#   dplyr::select("BO_bathymean")


# combine all:
env_data <- cbind(unique_coords, temp_mean, temp_range, sal_mean, pp_mean, pp_range)
glimpse(env_data)
write_csv(env_data, "data/env_bio_oracle.csv")

all_uvc <- read_csv("~/MEData/medata/data/all_uvc_data_101119.csv",
                          col_types = cols(depth = col_double())) %>%
           dplyr::select(-c(tmean, trange, sal_mean, pp_mean, pp_range)) # remove old data IF NEEDED

all_uvc_env <- left_join(all_uvc, env_data, by = c("lon", "lat"))
colnames(all_uvc_env) <- c("data.origin", "country", "site", "lon", "lat", "trans", 
                           "species", "sp.length", "sp.n", "season", "protection", "enforcement",
                           "total.mpa.ha", "size.notake", "yr.creation", "age.reserve.yr", "depth", "a",
                           "b", "tmean", "trange", "sal_mean", "pp_mean", "pp_range")
glimpse(all_uvc_env)
write_csv(all_uvc_env, "~/MEData/medata/data/all_uvc_data_101119.csv", append = FALSE)
