# Packages required: sdmpredictors (The package also contains external datasets (MARSPEC, BioClim))

# Load package 
library(tidyverse)
library(sdmpredictors)

# CITATION:
# Tyberghein L, Verbruggen H, Pauly K, Troupin C, Mineur F, De Clerck O (2012) Bio-ORACLE: A global environmental dataset for marine species distribution modelling. Global Ecology and Biogeography, 21, 272–281.
# 
# Assis, J., Tyberghein, L., Bosh, S., Verbruggen, H., Serrão, E. A., & De Clerck, O. (2017). Bio-ORACLE v2.0: Extending marine data layers for bioclimatic modelling. Global Ecology and Biogeography.

library(leaflet) 

raw_med <- read.csv("C:/Users/Shira/Documents/R/medata/med_raw.csv")

list_datasets() # which datasets Bio ORACLE contain?
all_bio_layers <- list_layers(datasets = "Bio-ORACLE") # explore layers from Bio ORACLE
all_bio_layers[2:3]

######################### temperature #########################

# Download temperature raster files from Bio ORACLE:
temperature <- load_layers(c("BO_sstmax", "BO_sstmean", "BO_sstmin"), datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data")

unique_coords <- raw_med %>% 
    dplyr::distinct(lon, lat)

temp_mean <- raster::extract(temperature[[2]], # raster layer
                     unique_coords[c("lon","lat")], # SPDF with centroids for buffer
                     buffer = 15000,  # buffer size, units depend on CRS
                     fun = mean, # what to value to extract
                     df = TRUE)
temp_mean <- temp_mean[, "BO_sstmean"]

temp_max <- raster::extract(temperature[[1]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
temp_max <- temp_max[, "BO_sstmax"]

temp_min <- raster::extract(temperature[[3]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
temp_min <- temp_min[, "BO_sstmin"]

temp_range <- temp_max - temp_min

temp_data <- cbind.data.frame(unique_coords, tmax = temp_max, tmin = temp_min, tmean = temp_mean) %>% 
  distinct_all()

# # add data to raw_med file:
# raw_med <- raw_med %>% 
#   dplyr::select(-"tmax", -"tmin", -"tmean") %>% 
#   left_join(temp_data, by = c("lon", "lat"))
# 
# head(raw_med)
# # write.csv(raw_med, "med_raw.csv", append = FALSE)


######################### chlorophyll #########################

# Download chl raster files from Bio ORACLE:
chlorophyll <- load_layers(c("BO_chlorange", "BO_chlomean"), datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data")

# unique_coords <- raw_med %>%
#   dplyr::distinct(lon, lat)

chl_range <- raster::extract(chlorophyll[[1]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
chl_range <- chl_range[, "BO_chlorange"]

chl_mean <- raster::extract(chlorophyll[[2]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
chl_mean <- chl_mean[, "BO_chlomean"]

chl_data <- cbind.data.frame(unique_coords, chl_mean, chl_range)

head(chl_data)

raw_med <- raw_med %>% 
  left_join(chl_data, by = c("lon", "lat"))

head(raw_med)
# write.csv(raw_med, "med_raw.csv", append = FALSE)

######################### chlorophyll #########################

# Download chl raster files from Bio ORACLE:
salinity <- load_layers("BO_salinity", datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data")

# unique_coords <- raw_med %>%
#   dplyr::distinct(lon, lat)

sal_mean <- raster::extract(salinity, unique_coords[c("lon","lat")], fun = mean, df = TRUE)
sal_mean <- sal_mean[, "BO_salinity"]

sal_data <- cbind.data.frame(unique_coords, sal_mean)

head(sal_data)

raw_med <- raw_med %>% 
  left_join(sal_data, by = c("lon", "lat"))

head(raw_med)
# write.csv(raw_med, "med_raw.csv", append = FALSE)
