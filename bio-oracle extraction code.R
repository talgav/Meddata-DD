# Packages required: sdmpredictors (The package also contains external datasets (MARSPEC, BioClim))

# Load package 
library(tidyverse)
library(sdmpredictors)
library(leaflet) 

# Download raster files from Bio ORACLE:
temperature <- load_layers(c("BO_sstmax", "BO_sstmean", "BO_sstmin"), datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data")
# med_ext <- raster::extent(-5.8299126019999221, 30.6333332060000885, 35.7853580580000425, 45.8222356830000876)
# temperature_med <- crop(temperature, med_ext)

# Extract environmental values from layers 
medata <- medata %>% 
  mutate(tm)
  data.frame(site = my.sites$Name , depth=extract(bathymetry,my.sites[,2:3]) , extract(environment.bottom,my.sites[,2:3]) ) 

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

temp_data <- cbind.data.frame(unique_coords, tmax = temp_max, tmin = temp_min, tmean = temp_mean) %>% 
  distinct_all()
# temp_data <- temp_data[order(temp_data$trans), ]
raw_med <- raw_med %>% 
  dplyr::select(-"tmax", -"tmin", -"tmean") %>% 
  left_join(temp_data, by = c("lon", "lat"))

head(raw_med)
# write.csv(raw_med, "med_raw.csv", append = FALSE)
