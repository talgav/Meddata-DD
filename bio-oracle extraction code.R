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
all_bio_layers[2:3] # to view layers names and code

######################### temperature #########################

# Download temperature raster files from Bio ORACLE:
temperature <- load_layers(c("BO_sstmean", "BO_sstrange"), datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data") # download temperature data from bio ORACLE to the appointed directory

unique_coords <- raw_med %>% 
    dplyr::distinct(lon, lat) # set relevant coordinates: all the unique coordinates from raw_med data set.

temp_mean <- raster::extract(temperature[[1]], # raster layer (the 2 raters are stacked so here we choose the relavent one)
                     unique_coords[c("lon","lat")], # SPDF with centroids for buffer
                     buffer = 15000,  # buffer size, units depend on CRS (here it's degrees)
                     fun = mean, # what to value to extract
                     df = TRUE) # create data frame
temp_mean <- temp_mean[, "BO_sstmean"] # select only the temperature column

temp_range <- raster::extract(temperature[[2]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
temp_range <- temp_range[, "BO_sstrange"]

temp_data <- cbind.data.frame(unique_coords, tmean = temp_mean, trange = temp_range) # put together the 2 data sets and combine with the coordinates

# add data to raw_med file:
raw_med <- raw_med %>%
  left_join(temp_data, by = c("lon", "lat"))
# 
# head(raw_med)
# # write.csv(raw_med, "med_raw.csv", append = FALSE)

######################### salinity #########################

# Download salinity raster files from Bio ORACLE:
salinity <- load_layers("BO2_salinitymean_bdmin", datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data")

# unique_coords <- raw_med %>%
#   dplyr::distinct(lon, lat)

sal_mean <- raster::extract(salinity, unique_coords[c("lon","lat")], fun = mean, df = TRUE)
sal_mean <- sal_mean[, "BO2_salinitymean_bdmin"]

sal_data <- cbind.data.frame(unique_coords, sal_mean)

head(sal_data)

raw_med <- raw_med %>%
  left_join(sal_data, by = c("lon", "lat"))

# head(raw_med)
# write.csv(raw_med, "med_raw.csv", append = FALSE)

######################### primary production #########################

# Download pp raster files from Bio ORACLE:
productivity <- load_layers(c("BO2_ppmean_bdmin", "BO2_pprange_bdmin"), datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data")

## uncomment the next two lines when running without the temperature:
# unique_coords <- raw_med %>%
#   dplyr::distinct(lon, lat)

pp_mean <- raster::extract(productivity[[1]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
pp_mean <- pp_mean[, "BO2_ppmean_bdmin"]

pp_range <- raster::extract(productivity[[2]], unique_coords[c("lon","lat")], buffer = 15000, fun = mean, df = TRUE)
pp_range <- pp_range[, "BO2_pprange_bdmin"]

pp_data <- cbind.data.frame(unique_coords, pp_mean, pp_range)
head(pp_data)

raw_med <- raw_med %>%
  left_join(pp_data, by = c("lon", "lat"))

# head(raw_med)
# write.csv(raw_med, "med_raw.csv", append = FALSE)

######################### depth #########################

# Download bathymertry raster files from Bio ORACLE:
bathymetry <- load_layers("BO_bathymean", datadir = "C:/Users/Shira/Documents/MEData/Bio ORACLE data")

## uncomment the next two lines when running without the temperature:
# unique_coords <- raw_med %>%
#   dplyr::distinct(lon, lat)

bathy_mean <- raster::extract(bathymetry, unique_coords[c("lon","lat")], fun = mean, df = TRUE)
bathy_mean <- bathy_mean[, "BO_bathymean"]

bathy_data <- cbind.data.frame(unique_coords, bathy_mean)
head(bathy_data)

raw_med <- raw_med %>%
  left_join(bathy_data, by = c("lon", "lat"))

colnames(raw_med)

# head(raw_med)
write.csv(raw_med, "med_raw.csv")

