rm(list = ls())
.rs.restartR()
gc()

# Load package 
library(sdmpredictors)
library(tidyverse)
# library(leaflet) file.copy("C:\\Users\\shira\\Documents\\MSc\\fish_social_network\\data\\medata.R", "C:\\Users\\shira\\Documents\\MSc\\medata\\data\\medata.R")

medata <- read_rds("data/medata.Rds")

# explore layers from Bio ORACLE
all_bio_layers <- list_layers(datasets = "Bio-ORACLE", marine = TRUE) %>%
  dplyr::select(layer_code, name) # to view layers names and code

# to search for a layer code by a specific string - change the 2nd arg in str_detect:
all_bio_layers %>% filter(stringr::str_detect(.$name, "temperature"))

# Download environmental layers -------------------------------------------

my.sites <- medata %>% dplyr::select(site, lon, lat)
environment.layers <- load_layers(layercodes = c("BO_sstmean", "BO_sstrange", # temperature
                                                 "BO2_salinitymean_ss",  # salinity
                                                 "BO2_ppmean_ss", "BO2_pprange_ss")) # productivity

# Extract environmental values for sites in medata ------------------------

my.sites.environment <- data.frame(site = my.sites$site,
                                   raster::extract(environment.layers, my.sites[,2:3]))
my.sites.environment

# Check for NAs
my.sites.environment %>% dplyr::filter(!complete.cases(.)) %>% 
  dplyr::distinct() %>% summarise(sites_with_NA = n())

# Merge with Medata -------------------------------------------------------
colnames(medata)

medata %<>%
  left_join(my.sites.environment, by = "site") %>% # join with new env. data
  select(c("data.origin", "country", "site", "trans", "lon", "lat", "season", # tidy up
           "species", "sp.length", "sp.n",
           "protection", "enforcement", "total.mpa.ha", "size.notake", "yr.creation", "age.reserve.yr",
           "a", "b", "depth",
           "tmean" = "BO_sstmean", "trange" = "BO_sstrange",
           "sal_mean" = "BO2_salinitymean_ss",
           "pp_mean" = "BO2_ppmean_ss", "pp_range" = "BO2_pprange_ss")) %>% 
  mutate_at(.vars = c("data.origin", "country", "site", "trans", "season", "species", "enforcement"),
            .funs = as_factor)

# Check for NAs per variables
medata %>% dplyr::filter(!complete.cases(sal_mean)) %>% dplyr::distinct()

## I'm not saving this yet, because I don't want to override NAs onto
## existing data in the original medata file, so for now I'll just export the salinity part
new_salinity <- medata %>% dplyr::select(site, trans, lat, lon, sal_mean)
# write_csv(new_salinity, "data/new_salinity_vals.csv")

# write_rds("data/medata.Rds")