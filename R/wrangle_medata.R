library(magrittr)
library(tidyverse)

# -------------------------------------------------------------------------

med_raw <- read_csv("data/uvc_data_19022020.csv", col_types = cols(depth = "d"))
summary(med_raw)


# Correct seasons ---------------------------------------------------------

# Season exhibits 4 levels, 2 of which are the same (`Fall` and `Autumn`), so let's fix that:
med_raw %<>% mutate(season = if_else(season == "Fall", "Autumn", season))
as.factor(med_raw$season) %>% levels()
# Convert all data to lower case to make things easier (except `country` and `species`)
med_raw %<>% mutate(site = str_to_lower(site), season = str_to_lower(season))


# Remove whitespaces ------------------------------------------------------

# Species names
med_raw %<>% mutate(species = str_trim(species, "both"))

# Sites (plus remove an unneeded `,` in one of the site names):
med_raw %<>% mutate(site = str_replace_all(site, "[[:space:]]", "\\_"), site = str_remove_all(site, "\\,"))


# MPAs --------------------------------------------------------------------

# Change `protection` to logical
table(med_raw$protection) # just to make sure it works fine
med_raw %<>% mutate(protection = if_else(protection == "NO", FALSE, TRUE))
table(med_raw$protection)

# Sites that are outside of MPAs (protection == FALSE) should have `total.mpa.ha` and `size.notake` set to NA
med_raw %<>% mutate(total.mpa.ha = ifelse(protection == FALSE & total.mpa.ha == 0, NA, total.mpa.ha),
                    size.notake = ifelse(protection == FALSE & size.notake == 0, NA, size.notake))


# Depth -------------------------------------------------------------------
# Fix depth data from three transects in Crete, where there are 2 depth values:

# First find them:
med_raw %>%
  dplyr::select(site, lon, lat, trans, depth) %>%
  distinct() %>% 
  count(site,lon,lat,trans) %>%
  arrange(desc(n))

med_raw %>%
  dplyr::select(site, lon, lat, trans, depth) %>%
  distinct() %>%
  filter(site == "assecret2210191mlsc_b") %>%
  count(site, lon, lat, trans)

med_raw %>%
  dplyr::select(site, lon, lat, trans, depth) %>%
  distinct() %>%
  filter(site == "assecret2210191mlsc_b") %>%
  filter(trans == 1)

# Then fix them
med_raw[which(med_raw$site == "assecret2210191mlsc_a"),]$depth %<>% mean()
med_raw[which(med_raw$site == "assecret2210191mlsc_b"),]$depth %<>% mean()
med_raw[which(med_raw$site == "assecret2210191mlsc_c"),]$depth %<>% mean()


# Redundancy --------------------------------------------------------------

# Minimise unnecessary rows so that every observation of a species, of a size, in a location (site-trans) will recieve one row:
medata <- med_raw %>% 
  group_by(data.origin, country, site, trans, lon, lat, season, 
           protection, enforcement, total.mpa.ha, size.notake,
           yr.creation, age.reserve.yr, depth, tmean, 
           trange, sal_mean, pp_mean, pp_range,
           species, sp.length, a, b) %>% 
  summarise(abundance = sum(sp.n)) %>% 
  rename(sp.n = abundance)

# check that there is no data loss:
sum(med_raw$sp.n) == sum(medata$sp.n)


# Save the new file -------------------------------------------------------

# # create an Rdata file of the dataset after all changes:
# write_rds(medata, "data/medata.Rds")
# # copy to my project
# file.copy("data/medata.Rds",
#           "C:/Users/shira/Documents/MSc/fish_social_network/data/medata.Rds",
#           overwrite = TRUE)


# Not fixed yet -----------------------------------------------------------

# species without biomass coefficients
medata %>%
  filter(is.na(a)) %>%
  distinct(species)
# Some of the species I'm interested in are missing this data

# locations without salinity data:
medata %>% filter(is.na(sal_mean)) %>% distinct(site)
# Fix salinity
library(raster)
library(sf)
med_sal <- medata %>% filter(is.na(sal_mean)) # filter all the NAs of salinity
med_sf <- sf::st_as_sf(med_sal, coords = c("lon", "lat"), crs = 4326) # convert df to sf
med_sf %<>% sf::st_as_sf() %>% sf::st_transform(crs = 3857) # transform to a WGS84 in meters for buffer creation
med_buffer <- sf::st_buffer(x = med_sf, dist = 50000) %>% sf::st_transform(crs = 4326) # create a buffer and transform back to crs in lat-lon (ESPG:4326)
# sdmpredictors::load_layers(layercodes = "BO2_salinitymean_ss", datadir = "data/bio_oracle") # import salinity raster from bio-oracle
sal_rast <- raster("data/bio_oracle/BO2_salinitymean_ss_lonlat.tif") # load the salinity raster (crs 4326)
salinity_vals <- raster::extract(x = sal_rast, y = med_buffer, fun = mean, df = TRUE) # extract salinity values for each polygon buffer



