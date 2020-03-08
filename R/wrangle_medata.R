library(magrittr)
library(tidyverse)

med_raw <- read_csv("data/uvc_data_19022020.csv", col_types = cols(depth = "d"))
summary(med_raw)

# Season exhibits 4 levels, 2 of which are the same (`Fall` and `Autumn`), so let's fix that:
med_raw %<>% mutate(season = if_else(season == "Fall", "Autumn", season))
as.factor(med_raw$season) %>% levels()

# Convert all data to lower case to make things easier (except `country` and `species`)
med_raw %<>% mutate(site = str_to_lower(site), season = str_to_lower(season))

# Make sure there's not white space in the species names
med_raw %<>% mutate(species = str_trim(species, "both"))

# Remove space from the sites to make it easier to work with (plus remove an unneeded `,` in one of the site names):
med_raw %<>% mutate(site = str_replace_all(site, "[[:space:]]", "\\_"), site = str_remove_all(site, "\\,"))

# Change `protection` to logical
table(med_raw$protection) # just to make sure it works fine
med_raw %<>% mutate(protection = if_else(protection == "NO", FALSE, TRUE))
table(med_raw$protection)

# Sites that are outside of MPAs (protection == FALSE) should have `total.mpa.ha` and `size.notake` set to NA
med_raw %<>% mutate(total.mpa.ha = ifelse(protection == FALSE & total.mpa.ha == 0, NA, total.mpa.ha),
                    size.notake = ifelse(protection == FALSE & size.notake == 0, NA, size.notake))

# create an Rdata file of the dataset after all changes:
write_rds(med_raw, "data/medata.Rds")


# FIXME:
# (1) sp.n missing
med_raw %>% filter(sp.length == is.na(sp.length)) %>% distinct(site)
# (2) species without biomass coefficients
med_raw %>% filter(is.na(a)) %>% distinct(species) # Some of the species I'm interested in are missing this data
# (3) locations without salinity data:
med_raw %>% filter(is.na(sal_mean)) %>% distinct(site)
