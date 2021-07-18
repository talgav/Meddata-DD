library(magrittr)
library(tidyverse)

# -------------------------------------------------------------------------

med_raw <- read_csv("data/uvc_data_19022020.csv", col_types = cols(depth = "d"))
summary(med_raw)


# Correct seasons ---------------------------------------------------------

# Season exhibits 4 levels, 2 of which are the same (`Fall` and `Autumn`)

med_raw %>% distinct(season)

med_raw <- med_raw %>% mutate(season = if_else(season == "Fall", "Autumn", season))
as.factor(med_raw$season) %>% levels()


# Convert case ------------------------------------------------------------

med_raw <- med_raw %>% mutate(site = str_to_lower(site), season = str_to_lower(season))


# Remove whitespaces ------------------------------------------------------

# Species names
med_raw <- med_raw %>% mutate(species = str_trim(species, "both"))

# Sites (plus remove an unneeded `,` in one of the site names):
med_raw <- med_raw %>% mutate(site = str_replace_all(site, "[[:space:]]", "\\_"), site = str_remove_all(site, "\\,"))


# MPAs --------------------------------------------------------------------

# Change `protection` to logical
table(med_raw$protection) # just to make sure it works fine
med_raw <- med_raw %>% mutate(protection = if_else(protection == "NO", FALSE, TRUE))
table(med_raw$protection)

# Sites that are outside of MPAs (protection == FALSE) should have `total.mpa.ha` and `size.notake` set to NA
med_raw <- med_raw %>% mutate(total.mpa.ha = ifelse(protection == FALSE & total.mpa.ha == 0, NA, total.mpa.ha),
                              size.notake = ifelse(protection == FALSE & size.notake == 0, NA, size.notake))


# Depth -------------------------------------------------------------------
# Fix depth data from three transects in Crete, where there are 2 depth values:

# First find them:
med_raw %>%
  dplyr::select(site, lon, lat, trans, depth) %>%
  distinct() %>% 
  count(site,lon,lat,trans) %>%
  arrange(desc(n))

# ...Check them out...
med_raw %>%
  dplyr::select(site, lon, lat, trans, depth) %>%
  distinct() %>%
  filter(site == "assecret2210191mlsc_b")

# Then fix them
med_raw[which(med_raw$site == "assecret2210191mlsc_a"),]$depth %<>% mean()
med_raw[which(med_raw$site == "assecret2210191mlsc_b"),]$depth %<>% mean()
med_raw[which(med_raw$site == "assecret2210191mlsc_c"),]$depth %<>% mean()


# Redundancy --------------------------------------------------------------

# Minimise unnecessary rows so that multiple counts of the same species in a transect
# will recieve one row (observation) by summing up count of individual:
medata <- med_raw %>% 
  group_by_at(setdiff(names(.), "sp.n")) %>% 
  summarise(abundance = sum(sp.n), .groups = "drop") %>% 
  rename(sp.n = abundance)

# check that there is no data loss:
sum(med_raw$sp.n) == sum(medata$sp.n)




