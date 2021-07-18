library(magrittr)
library(tidyverse)
library(rfishbase)

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


# write_rds(med_raw, "data/med_raw.Rds")


# Add species information -------------------------------------------------

medata <- read_rds("data/med_raw.Rds")

species_info <- medata %>% 
  distinct(species) %>% 
  mutate(species = str_replace(species, "\\.", "\\ ")) %>% 
  filter(!str_detect(species, "dae")) %>% 
  filter(!str_detect(species, "spp"))

species_info %>% arrange(species) %>% print(n = Inf)


# Add diet information ----------------------------------------------------

species_diet <- ecology(species_info$species, 
                        fields = c("Species", "DietTroph", "DietSeTroph", "FoodTroph", "FoodSeTroph"))

# Which metric (DietTroph/FoodTroph) has more NAs?
sapply(species_diet, function(x) sum(is.na(x)))

species_diet %>% 
  filter(is.na(FoodTroph))

# Species with missing FoodTroph also have missing DietTroph (except Parablennius zvonimiri)
# so we'll keep 'FoodTroph'
species_troph <- species_diet %>% 
  mutate(species = str_replace(Species, "\\ ", "\\.")) %>% 
  select(species, FoodTroph, FoodSeTroph)

species_troph

# Join
medata <- medata %>% left_join(species_troph)

# Lessepsian migrants -----------------------------------------------------

indie_species <- read_csv("data/exotic_species.csv")

medata <- medata %>% left_join(indie_species)

# write_rds(medata, "data/medata.rds")

# Add Length-Weight ratio constants ---------------------------------------

length_weight(species_info$species) %>% colnames()

missing_constants <- medata %>%
  filter(is.na(a)) %>%
  filter(is.na(b)) %>% 
  distinct(species) %>% 
  mutate(species = str_replace(.$species, pattern = "\\.", "\\ "))

species_LW <- length_weight(missing_constants$species, 
                            fields = c("Species", "a", "b", "Type", "Method")) %>% 
  filter(Method == "type I linear regression" & Type == "TL" | Method == "Type I linear regression" & Type == "TL") %>% 
  group_by(Species) %>% summarise(mean_a = mean(a), mean_b = mean(b)) %>% ungroup() %>% 
  mutate(species = str_replace(.$Species, pattern = "\\ ", "\\.")) %>% 
  select(-Species)

# Add to medata

medata <- medata %>% 
  left_join(species_LW) %>% 
  mutate(a = if_else(is.na(a), mean_a, a),
         b = if_else(is.na(b), mean_b, b)) %>% 
  select(-mean_a, -mean_b)


# Add family --------------------------------------------------------------

species_family <- rfishbase::load_taxa() %>% 
  as_tibble() %>% 
  filter(Species %in% species_info$species) %>%
  select(Species, Family) %>% 
  mutate(species = str_replace(.$Species, pattern = "\\ ", "\\.")) %>% 
  select(species, family = Family)

medata <- medata %>% left_join(species_family)

# Manually add families where applicable:
medata %>% filter(is.na(family)) %>% distinct(species) %>% arrange(species)

medata <- medata %>% mutate(family = case_when(str_detect(species, "Symphodus") ~ "Labridae",
                                     species == "Atherina.spp" ~ "Atherinidae",
                                     species == "Belonidae" ~ "Belonidae",
                                     species == "Clupeidae" ~ "Clupeidae",
                                     species == "Labridae" ~ "Labridae",
                                     species == "Liza.aurata" ~ "Mugilidae",
                                     species == "Mullus.barbatus" ~ "Mullidae",
                                     str_detect(species, "Tripterygion") ~ "Tripterygiidae",
                                     TRUE ~ as.character(family)))

# Save --------------------------------------------------------------------

skimr::skim(medata)
summary(medata)

medata <- medata %>% select(data.origin, country, season, lon, lat, site, trans, 
                  protection, enforcement, total.mpa.ha, size.notake, yr.creation, age.reserve.yr,
                  depth, tmean, trange, sal_mean, pp_mean, pp_range, 
                  species, sp.n, sp.length, a, b, family, exotic, FoodTroph, FoodSeTroph)

write_rds(medata, "data/medata.Rds")






