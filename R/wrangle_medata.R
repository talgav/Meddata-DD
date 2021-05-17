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


# Add local/exotic column -------------------------------------------------

# upload Hezi Buba's data to extract non indigenous information:
indie_raw <- read_csv("data/ab_data_med_hezibuba.csv") %>% 
  filter(!is.na(species)) %>% 
  select(species, les)

# Add missing
indie_data <- indie_raw %>% 
  mutate(les = case_when(species == "Abudefduf saxatilis" ~ 1,
                         species == "Balistes carolinensis" ~ 0,
                         species == "Epinephelus costea" ~ 0,
                         species == "pagellus erythrinus" ~ 0,
                         species == "Pagrus coeruleostictus" ~ 0,
                         species == "Pomatomus saltator" ~ 0,
                         species == "Rhinobatos cemiculus" ~ 0,
                         species == "Scomber Japonicus" ~ 1,
                         species == "Sillago sihama" ~ 1,
                         species == "Torepedo torpedo" ~ 0,
                         species == "Umbrina cirrosa" ~ 0,
                         TRUE ~ les),
         species = stringr::str_replace(.$species, pattern = "\\s", replacement = "\\."),
         species = str_to_title(species),
         species = case_when(species == "Epinephelus costea" ~ "Epinephelus costae",
                             species == "Pagrus coeruleostictus" ~ "Pagrus caeruleostictus",
                             species == "Pomatomus saltator" ~ "Pomatomus saltatrix",
                             species == "Rhinobatos cemiculus" ~ "Glaucostegus cemiculus",
                             TRUE ~ as.character(species))) %>% 
  transmute(species = species,
            exotic = as.logical(les))

# checking that the conversion went ok
indie_raw %>% 
  count(les)
indie_data %>% 
  count(exotic)

# Check species names match medata species names before joining:
species_in_medata_only <- setdiff(medata %>% distinct(species) %>% arrange() %>% pull(), 
        indie_data %>% distinct(species) %>% arrange() %>% pull())

# TODO: Add missing species (replace lines 135-175 with the following code (to be written):

medata <- medata %>% 
  left_join(indie_data, by = "species")

# Check for any species that are still 'exotic' == NA:
medata %>% 
  ungroup() %>%
  filter(is.na(exotic)) %>%
  distinct(species, exotic) %>% 
  arrange(species) %>% 
  print(n = Inf)

medata <- medata %>% 
  mutate(exotic = case_when(species == "Mugil.cephalus" |
                              species == "Labrus.merula" |
                              species == "Labrus.viridis" |
                              species == "Symphodus.melanocercus" |
                              species == "Gobius.cruentatus" |
                              species == "Parablennius.rouxi" |
                              species == "Tripterygion.tripteronotus" |
                              species == "Symphodus.ocellatus" |
                              species == "Tripterygion.delaisi" |
                              species == "Symphodus.rostratus"|
                              species == "Labrus.mixtus" |
                              species == "Gobius.bucchichi" |
                              species == "Symphodus.cinereus" |
                              species == "Parablennius.zvonimiri" |
                              species == "Tripterygion.melanurus" |
                              species == "Parablennius.sanguinolentus" |
                              species == "Oedalechilus.labeo" |
                              species == "Gobius.paganellus" |
                              species == "Parablennius.pilicornis" |
                              species == "Belone.belone" |
                              species == "Scartella.cristata" |
                              species == "Lichia.amia" |
                              species == "Gobius.xanthocephalus" |
                              species == "Pomatoschistus.quagga" |
                              species == "Parablennius.tentacularis" |
                              species == "Gobius.auratus" |
                              species == "Gobius.cobitis" |
                              species == "Gobius.geniporus" |
                              species == "Mullus.barbatus" |
                              species == "Epinephelus.caninus" |
                              species == "Trisopterus.minutus" |
                              species == "Gobius.vittatus" |
                              species == "Chelon.labrosus"
                            ~ FALSE,
                            species == "Cheilodipterus.novemstriatus" |
                              species == "Pterois.miles" |
                              species == "Parupeneus.forsskali" |
                              species == "Herklotsichthys.punctatus"
                            ~ TRUE,
                            TRUE ~ exotic))

# Add missing length-weight contnstants:
library("rfishbase")

missing_constants <- medata %>%
  ungroup() %>% 
  filter(is.na(a)) %>%
  distinct(species) %>% 
  mutate(species = str_replace(.$species, pattern = "\\.", "\\ "))

species_constants <- rfishbase::length_weight(missing_constants$species, fields = c("Species", "a", "b")) %>% 
  group_by(Species) %>% 
  summarise(mean_a = mean(a), mean_b = mean(b)) %>% 
  ungroup() %>% 
  rename(species = Species) %>% 
  mutate(species = str_replace(.$species, pattern = "\\ ", "\\."))

medata <- medata %>% 
  ungroup() %>% 
  left_join(species_constants, by = "species") %>%
  mutate(a = if_else(is.na(a), mean_a, a),
         b = if_else(is.na(b), mean_b, b)) %>% 
  select(-c("mean_a", "mean_b"))

# Check that the NAs (at least for most species) were completed:
medata %>%
  filter(is.na(a)) %>%
  distinct(species) %>% 
  print(n = Inf)

# # Check for fish_networks proj (which of left NAs are of my species?):
# groupers <- c("Epinephelus.costae", "Epinephelus.marginatus", "Serranus.cabrilla", "Serranus.scriba")
# diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
# herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa", "Sparisoma.cretense")
# 
# species_constants %>%
#   filter(is.na(mean_a)) %>%
#   filter(species %in% c(groupers, diplodus, herbivores))



# Save the new file -------------------------------------------------------

medata

# write_rds(medata, "data/medata.Rds")
# # copy to my project (personal laptop)
# file.copy("data/medata.Rds",
#           "C:/Users/shira/Documents/MSc/Paper/MedFishCooccurrence/data/medata.Rds",
#           overwrite = TRUE)


