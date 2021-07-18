library(tidyverse)
library(rfishbase)

medata <- read_rds("data/medata.Rds")


species_info <- medata %>% 
  distinct(species) %>% 
  mutate(species = str_replace(species, "\\.", "\\ ")) %>% 
  filter(!str_detect(species, "dae")) %>% 
  filter(!str_detect(species, "spp"))

species_info %>% arrange(species) %>% print(n = Inf)


# Add diet information ----------------------------------------------------

species_diet <- ecology(species_info$species, 
                        fields = c("Species", "DietTroph", "DietSeTroph", "FoodTroph", "FoodSeTroph"))

species_diet %>% filter(!is.na(DietTroph), !is.na(FoodTroph)) %>% 
  group_by(Species) %>% summarise(mean_DietTroph = mean(DietTroph), mean_FoodTroph = mean(FoodTroph)) %>% ungroup()


# Lessepsian migrants -----------------------------------------------------

# upload Hezi Buba's data to extract non indigenous information:
indie_raw <- read_csv("data/ab_data_med_hezibuba.csv") %>% 
  filter(!is.na(species)) %>% 
  select(species, les)

# Add missing and fix names
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
                         TRUE ~ les)) %>% 
  mutate(species = case_when(species == "Epinephelus costea" ~ "Epinephelus costae",
                             species == "Pagrus coeruleostictus" ~ "Pagrus caeruleostictus",
                             species == "Pomatomus saltator" ~ "Pomatomus saltatrix",
                             species == "Rhinobatos cemiculus" ~ "Glaucostegus cemiculus",
                             TRUE ~ as.character(species))) %>% 
  mutate(species = stringr::str_replace(.$species, pattern = "\\s", replacement = "\\."),
         species = str_to_title(species)) %>% 
  transmute(species = species,
            exotic = as.logical(les))

# checking that the conversion went ok
indie_raw %>% 
  count(les)
indie_data %>% 
  count(exotic)

medata <- medata %>% 
  left_join(indie_data, by = "species")

# Check for any species that are still 'exotic' == NA:
medata %>% 
  filter(is.na(exotic)) %>%
  distinct(species, exotic) %>% 
  arrange(species) %>% 
  print(n = Inf)

medata <- medata %>% 
  mutate(exotic = case_when(species == "Belone.belone" |
                              species == "Chelon.labrosus"|
                              species == "Epinephelus.caninus" |
                              species == "Gobius.auratus" |
                              species == "Gobius.bucchichi" |
                              species == "Gobius.cobitis" |
                              species == "Gobius.cruentatus" |
                              species == "Gobius.geniporus" |
                              species == "Gobius.paganellus" |
                              species == "Gobius.vittatus" |
                              species == "Gobius.xanthocephalus" |
                              species == "Labrus.merula" |
                              species == "Labrus.mixtus" |
                              species == "Labrus.viridis" |
                              species == "Lichia.amia" |
                              species == "Mugil.cephalus" |
                              species == "Mullus.barbatus" |
                              species == "Oedalechilus.labeo" |
                              species == "Parablennius.pilicornis" |
                              species == "Parablennius.rouxi" |
                              species == "Parablennius.sanguinolentus" |
                              species == "Parablennius.tentacularis" |
                              species == "Parablennius.zvonimiri" |
                              species == "Pomatoschistus.quagga" |
                              species == "Scartella.cristata" |
                              species == "Symphodus.cinereus" |
                              species == "Symphodus.melanocercus" |
                              species == "Symphodus.ocellatus" |
                              species == "Symphodus.rostratus"|
                              species == "Tripterygion.delaisi" |
                              species == "Tripterygion.melanurus" |
                              species == "Tripterygion.tripteronotus" |
                              species == "Trisopterus.minutus"
                            ~ FALSE,
                            species == "Cheilodipterus.novemstriatus" |
                              species == "Herklotsichthys.punctatus" |
                              species == "Parupeneus.forsskali" |
                              species == "Pterois.miles"
                            ~ TRUE,
                            TRUE ~ exotic))

# # Export list of exotic/non-exotic species
# medata %>% distinct(species, exotic) %>% write_csv("data/exotic_species.csv")


# Add Length-Weight ratio constants ---------------------------------------

length_weight(species_info$species) %>% colnames()

species_LW <- length_weight(species_info$species, 
                            fields = c("Species", "a", "b", "Type", "Method"))

species_LW <- species_LW %>% 
  filter(Method == "type I linear regression") %>% 
  filter(Type == "TL") %>% 
  group_by(Species) %>% summarise(mean_a = mean(a), mean_b = mean(b)) %>% ungroup()

missing_constants <- medata %>%
  filter(is.na(a)) %>%
  distinct(species) %>% 
  mutate(species = str_replace(.$species, pattern = "\\.", "\\ "))

length_weight(missing_constants$species, 
                fields = c("Species", "a", "b", "Type", "Method")) %>% print(n = Inf)
# All are NA, cannot complete missing ratios


# Combine and save the new file -------------------------------------------

medata <- medata %>% 
  left_join(species_diet %>% mutate(species = str_replace(.$Species, pattern = "\\.", "\\ "))) %>% 
  select(-Species)

write_rds(medata, "data/medata.Rds")


