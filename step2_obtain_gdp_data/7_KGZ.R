# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file retrieves Kyrgyzstan's regional GDP data and processes their corresponding geometries.
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(units)
library(sf)

# ------------------------------------------------- #
# Obtain GDP data: 

KGZ_regional_rgdp <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/KGZ/1010009 Валовой региональный продукт (ВРП) в текущих ценах..xlsx", skip = 3, n_max = 13)  %>% 
  dplyr::select(-c("Көрсөткүчтөрдүн аталыштары", "Наименование показателей")) %>% # exclude columns about each region's non-english names 
  slice(-1:-3) %>% # we do not need the first three rows: national GDP and two blank rows
  pivot_longer(cols = matches("\\d+"), names_to = "year")  %>% 
  mutate(year = as.numeric(year), admin_unit = 2) %>% 
  rename(rgdp_total = value) %>% 
  pivot_wider(names_from = admin_unit, values_from = c(matches("rgdp")),
              names_glue = "admin_{admin_unit}_{.value}") %>% 
  group_by(year) %>% 
  mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total)) %>% 
  ungroup()  %>% 
  mutate(id = paste0(Items, "_KGZ"), iso = "KGZ", 
         min_admin_unit = 2, admin_1_name = "Kyrgyzstan") %>% 
  rename(admin_2_name = Items) %>% 
  dplyr::select(id, iso, year, min_admin_unit, starts_with("admin_2"), starts_with("admin_1"))

write.csv(KGZ_regional_rgdp, "step2_obtain_gdp_data/temp/kgz_gdp_clean.csv", row.names = F)

# ------------------------------------------------- #
# Create shapefiles -----

KGZ_regional_sf <- read_sf("step1_obtain_gis_data/outputs/gdam_prov_level1_without_largewater.gpkg")  %>% 
  filter(GID_0 == "KGZ") %>%
  rename(name = NAME_1, iso = GID_0)  %>% 
  mutate(name = case_when(name == "Batken" ~ "Batken oblast",
                          name == "Biškek" ~ "Bishkek сity",
                          name == "Chüy" ~ "Chui oblast",
                          name == "Jalal-Abad" ~ "Jalal-Abat oblast",
                          name == "Naryn" ~ "Naryn oblast",
                          name == "Osh" ~ "Osh oblast",
                          name == "Osh (city)" ~ "Osh сity",
                          name == "Talas" ~ "Talas oblast",
                          name == "Ysyk-Köl" ~ "Yssyk-Kul oblast",
                          T ~ name))  %>% 
  filter(name %in% KGZ_regional_rgdp$admin_2_name) %>% 
  mutate(id = paste0(name, "_", iso)) %>% 
  dplyr::select(id, iso, geom) 

KGZ_national_sf <- KGZ_regional_sf %>% 
  mutate(name = "Kyrgyzstan") %>% 
  group_by(name, iso) %>% 
  summarize(geom = st_union(geom), admin_unit = 1, .groups = "drop")

training_df <- KGZ_regional_rgdp %>% 
  mutate(parent_admin_unit = 1) %>% 
  rename_with(starts_with("admin_1"), .fn = ~ gsub("admin_1", "parent", .x)) %>% 
  rename_with(starts_with("admin_2"), .fn = ~ gsub("admin_2", "unit", .x)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_name, matches("parent_rgdp"))

st_write(KGZ_regional_sf, "step2_obtain_gdp_data/temp/kgz_admin_2.gpkg", append = F)
write.csv(training_df, "step2_obtain_gdp_data/temp/kgz_training_data.csv", row.names = F)


# eof ----