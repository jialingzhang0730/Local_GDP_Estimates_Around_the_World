# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file retrieves China's provincial GDP data and processes their corresponding geometries.
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

CHN_regional_rgdp <- read_xls("step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/AnnualbyProvince.xls", skip = 3, n_max = 31)  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_to = "year")  %>% 
  mutate(year = as.numeric(year), admin_unit = 2)  %>% 
  rename(rgdp_total = value)  %>% 
  arrange(year)  %>% 
  pivot_wider(names_from = admin_unit, values_from = c(matches("rgdp")),
              names_glue = "admin_{admin_unit}_{.value}")  %>% 
  mutate(admin_2_rgdp_total = as.numeric(admin_2_rgdp_total))  %>%               
  group_by(year) %>% 
  mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total))  %>% 
  ungroup()  %>% 
  mutate(id = paste0(Region, "_CHN"), iso = "CHN", 
         min_admin_unit = 2, admin_1_name = "China") %>% 
  rename(admin_2_name = Region) %>% 
  dplyr::select(id, iso, year, min_admin_unit, starts_with("admin_2"), starts_with("admin_1"))

write.csv(CHN_regional_rgdp, "step2_obtain_gdp_data/temp/chn_gdp_clean.csv", row.names = F)

# ------------------------------------------------- #
# Create shapefiles -----

# obtain the province geometry boundary
CHN_regional_sf <- read_sf("step1_obtain_gis_data/outputs/gdam_prov_level1_without_largewater.gpkg")  %>% 
  filter(GID_0 == "CHN") %>%
  rename(name = NAME_1, iso = GID_0) %>%
  filter(!name %in% c("Hong Kong", "Macau")) %>% # separate those two out as national GDP data do not contain those two regions
  mutate(name = case_when(name == "Nei Mongol" ~ "Inner Mongolia",
                          name == "Ningxia Hui" ~ "Ningxia",
                          name == "Xinjiang Uygur" ~ "Xinjiang",
                          name == "Xizang" ~ "Tibet",
                          T ~ name)) %>% 
  filter(name %in% CHN_regional_rgdp$admin_2_name) %>% 
  mutate(id = paste0(name, "_", iso)) %>% 
  dplyr::select(id, iso, geom)  

# obtain the national boundary of China
CHN_national_sf <- CHN_regional_sf %>%
  mutate(name = "China") %>% 
  group_by(name, iso) %>% 
  summarize(geom = st_union(geom),
            admin_unit = 1, .groups = "drop")

training_df <- CHN_regional_rgdp %>% 
  mutate(parent_admin_unit = 1) %>% 
  rename_with(starts_with("admin_1"), .fn = ~ gsub("admin_1", "parent", .x)) %>% 
  rename_with(starts_with("admin_2"), .fn = ~ gsub("admin_2", "unit", .x)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_name, matches("parent_rgdp")) 

st_write(CHN_regional_sf, "step2_obtain_gdp_data/temp/chn_admin_2.gpkg", append = F)
write.csv(training_df, "step2_obtain_gdp_data/temp/chn_training_data.csv", row.names = F)

# eof ----