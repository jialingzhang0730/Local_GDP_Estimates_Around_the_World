# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file retrieves Philippines's regional GDP data and processes their corresponding geometries.
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

PHL_regional_rgdp <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/PHL/GRDP_Reg_2018PSNA_2000-2023.xlsx", skip = 9, n_max = 18)  %>% 
       slice(-1)  %>% # remove the first row, they are missing values
       dplyr::select(-1) %>% # remove the first column, we do not need that
       rename(admin_2_name = ...2)  %>% 
       pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
       mutate(year = as.numeric(year), admin_unit = 2) %>% 
       rename(rgdp_total = value) %>% 
       pivot_wider(names_from = admin_unit, values_from = c(matches("rgdp")),
                     names_glue = "admin_{admin_unit}_{.value}") %>% 
       group_by(year) %>% 
       mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total)) %>% 
       ungroup()  %>% 
       mutate(id = paste0(admin_2_name, "_PHL"), iso = "PHL", 
              min_admin_unit = 2, admin_1_name = "Philippines") %>% 
       dplyr::select(id, iso, year, min_admin_unit, starts_with("admin_2"), starts_with("admin_1"))

write.csv(PHL_regional_rgdp, "step2_obtain_gdp_data/temp/phl_gdp_clean.csv", row.names = F)

# ------------------------------------------------- #
# Create shapefiles -----

PHL_regional_sf <- read_sf("step1_obtain_gis_data/outputs/CGAZ_ADM1_without_large_waters.gpkg") %>%  
  rename(name = shapeName, iso = shapeGroup) %>%
  filter(iso == "PHL") %>%
  mutate(name = case_when(name == "ARMM" ~ "Bangsamoro Autonomous Region\r\nin Muslim Mindanao",
                          name == "NCR" ~ "National Capital Region",
                          name == "Calabarzon" ~ "CALABARZON",
                          name == "Mimaropa" ~ "MIMAROPA Region",
                          name == "Soccsksargen" ~ "SOCCSKSARGEN",
                          name == "CAR" ~ "Cordillera Administrative Region",
                          T ~ name))  %>% 
  filter(name %in% PHL_regional_rgdp$admin_2_name) %>% 
  mutate(id = paste0(name, "_", iso)) %>% 
  dplyr::select(id, iso, geom)

training_df <- PHL_regional_rgdp %>% 
  mutate(parent_admin_unit = 1) %>% 
  rename_with(starts_with("admin_1"), .fn = ~ gsub("admin_1", "parent", .x)) %>% 
  rename_with(starts_with("admin_2"), .fn = ~ gsub("admin_2", "unit", .x)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_name, matches("parent_rgdp"))

st_write(PHL_regional_sf, "step2_obtain_gdp_data/temp/phl_admin_2.gpkg", append = F)
write.csv(training_df, "step2_obtain_gdp_data/temp/phl_training_data.csv", row.names = F)

# eof ----