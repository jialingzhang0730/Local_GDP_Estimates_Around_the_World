# --------------------------------- Task Summary --------------------------------- #
# This file retrieves GDP data for India's provinces for all available years
# and processes the corresponding geometries.
#
# The GDP data for previous years have also been reprocessed, as the official source 
# from which we retrieve the data has been updated.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(units)
library(sf)

# ------------------------------------------------- #
# Obtain GDP data: 

# Only read the last two sheets, which contain data for the years 2012 to 2022.
IND_regional_rgdp <- map_dfr(.x = c("T_28(iii)", "T_28(iv)"), .f = function(sheet){
  
  df_out <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/IND/T28_09122024E699603AE68F445FB6E485839CCB697B.XLSX", skip = 5, n_max = 33, sheet = sheet) %>%
    dplyr::select(-any_of("2023-24")) %>% 
    pivot_longer(cols = matches("\\d{4}"), names_to = "year")
  
  return(df_out)
  
})  %>% 
  rename(admin_2_name = ...1,
         rgdp_total = value)  %>% 
  mutate(year = as.numeric(substr(year,1,4)), 
         admin_unit = 2)  %>% 
  mutate(admin_2_name = ifelse(admin_2_name == "Jammu & Kashmir*", "Jammu & Kashmir", admin_2_name)) %>%
  filter(year %in% c(2012:2022))  %>%
  mutate(rgdp_total = as.numeric(rgdp_total)) %>% 
  pivot_wider(names_from = admin_unit, values_from = c(matches("rgdp")),
              names_glue = "admin_{admin_unit}_{.value}")  %>% 
  mutate(admin_2_rgdp_total = as.numeric(admin_2_rgdp_total))  %>%               
  group_by(year) %>% 
  mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total))  %>% 
  ungroup()  %>% 
  mutate(id = paste0(admin_2_name, "_IND"), iso = "IND", 
         min_admin_unit = 2, admin_1_name = "India") %>% 
  dplyr::select(id, iso, year, min_admin_unit, starts_with("admin_2"), starts_with("admin_1"))

write.csv(IND_regional_rgdp, "step2_obtain_gdp_data/temp/ind_gdp_clean.csv", row.names = F)

# ------------------------------------------------- #
# Create training data

training_df <- IND_regional_rgdp %>% 
  mutate(parent_admin_unit = 1) %>% 
  rename_with(starts_with("admin_1"), .fn = ~ gsub("admin_1", "parent", .x)) %>% 
  rename_with(starts_with("admin_2"), .fn = ~ gsub("admin_2", "unit", .x)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_name, matches("parent_rgdp")) 

write.csv(training_df, "step2_obtain_gdp_data/temp/ind_training_data.csv", row.names = F)

# ------------------------------------------------- #
# Create shapefiles

IND_regional_sf <- read_sf("step1_obtain_gis_data/outputs/gdam_prov_level1_without_largewater.gpkg")  %>% 
  filter(GID_0 == "IND") %>%
  rename(name = NAME_1, iso = GID_0) %>%
  mutate(name = case_when(name == "Andaman and Nicobar" ~ "Andaman & Nicobar Islands",
                          name == "NCT of Delhi" ~ "Delhi",
                          name == "Jammu and Kashmir" ~ "Jammu & Kashmir",
                          T ~ name))  %>% 
  filter(name %in% IND_regional_rgdp$admin_2_name) %>% 
  mutate(id = paste0(name, "_", iso)) %>% 
  dplyr::select(id, iso, geom)  

st_write(IND_regional_sf, "step2_obtain_gdp_data/temp/ind_admin_2.gpkg", append = F)

# eof ----
