# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file retrieves India's provincial GDP data and processes their corresponding geometries.
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

# only read the last two sheets which contain year 2012 to 2021
IND_regional_rgdp_pre <- map_dfr(.x = c("T_27(iii)", "T_27(iv)"), .f = function(sheet){
  
  df_out <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/IND/27T_15112023E301A02422494F73BFAFD6CDD84EEEAE.XLSX", skip = 5, n_max = 34, sheet = sheet) %>%
    pivot_longer(cols = matches("\\d{4}"), names_to = "year")
  
  return(df_out)
  
})  %>% 
  rename(admin_2_name = ...1,
         rgdp_total = value)  %>% 
  mutate(year = as.numeric(substr(year,1,4)), 
         admin_unit = 2)  %>% 
  filter(!admin_2_name %in% c("Jammu & Kashmir*", "Jammu & Kashmir-U.T.")) %>% # Jammu & Kashmir changes to Jammu & Kashmir-U.T. after year 2019, deal with them below
  filter(year != 2022)  %>%
  mutate(rgdp_total = as.numeric(rgdp_total))

# deal with Jammu & Kashmir and Jammu & Kashmir-U.T.
# NOTE: if you see the following warning message, that's what we need
#   Warning message:
#   There was 1 warning in `mutate()`.
#   ℹ In argument: `rgdp_total = as.numeric(rgdp_total)`.
#   Caused by warning:
#   ! NAs introduced by coercion 
jk <- map_dfr(.x = c("T_27(iii)", "T_27(iv)"), .f = function(sheet){
  
  df_out <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/IND/27T_15112023E301A02422494F73BFAFD6CDD84EEEAE.XLSX", skip = 5, n_max = 34, sheet = sheet) %>%
    pivot_longer(cols = matches("\\d{4}"), names_to = "year")
  
  return(df_out)
  
})  %>% 
  rename(admin_2_name = ...1,
         rgdp_total = value)  %>% 
  mutate(year = as.numeric(substr(year,1,4)), 
         admin_unit = 2)  %>% 
  filter(admin_2_name %in% c("Jammu & Kashmir*", "Jammu & Kashmir-U.T."))  %>% 
  mutate(rgdp_total = as.numeric(rgdp_total))  %>% # if you see a warining message, that is correct
  group_by(year) %>%
  summarize(rgdp_total = coalesce(rgdp_total[admin_2_name == "Jammu & Kashmir-U.T."],
                                  rgdp_total[admin_2_name == "Jammu & Kashmir*"]),
            admin_2_name = "Jammu & Kashmir",
            admin_unit = 2) %>%
  ungroup()  %>% 
  filter(year != 2022)

# combine them
IND_regional_rgdp <- bind_rows(jk, IND_regional_rgdp_pre)  %>% 
  filter(year != 2022) %>% # year 2022 has many missing data, and we do not need it now
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
# Create shapefiles -----

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

IND_national_sf <- IND_regional_sf %>% 
  mutate(name = "India") %>% 
  group_by(name, iso) %>% 
  summarize(geom = st_union(geom),
            admin_unit = 1, .groups = "drop")

training_df <- IND_regional_rgdp %>% 
  mutate(parent_admin_unit = 1) %>% 
  rename_with(starts_with("admin_1"), .fn = ~ gsub("admin_1", "parent", .x)) %>% 
  rename_with(starts_with("admin_2"), .fn = ~ gsub("admin_2", "unit", .x)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_name, matches("parent_rgdp")) 

st_write(IND_regional_sf, "step2_obtain_gdp_data/temp/ind_admin_2.gpkg", append = F)
write.csv(training_df, "step2_obtain_gdp_data/temp/ind_training_data.csv", row.names = F)

# eof ----