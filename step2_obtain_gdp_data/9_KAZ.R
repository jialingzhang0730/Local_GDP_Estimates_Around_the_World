# --------------------------------- Task Summary --------------------------------- #
# This file retrieves regional GDP data for kazakhstan and processes the 
#   corresponding geometries.
# NOTE!!! kazakhstan's regions boundaries changed within 2008 to 2022:
#       1. Start 2018, Ontustik Kazakhstan is separated into Shymkent city + Turkistan
#       2. Start 2021, Abay is separated from Shygys Kazakhstan
#       2. Start 2021, Zhetisu is separated from Аlmaty
#       3. Start 2021, Ulytau is separated from Кaragandy
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(units)
library(sf)

# Obtain GDP data: 

KAZ_regional_rgdp_pre <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/KAZ/1. Gross regional product.xlsx", sheet = "2008-2023", skip = 2, n_max = 22)  %>% 
       slice(-1) %>% # Remove the first row, as it contains the total GDP for the entire country.
       rename(admin_2_name = ...1)  %>% 
       dplyr::select(c(admin_2_name, "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                       "2016", "2017", "2018", "2019", "2020", "2021", "2022")) %>%  # select annual data, not quarterly data
       filter(!admin_2_name %in% c("Ontustik Kazakhstan", "Shymkent city", "Turkistan",
                                   "Abay", "Shygys Kazakhstan", "Zhetisu", "Аlmaty", 
                                   "Ulytau", "Кaragandy")) # Those regions boundary have changed, deal with it below

# deal with: Start 2018, Ontustik Kazakhstan is separated into Shymkent city + Turkistan
ost <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/KAZ/1. Gross regional product.xlsx", sheet = "2008-2023", skip = 2, n_max = 22)  %>% 
       slice(-1) %>% # Remove the first row, as it contains the total GDP for the entire country.
       rename(admin_2_name = ...1)  %>% 
       dplyr::select(c(admin_2_name, "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                       "2016", "2017", "2018", "2019", "2020", "2021", "2022")) %>%  # select annual data, not quarterly data
       filter(admin_2_name %in% c("Ontustik Kazakhstan", "Shymkent city", "Turkistan"))  %>%   
       mutate(`2008` = ifelse(is.na(`2008`), sum(`2008`[2:3]), `2008`),
            `2009` = ifelse(is.na(`2009`), sum(`2009`[2:3]), `2009`),
            `2010` = ifelse(is.na(`2010`), sum(`2010`[2:3]), `2010`),
            `2011` = ifelse(is.na(`2011`), sum(`2011`[2:3]), `2011`),
            `2012` = ifelse(is.na(`2012`), sum(`2012`[2:3]), `2012`),
            `2013` = ifelse(is.na(`2013`), sum(`2013`[2:3]), `2013`),
            `2014` = ifelse(is.na(`2014`), sum(`2014`[2:3]), `2014`),
            `2015` = ifelse(is.na(`2015`), sum(`2015`[2:3]), `2015`),
            `2016` = ifelse(is.na(`2016`), sum(`2016`[2:3]), `2016`),
            `2017` = ifelse(is.na(`2017`), sum(`2017`[2:3]), `2017`),
            `2018` = ifelse(is.na(`2018`), sum(`2018`[2:3]), `2018`),
            `2019` = ifelse(is.na(`2019`), sum(`2019`[2:3]), `2019`),
            `2020` = ifelse(is.na(`2020`), sum(`2020`[2:3]), `2020`),
            `2021` = ifelse(is.na(`2021`), sum(`2021`[2:3]), `2021`),
            `2022` = ifelse(is.na(`2022`), sum(`2022`[2:3]), `2022`))  %>% # Before 2018, "Ontustik Kazakhstan" contains individual values. After 2018, the values for "Ontustik Kazakhstan" are replaced by the sum of the values for "Shymkent city" and "Turkistan".
       filter(admin_2_name == "Ontustik Kazakhstan")

# deal with: Start 2021, Abay is separated from Shygys Kazakhstan
as <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/KAZ/1. Gross regional product.xlsx", sheet = "2008-2023", skip = 2, n_max = 22)  %>% 
       slice(-1) %>% # Remove the first row, as it contains the total GDP for the entire country.
       rename(admin_2_name = ...1)  %>% 
       dplyr::select(c(admin_2_name, "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                       "2016", "2017", "2018", "2019", "2020", "2021", "2022")) %>%  # select annual data, not quarterly data
       filter(admin_2_name %in% c("Abay", "Shygys Kazakhstan"))  %>% 
       mutate_all(~ replace_na(., 0)) %>%  # Replace NA with 0
       mutate(`2008` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2008`[1:2]), `2008`),
            `2009` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2009`[1:2]), `2009`),
            `2010` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2010`[1:2]), `2010`),
            `2011` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2011`[1:2]), `2011`),
            `2012` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2012`[1:2]), `2012`),
            `2013` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2013`[1:2]), `2013`),
            `2014` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2014`[1:2]), `2014`),
            `2015` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2015`[1:2]), `2015`),
            `2016` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2016`[1:2]), `2016`),
            `2017` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2017`[1:2]), `2017`),
            `2018` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2018`[1:2]), `2018`),
            `2019` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2019`[1:2]), `2019`),
            `2020` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2020`[1:2]), `2020`),
            `2021` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2021`[1:2]), `2021`),
            `2022` = ifelse(admin_2_name == "Shygys Kazakhstan", sum(`2022`[1:2]), `2022`)) %>% # This replaces the values for "Shygys Kazakhstan" with the sum of "Abay" and "Shygys Kazakhstan"
            # However, this change only affects data from 2021 onward, as "Abay" had a value of 0 before 2021.
       filter(admin_2_name == "Shygys Kazakhstan")

# deal with: Start 2021, Zhetisu is separated from Аlmaty
za <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/KAZ/1. Gross regional product.xlsx", sheet = "2008-2023", skip = 2, n_max = 22)  %>% 
       slice(-1) %>% # Remove the first row, as it contains the total GDP for the entire country.
       rename(admin_2_name = ...1)  %>% 
       dplyr::select(c(admin_2_name, "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                       "2016", "2017", "2018", "2019", "2020", "2021", "2022")) %>%  # select annual data, not quarterly data
       filter(admin_2_name %in% c("Zhetisu", "Аlmaty"))  %>% 
       mutate_all(~ replace_na(., 0)) %>%  # Replace NA with 0
       mutate(`2008` = ifelse(admin_2_name == "Аlmaty", sum(`2008`[1:2]), `2008`),
            `2009` = ifelse(admin_2_name == "Аlmaty", sum(`2009`[1:2]), `2009`),
            `2010` = ifelse(admin_2_name == "Аlmaty", sum(`2010`[1:2]), `2010`),
            `2011` = ifelse(admin_2_name == "Аlmaty", sum(`2011`[1:2]), `2011`),
            `2012` = ifelse(admin_2_name == "Аlmaty", sum(`2012`[1:2]), `2012`),
            `2013` = ifelse(admin_2_name == "Аlmaty", sum(`2013`[1:2]), `2013`),
            `2014` = ifelse(admin_2_name == "Аlmaty", sum(`2014`[1:2]), `2014`),
            `2015` = ifelse(admin_2_name == "Аlmaty", sum(`2015`[1:2]), `2015`),
            `2016` = ifelse(admin_2_name == "Аlmaty", sum(`2016`[1:2]), `2016`),
            `2017` = ifelse(admin_2_name == "Аlmaty", sum(`2017`[1:2]), `2017`),
            `2018` = ifelse(admin_2_name == "Аlmaty", sum(`2018`[1:2]), `2018`),
            `2019` = ifelse(admin_2_name == "Аlmaty", sum(`2019`[1:2]), `2019`),
            `2020` = ifelse(admin_2_name == "Аlmaty", sum(`2020`[1:2]), `2020`),
            `2021` = ifelse(admin_2_name == "Аlmaty", sum(`2021`[1:2]), `2021`),
            `2022` = ifelse(admin_2_name == "Аlmaty", sum(`2022`[1:2]), `2022`)) %>% # This replaces the values for "Almaty" with the sum of "Zhetisu" and "Almaty"
            # However, this change only applies from 2021 onward, as "Zhetisu" had missing values (NA) before 2021.
       filter(admin_2_name == "Аlmaty")

# deal with: Start 2021, Ulytau is separated from Кaragandy
uk <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/KAZ/1. Gross regional product.xlsx", sheet = "2008-2023", skip = 2, n_max = 22)  %>% 
       slice(-1) %>% # Remove the first row, as it contains the total GDP for the entire country.
       rename(admin_2_name = ...1)  %>% 
       dplyr::select(c(admin_2_name, "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                       "2016", "2017", "2018", "2019", "2020", "2021", "2022")) %>%  # select annual data, not quarterly data
       filter(admin_2_name %in% c("Ulytau", "Кaragandy"))  %>% 
       mutate_all(~ replace_na(., 0)) %>%  # Replace NA with 0
       mutate(`2008` = ifelse(admin_2_name == "Кaragandy", sum(`2008`[1:2]), `2008`),
            `2009` = ifelse(admin_2_name == "Кaragandy", sum(`2009`[1:2]), `2009`),
            `2010` = ifelse(admin_2_name == "Кaragandy", sum(`2010`[1:2]), `2010`),
            `2011` = ifelse(admin_2_name == "Кaragandy", sum(`2011`[1:2]), `2011`),
            `2012` = ifelse(admin_2_name == "Кaragandy", sum(`2012`[1:2]), `2012`),
            `2013` = ifelse(admin_2_name == "Кaragandy", sum(`2013`[1:2]), `2013`),
            `2014` = ifelse(admin_2_name == "Кaragandy", sum(`2014`[1:2]), `2014`),
            `2015` = ifelse(admin_2_name == "Кaragandy", sum(`2015`[1:2]), `2015`),
            `2016` = ifelse(admin_2_name == "Кaragandy", sum(`2016`[1:2]), `2016`),
            `2017` = ifelse(admin_2_name == "Кaragandy", sum(`2017`[1:2]), `2017`),
            `2018` = ifelse(admin_2_name == "Кaragandy", sum(`2018`[1:2]), `2018`),
            `2019` = ifelse(admin_2_name == "Кaragandy", sum(`2019`[1:2]), `2019`),
            `2020` = ifelse(admin_2_name == "Кaragandy", sum(`2020`[1:2]), `2020`),
            `2021` = ifelse(admin_2_name == "Кaragandy", sum(`2021`[1:2]), `2021`),
            `2022` = ifelse(admin_2_name == "Кaragandy", sum(`2022`[1:2]), `2022`)) %>% # This replaces the values for "Karagandy" with the sum of "Ulytau" and "Karagandy"
            # However, this change only applies from 2021 onward, as "Ulytau" had missing values (NA) before 2021.
       filter(admin_2_name == "Кaragandy")

# combine them
KAZ_regional_rgdp <- bind_rows(KAZ_regional_rgdp_pre, ost, as, za, uk)  %>% 
       pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
       mutate(year = as.numeric(year), admin_unit = 2) %>% 
       rename(rgdp_total = value) %>% 
       pivot_wider(names_from = admin_unit, values_from = c(matches("rgdp")),
                     names_glue = "admin_{admin_unit}_{.value}") %>% 
       group_by(year) %>% 
       mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total)) %>% 
       ungroup()  %>% 
       mutate(id = paste0(admin_2_name, "_KAZ"), iso = "KAZ", 
              min_admin_unit = 2, admin_1_name = "Kazakhstan") %>% 
       dplyr::select(id, iso, year, min_admin_unit, starts_with("admin_2"), starts_with("admin_1"))

write.csv(KAZ_regional_rgdp, "step2_obtain_gdp_data/temp/kaz_gdp_clean.csv", row.names = F)

# ------------------------------------------------- #
# Create shapefiles -----

KAZ_regional_sf <- read_sf("step1_obtain_gis_data/outputs/CGAZ_ADM1_without_large_waters.gpkg") %>%  
  rename(name = shapeName, iso = shapeGroup) %>%
  filter(iso == "KAZ")  %>% 
  mutate(name = case_when(name == "Akmola Region" ~ "Akmola",
                          name == "Aktobe Region" ~ "Аktobе",
                          name == "Almaty" ~ "Almaty city",
                          name == "Almaty Region" ~ "Аlmaty",
                          name == "Astana" ~ "Astana city",
                          name == "Atyrau Region" ~ "Аtyrau",
                          name == "East Kazakhstan Region" ~ "Shygys Kazakhstan",
                          name == "Jambyl Region" ~ "Zhambyl",
                          name == "Karaganda Region" ~ "Кaragandy",
                          name == "Kostanay Region" ~ "Коstanay",
                          name == "Kyzylorda Region" ~ "Кyzylorda",
                          name == "Mangystau Region" ~ "Мangystau",
                          name == "North Kazakhstan Region" ~ "Soltustik Кazakhstan",
                          name == "Pavlodar Region" ~ "Pavlodar",
                          name == "South Kazakhstan Region" ~ "Ontustik Kazakhstan",
                          name == "West Kazakhstan Region" ~ "Batys Kazakhstan",
                          T ~ name))  %>% 
  filter(name %in% KAZ_regional_rgdp$admin_2_name) %>% 
  mutate(id = paste0(name, "_", iso)) %>% 
  dplyr::select(id, iso, geom)

training_df <- KAZ_regional_rgdp %>% 
  mutate(parent_admin_unit = 1) %>% 
  rename_with(starts_with("admin_1"), .fn = ~ gsub("admin_1", "parent", .x)) %>% 
  rename_with(starts_with("admin_2"), .fn = ~ gsub("admin_2", "unit", .x)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_name, matches("parent_rgdp"))

st_write(KAZ_regional_sf, "step2_obtain_gdp_data/temp/kaz_admin_2.gpkg", append = F)
write.csv(training_df, "step2_obtain_gdp_data/temp/kaz_training_data.csv", row.names = F)
