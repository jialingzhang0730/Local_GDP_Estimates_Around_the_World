# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file is to rescale the regional data to match with IMF or WB or UN's national GDP data
#
# Note:
# For USA, aggregated regional GDP does not equal to total country GDP, this is because:
#         USA national GDP data contains territories like Puerto Rico, but non of the county GDP or state GDP
#         contains this data. So sum of county GDP or state GDP definitely do not equal to USA national GDP.
#          We still have some of those territories as separate "country"
# For BEL, DNK, ESP, FRA, GBR, ITA, NLD, and NOR, according to OECD, there are some GDP not regionalized. 
#         For those countries, we rescale the subnational GDP data so that they aggregated into national GDP
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(lubridate)
library(wbstats)
library(jsonlite)
library(httr)
library(readxl)
library(sf)
library(units)
library(readr)

# ------------------------------------------------- #
# read those regional data obtained before
files <- list.files("step2_obtain_gdp_data/temp") %>% 
  .[grepl("training_data.csv$", .)]

training_data_complete_pre <- lapply(files, function(file){
  
  out_df <- read_csv(paste0("step2_obtain_gdp_data/temp/", file), col_types = cols(id = col_character()))  %>% 
    mutate(id = as.character(id))
  
  return(out_df)
  
}) %>% reduce(bind_rows) %>% 
  dplyr::select(-c(parent_id))  %>% # we don't need this column
  filter(year >= 2012, year <= 2021) # we only need year 2012-2021 for now, when you update, you want to extend the year 

# For those BEL, DNK, ESP, FRA, GBR, ITA, NLD, and NOR countries, some of GDP data are not regionalised according to OECD. We 
#   do not have a clear explanation for it. So let's make those regional data aggregated to match with national GDP data

iso_change <- c("BEL", "DNK", "ESP", "FRA", "GBR", "ITA", "NLD", "NOR")

# create a scalar
scalar <- training_data_complete_pre  %>% 
  filter(parent_admin_unit == 1) %>% # so that you do not double count the data
  group_by(iso, year)  %>% 
  mutate(aggre_regio_GDP = sum(unit_rgdp_total))  %>% 
  ungroup()  %>% 
  distinct(year, iso, parent_rgdp_total, aggre_regio_GDP)  %>% 
  mutate(scalr = parent_rgdp_total/aggre_regio_GDP)  %>% 
  dplyr::select(c(year, iso, scalr))

# change regional GDP data to match with national GDP data
training_data_complete <- training_data_complete_pre  %>% 
  left_join(scalar)  %>% 
  mutate(unit_rgdp_total = ifelse(iso %in% iso_change, unit_rgdp_total*scalr, unit_rgdp_total)) %>% # note, we only change specific countries, not USA!!!!!
  mutate(parent_rgdp_total = ifelse(iso %in% iso_change & parent_admin_unit == 2, 
                                    parent_rgdp_total*scalr, parent_rgdp_total)) %>% # note here, TL2 level GDP data are also need to be adjusted
  dplyr::select(-c(scalr))

# read the national data
# Note, we actually only need one type of national data, probabily use GDP in constant 2017 USD because it has no missing data.
# We only need one type because we only need "share" in training the model

national_gdp <- read.csv("step2_obtain_gdp_data/temp/national_gdp_const_2017_USD.csv")  %>% 
  rename(rgdp_2017_USD = rgdp_total)

# ------------------------------------------------- #
# rescale training data

# first, we need to obtain scale factor
national_gdp_scale_factor <- filter(training_data_complete, parent_admin_unit == 1)  %>% 
  group_by(year, iso, parent_name, parent_rgdp_total) %>% 
  summarize(.groups = "drop")  %>% 
  left_join(national_gdp)  %>% 
  mutate(scale_factor = rgdp_2017_USD/parent_rgdp_total)  %>% 
  dplyr::select(c(year, iso, rgdp_2017_USD, population, scale_factor))

# now we can rescale the training data
training_data_rescaled <- training_data_complete  %>% 
  left_join(national_gdp_scale_factor)  %>% 
  mutate(across(matches("rgdp_total"), # note, now the variables "unit_rgdp_total" and "parent_rgdp_total" are rescaled
                .fns = ~ .x * scale_factor))  %>% 
  dplyr::select(-c(scale_factor, rgdp_2017_USD))  %>% 
  rename(national_population = population)

# Alaska's data are not put in the training sample, but we cannot ignore it. 
# We need to consider Alaska as a "country" and store the data

alaska_state <- read.csv("step2_obtain_gdp_data/temp/usa_state_gdp.csv")  %>% 
  filter(admin_2_name == "Alaska")  %>% 
  filter(year >= 2012, year <= 2021) %>% # we only need year 2012-2021 for now
  left_join(national_gdp_scale_factor)  %>% 
  mutate(across(matches("rgdp_total"), # note, now the variables "admin_2_rgdp_total" and "admin_1_rgdp_total" are rescaled
                .fns = ~ .x * scale_factor))  %>% 
  # now we need to create it to same structure as others
  mutate(id = "02", unit_name = "Alaska", iso = "Ala",
         min_admin_unit = 2, parent_admin_unit = 1, parent_name = "United States")  %>% 
  rename(parent_rgdp_total = admin_1_rgdp_total, national_population = population, unit_rgdp_total = admin_2_rgdp_total)  %>% 
  dplyr::select(-c(state_fips, admin_2_name, admin_1_name, rgdp_2017_USD, scale_factor))

# ------------------------------------------------- #
# other countries that do not have subnational GDP data
national_gdp_rest <- national_gdp  %>% 
  filter(!iso %in% unique(training_data_rescaled$iso))  %>% 
  mutate(id = iso, min_admin_unit = 1, unit_rgdp_total = NA, 
         parent_admin_unit = 1, parent_name = Country)  %>% 
  rename(unit_name = Country, parent_rgdp_total = rgdp_2017_USD, national_population = population)

## Save rescaled GDP data

rgdp_total_rescaled <- rbind(training_data_rescaled, national_gdp_rest, alaska_state) %>% 
  arrange(iso, year, parent_name)  %>% 
  filter(year >= 2012, year <= 2021) # we only need year 2012-2021 for now

write.csv(rgdp_total_rescaled, "step2_obtain_gdp_data/outputs/rgdp_total_rescaled.csv", row.names = F)

# rescale DOSE's subnational gdp data

certain_developing_isos <- read.csv("step2_obtain_gdp_data/temp/DOSE_gdp_full.csv")  %>% 
  left_join(national_gdp)  %>% 
  group_by(iso, year)  %>% 
  mutate(unit_rgdp_total = rgdp_2017_USD*grp_lcu/sum(grp_lcu))  %>% 
  ungroup()  %>% 
  rename(parent_rgdp_total = rgdp_2017_USD, national_population = population)  %>% 
  dplyr::select(c(iso, id, year, unit_rgdp_total, parent_rgdp_total, national_population))  

write.csv(certain_developing_isos, "step2_obtain_gdp_data/outputs/DOSE_certain_developing_isos_total_rescaled.csv", row.names = F)

# eof -----