# --------------------------------- Task Summary --------------------------------- #
# Retrieve the full training sample of regional GDP data.
# Note: When updating, ensure that the same id does not appear twice for each year 
#      to avoid duplication.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
rm(list = ls())
gc()

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tictoc)
library(gdata)
library(units)
library(sf)
library(tidyverse)
library(readxl)

# ------------------------------------------------- #
training_isos <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC",
                  "HUN", "ITA", "JPN", "KOR", "LTU", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "HRV",
                  "LVA", "SVN", "SWE", "TUR", "NZL", "IDN", "COL", "PER", "CHL", "EST", # above are countries from OECD
                  "USA", "KGZ", "PHL", # those are what we collected
                  "THA", "MOZ", "UZB", "KEN", "VNM", "SRB", "ECU", "BLR",
                  "ALB", "LKA", "BIH")# above are developing countries from DOSE data

rgdp_total_rescaled <- read.csv("step2_obtain_gdp_data/outputs/rgdp_total_rescaled.csv")  %>% 
       filter(iso %in% training_isos) %>% # This ensures that only the training iso are selected.
       filter(min_admin_unit != 1) %>% # This ensures that the specified countries are excluded, and their data is obtained from DOSE instead.
       filter(parent_admin_unit == 1) %>% # This ensures that counties are not repeated. For example, in the case of the USA, each county's information appears twice with different parent names.           
       dplyr::select(-c(unit_name, min_admin_unit, parent_admin_unit, parent_name))

DOSE <- read.csv("step2_obtain_gdp_data/outputs/DOSE_certain_developing_isos_total_rescaled.csv") 

training_sample <- bind_rows(rgdp_total_rescaled, DOSE)  %>% 
       as.data.frame()     

write.csv(training_sample, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_training_data.csv", row.names = F)