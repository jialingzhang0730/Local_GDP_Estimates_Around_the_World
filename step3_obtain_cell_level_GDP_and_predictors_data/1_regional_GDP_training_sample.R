# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# Obtain full training sample regional GDP data
# Note, when you update, double check the same id for each year does not appear twice to avoid repeatation.
# ------------------------------------------------------------------------------------------------- #

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

# please change to your specific folder
setwd("/share/rossihansberglab/Nightlights_GDP/replication_packages_world_GCP")

# ------------------------------------------------- #
training_isos <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC",
                  "HUN", "ITA", "JPN", "KOR", "LTU", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "HRV",
                  "LVA", "SVN", "SWE", "TUR", "NZL", "IDN", "COL", "PER", "CHL", "EST", # above are countries from OECD
                  "USA", "KGZ", "PHL", # those are what we collected
                  "THA", "MOZ", "UZB", "KEN", "VNM", "SRB", "ECU", "BLR",
                  "ALB", "LKA", "BIH")# above are developing countries from DOSE data

rgdp_total_rescaled <- read.csv("step2_obtain_gdp_data/outputs/rgdp_total_rescaled.csv")  %>% 
       filter(iso %in% training_isos) %>% # so that we only select training isos
       filter(min_admin_unit != 1) %>% # so that we exclude those countries which we want the data from DOSE
       filter(parent_admin_unit == 1) %>% # so that the county will not be repeated. For example, for USA, each county's information appears twice with different parent_name           
       dplyr::select(-c(unit_name, min_admin_unit, parent_admin_unit, parent_name))

DOSE <- read.csv("step2_obtain_gdp_data/outputs/DOSE_certain_developing_isos_total_rescaled.csv") 

training_sample <- bind_rows(rgdp_total_rescaled, DOSE)  %>% 
       as.data.frame()     

write.csv(training_sample, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_training_data.csv", row.names = F)