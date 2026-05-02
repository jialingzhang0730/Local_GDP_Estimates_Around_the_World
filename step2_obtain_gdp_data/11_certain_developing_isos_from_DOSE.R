# --------------------------------- Task Summary --------------------------------- #
# This file obtains DOSE's subnational GDP data for the following developing countries:
#   THA, MOZ, UZB, KEN, VNM, SRB, ECU, BLR, ALB, LKA, BIH.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(sf)
library(jsonlite)
library(exactextractr)
library(terra)
library(qgisprocess)

iso_to_include <- c("THA", "MOZ", "UZB", "KEN", "VNM", "SRB", "ECU", "BLR",
                    "ALB", "LKA", "BIH")

DOSE_gdp_pre <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/regional/DOSE/DOSE_V2.11.csv")  %>% 
  dplyr::select(c(GID_0, GID_1, year, grp_lcu, pop, grp_pc_lcu))  %>%  # grp_lcu means regional product in local currency
  rename(iso = GID_0, id = GID_1)  %>% 
  filter(year >= 2012, iso %in% iso_to_include)  %>% 
  arrange(iso, year, id)

# there are some years for some countries have missing gdp data
which <- DOSE_gdp_pre  %>% 
  filter(is.na(grp_lcu))  %>% 
  distinct(iso, year)

# get rid of those countries in those years
DOSE_gdp_full <- DOSE_gdp_pre  %>% 
  anti_join(which, by = c("iso", "year"))

write.csv(DOSE_gdp_full, "step2_obtain_gdp_data/temp/DOSE_gdp_full.csv", row.names = F)
