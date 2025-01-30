# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file is to obtain Russia's regional GDP data for year 2020 and 2021

# Russia's subnational GDP data before 2020 can be obtained from OECD, but OECD suspend the participation of Russia and Belarus in its bodies.
# Thus, we can only obtain Russia's GDP data through table "GROSS REGIONAL PRODUCT BY CONSTITUENT ENTITIES OF THE RUSSIAN FEDERATION" from 
# "Russian Statistical Yearbook" on the "Federal State Statistics Service" Agency website "https://eng.rosstat.gov.ru/Publications/document/74811".
# Future updates of Russia's subnational GDP data should refer to the "Russian Statistical Yearbook"

# Note, the Gross Regional Product data are updated in a two year lag. So for example, if you want to find Gross Regional Product in 2020, one should
# refer to "Russian Statistical Yearbook 2022" for relevant data.

# Since the original table is in a word document and administrative units are slightly different than the OECD ones
# it will be eaiser to do it manually

# Following the description in the "inputs/gdp_data/regional/RUS/RUS.xlsx" file to get the data value for the
# OECD's Russian administrative units
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(units)
library(sf)
library(docxtractr)

# ------------------------------------------------- #
# Obtain GDP data:

# obtain RUS 2020 and 2021 subnational GDP data
RUS_2020_2021 <- read_excel(paste0("step2_obtain_gdp_data/inputs/gdp_data/regional/RUS/RUS.xlsx"), sheet = "data")  %>% 
    dplyr::select(c(id, name, region_gdp, year))  %>% 
    mutate(iso = "RUS", admin_1_name = "Russia")  %>% 
    rename(admin_2_id = id, admin_2_name = name, admin_2_rgdp_total = region_gdp)  %>% 
    group_by(iso, year)  %>% 
    mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total))  %>% 
    ungroup()

write.csv(RUS_2020_2021, "step2_obtain_gdp_data/temp/RUS_2020_2021.csv", row.names = F)

# ------------------------------------------------- #
# Create shapefiles -----

# Geometries are obtained together with OECD countries in file "3_oecd.R"