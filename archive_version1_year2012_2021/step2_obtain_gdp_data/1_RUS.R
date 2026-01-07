# --------------------------------- Task Summary --------------------------------- #
# This file retrieves regional GDP data for Russia for the years 2020 and 2021.
# Prior to 2020, Russia's subnational GDP data could be obtained from the OECD. However, 
#   following the suspension of Russia and Belarus' participation in OECD bodies, the only 
#   available source is the "GROSS REGIONAL PRODUCT BY CONSTITUENT ENTITIES OF THE RUSSIAN 
#   FEDERATION" table from the Russian Statistical Yearbook, published by the Federal State 
#   Statistics Service. The relevant data can be accessed at: https://eng.rosstat.gov.ru/Publications/document/74811.
# Note that the Gross Regional Product (GRP) data is updated with a two-year lag. For instance, 
#   data for 2020 can be found in the Russian Statistical Yearbook 2022.
# Since the original data is in a Word document format and the administrative units differ
#   slightly from the OECD's, manual extraction is recommended.
# The data values for Russia's OECD administrative units should be derived as described in 
#   the "inputs/gdp_data/regional/RUS/RUS.xlsx" file.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(units)
library(sf)
library(docxtractr)

# obtain RUS 2020 and 2021 subnational GDP data
RUS_2020_2021 <- read_excel(paste0("step2_obtain_gdp_data/inputs/gdp_data/regional/RUS/RUS.xlsx"), sheet = "data")  %>% 
    dplyr::select(c(id, name, region_gdp, year))  %>% 
    mutate(iso = "RUS", admin_1_name = "Russia")  %>% 
    rename(admin_2_id = id, admin_2_name = name, admin_2_rgdp_total = region_gdp)  %>% 
    group_by(iso, year)  %>% 
    mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total))  %>% 
    ungroup()

write.csv(RUS_2020_2021, "step2_obtain_gdp_data/temp/RUS_2020_2021.csv", row.names = F)

# Geometries are obtained together with OECD countries in file "3_oecd.R"