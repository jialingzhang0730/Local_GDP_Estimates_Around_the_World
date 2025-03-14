# --------------------------------- Task Summary --------------------------------- #
# This file retrieves DOSE's subnational GDP data for the following developing countries:
#     THA, MOZ, UZB, KEN, VNM, SRB, ECU, BLR, ALB, LKA, BIH
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(units)
library(sf)
library(jsonlite)
library(exactextractr)
library(terra)
library(qgisprocess)

iso_to_include <- c("THA", "MOZ", "UZB", "KEN", "VNM", "SRB", "ECU", "BLR",
                    "ALB", "LKA", "BIH")

# The spatial geometry follows DOSE's replication package

gadm_path <- "step1_obtain_gis_data/inputs/DOSE_spatial_data/"

gadm <- st_read(paste0(gadm_path, "gadm36_1.shp")) # Note: The GADM shapefile has to be downloaded from https://gadm.org/download_world36.html
custom <- st_read(paste0(gadm_path, "all_non_GADM_regions.shp"))  %>% dplyr::select(-c("fid"))
unneeded_list <- c("KAZ", "MKD", "NPL", "PHL", "LKA")
gadm_trim <- gadm[!gadm$GID_0 %in% unneeded_list, ]
gadm_custom <- rbind(gadm_trim, custom) %>% 
  rename(geom = geometry)

st_write(gadm_custom, "step2_obtain_gdp_data/temp/DOSE_gadm_custom_merged.gpkg", append = F) # This is DOSE dataset's geometry

DOSE_certain_developing_isos <- gadm_custom  %>% 
    dplyr::select(c(GID_0, GID_1))  %>%
    rename(iso = GID_0, id = GID_1)  %>% 
    filter(iso %in% iso_to_include)

st_write(DOSE_certain_developing_isos, "step2_obtain_gdp_data/temp/DOSE_certain_developing_isos.gpkg", append = F) # Those are the developing regions that will go to our training sample

# remove large inland waters
difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = "step2_obtain_gdp_data/temp/DOSE_certain_developing_isos.gpkg", 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step2_obtain_gdp_data/temp/DOSE_certain_developing_isos_without_large_water.gpkg", 
  .quiet = FALSE
)

# ------------------------------------------------- #
# Obtain GDP data: 

DOSE_gdp_pre <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/regional/DOSE/DOSE_V2.csv")  %>% 
    dplyr::select(c(GID_0, GID_1, year, grp_lcu, pop, grp_pc_lcu))  %>%  # grp_lcu means regional product in local currency
    rename(iso = GID_0, id = GID_1)  %>% 
    filter(year >= 2012, iso %in% iso_to_include)  %>% 
    arrange(iso, year, id)

# For some countries, GDP data is missing for certain years.
which <- DOSE_gdp_pre  %>% 
  filter(is.na(grp_lcu))  %>% 
  distinct(iso, year)

# Exclude those countries for the respective years where data is unavailable.
DOSE_gdp_full <- DOSE_gdp_pre  %>% 
    anti_join(which, by = c("iso", "year"))

write.csv(DOSE_gdp_full, "step2_obtain_gdp_data/temp/DOSE_gdp_full.csv", row.names = F)
