# --------------------------------- Task Summary --------------------------------- #
# This file is responsible for retrieving geometry data for subsequent processing.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tictoc)
library(sf)
library(parallel)
library(exactextractr)
library(gdata)
library(units)
library(tidyverse)
library(sp)
library(tidyr)
library(stars)
library(terra)
library(exactextractr)
library(readxl)
library(qgisprocess)

# 1. process country geom
chn_ind_pak <- read_sf("step1_obtain_gis_data/inputs/CGZA_ADM1/gadm_410-levels.gpkg", layer = "ADM_0") %>% 
    filter(COUNTRY %in% c("China", "India", "Pakistan")) %>% 
    group_by(COUNTRY) %>%
    summarize(geom = st_union(geom), .groups = "drop") %>% 
    mutate(GID_0 = case_when(COUNTRY == "China" ~ "CHN",
                             COUNTRY == "India" ~ "IND",
                             COUNTRY == "Pakistan" ~ "PAK"))

gadm_levl0 <- read_sf("step1_obtain_gis_data/inputs/CGZA_ADM1/gadm_410-levels.gpkg", layer = "ADM_0") %>% 
    filter(!COUNTRY %in% c("China", "India", "Pakistan")) %>% 
    bind_rows(chn_ind_pak) %>% 
    filter(GID_0 != "ATA")

st_write(gadm_levl0, "step1_obtain_gis_data/outputs/gadm_410_countrylevel0_dissolve_union.gpkg", append = FALSE)

# 2. process prov geom
chn_ind_pak <- read_sf("step1_obtain_gis_data/inputs/CGZA_ADM1/gadm_410-levels.gpkg", layer = "ADM_1") %>% 
    filter(COUNTRY %in% c("China", "India", "Pakistan")) %>% 
    group_by(COUNTRY, NAME_1, ENGTYPE_1)%>%
    summarize(geom = st_union(geom), .groups = "drop") %>% 
    mutate(GID_0 = case_when(COUNTRY == "China" ~ "CHN",
                             COUNTRY == "India" ~ "IND",
                             COUNTRY == "Pakistan" ~ "PAK"))

gadm_levl1 <- read_sf("step1_obtain_gis_data/inputs/CGZA_ADM1/gadm_410-levels.gpkg", layer = "ADM_1") %>% 
    filter(!COUNTRY %in% c("China", "India", "Pakistan")) %>% 
    dplyr::select(c(GID_0, COUNTRY, NAME_1, ENGTYPE_1)) %>% 
    bind_rows(chn_ind_pak) %>% 
    filter(GID_0 != "ATA")

st_write(gadm_levl1, "step1_obtain_gis_data/outputs/gadm_410_prov_level1_dissolve_union.gpkg", append = FALSE)

# 3. now remove large inland waters
file.remove("step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.sbn")

difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = "step1_obtain_gis_data/inputs/CGZA_ADM1/geoBoundariesCGAZ_ADM1.geojson", 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step1_obtain_gis_data/outputs/CGAZ_ADM1_without_large_waters.gpkg", 
  .quiet = FALSE
)

difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = "step1_obtain_gis_data/outputs/gadm_410_countrylevel0_dissolve_union.gpkg", 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step1_obtain_gis_data/outputs/gadm_country_level0_without_largerwater.gpkg", 
  .quiet = FALSE
)

difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = "step1_obtain_gis_data/outputs/gadm_410_prov_level1_dissolve_union.gpkg", 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step1_obtain_gis_data/outputs/gdam_prov_level1_without_largewater.gpkg", 
  .quiet = FALSE
)

difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = read_sf("step1_obtain_gis_data/inputs/CGZA_ADM1/gadm_410-levels.gpkg", layer = "ADM_2"), 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step1_obtain_gis_data/outputs/gdam_county_level2_without_water_CHN_IND_PAK_not_union.gpkg", 
  .quiet = FALSE
)



