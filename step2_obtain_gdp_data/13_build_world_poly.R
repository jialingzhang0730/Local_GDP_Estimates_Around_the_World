# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file is to obtain world polygons needed 
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(rmapshaper)
library(tidyverse)
library(tictoc)
library(sf)
library(gdata)
library(units)
library(countrycode)
library(qgisprocess)

# ------------------------------------------------- #
## Training polygons

regional_gis_files <- list.files("step2_obtain_gdp_data/temp") %>% 
  .[grepl("admin_[^1]\\.gpkg$", .)]

regional_subnational_poly <- lapply(regional_gis_files, function(file){
  
  out_sf <- read_sf(paste0("step2_obtain_gdp_data/temp/", file)) %>% 
           st_set_crs(4326)
  
  return(out_sf)
  
}) %>% reduce(rbind)


training_poly_complete <- read_sf("step2_obtain_gdp_data/temp/oecd_training_poly.gpkg") %>% 
  dplyr::select(id, iso, geom) %>% 
  rbind(regional_subnational_poly) %>% 
  st_set_crs(4326)

st_write(training_poly_complete, "step2_obtain_gdp_data/outputs/training_poly_full.gpkg", append = F)

# ------------------------------------------------- #
## World nation polygons
hkg_mac <- read_sf("step1_obtain_gis_data/outputs/gdam_prov_level1_without_largewater.gpkg")  %>% 
            filter(GID_0 == "CHN" & NAME_1 %in% c("Hong Kong", "Macau"))  %>% 
            dplyr::select(-c(NAME_1, ENGTYPE_1))  %>% 
            rename(iso = GID_0) %>%
            mutate(iso = c("HKG", "MAC")) %>%
            st_set_crs(4326)  %>% 
            dplyr::select(iso)

regional_subnational_poly_noid <- regional_subnational_poly  %>% 
      dplyr::select(-c(id))  %>% 
      st_set_crs(4326)

oecd <- read_sf("step2_obtain_gdp_data/temp/oecd_poly.gpkg")  %>% 
        filter(!iso %in% unique(regional_subnational_poly_noid$iso)) %>%
        st_set_crs(4326)  %>% 
        filter(iso != id)  %>% 
        dplyr::select(-c(id))

pak <- read_sf("step1_obtain_gis_data/outputs/gadm_country_level0_without_largerwater.gpkg")  %>% 
        filter(GID_0 == "PAK")  %>% 
        rename(iso = GID_0) %>%
        dplyr::select(c(iso)) %>%
        st_set_crs(4326)

islands_isos <- c("ABW","BMU","CUW","CYM","PRI","PSE","SXM","TCA")
islands <- read_sf("step1_obtain_gis_data/outputs/gadm_country_level0_without_largerwater.gpkg")  %>% 
        filter(GID_0 %in% islands_isos)  %>% 
        rename(iso = GID_0) %>%
        dplyr::select(c(iso)) %>%
        st_set_crs(4326)

# consider Alaska as a "separate" country because I did not put it as in the training sample
alaska <- read_sf("step1_obtain_gis_data/outputs/gdam_prov_level1_without_largewater.gpkg")  %>% 
        filter(GID_0 == "USA", NAME_1 == "Alaska")  %>% 
        rename(iso = GID_0) %>%
        mutate(iso = "Alaska_USA")  %>% 
        dplyr::select(c(iso)) %>%
        st_set_crs(4326)

world_poly_pre <- read_sf("step1_obtain_gis_data/outputs/CGAZ_ADM1_without_large_waters.gpkg") %>% 
  rename(iso = shapeGroup) %>% 
  st_set_crs(4326)  %>% 
  drop_na(iso) %>% 
  filter(!iso %in% c(unique(hkg_mac$iso), unique(oecd$iso), unique(regional_subnational_poly_noid$iso, islands_isos), "PAK"))  %>% 
  dplyr::select(iso, geom)  %>% 
  rbind(hkg_mac, oecd, regional_subnational_poly_noid, pak, islands, alaska) 

st_write(world_poly_pre, "step2_obtain_gdp_data/temp/world_poly_pre.gpkg", append = F)

temp_output_file <- tempfile(fileext = ".gpkg")

aggregate <- qgisprocess::qgis_run_algorithm(
  "native:aggregate",
  INPUT = "step2_obtain_gdp_data/temp/world_poly_pre.gpkg",
  GROUP_BY = "iso",
  AGGREGATES = list(list("aggregate" = "concatenate", "input" = '"iso"', "delimiter" = ",", "name" = "iso", "type" = 10, "length" = 0, "precision" = 0)),
  OUTPUT = temp_output_file
)

world_poly <- read_sf(temp_output_file) %>% 
  mutate(iso = substr(iso, 1, 3)) %>% 
  filter(!iso %in% c("ATA", "111", "112", "113", "114", "115", "116", "117", "118",
                     "119", "120", "121", "122", "123", "124", "125", "126", "127",
                     "128", "129")) 

st_write(world_poly, "step2_obtain_gdp_data/outputs/world_poly.gpkg", append = F)

# ------------------------------------------------- #
## build training sample polygons
# select the following countries to put in the training sample to train the random forest: DOSE's certain developing isos (have some years missing)
# for year 2021, there are some developed countries' data missing

training_isos <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC",
                  "HUN", "ITA", "JPN", "KOR", "LTU", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "HRV",
                  "LVA", "SVN", "SWE", "TUR", "NZL", "IDN", "COL", "PER", "CHL", "EST", # above are countries from OECD
                  "USA", "KGZ", "PHL", # those are what we collected
                  "THA", "MOZ", "UZB", "KEN", "VNM", "SRB", "ECU", "BLR",
                  "ALB", "LKA", "BIH")# above are developing countries from DOSE data

iso_1 <- training_poly_complete  %>% 
  filter(iso %in% training_isos)

iso_2 <- read_sf("step2_obtain_gdp_data/temp/DOSE_certain_developing_isos_without_large_water.gpkg")  %>% 
  filter(iso %in% training_isos) 

training_sample_geom <- bind_rows(iso_1, iso_2)

st_write(training_sample_geom, "step2_obtain_gdp_data/outputs/training_poly_sample.gpkg", append = F)

# ------------------------------------------------- #
# build complete polygons
# Note here, Western Sahara is not included in Morocco. We could not find Western Sahara's national data, so leave it blank

rgdp_total_rescaled <- read.csv("step2_obtain_gdp_data/outputs/rgdp_total_rescaled.csv", encoding = "UTF-8")  %>% 
  filter(!iso %in% c("UVK", "WBG")) # we already have WBG as "PSE" and "UVK" as "XKX" data

complete_poly <- rbind(training_poly_complete, world_poly %>% mutate(id = ifelse(iso == "Ala", "02", iso))) %>% 
  mutate(id = ifelse(id == "Bangsamoro Autonomous Region\r\nin Muslim Mindanao_PHL", "Bangsamoro Autonomous Region\nin Muslim Mindanao_PHL", id))  %>% # i do not know why the id name is not read correctly here
  filter(id %in% rgdp_total_rescaled$id)

st_write(complete_poly, "step2_obtain_gdp_data/outputs/complete_poly.gpkg", append = F)

# eof -----