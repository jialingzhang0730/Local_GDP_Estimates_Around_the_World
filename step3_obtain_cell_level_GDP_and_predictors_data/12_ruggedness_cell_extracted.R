# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# Obtain cell's ruggedness
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
rm(list = ls())
gc()

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(gdalUtilities)
library(parallel)
library(rhdf5)
library(terra)
library(tictoc)
library(tiff)
library(foreach)
library(iterators)
library(doParallel)
library(furrr)
library(exactextractr)
library(future)
library(future.apply)
library(sf)
library(dplyr)

# please change to your specific folder
setwd("/share/rossihansberglab/Nightlights_GDP/replication_packages_world_GCP")

# ------------------------------------------------------------------------------------------------------------
# 1 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

tic("Ruggedness")
ruggedness <- rast("step3_obtain_cell_level_GDP_and_predictors_data/inputs/ruggedness/tri.txt", crs("epsg:4326"))
mean_ruggedness <- exact_extract(ruggedness, simplified_poly, fun = 'mean', 
                                 coverage_area = T, progress = T, 
                                 append_cols = c("cell_id", "id", "iso")) %>% 
  mutate(mean_rug = mean/1000) %>% 
  dplyr::select(-mean)

write.csv(mean_ruggedness, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/mean_ruggedness_1deg.csv", row.names = F)
toc()

# ------------------------------------------------------------------------------------------------------------
# 0.5 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 


tic("Ruggedness")
ruggedness <- rast("step3_obtain_cell_level_GDP_and_predictors_data/inputs/ruggedness/tri.txt", crs("epsg:4326"))
mean_ruggedness <- exact_extract(ruggedness, simplified_poly, fun = 'mean', 
                                 coverage_area = T, progress = T, 
                                 append_cols = c("cell_id", "subcell_id", "id", "iso")) %>% 
  mutate(mean_rug = mean/1000) %>% 
  dplyr::select(-mean)

write.csv(mean_ruggedness, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/mean_ruggedness_0_5deg.csv", row.names = F)
toc()

# ------------------------------------------------------------------------------------------------------------
# 0.25 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

tic("Ruggedness")
ruggedness <- rast("step3_obtain_cell_level_GDP_and_predictors_data/inputs/ruggedness/tri.txt", crs("epsg:4326"))
mean_ruggedness <- exact_extract(ruggedness, simplified_poly, fun = 'mean', 
                                 coverage_area = T, progress = T, 
                                 append_cols = c("cell_id", "subcell_id", "subcell_id_0_25", "id", "iso")) %>% 
  mutate(mean_rug = mean/1000) %>% 
  dplyr::select(-mean)

write.csv(mean_ruggedness, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/mean_ruggedness_0_25deg.csv", row.names = F)
toc()
