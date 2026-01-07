# --------------- Task --------------- #
# This file is to obtain each cell's nighttime light emitted from urban areas and cropland areas:
# We want to split each city's geometry into sections corresponding to each of the landcover types present within its boundary,
# resulting in potentially multiple polygons per cell, each tagged with a different landcover type.
# ------------------------------------ #

rm(list = ls())
gc()

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
library(raster)
library(stringr)

# ------------------------------------------------------------------------------------------------------------
# 1 degree

# obtain the uban geom within each cell
population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23] #choose years only after 2012

pop_urban_extracted_region_level_1deg <- do.call(rbind, mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  year_folder <- str_extract(filename, "\\d{4}")
  
  simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_urban_inters_id_1deg_", year_folder, ".gpkg"))  %>% 
    dplyr::select(-c("fid_3"))  %>% 
    rename(land_type = paste0("test",year_folder))
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
    rename(pop_urban = exact_extract.r..simplified_poly...sum..) %>% 
    mutate(year = year_folder)
  return(extract)
  
}))
save(pop_urban_extracted_region_level_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_urban_extracted_region_level_1deg.RData")

# obtain the cropland geom within each cell

pop_cropland_extracted_region_level_1deg <- do.call(rbind, mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  year_folder <- str_extract(filename, "\\d{4}")
  
  simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_cropland_inters_id_1deg_", year_folder, ".gpkg"))  %>% 
    dplyr::select(-c("fid_3"))  %>% 
    rename(land_type = paste0("test",year_folder))
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
    rename(pop_cropland = exact_extract.r..simplified_poly...sum..) %>% 
    mutate(year = year_folder)
  return(extract)
  
}))
save(pop_cropland_extracted_region_level_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_cropland_extracted_region_level_1deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.5 degree

# obtain the uban geom within each cell
population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23] #choose years only after 2012

pop_urban_extracted_region_level_0_5deg <- do.call(rbind, mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  year_folder <- str_extract(filename, "\\d{4}")
  
  simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_urban_inters_id_0_5deg_", year_folder, ".gpkg"))  %>% 
    dplyr::select(-c("fid_3"))  %>% 
    rename(land_type = paste0("test",year_folder))
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
    rename(pop_urban = exact_extract.r..simplified_poly...sum..) %>% 
    mutate(year = year_folder)
  return(extract)
  
}))
save(pop_urban_extracted_region_level_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_urban_extracted_region_level_0_5deg.RData")


# obtain the cropland geom within each cell

pop_cropland_extracted_region_level_0_5deg <- do.call(rbind, mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  year_folder <- str_extract(filename, "\\d{4}")
  
  simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_cropland_inters_id_0_5deg_", year_folder, ".gpkg"))  %>% 
    dplyr::select(-c("fid_3"))  %>% 
    rename(land_type = paste0("test",year_folder))
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
    rename(pop_cropland = exact_extract.r..simplified_poly...sum..) %>% 
    mutate(year = year_folder)
  return(extract)
  
}))
save(pop_cropland_extracted_region_level_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_cropland_extracted_region_level_0_5deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.25 degree

# obtain the uban geom within each cell
population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23] #choose years only after 2012

pop_urban_extracted_region_level_0_25deg <- do.call(rbind, mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  year_folder <- str_extract(filename, "\\d{4}")
  
  simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_urban_inters_id_0_25deg_", year_folder, ".gpkg"))  %>% 
    dplyr::select(-c("fid_3"))  %>% 
    rename(land_type = paste0("test",year_folder))
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
    rename(pop_urban = exact_extract.r..simplified_poly...sum..) %>% 
    mutate(year = year_folder)
  return(extract)
  
}))
save(pop_urban_extracted_region_level_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_urban_extracted_region_level_0_25deg.RData")


# obtain the cropland geom within each cell

pop_cropland_extracted_region_level_0_25deg <- do.call(rbind, mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  year_folder <- str_extract(filename, "\\d{4}")
  
  simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_cropland_inters_id_0_25deg_", year_folder, ".gpkg"))  %>% 
    dplyr::select(-c("fid_3"))  %>% 
    rename(land_type = paste0("test",year_folder))
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
    rename(pop_cropland = exact_extract.r..simplified_poly...sum..) %>% 
    mutate(year = year_folder)
  return(extract)
  
}))
save(pop_cropland_extracted_region_level_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_cropland_extracted_region_level_0_25deg.RData")

