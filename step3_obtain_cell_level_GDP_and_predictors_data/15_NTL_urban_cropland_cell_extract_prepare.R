# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# This file is to isolate the urban and cropland geometries within each cell. 
# Then later we can extract nighttime light (NTL) emissions exclusively from the urban areas or from the cropland areas. 
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
library(raster)
library(qgisprocess)

# Set working directory
setwd("/share/rossihansberglab/Nightlights_GDP/replication_packages_world_GCP")

# ------------------------------------------------------------------------------------------------------------
# Convert the entire landcover raster to 11 polygons, each stand for a landcover type.

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/landcover_MCD12Q1V061"
year_folders <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)

lc_extracted <- mclapply(year_folders, mc.cores = 2, FUN = function(year_folder){
    lc_file <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test", year_folder,".vrt"))
    # Function to convert raster cell to sf object
    lc_polygons <- as.polygons(lc_file, values = TRUE, na.rm = TRUE) 
    lc_sf <- lc_polygons  %>% st_as_sf(lc_polygons)
    # save the landcover polygons
    st_write(lc_sf, paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_poly/lc_polygons_", year_folder, ".gpkg"))
})

# intersect cell polygons with urban polygons and cropland polygons
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]
    
    input <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_poly/lc_polygons_", year, ".gpkg")) %>% 
        filter(get(paste0("test", year)) == 9) 
      
    qgis_run_algorithm(
        "native:fixgeometries",
        INPUT = input,
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"),
        .quiet = FALSE
    )
  
    input <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_poly/lc_polygons_", year, ".gpkg")) %>% 
        filter(get(paste0("test", year)) %in% c(25,35,36))

    qgis_run_algorithm(
        "native:fixgeometries",
        INPUT = input,
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"),
        .quiet = FALSE
    )

})


mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_urban_inters_id_1deg_", year, ".gpkg")
    )


    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_cropland_inters_id_1deg_", year, ".gpkg")
    )
})


mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_urban_inters_id_0_5deg_", year, ".gpkg")
    )


    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_cropland_inters_id_0_5deg_", year, ".gpkg")
    )
})


mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_urban_inters_id_0_25deg_", year, ".gpkg")
    )


    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_cropland_inters_id_0_25deg_", year, ".gpkg")
    )
})