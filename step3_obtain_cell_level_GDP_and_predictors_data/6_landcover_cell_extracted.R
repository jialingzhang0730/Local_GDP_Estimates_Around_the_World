# --------------------------------- Task Summary --------------------------------- #
# Extract various types of landcover areas for each cell.
# Execute the .sh file on the server to utilize its larger memory capacity and increased 
#   number of cores, enabling more efficient parallel processing for the task.
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
library(terra)
library(tidyverse)
library(sp)
library(tidyr)

# Dynamically set working directory based on PBS environment
if (!is.null(Sys.getenv("PBS_O_WORKDIR")) && Sys.getenv("PBS_O_WORKDIR") != "") {
  setwd(Sys.getenv("PBS_O_WORKDIR"))
}

# ------------------------------------------------------------------------------------------------------------
# obtain landcover temp files: aggregate original landcover data (several pieces) into one big map and reproject from sino to WGS84

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/landcover_MCD12Q1V061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

lc_extracted <- mclapply(year_folders, mc.cores = 5, FUN = function(year_folder){

 folder_path <- file.path(data_folder, year_folder)
 h5_files <- list.files(folder_path, pattern = "\\.hdf$", full.names = TRUE)

 tic("list time")
 mclapply(1:length(h5_files), mc.cores = 2, function(i){
   h5_file <- h5_files[i]
   a <- rast(h5_file)[[10]]  %>% # "10" is the layer "LC_Prop2" 
        project("epsg:4326", method = "near") # project it from sino to WGS84
   writeRaster(a, paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test", i,"_",year_folder,".tif"), overwrite = TRUE)

 })
 toc()

 tif_files <- paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test", 1:length(h5_files), "_",year_folder, ".tif")
 vrt_file <- paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test",year_folder,".vrt")
 lc_raster <- terra::vrt(tif_files, vrt_file, overwrite=TRUE) 
})
 

# ------------------------------------------------------------------------------------------------------------
# now extract landcover values
# 1 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2"))  

tic("Landcover")

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/landcover_MCD12Q1V061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

lc_extracted <- mclapply(year_folders, mc.cores = 10, FUN = function(year_folder){

  lc_raster <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test", year_folder,".vrt"))
  
  df_extract <- exact_extract(lc_raster, simplified_poly, coverage_area = T, progress = T, 
                               include_cols = c("id", "iso", "cell_id"), summarize_df = T,
                               fun = function(df_in){
                                 
                                 out_df <- df_in %>% 
                                   mutate(value = ifelse(is.na(value), 3, value)) %>% 
                                   group_by(id, iso, cell_id, value) %>%
                                   summarize(lc_area = sum(coverage_area)/1e6, .groups = "drop")
                                 
                                 return(out_df)
                                 
                               })
    df_extract <- df_extract  %>%                       
    pivot_wider(names_from = value, 
                values_from = lc_area, 
                names_prefix = "class_")  %>% 
    replace(is.na(.), 0) %>% 
    rename(barren = class_1, snow_ice = class_2, water = class_3, urban = class_9, 
           dense_forest = class_10, open_forest = class_20, forest_cropland = class_25, 
           herbaceous = class_30, herbaceous_cropland = class_35, cropland = class_36, shrub = class_40)  %>% 
    mutate(year = as.integer(year_folder))
    
  save(df_extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_year_sep/landcover_extracted_1deg", year_folder, ".RData"))  
  
})
 
toc()

# Now combine those year files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021")
lc_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_year_sep/landcover_extracted_1deg", year, ".RData"))
                
          if (is.null(lc_full)) {
              lc_full <- df_extract
          } else {
              lc_full <- bind_rows(lc_full, df_extract)
          }
}

lc_full_1deg <- lc_full
save(lc_full_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_1deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.5 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

# 1. Landcover -----

tic("Landcover")

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/landcover_MCD12Q1V061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

lc_extracted <- mclapply(year_folders, mc.cores = 10, FUN = function(year_folder){

  lc_raster <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test", year_folder,".vrt"))
  
  df_extract <- exact_extract(lc_raster, simplified_poly, coverage_area = T, progress = T, 
                               include_cols = c("id", "iso", "cell_id", "subcell_id"), summarize_df = T,
                               fun = function(df_in){
                                 
                                 out_df <- df_in %>% 
                                   mutate(value = ifelse(is.na(value), 3, value)) %>% 
                                   group_by(id, iso, cell_id, subcell_id, value) %>%
                                   summarize(lc_area = sum(coverage_area)/1e6, .groups = "drop")
                                 
                                 return(out_df)
                                 
                               })
    df_extract <- df_extract  %>%                       
    pivot_wider(names_from = value, 
                values_from = lc_area, 
                names_prefix = "class_")  %>% 
    replace(is.na(.), 0) %>% 
    rename(barren = class_1, snow_ice = class_2, water = class_3, urban = class_9, 
           dense_forest = class_10, open_forest = class_20, forest_cropland = class_25, 
           herbaceous = class_30, herbaceous_cropland = class_35, cropland = class_36, shrub = class_40)  %>% 
    mutate(year = as.integer(year_folder))
    
  save(df_extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_year_sep/landcover_extracted_0_5deg", year_folder, ".RData"))  
  
})
 
toc()

# Now combine those year files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021")
lc_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_year_sep/landcover_extracted_0_5deg", year, ".RData"))

          if (is.null(lc_full)) {
              lc_full <- df_extract
          } else {
              lc_full <- bind_rows(lc_full, df_extract)
          }
}

lc_full_0_5deg <- lc_full
save(lc_full_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_5deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.25 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

# 1. Landcover -----

tic("Landcover")

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/landcover_MCD12Q1V061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

lc_extracted <- mclapply(year_folders, mc.cores = 10, FUN = function(year_folder){

  lc_raster <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test", year_folder,".vrt"))
  
  df_extract <- exact_extract(lc_raster, simplified_poly, coverage_area = T, progress = T, 
                               include_cols = c("id", "iso", "cell_id", "subcell_id", "subcell_id_0_25"), summarize_df = T,
                               fun = function(df_in){
                                 
                                 out_df <- df_in %>% 
                                   mutate(value = ifelse(is.na(value), 3, value)) %>% 
                                   group_by(id, iso, cell_id, subcell_id, subcell_id_0_25, value) %>%
                                   summarize(lc_area = sum(coverage_area)/1e6, .groups = "drop")
                                 
                                 return(out_df)
                                 
                               })
    df_extract <- df_extract  %>%                       
    pivot_wider(names_from = value, 
                values_from = lc_area, 
                names_prefix = "class_")  %>% 
    replace(is.na(.), 0) %>% 
    rename(barren = class_1, snow_ice = class_2, water = class_3, urban = class_9, 
           dense_forest = class_10, open_forest = class_20, forest_cropland = class_25, 
           herbaceous = class_30, herbaceous_cropland = class_35, cropland = class_36, shrub = class_40)  %>% 
    mutate(year = as.integer(year_folder))
    
  save(df_extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_year_sep/landcover_extracted_0_25deg", year_folder, ".RData"))  
  
})
 
toc()

# Now combine those year files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021")
lc_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_year_sep/landcover_extracted_0_25deg", year, ".RData"))

          if (is.null(lc_full)) {
              lc_full <- df_extract
          } else {
              lc_full <- bind_rows(lc_full, df_extract)
          }
}

lc_full_0_25deg <- lc_full
save(lc_full_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_25deg.RData")
