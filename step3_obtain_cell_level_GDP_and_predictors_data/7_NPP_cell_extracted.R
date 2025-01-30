# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# Extract NPP values for each cell size
# NPP data come from "MOD17A3HGF", units = "kgC/m²/year"
# Execute the .sh file on the server to utilize its larger memory capacity and increased number of cores, enabling more efficient parallel processing for the task.
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(raster)
library(terra)
library(dplyr)
library(parallel)
library(rhdf5)
library(tictoc)
library(tiff)
library(foreach)
library(iterators)
library(doParallel)
library(exactextractr)
library(future)
library(future.apply)
library(furrr)
library(sf)

# Dynamically set working directory based on PBS environment
if (!is.null(Sys.getenv("PBS_O_WORKDIR")) && Sys.getenv("PBS_O_WORKDIR") != "") {
  setwd(Sys.getenv("PBS_O_WORKDIR"))
}

# ------------------------------------------------------------------------------------------------------------
# obtain NPP temp files: aggregate original NPP data (several pieces) into one big map and reproject from sino to WGS84

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NPP_v061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

NPP <- mclapply(year_folders, mc.cores = 2, FUN= function(year_folder){
   folder_path <- file.path(data_folder, year_folder)
   hdf_files <- list.files(folder_path, pattern = "\\.hdf$", full.names = TRUE)

       tic("list time")
       mclapply(1:length(hdf_files), mc.cores = 5, function(i){
           hdf_file <- hdf_files[i]
           a <- rast(hdf_file)[[2]]  %>% # get the second layer "Npp_500m": net primary productivity
               project("epsg:4326") # project the "sinu" to be "epsg:4326"
           writeRaster(a, paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_temp_files/test", i,"_",year_folder,".tif"), overwrite = TRUE)
       })
       toc()

   tif_files <- paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_temp_files/test", 1:length(hdf_files), "_",year_folder, ".tif")
   vrt_file <- paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_temp_files/test",year_folder,".vrt")
   v <- terra::vrt(tif_files, vrt_file, overwrite=TRUE)
})

# ------------------------------------------------------------------------------------------------------------
# Now extract NPP values
# 1 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2"))

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NPP_v061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

NPP <- mclapply(year_folders, mc.cores = 11, FUN= function(year_folder){

    v <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_temp_files/test", year_folder,".vrt"))

    tic("extract") 
    extract <- exact_extract(v, simplified_poly, coverage_area = T, progress = T,
                             include_cols = c("id", "iso", "cell_id"), summarize_df = T, 
                             fun = function(df_in){
                                
                                out_df <- df_in  %>% 
                                    mutate(value = ifelse(value >= 3.2760 | value < -3 | is.na(value), 0, value))  %>% # those out of range values are not real "values" according to the data's user guide
                                    group_by(cell_id, iso, id)  %>% 
                                    summarize(NPP = sum(value * coverage_area), .groups = "drop")

                                return(out_df)
                           })
    toc()
 
    save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_year_sep/NPP_extracted_1deg", year_folder, ".RData"))

})

# Now combine all years files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
NPP_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_year_sep/NPP_extracted_1deg", year, ".RData"))
          extract <- extract %>%
              mutate(year = as.integer(year))
                
          if (is.null(NPP_full)) {
              NPP_full <- extract
          } else {
              NPP_full <- bind_rows(NPP_full, extract)
          }
}

NPP_full_1deg <- NPP_full
save(NPP_full_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_full_1deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.5 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NPP_v061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

NPP <- mclapply(year_folders, mc.cores = 11, FUN= function(year_folder){

    v <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_temp_files/test", year_folder,".vrt"))

    tic("extract") 
    extract <- exact_extract(v, simplified_poly, coverage_area = T, progress = T,
                             include_cols = c("id", "iso", "cell_id", "subcell_id"), summarize_df = T, 
                             fun = function(df_in){
                                
                                out_df <- df_in  %>% 
                                    mutate(value = ifelse(value >= 3.2760 | value < -3 | is.na(value), 0, value))  %>% # those out of range values are not real "values" according to the data's user guide
                                    group_by(subcell_id, cell_id, iso, id)  %>% 
                                    summarize(NPP = sum(value * coverage_area), .groups = "drop")

                                return(out_df)
                           })
    toc()
 
    save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_year_sep/NPP_extracted_0_5deg", year_folder, ".RData"))

})

# Now combine all years files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
NPP_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_year_sep/NPP_extracted_0_5deg", year, ".RData"))
          extract <- extract %>%
              mutate(year = as.integer(year))
                
          if (is.null(NPP_full)) {
              NPP_full <- extract
          } else {
              NPP_full <- bind_rows(NPP_full, extract)
          }
}

NPP_full_0_5deg <- NPP_full
save(NPP_full_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_full_0_5deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.25 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2"))  

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NPP_v061"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

NPP <- mclapply(year_folders, mc.cores = 11, FUN= function(year_folder){

    v <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_temp_files/test", year_folder,".vrt"))

    tic("extract") 
    extract <- exact_extract(v, simplified_poly, coverage_area = T, progress = T,
                             include_cols = c("id", "iso", "cell_id", "subcell_id", "subcell_id_0_25"), summarize_df = T, 
                             fun = function(df_in){
                                
                                out_df <- df_in  %>% 
                                    mutate(value = ifelse(value >= 3.2760 | value < -3 | is.na(value), 0, value))  %>% # those out of range values are not real "values" according to the data's user guide
                                    group_by(subcell_id_0_25, subcell_id, cell_id, iso, id)  %>% 
                                    summarize(NPP = sum(value * coverage_area), .groups = "drop")

                                return(out_df)
                           })
    toc()
 
    save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_year_sep/NPP_extracted_0_25deg", year_folder, ".RData"))

})

# Now combine all years files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
NPP_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_year_sep/NPP_extracted_0_25deg", year, ".RData"))
          extract <- extract %>%
              mutate(year = as.integer(year))
                
          if (is.null(NPP_full)) {
              NPP_full <- extract
          } else {
              NPP_full <- bind_rows(NPP_full, extract)
          }
}

NPP_full_0_25deg <- NPP_full
save(NPP_full_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_full_0_25deg.RData")
