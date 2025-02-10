# --------------------------------- Task Summary --------------------------------- #
# Prepare the NTL data prior to extracting cell-level NTL values.
# Execute the .sh file on the server to utilize its larger memory capacity and increased 
#   number of cores, enabling more efficient parallel processing for the task.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
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

# ------------------------------------------------------------------------------------------------------------
# obtain NTL temp files: aggregate original NTL data (several pieces) into one big map
# the original NTL data are in linear lat/lon grid, so we do not need reproject

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NTL_VNP46A4"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre

tic("extracting NTL values")

NTL <- mclapply(year_folders, mc.cores = 2, FUN= function(year_folder){
   folder_path <- file.path(data_folder, year_folder, "001")
   h5_files <- list.files(folder_path, pattern = "\\.h5$", full.names = TRUE)

       tic("list time")
       mclapply(1:length(h5_files), mc.cores = 5, function(i){
           h5_file <- h5_files[i]
           lon <- h5read(h5_file, "/HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lon")
           lat <- h5read(h5_file, "/HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lat")
    
           a <- rast(h5_file)
           ext(a) <- c(min(lon), max(lon), min(lat), max(lat))
           crs(a) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
           writeRaster(a, paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_temp_files/test", i,"_",year_folder,".tif"), overwrite = TRUE)
       })
       toc()

   tif_files <- paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_temp_files/test", 1:length(h5_files), "_",year_folder, ".tif")
   vrt_file <- paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_temp_files/test",year_folder,".vrt")
   v_pre <- terra::vrt(tif_files, vrt_file, overwrite=TRUE)
})
