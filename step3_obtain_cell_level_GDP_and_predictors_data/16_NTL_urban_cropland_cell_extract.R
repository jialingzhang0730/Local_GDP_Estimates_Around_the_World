# --------------- Task --------------- #
# This file is to extract nighttime light (NTL) emissions exclusively from the urban areas or from the cropland areas. 
# ------------------------------------ #

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

# argument value
args <- commandArgs(trailingOnly = TRUE)
print(as.numeric(args[1]))

# Dynamically set working directory based on PBS environment
if (!is.null(Sys.getenv("PBS_O_WORKDIR")) && Sys.getenv("PBS_O_WORKDIR") != "") {
  setwd(Sys.getenv("PBS_O_WORKDIR"))
}

# # ----------------
# # Now extract NTL from urban and cropland 1deg 

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NTL_VNP46A4"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre[as.numeric(args[1])]

tic("extracting NTL values")

NTL <- mclapply(year_folders, mc.cores = 1, FUN= function(year_folder){
    v_pre <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_temp_files/test",year_folder,".vrt"))

    v <- setNames(v_pre, c("AllAngle_Composite_Snow_Covered", "AllAngle_Composite_Snow_Covered_Num",
                 "AllAngle_Composite_Snow_Covered_Quality", "AllAngle_Composite_Snow_Covered_Std",     
                 "AllAngle_Composite_Snow_Free", "AllAngle_Composite_Snow_Free_Num",        
                 "AllAngle_Composite_Snow_Free_Quality", "AllAngle_Composite_Snow_Free_Std",        
                 "DNB_Platform", "Land_Water_Mask",                         
                 "NearNadir_Composite_Snow_Covered", "NearNadir_Composite_Snow_Covered_Num",    
                 "NearNadir_Composite_Snow_Covered_Quality", "NearNadir_Composite_Snow_Covered_Std",    
                 "NearNadir_Composite_Snow_Free", "NearNadir_Composite_Snow_Free_Num",       
                 "NearNadir_Composite_Snow_Free_Quality", "NearNadir_Composite_Snow_Free_Std",       
                 "OffNadir_Composite_Snow_Covered", "OffNadir_Composite_Snow_Covered_Num",     
                 "OffNadir_Composite_Snow_Covered_Quality", "OffNadir_Composite_Snow_Covered_Std",     
                 "OffNadir_Composite_Snow_Free", "OffNadir_Composite_Snow_Free_Num",        
                 "OffNadir_Composite_Snow_Free_Quality", "OffNadir_Composite_Snow_Free_Std"))
  
    vv <- v[[c(1,5)]] # only obtain snow_covered and snow_free 
    
    # Function to apply mask and set overlapping pixels to 0
    gas_flare_sq <- st_read("step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot_sf_square_0_2deg.gpkg")  %>% 
        filter(Year == year_folder)

    # first obtain which pixel in vv are overlapped with the gas_flare_sq
    coverage_fraction <- exactextractr::exact_extract(vv, gas_flare_sq, coverage_area = FALSE, include_cell = TRUE)

    # Extract the unique cell indices from the results (if not unique already)
    all_indices <- unlist(lapply(coverage_fraction, function(x) x$cell))
    unique_indices <- unique(all_indices)

    # obtain the uban geom within each cell
    simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_urban_inters_id_1deg_", year_folder, ".gpkg"))  %>% 
                    dplyr::select(-c("fid_3"))  %>% 
                    rename(land_type = paste0("test",year_folder))

    # now extract NTL values
    mclapply(c(1,2), mc.cores = 2, FUN = function(i){
        extract <- exact_extract(vv[[i]], simplified_poly, coverage_area = T,
                           summarize_df = T, include_cols = c("cell_id", "iso", "id", "land_type"), include_cell = TRUE,
                           fun = function(df_in){
                            df_out <- df_in %>%
                                      mutate(value = ifelse(is.na(value) | value == 65535 | cell %in% unique_indices, 0, value))  %>% # value 65535 refers to ocean; and mask those NTL from gas flaring places
                                      group_by(cell_id, iso, id, land_type) %>%
                                      summarize(NTL = sum(coverage_area * value), .groups = "drop")
                            return(df_out)
                           })
        save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_cropland_year_sep/NTL_urban_extracted_1deg_", i, year_folder, ".RData"))
    })

    # obtain the cropland geom within each cell
    simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_cropland_inters_id_1deg_", year_folder, ".gpkg"))  %>% 
                    dplyr::select(-c("fid_3"))  %>% 
                    rename(land_type = paste0("test",year_folder))

    # now extract NTL values
    mclapply(c(1,2), mc.cores = 2, FUN = function(i){
        extract <- exact_extract(vv[[i]], simplified_poly, coverage_area = T,
                           summarize_df = T, include_cols = c("cell_id", "iso", "id", "land_type"), include_cell = TRUE,
                           fun = function(df_in){
                            df_out <- df_in %>%
                                      mutate(value = ifelse(is.na(value) | value == 65535 | cell %in% unique_indices, 0, value))  %>% # value 65535 refers to ocean; and mask those NTL from gas flaring places
                                      group_by(cell_id, iso, id, land_type) %>%
                                      summarize(NTL = sum(coverage_area * value), .groups = "drop")
                            return(df_out)
                           })
        save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_cropland_year_sep/NTL_cropland_extracted_1deg_", i, year_folder, ".RData"))
    })
})
toc()

# # ----------------
# # Now extract NTL from urban and cropland 0.5deg 

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NTL_VNP46A4"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre[as.numeric(args[1])]

tic("extracting NTL values")

NTL <- mclapply(year_folders, mc.cores = 1, FUN= function(year_folder){
    v_pre <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_temp_files/test",year_folder,".vrt"))

    v <- setNames(v_pre, c("AllAngle_Composite_Snow_Covered", "AllAngle_Composite_Snow_Covered_Num",
                 "AllAngle_Composite_Snow_Covered_Quality", "AllAngle_Composite_Snow_Covered_Std",     
                 "AllAngle_Composite_Snow_Free", "AllAngle_Composite_Snow_Free_Num",        
                 "AllAngle_Composite_Snow_Free_Quality", "AllAngle_Composite_Snow_Free_Std",        
                 "DNB_Platform", "Land_Water_Mask",                         
                 "NearNadir_Composite_Snow_Covered", "NearNadir_Composite_Snow_Covered_Num",    
                 "NearNadir_Composite_Snow_Covered_Quality", "NearNadir_Composite_Snow_Covered_Std",    
                 "NearNadir_Composite_Snow_Free", "NearNadir_Composite_Snow_Free_Num",       
                 "NearNadir_Composite_Snow_Free_Quality", "NearNadir_Composite_Snow_Free_Std",       
                 "OffNadir_Composite_Snow_Covered", "OffNadir_Composite_Snow_Covered_Num",     
                 "OffNadir_Composite_Snow_Covered_Quality", "OffNadir_Composite_Snow_Covered_Std",     
                 "OffNadir_Composite_Snow_Free", "OffNadir_Composite_Snow_Free_Num",        
                 "OffNadir_Composite_Snow_Free_Quality", "OffNadir_Composite_Snow_Free_Std"))
  
    vv <- v[[c(1,5)]] # only obtain snow_covered and snow_free 
    
    # Function to apply mask and set overlapping pixels to 0
    gas_flare_sq <- st_read("step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot_sf_square_0_2deg.gpkg")  %>% 
        filter(Year == year_folder)

    # first obtain which pixel in vv are overlapped with the gas_flare_sq
    coverage_fraction <- exactextractr::exact_extract(vv, gas_flare_sq, coverage_area = FALSE, include_cell = TRUE)

    # Extract the unique cell indices from the results (if not unique already)
    all_indices <- unlist(lapply(coverage_fraction, function(x) x$cell))
    unique_indices <- unique(all_indices)

    # obtain the uban geom within each cell
    simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_urban_inters_id_0_5deg_", year_folder, ".gpkg"))  %>% 
                    dplyr::select(-c("fid_3"))  %>% 
                    rename(land_type = paste0("test",year_folder))

    # now extract NTL values
    mclapply(c(1,2), mc.cores = 2, FUN = function(i){
        extract <- exact_extract(vv[[i]], simplified_poly, coverage_area = T,
                           summarize_df = T, include_cols = c("cell_id", "subcell_id","iso", "id", "land_type"), include_cell = TRUE,
                           fun = function(df_in){
                            df_out <- df_in %>%
                                      mutate(value = ifelse(is.na(value) | value == 65535 | cell %in% unique_indices, 0, value))  %>% # value 65535 refers to ocean; and mask those NTL from gas flaring places
                                      group_by(cell_id, subcell_id, iso, id, land_type) %>%
                                      summarize(NTL = sum(coverage_area * value), .groups = "drop")
                            return(df_out)
                           })
        save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_cropland_year_sep/NTL_urban_extracted_0_5deg_", i, year_folder, ".RData"))
    })

    # obtain the cropland geom within each cell
    simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_cropland_inters_id_0_5deg_", year_folder, ".gpkg"))  %>% 
                    dplyr::select(-c("fid_3"))  %>% 
                    rename(land_type = paste0("test",year_folder))

    # now extract NTL values
    mclapply(c(1,2), mc.cores = 2, FUN = function(i){
        extract <- exact_extract(vv[[i]], simplified_poly, coverage_area = T,
                           summarize_df = T, include_cols = c("cell_id", "subcell_id","iso", "id", "land_type"), include_cell = TRUE,
                           fun = function(df_in){
                            df_out <- df_in %>%
                                      mutate(value = ifelse(is.na(value) | value == 65535 | cell %in% unique_indices, 0, value))  %>% # value 65535 refers to ocean; and mask those NTL from gas flaring places
                                      group_by(cell_id, subcell_id, iso, id, land_type) %>%
                                      summarize(NTL = sum(coverage_area * value), .groups = "drop")
                            return(df_out)
                           })
        save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_cropland_year_sep/NTL_cropland_extracted_0_5deg_", i, year_folder, ".RData"))
    })
})
toc()

# ----------------
# Now extract NTL from urban and cropland 0.25deg 

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/NTL_VNP46A4"
year_folders_pre <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
year_folders <- year_folders_pre[as.numeric(args[1])]

tic("extracting NTL values")

NTL <- mclapply(year_folders, mc.cores = 1, FUN= function(year_folder){
    v_pre <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_temp_files/test",year_folder,".vrt"))

    v <- setNames(v_pre, c("AllAngle_Composite_Snow_Covered", "AllAngle_Composite_Snow_Covered_Num",
                 "AllAngle_Composite_Snow_Covered_Quality", "AllAngle_Composite_Snow_Covered_Std",     
                 "AllAngle_Composite_Snow_Free", "AllAngle_Composite_Snow_Free_Num",        
                 "AllAngle_Composite_Snow_Free_Quality", "AllAngle_Composite_Snow_Free_Std",        
                 "DNB_Platform", "Land_Water_Mask",                         
                 "NearNadir_Composite_Snow_Covered", "NearNadir_Composite_Snow_Covered_Num",    
                 "NearNadir_Composite_Snow_Covered_Quality", "NearNadir_Composite_Snow_Covered_Std",    
                 "NearNadir_Composite_Snow_Free", "NearNadir_Composite_Snow_Free_Num",       
                 "NearNadir_Composite_Snow_Free_Quality", "NearNadir_Composite_Snow_Free_Std",       
                 "OffNadir_Composite_Snow_Covered", "OffNadir_Composite_Snow_Covered_Num",     
                 "OffNadir_Composite_Snow_Covered_Quality", "OffNadir_Composite_Snow_Covered_Std",     
                 "OffNadir_Composite_Snow_Free", "OffNadir_Composite_Snow_Free_Num",        
                 "OffNadir_Composite_Snow_Free_Quality", "OffNadir_Composite_Snow_Free_Std"))
  
    vv <- v[[c(1,5)]] # only obtain snow_covered and snow_free 
    
    # Function to apply mask and set overlapping pixels to 0
    gas_flare_sq <- st_read("step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot_sf_square_0_2deg.gpkg")  %>% 
        filter(Year == year_folder)

    # first obtain which pixel in vv are overlapped with the gas_flare_sq
    coverage_fraction <- exactextractr::exact_extract(vv, gas_flare_sq, coverage_area = FALSE, include_cell = TRUE)

    # Extract the unique cell indices from the results (if not unique already)
    all_indices <- unlist(lapply(coverage_fraction, function(x) x$cell))
    unique_indices <- unique(all_indices)

    # obtain the uban geom within each cell
    simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_urban_inters_id_0_25deg_", year_folder, ".gpkg"))  %>% 
                    dplyr::select(-c("fid_3"))  %>% 
                    rename(land_type = paste0("test",year_folder))

    # now extract NTL values
    mclapply(c(1,2), mc.cores = 2, FUN = function(i){
        extract <- exact_extract(vv[[i]], simplified_poly, coverage_area = T,
                           summarize_df = T, include_cols = c("cell_id", "subcell_id", "subcell_id_0_25","iso", "id", "land_type"), include_cell = TRUE,
                           fun = function(df_in){
                            df_out <- df_in %>%
                                      mutate(value = ifelse(is.na(value) | value == 65535 | cell %in% unique_indices, 0, value))  %>% # value 65535 refers to ocean; and mask those NTL from gas flaring places
                                      group_by(cell_id, subcell_id, subcell_id_0_25, iso, id, land_type) %>%
                                      summarize(NTL = sum(coverage_area * value), .groups = "drop")
                            return(df_out)
                           })
        save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_cropland_year_sep/NTL_urban_extracted_0_25deg_", i, year_folder, ".RData"))
    })

    # obtain the cropland geom within each cell
    simplified_poly <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_cropland_inters_id_0_25deg_", year_folder, ".gpkg"))  %>% 
                    dplyr::select(-c("fid_3"))  %>% 
                    rename(land_type = paste0("test",year_folder))

    # now extract NTL values
    mclapply(c(1,2), mc.cores = 2, FUN = function(i){
        extract <- exact_extract(vv[[i]], simplified_poly, coverage_area = T,
                           summarize_df = T, include_cols = c("cell_id", "subcell_id", "subcell_id_0_25","iso", "id", "land_type"), include_cell = TRUE,
                           fun = function(df_in){
                            df_out <- df_in %>%
                                      mutate(value = ifelse(is.na(value) | value == 65535 | cell %in% unique_indices, 0, value))  %>% # value 65535 refers to ocean; and mask those NTL from gas flaring places
                                      group_by(cell_id, subcell_id, subcell_id_0_25, iso, id, land_type) %>%
                                      summarize(NTL = sum(coverage_area * value), .groups = "drop")
                            return(df_out)
                           })
        save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_cropland_year_sep/NTL_cropland_extracted_0_25deg_", i, year_folder, ".RData"))
    })
})
toc()
