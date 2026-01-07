# --------------------------------- Task Summary --------------------------------- #
# Retrieve the CO2 bio values for each cell.
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
library(stringr)

# ------------------------------------------------------------------------------------------------------------
# 1 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/CO2_bio_specific_sectors"
sectors <- list.dirs(path = data_folder, full.names = FALSE, recursive = FALSE)

combined_results <- data.frame()

# Function to extract CO2_bio data for a given sector
extract_sector_data <- function(sector) {
  sector_path <- file.path(data_folder, sector)
  files <- list.files(path = sector_path, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
  
  emi_extracted <- lapply(files, FUN = function(filename) {
    r <- rast(filename)
    
    extract <- exact_extract(r, simplified_poly, 'sum', append_cols = c("id", "iso", "cell_id")) %>%
      rename(CO2_bio = sum) %>%
      mutate(year = as.numeric(str_extract(str_extract(filename, "_\\d{4}_"), "\\d{4}")))
    
    return(extract)

  }) %>% do.call('rbind', .)
  
  # Rename the CO2_bio column to include the sector name
  emi_extracted <- emi_extracted %>%
    rename(!!paste0("CO2_bio_", sector) := CO2_bio)
  
  return(emi_extracted)
}

# Loop through each sector and extract the data
for (sector in sectors) {
  sector_data <- extract_sector_data(sector)
  
  if (nrow(combined_results) == 0) {
    combined_results <- sector_data
  } else {
    combined_results <- combined_results %>%
      full_join(sector_data, by = c("id", "iso", "cell_id", "year"))
  }
}

CO2_bio_full_1deg <- combined_results
                      
save(CO2_bio_full_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_bio_full_1deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.5 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/CO2_bio_specific_sectors"
sectors <- list.dirs(path = data_folder, full.names = FALSE, recursive = FALSE)

combined_results <- data.frame()

# Function to extract CO2_bio data for a given sector
extract_sector_data <- function(sector) {
  sector_path <- file.path(data_folder, sector)
  files <- list.files(path = sector_path, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
  
  emi_extracted <- lapply(files, FUN = function(filename) {
    r <- rast(filename)
    
    extract <- exact_extract(r, simplified_poly, 'sum', append_cols = c("id", "iso", "cell_id", "subcell_id")) %>%
      rename(CO2_bio = sum) %>%
      mutate(year = as.numeric(str_extract(str_extract(filename, "_\\d{4}_"), "\\d{4}")))
    
    return(extract)

  }) %>% do.call('rbind', .)
  
  # Rename the CO2_bio column to include the sector name
  emi_extracted <- emi_extracted %>%
    rename(!!paste0("CO2_bio_", sector) := CO2_bio)
  
  return(emi_extracted)
}

# Loop through each sector and extract the data
for (sector in sectors) {
  sector_data <- extract_sector_data(sector)
  
  if (nrow(combined_results) == 0) {
    combined_results <- sector_data
  } else {
    combined_results <- combined_results %>%
      full_join(sector_data, by = c("id", "iso", "cell_id", "subcell_id", "year"))
  }
}

CO2_bio_full_0_5deg <- combined_results
                      
save(CO2_bio_full_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_bio_full_0_5deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.25 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/CO2_bio_specific_sectors"
sectors <- list.dirs(path = data_folder, full.names = FALSE, recursive = FALSE)

combined_results <- data.frame()

# Function to extract CO2_bio data for a given sector
extract_sector_data <- function(sector) {
  sector_path <- file.path(data_folder, sector)
  files <- list.files(path = sector_path, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
  
  emi_extracted <- lapply(files, FUN = function(filename) {
    r <- rast(filename)
    
    extract <- exact_extract(r, simplified_poly, 'sum', append_cols = c("id", "iso", "cell_id", "subcell_id", "subcell_id_0_25")) %>%
      rename(CO2_bio = sum) %>%
      mutate(year = as.numeric(str_extract(str_extract(filename, "_\\d{4}_"), "\\d{4}")))
    
    return(extract)

  }) %>% do.call('rbind', .)
  
  # Rename the CO2_bio column to include the sector name
  emi_extracted <- emi_extracted %>%
    rename(!!paste0("CO2_bio_", sector) := CO2_bio)
  
  return(emi_extracted)
}

# Loop through each sector and extract the data
for (sector in sectors) {
  sector_data <- extract_sector_data(sector)
  
  if (nrow(combined_results) == 0) {
    combined_results <- sector_data
  } else {
    combined_results <- combined_results %>%
      full_join(sector_data, by = c("id", "iso", "cell_id", "subcell_id", "year", "subcell_id_0_25"))
  }
}

CO2_bio_full_0_25deg <- combined_results
                      
save(CO2_bio_full_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_bio_full_0_25deg.RData")

