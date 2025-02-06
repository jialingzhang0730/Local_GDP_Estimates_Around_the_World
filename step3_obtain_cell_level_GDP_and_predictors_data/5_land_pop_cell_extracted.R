# --------------------------------- Task Summary --------------------------------- #
# Extract the population value for each cell size.
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
library(ncdf4)
library(terra)
library(exactextractr)
library(gdata)
library(units)
library(tidyverse)

# ------------------------------------------------------------------------------------------------------------
# 1 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 


tic("Population")

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:22] #choose years only after 2012

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
              rename(pop = exact_extract.r..simplified_poly...sum..)
  save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_year_sep/land_pop_extracted_1deg", as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021")
land_pop_full <- NULL 
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_year_sep/land_pop_extracted_1deg", year, ".RData"))
          extract <- extract %>%
              mutate(year = as.integer(year))
                
          if (is.null(land_pop_full)) {
              land_pop_full <- extract
          } else {
              land_pop_full <- bind_rows(land_pop_full, extract)
          }
}
land_pop_extracted_region_level_1deg <- land_pop_full  %>% 
                      replace_na(list(pop = 0)) 

save(land_pop_extracted_region_level_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_1deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.5 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 


tic("Population")

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:22] #choose years only after 2012

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
              rename(pop = exact_extract.r..simplified_poly...sum..)
  save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_year_sep/land_pop_extracted_0_5deg", as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021")
land_pop_full <- NULL 
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_year_sep/land_pop_extracted_0_5deg", year, ".RData"))
          extract <- extract %>%
              mutate(year = as.integer(year))
                
          if (is.null(land_pop_full)) {
              land_pop_full <- extract
          } else {
              land_pop_full <- bind_rows(land_pop_full, extract)
          }
}
land_pop_extracted_region_level_0_5deg <- land_pop_full  %>% 
                      replace_na(list(pop = 0)) 

save(land_pop_extracted_region_level_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_5deg.RData")

# ------------------------------------------------------------------------------------------------------------
# 0.25 degree

simplified_poly <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg")  %>% 
                    dplyr::select(-c("fid_2")) 


tic("Population")

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:22] #choose years only after 2012

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
              rename(pop = exact_extract.r..simplified_poly...sum..)
  save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_year_sep/land_pop_extracted_0_25deg", as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021")
land_pop_full <- NULL 
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_year_sep/land_pop_extracted_0_25deg", year, ".RData"))
          extract <- extract %>%
              mutate(year = as.integer(year))
                
          if (is.null(land_pop_full)) {
              land_pop_full <- extract
          } else {
              land_pop_full <- bind_rows(land_pop_full, extract)
          }
}
land_pop_extracted_region_level_0_25deg <- land_pop_full  %>% 
                      replace_na(list(pop = 0)) 

save(land_pop_extracted_region_level_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_25deg.RData")

