# --------------------------------- Task Summary --------------------------------- #
# Combine NTL data from all years and layers into a single file.
# -------------------------------------------------------------------------------- #

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

# 1deg
# Now combine all years files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
NTL_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_year_sep/NTL_extracted_1deg_1", year, ".RData"))
          extract_temp1 <- extract %>%
             mutate(year = as.integer(year))  %>% 
              rename(NTL_snow_covered_period = NTL)

          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_year_sep/NTL_extracted_1deg_2", year, ".RData"))
          extract_temp2 <- extract 

          extract_temp3 <- extract_temp1  %>% 
                           mutate(NTL_snow_free_period = extract_temp2$NTL)

          if (is.null(NTL_full)) {
              NTL_full <- extract_temp3
          } else {
              NTL_full <- bind_rows(NTL_full, extract_temp3)
          }
}

NTL_full_1deg <- NTL_full

save(NTL_full_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_full_1deg.RData")

# 0_5 degree
# Now combine all years files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
NTL_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_year_sep/NTL_extracted_0_5deg_1", year, ".RData"))
          extract_temp1 <- extract %>%
             mutate(year = as.integer(year))  %>% 
              rename(NTL_snow_covered_period = NTL)

          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_year_sep/NTL_extracted_0_5deg_2", year, ".RData"))
          extract_temp2 <- extract 

          extract_temp3 <- extract_temp1  %>% 
                           mutate(NTL_snow_free_period = extract_temp2$NTL)

          if (is.null(NTL_full)) {
              NTL_full <- extract_temp3
          } else {
              NTL_full <- bind_rows(NTL_full, extract_temp3)
          }
}

NTL_full_0_5deg <- NTL_full
save(NTL_full_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_full_0_5deg.RData")

# 0_25 degree
# Now combine all years files
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
NTL_full <- NULL
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_year_sep/NTL_extracted_0_25deg_1", year, ".RData"))
          extract_temp1 <- extract %>%
             mutate(year = as.integer(year))  %>% 
              rename(NTL_snow_covered_period = NTL)

          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_year_sep/NTL_extracted_0_25deg_2", year, ".RData"))
          extract_temp2 <- extract 

          extract_temp3 <- extract_temp1  %>% 
                           mutate(NTL_snow_free_period = extract_temp2$NTL)

          if (is.null(NTL_full)) {
              NTL_full <- extract_temp3
          } else {
              NTL_full <- bind_rows(NTL_full, extract_temp3)
          }
}

NTL_full_0_25deg <- NTL_full
save(NTL_full_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_full_0_25deg.RData")
