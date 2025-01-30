# ------------------------------------------------------------------------------------------------- #
# Task Summary:
# Obtain the population data for each county or subnational region in the training sample
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
rm(list = ls())
gc()

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

# please change to your specific folder
setwd("/share/rossihansberglab/Nightlights_GDP/replication_packages_world_GCP")

# ------------------------------------------------- #        
# read the polygons
simplified_poly <- read_sf("step2_obtain_gdp_data/outputs/training_poly_sample.gpkg")

# ------------------------------------------------- #        
# Obtain training isos county-level population (LandScan)
tic("Population")

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:22] #choose years only 2012-2021

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
              rename(pop = exact_extract.r..simplified_poly...sum..)
  save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_county_pop/land_pop_extracted_", as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021")
land_pop_full <- NULL 
for (year in years){
          load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_county_pop/land_pop_extracted_", year, ".RData"))
          extract <- extract %>%
              mutate(year = as.integer(year))
                
          if (is.null(land_pop_full)) {
              land_pop_full <- extract
          } else {
              land_pop_full <- bind_rows(land_pop_full, extract)
          }
}

# ------------------------------------------------- #        
# Don't forget that Alaska's population should be excluded from USA's national population
# I also double checked that the IMF USA national population does not include US territories, that is what we want

alaska <- read_sf("step2_obtain_gdp_data/outputs/world_poly.gpkg")  %>% 
    filter(iso == "Ala") # remember we assign Alaska a fake iso code "Ala"

alaska_pop <- mclapply(population_files, mc.cores = 5, FUN = function(filename){
  r <- rast(filename)
  extract <- cbind(alaska, exact_extract(r, alaska, 'sum')) %>% 
              rename(pop = exact_extract.r..alaska...sum..)  %>% 
              mutate(year = as.numeric(str_extract(filename, "\\d{4}")))
})  %>% 
    do.call(rbind, .)  %>% 
    as.data.frame()  %>% 
    dplyr::select(-c(geom))  %>% 
    mutate(id = "Ala", iso = "USA") # change the iso name, so that we can change US county's population share below

# let me save it because we will still use alaska's population later
write.csv(alaska_pop, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/alaska_population.csv")

# ------------------------------------------------- # 
# actually we want to get each county's national population share here, so that we can rescale to match the sum with national population
# Thus, what Alaska should affect is the USA county's population share below

land_pop_extracted_train_county <- bind_rows(land_pop_full, alaska_pop)  %>% 
                      group_by(iso, year)  %>% 
                      mutate(pop_share = pop/sum(pop))  %>% 
                      ungroup()  %>% 
                      filter(id != "Ala") # Alaska's data is not included in the training sample, we use it here only to obtain the correct US county national population share

save(land_pop_extracted_train_county, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_train_county.RData")


# Obtain the average areas of those subnational units for each country

sub_area <- land_pop_extracted_train_county %>% 
    filter(year == 2012) %>% 
    mutate(area = st_area(geom)) %>% 
    as.data.frame() %>% 
    dplyr::select(-c(geom)) %>% 
    group_by(iso) %>%
    mutate(avr_area = mean(area)/1000000) %>% # change the unit to km^2
    ungroup() %>% 
    distinct(iso, avr_area)

write.csv(sub_area, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_subnational_area.csv")



