# --------------------------------- Task Summary --------------------------------- #
# Retrieve the population data for each county or subnational region included 
#   in the training sample.
# -------------------------------------------------------------------------------- #

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

# read the polygons
simplified_poly <- read_sf("step2_obtain_gdp_data/outputs/training_poly_sample.gpkg")

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

# Ensure that Alaska's population is excluded from the USA's national population.
# It seems that the IMF's USA national population data does not include US territories, which aligns with our requirements.

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
    mutate(id = "Ala", iso = "USA") # allow for adjustments to the population share of US counties below

# Save Alaska's population, as it will be used later in the analysis.
write.csv(alaska_pop, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/alaska_population.csv")

# The goal is to obtain each county's national population share to ensure the sum matches the national population.
# Thus, Alaska’s population should only affect the population share of USA counties below.

land_pop_extracted_train_county <- bind_rows(land_pop_full, alaska_pop)  %>% 
                      group_by(iso, year)  %>% 
                      mutate(pop_share = pop/sum(pop))  %>% 
                      ungroup()  %>% 
                      filter(id != "Ala") # Alaska's data is not included in the training sample, we use it here only to obtain the correct US county national population share

save(land_pop_extracted_train_county, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_train_county.RData")


# Retrieve the average areas of the subnational units for each country.

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
