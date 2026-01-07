# --------------------------------- Task Summary --------------------------------- #
# Retrieve the population data for each county or subnational region included 
#   in the training sample.
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

# ------------------------------------------------- #        
# Read the polygons
simplified_poly <- read_sf("version2_year2012_2022/step2_obtain_gdp_data/outputs/training_poly_sample.gpkg")

# ------------------------------------------------- #        
# Obtain training isos county-level population (LandScan)
tic("Population")

# Select all population files for years 2012-2022
population_files <- list.files("version2_year2012_2022/step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", 
                               pattern = "landscan-global", full.names = TRUE)
# Filter for years 2012-2022
population_files <- population_files[grepl(paste(2012:2022, collapse="|"), population_files)]

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){
  
  r <- rast(filename)
  
  extract <- cbind(simplified_poly, exact_extract(r, simplified_poly, 'sum')) %>% 
              rename(pop = exact_extract.r..simplified_poly...sum..)
  save(extract, file = paste0("version2_year2012_2022/step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_county_pop/land_pop_extracted_", 
                              as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- 2012:2022
land_pop_full <- NULL 
for (year in years){
          load(paste0("version2_year2012_2022/step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_county_pop/land_pop_extracted_", year, ".RData"))
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
# I double checked that the IMF USA national population does not include US territories, that is what we want here

alaska <- read_sf("version2_year2012_2022/step2_obtain_gdp_data/outputs/world_poly.gpkg")  %>% 
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
write.csv(alaska_pop, "version2_year2012_2022/step3_obtain_cell_level_GDP_and_predictors_data/outputs/alaska_population.csv", row.names = FALSE)

# ------------------------------------------------- # 
# actually we want to get each county's national population share here, so that we can rescale to match the sum with national population
# Thus, what Alaska should affect is the USA county's population share below

land_pop_extracted_train_county <- bind_rows(land_pop_full, alaska_pop)  %>% 
                      group_by(iso, year)  %>% 
                      mutate(pop_share = pop/sum(pop))  %>% 
                      ungroup()  %>% 
                      filter(id != "Ala") # we do not need Alaska

save(land_pop_extracted_train_county, file = "version2_year2012_2022/step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_train_county.RData")

# ------------------------------------------------- #
# now I want to get the average areas of those subnational units for each country

sub_area <- land_pop_extracted_train_county %>% 
    filter(year == 2012) %>% 
    mutate(area = st_area(geom)) %>% 
    as.data.frame() %>% 
    dplyr::select(-c(geom)) %>% 
    group_by(iso) %>%
    mutate(avr_area = mean(area)/1000000) %>% # change the unit to km^2
    ungroup() %>% 
    distinct(iso, avr_area)

write.csv(sub_area, "version2_year2012_2022/step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_subnational_area.csv", row.names = FALSE)

# eof ----