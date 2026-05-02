# --------------------------------- Task Summary --------------------------------- #
# Retrieve cell-level GDP data.
# The objective is to calculate the true cell GDP and train the model on that data.
# The true cell GDP is calculated as GDPC * population, which is believed to closely 
#   approximate the actual values for the training countries.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(dplyr)
library(sf)
library(terra)
library(stars)
library(tidyverse)
library(ggthemes)
library(magrittr)
library(tictoc)
library(parallel)
library(gdata)
library(exactextractr)
library(purrr)
library(qgisprocess)

# ------------------------------------------------- # 
# Obtain county GDP
# DOSE has some regional data missing, please be carefull!!!!!!!!!!!! Very easy to make mistake
county_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_training_data.csv")  %>% 
  filter(!(iso == "THA" & year %in% c(2012, 2013, 2019))) %>% # THA has missing regional data for 2012, 2013 and 2019
  filter(!(iso == "ECU" & year %in% c(2012, 2013, 2014))) # ECU has missing regional data for 2012, 2013 and 2014

# ------------------------------------------------- # 
# Obtain county GDP per capita
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_train_county.RData")

county_pop <- land_pop_extracted_train_county %>%
  dplyr::select(c("id", "iso", "year", "pop_share", "geom"))

county_GDPC <- county_GDP  %>% 
  mutate(id = ifelse(id == "Bangsamoro Autonomous Region\nin Muslim Mindanao_PHL", "Bangsamoro Autonomous Region\r\nin Muslim Mindanao_PHL", id))  %>% # again this one still...always need to adjusted manually
  left_join(county_pop, by = c("id", "iso", "year")) %>% # good, every county that has GDP has population data
  mutate(pop_rescaled = round(pop_share * national_population))  %>% 
  mutate(county_GDPC = ifelse(pop_rescaled == 0, 0, unit_rgdp_total / pop_rescaled))  %>% 
  dplyr::select(c("id", "iso", "year", "county_GDPC", "geom"))  %>% 
  st_as_sf()

st_write(county_GDPC, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_GDPC.gpkg", append = F)

# ------------------------------------------------- # 
# obtain cell grids

# Add 1degree cells 
grid <- rast(resolution = 1, crs = "epsg:4326") %>% # by doing this, the world is seperated into 1°x1° cell: 180*360=64800
  setValues(NA) %>% st_as_stars() %>% st_as_sf(na.rm = F) %>%
  mutate(cell_id = rownames(.)) %>% 
  dplyr::select(-lyr.1)
st_write(grid, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg")

# bottom-left (longitude, latitude) of each 1deg cell, used by downstream lon/lat joins
just_grid_1deg_with_lon_lat <- grid %>%
  mutate(centroid = st_centroid(geom),
         longitude = st_coordinates(centroid)[, 1] - 0.5,
         latitude  = st_coordinates(centroid)[, 2] - 0.5) %>%
  as.data.frame() %>%
  dplyr::select(cell_id, longitude, latitude)
write.csv(just_grid_1deg_with_lon_lat,
          file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1deg_with_lon_lat.csv",
          row.names = FALSE)

# Add 0.5degree cells
# This is done through QGIS: 
# 1. create 0.5deg grids: Vector > Research Tools > Create Grid:
#     Choose Grid type: Rectangle (Polygon); Horizontal spacing: 0.5 degrees; Vertical spacing: 0.5 degrees; Horizontal overlay: 0 degree; Vertical overlay 0 degree
#     Grid CRS: EPSG:4326 - WGS 84
#     Grid Extend: -180,180,-90,90
# 2. Intersect "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg": Vector > Geoprocessing Tools; Input Layer is 0.5deg grids; 
#    Overlay Layer: "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg"
# By doing this, we can know each 0.5deg cell belongs to which 1deg cell
# Can also do this in R, but it takes very long time
# Obtain "step3_obtain_cell_level_GDP_and_predictors_data/inputs/subcell_0_5grid.gpkg"

# Add 0.5degree cells
grid_output <- tempfile(fileext = ".gpkg")

qgis_run_algorithm(
  "native:creategrid",
  TYPE = 2,  # Rectangle (Polygon)
  EXTENT = "-180,180,-90,90",  # Grid extent for the whole globe
  HSPACING = 0.5,  # Horizontal spacing: 0.5 degrees
  VSPACING = 0.5,  # Vertical spacing: 0.5 degrees
  HOVERLAY = 0,  # Horizontal overlay: 0 degrees
  VOVERLAY = 0,  # Vertical overlay: 0 degrees
  CRS = "EPSG:4326",  # Grid CRS: WGS84
  OUTPUT = grid_output
)

# Intersect 0.5-degree grids with the 1-degree grids so to know which 0.5deg belongs to which 1deg cell
qgis_run_algorithm(
  "native:intersection",
  INPUT = grid_output,  # 0.5-degree grid
  OVERLAY = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg"),  # 1-degree grid
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/subcell_0_5grid.gpkg"
)

grid0_5deg <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/subcell_0_5grid.gpkg") %>%  
  select(c(cell_id))  %>% 
  group_by(cell_id)  %>% 
  mutate(subcell_id = row_number())  %>% 
  arrange(as.numeric(cell_id))
st_write(grid0_5deg, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5degree.gpkg")

# bottom-left (longitude, latitude) of each 0.5deg subcell
just_grid_0_5deg_with_lon_lat <- grid0_5deg %>%
  ungroup() %>%
  mutate(centroid = st_centroid(geom),
         longitude = st_coordinates(centroid)[, 1] - 0.25,
         latitude  = st_coordinates(centroid)[, 2] - 0.25) %>%
  as.data.frame() %>%
  dplyr::select(cell_id, subcell_id, longitude, latitude)
write.csv(just_grid_0_5deg_with_lon_lat,
          file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5deg_with_lon_lat.csv",
          row.names = FALSE)

# Add 0.25degree cells: similar approach to obtain "step3_obtain_cell_level_GDP_and_predictors_data/inputs/subcell_0_25grid.gpkg":
# create 0.25deg grids
# intersect with 0.5degree

grid_output <- tempfile(fileext = ".gpkg")

qgis_run_algorithm(
  "native:creategrid",
  TYPE = 2,  # Rectangle (Polygon)
  EXTENT = "-180,180,-90,90",  # Grid extent for the whole globe
  HSPACING = 0.25,  # Horizontal spacing: 0.5 degrees
  VSPACING = 0.25,  # Vertical spacing: 0.5 degrees
  HOVERLAY = 0,  # Horizontal overlay: 0 degrees
  VOVERLAY = 0,  # Vertical overlay: 0 degrees
  CRS = "EPSG:4326",  # Grid CRS: WGS84
  OUTPUT = grid_output
)

# intersect with 0.5degree
qgis_run_algorithm(
  "native:intersection",
  INPUT = read_sf(grid_output), 
  OVERLAY = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5degree.gpkg"),
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/subcell_0_25grid.gpkg"
)

grid0_25deg <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/subcell_0_25grid.gpkg")  %>% 
  dplyr::select(c(cell_id, subcell_id, id))  %>% 
  group_by(cell_id, subcell_id)  %>% 
  arrange(id)  %>% 
  mutate(subcell_id_0_25 = row_number())  %>% 
  dplyr::select(-c(id))  %>% 
  arrange(as.numeric(cell_id), as.numeric(subcell_id))
st_write(grid0_25deg, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_25degree.gpkg")

# bottom-left (longitude, latitude) of each 0.25deg subcell
just_grid_0_25deg_with_lon_lat <- grid0_25deg %>%
  ungroup() %>%
  mutate(centroid = st_centroid(geom),
         longitude = st_coordinates(centroid)[, 1] - 0.125,
         latitude  = st_coordinates(centroid)[, 2] - 0.125) %>%
  as.data.frame() %>%
  dplyr::select(cell_id, subcell_id, subcell_id_0_25, longitude, latitude)
write.csv(just_grid_0_25deg_with_lon_lat,
          file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_25deg_with_lon_lat.csv",
          row.names = FALSE)

# ---------------------Important!!!---------------------------- # 
# obtain intersected polygons:

# In QGIS, intersect file "outputs/just_grid_xdegree.gpkg" and "outputs/county_GDPC.gpkg" to obtain polygons:
# 1. Processing > Tools > fix geometry command to "outputs/county_GDPC.gpkg"; obtain the file "Fixed geometries"
# 2. Vector > Geoprocessing > intersection the above file with "outputs/just_grid_xdegree.gpkg": input layer is "fixed geometries", overlay layer is "just_grid_xdegree.gpkg"
# 3. export as "inputs/county_gridded_xdegree.gpkg"

qgis_run_algorithm(
  "native:intersection",
  INPUT = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_GDPC.gpkg"), 
  OVERLAY = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg"), 
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_1degree.gpkg"
)

qgis_run_algorithm(
  "native:intersection",
  INPUT = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_GDPC.gpkg"), 
  OVERLAY = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5degree.gpkg"), 
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_0_5degree.gpkg"
)

qgis_run_algorithm(
  "native:intersection",
  INPUT = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_GDPC.gpkg"), 
  OVERLAY = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_25degree.gpkg"), 
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_0_25degree.gpkg"
)

# read the files
county_with_1cellid <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_1degree.gpkg") 

county_with_0_5cellid <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_0_5degree.gpkg") 

county_with_0_25cellid <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_0_25degree.gpkg")

# ---------------------Important!!!---------------------------- # 
# Obtain population for each county-cell intersected polygons

# 1degree
tic("Population")

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23]

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){

  r <- rast(filename)
  year_file <- gsub(".*landscan-global-(\\d{4}).*\\.tif", "\\1", filename)
  county_with_1cellid_year <- county_with_1cellid  %>% filter(year == as.integer(year_file))

  extract <- cbind(county_with_1cellid_year, exact_extract(r, county_with_1cellid_year, 'sum')) %>% 
    rename(pop = exact_extract.r..county_with_1cellid_year...sum..)
  save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_pop_year_sep/county_pop_extracted_1deg", as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
land_pop_full <- NULL 
for (year in years){
  load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_pop_year_sep/county_pop_extracted_1deg", year, ".RData"))

  if (is.null(land_pop_full)) {
    land_pop_full <- extract
  } else {
    land_pop_full <- bind_rows(land_pop_full, extract)
  }
}
county_cell_pop_extracted_1deg <- land_pop_full  %>% 
  replace_na(list(pop = 0))  

save(county_cell_pop_extracted_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_cell_pop_extracted_1deg.RData")

# 0.5 degree
tic("Population")

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23]

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){

  r <- rast(filename)
  year_file <- gsub(".*landscan-global-(\\d{4}).*\\.tif", "\\1", filename)
  county_with_0_5cellid_year <- county_with_0_5cellid  %>% filter(year == as.integer(year_file))

  extract <- cbind(county_with_0_5cellid_year, exact_extract(r, county_with_0_5cellid_year, 'sum')) %>% 
    rename(pop = exact_extract.r..county_with_0_5cellid_year...sum..)
  save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_pop_year_sep/county_pop_extracted_0_5deg", as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
land_pop_full <- NULL 
for (year in years){
  load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_pop_year_sep/county_pop_extracted_0_5deg", year, ".RData"))

  if (is.null(land_pop_full)) {
    land_pop_full <- extract
  } else {
    land_pop_full <- bind_rows(land_pop_full, extract)
  }
}
county_cell_pop_extracted_0_5deg <- land_pop_full  %>% 
  replace_na(list(pop = 0))  

save(county_cell_pop_extracted_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_cell_pop_extracted_0_5deg.RData")

# 0.25 degree
tic("Population")

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23]

pop_extracted <- mclapply(population_files, mc.cores = 5, FUN = function(filename){

  r <- rast(filename)
  year_file <- gsub(".*landscan-global-(\\d{4}).*\\.tif", "\\1", filename)
  county_with_0_25cellid_year <- county_with_0_25cellid  %>% filter(year == as.integer(year_file))

  extract <- cbind(county_with_0_25cellid_year, exact_extract(r, county_with_0_25cellid_year, 'sum')) %>% 
    rename(pop = exact_extract.r..county_with_0_25cellid_year...sum..)
  save(extract, file = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_pop_year_sep/county_pop_extracted_0_25deg", as.numeric(str_extract(filename, "\\d{4}")), ".RData"))

})
toc()

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020","2021","2022")
land_pop_full <- NULL 
for (year in years){
  load(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_gridded_pop_year_sep/county_pop_extracted_0_25deg", year, ".RData"))

  if (is.null(land_pop_full)) {
    land_pop_full <- extract
  } else {
    land_pop_full <- bind_rows(land_pop_full, extract)
  }
}
county_cell_pop_extracted_0_25deg <- land_pop_full  %>% 
  replace_na(list(pop = 0))  

save(county_cell_pop_extracted_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/county_cell_pop_extracted_0_25deg.RData")

# -------------------------------------------------------------------------------------------------------------------- #
# obtain cell GDP

# 1 degree
county_GDP_change_USA <- county_GDP  %>% 
  mutate(iso_real = iso)  %>%
  mutate(iso = ifelse(iso == "USA", paste0("USA_", substr(id, 1, 2)), iso))  %>% # change USA's state as country
  group_by(iso, year)  %>% 
  mutate(state_total_GDP = sum(unit_rgdp_total))  %>% 
  ungroup()  %>% 
  mutate(id = ifelse(id == "Bangsamoro Autonomous Region\nin Muslim Mindanao_PHL", "Bangsamoro Autonomous Region\r\nin Muslim Mindanao_PHL", id))

alaska_pop <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/alaska_population.csv")  %>% 
  rename(iso_real = iso)

county_cell_pop_extracted_1deg_GDP <- county_cell_pop_extracted_1deg  %>% 
  as.data.frame()  %>% 
  dplyr::select(c(cell_id, id, iso, year, county_GDPC, pop))  %>% 
  mutate(iso = ifelse(iso == "USA", paste0("USA_", substr(id, 1, 2)), iso)) %>% # by doing this, I change the state as new country
  left_join(county_GDP_change_USA) %>%  # because I want to get the national GDP and national population to further rescale
  bind_rows(alaska_pop)  %>%  # add alaska here so that we can have its population                                    
  group_by(year, iso_real)  %>%                                   
  mutate(GDP_subcell = county_GDPC*round(national_population*pop/sum(pop)))  %>% 
  ungroup()  %>% 
  filter(id != "Ala")  %>% # remove Alaska because we don't need it anymore
  group_by(iso, year)  %>% 
  mutate(GDP_subcell_rescl = GDP_subcell * state_total_GDP/sum(GDP_subcell))  %>% 
  ungroup()

training_iso_1deg_cell_GCP <- county_cell_pop_extracted_1deg_GDP  %>% 
  group_by(year, iso, cell_id)  %>% 
  mutate(GCP_1deg = sum(GDP_subcell_rescl))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, iso, year, GCP_1deg, state_total_GDP, parent_rgdp_total, national_population))  %>% 
  distinct(year, iso, cell_id, .keep_all = TRUE) 

save(training_iso_1deg_cell_GCP, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_iso_1deg_cell_GCP.RData")

# 0.5 degree                                 
county_cell_pop_extracted_0_5deg_GDP <- county_cell_pop_extracted_0_5deg  %>% 
  as.data.frame()  %>% 
  dplyr::select(c(cell_id, subcell_id, id, iso, year, county_GDPC, pop))  %>% 
  mutate(iso = ifelse(iso == "USA", paste0("USA_", substr(id, 1, 2)), iso))  %>% # by doing this, I change the state as new country                                                                        
  left_join(county_GDP_change_USA)  %>%  # because I want to get the national GDP and national population to further rescale
  bind_rows(alaska_pop)  %>%  # add alaska here so that we can have its population              
  group_by(year, iso_real)  %>%                                   
  mutate(GDP_subcell = county_GDPC*round(national_population*pop/sum(pop)))  %>% 
  ungroup()  %>% 
  filter(id != "Ala")  %>% # remove Alaska because we don't need it anymore
  group_by(iso, year)  %>% 
  mutate(GDP_subcell_rescl = GDP_subcell * state_total_GDP/sum(GDP_subcell))  %>% 
  ungroup()

training_iso_0_5deg_cell_GCP <- county_cell_pop_extracted_0_5deg_GDP  %>% 
  group_by(year, iso, cell_id, subcell_id)  %>% 
  mutate(GCP_0_5deg = sum(GDP_subcell_rescl))  %>% 
  ungroup()  %>%                               
  dplyr::select(c(cell_id, subcell_id, iso, year, GCP_0_5deg, state_total_GDP,parent_rgdp_total, national_population))  %>% 
  distinct(year, iso, cell_id, subcell_id, .keep_all = TRUE) 

save(training_iso_0_5deg_cell_GCP, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_iso_0_5deg_cell_GCP.RData")

# 0.25 degree                               
county_cell_pop_extracted_0_25deg_GDP <- county_cell_pop_extracted_0_25deg  %>% 
  as.data.frame()  %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, id, iso, year, county_GDPC, pop))  %>% 
  mutate(iso = ifelse(iso == "USA", paste0("USA_", substr(id, 1, 2)), iso))  %>% # by doing this, I change the state as new country
  left_join(county_GDP_change_USA)  %>%  # because I want to get the national GDP and national population to further rescale
  bind_rows(alaska_pop)  %>%  # add alaska here so that we can have its population     
  group_by(year, iso_real)  %>%                                   
  mutate(GDP_subcell_0_25 = county_GDPC*round(national_population*pop/sum(pop)))  %>% 
  ungroup()  %>% 
  filter(id != "Ala")  %>% # remove Alaska because we don't need it anymore
  group_by(iso, year)  %>% 
  mutate(GDP_subcell_0_25_rescl = GDP_subcell_0_25 * state_total_GDP/sum(GDP_subcell_0_25))  %>% 
  ungroup() 

training_iso_0_25deg_cell_GCP <- county_cell_pop_extracted_0_25deg_GDP  %>% 
  group_by(year, iso, cell_id, subcell_id, subcell_id_0_25)  %>% 
  mutate(GCP_0_25deg = sum(GDP_subcell_0_25_rescl))  %>% 
  ungroup()  %>%                               
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, GCP_0_25deg, state_total_GDP,parent_rgdp_total, national_population))  %>% 
  distinct(year, iso, cell_id, subcell_id, subcell_id_0_25, .keep_all = TRUE) 

save(training_iso_0_25deg_cell_GCP, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_iso_0_25deg_cell_GCP.RData")

# ----------------------------------------------------------------------------------------------------------------------------- #
# cell intersection GIS data
# In QGIS: intersect "world_model8_poly.gpkg" file and "just_grid_xdegree.gpkg" file:
# 1. Fix geometry of "world_model8_poly.gpkg",
# 2. Intersection: input layer is "fixed geometries", overlay layer is "just_grid_xdegree.gpkg"
# 3. Export as "inputs/world_province_xxdeg_with_cellid.gpkg"
#

qgis_run_algorithm(
  "native:intersection",
  INPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_model8_poly.gpkg", 
  OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg", 
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg"
)

qgis_run_algorithm(
  "native:intersection",
  INPUT = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_model8_poly.gpkg"), 
  OVERLAY = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5degree.gpkg"), 
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg"
)

qgis_run_algorithm(
  "native:intersection",
  INPUT = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_model8_poly.gpkg"), 
  OVERLAY = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_25degree.gpkg"), 
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg"
)
