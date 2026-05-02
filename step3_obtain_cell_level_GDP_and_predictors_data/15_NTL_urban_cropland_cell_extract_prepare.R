# --------------------------------- Task Summary --------------------------------- #
# This file isolates the urban and cropland geometries within each cell.
# Subsequently, nighttime light (NTL) emissions can be extracted exclusively from 
#   urban areas or cropland areas.
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
library(raster)
library(qgisprocess)

# ------------------------------------------------------------------------------------------------------------
# Convert the entire landcover raster to 11 polygons, each stand for a landcover type.

data_folder <- "step3_obtain_cell_level_GDP_and_predictors_data/inputs/landcover_MCD12Q1V061"
year_folders <- list.dirs(data_folder, recursive = FALSE, full.names = FALSE)

lc_extracted <- mclapply(year_folders, mc.cores = 2, FUN = function(year_folder){
    lc_file <- rast(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_temp_files/test", year_folder,".vrt"))
    # Function to convert raster cell to sf object
    lc_polygons <- as.polygons(lc_file, values = TRUE, na.rm = TRUE) 
    lc_sf <- lc_polygons  %>% st_as_sf(lc_polygons)
    # save the landcover polygons
    st_write(lc_sf, paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_poly/lc_polygons_", year_folder, ".gpkg"))
})

# intersect cell polygons with landcover_urban polygons, we do it in QGIS version 3.32.0-Lima to reduce the waiting time, follow the exact step:
#   Notice, these steps require really really long time even in QGIS.

# Do the following for each of 1/0.5/0.25/0.1degree files: "inputs/world_province_1deg_with_id.geojson" and "inputs/world_province_xdeg_with_cellid.geojson"
#   1. Drag each year "outputs/landcover_poly/lc_polygons_20xx.gpkg" into QGIS; drag "inputs/world_province_xdeg_with_id.geojson" into QGIS
#   2. Use "Processing -> Toolbox -> Fix geometry" to fix invalid geometry in "inputs/world_province_xdeg_with_id.geojson", choose "linework" for the Repaire method
#       there will be a new layer "Fix geometries" appear, change the name to be "xdeg"
#   3. Click "Open Attribute Table" of file "outputs/landcover_poly/lc_polygons_20xx.gpkg", click the pencil icon, and delete all features except for 
#       the one with column "test20xx" value == 9 (stand for urban)      
#   4. Use "Processing -> Toolbox -> Fix geometry" to fix invalid geometry for each year file "lc_polygons_20xx.gpkg", choose "linework" for the Repair method
#       there will be a new layer "Fix geometries" appear, change the name to be "20xx" to clarify
#   5. Use "Vector -> Geoprocessing Tools -> Intersection", and choose "Run as Batch Processing", add 10 pieces, for each piece:
#       select "20xx" as Input layer, Choose "xdeg" as the Overlay layer, 
#       click the "..." in the "Intersection" section and save it as "inputs/lc_inters_id_xdeg/lc_urban_inters_id_xdeg_20xx.gpkg" and run.
#       (I think batch process is faster and more convenient)

# Now intersect cell polygons with landcover_cropland polygons, we do it in QGIS 
# Do the following for each of 1/0.5/0.25/0.1degree files: "inputs/world_province_xdeg_with_id.geojson"
#   1. Drag each year "outputs/landcover_poly/lc_polygons_20xx.gpkg" into QGIS; drag "inputs/world_province_xdeg_with_id.geojson" into QGIS
#   2. Use "Processing -> Toolbox -> Fix geometry" to fix invalid geometry in "inputs/world_province_xdeg_with_id.geojson", choose "linework" for the Repaire method
#       there will be a new layer "Fix geometries" appear, change the name to be "xdeg"
#   3. Click "Open Attribute Table" of file "outputs/landcover_poly/lc_polygons_20xx.gpkg", click the pencil icon, and delete all features except for 
#       the ones with column "test20xx" value == 25 or 35 or 36 (stand for forest_cropland, herbaceous_cropland, cropland)      
#   4. Use "Processing -> Toolbox -> Fix geometry" to fix invalid geometry for each year file "lc_polygons_20xx.gpkg", choose "Run as Batch Processing", 
#       add 10 pieces, for each piece:
#       choose "linework" for the Repair method
#       click the "..." in the "Fix geometries" section and save it as "cropland_20xx" to clarify and run.
#   5. Use "Vector -> Geoprocessing Tools -> Intersection", and choose "Run as Batch Processing", add 10 pieces, for each piece:
#       select "20xx" as Input layer, Choose "xdeg" as the Overlay layer, 
#       click the "..." in the "Intersection" section and save it as "inputs/lc_inters_id_xdeg/lc_cropland_inters_id_xdeg_20xx.ggpkg" and run.

years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]

    input <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_poly/lc_polygons_", year, ".gpkg")) %>% 
        filter(get(paste0("test", year)) == 9) 

    qgis_run_algorithm(
        "native:fixgeometries",
        INPUT = input,
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"),
        .quiet = FALSE
    )

    input <- read_sf(paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/landcover_poly/lc_polygons_", year, ".gpkg")) %>% 
        filter(get(paste0("test", year)) %in% c(25,35,36))

    qgis_run_algorithm(
        "native:fixgeometries",
        INPUT = input,
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"),
        .quiet = FALSE
    )

})

mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_urban_inters_id_1deg_", year, ".gpkg")
    )

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_1deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_1deg/lc_cropland_inters_id_1deg_", year, ".gpkg")
    )
})

mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_urban_inters_id_0_5deg_", year, ".gpkg")
    )

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_5deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_5deg/lc_cropland_inters_id_0_5deg_", year, ".gpkg")
    )
})

mclapply(1:length(years), mc.cores = 5, function(i) {
    year <- years[i]

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_urban_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_urban_inters_id_0_25deg_", year, ".gpkg")
    )

    qgis_run_algorithm(
        "native:intersection",
        INPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_temp/temp_cropland_", year, ".gpkg"), 
        OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_province_0_25deg_with_cellid.gpkg", 
        OUTPUT = paste0("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_inters_id_0_25deg/lc_cropland_inters_id_0_25deg_", year, ".gpkg")
    )
})
