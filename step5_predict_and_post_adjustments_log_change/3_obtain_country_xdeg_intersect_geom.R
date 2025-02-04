# --------------------------------- Task Summary --------------------------------- #
# This file retrieves the geometry of countries intersected with the xdeg grid, 
#   which will be used in the R script "4_post_adjustment_filter_out_low_pop_density_xdeg".
# Note that the resulting geometries exclude large inland waters.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(qgisprocess)

qgis_run_algorithm(
    "native:intersection",
    INPUT = "step2_obtain_gdp_data/outputs/world_poly.gpkg", 
    OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg", 
    OUTPUT = "step5_predict_and_post_adjustments_log_change/outputs/country_1deg_intersected.gpkg"
)

qgis_run_algorithm(
    "native:intersection",
    INPUT = "step2_obtain_gdp_data/outputs/world_poly.gpkg", 
    OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5degree.gpkg", 
    OUTPUT = "step5_predict_and_post_adjustments_log_change/outputs/country_0_5deg_intersected.gpkg"
)

qgis_run_algorithm(
    "native:intersection",
    INPUT = "step2_obtain_gdp_data/outputs/world_poly.gpkg", 
    OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_25degree.gpkg", 
    OUTPUT = "step5_predict_and_post_adjustments_log_change/outputs/country_0_25deg_intersected.gpkg"
)

# Generate the output as a shapefile, as some users may not be able to use GPKG files.
deg1 <- st_read("step5_predict_and_post_adjustments_log_change/outputs/country_1deg_intersected.gpkg")
st_write(deg1, "step5_predict_and_post_adjustments_log_change/outputs/deg_geom_shapefile/geom_1deg.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

deg0_5 <- st_read("step5_predict_and_post_adjustments_log_change/outputs/country_0_5deg_intersected.gpkg")
st_write(deg0_5, "step5_predict_and_post_adjustments_log_change/outputs/deg_geom_shapefile/geom_0_5deg.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

deg0_25 <- st_read("step5_predict_and_post_adjustments_log_change/outputs/country_0_25deg_intersected.gpkg")
st_write(deg0_25, "step5_predict_and_post_adjustments_log_change/outputs/deg_geom_shapefile/geom_0_25deg.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

