# --------------------------------- Task Summary --------------------------------- #
# This file performs post-adjustment on the predicted 1-degree cell GDP values 
#       from the model trained using data from 2012 to 2021.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tictoc)
library(gdata)
library(units)
library(sf)
library(parallel)
library(tidyverse)
library(fs)
library(dplyr)
library(data.table)
library(vip)
library(ranger)
library(tmaptools)
library(scales)
library(workflows)
library(data.table)
library(tmaptools)
library(plotly)
library(htmlwidgets)
library(exactextractr)
library(terra)
library(raster)

# ------------------------------------------------------------------------------------------------------------------------------
# Model 9.1: 1deg

# obtain predicted GDP data
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_with_prov_boundary.RData")

# load population 
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_1deg.RData")
pop <- land_pop_extracted_region_level_1deg  %>%
       filter(year <= 2021)  %>% 
       as.data.frame()  %>% 
       dplyr::select(c("cell_id", "id", "iso", "year", "pop"))  %>% 
       mutate(pop = floor(pop)) %>%
       mutate(iso = ifelse(iso == "Ala", "USA", iso))

# load land area: 
# Note: the land area calculated is the area in square km based on a spherical approximation of the Earth

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_1deg.RData")
land_area <- lc_full_1deg  %>% 
        filter(year <= 2021)  %>% 
        as.data.frame()  %>% 
        dplyr::select(c("cell_id", "id", "iso", "year", "water", "barren", "snow_ice", "urban", "dense_forest",
                        "open_forest", "forest_cropland", "herbaceous", "cropland", "shrub", "herbaceous_cropland"))  %>% 
        replace(is.na(.), 0)  %>% 
        mutate(land_area_km2 = barren + snow_ice + urban + dense_forest + open_forest +
                    forest_cropland + herbaceous + cropland + shrub + herbaceous_cropland)  %>% 
        dplyr::select(c(cell_id, id, iso, year, land_area_km2)) %>%
        mutate(iso = ifelse(iso == "Ala", "USA", iso))

# ------------------------------------------------------------------------------------------------------------------------------

# load GDP
# Note: here also want the area in square km based on a spherical approximation of the Earth

pred_1deg_with_prov_bound <- predict_data_results_1deg_with_prov_boundary %>% 
                             dplyr::select(c(cell_id, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_1deg, pred_GCP_share_1deg_rescaled, pred_GCP_1deg, geom))  %>% 
                             left_join(pop)  %>% 
                             left_join(land_area)  %>% 
                             mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>% 
                             st_as_sf() %>% 
                             na.omit() # One cell for Saudi Arabia (SAU) has missing data due to differences in country border geometry between sources. This issue can be ignored.

# ------------------------------------------------------------------------------------------------------------------------------
# no extra adjustment
pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_1deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_1deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_1deg))  %>%
                                        mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_1deg_rescaled = ifelse(pred_GCP_share_1deg == 0, 0, pred_GCP_share_1deg/sum(pred_GCP_share_1deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 
save(pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust.RData")

deg1_geometry <- read_sf("step5_predict_and_post_adjustments_log_change/outputs/country_1deg_intersected.gpkg")  %>% 
                 dplyr::select(c(cell_id, iso, geom)) %>%
                 mutate(iso = ifelse(iso == "Ala", "USA", iso)) # need to put Alaska back to USA

organized_pred_1deg_postadjust_pop_dens_no_extra_adjust <- pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust  %>% 
                                        group_by(iso, year, cell_id)  %>%
                                        mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>% 
                                        mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_1deg))  %>% 
                                        ungroup()  %>% 
                                        as.data.frame()  %>%                                         
                                        dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, is_cell_censored))  %>% 
                                        distinct(iso, year, cell_id, .keep_all = TRUE)  %>%
                                        rename(predicted_GCP = pred_GCP_1deg_no_prov_bound)  %>%
                                        dplyr::select(c(cell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
                                        mutate(method = "post-adjust zero GDP for pop density = 0",
                                                cell_size = "1-deg by 1-deg")  %>% 
                                        left_join(deg1_geometry)

# 0.01 next

pred_1deg_with_prov_bound_postadjust_pop_dens_0_01_adjust <- pred_1deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_1deg = ifelse(pop_density_km2 <= 0.01, 0, pred_GCP_share_1deg))  %>%
                                        mutate(is_censored = ifelse(pop_density_km2 <= 0.01, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_1deg_rescaled = ifelse(pred_GCP_share_1deg == 0, 0, pred_GCP_share_1deg/sum(pred_GCP_share_1deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

organized_pred_1deg_postadjust_pop_dens_0_01_adjust <- pred_1deg_with_prov_bound_postadjust_pop_dens_0_01_adjust  %>% 
                                        group_by(iso, year, cell_id)  %>%
                                        mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>% 
                                        mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_1deg))  %>% 
                                        ungroup()  %>% 
                                        as.data.frame()  %>%                                         
                                        dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, is_cell_censored))  %>% 
                                        distinct(iso, year, cell_id, .keep_all = TRUE)  %>%
                                        rename(predicted_GCP = pred_GCP_1deg_no_prov_bound)  %>%
                                        dplyr::select(c(cell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
                                        mutate(method = "post-adjust zero GDP for pop density <= 0.01 (population per cell land area in km2)",
                                                cell_size = "1-deg by 1-deg")  %>% 
                                        left_join(deg1_geometry)

# 0.02 next
pred_1deg_with_prov_bound_postadjust_pop_dens_0_02_adjust <- pred_1deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_1deg = ifelse(pop_density_km2 <= 0.02, 0, pred_GCP_share_1deg))  %>%
                                        mutate(is_censored = ifelse(pop_density_km2 <= 0.02, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_1deg_rescaled = ifelse(pred_GCP_share_1deg == 0, 0, pred_GCP_share_1deg/sum(pred_GCP_share_1deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

organized_pred_1deg_postadjust_pop_dens_0_02_adjust <- pred_1deg_with_prov_bound_postadjust_pop_dens_0_02_adjust  %>% 
                                        group_by(iso, year, cell_id)  %>%
                                        mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>% 
                                        mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_1deg))  %>% 
                                        ungroup()  %>% 
                                        as.data.frame()  %>%                                         
                                        dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, is_cell_censored))  %>% 
                                        distinct(iso, year, cell_id, .keep_all = TRUE)  %>%
                                        rename(predicted_GCP = pred_GCP_1deg_no_prov_bound)  %>%
                                        dplyr::select(c(cell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
                                        mutate(method = "post-adjust zero GDP for pop density <= 0.02 (population per cell land area in km2)",
                                                cell_size = "1-deg by 1-deg")  %>% 
                                        left_join(deg1_geometry)

# 0.05 next
pred_1deg_with_prov_bound_postadjust_pop_dens_0_05_adjust <- pred_1deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_1deg = ifelse(pop_density_km2 <= 0.05, 0, pred_GCP_share_1deg))  %>%
                                        mutate(is_censored = ifelse(pop_density_km2 <= 0.05, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_1deg_rescaled = ifelse(pred_GCP_share_1deg == 0, 0, pred_GCP_share_1deg/sum(pred_GCP_share_1deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

organized_pred_1deg_postadjust_pop_dens_0_05_adjust <- pred_1deg_with_prov_bound_postadjust_pop_dens_0_05_adjust  %>% 
                                        group_by(iso, year, cell_id)  %>%
                                        mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>% 
                                        mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_1deg))  %>% 
                                        ungroup()  %>% 
                                        as.data.frame()  %>%                                         
                                        dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, is_cell_censored))  %>% 
                                        distinct(iso, year, cell_id, .keep_all = TRUE)  %>%
                                        rename(predicted_GCP = pred_GCP_1deg_no_prov_bound)  %>%
                                        dplyr::select(c(cell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
                                        mutate(method = "post-adjust zero GDP for pop density <= 0.05 (population per cell land area in km2)",
                                                cell_size = "1-deg by 1-deg")  %>% 
                                        left_join(deg1_geometry)

# ------------------------------------------------------------------------------------------------------------------------------
# obtain each 1 deg cell population

national_population <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv")  %>% 
                       as.data.frame()  %>% 
                       dplyr::select(c(iso, year, national_population))  %>% 
                       distinct(iso, year, national_population, .keep_all = TRUE)  %>% 
                       filter(year <= 2021) 

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_1deg.RData")
pop_cell_1deg <- land_pop_extracted_region_level_1deg  %>%
        filter(year <= 2021)  %>% 
        mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>%
        as.data.frame()  %>% 
        dplyr::select(c("cell_id", "id", "iso", "year", "pop"))  %>% 
        left_join(land_area) %>% 
        mutate(pop = ifelse(land_area_km2 == 0, 0, pop)) %>% # becasue pop should not live on water
        na.omit() %>% # One cell for Saudi Arabia (SAU) has missing data due to differences in country border geometry between sources. This issue can be ignored.
        group_by(year, iso, cell_id)  %>% 
        mutate(pop_cell = sum(pop))  %>% 
        distinct(year, iso, cell_id, .keep_all = TRUE)  %>% 
        ungroup()  %>%
        dplyr::select(c(cell_id, iso, year, pop_cell))  %>% 
        left_join(national_population) %>%
        group_by(iso, year)  %>% 
        mutate(pop_cell_rescaled = floor(ifelse(is.na(national_population), pop_cell, pop_cell*national_population/sum(pop_cell))))  %>% 
        mutate(pop_cell_rescaled = ifelse(pop_cell == 0, 0, pop_cell_rescaled)) %>% 
        ungroup()  %>% 
        left_join(deg1_geometry)

save(pop_cell_1deg, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pop_cell_1deg.RData")                                                               

# ------------------------------------------------------------------------------------------------------------------------------
# now obtain GDPC
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pop_cell_1deg.RData")

# no extra adjustment
GDPC_1deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_1deg_postadjust_pop_dens_no_extra_adjust  %>% 
             left_join(pop_cell_1deg) %>% 
             mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>% # in case after rescaling the pop, some places with very few population turns to 0
             mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
             dplyr::select(-c(pop_cell))  %>% 
             rename(pop_cell = pop_cell_rescaled)

save(GDPC_1deg_postadjust_pop_dens_no_extra_adjust, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_no_extra_adjust.RData")

# also generate csv file, instead of giving geometry, give longitude and latitude of the bottom-left corner of each cell
# just_grid_1deg <- read_sf("/share/rossihansberglab/Nightlights_GDP/replication_packages/step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg")

# just_grid_1deg$longitude <- NA
# just_grid_1deg$latitude <- NA

# # Extract the bottom-left point for each cell
# for (i in 1:nrow(just_grid_1deg)) {
#     bbox <- st_bbox(just_grid_1deg[i, ])
#     just_grid_1deg$longitude[i] <- bbox$xmin
#     just_grid_1deg$latitude[i] <- bbox$ymin
# }

# just_grid_1deg_with_lon_lat <- just_grid_1deg  %>% as.data.frame()  %>% dplyr::select(-c(geom))
# write.csv(just_grid_1deg_with_lon_lat, file = "step5_predict_and_post_adjustments_log_change/outputs/just_grid_1deg_with_lon_lat.csv", row.names = FALSE)

just_grid_1deg <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/just_grid_1deg_with_lon_lat.csv")

GDPC_1deg_postadjust_pop_dens_no_extra_adjust_csv <- GDPC_1deg_postadjust_pop_dens_no_extra_adjust  %>% 
  left_join(just_grid_1deg  %>% mutate(cell_id = as.character(cell_id)))  %>% 
  as.data.frame()  %>% 
  dplyr::select(-c(geom))
write.csv(GDPC_1deg_postadjust_pop_dens_no_extra_adjust_csv, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_no_extra_adjust.csv", row.names = FALSE)


# 0.01 threshold
GDPC_1deg_postadjust_pop_dens_0_01_adjust <- organized_pred_1deg_postadjust_pop_dens_0_01_adjust  %>% 
             left_join(pop_cell_1deg) %>% 
             mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
             mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
             dplyr::select(-c(pop_cell))  %>% 
             rename(pop_cell = pop_cell_rescaled)

# also generate csv file, instead of giving geometry, give longitude and latitude of the bottom-left corner of each cell
GDPC_1deg_postadjust_pop_dens_0_01_adjust_csv <- GDPC_1deg_postadjust_pop_dens_0_01_adjust  %>% 
  left_join(just_grid_1deg  %>% mutate(cell_id = as.character(cell_id)))  %>% 
  as.data.frame()  %>% 
  dplyr::select(-c(geom))
write.csv(GDPC_1deg_postadjust_pop_dens_0_01_adjust_csv, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_0_01_adjust.csv", row.names = FALSE)


# 0.02 threshold
GDPC_1deg_postadjust_pop_dens_0_02_adjust <- organized_pred_1deg_postadjust_pop_dens_0_02_adjust  %>% 
             left_join(pop_cell_1deg) %>% 
             mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
             mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
             dplyr::select(-c(pop_cell))  %>% 
             rename(pop_cell = pop_cell_rescaled)

# also generate csv file, instead of giving geometry, give longitude and latitude of the bottom-left corner of each cell
GDPC_1deg_postadjust_pop_dens_0_02_adjust_csv <- GDPC_1deg_postadjust_pop_dens_0_02_adjust  %>% 
  left_join(just_grid_1deg  %>% mutate(cell_id = as.character(cell_id)))  %>% 
  as.data.frame()  %>% 
  dplyr::select(-c(geom))
write.csv(GDPC_1deg_postadjust_pop_dens_0_02_adjust_csv, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_0_02_adjust.csv", row.names = FALSE)


# 0.05 threshold
GDPC_1deg_postadjust_pop_dens_0_05_adjust <- organized_pred_1deg_postadjust_pop_dens_0_05_adjust  %>% 
             left_join(pop_cell_1deg) %>% 
             mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
             mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
             dplyr::select(-c(pop_cell))  %>% 
             rename(pop_cell = pop_cell_rescaled)

# also generate csv file, instead of giving geometry, give longitude and latitude of the bottom-left corner of each cell
GDPC_1deg_postadjust_pop_dens_0_05_adjust_csv <- GDPC_1deg_postadjust_pop_dens_0_05_adjust  %>% 
  left_join(just_grid_1deg  %>% mutate(cell_id = as.character(cell_id)))  %>% 
  as.data.frame()  %>% 
  dplyr::select(-c(geom))
write.csv(GDPC_1deg_postadjust_pop_dens_0_05_adjust_csv, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_0_05_adjust.csv", row.names = FALSE)
