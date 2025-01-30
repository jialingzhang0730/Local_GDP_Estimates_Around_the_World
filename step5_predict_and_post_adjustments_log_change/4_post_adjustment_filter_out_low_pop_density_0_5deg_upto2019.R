# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to predict GDP using the model trained using year 2012-2019. It will be used in "step6_shocks/2_COVID_shock.R"
# ------------------------------------------------------------------------------------------------- #

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
# Model 9.1: 0_5deg

# obtain predicted GDP data
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_with_prov_boundary_model_up_to_2019.RData")

# load population 
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_5deg.RData")
pop <- land_pop_extracted_region_level_0_5deg  %>%
       filter(year <= 2021)  %>% 
       as.data.frame()  %>% 
       dplyr::select(c("cell_id", "subcell_id", "id", "iso", "year", "pop"))  %>% 
       mutate(pop = floor(pop)) %>%
       mutate(iso = ifelse(iso == "Ala", "USA", iso))


# load land area: 
# Note: the land area calculated is the area in square km based on a spherical approximation of the Earth

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_5deg.RData")
land_area <- lc_full_0_5deg  %>% 
        filter(year <= 2021)  %>% 
        as.data.frame()  %>% 
        dplyr::select(c("cell_id", "subcell_id", "id", "iso", "year", "water", "barren", "snow_ice", "urban", "dense_forest",
                        "open_forest", "forest_cropland", "herbaceous", "cropland", "shrub", "herbaceous_cropland"))  %>% 
        replace(is.na(.), 0)  %>% 
        mutate(land_area_km2 = barren + snow_ice + urban + dense_forest + open_forest +
                forest_cropland + herbaceous + cropland + shrub + herbaceous_cropland)  %>% 
        dplyr::select(c(cell_id, subcell_id, id, iso, year, land_area_km2)) %>%
        mutate(iso = ifelse(iso == "Ala", "USA", iso))

# ------------------------------------------------------------------------------------------------------------------------------

# load GDP
# Note: here I also want the area in square km based on a spherical approximation of the Earth

pred_0_5deg_with_prov_bound <- predict_data_results_0_5deg_with_prov_boundary_model_up_to_2019 %>% 
                             dplyr::select(c(cell_id, subcell_id, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_0_5deg, pred_GCP_share_0_5deg_rescaled, pred_GCP_0_5deg, geom))  %>% 
                             left_join(pop)  %>% 
                             left_join(land_area)  %>% 
                             mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>% 
                             st_as_sf()  %>% 
                             na.omit()

# ------------------------------------------------------------------------------------------------------------------------------

# no extra adjustment
pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_0_5deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_0_5deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_0_5deg))  %>% 
                                        mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_0_5deg_rescaled = ifelse(pred_GCP_share_0_5deg == 0, 0, pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl) 

pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019 <- pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust
save(pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019.RData")


deg0_5_geometry <- read_sf("step5_predict_and_post_adjustments_log_change/outputs/country_0_5deg_intersected.gpkg")  %>% 
                 dplyr::select(c(cell_id,  subcell_id, iso, geom)) %>%
                 mutate(iso = ifelse(iso == "Ala", "USA", iso)) # need to put Alaska back to USA

organized_pred_0_5deg_postadjust_pop_dens_no_extra_adjust <- pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust  %>% 
                                        group_by(iso, year, cell_id, subcell_id)  %>%
                                        mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>% 
                                        mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>% 
                                        ungroup()  %>% 
                                        as.data.frame()  %>%                                         
                                        dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, is_cell_censored))  %>% 
                                        distinct(iso, year, cell_id, subcell_id, .keep_all = TRUE)  %>%
                                        rename(predicted_GCP = pred_GCP_0_5deg_no_prov_bound)  %>%
                                        dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
                                        mutate(method = "post-adjust zero GDP for pop density = 0",
                                                cell_size = "0_5-deg by 0_5-deg")  %>% 
                                        left_join(deg0_5_geometry)

# ------------------------------------------------------------------------------------------------------------------------------
# now obtain GDPC
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pop_cell_0_5deg.RData")

# no extra adjustment

GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_up_to_2019 <- organized_pred_0_5deg_postadjust_pop_dens_no_extra_adjust  %>% 
             left_join(pop_cell_0_5deg) %>% 
             mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
             mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
             dplyr::select(-c(pop_cell))  %>% 
             rename(pop_cell = pop_cell_rescaled)

save(GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_up_to_2019.RData")
