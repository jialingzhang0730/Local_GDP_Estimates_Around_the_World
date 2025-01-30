# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to obtain the predictions from the model trained in "1_put_all_isos_to_train_xdeg.R"
#   prepare the predictions so to compare with our formal benchmark model in section "step4_train_and_tune_log_change" (also our model in the paper)
# Predictions are obtained using exactly the same way as the formal benchmark model 
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

# ---------------------------------------------------------------------------------------------------------------------------------------
# 1 degree

#load the model
load("step7_robust_analysis/model_tune_MSE/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_1deg.RData")
rf_model_good <- rf_model9_good_grid_search_1deg

importance_scores <- vip::vi(rf_model_good) %>% as.data.frame()
write.csv(importance_scores, "step7_robust_analysis/model_tune_MSE/outputs/importance_scores_1deg.csv", row.names = FALSE)

#load province/country GDP data
province_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") 

#load predictors dataset
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_1deg.RData")

predict_data_complete <- predictors_put_in_model_1deg %>% 
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
  left_join(province_GDP) 

#obtain training dataset, remember to use oob predictions for the training dataset

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_1deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_1deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_1deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_1deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_1deg.csv") 

# oob predictions obtained during the model training
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "4_put_all_isos_to_train_xdeg.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
    rename(id = iso)  %>% 
    mutate(pred_GCP_share_1deg = pred_train_sam[,1])  %>%  # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "4_put_all_isos_to_train_xdeg.R").
    dplyr::select(c(cell_id, id, year, pred_GCP_share_1deg))  %>% 
    mutate(cell_id = as.character(cell_id))  %>% 
    mutate(id = ifelse(substr(id,1,4) == "USA_", substr(id,5,6), id)) # so to match with "predict_data_complete"

# ------------------------------
# Now we can predict them

predictions_model <- as.data.frame(predict(object = rf_model_good, new_data = predict_data_complete))

# replace the training sample's predictions with oob predictions
predictions_predict <- predict_data_complete %>%
  left_join(data_full)  %>% 
  mutate(pred_model = predictions_model[,1])  %>% 
  mutate(pred_GCP_share_1deg = ifelse(!is.na(pred_GCP_share_1deg), pred_GCP_share_1deg, pred_model)) %>%
  as.data.frame()  %>%   
  dplyr::select(c(pred_GCP_share_1deg))

raw_predictions_1deg_model_all_years <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_1deg = predictions_predict[,1])

predict_data_results_1deg_with_prov_boundary <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_1deg = predictions_predict[,1])  %>% 
                              mutate(pred_GCP_share_1deg = ifelse(pop_share == 0, 0, pred_GCP_share_1deg))  %>%                               
                              group_by(id, year)  %>% 
                              mutate(pred_GCP_share_1deg_rescaled = pred_GCP_share_1deg/sum(pred_GCP_share_1deg))  %>% 
                              ungroup()  %>% 
                              mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

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

# load GDP
# Note: here I also want the area in square km based on a spherical approximation of the Earth
pred_1deg_with_prov_bound <- predict_data_results_1deg_with_prov_boundary %>% 
                             as.data.frame() %>% 
                             dplyr::select(c(cell_id, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_1deg, pred_GCP_share_1deg_rescaled, pred_GCP_1deg))  %>% 
                             left_join(pop)  %>% 
                             left_join(land_area)  %>% 
                             mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>% 
                             na.omit() # there is one cell for SAU that have missing data purely because of country border geometry differences from different sources, ignore it.

# no extra adjustment
pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_1deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_1deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_1deg))  %>%
                                        mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_1deg_rescaled = ifelse(pred_GCP_share_1deg == 0, 0, pred_GCP_share_1deg/sum(pred_GCP_share_1deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

save(pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust, file = "step7_robust_analysis/model_tune_MSE/outputs/pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust.RData")

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
                                                cell_size = "1-deg by 1-deg") 

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pop_cell_1deg.RData")

# no extra adjustment
just_grid_1deg <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/just_grid_1deg_with_lon_lat.csv")

GDPC_1deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_1deg_postadjust_pop_dens_no_extra_adjust  %>% 
            left_join(pop_cell_1deg %>% as.data.frame() %>% dplyr::select(-c(geom))) %>% 
            mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>% # in case after rescaling the pop, some places with very few population turns to 0
            mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
            dplyr::select(-c(pop_cell))  %>% 
            rename(pop_cell = pop_cell_rescaled) %>% 
            left_join(just_grid_1deg  %>% mutate(cell_id = as.character(cell_id)))  %>% 
            as.data.frame() 

write.csv(GDPC_1deg_postadjust_pop_dens_no_extra_adjust, file = "step7_robust_analysis/model_tune_MSE/outputs/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_m_mse.csv", row.names = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# 0.5 degree

#load the model
load("step7_robust_analysis/model_tune_MSE/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_5deg.RData")
rf_model_good <- rf_model9_good_grid_search_0_5deg

importance_scores <- vip::vi(rf_model_good) %>% as.data.frame()
write.csv(importance_scores, "step7_robust_analysis/model_tune_MSE/outputs/importance_scores_0_5deg.csv", row.names = FALSE)

#load province/country GDP data
province_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") 

#load predictors dataset
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_0_5deg.RData")
predict_data_complete <- predictors_put_in_model_0_5deg  %>% 
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
  left_join(province_GDP)  

#obtain training dataset, remember to use oob predictions for the training dataset

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_0_5deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_0_5deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_5deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_0_5deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_0_5deg.csv") 

# oob predictions obtained during the model training
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "4_put_all_isos_to_train_xdeg.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
    rename(id = iso)  %>% 
    mutate(pred_GCP_share_0_5deg = pred_train_sam[,1])  %>%  # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "4_put_all_isos_to_train_xdeg.R").
    dplyr::select(c(cell_id, subcell_id, id, year, pred_GCP_share_0_5deg))  %>% 
    mutate(cell_id = as.character(cell_id))  %>% 
    mutate(id = ifelse(substr(id,1,4) == "USA_", substr(id,5,6), id)) # so to match with "predict_data_complete"

# ------------------------------
# Now we can predict them

predictions_model <- as.data.frame(predict(object = rf_model_good, new_data = predict_data_complete))

# replace the training sample's predictions with oob predictions
predictions_predict <- predict_data_complete %>%
  left_join(data_full)  %>% 
  mutate(pred_model = predictions_model[,1])  %>% 
  mutate(pred_GCP_share_0_5deg = ifelse(!is.na(pred_GCP_share_0_5deg), pred_GCP_share_0_5deg, pred_model)) %>%
  as.data.frame()  %>%  
  dplyr::select(c(pred_GCP_share_0_5deg))

raw_predictions_0_5deg_model_all_years <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_5deg = predictions_predict[,1])

predict_data_results_0_5deg_with_prov_boundary <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_5deg = predictions_predict[,1])  %>% 
                              mutate(pred_GCP_share_0_5deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_5deg))  %>%                               
                              group_by(id, year)  %>% 
                              mutate(pred_GCP_share_0_5deg_rescaled = pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg))  %>% 
                              ungroup()  %>% 
                              mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl) 

# load population 
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_5deg.RData")
pop <- land_pop_extracted_region_level_0_5deg  %>%
       filter(year <= 2021)  %>% 
       as.data.frame()  %>% 
       dplyr::select(c("cell_id", "subcell_id","id", "iso", "year", "pop"))  %>% 
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

# load GDP
# Note: here I also want the area in square km based on a spherical approximation of the Earth

pred_0_5deg_with_prov_bound <- predict_data_results_0_5deg_with_prov_boundary %>%
                            as.data.frame() %>% 
                            dplyr::select(c(cell_id, subcell_id, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_0_5deg, pred_GCP_share_0_5deg_rescaled, pred_GCP_0_5deg))  %>% 
                            left_join(pop)  %>% 
                            left_join(land_area)  %>% 
                            mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>% # some small islands have population, but landcover data do not able to catch them, so ignore this problem
                            na.omit() # there is one cell for SAU that have missing data purely because of country border geometry differences from different sources, ignore it.

# no extra adjustment
pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_0_5deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_0_5deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_0_5deg))  %>% 
                                        mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_0_5deg_rescaled = ifelse(pred_GCP_share_0_5deg == 0, 0, pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl) 

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
                                                cell_size = "0.5-deg by 0.5-deg") 

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pop_cell_0_5deg.RData")

just_grid_0_5deg <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/just_grid_0_5deg_with_lon_lat.csv")

GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_0_5deg_postadjust_pop_dens_no_extra_adjust  %>% 
            left_join(pop_cell_0_5deg %>% as.data.frame() %>% dplyr::select(-c(geom))) %>% 
            mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
            mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
            dplyr::select(-c(pop_cell))  %>% 
            rename(pop_cell = pop_cell_rescaled) %>% 
            left_join(just_grid_0_5deg  %>% mutate(cell_id = as.character(cell_id)))  %>% 
            as.data.frame()

write.csv(GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust, file = "step7_robust_analysis/model_tune_MSE/outputs/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_m_mse.csv", row.names = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# 0.25 degree

#load the model
load("step7_robust_analysis/model_tune_MSE/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_25deg.RData")
rf_model_good <- rf_model9_good_grid_search_0_25deg

importance_scores <- vip::vi(rf_model_good) %>% as.data.frame()
write.csv(importance_scores, "step7_robust_analysis/model_tune_MSE/outputs/importance_scores_0_25deg.csv", row.names = FALSE)

#load province/country GDP data
province_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") 

#load predictors dataset
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_0_25deg.RData")
predict_data_complete <- predictors_put_in_model_0_25deg  %>% 
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
  left_join(province_GDP) 

#obtain training dataset, remember to use oob predictions for the training dataset

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_0_25deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_0_25deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_25deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_0_25deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_0_25deg.csv") 

# oob predictions obtained during the model training
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "4_put_all_isos_to_train_xdeg.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
    rename(id = iso)  %>% 
    mutate(pred_GCP_share_0_25deg = pred_train_sam[,1])  %>%  # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "4_put_all_isos_to_train_xdeg.R").
    dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, id, year, pred_GCP_share_0_25deg))  %>% 
    mutate(cell_id = as.character(cell_id))  %>% 
    mutate(id = ifelse(substr(id,1,4) == "USA_", substr(id,5,6), id)) # so to match with "predict_data_complete"

# ------------------------------
# Now we can predict them

predictions_model <- as.data.frame(predict(object = rf_model_good, new_data = predict_data_complete))

# replace the training sample's predictions with oob predictions
predictions_predict <- predict_data_complete %>%
  left_join(data_full)  %>% 
  mutate(pred_model = predictions_model[,1])  %>% 
  mutate(pred_GCP_share_0_25deg = ifelse(!is.na(pred_GCP_share_0_25deg), pred_GCP_share_0_25deg, pred_model)) %>%
  as.data.frame()  %>%  
  dplyr::select(c(pred_GCP_share_0_25deg))

raw_predictions_0_25deg_model_all_years <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_25deg = predictions_predict[,1])

predict_data_results_0_25deg_with_prov_boundary <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_25deg = predictions_predict[,1])  %>% 
                              mutate(pred_GCP_share_0_25deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_25deg))  %>%                               
                              group_by(id, year)  %>% 
                              mutate(pred_GCP_share_0_25deg_rescaled = pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg))  %>% 
                              ungroup()  %>% 
                              mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl) 

# load population 
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_25deg.RData")
pop <- land_pop_extracted_region_level_0_25deg  %>%
       filter(year <= 2021)  %>% 
       as.data.frame()  %>% 
       dplyr::select(c("cell_id", "subcell_id", "subcell_id_0_25", "id", "iso", "year", "pop"))  %>% 
       mutate(pop = floor(pop)) %>%
       mutate(iso = ifelse(iso == "Ala", "USA", iso))

# load land area: 
# Note: the land area calculated is the area in square km based on a spherical approximation of the Earth

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_25deg.RData")
land_area <- lc_full_0_25deg  %>% 
        filter(year <= 2021)  %>% 
        as.data.frame()  %>% 
        dplyr::select(c("cell_id", "subcell_id", "subcell_id_0_25", "id", "iso", "year", "water", "barren", "snow_ice", "urban", "dense_forest",
                        "open_forest", "forest_cropland", "herbaceous", "cropland", "shrub", "herbaceous_cropland"))  %>% 
        replace(is.na(.), 0)  %>% 
        mutate(land_area_km2 = barren + snow_ice + urban + dense_forest + open_forest +
                    forest_cropland + herbaceous + cropland + shrub + herbaceous_cropland)  %>% 
        dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, id, iso, year, land_area_km2)) %>%
        mutate(iso = ifelse(iso == "Ala", "USA", iso))

# load GDP
# Note: here I also want the area in square km based on a spherical approximation of the Earth

pred_0_25deg_with_prov_bound <- predict_data_results_0_25deg_with_prov_boundary %>%
                            as.data.frame() %>% 
                            dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_0_25deg, pred_GCP_share_0_25deg_rescaled, pred_GCP_0_25deg))  %>% 
                            left_join(pop)  %>% 
                            left_join(land_area)  %>% 
                            mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>% # some small islands have population, but landcover data do not able to catch them, so ignore this problem
                            na.omit() # there is one cell for SAU that have missing data purely because of country border geometry differences from different sources, ignore it.
                            
# no extra adjustment
pred_0_25deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_0_25deg_with_prov_bound  %>%
                                        mutate(pred_GCP_share_0_25deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_0_25deg))  %>%
                                        mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_0_25deg_rescaled = ifelse(pred_GCP_share_0_25deg == 0, 0, pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl) 

organized_pred_0_25deg_postadjust_pop_dens_no_extra_adjust <- pred_0_25deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust  %>% 
                                        group_by(iso, year, cell_id, subcell_id, subcell_id_0_25)  %>% 
                                        mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
                                        mutate(pred_GCP_0_25deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>% 
                                        ungroup()  %>% 
                                        as.data.frame()  %>%                                         
                                        dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pred_GCP_0_25deg_no_prov_bound, is_cell_censored))  %>% 
                                        distinct(iso, year, cell_id, subcell_id, subcell_id_0_25, .keep_all = TRUE)  %>%
                                        rename(predicted_GCP = pred_GCP_0_25deg_no_prov_bound)  %>%
                                        dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP, is_cell_censored))  %>%
                                        mutate(method = "post-adjust zero GDP for pop density = 0",
                                                cell_size = "0_25-deg by 0_25-deg") 

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pop_cell_0_25deg.RData")

just_grid_0_25deg_with_lon_lat <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/just_grid_0_25deg_with_lon_lat.csv")  %>% 
  mutate(cell_id = as.character(cell_id))

GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_0_25deg_postadjust_pop_dens_no_extra_adjust  %>% 
            left_join(pop_cell_0_25deg %>% as.data.frame() %>% dplyr::select(-c(geom))) %>% 
            mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
            mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled)) %>% 
            dplyr::select(-c(pop_cell))  %>% 
            rename(pop_cell = pop_cell_rescaled) %>% 
            left_join(just_grid_0_25deg_with_lon_lat)  %>% 
            as.data.frame()

write.csv(GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust, file = "step7_robust_analysis/model_tune_MSE/outputs/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_m_mse.csv", row.names = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# 1 degree trained using on year 2012 to 2019

#load the model
load("step7_robust_analysis/model_tune_MSE/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_1deg_up_to_2019.RData")
rf_model_good <- rf_model9_good_grid_search_1deg # because the name of the df in "rf_model9_good_grid_search_1deg_up_to_2019.RData" is "rf_model9_good_grid_search_1deg", so don't make it wrong

#load province/country GDP data
province_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") 
                       
#load predictors dataset
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_1deg.RData")
predict_data_complete <- predictors_put_in_model_1deg  %>% 
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
  left_join(province_GDP)

#obtain training dataset, remember to use oob predictions for the training dataset

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_1deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_1deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_1deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_1deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_1deg.csv") 

# oob predictions obtained during the model training
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "4_put_all_isos_to_train_xdeg_up_to_2019.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
    filter(year <= 2019)  %>% 
    rename(id = iso)  %>% 
    mutate(pred_GCP_share_1deg = pred_train_sam[,1])  %>%  # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "4_put_all_isos_to_train_xdeg_up_to_2019.R").
    dplyr::select(c(cell_id, id, year, pred_GCP_share_1deg))  %>% 
    mutate(cell_id = as.character(cell_id))  %>% 
    mutate(id = ifelse(substr(id,1,4) == "USA_", substr(id,5,6), id)) # so to match with "predict_data_complete"

# ------------------------------
# Now we can predict them

predictions_model <- as.data.frame(predict(object = rf_model_good, new_data = predict_data_complete))

# replace the training sample's predictions with oob predictions
predictions_predict <- predict_data_complete %>%
  left_join(data_full)  %>% 
  mutate(pred_model = predictions_model[,1])  %>% 
  mutate(pred_GCP_share_1deg = ifelse(!is.na(pred_GCP_share_1deg), pred_GCP_share_1deg, pred_model)) %>%
  as.data.frame()  %>% 
  dplyr::select(c(pred_GCP_share_1deg))

raw_predictions_1deg_model_upto_2019 <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_1deg = predictions_predict[,1])

predict_data_results_1deg_with_prov_boundary_model_up_to_2019 <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_1deg = predictions_predict[,1])  %>% 
                              mutate(pred_GCP_share_1deg = ifelse(pop_share == 0, 0, pred_GCP_share_1deg))  %>%                               
                              group_by(id, year)  %>% 
                              mutate(pred_GCP_share_1deg_rescaled = pred_GCP_share_1deg/sum(pred_GCP_share_1deg))  %>% 
                              ungroup()  %>% 
                              mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

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

# load GDP
# Note: here I also want the area in square km based on a spherical approximation of the Earth

pred_1deg_with_prov_bound <- predict_data_results_1deg_with_prov_boundary_model_up_to_2019 %>% 
                             dplyr::select(c(cell_id, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_1deg, pred_GCP_share_1deg_rescaled, pred_GCP_1deg, geom))  %>% 
                             left_join(pop)  %>% 
                             left_join(land_area)  %>% 
                             mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>% 
                             st_as_sf()  %>% 
                             na.omit()

# no extra adjustment
pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_1deg_with_prov_bound  %>% 
                                        mutate(pred_GCP_share_1deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_1deg))  %>%
                                        mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>% 
                                        group_by(id, year)  %>% 
                                        mutate(pred_GCP_share_1deg_rescaled = ifelse(pred_GCP_share_1deg == 0, 0, pred_GCP_share_1deg/sum(pred_GCP_share_1deg)))  %>% 
                                        ungroup()  %>% 
                                        mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019 <- pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust
save(pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019, file = "step7_robust_analysis/model_tune_MSE/outputs/pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019.RData")

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
                                                cell_size = "1-deg by 1-deg") 

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pop_cell_1deg.RData")

# no extra adjustment
GDPC_1deg_postadjust_pop_dens_no_extra_adjust_up_to_2019 <- organized_pred_1deg_postadjust_pop_dens_no_extra_adjust  %>% 
             left_join(pop_cell_1deg) %>%
             mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>% 
             mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>% 
             dplyr::select(-c(pop_cell))  %>% 
             rename(pop_cell = pop_cell_rescaled)

save(GDPC_1deg_postadjust_pop_dens_no_extra_adjust_up_to_2019, file = "step7_robust_analysis/model_tune_MSE/outputs/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_up_to_2019.RData")
