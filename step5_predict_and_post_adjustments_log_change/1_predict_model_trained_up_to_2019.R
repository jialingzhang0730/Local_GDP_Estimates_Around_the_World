# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to predict the rest of the cells using the model trained in step4 "4_put_all_isos_to_train_xdeg_up_to_2019.R"
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
load("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_1deg_up_to_2019.RData")
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

save(predict_data_results_1deg_with_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_with_prov_boundary_model_up_to_2019.RData")

predict_data_results_1deg_without_prov_boundary_model_up_to_2019 <- predict_data_results_1deg_with_prov_boundary_model_up_to_2019  %>% 
                                                   group_by(iso, year, cell_id)  %>% 
                                                   mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_1deg))  %>% 
                                                   ungroup()  %>% 
                                                   dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, country_total_GDP, national_population, geom)) 
save(predict_data_results_1deg_without_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_without_prov_boundary_model_up_to_2019.RData")

# ---------------------------------------------------------------------------------------------------------------------------------------
# 0.5 degree

#load the model
load("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_5deg_up_to_2019.RData")
rf_model_good <- rf_model9_good_grid_search_0_5deg

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
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "4_put_all_isos_to_train_xdeg_up_to_2019.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
    filter(year <= 2019)  %>% 
    rename(id = iso)  %>% 
    mutate(pred_GCP_share_0_5deg = pred_train_sam[,1])  %>%   # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "4_put_all_isos_to_train_xdeg_up_to_2019.R").
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

raw_predictions_0_5deg_model_upto_2019 <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_5deg = predictions_predict[,1])

predict_data_results_0_5deg_with_prov_boundary_model_up_to_2019 <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_5deg = predictions_predict[,1])  %>% 
                              mutate(pred_GCP_share_0_5deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_5deg))  %>%                               
                              group_by(id, year)  %>% 
                              mutate(pred_GCP_share_0_5deg_rescaled = pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg))  %>% 
                              ungroup()  %>% 
                              mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl) 

save(predict_data_results_0_5deg_with_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_with_prov_boundary_model_up_to_2019.RData")

predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019 <- predict_data_results_0_5deg_with_prov_boundary_model_up_to_2019  %>% 
                                                   group_by(iso, year, cell_id, subcell_id)  %>% 
                                                   mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>% 
                                                   ungroup()  %>% 
                                                   dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, country_total_GDP, national_population, geom)) 
save(predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019.RData")

predict_data_results_1deg_from_0_5deg_without_prov_boundary_model_up_to_2019 <- predict_data_results_0_5deg_with_prov_boundary_model_up_to_2019  %>% 
                                                   group_by(iso, year, cell_id)  %>% 
                                                   mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>% 
                                                   ungroup()  %>% 
                                                   dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, country_total_GDP, national_population, geom)) 
save(predict_data_results_1deg_from_0_5deg_without_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_from_0_5deg_without_prov_boundary_model_up_to_2019.RData")


# ---------------------------------------------------------------------------------------------------------------------------------------
# 0.25 degree

#load the model
load("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_25deg_up_to_2019.RData")
rf_model_good <- rf_model9_good_grid_search_0_25deg

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
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "4_put_all_isos_to_train_xdeg_up_to_2019.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
    filter(year <= 2019)  %>% 
    rename(id = iso)  %>% 
    mutate(pred_GCP_share_0_25deg = pred_train_sam[,1])  %>%   # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "4_put_all_isos_to_train_xdeg_up_to_2019.R").
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

raw_predictions_0_25deg_model_upto_2019 <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_25deg = predictions_predict[,1])

predict_data_results_0_25deg_with_prov_boundary_model_up_to_2019 <- predict_data_complete %>%
                              as.data.frame()  %>% 
                              mutate(pred_GCP_share_0_25deg = predictions_predict[,1])  %>% 
                              mutate(pred_GCP_share_0_25deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_25deg))  %>%                               
                              group_by(id, year)  %>% 
                              mutate(pred_GCP_share_0_25deg_rescaled = pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg))  %>% 
                              ungroup()  %>% 
                              mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl) 

save(predict_data_results_0_25deg_with_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_25deg_with_prov_boundary_model_up_to_2019.RData")

predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019 <- predict_data_results_0_25deg_with_prov_boundary_model_up_to_2019  %>% 
                                                   group_by(iso, year, cell_id, subcell_id, subcell_id_0_25)  %>% 
                                                   mutate(pred_GCP_0_25deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>% 
                                                   ungroup()  %>% 
                                                   dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pred_GCP_0_25deg_no_prov_bound, country_total_GDP, national_population, geom)) 
save(predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019.RData")

predict_data_results_1deg_from_0_25deg_without_prov_boundary_model_up_to_2019 <- predict_data_results_0_25deg_with_prov_boundary_model_up_to_2019  %>% 
                                                   group_by(iso, year, cell_id)  %>% 
                                                   mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>% 
                                                   ungroup()  %>% 
                                                   dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, country_total_GDP, national_population, geom)) 
save(predict_data_results_1deg_from_0_25deg_without_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_from_0_25deg_without_prov_boundary_model_up_to_2019.RData")

predict_data_results_0_5deg_from_0_25deg_without_prov_boundary_model_up_to_2019 <- predict_data_results_0_25deg_with_prov_boundary_model_up_to_2019  %>% 
                                                   group_by(iso, year, cell_id, subcell_id)  %>% 
                                                   mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>% 
                                                   ungroup()  %>% 
                                                   dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, country_total_GDP, national_population, geom)) 
save(predict_data_results_0_5deg_from_0_25deg_without_prov_boundary_model_up_to_2019, file = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_from_0_25deg_without_prov_boundary_model_up_to_2019.RData")

