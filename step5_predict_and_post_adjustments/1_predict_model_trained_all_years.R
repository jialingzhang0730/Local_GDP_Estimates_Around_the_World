# --------------------------------- Task Summary --------------------------------- #
# This file predicts cell-level GDP at all three resolutions using the random forest models trained in step 4, and extracts per-tree predictions for downstream uncertainty propagation.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tictoc)
library(gdata)
library(sf)
library(parallel)
library(tidyverse)
library(fs)
library(dplyr)
library(data.table)
library(ranger)
library(tmaptools)
library(scales)
library(workflows)

# ---------------------------------------------------------------------------------------------------------------------------------------
# 1 degree

#load the model
load("step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_1deg.RData")
rf_model_good <- rf_model9_good_grid_search_1deg

#load province/country GDP data
province_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") 

#load predictors dataset
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_1deg.RData")

predict_data_complete <- predictors_put_in_model_1deg %>% 
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
  left_join(province_GDP) 

#obtain training dataset, remember to use oob predictions for the training dataset

data_train <- read.csv("step4_benchmark_model/outputs/new_data_train_1deg.csv")
data_valid_year <- read.csv("step4_benchmark_model/outputs/new_data_valid_year_1deg.csv")  
data_valid_iso <- read.csv("step4_benchmark_model/outputs/new_data_valid_iso_1deg.csv")  
data_test_year <- read.csv("step4_benchmark_model/outputs/new_data_test_year_1deg.csv") 
data_test_iso <- read.csv("step4_benchmark_model/outputs/new_data_test_iso_1deg.csv") 

# oob predictions obtained during the model training
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "2_put_all_isos_to_train_xdeg.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  rename(id = iso)  %>% 
  mutate(pred_GCP_share_1deg = pred_train_sam[,1])  %>%  # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "2_put_all_isos_to_train_xdeg.R").
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
save(raw_predictions_1deg_model_all_years, file = "step5_predict_and_post_adjustments/outputs/raw_predictions_1deg_model_all_years.RData")

# ---- Per-tree predictions for uncertainty propagation ----
if (inherits(rf_model_good, "workflow")) {
  ranger_obj <- extract_fit_engine(rf_model_good)
  prepped_data <- bake(extract_recipe(rf_model_good), new_data = predict_data_complete)
} else if (inherits(rf_model_good, "model_fit")) {
  ranger_obj <- rf_model_good$fit
  prepped_data <- predict_data_complete
} else {
  ranger_obj <- rf_model_good
  prepped_data <- predict_data_complete
}
# Predict all trees; store full result to control reference counting
.pred_result <- predict(ranger_obj, data = as.data.frame(prepped_data), predict.all = TRUE)
rm(ranger_obj, prepped_data); gc()

# Extract matrix then drop the result list so matrix refcount == 1
tree_preds_raw_1deg <- .pred_result$predictions
num_trees_1deg <- ncol(tree_preds_raw_1deg)
rm(.pred_result); gc()
cat(paste0("  Number of trees: ", num_trees_1deg, "\n"))

# Zero out predictions for zero-population cells (same logic as point estimate)
# With refcount == 1 this modifies in-place (no copy-on-modify)
tree_preds_raw_1deg[floor(predict_data_complete$pop_total) == 0, ] <- 0

# Save tree-level raw shares for downstream uncertainty propagation (script 4)
save(tree_preds_raw_1deg, num_trees_1deg,
     file = "step5_predict_and_post_adjustments/outputs/tree_preds_raw_1deg.RData")
cat("  Saved per-tree predictions for uncertainty propagation.\n")

predict_data_results_1deg_with_prov_boundary <- predict_data_complete %>%
  as.data.frame()  %>%
  mutate(tree_row_idx = row_number())  %>%
  mutate(pred_GCP_share_1deg = predictions_predict[,1])  %>% 
  mutate(pred_GCP_share_1deg = ifelse(floor(pop_total) == 0, 0, pred_GCP_share_1deg))  %>%                               
  group_by(id, year)  %>% 
  mutate(pred_GCP_share_1deg_rescaled = pred_GCP_share_1deg/sum(pred_GCP_share_1deg))  %>% 
  ungroup()  %>% 
  mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * unit_gdp_af_sum_rescl) 

save(predict_data_results_1deg_with_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_1deg_with_prov_boundary.RData")

predict_data_results_1deg_without_prov_boundary <- predict_data_results_1deg_with_prov_boundary  %>% 
  group_by(iso, year, cell_id)  %>% 
  mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_1deg))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, country_total_GDP, national_population)) 
save(predict_data_results_1deg_without_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_1deg_without_prov_boundary.RData")

# ---- Free 1-degree objects before starting 0.5-degree ----
rm(rf_model_good, rf_model9_good_grid_search_1deg,
   predictors_put_in_model_1deg,
   raw_predictions_1deg_model_all_years,
   tree_preds_raw_1deg, num_trees_1deg,
   predict_data_results_1deg_with_prov_boundary,
   predict_data_results_1deg_without_prov_boundary,
   predict_data_complete, predictions_model, predictions_predict,
   data_full, data_train, data_valid_year, data_valid_iso,
   data_test_year, data_test_iso, pred_train_sam, province_GDP)
gc()

# ---------------------------------------------------------------------------------------------------------------------------------------
# 0.5 degree

#load the model
load("step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_5deg.RData")
rf_model_good <- rf_model9_good_grid_search_0_5deg

#load province/country GDP data
province_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") 

#load predictors dataset
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_0_5deg.RData")
predict_data_complete <- predictors_put_in_model_0_5deg  %>% 
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
  left_join(province_GDP)  

#obtain training dataset, remember to use oob predictions for the training dataset

data_train <- read.csv("step4_benchmark_model/outputs/new_data_train_0_5deg.csv")
data_valid_year <- read.csv("step4_benchmark_model/outputs/new_data_valid_year_0_5deg.csv")  
data_valid_iso <- read.csv("step4_benchmark_model/outputs/new_data_valid_iso_0_5deg.csv")  
data_test_year <- read.csv("step4_benchmark_model/outputs/new_data_test_year_0_5deg.csv") 
data_test_iso <- read.csv("step4_benchmark_model/outputs/new_data_test_iso_0_5deg.csv") 

# oob predictions obtained during the model training
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "2_put_all_isos_to_train_xdeg.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  rename(id = iso)  %>% 
  mutate(pred_GCP_share_0_5deg = pred_train_sam[,1])  %>%  # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "2_put_all_isos_to_train_xdeg.R").
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
save(raw_predictions_0_5deg_model_all_years, file = "step5_predict_and_post_adjustments/outputs/raw_predictions_0_5deg_model_all_years.RData")

# ---- Per-tree predictions for uncertainty propagation ----
if (inherits(rf_model_good, "workflow")) {
  ranger_obj <- extract_fit_engine(rf_model_good)
  prepped_data <- bake(extract_recipe(rf_model_good), new_data = predict_data_complete)
} else if (inherits(rf_model_good, "model_fit")) {
  ranger_obj <- rf_model_good$fit
  prepped_data <- predict_data_complete
} else {
  ranger_obj <- rf_model_good
  prepped_data <- predict_data_complete
}
# Predict all trees; store full result to control reference counting
.pred_result <- predict(ranger_obj, data = as.data.frame(prepped_data), predict.all = TRUE)

# Free model and all objects no longer needed BEFORE extracting the matrix
# (predictions_predict and predict_data_complete are still needed downstream)
rm(ranger_obj, prepped_data, rf_model_good, rf_model9_good_grid_search_0_5deg,
   predictors_put_in_model_0_5deg,
   predictions_model, raw_predictions_0_5deg_model_all_years,
   data_full, data_train, data_valid_year, data_valid_iso,
   data_test_year, data_test_iso, pred_train_sam, province_GDP)
gc()

# Extract matrix then drop the result list so matrix refcount == 1
tree_preds_raw_0_5deg <- .pred_result$predictions
num_trees_0_5deg <- ncol(tree_preds_raw_0_5deg)
rm(.pred_result); gc()
cat(paste0("  Number of trees: ", num_trees_0_5deg, "\n"))

# With refcount == 1 this modifies in-place (no copy-on-modify)
tree_preds_raw_0_5deg[floor(predict_data_complete$pop_total) == 0, ] <- 0

save(tree_preds_raw_0_5deg, num_trees_0_5deg,
     file = "step5_predict_and_post_adjustments/outputs/tree_preds_raw_0_5deg.RData")
cat("  Saved per-tree predictions for 0.5deg uncertainty propagation.\n")

predict_data_results_0_5deg_with_prov_boundary <- predict_data_complete %>%
  as.data.frame()  %>%
  mutate(tree_row_idx = row_number())  %>%
  mutate(pred_GCP_share_0_5deg = predictions_predict[,1])  %>% 
  mutate(pred_GCP_share_0_5deg = ifelse(floor(pop_total) == 0, 0, pred_GCP_share_0_5deg))  %>%                               
  group_by(id, year)  %>% 
  mutate(pred_GCP_share_0_5deg_rescaled = pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg))  %>% 
  ungroup()  %>% 
  mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl) 

save(predict_data_results_0_5deg_with_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_0_5deg_with_prov_boundary.RData")

predict_data_results_0_5deg_without_prov_boundary <- predict_data_results_0_5deg_with_prov_boundary  %>% 
  group_by(iso, year, cell_id, subcell_id)  %>% 
  mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, country_total_GDP, national_population)) 
save(predict_data_results_0_5deg_without_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_0_5deg_without_prov_boundary.RData")

predict_data_results_1deg_from_0_5deg_without_prov_boundary <- predict_data_results_0_5deg_with_prov_boundary  %>% 
  group_by(iso, year, cell_id)  %>% 
  mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, country_total_GDP, national_population)) 
save(predict_data_results_1deg_from_0_5deg_without_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_1deg_from_0_5deg_without_prov_boundary.RData")

# ---- Free 0.5-degree objects before starting 0.25-degree ----
rm(tree_preds_raw_0_5deg, num_trees_0_5deg,
   predict_data_results_0_5deg_with_prov_boundary,
   predict_data_results_0_5deg_without_prov_boundary,
   predict_data_results_1deg_from_0_5deg_without_prov_boundary,
   predict_data_complete, predictions_predict)
gc()

# ---------------------------------------------------------------------------------------------------------------------------------------
# 0.25 degree

#load the model
load("step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_25deg.RData")
rf_model_good <- rf_model9_good_grid_search_0_25deg

#load province/country GDP data
province_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") 

#load predictors dataset
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_0_25deg.RData")
predict_data_complete <- predictors_put_in_model_0_25deg  %>% 
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
  left_join(province_GDP) 

#obtain training dataset, remember to use oob predictions for the training dataset

data_train <- read.csv("step4_benchmark_model/outputs/new_data_train_0_25deg.csv")
data_valid_year <- read.csv("step4_benchmark_model/outputs/new_data_valid_year_0_25deg.csv")  
data_valid_iso <- read.csv("step4_benchmark_model/outputs/new_data_valid_iso_0_25deg.csv")  
data_test_year <- read.csv("step4_benchmark_model/outputs/new_data_test_year_0_25deg.csv") 
data_test_iso <- read.csv("step4_benchmark_model/outputs/new_data_test_iso_0_25deg.csv") 

# oob predictions obtained during the model training
# Important !!!!!: out-of-bag (OOB) predictions should correspond to the rows in the same order as they appear in the data_full dataset in "2_put_all_isos_to_train_xdeg.R"
pred_train_sam <- as.data.frame(rf_model_good$fit$predictions)

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  rename(id = iso)  %>% 
  mutate(pred_GCP_share_0_25deg = pred_train_sam[,1])  %>%  # Important!!! Make sure the order of observations is the same as the order in your training sample (i.e., "2_put_all_isos_to_train_xdeg.R").
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
save(raw_predictions_0_25deg_model_all_years, file = "step5_predict_and_post_adjustments/outputs/raw_predictions_0_25deg_model_all_years.RData")

# ---- Per-tree predictions for uncertainty propagation ----
if (inherits(rf_model_good, "workflow")) {
  ranger_obj <- extract_fit_engine(rf_model_good)
  prepped_data <- bake(extract_recipe(rf_model_good), new_data = predict_data_complete)
} else if (inherits(rf_model_good, "model_fit")) {
  ranger_obj <- rf_model_good$fit
  prepped_data <- predict_data_complete
} else {
  ranger_obj <- rf_model_good
  prepped_data <- predict_data_complete
}
# Predict all trees; store full result to control reference counting
.pred_result <- predict(ranger_obj, data = as.data.frame(prepped_data), predict.all = TRUE)

# Free model and all objects no longer needed BEFORE extracting the matrix
# (predictions_predict and predict_data_complete are still needed downstream)
rm(ranger_obj, prepped_data, rf_model_good, rf_model9_good_grid_search_0_25deg,
   predictors_put_in_model_0_25deg,
   predictions_model, raw_predictions_0_25deg_model_all_years,
   data_full, data_train, data_valid_year, data_valid_iso,
   data_test_year, data_test_iso, pred_train_sam, province_GDP)
gc()

# Extract matrix then drop the result list so matrix refcount == 1
tree_preds_raw_0_25deg <- .pred_result$predictions
num_trees_0_25deg <- ncol(tree_preds_raw_0_25deg)
rm(.pred_result); gc()
cat(paste0("  Number of trees: ", num_trees_0_25deg, "\n"))

# With refcount == 1 this modifies in-place (no copy-on-modify)
tree_preds_raw_0_25deg[floor(predict_data_complete$pop_total) == 0, ] <- 0

save(tree_preds_raw_0_25deg, num_trees_0_25deg,
     file = "step5_predict_and_post_adjustments/outputs/tree_preds_raw_0_25deg.RData")
cat("  Saved per-tree predictions for 0.25deg uncertainty propagation.\n")

predict_data_results_0_25deg_with_prov_boundary <- predict_data_complete %>%
  as.data.frame()  %>%
  mutate(tree_row_idx = row_number())  %>%
  mutate(pred_GCP_share_0_25deg = predictions_predict[,1])  %>% 
  mutate(pred_GCP_share_0_25deg = ifelse(floor(pop_total) == 0, 0, pred_GCP_share_0_25deg))  %>%                               
  group_by(id, year)  %>% 
  mutate(pred_GCP_share_0_25deg_rescaled = pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg))  %>% 
  ungroup()  %>% 
  mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl) 

save(predict_data_results_0_25deg_with_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_0_25deg_with_prov_boundary.RData")

predict_data_results_0_25deg_without_prov_boundary <- predict_data_results_0_25deg_with_prov_boundary  %>% 
  group_by(iso, year, cell_id, subcell_id, subcell_id_0_25)  %>% 
  mutate(pred_GCP_0_25deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pred_GCP_0_25deg_no_prov_bound, country_total_GDP, national_population)) 
save(predict_data_results_0_25deg_without_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_0_25deg_without_prov_boundary.RData")

predict_data_results_1deg_from_0_25deg_without_prov_boundary <- predict_data_results_0_25deg_with_prov_boundary  %>% 
  group_by(iso, year, cell_id)  %>% 
  mutate(pred_GCP_1deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, iso, year, pred_GCP_1deg_no_prov_bound, country_total_GDP, national_population)) 
save(predict_data_results_1deg_from_0_25deg_without_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_1deg_from_0_25deg_without_prov_boundary.RData")

predict_data_results_0_5deg_from_0_25deg_without_prov_boundary <- predict_data_results_0_25deg_with_prov_boundary  %>% 
  group_by(iso, year, cell_id, subcell_id)  %>% 
  mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, country_total_GDP, national_population)) 
save(predict_data_results_0_5deg_from_0_25deg_without_prov_boundary, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_0_5deg_from_0_25deg_without_prov_boundary.RData")

