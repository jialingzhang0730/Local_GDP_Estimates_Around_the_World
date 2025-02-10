# --------------------------------- Task Summary --------------------------------- #
# This file trains the 0.5-degree random forest model using data from 2012 to 2021.
# Developing countries data are not included in the training sample.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load packages
library(randomForest)
library(dplyr)
library(ranger)
library(tidyverse)
library(magrittr)
library(tictoc)
library(gdata)
library(ranger)
library(tidymodels)
library(speedglm)
library(vip)
library(data.table)
library(parallel)
library(readxl)
library(units)
library(sf)
library(tmaptools)
library(plotly)
library(htmlwidgets)

# ------------------------------------------------- #
# obtain full training data
data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_0_5deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_0_5deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_5deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_0_5deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_0_5deg.csv") 

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  filter(!iso %in% developing_group) %>% 
  mutate(unit_gdp_af_sum_rescl = state_total_GDP) 

##############################################################################################################################
`%notin%` <- Negate(`%in%`)

# Since our main task is to use some countries data to predict other countries, the usual cross validation (randomly separate data into x folds) does not work
# what we want is to randomly pick some countries data to train, and predict on the rest countries to select the best hyperparameters
# thus here we use group_vfold_cv()

set.seed(1234567)
folds <- group_vfold_cv(data_full, group = "iso", v = 5) 

set.seed(1234567)
train_rf <- function(data_full, df.cv = folds, name = "RF", tune_par = T){
  
  target_var <- "GCP_share_0_5deg"
  predictor_vars <- c("pop_share", "CO2_bio_manuf_conbust_share", "CO2_bio_heavy_indus_share", "CO2_bio_tspt_share",
                      "CO2_non_org_manuf_conbust_share", "CO2_non_org_heavy_indus_share", "CO2_non_org_tspt_share", "NPP_share",
                      "NTL_urban_snow_free_period_share", "NTL_cropland_snow_free_period_share", "NTL_other_snow_free_period_share",
                      "snow_ice_share", "water_share", "urban_share", "forest_share", "cropland_share", "mean_rug", "national_gdpc",
                      "lag_NTL_urban_share", "lag_urban_share", "lag_cropland_share", "lag_NTL_other_share", "lag_NTL_cropland_share", 
                      "lag_CO2_bio_mc_share", "lag_CO2_nonorg_mc_share", "lag_CO2_bio_heavy_indus_share", "lag_CO2_non_org_heavy_indus_share",
                      "lag_CO2_bio_tspt_share", "lag_CO2_non_org_tspt_share", "lag_pop_share", "lag_NPP_share")
  formula = as.formula(paste(target_var, "~", paste(predictor_vars, collapse = " + ")))

  rf_recipe <- recipe(formula = formula, data = data_full)

  if(tune_par){
    tic(paste0(name, "tuning parameters"))

    # Find a reasonable range for the hyperparameters are hard, but for random forest, the results do not change dramatically around the default values.
    # 1. Range for `mtry` (number of variables randomly sampled as candidates at each split):
    #    - usually people use [max(1, p/5), min(p, p/2)] for estimating level, p is total number of predictors, 
    # 2. Number of trees (`ntree`):
    #    - Options: 500, 750, 1000, people usually do not go beyond this because it requires too much memory
    # 3. Minimum node size (`min_n`):
    #    - Lower bound: 10, because we care more about the generalization of the model
    #    - Increase if the model suggests larger values, can use increment = 3
    # if you find better hyperparameters and the results are dramatically improved, let us know.

    rf_grid <- expand.grid(mtry = c(7,10,13,16),
                           trees = c(750,1000), # for 0.5deg, trees = 500 might be too few, remove this choice to reduce computational costs
                           min_n = c(10,13,16,19))
                           
    tune_hyperparameters <- function(params, data_full, df.cv) {

      cat("Tuning hyperparameters for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "\n") # the codes take very long time to run, this is tell us where it is now

      gdp_losses_all <- numeric(length(df.cv$splits))
      mse_all <- numeric(length(df.cv$splits)) 
      r2_all <- numeric(length(df.cv$splits))     
      chan_r2_all <- numeric(length(df.cv$splits))    

      calculate_r2 <- function(true_values, predicted_values) {
        valid <- true_values > 0 & predicted_values > 0
        true_log <- log(true_values[valid])
        predicted_log <- log(predicted_values[valid])        
        1 - (sum((true_log - predicted_log)^2) / sum((true_log - mean(true_log))^2))
      }

      calculate_mse <- function(true_values, predicted_values) {
        mean((true_values - predicted_values)^2)
      }

      calculate_chan_r2 <- function(true_values, true_last, predicted_values, predicted_last) {
        valid <- true_values > 0 & predicted_values > 0 & true_last > 0 & predicted_last > 0
        true_log <- log(true_values[valid])
        true_last_log <- log(true_last[valid])
        true_log_diff <- true_log - true_last_log
        pred_log <- log(predicted_values[valid])   
        pred_last_log <- log(predicted_last[valid])
        pred_log_diff <- pred_log - pred_last_log
        1 - (sum((true_log_diff - pred_log_diff)^2) / sum((true_log_diff - mean(true_log_diff))^2))
      }
            
      for (i in seq_along(df.cv$splits)) {

        analysis <- as.data.frame(analysis(df.cv$splits[[i]]))
        assessment <- as.data.frame(assessment(df.cv$splits[[i]]))

        # fit the model using training folds
        fit <- rand_forest(mtry = params$mtry, trees = params$trees, min_n = params$min_n) %>%
          set_engine("ranger", verbose = FALSE, seed = 1234567) %>%
          set_mode("regression") %>%
          fit(formula, data = analysis)

        # assess the model using testing fold
        preds <- as.data.frame(predict(fit, assessment))

        # Calculate Weighted MSE
        assessment_with_preds <- assessment %>%
            mutate(pred_GCP_share_0_5deg = preds[,1]) %>% 
            mutate(pred_GCP_share_0_5deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_5deg))

        mse_all[i] <- calculate_mse(assessment_with_preds$GCP_share_0_5deg, assessment_with_preds$pred_GCP_share_0_5deg)

        # calculate r2 for log GDP
        assessment_with_preds <- assessment %>%
            mutate(pred_GCP_share_0_5deg = preds[,1]) %>%
            mutate(pred_GCP_share_0_5deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_5deg))  %>% 
            group_by(iso, year) %>%
            mutate(pred_GCP_share_0_5deg_rescaled = pred_GCP_share_0_5deg / sum(pred_GCP_share_0_5deg)) %>%
            ungroup() %>%
            mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * state_total_GDP)

        r2_all[i] <- calculate_r2(assessment_with_preds$GCP_0_5deg, assessment_with_preds$pred_GCP_0_5deg)

        # Calculate GDP_loss
        # the formula for GDP_loss has a real value 2 in the denominator because misallocated GDP will be counted twice in the nominator
        gdp_loss_df <- assessment_with_preds  %>% 
                    group_by(iso, year)  %>% 
                    mutate(GDP_loss = sum(abs(GCP_0_5deg - pred_GCP_0_5deg)) / (2*sum(GCP_0_5deg)))  %>% # this tells us how many percentage of the country's national GDP is misallocated
                    ungroup()  %>% 
                    dplyr::select(c("iso", "year", "GDP_loss")) %>%
                    distinct()

        gdp_losses_all[i] <- sum(gdp_loss_df$GDP_loss)

        # Calculate r2 for annual growth rate
        assessment_with_preds <- assessment_with_preds %>%
          arrange(iso, cell_id, subcell_id, year) %>%
          group_by(iso, cell_id, subcell_id) %>%
          mutate(prev_year_pred = ifelse(year - 1 %in% year, pred_GCP_0_5deg[match(year - 1, year)], NA),
                prev_year_true = ifelse(year - 1 %in% year, GCP_0_5deg[match(year - 1, year)], NA)) %>%
          ungroup() %>% 
          filter(!is.na(prev_year_pred) & !is.na(prev_year_true))        

        chan_r2_all[i] <- calculate_chan_r2(assessment_with_preds$GCP_0_5deg, assessment_with_preds$prev_year_true, assessment_with_preds$pred_GCP_0_5deg, assessment_with_preds$prev_year_pred)

        # clear some spaces, otherwise the memory used is too large
        rm(preds) 
        rm(fit)
        rm(assessment_with_preds)
        rm(analysis)
        rm(assessment)
        gc()
 
      }

      mean_mse_all <- mean(mse_all)

      mean_r2_all <- mean(r2_all)

      mean_gdp_loss_all <- sum(gdp_losses_all)/nrow(data_full %>% group_by(iso, year) %>% summarise(.groups = "drop"))      

      mean_chan_r2_all <- mean(chan_r2_all)

      cat("MSE all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_mse_all, "\n")
      cat("r2 all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_r2_all, "\n")
      cat("Mean GDP Loss all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_gdp_loss_all, "\n")
      cat("chan r2 all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_chan_r2_all, "\n")
  
      return(data.frame(mtry = params$mtry, trees = params$trees, min_n = params$min_n, 

                        mean_mse_all = mean_mse_all,

                        mean_r2_all = mean_r2_all,

                        mean_gdp_loss_all = mean_gdp_loss_all,

                        mean_chan_r2_all = mean_chan_r2_all))

    }

    RNGkind("L'Ecuyer-CMRG")
    set.seed(1234567)
    results <- mclapply(1:nrow(rf_grid), mc.cores = 8, function(i) {
      params <- rf_grid[i,]
      tune_hyperparameters(params, data_full, df.cv)
    }, mc.preschedule = TRUE)

    tuning_results <- bind_rows(results)   
    tuning_results_0_5deg <- tuning_results
    save(tuning_results_0_5deg, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/tuning_results_0_5deg.RData")

    param_final <- tuning_results_0_5deg %>%
      arrange(desc(mean_chan_r2_all)) %>%
      slice(1) # pick the hyperparameters that generate the largest r2

    best_model_metrics <- tuning_results_0_5deg %>%
      arrange(desc(mean_chan_r2_all)) %>%
      slice(1) %>%  # Select the row with the best parameters
      select(mtry, trees, min_n,
            mean_mse_all, 
            mean_r2_all, 
            mean_gdp_loss_all, 
            mean_chan_r2_all)

    write.csv(best_model_metrics, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_0_5deg.csv", row.names = FALSE)

    rf_model_final <- rand_forest(mtry = param_final$mtry, trees = param_final$trees, min_n = param_final$min_n) %>%
      set_engine("ranger", importance = "impurity", verbose = T,
                num.threads = 20, seed = 1234567) %>%
      set_mode("regression")

    rf_workflow_final <- workflow() %>% 
      add_recipe(rf_recipe) %>% 
      add_model(rf_model_final) 

    save(rf_workflow_final, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/rf_workflow_final_0_5deg.RData")
    toc()
  }
  
  tic(paste0("fitting ", name))
  rf_fit <- fit(rf_workflow_final, data = data_full)
  rf_fit_0_5deg <- rf_fit
  save(rf_fit_0_5deg, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/rf_fit_0_5deg.RData")
  toc()
  
  return(rf_fit)
}

set.seed(1234567)
tic("Train RF")
rf_model9_good_grid_search_0_5deg <- train_rf(data_full = data_full)
save(rf_model9_good_grid_search_0_5deg, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_5deg.RData")
toc()





