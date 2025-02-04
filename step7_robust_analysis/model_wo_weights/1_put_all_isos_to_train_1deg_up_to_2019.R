# --------------------------------- Task Summary --------------------------------- #
# This file trains the 1-degree random forest model using data from 2012 to 2019.
# Train the model without weights
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

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

# Dynamically set working directory based on PBS environment
if (!is.null(Sys.getenv("PBS_O_WORKDIR")) && Sys.getenv("PBS_O_WORKDIR") != "") {
  setwd(Sys.getenv("PBS_O_WORKDIR"))
}

# ------------------------------------------------- #
# obtain full training data
data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_1deg.csv") 
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_1deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_1deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_1deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_1deg.csv") 

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

# ----------- calculate real world developed cell and developing cell share ----------- #
devling_dped_list <- read_excel("step4_train_and_tune_log_change/inputs/list_developed_developing.xlsx")
developing_isos <- devling_dped_list[,"developing"]

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_1deg.RData")

real_share <- predictors_put_in_model_1deg  %>% 
    mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>% 
    as.data.frame() %>% 
    dplyr::select(-c(geom)) %>% 
    distinct(cell_id, iso) %>% 
    mutate(is_developing = ifelse(iso %in% developing_isos$developing, 1, 0)) %>% 
    group_by(is_developing) %>%
    summarise(count = n()) %>% 
    mutate(share = count / sum(count))

# ----------- end of calculating share ----------- #

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  mutate(unit_gdp_af_sum_rescl = state_total_GDP) %>% 
  mutate(is_developing = ifelse(iso %in% developing_group, 1, 0)) 

data_full <- data_full  %>%  filter(year <= 2019)

##############################################################################################################################
`%notin%` <- Negate(`%in%`)

# Since our main task is to use some countries data to predict other countries, the usual cross validation (randomly separate data into x folds) does not work
# what we want is to randomly pick some countries data to train, and predict on the rest countries to select the best hyperparameters
# thus here we use group_vfold_cv()

set.seed(1234567)
folds <- group_vfold_cv(data_full, group = "iso", v = 5)

set.seed(1234567)
train_rf <- function(data_full, df.cv = folds, name = "RF", tune_par = T){
  
  target_var <- "GCP_share_1deg"
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
                           trees = c(500,750,1000),
                           min_n = c(10,13,16,19))

    tune_hyperparameters <- function(params, data_full, df.cv) {

      cat("Tuning hyperparameters for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "\n") # the codes take very long time to run, this is tell us where it is now

      gdp_losses_developed <- numeric(length(df.cv$splits))
      gdp_losses_developing <- numeric(length(df.cv$splits))
      gdp_losses_all <- numeric(length(df.cv$splits))
      mse_developed <- numeric(length(df.cv$splits))
      mse_developing <- numeric(length(df.cv$splits))
      mse_all <- numeric(length(df.cv$splits))
      r2_developed <- numeric(length(df.cv$splits))
      r2_developing <- numeric(length(df.cv$splits))   
      r2_all <- numeric(length(df.cv$splits))        
      chan_r2_developed <- numeric(length(df.cv$splits))
      chan_r2_developing <- numeric(length(df.cv$splits))   
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
            mutate(pred_GCP_share_1deg = preds[,1]) %>% 
            mutate(pred_GCP_share_1deg = ifelse(pop_share == 0, 0, pred_GCP_share_1deg))
        
        developed <- assessment_with_preds %>% filter(is_developing == 0)
        developing <- assessment_with_preds %>% filter(is_developing == 1)

        mse_developed[i] <- calculate_mse(developed$GCP_share_1deg, developed$pred_GCP_share_1deg)
        mse_developing[i] <- calculate_mse(developing$GCP_share_1deg, developing$pred_GCP_share_1deg)
        mse_all[i] <- calculate_mse(assessment_with_preds$GCP_share_1deg, assessment_with_preds$pred_GCP_share_1deg)

        # calculate r2 for log GDP
        assessment_with_preds <- assessment %>%
            mutate(pred_GCP_share_1deg = preds[,1]) %>%
            mutate(pred_GCP_share_1deg = ifelse(pop_share == 0, 0, pred_GCP_share_1deg))  %>% 
            group_by(iso, year) %>%
            mutate(pred_GCP_share_1deg_rescaled = pred_GCP_share_1deg / sum(pred_GCP_share_1deg)) %>%
            ungroup() %>%
            mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * state_total_GDP)

        developed <- assessment_with_preds %>% filter(is_developing == 0)
        developing <- assessment_with_preds %>% filter(is_developing == 1)

        r2_developed[i] <- calculate_r2(developed$GCP_1deg, developed$pred_GCP_1deg)
        r2_developing[i] <- calculate_r2(developing$GCP_1deg, developing$pred_GCP_1deg)
        r2_all[i] <- calculate_r2(assessment_with_preds$GCP_1deg, assessment_with_preds$pred_GCP_1deg)

        # Calculate GDP_loss
        # the formula for GDP_loss has a real value 2 in the denominator because misallocated GDP will be counted twice in the nominator
        gdp_loss_df <- assessment_with_preds  %>% 
                    group_by(iso, year)  %>% 
                    mutate(GDP_loss = sum(abs(GCP_1deg - pred_GCP_1deg)) / (2*sum(GCP_1deg)))  %>% # this tells us how many percentage of the country's national GDP is misallocated
                    ungroup()  %>% 
                    dplyr::select(c("iso", "year", "GDP_loss", "is_developing")) %>%
                    distinct()

        gdp_losses_developed[i] <- sum(gdp_loss_df$GDP_loss[gdp_loss_df$is_developing == 0])
        gdp_losses_developing[i] <- sum(gdp_loss_df$GDP_loss[gdp_loss_df$is_developing == 1])
        gdp_losses_all[i] <- sum(gdp_loss_df$GDP_loss)

        # Calculate r2 for annual growth rate
        assessment_with_preds <- assessment_with_preds %>%
          arrange(iso, cell_id, year) %>%
          group_by(iso, cell_id) %>%
          mutate(prev_year_pred = ifelse(year - 1 %in% year, pred_GCP_1deg[match(year - 1, year)], NA),
                prev_year_true = ifelse(year - 1 %in% year, GCP_1deg[match(year - 1, year)], NA)) %>%
          ungroup() %>% 
          filter(!is.na(prev_year_pred) & !is.na(prev_year_true))        

        developed <- assessment_with_preds %>% filter(is_developing == 0)
        developing <- assessment_with_preds %>% filter(is_developing == 1)

        chan_r2_developed[i] <- calculate_chan_r2(developed$GCP_1deg, developed$prev_year_true, developed$pred_GCP_1deg, developed$prev_year_pred)
        chan_r2_developing[i] <- calculate_chan_r2(developing$GCP_1deg, developing$prev_year_true, developing$pred_GCP_1deg, developing$prev_year_pred)
        chan_r2_all[i] <- calculate_chan_r2(assessment_with_preds$GCP_1deg, assessment_with_preds$prev_year_true, assessment_with_preds$pred_GCP_1deg, assessment_with_preds$prev_year_pred)

      }

      mean_mse_developed <- mean(mse_developed)
      mean_mse_developing <- mean(mse_developing)
      wgt_mse <- mean_mse_developed*real_share$share[real_share$is_developing == 0] + mean_mse_developing*real_share$share[real_share$is_developing == 1]
      mean_mse_all <- mean(mse_all)

      mean_r2_developed <- mean(r2_developed)
      mean_r2_developing <- mean(r2_developing)
      wgt_r2 <- mean_r2_developed*real_share$share[real_share$is_developing == 0] + mean_r2_developing*real_share$share[real_share$is_developing == 1]
      mean_r2_all <- mean(r2_all)

      mean_gdp_loss_developed <- sum(gdp_losses_developed)/nrow(data_full %>% filter(is_developing == 0) %>% group_by(iso, year) %>% summarise(.groups = "drop"))
      mean_gdp_loss_developing <- sum(gdp_losses_developing)/nrow(data_full %>% filter(is_developing == 1) %>% group_by(iso, year) %>% summarise(.groups = "drop"))      
      wgt_gdp_loss <- mean_gdp_loss_developed*real_share$share[real_share$is_developing == 0] + mean_gdp_loss_developing*real_share$share[real_share$is_developing == 1]
      mean_gdp_loss_all <- sum(gdp_losses_all)/nrow(data_full %>% group_by(iso, year) %>% summarise(.groups = "drop"))      

      mean_chan_r2_developed <- mean(chan_r2_developed)
      mean_chan_r2_developing <- mean(chan_r2_developing)
      wgt_chan_r2 <- mean_chan_r2_developed*real_share$share[real_share$is_developing == 0] + mean_chan_r2_developing*real_share$share[real_share$is_developing == 1]
      mean_chan_r2_all <- mean(chan_r2_all)

      cat("MSE developed for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_mse_developed, "\n")
      cat("MSE developing for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_mse_developing, "\n")
      cat("MSE all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_mse_all, "\n")
      cat("Weighted MSE for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", wgt_mse, "\n")

      cat("r2 developed for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_r2_developed, "\n")
      cat("r2 developing for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_r2_developing, "\n")
      cat("r2 all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_r2_all, "\n")
      cat("Weighted r2 for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", wgt_r2, "\n")
  
      cat("Mean GDP Loss developed for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_gdp_loss_developed, "\n") # tell us the results 
      cat("Mean GDP Loss developing for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_gdp_loss_developing, "\n")
      cat("Mean GDP Loss all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_gdp_loss_all, "\n")
      cat("Weighted mean GDP Loss for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", wgt_gdp_loss, "\n")

      cat("chan r2 developed for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_chan_r2_developed, "\n")
      cat("chan r2 developing for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_chan_r2_developing, "\n")
      cat("chan r2 all wo weight for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", mean_chan_r2_all, "\n")
      cat("Weighted chan r2 for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, ": ", wgt_chan_r2, "\n")
  
      return(data.frame(mtry = params$mtry, trees = params$trees, min_n = params$min_n, 

                        mean_mse_developed = mean_mse_developed,
                        mean_mse_developing = mean_mse_developing,
                        mean_mse_all = mean_mse_all,
                        wgt_mse = wgt_mse,

                        mean_r2_developed = mean_r2_developed,
                        mean_r2_developing = mean_r2_developing,
                        mean_r2_all = mean_r2_all,
                        wgt_r2 = wgt_r2,

                        mean_gdp_loss_developed = mean_gdp_loss_developed, 
                        mean_gdp_loss_developing = mean_gdp_loss_developing, 
                        wgt_gdp_loss = wgt_gdp_loss, 
                        mean_gdp_loss_all = mean_gdp_loss_all,
                        
                        mean_chan_r2_developed = mean_chan_r2_developed,
                        mean_chan_r2_developing = mean_chan_r2_developing,
                        mean_chan_r2_all = mean_chan_r2_all,
                        wgt_chan_r2 = wgt_chan_r2))

    }

    RNGkind("L'Ecuyer-CMRG")
    set.seed(1234567)
    results <- mclapply(1:nrow(rf_grid), mc.cores = 12, function(i) {
      params <- rf_grid[i,]
      tune_hyperparameters(params, data_full, df.cv)
    }, mc.preschedule = TRUE)

    tuning_results <- bind_rows(results)   
    tuning_results_1deg <- tuning_results
    save(tuning_results_1deg, file = "step7_robust_analysis/model_wo_weights/outputs/model9_tuning/put_all_isos_to_train/tuning_results_1deg_up_to_2019.RData")

    param_final <- tuning_results_1deg %>%
      arrange(desc(wgt_chan_r2)) %>%
      slice(1) # pick the hyperparameters that generate the largest r2

    best_model_metrics <- tuning_results_1deg %>%
      arrange(desc(wgt_chan_r2)) %>%
      slice(1) %>%  # Select the row with the best parameters
      select(mtry, trees, min_n,
            mean_mse_developed, mean_mse_developing, mean_mse_all, wgt_mse,
            mean_r2_developed, mean_r2_developing, mean_r2_all, wgt_r2,
            mean_gdp_loss_developed, mean_gdp_loss_developing, mean_gdp_loss_all, wgt_gdp_loss,
            mean_chan_r2_developed, mean_chan_r2_developing, mean_chan_r2_all, wgt_chan_r2)

    write.csv(best_model_metrics, file = "step7_robust_analysis/model_wo_weights/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_1deg_up_to_2019.csv", row.names = FALSE)

    rf_model_final <- rand_forest(mtry = param_final$mtry, trees = param_final$trees, min_n = param_final$min_n) %>%
      set_engine("ranger", importance = "impurity", verbose = T,
                num.threads = 20, seed = 1234567) %>%
      set_mode("regression")

    rf_workflow_final <- workflow() %>% 
      add_recipe(rf_recipe) %>% 
      add_model(rf_model_final) 

    save(rf_workflow_final, file = "step7_robust_analysis/model_wo_weights/outputs/model9_tuning/put_all_isos_to_train/rf_workflow_final_1deg_up_to_2019.RData")
    toc()
  }
  
  tic(paste0("fitting ", name))
  rf_fit <- fit(rf_workflow_final, data = data_full)
  rf_fit_1deg <- rf_fit
  save(rf_fit_1deg, file = "step7_robust_analysis/model_wo_weights/outputs/model9_tuning/put_all_isos_to_train/rf_fit_1deg_up_to_2019.RData")
  toc()
  
  return(rf_fit)
}

set.seed(1234567)
tic("Train RF")
rf_model9_good_grid_search_1deg <- train_rf(data_full = data_full)
save(rf_model9_good_grid_search_1deg, file = "step7_robust_analysis/model_wo_weights/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_1deg_up_to_2019.RData")
toc()






