# --------------------------------- Task Summary --------------------------------- #
# This file trains the 1-degree random forest model using data from 2012 to 2022.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load packages
library(dplyr)
library(ranger)
library(tidyverse)
library(magrittr)
library(tictoc)
library(tidymodels)
library(vip)
library(parallel)
library(readxl)

# ------------------------------------------------- #
# obtain full training data
data_train <- read.csv("step4_benchmark_model/outputs/new_data_train_1deg.csv") 
data_valid_year <- read.csv("step4_benchmark_model/outputs/new_data_valid_year_1deg.csv")  
data_valid_iso <- read.csv("step4_benchmark_model/outputs/new_data_valid_iso_1deg.csv")  
data_test_year <- read.csv("step4_benchmark_model/outputs/new_data_test_year_1deg.csv") 
data_test_iso <- read.csv("step4_benchmark_model/outputs/new_data_test_iso_1deg.csv") 

dped_list <- read_excel("step4_benchmark_model/inputs/list_developed_isos.xlsx")
developed_isos <- dped_list[,"developed"]

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso) %>% 
  filter(iso %in% developed_isos$developed) %>%  # Use the column!
  mutate(unit_gdp_af_sum_rescl = state_total_GDP,
         is_developing = 0)  # Add this line - all are developed countries

##############################################################################################################################
# Since our main task is to use some countries data to predict other countries, the usual cross validation (randomly separate data into x folds) does not work
# what we want is to randomly pick some countries data to train, and predict on the rest countries to select the best hyperparameters
# thus here we use group_vfold_cv()

set.seed(1234567)
folds <- group_vfold_cv(data_full, group = "iso", v = 5)

set.seed(1234567)
train_rf <- function(data_full, df.cv = folds, name = "RF", tune_par = T){

  target_var <- "GCP_share_1deg"
  predictor_vars <- c("pop_total_share","pop_urban_share", "pop_cropland_share", "pop_other_share", "CO2_bio_manuf_conbust_share", "CO2_bio_heavy_indus_share", "CO2_bio_tspt_share",
                      "CO2_non_org_manuf_conbust_share", "CO2_non_org_heavy_indus_share", "CO2_non_org_tspt_share", "NPP_share",
                      "NTL_urban_snow_free_period_share", "NTL_cropland_snow_free_period_share", "NTL_other_snow_free_period_share",
                      "snow_ice_share", "water_share", "urban_share", "forest_share", "cropland_share", "mean_rug", "national_gdpc",
                      "lag_NTL_urban_share", "lag_urban_share", "lag_cropland_share", "lag_NTL_other_share", "lag_NTL_cropland_share", 
                      "lag_CO2_bio_mc_share", "lag_CO2_nonorg_mc_share", "lag_CO2_bio_heavy_indus_share", "lag_CO2_non_org_heavy_indus_share",
                      "lag_CO2_bio_tspt_share", "lag_CO2_non_org_tspt_share", "lag_pop_urban_share", "lag_NPP_share", "lag_pop_cropland_share","lag_pop_other_share")
  formula = as.formula(paste(target_var, "~", paste(predictor_vars, collapse = " + ")))

  if(tune_par){
    tic(paste0(name, "tuning parameters"))

    rf_grid <- expand.grid(mtry = c(27,30,32,34),
                           trees = c(1500),
                           min_n = c(1200,1500,1800,2100,2400,2700))

    tune_hyperparameters <- function(params, data_full, df.cv) {

      cat("Tuning hyperparameters for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "\n") # the codes take very long time to run, this is tell us where it is now

      # metric will be saved in those vectors - CORRECTED: using df.cv instead of folds
      mse_GDP_sh_dped_fit <- numeric(length(df.cv$splits))     
      mse_levl_dped_fit <- numeric(length(df.cv$splits))
      mse_chan_dped_fit <- numeric(length(df.cv$splits))
      o_r2_levl_dped_fit <- numeric(length(df.cv$splits))
      o_r2_chan_dped_fit <- numeric(length(df.cv$splits))
      w_r2_levl_dped_fit <- numeric(length(df.cv$splits))
      w_r2_chan_dped_fit <- numeric(length(df.cv$splits))

      mse_GDP_sh_dped <- numeric(length(df.cv$splits))   
      mse_levl_dped <- numeric(length(df.cv$splits))
      mse_chan_dped <- numeric(length(df.cv$splits))
      o_r2_levl_dped <- numeric(length(df.cv$splits))
      o_r2_chan_dped <- numeric(length(df.cv$splits))
      w_r2_levl_dped <- numeric(length(df.cv$splits))
      w_r2_chan_dped <- numeric(length(df.cv$splits))

      # define functions
      MSE_GDP_sh <- function(true_values, predicted_values) {
          mean((true_values - predicted_values)^2)
      }

      MSE_levl <- function(true_values, predicted_values) {
          valid <- true_values > 0 & predicted_values > 0
          true_log <- log(true_values[valid])
          predicted_log <- log(predicted_values[valid])   
          mean((true_log - predicted_log)^2)
      }

      MSE_chan <- function(true_values, true_last, predicted_values, predicted_last) {
          valid <- true_values > 0 & predicted_values > 0 & true_last > 0 & predicted_last > 0
          true_log <- log(true_values[valid])
          true_last_log <- log(true_last[valid])
          true_log_diff <- true_log - true_last_log
          pred_log <- log(predicted_values[valid])   
          pred_last_log <- log(predicted_last[valid])
          pred_log_diff <- pred_log - pred_last_log
          mean((true_log_diff - pred_log_diff)^2)
      }

      overall_r2_levl <- function(true_values, predicted_values) {
          valid <- true_values > 0 & predicted_values > 0
          true_log <- log(true_values[valid])
          predicted_log <- log(predicted_values[valid])        
          1 - (sum((true_log - predicted_log)^2) / sum((true_log - mean(true_log))^2))
      }

      overall_r2_chan <- function(true_values, true_last, predicted_values, predicted_last) {
          valid <- true_values > 0 & predicted_values > 0 & true_last > 0 & predicted_last > 0
          true_log <- log(true_values[valid])
          true_last_log <- log(true_last[valid])
          true_log_diff <- true_log - true_last_log
          pred_log <- log(predicted_values[valid])   
          pred_last_log <- log(predicted_last[valid])
          pred_log_diff <- pred_log - pred_last_log
          1 - (sum((true_log_diff - pred_log_diff)^2) / sum((true_log_diff - mean(true_log_diff))^2))
      }

      within_iso_r2_levl <- function(df, true_var, pred_var) {
        df_af <- df %>% 
          filter({{ true_var }} > 0 & {{ pred_var }} > 0) %>%
          mutate(true_log = log({{ true_var }}),
                pred_log = log({{ pred_var }})) %>%
          group_by(iso, year) %>%
          mutate(iso_mean_true_log = mean(true_log)) %>%
          ungroup()

        rss <- sum((df_af$true_log - df_af$pred_log)^2)
        wss <- sum((df_af$true_log - df_af$iso_mean_true_log)^2)

        1 - rss / wss
      }

      within_iso_r2_chan <- function(df, true_var, true_var_last, pred_var, pred_var_last) {
        df_af <- df %>% 
          filter({{ true_var }} > 0 & {{ true_var_last }} > 0 & {{ pred_var }} > 0 & {{ pred_var_last }} > 0) %>% 
          mutate(true_log = log({{ true_var }}),
                true_log_last = log({{ true_var_last }}),
                pred_log = log({{ pred_var }}),
                pred_log_last = log({{ pred_var_last }}),
                true_log_diff = true_log - true_log_last,
                pred_log_diff = pred_log - pred_log_last) %>%
          group_by(iso, year) %>% 
          mutate(iso_mean_true_log_diff = mean(true_log_diff)) %>% 
          ungroup() 

        rss <- sum((df_af$true_log_diff - df_af$pred_log_diff)^2)
        wss <- sum((df_af$true_log_diff - df_af$iso_mean_true_log_diff)^2)

        1 - rss / wss
      }

      # define the following to save: n
      datapoint_counts <- list()
      var_importance <- list()

      for (i in seq_along(df.cv$splits)) {

        analysis <- as.data.frame(analysis(df.cv$splits[[i]]))
        assessment <- as.data.frame(assessment(df.cv$splits[[i]]))

        # fit the model using training folds
        fit <- rand_forest(mtry = params$mtry, trees = params$trees, min_n = params$min_n) %>%
          set_engine("ranger", verbose = FALSE, importance = "impurity", seed = 1234567) %>%
          set_mode("regression") %>%
          fit(formula, data = analysis)

        var_importance[[i]] <- vi(fit)

        # obtain model fit: note here we care about fit, so do not use out of bag predictions
        analysis_fit <- analysis  %>% 
          mutate(pred_GCP_sh_ns = predict(fit, analysis)$.pred) %>% 
          mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>% 
          group_by(iso, year) %>%
          mutate(pred_GCP_sh = pred_GCP_sh_ns / sum(pred_GCP_sh_ns)) %>%
          ungroup() %>%
          mutate(pred_GCP = pred_GCP_sh * state_total_GDP) %>% 
          arrange(iso, cell_id, year) %>%
          group_by(iso, cell_id) %>%
          mutate(prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_1deg[match(year - 1, year)], NA),
                prev_yr_pred_gdp = ifelse(year - 1 %in% year, pred_GCP[match(year - 1, year)], NA),
                prev_yr_true_gdp_sh = ifelse(year - 1 %in% year, GCP_share_1deg[match(year - 1, year)], NA),
                prev_yr_pred_gdp_sh = ifelse(year - 1 %in% year, pred_GCP_sh[match(year - 1, year)], NA)) %>%
          ungroup() %>% 
          as.data.frame()  

        developed_fit <- analysis_fit %>% filter(is_developing == 0)

        # When checking annual growth, exclude each ISO's first year (no prior data); note start years vary by country
        developed_fit_ch <- developed_fit %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        analysis_fit_ch <- analysis_fit %>% 
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()  

        # ---------
        # now prepare the assessment dataset for the out-of-sample performance: 
        assessment_pred <- assessment %>% 
          mutate(pred_GCP_sh_ns = predict(fit, assessment)$.pred) %>% 
          mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>% 
          group_by(iso, year) %>%
          mutate(pred_GCP_sh = pred_GCP_sh_ns / sum(pred_GCP_sh_ns)) %>%
          ungroup() %>%
          mutate(pred_GCP = pred_GCP_sh * state_total_GDP) %>% 
          arrange(iso, cell_id, year) %>%
          group_by(iso, cell_id) %>%
          mutate(prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_1deg[match(year - 1, year)], NA),
                prev_yr_pred_gdp = ifelse(year - 1 %in% year, pred_GCP[match(year - 1, year)], NA),
                prev_yr_true_gdp_sh = ifelse(year - 1 %in% year, GCP_share_1deg[match(year - 1, year)], NA),
                prev_yr_pred_gdp_sh = ifelse(year - 1 %in% year, pred_GCP_sh[match(year - 1, year)], NA)) %>%
          ungroup() %>% 
          as.data.frame()  

        developed <- assessment_pred %>% filter(is_developing == 0)

        # When checking annual growth, exclude each ISO's first year (no prior data); note start years vary by country
        developed_ch <- developed %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        assessment_pred_ch <- assessment_pred %>% 
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()  

        # before checking the performance, document how many data points
        datapoint_counts[[i]] <- c(
          developed_ch = nrow(developed_ch)
        )

        # check the performance
        # ------- for the within-sample fit ------- # 
        # metric 1: MSE for pred GDP share vs true GDP share
        mse_GDP_sh_dped_fit[i] <- MSE_GDP_sh(developed_fit$GCP_share_1deg, developed_fit$pred_GCP_sh)

        # metric 2: MSE for pred log(GDP) vs true log(GDP)
        mse_levl_dped_fit[i] <- MSE_levl(developed_fit$GCP_1deg, developed_fit$pred_GCP)

        # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        mse_chan_dped_fit[i] <- MSE_chan(developed_fit_ch$GCP_1deg, developed_fit_ch$prev_yr_true_gdp, developed_fit_ch$pred_GCP, developed_fit_ch$prev_yr_pred_gdp)

        # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
        o_r2_levl_dped_fit[i] <- overall_r2_levl(developed_fit$GCP_1deg, developed_fit$pred_GCP)

        # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        o_r2_chan_dped_fit[i] <- overall_r2_chan(developed_fit_ch$GCP_1deg, developed_fit_ch$prev_yr_true_gdp, developed_fit_ch$pred_GCP, developed_fit_ch$prev_yr_pred_gdp)

        # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
        w_r2_levl_dped_fit[i] <- within_iso_r2_levl(developed_fit, GCP_1deg, pred_GCP)

        # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        w_r2_chan_dped_fit[i] <- within_iso_r2_chan(developed_fit_ch, GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)

        # ---------------------------------------------------------------------- # 

        # ------- for the out of sample cross validation predictions ------- # 
        # metric 1: MSE for pred GDP share vs true GDP share
        mse_GDP_sh_dped[i] <- MSE_GDP_sh(developed$GCP_share_1deg, developed$pred_GCP_sh)

        # metric 2: MSE for pred log(GDP) vs true log(GDP)
        mse_levl_dped[i] <- MSE_levl(developed$GCP_1deg, developed$pred_GCP)

        # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        mse_chan_dped[i] <- MSE_chan(developed_ch$GCP_1deg, developed_ch$prev_yr_true_gdp, developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)

        # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
        o_r2_levl_dped[i] <- overall_r2_levl(developed$GCP_1deg, developed$pred_GCP)

        # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        o_r2_chan_dped[i] <- overall_r2_chan(developed_ch$GCP_1deg, developed_ch$prev_yr_true_gdp, developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)

        # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
        w_r2_levl_dped[i] <- within_iso_r2_levl(developed, GCP_1deg, pred_GCP)

        # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        w_r2_chan_dped[i] <- within_iso_r2_chan(developed_ch, GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)

        # ---------------------------------------------------------------------- # 
      }

      metrics_df <- tibble(mtry = params$mtry, trees = params$trees, min_n = params$min_n,    

        mse_GDP_sh_dped_fit = mse_GDP_sh_dped_fit,
        mse_levl_dped_fit = mse_levl_dped_fit,
        mse_chan_dped_fit = mse_chan_dped_fit,
        o_r2_levl_dped_fit = o_r2_levl_dped_fit,
        o_r2_chan_dped_fit = o_r2_chan_dped_fit,
        w_r2_levl_dped_fit = w_r2_levl_dped_fit,
        w_r2_chan_dped_fit = w_r2_chan_dped_fit,
        mse_GDP_sh_dped = mse_GDP_sh_dped,
        mse_levl_dped = mse_levl_dped,
        mse_chan_dped = mse_chan_dped,
        o_r2_levl_dped = o_r2_levl_dped,
        o_r2_chan_dped = o_r2_chan_dped,
        w_r2_levl_dped = w_r2_levl_dped,
        w_r2_chan_dped = w_r2_chan_dped
      )

      write.csv(metrics_df, file = sprintf("step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/detailed_metric_1deg/detail_metrics_%s_%s_%s.csv", params$mtry, params$trees, params$min_n), row.names = FALSE)

      # Save the data points before evaluating model performance:
      # Since all parameter choices use the same dataset, the data points stay the same. It's fine if they're overwritten each time—they won't change.
      datapoint_counts_df <- bind_cols(
        metric_df = names(datapoint_counts[[1]]),
        as.data.frame(datapoint_counts) %>%
          setNames(paste0("N_fold_", seq_along(datapoint_counts)))
      )
      write.csv(datapoint_counts_df, "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/rf_metrics_fold_N_1deg.csv", row.names = FALSE)

      # Save the variable importance scores
      save(var_importance, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/var_imptc_score_1deg.RData")

      # now collect the metrics for each of the five held-out samples:
      # ------- for the within-sample fit ------- # 
      # metric 1: MSE for pred GDP share vs true GDP share
      m_mse_GDP_sh_dped_fit <- mean(mse_GDP_sh_dped_fit)

      # metric 2: MSE for pred log(GDP) vs true log(GDP)
      m_mse_levl_dped_fit <- mean(mse_levl_dped_fit)

      # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_mse_chan_dped_fit <- mean(mse_chan_dped_fit)

      # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
      m_or2_levl_dped_fit <- mean(o_r2_levl_dped_fit)

      # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_or2_chan_dped_fit <- mean(o_r2_chan_dped_fit)

      # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
      m_wr2_levl_dped_fit <- mean(w_r2_levl_dped_fit)

      # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_wr2_chan_dped_fit <- mean(w_r2_chan_dped_fit)

      # ---------------------------------------------------------------------- # 

      # ------- for the out of sample cross validation predictions ------- # 
      # metric 1: MSE for pred GDP share vs true GDP share
      m_mse_GDP_sh_dped <- mean(mse_GDP_sh_dped)

      # metric 2: MSE for pred log(GDP) vs true log(GDP)
      m_mse_levl_dped <- mean(mse_levl_dped)

      # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_mse_chan_dped <- mean(mse_chan_dped)

      # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
      m_or2_levl_dped <- mean(o_r2_levl_dped)

      # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_or2_chan_dped <- mean(o_r2_chan_dped)

      # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
      m_wr2_levl_dped <- mean(w_r2_levl_dped)

      # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_wr2_chan_dped <- mean(w_r2_chan_dped)

      # ---------------------------------------------------------------------- # 

      results <- data.frame(mtry = params$mtry, trees = params$trees, min_n = params$min_n,   
            m_mse_GDP_sh_dped_fit = m_mse_GDP_sh_dped_fit,
            m_mse_levl_dped_fit = m_mse_levl_dped_fit,
            m_mse_chan_dped_fit = m_mse_chan_dped_fit,
            m_or2_levl_dped_fit = m_or2_levl_dped_fit,
            m_or2_chan_dped_fit = m_or2_chan_dped_fit,
            m_wr2_levl_dped_fit = m_wr2_levl_dped_fit,
            m_wr2_chan_dped_fit = m_wr2_chan_dped_fit,
            m_mse_GDP_sh_dped = m_mse_GDP_sh_dped,
            m_mse_levl_dped = m_mse_levl_dped,
            m_mse_chan_dped = m_mse_chan_dped,
            m_or2_levl_dped = m_or2_levl_dped,
            m_or2_chan_dped = m_or2_chan_dped,
            m_wr2_levl_dped = m_wr2_levl_dped,
            m_wr2_chan_dped = m_wr2_chan_dped)

      cat(" --> [mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "]\n\n")
      cat("MSE GDP Share\n")
      cat(" - Developed: ", m_mse_GDP_sh_dped, "\n\n")

      cat("MSE (Level) log(GDP)\n")
      cat(" - Developed: ", m_mse_levl_dped, "\n\n")

      cat("MSE (Change) log(GDP)\n")
      cat(" - Developed: ", m_mse_chan_dped, "\n\n")

      cat("Overall R² (Level) log(GDP)\n")
      cat(" - Developed: ", m_or2_levl_dped, "\n\n")

      cat("Overall R² (Change) log(GDP)\n")
      cat(" - Developed: ", m_or2_chan_dped, "\n\n")

      cat("Within R² (Level) log(GDP)\n")
      cat(" - Developed: ", m_wr2_levl_dped, "\n\n")

      cat("Within R² (Change) log(GDP)\n")
      cat(" - Developed: ", m_wr2_chan_dped, "\n\n")

      return(results)
    }

    set.seed(1234567)
    results <- mclapply(1:nrow(rf_grid), mc.cores = 10, function(i) {
      set.seed(1234567 + i)      
      params <- rf_grid[i,]
      tune_hyperparameters(params, data_full, df.cv)
    }, mc.preschedule = TRUE)

    tuning_results <- bind_rows(results)   
    tuning_results_1deg <- tuning_results
    save(tuning_results_1deg, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/tuning_results_1deg.RData")

    param_final <- tuning_results_1deg %>%
      arrange(desc(m_or2_chan_dped)) %>%
      slice(1) # pick the hyperparameters that generate the largest r2

    best_model_metrics <- tuning_results_1deg %>%
      arrange(desc(m_or2_chan_dped)) %>%
      slice(1) %>%  # Select the row with the best parameters
      pivot_longer(cols = everything(), names_to = "metric", values_to = "value")

    write.csv(best_model_metrics, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_1deg.csv", row.names = FALSE)

    rf_fit <- rand_forest(mtry = param_final$mtry, trees = param_final$trees, min_n = param_final$min_n) %>%
      set_engine("ranger", importance = "impurity", verbose = T, seed = 1234567) %>%
      set_mode("regression") %>%
      fit(formula, data = data_full)

  toc()
  }

  return(rf_fit)
}

set.seed(1234567)
tic("Train RF")
rf_model9_good_grid_search_1deg <- train_rf(data_full = data_full)
save(rf_model9_good_grid_search_1deg, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_1deg.RData")
toc()
