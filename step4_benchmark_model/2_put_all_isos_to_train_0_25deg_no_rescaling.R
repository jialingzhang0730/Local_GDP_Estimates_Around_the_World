# --------------------------------- Task Summary --------------------------------- #
# This file trains the 0.25-degree random forest model using data from 2012 to 2022, omitting the within-country GDP-share rescaling step (Online Appendix Section 8.3).
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
data_train <- read.csv("step4_benchmark_model/outputs/new_data_train_0_25deg.csv")
data_valid_year <- read.csv("step4_benchmark_model/outputs/new_data_valid_year_0_25deg.csv")
data_valid_iso <- read.csv("step4_benchmark_model/outputs/new_data_valid_iso_0_25deg.csv")
data_test_year <- read.csv("step4_benchmark_model/outputs/new_data_test_year_0_25deg.csv")
data_test_iso <- read.csv("step4_benchmark_model/outputs/new_data_test_iso_0_25deg.csv")

# ----------- calculate real world developed cell and developing cell share ----------- #
# we assign weights to the cells in our training sample based on whether they are from developed or developing countries,
#   with the weights reflecting the actual proportion of these cells in the real world.

dped_list <- read_excel("step4_benchmark_model/inputs/list_developed_isos.xlsx")
developed_isos <- dped_list[,"developed"]

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_0_25deg.RData")

real_share <- predictors_put_in_model_0_25deg  %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>%
  as.data.frame() %>%
  distinct(cell_id, subcell_id, subcell_id_0_25, iso) %>%
  mutate(is_developing = ifelse(iso %in% developed_isos$developed, 0, 1)) %>%
  group_by(is_developing) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

# ----------- end of calculating share ----------- #

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>%
  mutate(unit_gdp_af_sum_rescl = state_total_GDP) %>%
  mutate(original_order = row_number()) %>%
  mutate(is_developing = ifelse(iso %in% developed_isos$developed | substr(iso,1,3) == "USA", 0, 1)) %>%
  group_by(is_developing) %>%
  mutate(sample_count = n()) %>%
  ungroup() %>%
  mutate(sample_share = sample_count / n()) %>%
  left_join(real_share) %>%
  mutate(normalized_weight = share / sample_share) %>%
  dplyr::select(-c(sample_count, sample_share, count, share))  %>%
  mutate(import_weight = importance_weights(normalized_weight)) %>% # ensure that the weight can be correctly recognized by the random forest as weights
  arrange(original_order) %>%
  dplyr::select(-c(original_order))

##############################################################################################################################
# Since our main task is to use some countries data to predict other countries, the usual cross validation (randomly separate data into x folds) does not work
# what we want is to randomly pick some countries data to train, and predict on the rest countries to select the best hyperparameters
# thus here we use group_vfold_cv()

set.seed(1234567)
folds <- group_vfold_cv(data_full, group = "iso", v = 5)

set.seed(1234567)
train_rf <- function(data_full, df.cv = folds, name = "RF", tune_par = T){

  target_var <- "GCP_share_0_25deg"
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

    rf_grid <- expand.grid(mtry = c(25,27,29),
                           trees = c(1500),
                           min_n = c(2000,3000,4000,5000,6000))

    tune_hyperparameters <- function(params, data_full, df.cv) {

      cat("Tuning hyperparameters for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "\n") # the codes take very long time to run, this is tell us where it is now

      # metric will be saved in those vectors
      mse_GDP_sh_dped_fit <- numeric(length(folds$splits))
      mse_GDP_sh_dping_fit <- numeric(length(folds$splits))
      mse_GDP_sh_all_fit <- numeric(length(folds$splits))
      mse_levl_dped_fit <- numeric(length(folds$splits))
      mse_levl_dping_fit <- numeric(length(folds$splits))
      mse_levl_all_fit <- numeric(length(folds$splits))
      mse_chan_dped_fit <- numeric(length(folds$splits))
      mse_chan_dping_fit <- numeric(length(folds$splits))
      mse_chan_all_fit <- numeric(length(folds$splits))
      o_r2_levl_dped_fit <- numeric(length(folds$splits))
      o_r2_levl_dping_fit <- numeric(length(folds$splits))
      o_r2_levl_all_fit <- numeric(length(folds$splits))
      o_r2_chan_dped_fit <- numeric(length(folds$splits))
      o_r2_chan_dping_fit <- numeric(length(folds$splits))
      o_r2_chan_all_fit <- numeric(length(folds$splits))
      w_r2_levl_dped_fit <- numeric(length(folds$splits))
      w_r2_levl_dping_fit <- numeric(length(folds$splits))
      w_r2_levl_all_fit <- numeric(length(folds$splits))
      w_r2_chan_dped_fit <- numeric(length(folds$splits))
      w_r2_chan_dping_fit <- numeric(length(folds$splits))
      w_r2_chan_all_fit <- numeric(length(folds$splits))

      mse_GDP_sh_dped <- numeric(length(folds$splits))
      mse_GDP_sh_dping <- numeric(length(folds$splits))
      mse_GDP_sh_all <- numeric(length(folds$splits))
      mse_levl_dped <- numeric(length(folds$splits))
      mse_levl_dping <- numeric(length(folds$splits))
      mse_levl_all <- numeric(length(folds$splits))
      mse_chan_dped <- numeric(length(folds$splits))
      mse_chan_dping <- numeric(length(folds$splits))
      mse_chan_all <- numeric(length(folds$splits))
      o_r2_levl_dped <- numeric(length(folds$splits))
      o_r2_levl_dping <- numeric(length(folds$splits))
      o_r2_levl_all <- numeric(length(folds$splits))
      o_r2_chan_dped <- numeric(length(folds$splits))
      o_r2_chan_dping <- numeric(length(folds$splits))
      o_r2_chan_all <- numeric(length(folds$splits))
      w_r2_levl_dped <- numeric(length(folds$splits))
      w_r2_levl_dping <- numeric(length(folds$splits))
      w_r2_levl_all <- numeric(length(folds$splits))
      w_r2_chan_dped <- numeric(length(folds$splits))
      w_r2_chan_dping <- numeric(length(folds$splits))
      w_r2_chan_all <- numeric(length(folds$splits))

      pre_r2_levl_dped_fit <- numeric(length(folds$splits))
      pre_r2_levl_dping_fit <- numeric(length(folds$splits))
      pre_r2_levl_all_fit <- numeric(length(folds$splits))

      pre_r2_levl_dped <- numeric(length(folds$splits))
      pre_r2_levl_dping <- numeric(length(folds$splits))
      pre_r2_levl_all <- numeric(length(folds$splits))

      # No-rescaling OOS metric vectors
      o_r2_levl_norescale_dped <- numeric(length(folds$splits))
      o_r2_levl_norescale_dping <- numeric(length(folds$splits))
      o_r2_levl_norescale_all <- numeric(length(folds$splits))
      o_r2_chan_norescale_dped <- numeric(length(folds$splits))
      o_r2_chan_norescale_dping <- numeric(length(folds$splits))
      o_r2_chan_norescale_all <- numeric(length(folds$splits))
      w_r2_levl_norescale_dped <- numeric(length(folds$splits))
      w_r2_levl_norescale_dping <- numeric(length(folds$splits))
      w_r2_levl_norescale_all <- numeric(length(folds$splits))
      w_r2_chan_norescale_dped <- numeric(length(folds$splits))
      w_r2_chan_norescale_dping <- numeric(length(folds$splits))
      w_r2_chan_norescale_all <- numeric(length(folds$splits))

      # No-log R2 OOS metrics (after rescaling, raw levels)
      o_r2_levl_dped_nolog <- numeric(length(folds$splits))
      o_r2_levl_dping_nolog <- numeric(length(folds$splits))
      o_r2_levl_all_nolog <- numeric(length(folds$splits))
      o_r2_chan_dped_nolog <- numeric(length(folds$splits))
      o_r2_chan_dping_nolog <- numeric(length(folds$splits))
      o_r2_chan_all_nolog <- numeric(length(folds$splits))

      # MAPE OOS metrics (after rescaling)
      mape_dped <- numeric(length(folds$splits))
      mape_dping <- numeric(length(folds$splits))
      mape_all <- numeric(length(folds$splits))

      # Growth rate error metrics (OOS)
      growth_met_dped <- list(); growth_met_dping <- list(); growth_met_all <- list()
      # Growth rate error metrics (fit)
      growth_met_dped_fit <- list(); growth_met_dping_fit <- list(); growth_met_all_fit <- list()

      # define functions
      MSE_GDP_sh <- function(true_values, predicted_values) {
        mean((true_values - predicted_values)^2)
      }

      MSE_levl <- function(true_values, predicted_values) {
        valid <- true_values > 0 & predicted_values > 0
        if (sum(valid) == 0) return(NA)
        true_log <- log(true_values[valid])
        predicted_log <- log(predicted_values[valid])
        mean((true_log - predicted_log)^2)
      }

      MSE_chan <- function(true_values, true_last, predicted_values, predicted_last) {
        # Create valid mask, treating NA as FALSE
        valid <- !is.na(true_values) & !is.na(true_last) &
          !is.na(predicted_values) & !is.na(predicted_last) &
          true_values > 0 & predicted_values > 0 &
          true_last > 0 & predicted_last > 0

        # Check if we have any valid data
        if(sum(valid) == 0) {
          return(NA)
        }

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
        if (sum(valid) < 2) return(NA)
        true_log <- log(true_values[valid])
        predicted_log <- log(predicted_values[valid])
        1 - (sum((true_log - predicted_log)^2) / sum((true_log - mean(true_log))^2))
      }

      overall_r2_chan <- function(true_values, true_last, predicted_values, predicted_last) {
        # Create valid mask, treating NA as FALSE
        valid <- !is.na(true_values) & !is.na(true_last) &
          !is.na(predicted_values) & !is.na(predicted_last) &
          true_values > 0 & predicted_values > 0 &
          true_last > 0 & predicted_last > 0

        # Check if we have enough valid data
        if(sum(valid) < 2) {  # Need at least 2 points for variance
          return(NA)
        }

        true_log <- log(true_values[valid])
        true_last_log <- log(true_last[valid])
        true_log_diff <- true_log - true_last_log
        pred_log <- log(predicted_values[valid])
        pred_last_log <- log(predicted_last[valid])
        pred_log_diff <- pred_log - pred_last_log

        # Check for zero variance (all values the same)
        if(var(true_log_diff) < 1e-10) {
          return(NA)
        }

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
        if (nrow(df_af) < 2) return(NA)

        rss <- sum((df_af$true_log - df_af$pred_log)^2)
        wss <- sum((df_af$true_log - df_af$iso_mean_true_log)^2)
        if (wss < 1e-10) return(NA)

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
        if (nrow(df_af) < 2) return(NA)

        rss <- sum((df_af$true_log_diff - df_af$pred_log_diff)^2)
        wss <- sum((df_af$true_log_diff - df_af$iso_mean_true_log_diff)^2)
        if (wss < 1e-10) return(NA)

        1 - rss / wss
      }

      # New function for pre-rescaling R2 on the raw share predictions
      pre_rescale_r2 <- function(true_shares, predicted_shares) {
        # This is a standard R-squared calculation, assuming no NAs
        # It directly measures the fit of the raw RF output
        rss <- sum((true_shares - predicted_shares)^2)
        tss <- sum((true_shares - mean(true_shares))^2)
        1 - (rss / tss)
      }

      # No-log R2 for GDP levels (raw, no log transformation)
      overall_r2_levl_nolog <- function(true_values, predicted_values) {
        valid <- true_values > 0 & predicted_values > 0
        if (sum(valid) < 2) return(NA)
        tv <- true_values[valid]
        pv <- predicted_values[valid]
        1 - (sum((tv - pv)^2) / sum((tv - mean(tv))^2))
      }

      # No-log R2 for GDP changes (raw first differences, no log transformation)
      overall_r2_chan_nolog <- function(true_values, true_last, predicted_values, predicted_last) {
        valid <- !is.na(true_values) & !is.na(true_last) &
          !is.na(predicted_values) & !is.na(predicted_last) &
          true_values > 0 & predicted_values > 0 &
          true_last > 0 & predicted_last > 0
        if (sum(valid) < 2) return(NA)
        true_diff <- true_values[valid] - true_last[valid]
        pred_diff <- predicted_values[valid] - predicted_last[valid]
        if (var(true_diff) < 1e-10) return(NA)
        1 - (sum((true_diff - pred_diff)^2) / sum((true_diff - mean(true_diff))^2))
      }

      # Mean Absolute Percentage Error
      mape_fn <- function(true_values, predicted_values) {
        valid <- true_values > 0
        if (sum(valid) == 0) return(NA)
        mean(abs((predicted_values[valid] - true_values[valid]) / true_values[valid])) * 100
      }

      # Growth rate error metrics
      growth_metrics <- function(true_values, true_last, predicted_values, predicted_last) {
        valid <- !is.na(true_values) & !is.na(true_last) &
          !is.na(predicted_values) & !is.na(predicted_last) &
          true_values > 0 & predicted_values > 0 &
          true_last > 0 & predicted_last > 0
        if (sum(valid) == 0) return(list(mean_true_growth = NA, mean_pred_growth = NA,
                                           mean_growth_error = NA, mean_abs_growth_error = NA,
                                           mean_log_change_error = NA, mean_abs_log_change_error = NA))
        tv <- true_values[valid]; tl <- true_last[valid]
        pv <- predicted_values[valid]; pl <- predicted_last[valid]
        true_growth <- (tv - tl) / tl * 100
        pred_growth <- (pv - pl) / pl * 100
        growth_err <- pred_growth - true_growth
        true_log_ch <- log(tv) - log(tl)
        pred_log_ch <- log(pv) - log(pl)
        log_ch_err <- pred_log_ch - true_log_ch
        list(mean_true_growth = mean(true_growth),
             mean_pred_growth = mean(pred_growth),
             mean_growth_error = mean(growth_err),
             mean_abs_growth_error = mean(abs(growth_err)),
             mean_log_change_error = mean(log_ch_err),
             mean_abs_log_change_error = mean(abs(log_ch_err)))
      }

      # define the following to save: n
      datapoint_counts <- list()
      var_importance <- list()
      decile_metrics <- list()

      for (i in seq_along(df.cv$splits)) {

        analysis <- as.data.frame(analysis(df.cv$splits[[i]]))
        assessment <- as.data.frame(assessment(df.cv$splits[[i]]))

        # fit the model using training folds
        fit <- rand_forest(mtry = params$mtry, trees = params$trees, min_n = params$min_n) %>%
          set_engine("ranger", verbose = FALSE, importance = "impurity", seed = 1234567) %>%
          set_mode("regression") %>%
          fit(formula, data = analysis, weights = import_weight)

        var_importance[[i]] <- vi(fit)

        # obtain model fit: note here we care about fit, so do not use out of bag predictions
        analysis_fit <- analysis  %>%
          mutate(pred_GCP_sh_ns = predict(fit, analysis)$.pred)

        # Calculate pre-rescaling R-squared for in-sample fit
        analysis_fit_dped_pre <- analysis_fit %>% filter(is_developing == 0)
        analysis_fit_dping_pre <- analysis_fit %>% filter(is_developing == 1)

        pre_r2_levl_dped_fit[i] <- pre_rescale_r2(analysis_fit_dped_pre[[target_var]], analysis_fit_dped_pre$pred_GCP_sh_ns)
        pre_r2_levl_dping_fit[i] <- pre_rescale_r2(analysis_fit_dping_pre[[target_var]], analysis_fit_dping_pre$pred_GCP_sh_ns)
        pre_r2_levl_all_fit[i] <- pre_rescale_r2(analysis_fit[[target_var]], analysis_fit$pred_GCP_sh_ns)

        analysis_fit <- analysis_fit %>%
          mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>%
          group_by(iso, year) %>%
          mutate(pred_GCP_sh = pred_GCP_sh_ns / sum(pred_GCP_sh_ns)) %>%
          ungroup() %>%
          mutate(pred_GCP = pred_GCP_sh * state_total_GDP) %>%
          arrange(iso, cell_id, subcell_id, subcell_id_0_25, year) %>%
          group_by(iso, cell_id, subcell_id, subcell_id_0_25) %>%
          mutate(prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_0_25deg[match(year - 1, year)], NA),
                 prev_yr_pred_gdp = ifelse(year - 1 %in% year, pred_GCP[match(year - 1, year)], NA),
                 prev_yr_true_gdp_sh = ifelse(year - 1 %in% year, GCP_share_0_25deg[match(year - 1, year)], NA),
                 prev_yr_pred_gdp_sh = ifelse(year - 1 %in% year, pred_GCP_sh[match(year - 1, year)], NA)) %>%
          ungroup() %>%
          as.data.frame()

        developed_fit <- analysis_fit %>% filter(is_developing == 0)
        developing_fit <- analysis_fit %>% filter(is_developing == 1)

        # When checking annual growth, exclude each ISO's first year (no prior data); note start years vary by country
        developed_fit_ch <- developed_fit %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        developing_fit_ch <- developing_fit %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        analysis_fit_ch <- analysis_fit %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        # ---------
        # now prepare the assessment dataset for the out-of-sample performance:
        # Save raw predictions once to avoid double prediction
        raw_preds <- predict(fit, assessment)$.pred

        assessment_pred <- assessment %>%
          mutate(pred_GCP_sh_ns = raw_preds)

        # Calculate pre-rescaling R-squared for out-of-sample predictions
        assessment_pred_dped_pre <- assessment_pred %>% filter(is_developing == 0)
        assessment_pred_dping_pre <- assessment_pred %>% filter(is_developing == 1)

        pre_r2_levl_dped[i] <- pre_rescale_r2(assessment_pred_dped_pre[[target_var]], assessment_pred_dped_pre$pred_GCP_sh_ns)
        pre_r2_levl_dping[i] <- pre_rescale_r2(assessment_pred_dping_pre[[target_var]], assessment_pred_dping_pre$pred_GCP_sh_ns)
        pre_r2_levl_all[i] <- pre_rescale_r2(assessment_pred[[target_var]], assessment_pred$pred_GCP_sh_ns)

        assessment_pred <- assessment_pred %>%
          mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>%
          group_by(iso, year) %>%
          mutate(pred_GCP_sh = pred_GCP_sh_ns / sum(pred_GCP_sh_ns)) %>%
          ungroup() %>%
          mutate(pred_GCP = pred_GCP_sh * state_total_GDP) %>%
          arrange(iso, cell_id, subcell_id, subcell_id_0_25, year) %>%
          group_by(iso, cell_id, subcell_id, subcell_id_0_25) %>%
          mutate(prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_0_25deg[match(year - 1, year)], NA),
                 prev_yr_pred_gdp = ifelse(year - 1 %in% year, pred_GCP[match(year - 1, year)], NA),
                 prev_yr_true_gdp_sh = ifelse(year - 1 %in% year, GCP_share_0_25deg[match(year - 1, year)], NA),
                 prev_yr_pred_gdp_sh = ifelse(year - 1 %in% year, pred_GCP_sh[match(year - 1, year)], NA)) %>%
          ungroup() %>%
          as.data.frame()

        # --- No-rescaling OOS path --- #
        assessment_norescale <- assessment %>%
          mutate(pred_GCP_sh_ns = raw_preds) %>%
          mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>%
          mutate(pred_GCP_norescale = pred_GCP_sh_ns * state_total_GDP) %>%
          arrange(iso, cell_id, subcell_id, subcell_id_0_25, year) %>%
          group_by(iso, cell_id, subcell_id, subcell_id_0_25) %>%
          mutate(prev_yr_pred_gdp_norescale = ifelse(year - 1 %in% year, pred_GCP_norescale[match(year - 1, year)], NA),
                 prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_0_25deg[match(year - 1, year)], NA)) %>%
          ungroup() %>%
          as.data.frame()

        developed <- assessment_pred %>% filter(is_developing == 0)
        developing <- assessment_pred %>% filter(is_developing == 1)

        # When checking annual growth, exclude each ISO's first year (no prior data); note start years vary by country
        developed_ch <- developed %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        developing_ch <- developing %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        assessment_pred_ch <- assessment_pred %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        # No-rescaling subsets for OOS R2
        developed_norescale <- assessment_norescale %>% filter(is_developing == 0)
        developing_norescale <- assessment_norescale %>% filter(is_developing == 1)

        developed_norescale_ch <- developed_norescale %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        developing_norescale_ch <- developing_norescale %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        assessment_norescale_ch <- assessment_norescale %>%
          group_by(iso) %>%
          filter(year != min(year)) %>%
          ungroup()

        # before checking the performance, document how many data points
        datapoint_counts[[i]] <- c(
          developed_ch = nrow(developed_ch),
          developing_ch = nrow(developing_ch),
          assessment_pred_ch = nrow(assessment_pred_ch)
        )

        # check the performance
        # ------- for the within-sample fit ------- #
        # metric 1: MSE for pred GDP share vs true GDP share
        mse_GDP_sh_dped_fit[i] <- MSE_GDP_sh(developed_fit$GCP_share_0_25deg, developed_fit$pred_GCP_sh)
        mse_GDP_sh_dping_fit[i] <- MSE_GDP_sh(developing_fit$GCP_share_0_25deg, developing_fit$pred_GCP_sh)
        mse_GDP_sh_all_fit[i] <- MSE_GDP_sh(analysis_fit$GCP_share_0_25deg, analysis_fit$pred_GCP_sh)

        # metric 2: MSE for pred log(GDP) vs true log(GDP)
        mse_levl_dped_fit[i] <- MSE_levl(developed_fit$GCP_0_25deg, developed_fit$pred_GCP)
        mse_levl_dping_fit[i] <- MSE_levl(developing_fit$GCP_0_25deg, developing_fit$pred_GCP)
        mse_levl_all_fit[i] <- MSE_levl(analysis_fit$GCP_0_25deg, analysis_fit$pred_GCP)

        # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        mse_chan_dped_fit[i] <- MSE_chan(developed_fit_ch$GCP_0_25deg, developed_fit_ch$prev_yr_true_gdp, developed_fit_ch$pred_GCP, developed_fit_ch$prev_yr_pred_gdp)
        mse_chan_dping_fit[i] <- MSE_chan(developing_fit_ch$GCP_0_25deg, developing_fit_ch$prev_yr_true_gdp, developing_fit_ch$pred_GCP, developing_fit_ch$prev_yr_pred_gdp)
        mse_chan_all_fit[i] <- MSE_chan(analysis_fit_ch$GCP_0_25deg, analysis_fit_ch$prev_yr_true_gdp, analysis_fit_ch$pred_GCP, analysis_fit_ch$prev_yr_pred_gdp)

        # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
        o_r2_levl_dped_fit[i] <- overall_r2_levl(developed_fit$GCP_0_25deg, developed_fit$pred_GCP)
        o_r2_levl_dping_fit[i] <- overall_r2_levl(developing_fit$GCP_0_25deg, developing_fit$pred_GCP)
        o_r2_levl_all_fit[i] <- overall_r2_levl(analysis_fit$GCP_0_25deg, analysis_fit$pred_GCP)

        # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        o_r2_chan_dped_fit[i] <- overall_r2_chan(developed_fit_ch$GCP_0_25deg, developed_fit_ch$prev_yr_true_gdp, developed_fit_ch$pred_GCP, developed_fit_ch$prev_yr_pred_gdp)
        o_r2_chan_dping_fit[i] <- overall_r2_chan(developing_fit_ch$GCP_0_25deg, developing_fit_ch$prev_yr_true_gdp, developing_fit_ch$pred_GCP, developing_fit_ch$prev_yr_pred_gdp)
        o_r2_chan_all_fit[i] <- overall_r2_chan(analysis_fit_ch$GCP_0_25deg, analysis_fit_ch$prev_yr_true_gdp, analysis_fit_ch$pred_GCP, analysis_fit_ch$prev_yr_pred_gdp)

        # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
        w_r2_levl_dped_fit[i] <- within_iso_r2_levl(developed_fit, GCP_0_25deg, pred_GCP)
        w_r2_levl_dping_fit[i] <- within_iso_r2_levl(developing_fit, GCP_0_25deg, pred_GCP)
        w_r2_levl_all_fit[i] <- within_iso_r2_levl(analysis_fit, GCP_0_25deg, pred_GCP)

        # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        w_r2_chan_dped_fit[i] <- within_iso_r2_chan(developed_fit_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)
        w_r2_chan_dping_fit[i] <- within_iso_r2_chan(developing_fit_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)
        w_r2_chan_all_fit[i] <- within_iso_r2_chan(analysis_fit_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)

        # Growth rate error metrics (fit)
        growth_met_dped_fit[[i]] <- growth_metrics(developed_fit_ch$GCP_0_25deg, developed_fit_ch$prev_yr_true_gdp,
                                                    developed_fit_ch$pred_GCP, developed_fit_ch$prev_yr_pred_gdp)
        growth_met_dping_fit[[i]] <- growth_metrics(developing_fit_ch$GCP_0_25deg, developing_fit_ch$prev_yr_true_gdp,
                                                     developing_fit_ch$pred_GCP, developing_fit_ch$prev_yr_pred_gdp)
        growth_met_all_fit[[i]] <- growth_metrics(analysis_fit_ch$GCP_0_25deg, analysis_fit_ch$prev_yr_true_gdp,
                                                   analysis_fit_ch$pred_GCP, analysis_fit_ch$prev_yr_pred_gdp)

        # ---------------------------------------------------------------------- #

        # ------- for the out of sample cross validation predictions ------- #
        # metric 1: MSE for pred GDP share vs true GDP share
        mse_GDP_sh_dped[i] <- MSE_GDP_sh(developed$GCP_share_0_25deg, developed$pred_GCP_sh)
        mse_GDP_sh_dping[i] <- MSE_GDP_sh(developing$GCP_share_0_25deg, developing$pred_GCP_sh)
        mse_GDP_sh_all[i] <- MSE_GDP_sh(assessment_pred$GCP_share_0_25deg, assessment_pred$pred_GCP_sh)

        # metric 2: MSE for pred log(GDP) vs true log(GDP)
        mse_levl_dped[i] <- MSE_levl(developed$GCP_0_25deg, developed$pred_GCP)
        mse_levl_dping[i] <- MSE_levl(developing$GCP_0_25deg, developing$pred_GCP)
        mse_levl_all[i] <- MSE_levl(assessment_pred$GCP_0_25deg, assessment_pred$pred_GCP)

        # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        mse_chan_dped[i] <- MSE_chan(developed_ch$GCP_0_25deg, developed_ch$prev_yr_true_gdp, developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)
        mse_chan_dping[i] <- MSE_chan(developing_ch$GCP_0_25deg, developing_ch$prev_yr_true_gdp, developing_ch$pred_GCP, developing_ch$prev_yr_pred_gdp)
        mse_chan_all[i] <- MSE_chan(assessment_pred_ch$GCP_0_25deg, assessment_pred_ch$prev_yr_true_gdp, assessment_pred_ch$pred_GCP, assessment_pred_ch$prev_yr_pred_gdp)

        # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
        o_r2_levl_dped[i] <- overall_r2_levl(developed$GCP_0_25deg, developed$pred_GCP)
        o_r2_levl_dping[i] <- overall_r2_levl(developing$GCP_0_25deg, developing$pred_GCP)
        o_r2_levl_all[i] <- overall_r2_levl(assessment_pred$GCP_0_25deg, assessment_pred$pred_GCP)

        # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        o_r2_chan_dped[i] <- overall_r2_chan(developed_ch$GCP_0_25deg, developed_ch$prev_yr_true_gdp, developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)
        o_r2_chan_dping[i] <- overall_r2_chan(developing_ch$GCP_0_25deg, developing_ch$prev_yr_true_gdp, developing_ch$pred_GCP, developing_ch$prev_yr_pred_gdp)
        o_r2_chan_all[i] <- overall_r2_chan(assessment_pred_ch$GCP_0_25deg, assessment_pred_ch$prev_yr_true_gdp, assessment_pred_ch$pred_GCP, assessment_pred_ch$prev_yr_pred_gdp)

        # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
        w_r2_levl_dped[i] <- within_iso_r2_levl(developed, GCP_0_25deg, pred_GCP)
        w_r2_levl_dping[i] <- within_iso_r2_levl(developing, GCP_0_25deg, pred_GCP)
        w_r2_levl_all[i] <- within_iso_r2_levl(assessment_pred, GCP_0_25deg, pred_GCP)

        # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
        w_r2_chan_dped[i] <- within_iso_r2_chan(developed_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)
        w_r2_chan_dping[i] <- within_iso_r2_chan(developing_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)
        w_r2_chan_all[i] <- within_iso_r2_chan(assessment_pred_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)

        # --- No-log R2 metrics (after rescaling, raw GDP levels) ---
        o_r2_levl_dped_nolog[i] <- overall_r2_levl_nolog(developed$GCP_0_25deg, developed$pred_GCP)
        o_r2_levl_dping_nolog[i] <- overall_r2_levl_nolog(developing$GCP_0_25deg, developing$pred_GCP)
        o_r2_levl_all_nolog[i] <- overall_r2_levl_nolog(assessment_pred$GCP_0_25deg, assessment_pred$pred_GCP)

        o_r2_chan_dped_nolog[i] <- overall_r2_chan_nolog(developed_ch$GCP_0_25deg, developed_ch$prev_yr_true_gdp, developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)
        o_r2_chan_dping_nolog[i] <- overall_r2_chan_nolog(developing_ch$GCP_0_25deg, developing_ch$prev_yr_true_gdp, developing_ch$pred_GCP, developing_ch$prev_yr_pred_gdp)
        o_r2_chan_all_nolog[i] <- overall_r2_chan_nolog(assessment_pred_ch$GCP_0_25deg, assessment_pred_ch$prev_yr_true_gdp, assessment_pred_ch$pred_GCP, assessment_pred_ch$prev_yr_pred_gdp)

        # --- MAPE metrics (after rescaling) ---
        mape_dped[i] <- mape_fn(developed$GCP_0_25deg, developed$pred_GCP)
        mape_dping[i] <- mape_fn(developing$GCP_0_25deg, developing$pred_GCP)
        mape_all[i] <- mape_fn(assessment_pred$GCP_0_25deg, assessment_pred$pred_GCP)

        # --- Growth rate error metrics (OOS) ---
        growth_met_dped[[i]] <- growth_metrics(developed_ch$GCP_0_25deg, developed_ch$prev_yr_true_gdp,
                                                developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)
        growth_met_dping[[i]] <- growth_metrics(developing_ch$GCP_0_25deg, developing_ch$prev_yr_true_gdp,
                                                 developing_ch$pred_GCP, developing_ch$prev_yr_pred_gdp)
        growth_met_all[[i]] <- growth_metrics(assessment_pred_ch$GCP_0_25deg, assessment_pred_ch$prev_yr_true_gdp,
                                               assessment_pred_ch$pred_GCP, assessment_pred_ch$prev_yr_pred_gdp)

        # --- Decile-level metrics (after rescaling) ---
        assessment_pred_decile <- assessment_pred %>%
          mutate(decile = ntile(GCP_0_25deg, 10))

        for (d in 1:10) {
          dec_data <- assessment_pred_decile %>% filter(decile == d)
          dec_data_ch <- dec_data %>% group_by(iso) %>% filter(year != min(year)) %>% ungroup()

          if (nrow(dec_data) > 1 & nrow(dec_data_ch) > 1) {
            decile_metrics[[paste0("fold_", i, "_decile_", d)]] <- data.frame(
              fold = i, decile = d, n_cells = nrow(dec_data),
              o_r2_levl = overall_r2_levl(dec_data$GCP_0_25deg, dec_data$pred_GCP),
              o_r2_chan = overall_r2_chan(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp, dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp),
              o_r2_levl_nolog = overall_r2_levl_nolog(dec_data$GCP_0_25deg, dec_data$pred_GCP),
              o_r2_chan_nolog = overall_r2_chan_nolog(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp, dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp),
              mean_log_error = mean(log(dec_data$pred_GCP) - log(dec_data$GCP_0_25deg)),
              mean_abs_log_error = mean(abs(log(dec_data$pred_GCP) - log(dec_data$GCP_0_25deg))),
              mean_pct_error = mean((dec_data$pred_GCP - dec_data$GCP_0_25deg) / dec_data$GCP_0_25deg) * 100,
              mape = mean(abs(dec_data$pred_GCP - dec_data$GCP_0_25deg) / dec_data$GCP_0_25deg) * 100,
              mean_true_GCP = mean(dec_data$GCP_0_25deg),
              mean_pred_GCP = mean(dec_data$pred_GCP),
              mean_true_growth = growth_metrics(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp,
                                                 dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp)$mean_true_growth,
              mean_pred_growth = growth_metrics(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp,
                                                 dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp)$mean_pred_growth,
              mean_growth_error = growth_metrics(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp,
                                                  dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp)$mean_growth_error,
              mean_abs_growth_error = growth_metrics(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp,
                                                      dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp)$mean_abs_growth_error,
              mean_log_change_error = growth_metrics(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp,
                                                      dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp)$mean_log_change_error,
              mean_abs_log_change_error = growth_metrics(dec_data_ch$GCP_0_25deg, dec_data_ch$prev_yr_true_gdp,
                                                          dec_data_ch$pred_GCP, dec_data_ch$prev_yr_pred_gdp)$mean_abs_log_change_error,
              stringsAsFactors = FALSE
            )
          }
        }

        # ------- No-rescaling OOS R2 metrics ------- #
        # overall R2 level (no rescaling)
        o_r2_levl_norescale_dped[i] <- overall_r2_levl(developed_norescale$GCP_0_25deg, developed_norescale$pred_GCP_norescale)
        o_r2_levl_norescale_dping[i] <- overall_r2_levl(developing_norescale$GCP_0_25deg, developing_norescale$pred_GCP_norescale)
        o_r2_levl_norescale_all[i] <- overall_r2_levl(assessment_norescale$GCP_0_25deg, assessment_norescale$pred_GCP_norescale)

        # overall R2 change (no rescaling)
        o_r2_chan_norescale_dped[i] <- overall_r2_chan(developed_norescale_ch$GCP_0_25deg, developed_norescale_ch$prev_yr_true_gdp, developed_norescale_ch$pred_GCP_norescale, developed_norescale_ch$prev_yr_pred_gdp_norescale)
        o_r2_chan_norescale_dping[i] <- overall_r2_chan(developing_norescale_ch$GCP_0_25deg, developing_norescale_ch$prev_yr_true_gdp, developing_norescale_ch$pred_GCP_norescale, developing_norescale_ch$prev_yr_pred_gdp_norescale)
        o_r2_chan_norescale_all[i] <- overall_r2_chan(assessment_norescale_ch$GCP_0_25deg, assessment_norescale_ch$prev_yr_true_gdp, assessment_norescale_ch$pred_GCP_norescale, assessment_norescale_ch$prev_yr_pred_gdp_norescale)

        # within-country R2 level (no rescaling)
        w_r2_levl_norescale_dped[i] <- within_iso_r2_levl(developed_norescale, GCP_0_25deg, pred_GCP_norescale)
        w_r2_levl_norescale_dping[i] <- within_iso_r2_levl(developing_norescale, GCP_0_25deg, pred_GCP_norescale)
        w_r2_levl_norescale_all[i] <- within_iso_r2_levl(assessment_norescale, GCP_0_25deg, pred_GCP_norescale)

        # within-country R2 change (no rescaling)
        w_r2_chan_norescale_dped[i] <- within_iso_r2_chan(developed_norescale_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP_norescale, prev_yr_pred_gdp_norescale)
        w_r2_chan_norescale_dping[i] <- within_iso_r2_chan(developing_norescale_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP_norescale, prev_yr_pred_gdp_norescale)
        w_r2_chan_norescale_all[i] <- within_iso_r2_chan(assessment_norescale_ch, GCP_0_25deg, prev_yr_true_gdp, pred_GCP_norescale, prev_yr_pred_gdp_norescale)

        # ---------------------------------------------------------------------- #
      }

      # --- Decile summary ---
      decile_df <- bind_rows(decile_metrics)
      decile_summary <- decile_df %>%
        group_by(decile) %>%
        summarise(across(where(is.numeric) & !matches("fold"), mean, na.rm = TRUE), .groups = "drop")

      write.csv(decile_summary, file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/decile_metrics_0_25deg_no_rescaling.csv", row.names = FALSE)

      metrics_df <- tibble(mtry = params$mtry, trees = params$trees, min_n = params$min_n,

                           # --- Pre-rescaling R2 Level (Fit) ---
                           pre_r2_levl_dped_fit = pre_r2_levl_dped_fit,
                           pre_r2_levl_dping_fit = pre_r2_levl_dping_fit,
                           pre_r2_levl_all_fit = pre_r2_levl_all_fit,

                           mse_GDP_sh_dped_fit = mse_GDP_sh_dped_fit,
                           mse_GDP_sh_dping_fit = mse_GDP_sh_dping_fit,
                           mse_GDP_sh_all_fit = mse_GDP_sh_all_fit,

                           mse_levl_dped_fit = mse_levl_dped_fit,
                           mse_levl_dping_fit = mse_levl_dping_fit,
                           mse_levl_all_fit = mse_levl_all_fit,

                           mse_chan_dped_fit = mse_chan_dped_fit,
                           mse_chan_dping_fit = mse_chan_dping_fit,
                           mse_chan_all_fit = mse_chan_all_fit,

                           o_r2_levl_dped_fit = o_r2_levl_dped_fit,
                           o_r2_levl_dping_fit = o_r2_levl_dping_fit,
                           o_r2_levl_all_fit = o_r2_levl_all_fit,

                           o_r2_chan_dped_fit = o_r2_chan_dped_fit,
                           o_r2_chan_dping_fit = o_r2_chan_dping_fit,
                           o_r2_chan_all_fit = o_r2_chan_all_fit,

                           w_r2_levl_dped_fit = w_r2_levl_dped_fit,
                           w_r2_levl_dping_fit = w_r2_levl_dping_fit,
                           w_r2_levl_all_fit = w_r2_levl_all_fit,

                           w_r2_chan_dped_fit = w_r2_chan_dped_fit,
                           w_r2_chan_dping_fit = w_r2_chan_dping_fit,
                           w_r2_chan_all_fit = w_r2_chan_all_fit,

                           mse_GDP_sh_dped = mse_GDP_sh_dped,
                           mse_GDP_sh_dping = mse_GDP_sh_dping,
                           mse_GDP_sh_all = mse_GDP_sh_all,

                           mse_levl_dped = mse_levl_dped,
                           mse_levl_dping = mse_levl_dping,
                           mse_levl_all = mse_levl_all,

                           mse_chan_dped = mse_chan_dped,
                           mse_chan_dping = mse_chan_dping,
                           mse_chan_all = mse_chan_all,

                           o_r2_levl_dped = o_r2_levl_dped,
                           o_r2_levl_dping = o_r2_levl_dping,
                           o_r2_levl_all = o_r2_levl_all,

                           o_r2_chan_dped = o_r2_chan_dped,
                           o_r2_chan_dping = o_r2_chan_dping,
                           o_r2_chan_all = o_r2_chan_all,

                           w_r2_levl_dped = w_r2_levl_dped,
                           w_r2_levl_dping = w_r2_levl_dping,
                           w_r2_levl_all = w_r2_levl_all,

                           w_r2_chan_dped = w_r2_chan_dped,
                           w_r2_chan_dping = w_r2_chan_dping,
                           w_r2_chan_all = w_r2_chan_all,

                           # --- No-rescaling OOS R2 metrics ---
                           o_r2_levl_norescale_dped = o_r2_levl_norescale_dped,
                           o_r2_levl_norescale_dping = o_r2_levl_norescale_dping,
                           o_r2_levl_norescale_all = o_r2_levl_norescale_all,

                           o_r2_chan_norescale_dped = o_r2_chan_norescale_dped,
                           o_r2_chan_norescale_dping = o_r2_chan_norescale_dping,
                           o_r2_chan_norescale_all = o_r2_chan_norescale_all,

                           w_r2_levl_norescale_dped = w_r2_levl_norescale_dped,
                           w_r2_levl_norescale_dping = w_r2_levl_norescale_dping,
                           w_r2_levl_norescale_all = w_r2_levl_norescale_all,

                           w_r2_chan_norescale_dped = w_r2_chan_norescale_dped,
                           w_r2_chan_norescale_dping = w_r2_chan_norescale_dping,
                           w_r2_chan_norescale_all = w_r2_chan_norescale_all,

                           # No-log R2 (after rescaling)
                           o_r2_levl_dped_nolog = o_r2_levl_dped_nolog,
                           o_r2_levl_dping_nolog = o_r2_levl_dping_nolog,
                           o_r2_levl_all_nolog = o_r2_levl_all_nolog,
                           o_r2_chan_dped_nolog = o_r2_chan_dped_nolog,
                           o_r2_chan_dping_nolog = o_r2_chan_dping_nolog,
                           o_r2_chan_all_nolog = o_r2_chan_all_nolog,

                           # MAPE (after rescaling)
                           mape_dped = mape_dped,
                           mape_dping = mape_dping,
                           mape_all = mape_all
      )

      write.csv(metrics_df, file = sprintf("step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/detailed_metric_0_25deg_no_rescaling/detail_metrics_%s_%s_%s.csv", params$mtry, params$trees, params$min_n), row.names = FALSE)

      # Save the data points before evaluating model performance:
      # Since all parameter choices use the same dataset, the data points stay the same. It's fine if they're overwritten each time—they won't change.
      datapoint_counts_df <- bind_cols(
        metric_df = names(datapoint_counts[[1]]),
        as.data.frame(datapoint_counts) %>%
          setNames(paste0("N_fold_", seq_along(datapoint_counts)))
      )
      write.csv(datapoint_counts_df, "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/rf_metrics_fold_N_0_25deg_no_rescaling.csv", row.names = FALSE)

      # Save the variable importance scores
      save(var_importance, file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/var_imptc_score_0_25deg_no_rescaling.RData")

      # now collect the metrics for each of the five held-out samples:
      # ------- for the within-sample fit ------- #

      # Calculate the mean of the new pre-rescaling metrics
      # -- Fit --
      m_prer2_levl_dped_fit <- mean(pre_r2_levl_dped_fit)
      m_prer2_levl_dping_fit <- mean(pre_r2_levl_dping_fit)
      wgtm_prer2_levl_fit <- m_prer2_levl_dped_fit*real_share$share[real_share$is_developing == 0] + m_prer2_levl_dping_fit*real_share$share[real_share$is_developing == 1]
      m_prer2_levl_all_fit <- mean(pre_r2_levl_all_fit)

      # metric 1: MSE for pred GDP share vs true GDP share
      m_mse_GDP_sh_dped_fit <- mean(mse_GDP_sh_dped_fit)
      m_mse_GDP_sh_dping_fit <- mean(mse_GDP_sh_dping_fit)
      wgtm_mse_GDP_sh_fit <- m_mse_GDP_sh_dped_fit*real_share$share[real_share$is_developing == 0] + m_mse_GDP_sh_dping_fit*real_share$share[real_share$is_developing == 1]
      m_mse_GDP_sh_all_fit <- mean(mse_GDP_sh_all)

      # metric 2: MSE for pred log(GDP) vs true log(GDP)
      m_mse_levl_dped_fit <- mean(mse_levl_dped_fit)
      m_mse_levl_dping_fit <- mean(mse_levl_dping_fit)
      wgtm_mse_levl_fit <- m_mse_levl_dped_fit*real_share$share[real_share$is_developing == 0] + m_mse_levl_dping_fit*real_share$share[real_share$is_developing == 1]
      m_mse_levl_all_fit <- mean(mse_levl_all_fit)

      # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_mse_chan_dped_fit <- mean(mse_chan_dped_fit)
      m_mse_chan_dping_fit <- mean(mse_chan_dping_fit)
      wgtm_mse_chan_fit <- m_mse_chan_dped_fit*real_share$share[real_share$is_developing == 0] + m_mse_chan_dping_fit*real_share$share[real_share$is_developing == 1]
      m_mse_chan_all_fit <- mean(mse_chan_all_fit)

      # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
      m_or2_levl_dped_fit <- mean(o_r2_levl_dped_fit)
      m_or2_levl_dping_fit <- mean(o_r2_levl_dping_fit)
      wgtm_or2_levl_fit <- m_or2_levl_dped_fit*real_share$share[real_share$is_developing == 0] + m_or2_levl_dping_fit*real_share$share[real_share$is_developing == 1]
      m_or2_levl_all_fit <- mean(o_r2_levl_all_fit)

      # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_or2_chan_dped_fit <- mean(o_r2_chan_dped_fit)
      m_or2_chan_dping_fit <- mean(o_r2_chan_dping_fit)
      wgtm_or2_chan_fit <- m_or2_chan_dped_fit*real_share$share[real_share$is_developing == 0] + m_or2_chan_dping_fit*real_share$share[real_share$is_developing == 1]
      m_or2_chan_all_fit <- mean(o_r2_chan_all_fit)

      # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
      m_wr2_levl_dped_fit <- mean(w_r2_levl_dped_fit)
      m_wr2_levl_dping_fit <- mean(w_r2_levl_dping_fit)
      wgtm_wr2_levl_fit <- m_wr2_levl_dped_fit*real_share$share[real_share$is_developing == 0] + m_wr2_levl_dping_fit*real_share$share[real_share$is_developing == 1]
      m_wr2_levl_all_fit <- mean(w_r2_levl_all_fit)

      # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_wr2_chan_dped_fit <- mean(w_r2_chan_dped_fit)
      m_wr2_chan_dping_fit <- mean(w_r2_chan_dping_fit)
      wgtm_wr2_chan_fit <- m_wr2_chan_dped_fit*real_share$share[real_share$is_developing == 0] + m_wr2_chan_dping_fit*real_share$share[real_share$is_developing == 1]
      m_wr2_chan_all_fit <- mean(w_r2_chan_all_fit)

      # ---------------------------------------------------------------------- #

      # ------- for the out of sample cross validation predictions ------- #
      # -- OOS --
      m_prer2_levl_dped <- mean(pre_r2_levl_dped)
      m_prer2_levl_dping <- mean(pre_r2_levl_dping)
      wgtm_prer2_levl <- m_prer2_levl_dped*real_share$share[real_share$is_developing == 0] + m_prer2_levl_dping*real_share$share[real_share$is_developing == 1]
      m_prer2_levl_all <- mean(pre_r2_levl_all)

      # metric 1: MSE for pred GDP share vs true GDP share
      m_mse_GDP_sh_dped <- mean(mse_GDP_sh_dped)
      m_mse_GDP_sh_dping <- mean(mse_GDP_sh_dping)
      wgtm_mse_GDP_sh <- m_mse_GDP_sh_dped*real_share$share[real_share$is_developing == 0] + m_mse_GDP_sh_dping*real_share$share[real_share$is_developing == 1]
      m_mse_GDP_sh_all <- mean(mse_GDP_sh_all)

      # metric 2: MSE for pred log(GDP) vs true log(GDP)
      m_mse_levl_dped <- mean(mse_levl_dped)
      m_mse_levl_dping <- mean(mse_levl_dping)
      wgtm_mse_levl <- m_mse_levl_dped*real_share$share[real_share$is_developing == 0] + m_mse_levl_dping*real_share$share[real_share$is_developing == 1]
      m_mse_levl_all <- mean(mse_levl_all)

      # metric 3: MSE for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_mse_chan_dped <- mean(mse_chan_dped)
      m_mse_chan_dping <- mean(mse_chan_dping)
      wgtm_mse_chan <- m_mse_chan_dped*real_share$share[real_share$is_developing == 0] + m_mse_chan_dping*real_share$share[real_share$is_developing == 1]
      m_mse_chan_all <- mean(mse_chan_all)

      # metric 4: overall R2 for pred log(GDP) vs true log(GDP)
      m_or2_levl_dped <- mean(o_r2_levl_dped)
      m_or2_levl_dping <- mean(o_r2_levl_dping)
      wgtm_or2_levl <- m_or2_levl_dped*real_share$share[real_share$is_developing == 0] + m_or2_levl_dping*real_share$share[real_share$is_developing == 1]
      m_or2_levl_all <- mean(o_r2_levl_all)

      # metric 5: overall R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_or2_chan_dped <- mean(o_r2_chan_dped)
      m_or2_chan_dping <- mean(o_r2_chan_dping)
      wgtm_or2_chan <- m_or2_chan_dped*real_share$share[real_share$is_developing == 0] + m_or2_chan_dping*real_share$share[real_share$is_developing == 1]
      m_or2_chan_all <- mean(o_r2_chan_all)

      # metric 6: within-country R2 for pred log(GDP) vs true log(GDP)
      m_wr2_levl_dped <- mean(w_r2_levl_dped)
      m_wr2_levl_dping <- mean(w_r2_levl_dping)
      wgtm_wr2_levl <- m_wr2_levl_dped*real_share$share[real_share$is_developing == 0] + m_wr2_levl_dping*real_share$share[real_share$is_developing == 1]
      m_wr2_levl_all <- mean(w_r2_levl_all)

      # metric 7: within-country R2 for log(pred GDP_t) - log(pred GDP_t-1)  vs log(true GDP_t) - log(true GDP_t-1)
      m_wr2_chan_dped <- mean(w_r2_chan_dped)
      m_wr2_chan_dping <- mean(w_r2_chan_dping)
      wgtm_wr2_chan <- m_wr2_chan_dped*real_share$share[real_share$is_developing == 0] + m_wr2_chan_dping*real_share$share[real_share$is_developing == 1]
      m_wr2_chan_all <- mean(w_r2_chan_all)

      # --- No-rescaling OOS summary means with weighted averages --- #
      # overall R2 level (no rescaling)
      m_or2_levl_norescale_dped <- mean(o_r2_levl_norescale_dped)
      m_or2_levl_norescale_dping <- mean(o_r2_levl_norescale_dping)
      wgtm_or2_levl_norescale <- m_or2_levl_norescale_dped*real_share$share[real_share$is_developing == 0] + m_or2_levl_norescale_dping*real_share$share[real_share$is_developing == 1]
      m_or2_levl_norescale_all <- mean(o_r2_levl_norescale_all)

      # overall R2 change (no rescaling)
      m_or2_chan_norescale_dped <- mean(o_r2_chan_norescale_dped)
      m_or2_chan_norescale_dping <- mean(o_r2_chan_norescale_dping)
      wgtm_or2_chan_norescale <- m_or2_chan_norescale_dped*real_share$share[real_share$is_developing == 0] + m_or2_chan_norescale_dping*real_share$share[real_share$is_developing == 1]
      m_or2_chan_norescale_all <- mean(o_r2_chan_norescale_all)

      # within-country R2 level (no rescaling)
      m_wr2_levl_norescale_dped <- mean(w_r2_levl_norescale_dped)
      m_wr2_levl_norescale_dping <- mean(w_r2_levl_norescale_dping)
      wgtm_wr2_levl_norescale <- m_wr2_levl_norescale_dped*real_share$share[real_share$is_developing == 0] + m_wr2_levl_norescale_dping*real_share$share[real_share$is_developing == 1]
      m_wr2_levl_norescale_all <- mean(w_r2_levl_norescale_all)

      # within-country R2 change (no rescaling)
      m_wr2_chan_norescale_dped <- mean(w_r2_chan_norescale_dped)
      m_wr2_chan_norescale_dping <- mean(w_r2_chan_norescale_dping)
      wgtm_wr2_chan_norescale <- m_wr2_chan_norescale_dped*real_share$share[real_share$is_developing == 0] + m_wr2_chan_norescale_dping*real_share$share[real_share$is_developing == 1]
      m_wr2_chan_norescale_all <- mean(w_r2_chan_norescale_all)

      # --- No-log R2 OOS summary means (after rescaling) ---
      m_or2_levl_dped_nolog <- mean(o_r2_levl_dped_nolog)
      m_or2_levl_dping_nolog <- mean(o_r2_levl_dping_nolog)
      m_or2_levl_all_nolog <- mean(o_r2_levl_all_nolog)
      wgtm_or2_levl_nolog <- m_or2_levl_dped_nolog*real_share$share[real_share$is_developing == 0] + m_or2_levl_dping_nolog*real_share$share[real_share$is_developing == 1]

      m_or2_chan_dped_nolog <- mean(o_r2_chan_dped_nolog)
      m_or2_chan_dping_nolog <- mean(o_r2_chan_dping_nolog)
      m_or2_chan_all_nolog <- mean(o_r2_chan_all_nolog)
      wgtm_or2_chan_nolog <- m_or2_chan_dped_nolog*real_share$share[real_share$is_developing == 0] + m_or2_chan_dping_nolog*real_share$share[real_share$is_developing == 1]

      # --- MAPE OOS summary means ---
      m_mape_dped <- mean(mape_dped)
      m_mape_dping <- mean(mape_dping)
      m_mape_all <- mean(mape_all)
      wgtm_mape <- m_mape_dped*real_share$share[real_share$is_developing == 0] + m_mape_dping*real_share$share[real_share$is_developing == 1]

      # --- Growth rate error OOS summary means ---
      extract_growth_mean <- function(met_list, field) {
        mean(sapply(met_list, function(x) x[[field]]), na.rm = TRUE)
      }

      m_growth_error_dped <- extract_growth_mean(growth_met_dped, "mean_growth_error")
      m_growth_error_dping <- extract_growth_mean(growth_met_dping, "mean_growth_error")
      m_growth_error_all <- extract_growth_mean(growth_met_all, "mean_growth_error")
      wgtm_growth_error <- m_growth_error_dped*real_share$share[real_share$is_developing == 0] + m_growth_error_dping*real_share$share[real_share$is_developing == 1]

      m_abs_growth_error_dped <- extract_growth_mean(growth_met_dped, "mean_abs_growth_error")
      m_abs_growth_error_dping <- extract_growth_mean(growth_met_dping, "mean_abs_growth_error")
      m_abs_growth_error_all <- extract_growth_mean(growth_met_all, "mean_abs_growth_error")
      wgtm_abs_growth_error <- m_abs_growth_error_dped*real_share$share[real_share$is_developing == 0] + m_abs_growth_error_dping*real_share$share[real_share$is_developing == 1]

      m_log_change_error_dped <- extract_growth_mean(growth_met_dped, "mean_log_change_error")
      m_log_change_error_dping <- extract_growth_mean(growth_met_dping, "mean_log_change_error")
      m_log_change_error_all <- extract_growth_mean(growth_met_all, "mean_log_change_error")
      wgtm_log_change_error <- m_log_change_error_dped*real_share$share[real_share$is_developing == 0] + m_log_change_error_dping*real_share$share[real_share$is_developing == 1]

      m_abs_log_change_error_dped <- extract_growth_mean(growth_met_dped, "mean_abs_log_change_error")
      m_abs_log_change_error_dping <- extract_growth_mean(growth_met_dping, "mean_abs_log_change_error")
      m_abs_log_change_error_all <- extract_growth_mean(growth_met_all, "mean_abs_log_change_error")
      wgtm_abs_log_change_error <- m_abs_log_change_error_dped*real_share$share[real_share$is_developing == 0] + m_abs_log_change_error_dping*real_share$share[real_share$is_developing == 1]

      # --- Growth rate error Fit summary means ---
      m_growth_error_dped_fit <- extract_growth_mean(growth_met_dped_fit, "mean_growth_error")
      m_growth_error_dping_fit <- extract_growth_mean(growth_met_dping_fit, "mean_growth_error")
      m_growth_error_all_fit <- extract_growth_mean(growth_met_all_fit, "mean_growth_error")
      wgtm_growth_error_fit <- m_growth_error_dped_fit*real_share$share[real_share$is_developing == 0] + m_growth_error_dping_fit*real_share$share[real_share$is_developing == 1]

      m_abs_growth_error_dped_fit <- extract_growth_mean(growth_met_dped_fit, "mean_abs_growth_error")
      m_abs_growth_error_dping_fit <- extract_growth_mean(growth_met_dping_fit, "mean_abs_growth_error")
      m_abs_growth_error_all_fit <- extract_growth_mean(growth_met_all_fit, "mean_abs_growth_error")
      wgtm_abs_growth_error_fit <- m_abs_growth_error_dped_fit*real_share$share[real_share$is_developing == 0] + m_abs_growth_error_dping_fit*real_share$share[real_share$is_developing == 1]

      m_log_change_error_dped_fit <- extract_growth_mean(growth_met_dped_fit, "mean_log_change_error")
      m_log_change_error_dping_fit <- extract_growth_mean(growth_met_dping_fit, "mean_log_change_error")
      m_log_change_error_all_fit <- extract_growth_mean(growth_met_all_fit, "mean_log_change_error")
      wgtm_log_change_error_fit <- m_log_change_error_dped_fit*real_share$share[real_share$is_developing == 0] + m_log_change_error_dping_fit*real_share$share[real_share$is_developing == 1]

      m_abs_log_change_error_dped_fit <- extract_growth_mean(growth_met_dped_fit, "mean_abs_log_change_error")
      m_abs_log_change_error_dping_fit <- extract_growth_mean(growth_met_dping_fit, "mean_abs_log_change_error")
      m_abs_log_change_error_all_fit <- extract_growth_mean(growth_met_all_fit, "mean_abs_log_change_error")
      wgtm_abs_log_change_error_fit <- m_abs_log_change_error_dped_fit*real_share$share[real_share$is_developing == 0] + m_abs_log_change_error_dping_fit*real_share$share[real_share$is_developing == 1]

      # ---------------------------------------------------------------------- #

      results <- data.frame(mtry = params$mtry, trees = params$trees, min_n = params$min_n,

                            # --- New Pre-rescaling Metrics (Fit) ---
                            m_prer2_levl_dped_fit = m_prer2_levl_dped_fit,
                            m_prer2_levl_dping_fit = m_prer2_levl_dping_fit,
                            wgtm_prer2_levl_fit = wgtm_prer2_levl_fit,
                            m_prer2_levl_all_fit = m_prer2_levl_all_fit,

                            m_mse_GDP_sh_dped_fit = m_mse_GDP_sh_dped_fit,
                            m_mse_GDP_sh_dping_fit = m_mse_GDP_sh_dping_fit,
                            wgtm_mse_GDP_sh_fit = wgtm_mse_GDP_sh_fit,
                            m_mse_GDP_sh_all_fit = m_mse_GDP_sh_all_fit,

                            m_mse_levl_dped_fit = m_mse_levl_dped_fit,
                            m_mse_levl_dping_fit = m_mse_levl_dping_fit,
                            wgtm_mse_levl_fit = wgtm_mse_levl_fit,
                            m_mse_levl_all_fit = m_mse_levl_all_fit,

                            m_mse_chan_dped_fit = m_mse_chan_dped_fit,
                            m_mse_chan_dping_fit = m_mse_chan_dping_fit,
                            wgtm_mse_chan_fit = wgtm_mse_chan_fit,
                            m_mse_chan_all_fit = m_mse_chan_all_fit,

                            m_or2_levl_dped_fit = m_or2_levl_dped_fit,
                            m_or2_levl_dping_fit = m_or2_levl_dping_fit,
                            wgtm_or2_levl_fit = wgtm_or2_levl_fit,
                            m_or2_levl_all_fit = m_or2_levl_all_fit,

                            m_or2_chan_dped_fit = m_or2_chan_dped_fit,
                            m_or2_chan_dping_fit = m_or2_chan_dping_fit,
                            wgtm_or2_chan_fit = wgtm_or2_chan_fit,
                            m_or2_chan_all_fit = m_or2_chan_all_fit,

                            m_wr2_levl_dped_fit = m_wr2_levl_dped_fit,
                            m_wr2_levl_dping_fit = m_wr2_levl_dping_fit,
                            wgtm_wr2_levl_fit = wgtm_wr2_levl_fit,
                            m_wr2_levl_all_fit = m_wr2_levl_all_fit,

                            m_wr2_chan_dped_fit = m_wr2_chan_dped_fit,
                            m_wr2_chan_dping_fit = m_wr2_chan_dping_fit,
                            wgtm_wr2_chan_fit = wgtm_wr2_chan_fit,
                            m_wr2_chan_all_fit = m_wr2_chan_all_fit,

                            # --- New Pre-rescaling Metrics (OOS) ---
                            m_prer2_levl_dped = m_prer2_levl_dped,
                            m_prer2_levl_dping = m_prer2_levl_dping,
                            wgtm_prer2_levl = wgtm_prer2_levl,
                            m_prer2_levl_all = m_prer2_levl_all,

                            m_mse_GDP_sh_dped = m_mse_GDP_sh_dped,
                            m_mse_GDP_sh_dping = m_mse_GDP_sh_dping,
                            wgtm_mse_GDP_sh = wgtm_mse_GDP_sh,
                            m_mse_GDP_sh_all = m_mse_GDP_sh_all,

                            m_mse_levl_dped = m_mse_levl_dped,
                            m_mse_levl_dping = m_mse_levl_dping,
                            wgtm_mse_levl = wgtm_mse_levl,
                            m_mse_levl_all = m_mse_levl_all,

                            m_mse_chan_dped = m_mse_chan_dped,
                            m_mse_chan_dping = m_mse_chan_dping,
                            wgtm_mse_chan = wgtm_mse_chan,
                            m_mse_chan_all = m_mse_chan_all,

                            m_or2_levl_dped = m_or2_levl_dped,
                            m_or2_levl_dping = m_or2_levl_dping,
                            wgtm_or2_levl = wgtm_or2_levl,
                            m_or2_levl_all = m_or2_levl_all,

                            m_or2_chan_dped = m_or2_chan_dped,
                            m_or2_chan_dping = m_or2_chan_dping,
                            wgtm_or2_chan = wgtm_or2_chan,
                            m_or2_chan_all = m_or2_chan_all,

                            m_wr2_levl_dped = m_wr2_levl_dped,
                            m_wr2_levl_dping = m_wr2_levl_dping,
                            wgtm_wr2_levl = wgtm_wr2_levl,
                            m_wr2_levl_all = m_wr2_levl_all,

                            m_wr2_chan_dped = m_wr2_chan_dped,
                            m_wr2_chan_dping = m_wr2_chan_dping,
                            wgtm_wr2_chan = wgtm_wr2_chan,
                            m_wr2_chan_all = m_wr2_chan_all,

                            # --- No-rescaling OOS R2 summary results ---
                            m_or2_levl_norescale_dped = m_or2_levl_norescale_dped,
                            m_or2_levl_norescale_dping = m_or2_levl_norescale_dping,
                            wgtm_or2_levl_norescale = wgtm_or2_levl_norescale,
                            m_or2_levl_norescale_all = m_or2_levl_norescale_all,

                            m_or2_chan_norescale_dped = m_or2_chan_norescale_dped,
                            m_or2_chan_norescale_dping = m_or2_chan_norescale_dping,
                            wgtm_or2_chan_norescale = wgtm_or2_chan_norescale,
                            m_or2_chan_norescale_all = m_or2_chan_norescale_all,

                            m_wr2_levl_norescale_dped = m_wr2_levl_norescale_dped,
                            m_wr2_levl_norescale_dping = m_wr2_levl_norescale_dping,
                            wgtm_wr2_levl_norescale = wgtm_wr2_levl_norescale,
                            m_wr2_levl_norescale_all = m_wr2_levl_norescale_all,

                            m_wr2_chan_norescale_dped = m_wr2_chan_norescale_dped,
                            m_wr2_chan_norescale_dping = m_wr2_chan_norescale_dping,
                            wgtm_wr2_chan_norescale = wgtm_wr2_chan_norescale,
                            m_wr2_chan_norescale_all = m_wr2_chan_norescale_all,

                            # --- No-log R2 OOS metrics (after rescaling) ---
                            m_or2_levl_dped_nolog = m_or2_levl_dped_nolog,
                            m_or2_levl_dping_nolog = m_or2_levl_dping_nolog,
                            m_or2_levl_all_nolog = m_or2_levl_all_nolog,
                            wgtm_or2_levl_nolog = wgtm_or2_levl_nolog,
                            m_or2_chan_dped_nolog = m_or2_chan_dped_nolog,
                            m_or2_chan_dping_nolog = m_or2_chan_dping_nolog,
                            m_or2_chan_all_nolog = m_or2_chan_all_nolog,
                            wgtm_or2_chan_nolog = wgtm_or2_chan_nolog,

                            # --- MAPE OOS metrics (after rescaling) ---
                            m_mape_dped = m_mape_dped,
                            m_mape_dping = m_mape_dping,
                            m_mape_all = m_mape_all,
                            wgtm_mape = wgtm_mape,

                            # --- Growth rate error metrics (Fit) ---
                            m_growth_error_dped_fit = m_growth_error_dped_fit,
                            m_growth_error_dping_fit = m_growth_error_dping_fit,
                            m_growth_error_all_fit = m_growth_error_all_fit,
                            wgtm_growth_error_fit = wgtm_growth_error_fit,
                            m_abs_growth_error_dped_fit = m_abs_growth_error_dped_fit,
                            m_abs_growth_error_dping_fit = m_abs_growth_error_dping_fit,
                            m_abs_growth_error_all_fit = m_abs_growth_error_all_fit,
                            wgtm_abs_growth_error_fit = wgtm_abs_growth_error_fit,
                            m_log_change_error_dped_fit = m_log_change_error_dped_fit,
                            m_log_change_error_dping_fit = m_log_change_error_dping_fit,
                            m_log_change_error_all_fit = m_log_change_error_all_fit,
                            wgtm_log_change_error_fit = wgtm_log_change_error_fit,
                            m_abs_log_change_error_dped_fit = m_abs_log_change_error_dped_fit,
                            m_abs_log_change_error_dping_fit = m_abs_log_change_error_dping_fit,
                            m_abs_log_change_error_all_fit = m_abs_log_change_error_all_fit,
                            wgtm_abs_log_change_error_fit = wgtm_abs_log_change_error_fit,

                            # --- Growth rate error metrics (OOS) ---
                            m_growth_error_dped = m_growth_error_dped,
                            m_growth_error_dping = m_growth_error_dping,
                            m_growth_error_all = m_growth_error_all,
                            wgtm_growth_error = wgtm_growth_error,
                            m_abs_growth_error_dped = m_abs_growth_error_dped,
                            m_abs_growth_error_dping = m_abs_growth_error_dping,
                            m_abs_growth_error_all = m_abs_growth_error_all,
                            wgtm_abs_growth_error = wgtm_abs_growth_error,
                            m_log_change_error_dped = m_log_change_error_dped,
                            m_log_change_error_dping = m_log_change_error_dping,
                            m_log_change_error_all = m_log_change_error_all,
                            wgtm_log_change_error = wgtm_log_change_error,
                            m_abs_log_change_error_dped = m_abs_log_change_error_dped,
                            m_abs_log_change_error_dping = m_abs_log_change_error_dping,
                            m_abs_log_change_error_all = m_abs_log_change_error_all,
                            wgtm_abs_log_change_error = wgtm_abs_log_change_error)

      cat(" --> [mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "]\n\n")

      cat("MSE GDP Share\n")
      cat(" - Developed: ", m_mse_GDP_sh_dped,
          " | Developing: ", m_mse_GDP_sh_dping,
          " | All (unweighted): ", m_mse_GDP_sh_all,
          " | Weighted: ", wgtm_mse_GDP_sh, "\n\n")

      cat("MSE (Level) log(GDP)\n")
      cat(" - Developed: ", m_mse_levl_dped,
          " | Developing: ", m_mse_levl_dping,
          " | All (unweighted): ", m_mse_levl_all,
          " | Weighted: ", wgtm_mse_levl, "\n\n")

      cat("MSE (Change) log(GDP)\n")
      cat(" - Developed: ", m_mse_chan_dped,
          " | Developing: ", m_mse_chan_dping,
          " | All (unweighted): ", m_mse_chan_all,
          " | Weighted: ", wgtm_mse_chan, "\n\n")

      cat("Overall R2 (Level) log(GDP)\n")
      cat(" - Developed: ", m_or2_levl_dped,
          " | Developing: ", m_or2_levl_dping,
          " | All (unweighted): ", m_or2_levl_all,
          " | Weighted: ", wgtm_or2_levl, "\n\n")

      cat("Overall R2 (Change) log(GDP)\n")
      cat(" - Developed: ", m_or2_chan_dped,
          " | Developing: ", m_or2_chan_dping,
          " | All (unweighted): ", m_or2_chan_all,
          " | Weighted: ", wgtm_or2_chan, "\n\n")

      cat("Within R2 (Level) log(GDP)\n")
      cat(" - Developed: ", m_wr2_levl_dped,
          " | Developing: ", m_wr2_levl_dping,
          " | All (unweighted): ", m_wr2_levl_all,
          " | Weighted: ", wgtm_wr2_levl, "\n\n")

      cat("Within R2 (Change) log(GDP)\n")
      cat(" - Developed: ", m_wr2_chan_dped,
          " | Developing: ", m_wr2_chan_dping,
          " | All (unweighted): ", m_wr2_chan_all,
          " | Weighted: ", wgtm_wr2_chan, "\n\n")

      cat("--- No-Rescaling OOS R2 Comparison ---\n\n")

      cat("Overall R2 (Level) log(GDP) [No Rescaling]\n")
      cat(" - Developed: ", m_or2_levl_norescale_dped,
          " | Developing: ", m_or2_levl_norescale_dping,
          " | All (unweighted): ", m_or2_levl_norescale_all,
          " | Weighted: ", wgtm_or2_levl_norescale, "\n\n")

      cat("Overall R2 (Change) log(GDP) [No Rescaling]\n")
      cat(" - Developed: ", m_or2_chan_norescale_dped,
          " | Developing: ", m_or2_chan_norescale_dping,
          " | All (unweighted): ", m_or2_chan_norescale_all,
          " | Weighted: ", wgtm_or2_chan_norescale, "\n\n")

      cat("Within R2 (Level) log(GDP) [No Rescaling]\n")
      cat(" - Developed: ", m_wr2_levl_norescale_dped,
          " | Developing: ", m_wr2_levl_norescale_dping,
          " | All (unweighted): ", m_wr2_levl_norescale_all,
          " | Weighted: ", wgtm_wr2_levl_norescale, "\n\n")

      cat("Within R2 (Change) log(GDP) [No Rescaling]\n")
      cat(" - Developed: ", m_wr2_chan_norescale_dped,
          " | Developing: ", m_wr2_chan_norescale_dping,
          " | All (unweighted): ", m_wr2_chan_norescale_all,
          " | Weighted: ", wgtm_wr2_chan_norescale, "\n\n")

      cat("No-Log Overall R2 (Level) GDP\n")
      cat(" - Developed: ", m_or2_levl_dped_nolog,
          " | Developing: ", m_or2_levl_dping_nolog,
          " | All (unweighted): ", m_or2_levl_all_nolog,
          " | Weighted: ", wgtm_or2_levl_nolog, "\n\n")

      cat("No-Log Overall R2 (Change) GDP\n")
      cat(" - Developed: ", m_or2_chan_dped_nolog,
          " | Developing: ", m_or2_chan_dping_nolog,
          " | All (unweighted): ", m_or2_chan_all_nolog,
          " | Weighted: ", wgtm_or2_chan_nolog, "\n\n")

      cat("MAPE (%)\n")
      cat(" - Developed: ", m_mape_dped,
          " | Developing: ", m_mape_dping,
          " | All (unweighted): ", m_mape_all,
          " | Weighted: ", wgtm_mape, "\n\n")

      cat("Growth Rate Error (%)\n")
      cat(" - Developed: ", m_growth_error_dped,
          " | Developing: ", m_growth_error_dping,
          " | All (unweighted): ", m_growth_error_all,
          " | Weighted: ", wgtm_growth_error, "\n\n")

      cat("Abs Growth Rate Error (%)\n")
      cat(" - Developed: ", m_abs_growth_error_dped,
          " | Developing: ", m_abs_growth_error_dping,
          " | All (unweighted): ", m_abs_growth_error_all,
          " | Weighted: ", wgtm_abs_growth_error, "\n\n")

      cat("Log Change Error\n")
      cat(" - Developed: ", m_log_change_error_dped,
          " | Developing: ", m_log_change_error_dping,
          " | All (unweighted): ", m_log_change_error_all,
          " | Weighted: ", wgtm_log_change_error, "\n\n")

      cat("Abs Log Change Error\n")
      cat(" - Developed: ", m_abs_log_change_error_dped,
          " | Developing: ", m_abs_log_change_error_dping,
          " | All (unweighted): ", m_abs_log_change_error_all,
          " | Weighted: ", wgtm_abs_log_change_error, "\n\n")

      return(results)
    }

    set.seed(1234567)
    results <- mclapply(1:nrow(rf_grid), mc.cores = 10, function(i) {
      set.seed(1234567 + i)
      params <- rf_grid[i,]
      tune_hyperparameters(params, data_full, df.cv)
    }, mc.preschedule = TRUE)

    tuning_results <- bind_rows(results)
    tuning_results_0_25deg <- tuning_results
    save(tuning_results_0_25deg, file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/tuning_results_0_25deg_no_rescaling.RData")

    param_final <- tuning_results_0_25deg %>%
      arrange(desc(wgtm_or2_chan)) %>%
      slice(1) # pick the hyperparameters that generate the largest r2

    best_model_metrics <- tuning_results_0_25deg %>%
      arrange(desc(wgtm_or2_chan)) %>%
      slice(1) %>%  # Select the row with the best parameters
      pivot_longer(cols = everything(), names_to = "metric", values_to = "value")

    write.csv(best_model_metrics, file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_0_25deg_no_rescaling.csv", row.names = FALSE)

    rf_fit <- rand_forest(mtry = param_final$mtry, trees = param_final$trees, min_n = param_final$min_n) %>%
      set_engine("ranger", importance = "impurity", verbose = T, seed = 1234567) %>%
      set_mode("regression") %>%
      fit(formula, data = data_full, weights = import_weight)

    toc()
  }

  return(rf_fit)
}

set.seed(1234567)
tic("Train RF")
rf_model9_good_grid_search_0_25deg <- train_rf(data_full = data_full)
save(rf_model9_good_grid_search_0_25deg, file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_25deg_no_rescaling.RData")
toc()
