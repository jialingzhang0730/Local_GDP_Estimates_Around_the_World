# --------------------------------- Task Summary --------------------------------- #
# This file trains the 0.5-degree random forest model using data from 2012 to 2022.
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
data_train <- read.csv("step4_benchmark_model/outputs/new_data_train_0_5deg.csv")
data_valid_year <- read.csv("step4_benchmark_model/outputs/new_data_valid_year_0_5deg.csv")  
data_valid_iso <- read.csv("step4_benchmark_model/outputs/new_data_valid_iso_0_5deg.csv")  
data_test_year <- read.csv("step4_benchmark_model/outputs/new_data_test_year_0_5deg.csv") 
data_test_iso <- read.csv("step4_benchmark_model/outputs/new_data_test_iso_0_5deg.csv") 

dped_list <- read_excel("step4_benchmark_model/inputs/list_developed_isos.xlsx")
developed_isos <- dped_list[,"developed"]

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso) %>% 
  filter(iso %in% developed_isos$developed) %>%  # Use the column!
  mutate(unit_gdp_af_sum_rescl = state_total_GDP,
         is_developing = 0)  # Add this line - all are developed countries

# Check data integrity
cat("Data dimensions after filtering:\n")
cat("Rows:", nrow(data_full), "\n")
cat("Columns:", ncol(data_full), "\n")
cat("Unique ISOs:", length(unique(data_full$iso)), "\n\n")

# Check for required columns
required_cols <- c("GCP_share_0_5deg", "GCP_0_5deg", "state_total_GDP", "pop_total", 
                   "cell_id", "subcell_id", "iso", "year", "is_developing")
missing_cols <- setdiff(required_cols, names(data_full))
if(length(missing_cols) > 0) {
  stop(paste("CRITICAL ERROR: Missing required columns in data_full:", paste(missing_cols, collapse=", ")))
}

##############################################################################################################################
# Since our main task is to use some countries data to predict other countries, the usual cross validation (randomly separate data into x folds) does not work
# what we want is to randomly pick some countries data to train, and predict on the rest countries to select the best hyperparameters
# thus here we use group_vfold_cv()

set.seed(1234567)
folds <- group_vfold_cv(data_full, group = "iso", v = 5) 

set.seed(1234567)
train_rf <- function(data_full, df.cv = folds, name = "RF", tune_par = T){

  target_var <- "GCP_share_0_5deg"
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

    rf_grid <- expand.grid(mtry = c(27,29,31),
                           trees = c(1500),
                           min_n = c(1800,2300,2800,3300))

    tune_hyperparameters <- function(params, data_full, df.cv) {

      cat("Tuning hyperparameters for mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "\n")

      # Enhanced error handling wrapper
      error_log <- list()

      tryCatch({
        # metric will be saved in those vectors
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

        # define functions with error handling
        MSE_GDP_sh <- function(true_values, predicted_values) {
          tryCatch({
            mean((true_values - predicted_values)^2)
          }, error = function(e) {
            cat("ERROR in MSE_GDP_sh: ", e$message, "\n")
            return(NA)
          })
        }

        MSE_levl <- function(true_values, predicted_values) {
          tryCatch({
            valid <- true_values > 0 & predicted_values > 0
            if(sum(valid) == 0) {
              cat("WARNING: No valid values for MSE_levl calculation\n")
              return(NA)
            }
            true_log <- log(true_values[valid])
            predicted_log <- log(predicted_values[valid])   
            mean((true_log - predicted_log)^2)
          }, error = function(e) {
            cat("ERROR in MSE_levl: ", e$message, "\n")
            return(NA)
          })
        }

        MSE_chan <- function(true_values, true_last, predicted_values, predicted_last) {
          tryCatch({
            valid <- true_values > 0 & predicted_values > 0 & true_last > 0 & predicted_last > 0
            if(sum(valid) == 0) {
              cat("WARNING: No valid values for MSE_chan calculation\n")
              return(NA)
            }
            true_log <- log(true_values[valid])
            true_last_log <- log(true_last[valid])
            true_log_diff <- true_log - true_last_log
            pred_log <- log(predicted_values[valid])   
            pred_last_log <- log(predicted_last[valid])
            pred_log_diff <- pred_log - pred_last_log
            mean((true_log_diff - pred_log_diff)^2)
          }, error = function(e) {
            cat("ERROR in MSE_chan: ", e$message, "\n")
            return(NA)
          })
        }

        overall_r2_levl <- function(true_values, predicted_values) {
          tryCatch({
            valid <- true_values > 0 & predicted_values > 0
            if(sum(valid) == 0) {
              cat("WARNING: No valid values for overall_r2_levl calculation\n")
              return(NA)
            }
            true_log <- log(true_values[valid])
            predicted_log <- log(predicted_values[valid])        
            1 - (sum((true_log - predicted_log)^2) / sum((true_log - mean(true_log))^2))
          }, error = function(e) {
            cat("ERROR in overall_r2_levl: ", e$message, "\n")
            return(NA)
          })
        }

        overall_r2_chan <- function(true_values, true_last, predicted_values, predicted_last) {
          tryCatch({
            valid <- true_values > 0 & predicted_values > 0 & true_last > 0 & predicted_last > 0
            if(sum(valid) == 0) {
              cat("WARNING: No valid values for overall_r2_chan calculation\n")
              return(NA)
            }
            true_log <- log(true_values[valid])
            true_last_log <- log(true_last[valid])
            true_log_diff <- true_log - true_last_log
            pred_log <- log(predicted_values[valid])   
            pred_last_log <- log(predicted_last[valid])
            pred_log_diff <- pred_log - pred_last_log
            1 - (sum((true_log_diff - pred_log_diff)^2) / sum((true_log_diff - mean(true_log_diff))^2))
          }, error = function(e) {
            cat("ERROR in overall_r2_chan: ", e$message, "\n")
            return(NA)
          })
        }

        within_iso_r2_levl <- function(df, true_var, pred_var) {
          tryCatch({
            df_af <- df %>% 
              filter({{ true_var }} > 0 & {{ pred_var }} > 0) %>%
              mutate(true_log = log({{ true_var }}),
                     pred_log = log({{ pred_var }})) %>%
              group_by(iso, year) %>%
              mutate(iso_mean_true_log = mean(true_log)) %>%
              ungroup()

            if(nrow(df_af) == 0) {
              cat("WARNING: No valid data in within_iso_r2_levl\n")
              return(NA)
            }

            rss <- sum((df_af$true_log - df_af$pred_log)^2)
            wss <- sum((df_af$true_log - df_af$iso_mean_true_log)^2)

            if(wss == 0) {
              cat("WARNING: Zero within-group variation in within_iso_r2_levl\n")
              return(NA)
            }

            1 - rss / wss
          }, error = function(e) {
            cat("ERROR in within_iso_r2_levl: ", e$message, "\n")
            return(NA)
          })
        }

        within_iso_r2_chan <- function(df, true_var, true_var_last, pred_var, pred_var_last) {
          tryCatch({
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

            if(nrow(df_af) == 0) {
              cat("WARNING: No valid data in within_iso_r2_chan\n")
              return(NA)
            }

            rss <- sum((df_af$true_log_diff - df_af$pred_log_diff)^2)
            wss <- sum((df_af$true_log_diff - df_af$iso_mean_true_log_diff)^2)

            if(wss == 0) {
              cat("WARNING: Zero within-group variation in within_iso_r2_chan\n")
              return(NA)
            }

            1 - rss / wss
          }, error = function(e) {
            cat("ERROR in within_iso_r2_chan: ", e$message, "\n")
            return(NA)
          })
        }

        # define the following to save: n
        datapoint_counts <- list()
        var_importance <- list()

        for (i in seq_along(df.cv$splits)) {
          cat("Processing fold", i, "of", length(df.cv$splits), "\n")

          tryCatch({
            analysis <- as.data.frame(analysis(df.cv$splits[[i]]))
            assessment <- as.data.frame(assessment(df.cv$splits[[i]]))

            cat("  Analysis data: ", nrow(analysis), "rows\n")
            cat("  Assessment data: ", nrow(assessment), "rows\n")

            # Check for required columns in fold data
            fold_required <- c("GCP_share_0_5deg", "GCP_0_5deg", "state_total_GDP", 
                               "pop_total", "cell_id", "subcell_id", "iso", "year")
            fold_missing <- setdiff(fold_required, names(analysis))
            if(length(fold_missing) > 0) {
              stop(paste("Missing columns in fold", i, ":", paste(fold_missing, collapse=", ")))
            }

            # fit the model using training folds
            cat("  Fitting model...\n")
            fit <- tryCatch({
              rand_forest(mtry = params$mtry, trees = params$trees, min_n = params$min_n) %>%
                set_engine("ranger", verbose = FALSE, importance = "impurity", seed = 1234567) %>%
                set_mode("regression") %>%
                fit(formula, data = analysis)
            }, error = function(e) {
              cat("ERROR fitting model in fold", i, ":", e$message, "\n")
              stop(e)
            })

            var_importance[[i]] <- vi(fit)

            # obtain model fit
            cat("  Processing analysis predictions...\n")
            analysis_fit <- tryCatch({
              analysis %>% 
                mutate(pred_GCP_sh_ns = predict(fit, analysis)$.pred) %>% 
                mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>% 
                group_by(iso, year) %>%
                mutate(pred_GCP_sh = pred_GCP_sh_ns / sum(pred_GCP_sh_ns)) %>%
                ungroup() %>%
                mutate(pred_GCP = pred_GCP_sh * state_total_GDP) %>% 
                arrange(iso, cell_id, subcell_id, year) %>%
                group_by(iso, cell_id, subcell_id) %>%
                mutate(prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_0_5deg[match(year - 1, year)], NA),
                       prev_yr_pred_gdp = ifelse(year - 1 %in% year, pred_GCP[match(year - 1, year)], NA),
                       prev_yr_true_gdp_sh = ifelse(year - 1 %in% year, GCP_share_0_5deg[match(year - 1, year)], NA),
                       prev_yr_pred_gdp_sh = ifelse(year - 1 %in% year, pred_GCP_sh[match(year - 1, year)], NA)) %>%
                ungroup() %>% 
                as.data.frame()
            }, error = function(e) {
              cat("ERROR processing analysis_fit in fold", i, ":", e$message, "\n")
              cat("Columns in analysis:", paste(names(analysis), collapse=", "), "\n")
              stop(e)
            })

            developed_fit <- analysis_fit %>% filter(is_developing == 0)
            cat("  Developed fit data: ", nrow(developed_fit), "rows\n")

            # When checking annual growth, exclude each ISO's first year
            developed_fit_ch <- developed_fit %>%
              group_by(iso) %>%
              filter(year != min(year)) %>%
              ungroup()

            analysis_fit_ch <- analysis_fit %>% 
              group_by(iso) %>%
              filter(year != min(year)) %>%
              ungroup()  

            # now prepare the assessment dataset
            cat("  Processing assessment predictions...\n")
            assessment_pred <- tryCatch({
              assessment %>% 
                mutate(pred_GCP_sh_ns = predict(fit, assessment)$.pred) %>% 
                mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>% 
                group_by(iso, year) %>%
                mutate(pred_GCP_sh = pred_GCP_sh_ns / sum(pred_GCP_sh_ns)) %>%
                ungroup() %>%
                mutate(pred_GCP = pred_GCP_sh * state_total_GDP) %>% 
                arrange(iso, cell_id, subcell_id, year) %>%
                group_by(iso, cell_id, subcell_id) %>%
                mutate(prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_0_5deg[match(year - 1, year)], NA),
                       prev_yr_pred_gdp = ifelse(year - 1 %in% year, pred_GCP[match(year - 1, year)], NA),
                       prev_yr_true_gdp_sh = ifelse(year - 1 %in% year, GCP_share_0_5deg[match(year - 1, year)], NA),
                       prev_yr_pred_gdp_sh = ifelse(year - 1 %in% year, pred_GCP_sh[match(year - 1, year)], NA)) %>%
                ungroup() %>% 
                as.data.frame()
            }, error = function(e) {
              cat("ERROR processing assessment_pred in fold", i, ":", e$message, "\n")
              stop(e)
            })

            developed <- assessment_pred %>% filter(is_developing == 0)
            cat("  Developed assessment data: ", nrow(developed), "rows\n")

            developed_ch <- developed %>%
              group_by(iso) %>%
              filter(year != min(year)) %>%
              ungroup()

            assessment_pred_ch <- assessment_pred %>% 
              group_by(iso) %>%
              filter(year != min(year)) %>%
              ungroup()  

            # document data points
            datapoint_counts[[i]] <- c(developed_ch = nrow(developed_ch))

            # Calculate metrics with detailed error handling
            cat("  Calculating metrics...\n")

            # within-sample fit metrics
            mse_GDP_sh_dped_fit[i] <- MSE_GDP_sh(developed_fit$GCP_share_0_5deg, developed_fit$pred_GCP_sh)
            mse_levl_dped_fit[i] <- MSE_levl(developed_fit$GCP_0_5deg, developed_fit$pred_GCP)
            mse_chan_dped_fit[i] <- MSE_chan(developed_fit_ch$GCP_0_5deg, developed_fit_ch$prev_yr_true_gdp, 
                                             developed_fit_ch$pred_GCP, developed_fit_ch$prev_yr_pred_gdp)
            o_r2_levl_dped_fit[i] <- overall_r2_levl(developed_fit$GCP_0_5deg, developed_fit$pred_GCP)
            o_r2_chan_dped_fit[i] <- overall_r2_chan(developed_fit_ch$GCP_0_5deg, developed_fit_ch$prev_yr_true_gdp,
                                                     developed_fit_ch$pred_GCP, developed_fit_ch$prev_yr_pred_gdp)
            w_r2_levl_dped_fit[i] <- within_iso_r2_levl(developed_fit, GCP_0_5deg, pred_GCP)
            w_r2_chan_dped_fit[i] <- within_iso_r2_chan(developed_fit_ch, GCP_0_5deg, prev_yr_true_gdp, 
                                                        pred_GCP, prev_yr_pred_gdp)

            # out of sample metrics
            mse_GDP_sh_dped[i] <- MSE_GDP_sh(developed$GCP_share_0_5deg, developed$pred_GCP_sh)
            mse_levl_dped[i] <- MSE_levl(developed$GCP_0_5deg, developed$pred_GCP)
            mse_chan_dped[i] <- MSE_chan(developed_ch$GCP_0_5deg, developed_ch$prev_yr_true_gdp,
                                         developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)
            o_r2_levl_dped[i] <- overall_r2_levl(developed$GCP_0_5deg, developed$pred_GCP)
            o_r2_chan_dped[i] <- overall_r2_chan(developed_ch$GCP_0_5deg, developed_ch$prev_yr_true_gdp,
                                                 developed_ch$pred_GCP, developed_ch$prev_yr_pred_gdp)
            w_r2_levl_dped[i] <- within_iso_r2_levl(developed, GCP_0_5deg, pred_GCP)
            w_r2_chan_dped[i] <- within_iso_r2_chan(developed_ch, GCP_0_5deg, prev_yr_true_gdp, 
                                                    pred_GCP, prev_yr_pred_gdp)

            cat("  Fold", i, "completed successfully\n")

          }, error = function(e) {
            cat("CRITICAL ERROR in fold", i, ":\n")
            cat("  Message:", e$message, "\n")
            cat("  Call:", deparse(e$call), "\n")
            error_log[[paste0("fold_", i)]] <- list(message = e$message, call = e$call)

            # Set all metrics to NA for this fold
            mse_GDP_sh_dped_fit[i] <- NA
            mse_levl_dped_fit[i] <- NA
            mse_chan_dped_fit[i] <- NA
            o_r2_levl_dped_fit[i] <- NA
            o_r2_chan_dped_fit[i] <- NA
            w_r2_levl_dped_fit[i] <- NA
            w_r2_chan_dped_fit[i] <- NA
            mse_GDP_sh_dped[i] <- NA
            mse_levl_dped[i] <- NA
            mse_chan_dped[i] <- NA
            o_r2_levl_dped[i] <- NA
            o_r2_chan_dped[i] <- NA
            w_r2_levl_dped[i] <- NA
            w_r2_chan_dped[i] <- NA
          })
        }

        # Create metrics dataframe
        metrics_df <- tryCatch({
          tibble(mtry = params$mtry, trees = params$trees, min_n = params$min_n,
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
                 w_r2_chan_dped = w_r2_chan_dped)
        }, error = function(e) {
          cat("ERROR creating metrics_df:", e$message, "\n")
          stop(e)
        })

        # Save outputs
        tryCatch({
          write.csv(metrics_df, file = sprintf("step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/detailed_metric_0_5deg/detail_metrics_%s_%s_%s.csv", 
                                               params$mtry, params$trees, params$min_n), row.names = FALSE)
        }, error = function(e) {
          cat("ERROR saving metrics_df:", e$message, "\n")
        })

        # Save datapoint counts
        if(length(datapoint_counts) > 0) {
          tryCatch({
            datapoint_counts_df <- bind_cols(
              metric_df = names(datapoint_counts[[1]]),
              as.data.frame(datapoint_counts) %>%
                setNames(paste0("N_fold_", seq_along(datapoint_counts)))
            )
            write.csv(datapoint_counts_df, "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/rf_metrics_fold_N_0_5deg.csv", row.names = FALSE)
          }, error = function(e) {
            cat("ERROR saving datapoint_counts:", e$message, "\n")
          })
        }

        # Save variable importance
        tryCatch({
          save(var_importance, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/var_imptc_score_0_5deg.RData")
        }, error = function(e) {
          cat("ERROR saving variable importance:", e$message, "\n")
        })

        # Calculate mean metrics
        m_mse_GDP_sh_dped_fit <- mean(mse_GDP_sh_dped_fit, na.rm = TRUE)
        m_mse_levl_dped_fit <- mean(mse_levl_dped_fit, na.rm = TRUE)
        m_mse_chan_dped_fit <- mean(mse_chan_dped_fit, na.rm = TRUE)
        m_or2_levl_dped_fit <- mean(o_r2_levl_dped_fit, na.rm = TRUE)
        m_or2_chan_dped_fit <- mean(o_r2_chan_dped_fit, na.rm = TRUE)
        m_wr2_levl_dped_fit <- mean(w_r2_levl_dped_fit, na.rm = TRUE)
        m_wr2_chan_dped_fit <- mean(w_r2_chan_dped_fit, na.rm = TRUE)

        m_mse_GDP_sh_dped <- mean(mse_GDP_sh_dped, na.rm = TRUE)
        m_mse_levl_dped <- mean(mse_levl_dped, na.rm = TRUE)
        m_mse_chan_dped <- mean(mse_chan_dped, na.rm = TRUE)
        m_or2_levl_dped <- mean(o_r2_levl_dped, na.rm = TRUE)
        m_or2_chan_dped <- mean(o_r2_chan_dped, na.rm = TRUE)
        m_wr2_levl_dped <- mean(w_r2_levl_dped, na.rm = TRUE)
        m_wr2_chan_dped <- mean(w_r2_chan_dped, na.rm = TRUE)

        # Create results dataframe
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

        if(length(error_log) > 0) {
          cat("\n!!! ERRORS ENCOUNTERED DURING PROCESSING !!!\n")
          print(error_log)
          cat("\n")
        }

        return(results)

      }, error = function(e) {
        cat("\n\nCRITICAL FAILURE in tune_hyperparameters:\n")
        cat("Parameters: mtry=", params$mtry, ", trees=", params$trees, ", min_n=", params$min_n, "\n")
        cat("Error message:", e$message, "\n")
        cat("Error call:", deparse(e$call), "\n\n")

        # Return NULL to indicate failure
        return(NULL)
      })
    }

    set.seed(1234567)
    results <- mclapply(1:nrow(rf_grid), mc.cores = 10, function(i) {
      tryCatch({
        set.seed(1234567 + i)      
        params <- rf_grid[i,]
        result <- tune_hyperparameters(params, data_full, df.cv)

        if(is.null(result)) {
          cat("Parameter set", i, "returned NULL (failed)\n")
        } else if(!is.data.frame(result)) {
          cat("Parameter set", i, "returned non-dataframe:", class(result), "\n")
          result <- NULL
        }

        return(result)
      }, error = function(e) {
        cat("\n\nERROR in mclapply for parameter set", i, ":\n")
        cat("  mtry =", rf_grid[i, "mtry"], 
            ", trees =", rf_grid[i, "trees"], 
            ", min_n =", rf_grid[i, "min_n"], "\n")
        cat("  Error message:", e$message, "\n")
        cat("  Error call:", deparse(e$call), "\n\n")
        return(NULL)
      })
    }, mc.preschedule = TRUE)

    # Check results before binding
    cat("\n\nChecking mclapply results:\n")
    cat("Total results:", length(results), "\n")
    cat("NULL results:", sum(sapply(results, is.null)), "\n")
    cat("Non-dataframe results:", sum(!sapply(results, is.data.frame)), "\n")

    # Remove NULL and non-dataframe results
    valid_results <- results[sapply(results, function(x) !is.null(x) && is.data.frame(x))]

    if(length(valid_results) == 0) {
      stop("\n\nFATAL ERROR: All parameter combinations failed!\n",
           "No valid results to process.\n",
           "Check the error messages above for details.")
    }

    cat("Valid results to bind:", length(valid_results), "\n\n")

    # Bind valid results
    tuning_results <- tryCatch({
      bind_rows(valid_results)
    }, error = function(e) {
      cat("\n\nERROR in bind_rows:\n")
      cat("  Message:", e$message, "\n")
      cat("  Structure of first valid result:\n")
      if(length(valid_results) > 0) {
        print(str(valid_results[[1]]))
      }
      stop(e)
    })

    tuning_results_0_5deg <- tuning_results
    save(tuning_results_0_5deg, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/tuning_results_0_5deg.RData")

    param_final <- tuning_results_0_5deg %>%
      arrange(desc(m_or2_chan_dped)) %>%
      slice(1)

    best_model_metrics <- tuning_results_0_5deg %>%
      arrange(desc(m_or2_chan_dped)) %>%
      slice(1) %>%
      pivot_longer(cols = everything(), names_to = "metric", values_to = "value")

    write.csv(best_model_metrics, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_0_5deg.csv", row.names = FALSE)

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
rf_model9_good_grid_search_0_5deg <- train_rf(data_full = data_full)
save(rf_model9_good_grid_search_0_5deg, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_5deg.RData")
toc()
