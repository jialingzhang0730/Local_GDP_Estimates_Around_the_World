# --------------------------------- Task Summary --------------------------------- #
# This file evaluates random forest performance under spatial block cross-validation by holding out contiguous clusters of cells, varying cluster size to test sensitivity to within-country spatial autocorrelation (Online Appendix Section 8.2).
# -------------------------------------------------------------------------------- #

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# ===== USER PARAMETERS ================================================================= #
# Number of cells per spatial cluster.
# Larger group_size = fewer, larger clusters = bigger gap between training and test cells
# = harder spatial generalization test.
group_sizes <- c(1,2,5,10,25,50)

# Fixed hyperparameters (best from tuning in 2_put_all_isos_to_train_1deg.R)
best_mtry  <- 33
best_trees <- 1500
best_min_n <- 2325

# Number of CV folds (to match the original v=5 structure)
n_cv_folds <- 5

# Whether to also run the baseline random country CV (v=5) for comparison
run_baseline <- TRUE
# ======================================================================================== #

library(dplyr)
library(ranger)
library(tidyverse)
library(tidymodels)
library(parallel)
library(readxl)
library(sf)
library(tictoc)

output_dir <- "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/spatial_block_cv"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# 1. Load training data 
# ---------------------------------------------------------------------------------------------------------------------------------------

data_train      <- read.csv("step4_benchmark_model/outputs/new_data_train_1deg.csv")
data_valid_year <- read.csv("step4_benchmark_model/outputs/new_data_valid_year_1deg.csv")
data_valid_iso  <- read.csv("step4_benchmark_model/outputs/new_data_valid_iso_1deg.csv")
data_test_year  <- read.csv("step4_benchmark_model/outputs/new_data_test_year_1deg.csv")
data_test_iso   <- read.csv("step4_benchmark_model/outputs/new_data_test_iso_1deg.csv")

# ---------------------------------------------------------------------------------------------------------------------------------------
# 2. Calculate real-world developed/developing cell shares 
# ---------------------------------------------------------------------------------------------------------------------------------------

dped_list <- read_excel("step4_benchmark_model/inputs/list_developed_isos.xlsx")
developed_isos <- dped_list[, "developed"]

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_1deg.RData")

real_share <- predictors_put_in_model_1deg %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>%
  as.data.frame() %>%
  distinct(cell_id, iso) %>%
  mutate(is_developing = ifelse(iso %in% developed_isos$developed, 0, 1)) %>%
  group_by(is_developing) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

# ---------------------------------------------------------------------------------------------------------------------------------------
# 3. Get cell coordinates from the grid lookup file
# ---------------------------------------------------------------------------------------------------------------------------------------

cell_coords_df <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1deg_with_lon_lat.csv") %>%
  mutate(cell_id = as.character(cell_id),
         lon = longitude + 0.5,
         lat = latitude + 0.5) %>%
  dplyr::select(cell_id, lon, lat)

# Free the predictors object (only needed for real_share above)
rm(predictors_put_in_model_1deg); gc()

# ---------------------------------------------------------------------------------------------------------------------------------------
# 4. Construct data_full with importance weights 
# ---------------------------------------------------------------------------------------------------------------------------------------

data_full <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso) %>%
  mutate(unit_gdp_af_sum_rescl = state_total_GDP) %>%
  mutate(original_order = row_number()) %>%
  mutate(is_developing = ifelse(iso %in% developed_isos$developed | substr(iso, 1, 3) == "USA", 0, 1)) %>%
  group_by(is_developing) %>%
  mutate(sample_count = n()) %>%
  ungroup() %>%
  mutate(sample_share = sample_count / n()) %>%
  left_join(real_share) %>%
  mutate(normalized_weight = share / sample_share) %>%
  dplyr::select(-c(sample_count, sample_share, count, share)) %>%
  mutate(import_weight = importance_weights(normalized_weight)) %>%
  arrange(original_order) %>%
  dplyr::select(-c(original_order)) %>%
  mutate(row_id = row_number())  # persistent row identifier for predict-all approach

rm(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso); gc()

# Ensure cell_id types match for joining
data_full$cell_id <- as.character(data_full$cell_id)
cell_coords_df$cell_id <- as.character(cell_coords_df$cell_id)

# Build lookup of unique (cell_id, iso) pairs with their centroids.
# A single cell_id can appear with multiple iso values (province boundaries),
# so the clustering unit is (cell_id, iso), not cell_id alone.
cell_iso_coords <- data_full %>%
  distinct(cell_id, iso) %>%
  left_join(cell_coords_df, by = "cell_id")

cat(paste0("  Unique (cell_id, iso) pairs for clustering: ", nrow(cell_iso_coords), "\n"))

# Count cells per iso (for reporting and convergence behavior)
cells_per_iso <- cell_iso_coords %>%
  group_by(iso) %>%
  summarise(n_cells = n(), .groups = "drop")
cat(sprintf("  Cells per iso: min=%d, median=%d, max=%d\n",
            min(cells_per_iso$n_cells), median(cells_per_iso$n_cells), max(cells_per_iso$n_cells)))

# ---------------------------------------------------------------------------------------------------------------------------------------
# 5. Define the model formula and metric functions 
# ---------------------------------------------------------------------------------------------------------------------------------------

target_var <- "GCP_share_1deg"
predictor_vars <- c("pop_total_share", "pop_urban_share", "pop_cropland_share", "pop_other_share",
                    "CO2_bio_manuf_conbust_share", "CO2_bio_heavy_indus_share", "CO2_bio_tspt_share",
                    "CO2_non_org_manuf_conbust_share", "CO2_non_org_heavy_indus_share", "CO2_non_org_tspt_share", "NPP_share",
                    "NTL_urban_snow_free_period_share", "NTL_cropland_snow_free_period_share", "NTL_other_snow_free_period_share",
                    "snow_ice_share", "water_share", "urban_share", "forest_share", "cropland_share", "mean_rug", "national_gdpc",
                    "lag_NTL_urban_share", "lag_urban_share", "lag_cropland_share", "lag_NTL_other_share", "lag_NTL_cropland_share",
                    "lag_CO2_bio_mc_share", "lag_CO2_nonorg_mc_share", "lag_CO2_bio_heavy_indus_share", "lag_CO2_non_org_heavy_indus_share",
                    "lag_CO2_bio_tspt_share", "lag_CO2_non_org_tspt_share", "lag_pop_urban_share", "lag_NPP_share",
                    "lag_pop_cropland_share", "lag_pop_other_share")
formula <- as.formula(paste(target_var, "~", paste(predictor_vars, collapse = " + ")))

# --- Metric functions ---

MSE_GDP_sh <- function(true_values, predicted_values) {
  mean((true_values - predicted_values)^2)
}

MSE_levl <- function(true_values, predicted_values) {
  valid <- true_values > 0 & predicted_values > 0
  if (sum(valid) == 0) return(NA)
  mean((log(true_values[valid]) - log(predicted_values[valid]))^2)
}

MSE_chan <- function(true_values, true_last, predicted_values, predicted_last) {
  valid <- !is.na(true_values) & !is.na(true_last) &
    !is.na(predicted_values) & !is.na(predicted_last) &
    true_values > 0 & predicted_values > 0 &
    true_last > 0 & predicted_last > 0
  if (sum(valid) == 0) return(NA)
  true_log_diff <- log(true_values[valid]) - log(true_last[valid])
  pred_log_diff <- log(predicted_values[valid]) - log(predicted_last[valid])
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
  valid <- !is.na(true_values) & !is.na(true_last) &
    !is.na(predicted_values) & !is.na(predicted_last) &
    true_values > 0 & predicted_values > 0 &
    true_last > 0 & predicted_last > 0
  if (sum(valid) < 2) return(NA)
  true_log_diff <- log(true_values[valid]) - log(true_last[valid])
  pred_log_diff <- log(predicted_values[valid]) - log(predicted_last[valid])
  if (var(true_log_diff) < 1e-10) return(NA)
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
    mutate(true_log_diff = log({{ true_var }}) - log({{ true_var_last }}),
           pred_log_diff = log({{ pred_var }}) - log({{ pred_var_last }})) %>%
    group_by(iso, year) %>%
    mutate(iso_mean_true_log_diff = mean(true_log_diff)) %>%
    ungroup()
  if (nrow(df_af) < 2) return(NA)
  rss <- sum((df_af$true_log_diff - df_af$pred_log_diff)^2)
  wss <- sum((df_af$true_log_diff - df_af$iso_mean_true_log_diff)^2)
  if (wss < 1e-10) return(NA)
  1 - rss / wss
}

pre_rescale_r2 <- function(true_shares, predicted_shares) {
  if (length(true_shares) < 2) return(NA)
  rss <- sum((true_shares - predicted_shares)^2)
  tss <- sum((true_shares - mean(true_shares))^2)
  if (tss < 1e-10) return(NA)
  1 - (rss / tss)
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# 6. Core CV evaluation function 
# ---------------------------------------------------------------------------------------------------------------------------------------
# For each fold:
#   1. Train on analysis cells
#   2. Predict ALL cells in data_full (both analysis and assessment)
#   3. Rescale within (iso, year) across ALL cells
#   4. Compute GDP levels
#   5. Evaluate metrics only on held-out (assessment) cells
#
# This is necessary for cell-level spatial blocks because a country's cells can be split
# across folds, making held-out-only rescaling incorrect. For the country-level baseline,
# this approach gives identical OOS results since countries are entirely in one fold.

run_cv_evaluation <- function(data_full, folds, cv_label, real_share) {

  n_folds <- length(folds$splits)

  # Initialize metric vectors for each fold
  # In-sample (fit) metrics
  pre_r2_levl_dped_fit  <- numeric(n_folds)
  pre_r2_levl_dping_fit <- numeric(n_folds)
  pre_r2_levl_all_fit   <- numeric(n_folds)
  mse_GDP_sh_dped_fit   <- numeric(n_folds)
  mse_GDP_sh_dping_fit  <- numeric(n_folds)
  mse_GDP_sh_all_fit    <- numeric(n_folds)
  mse_levl_dped_fit     <- numeric(n_folds)
  mse_levl_dping_fit    <- numeric(n_folds)
  mse_levl_all_fit      <- numeric(n_folds)
  mse_chan_dped_fit      <- numeric(n_folds)
  mse_chan_dping_fit     <- numeric(n_folds)
  mse_chan_all_fit       <- numeric(n_folds)
  o_r2_levl_dped_fit    <- numeric(n_folds)
  o_r2_levl_dping_fit   <- numeric(n_folds)
  o_r2_levl_all_fit     <- numeric(n_folds)
  o_r2_chan_dped_fit     <- numeric(n_folds)
  o_r2_chan_dping_fit    <- numeric(n_folds)
  o_r2_chan_all_fit      <- numeric(n_folds)
  w_r2_levl_dped_fit    <- numeric(n_folds)
  w_r2_levl_dping_fit   <- numeric(n_folds)
  w_r2_levl_all_fit     <- numeric(n_folds)
  w_r2_chan_dped_fit     <- numeric(n_folds)
  w_r2_chan_dping_fit    <- numeric(n_folds)
  w_r2_chan_all_fit      <- numeric(n_folds)

  # Out-of-sample (OOS) metrics
  pre_r2_levl_dped  <- numeric(n_folds)
  pre_r2_levl_dping <- numeric(n_folds)
  pre_r2_levl_all   <- numeric(n_folds)
  mse_GDP_sh_dped   <- numeric(n_folds)
  mse_GDP_sh_dping  <- numeric(n_folds)
  mse_GDP_sh_all    <- numeric(n_folds)
  mse_levl_dped     <- numeric(n_folds)
  mse_levl_dping    <- numeric(n_folds)
  mse_levl_all      <- numeric(n_folds)
  mse_chan_dped      <- numeric(n_folds)
  mse_chan_dping     <- numeric(n_folds)
  mse_chan_all       <- numeric(n_folds)
  o_r2_levl_dped    <- numeric(n_folds)
  o_r2_levl_dping   <- numeric(n_folds)
  o_r2_levl_all     <- numeric(n_folds)
  o_r2_chan_dped     <- numeric(n_folds)
  o_r2_chan_dping    <- numeric(n_folds)
  o_r2_chan_all      <- numeric(n_folds)
  w_r2_levl_dped    <- numeric(n_folds)
  w_r2_levl_dping   <- numeric(n_folds)
  w_r2_levl_all     <- numeric(n_folds)
  w_r2_chan_dped     <- numeric(n_folds)
  w_r2_chan_dping    <- numeric(n_folds)
  w_r2_chan_all      <- numeric(n_folds)

  for (i in seq_along(folds$splits)) {
    cat(paste0("  [", cv_label, "] Fold ", i, "/", n_folds, "...\n"))

    analysis_data   <- as.data.frame(analysis(folds$splits[[i]]))
    assessment_data <- as.data.frame(assessment(folds$splits[[i]]))

    held_out_row_ids <- assessment_data$row_id
    cat(paste0("    Analysis: ", nrow(analysis_data), " rows | Assessment: ", nrow(assessment_data), " rows\n"))

    # Train model on analysis cells
    set.seed(1234567)
    fit <- rand_forest(mtry = best_mtry, trees = best_trees, min_n = best_min_n) %>%
      set_engine("ranger", importance = "impurity", verbose = FALSE, seed = 1234567) %>%
      set_mode("regression") %>%
      fit(formula, data = analysis_data, weights = import_weight)

    # ---- Predict ALL cells in data_full ----
    all_predictions <- predict(fit, data_full)$.pred

    # ---- Pre-rescaling R2 (computed directly, no rescaling needed) ----
    # In-sample (analysis cells)
    analysis_raw_pred <- all_predictions[!(data_full$row_id %in% held_out_row_ids)]
    analysis_raw_true <- data_full[[target_var]][!(data_full$row_id %in% held_out_row_ids)]
    analysis_is_dev   <- data_full$is_developing[!(data_full$row_id %in% held_out_row_ids)]

    pre_r2_levl_dped_fit[i]  <- pre_rescale_r2(analysis_raw_true[analysis_is_dev == 0], analysis_raw_pred[analysis_is_dev == 0])
    pre_r2_levl_dping_fit[i] <- pre_rescale_r2(analysis_raw_true[analysis_is_dev == 1], analysis_raw_pred[analysis_is_dev == 1])
    pre_r2_levl_all_fit[i]   <- pre_rescale_r2(analysis_raw_true, analysis_raw_pred)

    # Out-of-sample (assessment cells)
    assess_raw_pred <- all_predictions[data_full$row_id %in% held_out_row_ids]
    assess_raw_true <- data_full[[target_var]][data_full$row_id %in% held_out_row_ids]
    assess_is_dev   <- data_full$is_developing[data_full$row_id %in% held_out_row_ids]

    pre_r2_levl_dped[i]  <- pre_rescale_r2(assess_raw_true[assess_is_dev == 0], assess_raw_pred[assess_is_dev == 0])
    pre_r2_levl_dping[i] <- pre_rescale_r2(assess_raw_true[assess_is_dev == 1], assess_raw_pred[assess_is_dev == 1])
    pre_r2_levl_all[i]   <- pre_rescale_r2(assess_raw_true, assess_raw_pred)

    # ---- Rescale across ALL cells and compute GDP levels ----
    all_preds <- data_full %>%
      mutate(pred_GCP_sh_ns = all_predictions) %>%
      mutate(pred_GCP_sh_ns = ifelse(floor(pop_total) == 0, 0, pred_GCP_sh_ns)) %>%
      group_by(iso, year) %>%
      mutate(pred_GCP_sh = pred_GCP_sh_ns / sum(pred_GCP_sh_ns)) %>%
      ungroup() %>%
      mutate(pred_GCP = pred_GCP_sh * state_total_GDP) %>%
      arrange(iso, cell_id, year) %>%
      group_by(iso, cell_id) %>%
      mutate(prev_yr_true_gdp = ifelse(year - 1 %in% year, GCP_1deg[match(year - 1, year)], NA),
             prev_yr_pred_gdp = ifelse(year - 1 %in% year, pred_GCP[match(year - 1, year)], NA)) %>%
      ungroup() %>%
      as.data.frame()

    # ---- Split into analysis_fit and assessment_pred ----
    analysis_fit    <- all_preds %>% filter(!(row_id %in% held_out_row_ids))
    assessment_pred <- all_preds %>% filter(row_id %in% held_out_row_ids)

    rm(all_preds, all_predictions); gc()

    # ---- Compute in-sample (fit) metrics ----
    developed_fit  <- analysis_fit %>% filter(is_developing == 0)
    developing_fit <- analysis_fit %>% filter(is_developing == 1)

    developed_fit_ch  <- developed_fit  %>% group_by(iso) %>% filter(year != min(year)) %>% ungroup()
    developing_fit_ch <- developing_fit %>% group_by(iso) %>% filter(year != min(year)) %>% ungroup()
    analysis_fit_ch   <- analysis_fit   %>% group_by(iso) %>% filter(year != min(year)) %>% ungroup()

    mse_GDP_sh_dped_fit[i]  <- MSE_GDP_sh(developed_fit$GCP_share_1deg,  developed_fit$pred_GCP_sh)
    mse_GDP_sh_dping_fit[i] <- MSE_GDP_sh(developing_fit$GCP_share_1deg, developing_fit$pred_GCP_sh)
    mse_GDP_sh_all_fit[i]   <- MSE_GDP_sh(analysis_fit$GCP_share_1deg,   analysis_fit$pred_GCP_sh)

    mse_levl_dped_fit[i]  <- MSE_levl(developed_fit$GCP_1deg,  developed_fit$pred_GCP)
    mse_levl_dping_fit[i] <- MSE_levl(developing_fit$GCP_1deg, developing_fit$pred_GCP)
    mse_levl_all_fit[i]   <- MSE_levl(analysis_fit$GCP_1deg,   analysis_fit$pred_GCP)

    mse_chan_dped_fit[i]  <- MSE_chan(developed_fit_ch$GCP_1deg,  developed_fit_ch$prev_yr_true_gdp,  developed_fit_ch$pred_GCP,  developed_fit_ch$prev_yr_pred_gdp)
    mse_chan_dping_fit[i] <- MSE_chan(developing_fit_ch$GCP_1deg, developing_fit_ch$prev_yr_true_gdp, developing_fit_ch$pred_GCP, developing_fit_ch$prev_yr_pred_gdp)
    mse_chan_all_fit[i]   <- MSE_chan(analysis_fit_ch$GCP_1deg,   analysis_fit_ch$prev_yr_true_gdp,   analysis_fit_ch$pred_GCP,   analysis_fit_ch$prev_yr_pred_gdp)

    o_r2_levl_dped_fit[i]  <- overall_r2_levl(developed_fit$GCP_1deg,  developed_fit$pred_GCP)
    o_r2_levl_dping_fit[i] <- overall_r2_levl(developing_fit$GCP_1deg, developing_fit$pred_GCP)
    o_r2_levl_all_fit[i]   <- overall_r2_levl(analysis_fit$GCP_1deg,   analysis_fit$pred_GCP)

    o_r2_chan_dped_fit[i]  <- overall_r2_chan(developed_fit_ch$GCP_1deg,  developed_fit_ch$prev_yr_true_gdp,  developed_fit_ch$pred_GCP,  developed_fit_ch$prev_yr_pred_gdp)
    o_r2_chan_dping_fit[i] <- overall_r2_chan(developing_fit_ch$GCP_1deg, developing_fit_ch$prev_yr_true_gdp, developing_fit_ch$pred_GCP, developing_fit_ch$prev_yr_pred_gdp)
    o_r2_chan_all_fit[i]   <- overall_r2_chan(analysis_fit_ch$GCP_1deg,   analysis_fit_ch$prev_yr_true_gdp,   analysis_fit_ch$pred_GCP,   analysis_fit_ch$prev_yr_pred_gdp)

    w_r2_levl_dped_fit[i]  <- within_iso_r2_levl(developed_fit,  GCP_1deg, pred_GCP)
    w_r2_levl_dping_fit[i] <- within_iso_r2_levl(developing_fit, GCP_1deg, pred_GCP)
    w_r2_levl_all_fit[i]   <- within_iso_r2_levl(analysis_fit,   GCP_1deg, pred_GCP)

    w_r2_chan_dped_fit[i]  <- within_iso_r2_chan(developed_fit_ch,  GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)
    w_r2_chan_dping_fit[i] <- within_iso_r2_chan(developing_fit_ch, GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)
    w_r2_chan_all_fit[i]   <- within_iso_r2_chan(analysis_fit_ch,   GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)

    # ---- Compute out-of-sample (OOS) metrics ----
    developed  <- assessment_pred %>% filter(is_developing == 0)
    developing <- assessment_pred %>% filter(is_developing == 1)

    developed_ch       <- developed       %>% group_by(iso) %>% filter(year != min(year)) %>% ungroup()
    developing_ch      <- developing      %>% group_by(iso) %>% filter(year != min(year)) %>% ungroup()
    assessment_pred_ch <- assessment_pred  %>% group_by(iso) %>% filter(year != min(year)) %>% ungroup()

    mse_GDP_sh_dped[i]  <- if (nrow(developed) > 0)  MSE_GDP_sh(developed$GCP_share_1deg,  developed$pred_GCP_sh)  else NA
    mse_GDP_sh_dping[i] <- if (nrow(developing) > 0) MSE_GDP_sh(developing$GCP_share_1deg, developing$pred_GCP_sh) else NA
    mse_GDP_sh_all[i]   <- MSE_GDP_sh(assessment_pred$GCP_share_1deg, assessment_pred$pred_GCP_sh)

    mse_levl_dped[i]  <- if (nrow(developed) > 0)  MSE_levl(developed$GCP_1deg,  developed$pred_GCP)  else NA
    mse_levl_dping[i] <- if (nrow(developing) > 0) MSE_levl(developing$GCP_1deg, developing$pred_GCP) else NA
    mse_levl_all[i]   <- MSE_levl(assessment_pred$GCP_1deg, assessment_pred$pred_GCP)

    mse_chan_dped[i]  <- if (nrow(developed_ch) > 0)  MSE_chan(developed_ch$GCP_1deg,  developed_ch$prev_yr_true_gdp,  developed_ch$pred_GCP,  developed_ch$prev_yr_pred_gdp)  else NA
    mse_chan_dping[i] <- if (nrow(developing_ch) > 0) MSE_chan(developing_ch$GCP_1deg, developing_ch$prev_yr_true_gdp, developing_ch$pred_GCP, developing_ch$prev_yr_pred_gdp) else NA
    mse_chan_all[i]   <- MSE_chan(assessment_pred_ch$GCP_1deg, assessment_pred_ch$prev_yr_true_gdp, assessment_pred_ch$pred_GCP, assessment_pred_ch$prev_yr_pred_gdp)

    o_r2_levl_dped[i]  <- if (nrow(developed) > 0)  overall_r2_levl(developed$GCP_1deg,  developed$pred_GCP)  else NA
    o_r2_levl_dping[i] <- if (nrow(developing) > 0) overall_r2_levl(developing$GCP_1deg, developing$pred_GCP) else NA
    o_r2_levl_all[i]   <- overall_r2_levl(assessment_pred$GCP_1deg, assessment_pred$pred_GCP)

    o_r2_chan_dped[i]  <- if (nrow(developed_ch) > 0)  overall_r2_chan(developed_ch$GCP_1deg,  developed_ch$prev_yr_true_gdp,  developed_ch$pred_GCP,  developed_ch$prev_yr_pred_gdp)  else NA
    o_r2_chan_dping[i] <- if (nrow(developing_ch) > 0) overall_r2_chan(developing_ch$GCP_1deg, developing_ch$prev_yr_true_gdp, developing_ch$pred_GCP, developing_ch$prev_yr_pred_gdp) else NA
    o_r2_chan_all[i]   <- overall_r2_chan(assessment_pred_ch$GCP_1deg, assessment_pred_ch$prev_yr_true_gdp, assessment_pred_ch$pred_GCP, assessment_pred_ch$prev_yr_pred_gdp)

    w_r2_levl_dped[i]  <- if (nrow(developed) > 1)  within_iso_r2_levl(developed,  GCP_1deg, pred_GCP)  else NA
    w_r2_levl_dping[i] <- if (nrow(developing) > 1) within_iso_r2_levl(developing, GCP_1deg, pred_GCP) else NA
    w_r2_levl_all[i]   <- within_iso_r2_levl(assessment_pred, GCP_1deg, pred_GCP)

    w_r2_chan_dped[i]  <- if (nrow(developed_ch) > 1)  within_iso_r2_chan(developed_ch,  GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)  else NA
    w_r2_chan_dping[i] <- if (nrow(developing_ch) > 1) within_iso_r2_chan(developing_ch, GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp) else NA
    w_r2_chan_all[i]   <- within_iso_r2_chan(assessment_pred_ch, GCP_1deg, prev_yr_true_gdp, pred_GCP, prev_yr_pred_gdp)

    rm(fit, analysis_data, assessment_data, analysis_fit, assessment_pred); gc()
  }

  # ---- Collect per-fold detail ----
  fold_detail <- tibble(
    fold = 1:n_folds,
    pre_r2_levl_dped_fit, pre_r2_levl_dping_fit, pre_r2_levl_all_fit,
    mse_GDP_sh_dped_fit, mse_GDP_sh_dping_fit, mse_GDP_sh_all_fit,
    mse_levl_dped_fit, mse_levl_dping_fit, mse_levl_all_fit,
    mse_chan_dped_fit, mse_chan_dping_fit, mse_chan_all_fit,
    o_r2_levl_dped_fit, o_r2_levl_dping_fit, o_r2_levl_all_fit,
    o_r2_chan_dped_fit, o_r2_chan_dping_fit, o_r2_chan_all_fit,
    w_r2_levl_dped_fit, w_r2_levl_dping_fit, w_r2_levl_all_fit,
    w_r2_chan_dped_fit, w_r2_chan_dping_fit, w_r2_chan_all_fit,
    pre_r2_levl_dped, pre_r2_levl_dping, pre_r2_levl_all,
    mse_GDP_sh_dped, mse_GDP_sh_dping, mse_GDP_sh_all,
    mse_levl_dped, mse_levl_dping, mse_levl_all,
    mse_chan_dped, mse_chan_dping, mse_chan_all,
    o_r2_levl_dped, o_r2_levl_dping, o_r2_levl_all,
    o_r2_chan_dped, o_r2_chan_dping, o_r2_chan_all,
    w_r2_levl_dped, w_r2_levl_dping, w_r2_levl_all,
    w_r2_chan_dped, w_r2_chan_dping, w_r2_chan_all
  )

  # ---- Compute summary: mean across folds, plus weighted averages ----
  w_dev     <- real_share$share[real_share$is_developing == 0]
  w_devping <- real_share$share[real_share$is_developing == 1]
  safe_mean <- function(x) mean(x, na.rm = TRUE)

  summary_row <- data.frame(
    cv_type = cv_label, n_folds = n_folds,
    # Fit
    m_prer2_levl_dped_fit  = safe_mean(pre_r2_levl_dped_fit),
    m_prer2_levl_dping_fit = safe_mean(pre_r2_levl_dping_fit),
    wgtm_prer2_levl_fit    = safe_mean(pre_r2_levl_dped_fit) * w_dev + safe_mean(pre_r2_levl_dping_fit) * w_devping,
    m_prer2_levl_all_fit   = safe_mean(pre_r2_levl_all_fit),
    m_mse_GDP_sh_dped_fit = safe_mean(mse_GDP_sh_dped_fit), m_mse_GDP_sh_dping_fit = safe_mean(mse_GDP_sh_dping_fit),
    wgtm_mse_GDP_sh_fit = safe_mean(mse_GDP_sh_dped_fit)*w_dev + safe_mean(mse_GDP_sh_dping_fit)*w_devping,
    m_mse_GDP_sh_all_fit = safe_mean(mse_GDP_sh_all_fit),
    m_mse_levl_dped_fit = safe_mean(mse_levl_dped_fit), m_mse_levl_dping_fit = safe_mean(mse_levl_dping_fit),
    wgtm_mse_levl_fit = safe_mean(mse_levl_dped_fit)*w_dev + safe_mean(mse_levl_dping_fit)*w_devping,
    m_mse_levl_all_fit = safe_mean(mse_levl_all_fit),
    m_mse_chan_dped_fit = safe_mean(mse_chan_dped_fit), m_mse_chan_dping_fit = safe_mean(mse_chan_dping_fit),
    wgtm_mse_chan_fit = safe_mean(mse_chan_dped_fit)*w_dev + safe_mean(mse_chan_dping_fit)*w_devping,
    m_mse_chan_all_fit = safe_mean(mse_chan_all_fit),
    m_or2_levl_dped_fit = safe_mean(o_r2_levl_dped_fit), m_or2_levl_dping_fit = safe_mean(o_r2_levl_dping_fit),
    wgtm_or2_levl_fit = safe_mean(o_r2_levl_dped_fit)*w_dev + safe_mean(o_r2_levl_dping_fit)*w_devping,
    m_or2_levl_all_fit = safe_mean(o_r2_levl_all_fit),
    m_or2_chan_dped_fit = safe_mean(o_r2_chan_dped_fit), m_or2_chan_dping_fit = safe_mean(o_r2_chan_dping_fit),
    wgtm_or2_chan_fit = safe_mean(o_r2_chan_dped_fit)*w_dev + safe_mean(o_r2_chan_dping_fit)*w_devping,
    m_or2_chan_all_fit = safe_mean(o_r2_chan_all_fit),
    m_wr2_levl_dped_fit = safe_mean(w_r2_levl_dped_fit), m_wr2_levl_dping_fit = safe_mean(w_r2_levl_dping_fit),
    wgtm_wr2_levl_fit = safe_mean(w_r2_levl_dped_fit)*w_dev + safe_mean(w_r2_levl_dping_fit)*w_devping,
    m_wr2_levl_all_fit = safe_mean(w_r2_levl_all_fit),
    m_wr2_chan_dped_fit = safe_mean(w_r2_chan_dped_fit), m_wr2_chan_dping_fit = safe_mean(w_r2_chan_dping_fit),
    wgtm_wr2_chan_fit = safe_mean(w_r2_chan_dped_fit)*w_dev + safe_mean(w_r2_chan_dping_fit)*w_devping,
    m_wr2_chan_all_fit = safe_mean(w_r2_chan_all_fit),
    # OOS
    m_prer2_levl_dped  = safe_mean(pre_r2_levl_dped),
    m_prer2_levl_dping = safe_mean(pre_r2_levl_dping),
    wgtm_prer2_levl    = safe_mean(pre_r2_levl_dped) * w_dev + safe_mean(pre_r2_levl_dping) * w_devping,
    m_prer2_levl_all   = safe_mean(pre_r2_levl_all),
    m_mse_GDP_sh_dped = safe_mean(mse_GDP_sh_dped), m_mse_GDP_sh_dping = safe_mean(mse_GDP_sh_dping),
    wgtm_mse_GDP_sh = safe_mean(mse_GDP_sh_dped)*w_dev + safe_mean(mse_GDP_sh_dping)*w_devping,
    m_mse_GDP_sh_all = safe_mean(mse_GDP_sh_all),
    m_mse_levl_dped = safe_mean(mse_levl_dped), m_mse_levl_dping = safe_mean(mse_levl_dping),
    wgtm_mse_levl = safe_mean(mse_levl_dped)*w_dev + safe_mean(mse_levl_dping)*w_devping,
    m_mse_levl_all = safe_mean(mse_levl_all),
    m_mse_chan_dped = safe_mean(mse_chan_dped), m_mse_chan_dping = safe_mean(mse_chan_dping),
    wgtm_mse_chan = safe_mean(mse_chan_dped)*w_dev + safe_mean(mse_chan_dping)*w_devping,
    m_mse_chan_all = safe_mean(mse_chan_all),
    m_or2_levl_dped = safe_mean(o_r2_levl_dped), m_or2_levl_dping = safe_mean(o_r2_levl_dping),
    wgtm_or2_levl = safe_mean(o_r2_levl_dped)*w_dev + safe_mean(o_r2_levl_dping)*w_devping,
    m_or2_levl_all = safe_mean(o_r2_levl_all),
    m_or2_chan_dped = safe_mean(o_r2_chan_dped), m_or2_chan_dping = safe_mean(o_r2_chan_dping),
    wgtm_or2_chan = safe_mean(o_r2_chan_dped)*w_dev + safe_mean(o_r2_chan_dping)*w_devping,
    m_or2_chan_all = safe_mean(o_r2_chan_all),
    m_wr2_levl_dped = safe_mean(w_r2_levl_dped), m_wr2_levl_dping = safe_mean(w_r2_levl_dping),
    wgtm_wr2_levl = safe_mean(w_r2_levl_dped)*w_dev + safe_mean(w_r2_levl_dping)*w_devping,
    m_wr2_levl_all = safe_mean(w_r2_levl_all),
    m_wr2_chan_dped = safe_mean(w_r2_chan_dped), m_wr2_chan_dping = safe_mean(w_r2_chan_dping),
    wgtm_wr2_chan = safe_mean(w_r2_chan_dped)*w_dev + safe_mean(w_r2_chan_dping)*w_devping,
    m_wr2_chan_all = safe_mean(w_r2_chan_all),
    stringsAsFactors = FALSE
  )

  return(list(fold_detail = fold_detail, summary = summary_row))
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# 7. Run baseline: random country-level group_vfold_cv (v=5)
# ---------------------------------------------------------------------------------------------------------------------------------------

all_summary_results <- list()
all_fold_details    <- list()
all_cluster_assignments <- list()

if (run_baseline) {

  set.seed(1234567)
  folds_baseline <- group_vfold_cv(data_full, group = "iso", v = 5)

  baseline_results <- run_cv_evaluation(data_full, folds_baseline, "baseline_random_v5", real_share)

  all_summary_results[["baseline"]] <- baseline_results$summary %>%
    mutate(group_size = NA_integer_, n_clusters = NA_integer_, cv_method = "random_country")
  all_fold_details[["baseline"]] <- baseline_results$fold_detail %>%
    mutate(cv_type = "baseline_random_v5", group_size = NA_integer_)

  toc()

  cat("\n--- Baseline OOS Results ---\n")
  cat(sprintf("  Overall R2 Level (weighted): %.4f\n", baseline_results$summary$wgtm_or2_levl))
  cat(sprintf("  Overall R2 Change (weighted): %.4f\n", baseline_results$summary$wgtm_or2_chan))
  cat(sprintf("  Within R2 Level (weighted): %.4f\n", baseline_results$summary$wgtm_wr2_levl))
  cat(sprintf("  Within R2 Change (weighted): %.4f\n", baseline_results$summary$wgtm_wr2_chan))
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# 8. Run spatial block CV for each group_size
# ---------------------------------------------------------------------------------------------------------------------------------------

for (gs in group_sizes) {
  cat(paste0("\n========== Spatial Block CV: group_size = ", gs, " cells/cluster ==========\n"))
  tic(paste0("Spatial block CV (group_size=", gs, ")"))

  # Within-iso k-means: for each iso, cluster its cells into groups of ~gs cells.
  # As gs approaches the number of cells in an iso, that iso collapses to 1 cluster
  # (= the entire country), converging to the original country-level CV.
  set.seed(1234567)
  cluster_lookup_list <- list()
  n_total_clusters <- 0
  n_single_cluster_isos <- 0

  for (current_iso in unique(cell_iso_coords$iso)) {
    iso_cells <- cell_iso_coords %>% filter(iso == current_iso)
    n_cells <- nrow(iso_cells)
    n_cl <- max(1, ceiling(n_cells / gs))

    if (n_cl <= 1 || n_cells <= 1) {
      # Entire iso is one cluster
      iso_cells$spatial_cluster <- paste0(current_iso, "_cl_1")
      n_single_cluster_isos <- n_single_cluster_isos + 1
    } else if (n_cl >= n_cells) {
      # Each cell is its own cluster (degenerate case, e.g. group_size = 1)
      iso_cells$spatial_cluster <- paste0(current_iso, "_cl_", seq_len(n_cells))
    } else {
      # Scale longitude by cos(latitude) to approximate equal-area distances
      # Without this, 1 degree of longitude at 60N covers ~56 km vs ~111 km for latitude,
      # causing clusters to be elongated east-west at high latitudes
      km_coords <- iso_cells[, c("lon", "lat")]
      km_coords$lon_scaled <- km_coords$lon * cos(km_coords$lat * pi / 180)
      km <- kmeans(km_coords[, c("lon_scaled", "lat")], centers = n_cl, nstart = 25, iter.max = 100)
      iso_cells$spatial_cluster <- paste0(current_iso, "_cl_", km$cluster)
    }

    n_total_clusters <- n_total_clusters + max(1, n_cl)
    cluster_lookup_list[[current_iso]] <- iso_cells %>%
      dplyr::select(cell_id, iso, spatial_cluster)
  }

  cluster_lookup <- bind_rows(cluster_lookup_list)
  cat(sprintf("  Total clusters: %d across %d isos (%d isos have 1 cluster = full country)\n",
              n_total_clusters, length(unique(cell_iso_coords$iso)), n_single_cluster_isos))

  # Save cluster assignments (with coordinates for plotting)
  cluster_with_coords <- cluster_lookup %>%
    left_join(cell_coords_df, by = "cell_id")
  write.csv(cluster_with_coords,
            file = file.path(output_dir, paste0("cell_clusters_gs", gs, ".csv")),
            row.names = FALSE)
  all_cluster_assignments[[as.character(gs)]] <- cluster_with_coords

  # Print summary
  cluster_sizes <- table(cluster_lookup$spatial_cluster)
  cat(sprintf("  Cluster sizes: min=%d, median=%d, max=%d\n",
              min(cluster_sizes), median(cluster_sizes), max(cluster_sizes)))

  # Add cluster assignment to data_full (join by cell_id AND iso)
  data_full_clustered <- data_full %>%
    left_join(cluster_lookup, by = c("cell_id", "iso"))

  # Create spatial CV folds: assign clusters to n_cv_folds folds
  set.seed(1234567)
  folds_spatial <- group_vfold_cv(data_full_clustered, group = "spatial_cluster", v = n_cv_folds)

  # Run CV
  cv_label <- paste0("spatial_gs", gs)
  spatial_results <- run_cv_evaluation(data_full_clustered, folds_spatial, cv_label, real_share)

  # Store results
  all_summary_results[[as.character(gs)]] <- spatial_results$summary %>%
    mutate(group_size = gs, n_clusters = n_total_clusters, cv_method = "spatial_within_iso_kmeans")
  all_fold_details[[as.character(gs)]] <- spatial_results$fold_detail %>%
    mutate(cv_type = cv_label, group_size = gs)

  write.csv(spatial_results$fold_detail,
            file = file.path(output_dir, paste0("fold_metrics_gs", gs, ".csv")),
            row.names = FALSE)

  toc()

  cat(paste0("\n--- Spatial Block CV (group_size=", gs, ") OOS Results ---\n"))
  cat(sprintf("  Overall R2 Level (weighted): %.4f\n", spatial_results$summary$wgtm_or2_levl))
  cat(sprintf("  Overall R2 Change (weighted): %.4f\n", spatial_results$summary$wgtm_or2_chan))
  cat(sprintf("  Within R2 Level (weighted): %.4f\n", spatial_results$summary$wgtm_wr2_levl))
  cat(sprintf("  Within R2 Change (weighted): %.4f\n", spatial_results$summary$wgtm_wr2_chan))

  data_full_clustered$spatial_cluster <- NULL
  gc()
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# 9. Consolidate and save all results
# ---------------------------------------------------------------------------------------------------------------------------------------

summary_results    <- bind_rows(all_summary_results)
fold_details_all   <- bind_rows(all_fold_details)

save(summary_results, fold_details_all, all_cluster_assignments, cell_coords_df,
     file = file.path(output_dir, "spatial_block_cv_all_results.RData"))

write.csv(summary_results, file = file.path(output_dir, "spatial_block_cv_summary.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# 10. Appendix Section 8.2 table — out-of-sample R^2 by group size and category
# ---------------------------------------------------------------------------------------------------------------------------------------

# Hardcoded baseline (random country CV, v=5) values from the 1-degree benchmark
# model — same values used for the "Baseline" column of the LaTeX table.
baseline_vals <- list(
  Level  = c(Developed = 0.9832, Developing = 0.9594, All = 0.9795, Weighted = 0.9660),
  Change = c(Developed = 0.7355, Developing = 0.7937, All = 0.7620, Weighted = 0.7774)
)

metric_cols <- list(
  Level  = c(Developed = "m_or2_levl_dped",  Developing = "m_or2_levl_dping",
             All       = "m_or2_levl_all",   Weighted   = "wgtm_or2_levl"),
  Change = c(Developed = "m_or2_chan_dped",  Developing = "m_or2_chan_dping",
             All       = "m_or2_chan_all",   Weighted   = "wgtm_or2_chan")
)

spatial_summary <- summary_results %>%
  filter(cv_method == "spatial_within_iso_kmeans") %>%
  arrange(group_size)

appendix_rows <- list()
for (panel in names(metric_cols)) {
  for (cat_name in names(metric_cols[[panel]])) {
    col_name <- metric_cols[[panel]][[cat_name]]
    # Spatial CV values, one per group_size
    for (i in seq_len(nrow(spatial_summary))) {
      appendix_rows[[length(appendix_rows) + 1]] <- data.frame(
        panel       = panel,
        category    = cat_name,
        group_size  = as.character(spatial_summary$group_size[i]),
        r_squared   = as.numeric(spatial_summary[[col_name]][i]),
        stringsAsFactors = FALSE
      )
    }
    # Baseline (random country CV) reference value
    appendix_rows[[length(appendix_rows) + 1]] <- data.frame(
      panel       = panel,
      category    = cat_name,
      group_size  = "Baseline",
      r_squared   = as.numeric(baseline_vals[[panel]][[cat_name]]),
      stringsAsFactors = FALSE
    )
  }
}

appendix_table <- do.call(rbind, appendix_rows)

write.csv(appendix_table,
          file = file.path(output_dir, "spatial_block_cv_results.csv"),
          row.names = FALSE)
cat(paste0("CSV saved to: ",
           file.path(output_dir, "spatial_block_cv_results.csv"), "\n"))

