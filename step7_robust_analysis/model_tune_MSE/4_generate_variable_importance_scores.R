# --------------------------------- Task Summary --------------------------------- #
# This script calculates cell-level variable importance scores for the MSE-tuned model variants (1deg, 0.5deg, 0.25deg) by mean-imputing each predictor in turn and recording the change in out-of-sample R^2 / MSE / squared correlation.
# -------------------------------------------------------------------------------- #

Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(ranger)
library(tidymodels)

# ============================================================================ #
# CONFIGURATION
# ============================================================================ #

resolutions <- c("1deg", "0_5deg", "0_25deg")

predictor_vars <- c(
  "pop_total_share", "pop_urban_share", "pop_cropland_share", "pop_other_share",
  "CO2_bio_manuf_conbust_share", "CO2_bio_heavy_indus_share", "CO2_bio_tspt_share",
  "CO2_non_org_manuf_conbust_share", "CO2_non_org_heavy_indus_share", "CO2_non_org_tspt_share",
  "NPP_share", "NTL_urban_snow_free_period_share", "NTL_cropland_snow_free_period_share",
  "NTL_other_snow_free_period_share", "snow_ice_share", "water_share", "urban_share",
  "forest_share", "cropland_share", "mean_rug", "national_gdpc",
  "lag_NTL_urban_share", "lag_urban_share", "lag_cropland_share", "lag_NTL_other_share",
  "lag_NTL_cropland_share", "lag_CO2_bio_mc_share", "lag_CO2_nonorg_mc_share",
  "lag_CO2_bio_heavy_indus_share", "lag_CO2_non_org_heavy_indus_share",
  "lag_CO2_bio_tspt_share", "lag_CO2_non_org_tspt_share", "lag_pop_urban_share",
  "lag_NPP_share", "lag_pop_cropland_share", "lag_pop_other_share"
)

# ============================================================================ #
# Helper Functions
# ============================================================================ #

calc_overall_r2 <- function(true_vals, pred_vals) {
  valid <- !is.na(true_vals) & !is.na(pred_vals) & is.finite(true_vals) & is.finite(pred_vals)
  true_vals <- true_vals[valid]; pred_vals <- pred_vals[valid]
  1 - sum((true_vals - pred_vals)^2) / sum((true_vals - mean(true_vals))^2)
}

calc_overall_mse <- function(true_vals, pred_vals) {
  valid <- !is.na(true_vals) & !is.na(pred_vals) & is.finite(true_vals) & is.finite(pred_vals)
  mean((true_vals[valid] - pred_vals[valid])^2)
}

calc_corr_sq_overall <- function(true_vals, pred_vals) {
  valid <- !is.na(true_vals) & !is.na(pred_vals) & is.finite(true_vals) & is.finite(pred_vals)
  cor(true_vals[valid], pred_vals[valid])^2
}

calc_within_r2 <- function(df, true_col, pred_col) {
  df_calc <- df %>%
    filter(!is.na(.data[[true_col]]) & !is.na(.data[[pred_col]]) &
             is.finite(.data[[true_col]]) & is.finite(.data[[pred_col]])) %>%
    group_by(iso, year) %>%
    mutate(mean_true = mean(.data[[true_col]])) %>%
    ungroup()
  1 - sum((df_calc[[true_col]] - df_calc[[pred_col]])^2) / sum((df_calc[[true_col]] - df_calc$mean_true)^2)
}

calc_within_mse <- function(df, true_col, pred_col) {
  df_calc <- df %>%
    filter(!is.na(.data[[true_col]]) & !is.na(.data[[pred_col]]) &
             is.finite(.data[[true_col]]) & is.finite(.data[[pred_col]])) %>%
    group_by(iso, year) %>%
    mutate(true_dm = .data[[true_col]] - mean(.data[[true_col]]),
           pred_dm = .data[[pred_col]] - mean(.data[[pred_col]])) %>%
    ungroup()
  mean((df_calc$true_dm - df_calc$pred_dm)^2)
}

calc_corr_sq_within <- function(df, true_col, pred_col) {
  df_calc <- df %>%
    filter(!is.na(.data[[true_col]]) & !is.na(.data[[pred_col]]) &
             is.finite(.data[[true_col]]) & is.finite(.data[[pred_col]])) %>%
    group_by(iso, year) %>%
    mutate(true_dm = .data[[true_col]] - mean(.data[[true_col]]),
           pred_dm = .data[[pred_col]] - mean(.data[[pred_col]])) %>%
    ungroup()
  cor(df_calc$true_dm, df_calc$pred_dm)^2
}

# ============================================================================ #
# Main Processing Function
# ============================================================================ #

process_resolution <- function(res) {

  cat("Processing:", res, "\n")

  target_var <- paste0("GCP_share_", res)

  # Load model
  load(paste0("step7_robust_analysis/model_tune_MSE/rf_model9_good_grid_search_", res, ".RData"))
  rf_model <- get(paste0("rf_model9_good_grid_search_", res))

  # Load and combine data
  data_full <- bind_rows(
    read.csv(paste0("step4_benchmark_model/outputs/new_data_train_", res, ".csv")),
    read.csv(paste0("step4_benchmark_model/outputs/new_data_valid_year_", res, ".csv")),
    read.csv(paste0("step4_benchmark_model/outputs/new_data_valid_iso_", res, ".csv")),
    read.csv(paste0("step4_benchmark_model/outputs/new_data_test_year_", res, ".csv")),
    read.csv(paste0("step4_benchmark_model/outputs/new_data_test_iso_", res, ".csv"))
  )

  # Calculate productivity ratio
  calc_ratio <- function(pred_shares, data) {
    data %>%
      mutate(pred_share_raw = pred_shares,
             pred_ratio = ifelse(pop_total_share > 0, pred_share_raw / pop_total_share, NA),
             true_ratio = ifelse(pop_total_share > 0, .data[[target_var]] / pop_total_share, NA),
             log_pred_ratio = ifelse(pred_ratio > 0, log(pred_ratio), NA),
             log_true_ratio = ifelse(true_ratio > 0, log(true_ratio), NA))
  }

  # Baseline predictions
  baseline_pred <- predict(rf_model, data_full)$.pred
  data_with_ratio <- calc_ratio(baseline_pred, data_full)
  data_valid <- data_with_ratio %>%
    filter(!is.na(log_pred_ratio) & !is.na(log_true_ratio) &
             is.finite(log_pred_ratio) & is.finite(log_true_ratio))

  # Baseline metrics
  baseline <- list(
    r2_overall = calc_overall_r2(data_valid$log_true_ratio, data_valid$log_pred_ratio),
    r2_within = calc_within_r2(data_valid, "log_true_ratio", "log_pred_ratio"),
    corr_sq_overall = calc_corr_sq_overall(data_valid$log_true_ratio, data_valid$log_pred_ratio),
    corr_sq_within = calc_corr_sq_within(data_valid, "log_true_ratio", "log_pred_ratio"),
    mse_overall = calc_overall_mse(data_valid$log_true_ratio, data_valid$log_pred_ratio),
    mse_within = calc_within_mse(data_valid, "log_true_ratio", "log_pred_ratio")
  )

  # Variable importance loop
  results <- map_dfr(predictor_vars, function(var_name) {
    data_replaced <- data_full %>% mutate(!!var_name := mean(.data[[var_name]], na.rm = TRUE))
    replaced_pred <- predict(rf_model, data_replaced)$.pred
    data_replaced_ratio <- calc_ratio(replaced_pred, data_replaced)
    dv <- data_replaced_ratio %>%
      filter(!is.na(log_pred_ratio) & !is.na(log_true_ratio) &
               is.finite(log_pred_ratio) & is.finite(log_true_ratio))

    tibble(
      Variable = var_name,
      R2_Overall_After = calc_overall_r2(dv$log_true_ratio, dv$log_pred_ratio),
      R2_Within_After = calc_within_r2(dv, "log_true_ratio", "log_pred_ratio"),
      CorrSq_Overall_After = calc_corr_sq_overall(dv$log_true_ratio, dv$log_pred_ratio),
      CorrSq_Within_After = calc_corr_sq_within(dv, "log_true_ratio", "log_pred_ratio"),
      MSE_Overall_After = calc_overall_mse(dv$log_true_ratio, dv$log_pred_ratio),
      MSE_Within_After = calc_within_mse(dv, "log_true_ratio", "log_pred_ratio")
    )
  })

  # Calculate importance and ranks
  importance_df <- results %>%
    mutate(
      Baseline_R2_Overall = baseline$r2_overall,
      Baseline_R2_Within = baseline$r2_within,
      Baseline_CorrSq_Overall = baseline$corr_sq_overall,
      Baseline_CorrSq_Within = baseline$corr_sq_within,
      Baseline_MSE_Overall = baseline$mse_overall,
      Baseline_MSE_Within = baseline$mse_within,
      Importance_R2_Overall = baseline$r2_overall - R2_Overall_After,
      Importance_R2_Within = baseline$r2_within - R2_Within_After,
      Importance_CorrSq_Overall = baseline$corr_sq_overall - CorrSq_Overall_After,
      Importance_CorrSq_Within = baseline$corr_sq_within - CorrSq_Within_After,
      Importance_MSE_Overall = MSE_Overall_After - baseline$mse_overall,
      Importance_MSE_Within = MSE_Within_After - baseline$mse_within,
      Rank_R2_Overall = rank(-Importance_R2_Overall),
      Rank_R2_Within = rank(-Importance_R2_Within),
      Rank_CorrSq_Overall = rank(-Importance_CorrSq_Overall),
      Rank_CorrSq_Within = rank(-Importance_CorrSq_Within),
      Rank_MSE_Overall = rank(-Importance_MSE_Overall),
      Rank_MSE_Within = rank(-Importance_MSE_Within)
    ) %>%
    arrange(desc(Importance_CorrSq_Within))

  # Save output
  output_csv <- paste0("step7_robust_analysis/model_tune_MSE/outputs/importance_scores_", res, ".csv")
  write.csv(importance_df, output_csv, row.names = FALSE)
  cat("Saved:", output_csv, "\n")
}

# ============================================================================ #
# MAIN EXECUTION
# ============================================================================ #

for (res in resolutions) {
  process_resolution(res)
}

cat("Done.\n")
