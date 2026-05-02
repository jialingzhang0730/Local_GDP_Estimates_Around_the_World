# --------------------------------- Task Summary --------------------------------- #
# This file mirrors 1_agriculture_robustness.R but evaluated on year-over-year log changes in cell-level GDP rather than log levels, to test the referee concern that agricultural dynamics are not captured by the model (Online Appendix Section 9.3).
# -------------------------------------------------------------------------------- #

Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(dplyr)
library(sandwich)
library(lmtest)

output_dir <- "step7_robust_analysis/cropland/outputs"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------------------------------------------------------------------

# Mirror of overall_r2_chan in 2_put_all_isos_to_train_1deg.R, but accepting precomputed diffs.
overall_r2_chan_from_diffs <- function(true_diff, pred_diff) {
  valid <- is.finite(true_diff) & is.finite(pred_diff)
  if (sum(valid) < 2) return(NA_real_)
  td <- true_diff[valid]
  pd <- pred_diff[valid]
  if (var(td) < 1e-10) return(NA_real_)
  1 - sum((td - pd)^2) / sum((td - mean(td))^2)
}

fmt4 <- function(x) {
  if (is.na(x)) return("---")
  formatC(x, format = "f", digits = 4)
}

# Percent-formatter for R^2 values in LaTeX tables (matches the standalone writeup).
fmt_pct <- function(x, digits = 2) {
  if (is.na(x)) return("---")
  paste0(formatC(100 * x, format = "f", digits = digits), "\\%")
}

fmt_n <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.1)  return("$^{*}$")
  return("")
}

fmt_coef <- function(x, p, digits = 4) {
  paste0(formatC(x, format = "f", digits = digits), stars_fn(p))
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Degree-level configurations (identical to 1_agriculture_robustness.R)
# ---------------------------------------------------------------------------------------------------------------------------------------

deg_configs <- list(
  list(
    label = "1deg", deg_label = "1-degree Model", cell_step = 1,
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_1deg.csv",
    oos_gdp_col = "GCP_1deg",
    id_cols = "cell_id",
    extra_char_cols = character(0),
    lc_file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_1deg.RData",
    lc_obj_name = "lc_full_1deg"
  ),
  list(
    label = "0_5deg", deg_label = "0.5-degree Model", cell_step = 0.5,
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_5deg.csv",
    oos_gdp_col = "GCP_0_5deg",
    id_cols = c("cell_id", "subcell_id"),
    extra_char_cols = "subcell_id",
    lc_file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_5deg.RData",
    lc_obj_name = "lc_full_0_5deg"
  ),
  list(
    label = "0_25deg", deg_label = "0.25-degree Model", cell_step = 0.25,
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_25deg.csv",
    oos_gdp_col = "GCP_0_25deg",
    id_cols = c("cell_id", "subcell_id", "subcell_id_0_25"),
    extra_char_cols = c("subcell_id", "subcell_id_0_25"),
    lc_file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_25deg.RData",
    lc_obj_name = "lc_full_0_25deg"
  )
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------------------------------------------------------------------

results <- list()

for (cfg in deg_configs) {
  cat(paste0("\n==================== Processing ", cfg$label, " (changes) ====================\n"))

  # --- 1. Load OOS cross-validation predictions ---
  cat("  Loading OOS CV predictions...\n")
  oos_raw <- read.csv(cfg$oos_file)
  oos_raw$cell_id <- as.character(oos_raw$cell_id)
  for (col in cfg$extra_char_cols) oos_raw[[col]] <- as.character(oos_raw[[col]])

  # --- 2. Aggregate to country level (handling USA states) ---
  true_and_predictors_agg <- oos_raw %>%
    mutate(iso_for_join = ifelse(substr(iso, 1, 4) == "USA_", "USA", iso)) %>%
    group_by(across(all_of(c("iso_for_join", cfg$id_cols, "year")))) %>%
    summarise(
      oos_predicted_GCP = sum(oos_predicted_GCP),
      true_GCP = sum(.data[[cfg$oos_gdp_col]]),
      pop_total_share = max(pop_total_share),
      iso_original = first(iso),
      .groups = "drop"
    ) %>%
    rename(iso = iso_for_join)

  # --- 3. Load land cover data and compute cropland proportion (per cell-year) ---
  cat("  Loading land cover data and computing cropland proportion...\n")
  load(cfg$lc_file)
  lc_raw <- get(cfg$lc_obj_name)
  rm(list = cfg$lc_obj_name)

  lc_cropland <- lc_raw %>%
    filter(year <= 2022) %>%
    as.data.frame() %>%
    dplyr::select(all_of(c(cfg$id_cols, "year",
                           "barren", "snow_ice", "urban", "dense_forest", "open_forest",
                           "forest_cropland", "herbaceous", "cropland", "shrub", "herbaceous_cropland"))) %>%
    replace(is.na(.), 0) %>%
    mutate(
      cropland_full = cropland + forest_cropland + herbaceous_cropland,
      land_area_km2 = barren + snow_ice + urban + dense_forest + open_forest +
                      forest_cropland + herbaceous + cropland + shrub + herbaceous_cropland
    ) %>%
    group_by(across(all_of(c(cfg$id_cols, "year")))) %>%
    summarise(cropland_full = sum(cropland_full),
              land_area_km2 = sum(land_area_km2),
              .groups = "drop") %>%
    mutate(cropland_proportion = ifelse(land_area_km2 > 0, cropland_full / land_area_km2, 0)) %>%
    dplyr::select(all_of(c(cfg$id_cols, "year", "cropland_proportion")))

  lc_cropland$cell_id <- as.character(lc_cropland$cell_id)
  for (col in cfg$extra_char_cols) lc_cropland[[col]] <- as.character(lc_cropland[[col]])

  rm(lc_raw)

  # --- 4. Merge time-varying cropland proportion onto panel ---
  true_and_predictors_agg <- true_and_predictors_agg %>%
    left_join(lc_cropland, by = c(cfg$id_cols, "year"))

  rm(lc_cropland)

  # --- 5. Compute within-cell lag (year-over-year) of true and predicted GCP ---
  #        Grouping key includes iso_original because border cells have separate rows
  #        per country portion and lag must stay within each portion.
  cat("  Computing lagged GCP for year-over-year log changes...\n")
  true_and_predictors_agg <- true_and_predictors_agg %>%
    arrange(across(all_of(c("iso_original", cfg$id_cols, "year")))) %>%
    group_by(across(all_of(c("iso_original", cfg$id_cols)))) %>%
    mutate(
      true_GCP_last = dplyr::lag(true_GCP, order_by = year),
      pred_GCP_last = dplyr::lag(oos_predicted_GCP, order_by = year)
    ) %>%
    ungroup()

  # --- 6. Compute log diffs and growth error for every valid obs ---
  panel_diffs <- true_and_predictors_agg %>%
    filter(
      !is.na(true_GCP_last) & !is.na(pred_GCP_last) &
      true_GCP > 0 & oos_predicted_GCP > 0 &
      true_GCP_last > 0 & pred_GCP_last > 0 &
      !is.na(pop_total_share) & pop_total_share > 0
    ) %>%
    mutate(
      true_log_diff = log(true_GCP) - log(true_GCP_last),
      pred_log_diff = log(oos_predicted_GCP) - log(pred_GCP_last),
      growth_error  = pred_log_diff - true_log_diff
    )

  # --- 7. Compute cropland-proportion groups (within-ISO percentile rank) ---
  #        Mirrors script 13: rank computed on cell-years with positive cropland
  #        proportion, pooled across years within iso_original. Group flags are
  #        therefore time-varying — each cell-year carries the rank of its own
  #        year's cropland share.
  cat("  Computing cropland proportion groups...\n")
  ag_panel <- panel_diffs %>%
    filter(!is.na(cropland_proportion) & cropland_proportion > 0) %>%
    group_by(iso_original) %>%
    mutate(
      pctile_within_iso = percent_rank(cropland_proportion),
      top_5pct = pctile_within_iso >= 0.95,
      top_10pct = pctile_within_iso >= 0.90
    ) %>%
    ungroup()

  all_panel <- panel_diffs  # baseline includes all cells with valid log diffs

  cat(sprintf("  Ag-classified (cropland > 0) obs: %d | All-in-sample obs: %d\n",
              nrow(ag_panel), nrow(all_panel)))
  cat(sprintf("  Top 5%%: %d | Top 10%%: %d | Bot 90%%: %d\n",
              sum(ag_panel$top_5pct, na.rm = TRUE),
              sum(ag_panel$top_10pct, na.rm = TRUE),
              sum(!ag_panel$top_10pct, na.rm = TRUE)))

  # --- 9. Table 1 metrics: overall and within-ISO R^2 on log changes, by group ---
  top5  <- ag_panel  %>% filter(top_5pct)
  top10 <- ag_panel  %>% filter(top_10pct)
  bot90 <- ag_panel  %>% filter(!top_10pct)

  # Panel A: overall R^2 on log changes
  r2_top5_ov  <- overall_r2_chan_from_diffs(top5$true_log_diff,  top5$pred_log_diff)
  r2_top10_ov <- overall_r2_chan_from_diffs(top10$true_log_diff, top10$pred_log_diff)
  r2_bot90_ov <- overall_r2_chan_from_diffs(bot90$true_log_diff, bot90$pred_log_diff)
  r2_all_ov   <- overall_r2_chan_from_diffs(all_panel$true_log_diff, all_panel$pred_log_diff)

  cat(sprintf("  Overall  R^2(chan) -- Top5: %.4f | Top10: %.4f | Bot90: %.4f | All: %.4f\n",
              r2_top5_ov, r2_top10_ov, r2_bot90_ov, r2_all_ov))

  # Mean cropland_proportion per group (for back-of-envelope calculations)
  mean_crop_top5  <- mean(top5$cropland_proportion,  na.rm = TRUE)
  mean_crop_top10 <- mean(top10$cropland_proportion, na.rm = TRUE)
  mean_crop_bot90 <- mean(bot90$cropland_proportion, na.rm = TRUE)
  cat(sprintf("  Mean cropland -- Top5: %.4f | Top10: %.4f | Bot90: %.4f\n",
              mean_crop_top5, mean_crop_top10, mean_crop_bot90))

  # --- 10. Table 2 regression: growth_error on cropland proportion + pop share + ISO FE ---
  cat("  Running regression with cluster-robust SEs...\n")
  reg_data <- ag_panel %>%
    filter(is.finite(cropland_proportion) & cropland_proportion > 0 &
           is.finite(growth_error) & is.finite(pop_total_share) & pop_total_share > 0)
  reg_data$iso_fe <- factor(reg_data$iso_original)
  reg_data$cluster_id <- do.call(paste, c(reg_data[cfg$id_cols], sep = "_"))

  reg <- lm(growth_error ~ cropland_proportion + pop_total_share + iso_fe, data = reg_data)
  stopifnot(nrow(reg$model) == nrow(reg_data))
  s <- summary(reg)

  vcov_cl <- sandwich::vcovCL(reg, cluster = reg_data$cluster_id, type = "HC1")
  ct <- lmtest::coeftest(reg, vcov. = vcov_cl)

  res <- list(
    label = cfg$label, deg_label = cfg$deg_label,
    n_top5 = nrow(top5), n_top10 = nrow(top10), n_bot90 = nrow(bot90), n_all = nrow(all_panel),
    mean_crop_top5 = mean_crop_top5, mean_crop_top10 = mean_crop_top10, mean_crop_bot90 = mean_crop_bot90,
    r2_top5_ov  = r2_top5_ov,  r2_top10_ov = r2_top10_ov, r2_bot90_ov = r2_bot90_ov, r2_all_ov = r2_all_ov,
    b_cropland  = unname(ct["cropland_proportion", "Estimate"]),
    se_cropland = unname(ct["cropland_proportion", "Std. Error"]),
    p_cropland  = unname(ct["cropland_proportion", "Pr(>|t|)"]),
    b_pop  = unname(ct["pop_total_share", "Estimate"]),
    se_pop = unname(ct["pop_total_share", "Std. Error"]),
    p_pop  = unname(ct["pop_total_share", "Pr(>|t|)"]),
    adj_r2 = s$adj.r.squared,
    n_reg = nrow(reg$model),
    n_clusters = length(unique(reg_data$cluster_id))
  )

  cat(sprintf("  Reg: beta = %.4f (cluster-robust se = %.4f, p = %.4f) | %d clusters\n",
              res$b_cropland, res$se_cropland, res$p_cropland, res$n_clusters))

  results[[cfg$label]] <- res

  rm(oos_raw, true_and_predictors_agg, panel_diffs, ag_panel, all_panel,
     top5, top10, bot90, reg_data, reg)
  gc()
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------------------------------------------------------------------

save(results, file = file.path(output_dir, "agriculture_changes_results.RData"))

write.csv(
  bind_rows(lapply(results, function(x) {
    data.frame(
      resolution = x$label,
      n_top5 = x$n_top5, n_top10 = x$n_top10, n_bot90 = x$n_bot90, n_all = x$n_all,
      mean_crop_top5 = x$mean_crop_top5, mean_crop_top10 = x$mean_crop_top10,
      mean_crop_bot90 = x$mean_crop_bot90,
      r2_top5_ov = x$r2_top5_ov, r2_top10_ov = x$r2_top10_ov,
      r2_bot90_ov = x$r2_bot90_ov, r2_all_ov = x$r2_all_ov,
      b_cropland = x$b_cropland, se_cropland = x$se_cropland, p_cropland = x$p_cropland,
      b_pop = x$b_pop, se_pop = x$se_pop, p_pop = x$p_pop,
      adj_r2 = x$adj_r2, n_reg = x$n_reg, n_clusters = x$n_clusters,
      stringsAsFactors = FALSE
    )
  })),
  file = file.path(output_dir, "agriculture_changes_summary.csv"),
  row.names = FALSE
)

cat("\nAgriculture robustness check (changes) complete.\n")
