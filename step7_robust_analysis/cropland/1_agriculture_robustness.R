# --------------------------------- Task Summary --------------------------------- #
# This file evaluates whether the model under-predicts in cropland-intensive areas by regressing the cell-level log prediction error on the within-ISO percentile rank of cropland proportion at all three grid resolutions (Online Appendix Section 9.2).
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

overall_r2_levl <- function(true_values, predicted_values) {
  valid <- true_values > 0 & predicted_values > 0
  if (sum(valid) < 2) return(NA)
  true_log <- log(true_values[valid])
  predicted_log <- log(predicted_values[valid])
  1 - (sum((true_log - predicted_log)^2) / sum((true_log - mean(true_log))^2))
}

fmt4 <- function(x) {
  if (is.na(x)) return("---")
  formatC(x, format = "f", digits = 4)
}

fmt6 <- function(x) {
  if (is.na(x)) return("---")
  formatC(x, format = "f", digits = 6)
}

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.1) return("$^{*}$")
  return("")
}

fmt_coef <- function(x, p, digits = 4) {
  paste0(formatC(x, format = "f", digits = digits), stars_fn(p))
}

fmt_n <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Degree-level configurations
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
# Main loop: process each degree level
# ---------------------------------------------------------------------------------------------------------------------------------------

results <- list()

for (cfg in deg_configs) {

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

  # --- 3. Load land cover data and compute cropland proportion ---
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
    # Aggregate to cell level (sum across country portions for border cells)
    group_by(across(all_of(c(cfg$id_cols, "year")))) %>%
    summarise(cropland_full = sum(cropland_full),
              land_area_km2 = sum(land_area_km2),
              .groups = "drop") %>%
    mutate(cropland_proportion = ifelse(land_area_km2 > 0, cropland_full / land_area_km2, 0)) %>%
    dplyr::select(all_of(c(cfg$id_cols, "year", "cropland_proportion")))

  # Convert join keys to character to match OOS data
  lc_cropland$cell_id <- as.character(lc_cropland$cell_id)
  for (col in cfg$extra_char_cols) lc_cropland[[col]] <- as.character(lc_cropland[[col]])

  rm(lc_raw)

  # --- 4. Merge cropland proportion with OOS data ---
  true_and_predictors_agg <- true_and_predictors_agg %>%
    left_join(lc_cropland, by = c(cfg$id_cols, "year"))

  rm(lc_cropland)

  # --- 5. Compute cropland proportion groups ---
  cat("  Computing cropland proportion groups...\n")
  ag_data <- true_and_predictors_agg %>%
    filter(!is.na(cropland_proportion) & cropland_proportion > 0 & pop_total_share > 0) %>%
    group_by(iso_original) %>%
    mutate(
      pctile_within_iso = percent_rank(cropland_proportion),
      top_5pct = pctile_within_iso >= 0.95,
      top_10pct = pctile_within_iso >= 0.90
    ) %>%
    ungroup()

  # --- 6. Compute prediction errors from OOS CV predictions ---

  # All years -- ag-classified cells
  ag_all_years <- ag_data %>%
    filter(true_GCP > 0 & oos_predicted_GCP > 0) %>%
    mutate(log_error = log(oos_predicted_GCP) - log(true_GCP))

  # All in-sample cells (no cropland filter) for baseline
  all_insample <- true_and_predictors_agg %>%
    filter(true_GCP > 0 & oos_predicted_GCP > 0) %>%
    mutate(log_error = log(oos_predicted_GCP) - log(true_GCP))

  cat(sprintf("  Ag-classified cells: %d | All in-sample: %d\n",
              nrow(ag_all_years), nrow(all_insample)))
  cat(sprintf("  Top 5%%: %d | Top 10%%: %d\n",
              sum(ag_all_years$top_5pct), sum(ag_all_years$top_10pct)))

  # --- 7. R-squared by group ---
  top5  <- ag_all_years %>% filter(top_5pct)
  top10 <- ag_all_years %>% filter(top_10pct)
  bot90 <- ag_all_years %>% filter(!top_10pct)

  r2_top5  <- overall_r2_levl(top5$true_GCP,  top5$oos_predicted_GCP)
  r2_top10 <- overall_r2_levl(top10$true_GCP, top10$oos_predicted_GCP)
  r2_bot90 <- overall_r2_levl(bot90$true_GCP, bot90$oos_predicted_GCP)
  r2_all   <- overall_r2_levl(all_insample$true_GCP, all_insample$oos_predicted_GCP)

  cat(sprintf("  R2 -- Top 5%%: %.4f | Top 10%%: %.4f | Bottom 90%%: %.4f | All: %.4f\n",
              r2_top5, r2_top10, r2_bot90, r2_all))

  # --- 8. Regression with cluster-robust SEs (clustered at cell level) ---
  # Cropland proportion is (near-)cell-invariant across 2012-2022 and the same
  # cell contributes up to 11 observations, so plain OLS SEs would understate
  # uncertainty. We cluster on the composite cell identifier.
  reg_data <- ag_all_years %>%
    filter(is.finite(cropland_proportion) & cropland_proportion > 0 &
           is.finite(log_error) & is.finite(pop_total_share) & pop_total_share > 0)
  reg_data$iso_fe <- factor(reg_data$iso_original)
  reg_data$cluster_id <- do.call(paste, c(reg_data[cfg$id_cols], sep = "_"))

  reg <- lm(log_error ~ cropland_proportion + pop_total_share + iso_fe, data = reg_data)
  stopifnot(nrow(reg$model) == nrow(reg_data))
  s <- summary(reg)

  vcov_cl <- sandwich::vcovCL(reg, cluster = reg_data$cluster_id, type = "HC1")
  ct <- lmtest::coeftest(reg, vcov. = vcov_cl)

  res <- list(
    label = cfg$label, deg_label = cfg$deg_label,
    n_top5 = nrow(top5), n_top10 = nrow(top10), n_bot90 = nrow(bot90), n_all = nrow(all_insample),
    r2_top5 = r2_top5, r2_top10 = r2_top10, r2_bot90 = r2_bot90, r2_all = r2_all,
    b_cropland = unname(ct["cropland_proportion", "Estimate"]),
    se_cropland = unname(ct["cropland_proportion", "Std. Error"]),
    p_cropland = unname(ct["cropland_proportion", "Pr(>|t|)"]),
    b_pop = unname(ct["pop_total_share", "Estimate"]),
    se_pop = unname(ct["pop_total_share", "Std. Error"]),
    p_pop = unname(ct["pop_total_share", "Pr(>|t|)"]),
    adj_r2 = s$adj.r.squared,
    n_reg = nrow(reg$model),
    n_clusters = length(unique(reg_data$cluster_id))
  )

  cat(sprintf("  Reg: beta = %.4f (cluster-robust se = %.4f, p = %.4f) | %d clusters\n",
              res$b_cropland, res$se_cropland, res$p_cropland, res$n_clusters))

  results[[cfg$label]] <- res

  # --- Clean up ---
  rm(oos_raw, true_and_predictors_agg, ag_data,
     ag_all_years, all_insample, top5, top10, bot90,
     reg_data, reg)
  gc()
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------------------------------------------------------------------

save(results, file = file.path(output_dir, "agriculture_results.RData"))

write.csv(
  bind_rows(lapply(results, function(x) {
    data.frame(resolution = x$label,
               n_top5 = x$n_top5, n_top10 = x$n_top10, n_bot90 = x$n_bot90, n_all = x$n_all,
               r2_top5 = x$r2_top5, r2_top10 = x$r2_top10, r2_bot90 = x$r2_bot90, r2_all = x$r2_all,
               b_cropland = x$b_cropland, se_cropland = x$se_cropland, p_cropland = x$p_cropland,
               b_pop = x$b_pop, se_pop = x$se_pop, p_pop = x$p_pop,
               adj_r2 = x$adj_r2, n_reg = x$n_reg, n_clusters = x$n_clusters,
               stringsAsFactors = FALSE)
  })),
  file = file.path(output_dir, "agriculture_summary.csv"),
  row.names = FALSE
)

cat("\nAgriculture robustness check complete.\n")
