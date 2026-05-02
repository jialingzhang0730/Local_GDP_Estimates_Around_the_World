# --------------------------------- Task Summary --------------------------------- #
# This file evaluates whether emissions-based predictors mechanically inflate predicted output along major transport corridors by regressing the cell-level log prediction error on the within-ISO percentile rank of corridor intensity at all three grid resolutions (Online Appendix Section 9.4).
# -------------------------------------------------------------------------------- #

Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(dplyr)
library(sandwich)
library(lmtest)

output_dir <- "step7_robust_analysis/transport_corridors/outputs"
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
    extra_char_cols = character(0)
  ),
  list(
    label = "0_5deg", deg_label = "0.5-degree Model", cell_step = 0.5,
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_5deg.csv",
    oos_gdp_col = "GCP_0_5deg",
    id_cols = c("cell_id", "subcell_id"),
    extra_char_cols = "subcell_id"
  ),
  list(
    label = "0_25deg", deg_label = "0.25-degree Model", cell_step = 0.25,
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_25deg.csv",
    oos_gdp_col = "GCP_0_25deg",
    id_cols = c("cell_id", "subcell_id", "subcell_id_0_25"),
    extra_char_cols = c("subcell_id", "subcell_id_0_25")
  )
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Main loop: process each degree level
# ---------------------------------------------------------------------------------------------------------------------------------------

results <- list()

for (cfg in deg_configs) {

  # --- 1. Load OOS cross-validation predictions ---
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
      CO2_non_org_tspt_share = max(CO2_non_org_tspt_share),
      CO2_bio_tspt_share = max(CO2_bio_tspt_share),
      pop_total_share = max(pop_total_share),
      iso_original = first(iso),
      .groups = "drop"
    ) %>%
    rename(iso = iso_for_join)

  # --- 3. Compute corridor intensity ---
  corridor_data <- true_and_predictors_agg %>%
    filter(pop_total_share > 0 & CO2_non_org_tspt_share > 0) %>%
    mutate(
      corridor_intensity = CO2_non_org_tspt_share / pop_total_share,
      total_tspt_share = CO2_non_org_tspt_share + CO2_bio_tspt_share,
      corridor_intensity_total = total_tspt_share / pop_total_share
    )

  corridor_data <- corridor_data %>%
    group_by(iso_original) %>%
    mutate(
      pctile_within_iso = percent_rank(corridor_intensity),
      top_5pct = pctile_within_iso >= 0.95,
      top_10pct = pctile_within_iso >= 0.90
    ) %>%
    ungroup()

  # --- 4. Compute prediction errors from OOS CV predictions ---

  # All years — corridor-classified cells
  corridor_all_years <- corridor_data %>%
    filter(true_GCP > 0 & oos_predicted_GCP > 0) %>%
    mutate(log_error = log(oos_predicted_GCP) - log(true_GCP))

  # All in-sample cells (no CO2/pop filter) for baseline
  all_insample <- true_and_predictors_agg %>%
    filter(true_GCP > 0 & oos_predicted_GCP > 0) %>%
    mutate(log_error = log(oos_predicted_GCP) - log(true_GCP))

  cat(sprintf("  Corridor-classified cells: %d | All in-sample: %d\n",
              nrow(corridor_all_years), nrow(all_insample)))
  cat(sprintf("  Top 5%%: %d | Top 10%%: %d\n",
              sum(corridor_all_years$top_5pct), sum(corridor_all_years$top_10pct)))

  # --- 5. R-squared by group ---
  top5  <- corridor_all_years %>% filter(top_5pct)
  top10 <- corridor_all_years %>% filter(top_10pct)
  bot90 <- corridor_all_years %>% filter(!top_10pct)

  r2_top5  <- overall_r2_levl(top5$true_GCP,  top5$oos_predicted_GCP)
  r2_top10 <- overall_r2_levl(top10$true_GCP, top10$oos_predicted_GCP)
  r2_bot90 <- overall_r2_levl(bot90$true_GCP, bot90$oos_predicted_GCP)
  r2_all   <- overall_r2_levl(all_insample$true_GCP, all_insample$oos_predicted_GCP)

  cat(sprintf("  R2 -- Top 5%%: %.4f | Top 10%%: %.4f | Bottom 90%%: %.4f | All: %.4f\n",
              r2_top5, r2_top10, r2_bot90, r2_all))

  # --- 6. Regression with cluster-robust SEs (clustered at cell level) ---
  # Same cell enters up to 11 times (2012-2022). Residuals are almost certainly
  # serially correlated within cell, so plain OLS SEs understate uncertainty.
  # We cluster on the composite cell identifier (cell_id + any subcell cols).
  reg_data <- corridor_all_years %>%
    filter(is.finite(corridor_intensity) & corridor_intensity > 0 &
           is.finite(log_error) & is.finite(pop_total_share) & pop_total_share > 0)
  reg_data$iso_fe <- factor(reg_data$iso_original)
  reg_data$cluster_id <- do.call(paste, c(reg_data[cfg$id_cols], sep = "_"))

  reg <- lm(log_error ~ corridor_intensity + pop_total_share + iso_fe, data = reg_data)
  stopifnot(nrow(reg$model) == nrow(reg_data))
  s <- summary(reg)

  vcov_cl <- sandwich::vcovCL(reg, cluster = reg_data$cluster_id, type = "HC1")
  ct <- lmtest::coeftest(reg, vcov. = vcov_cl)

  res <- list(
    label = cfg$label, deg_label = cfg$deg_label,
    n_top5 = nrow(top5), n_top10 = nrow(top10), n_bot90 = nrow(bot90), n_all = nrow(all_insample),
    r2_top5 = r2_top5, r2_top10 = r2_top10, r2_bot90 = r2_bot90, r2_all = r2_all,
    b_intensity = unname(ct["corridor_intensity", "Estimate"]),
    se_intensity = unname(ct["corridor_intensity", "Std. Error"]),
    p_intensity = unname(ct["corridor_intensity", "Pr(>|t|)"]),
    b_pop = unname(ct["pop_total_share", "Estimate"]),
    se_pop = unname(ct["pop_total_share", "Std. Error"]),
    p_pop = unname(ct["pop_total_share", "Pr(>|t|)"]),
    adj_r2 = s$adj.r.squared,
    n_reg = nrow(reg$model),
    n_clusters = length(unique(reg_data$cluster_id))
  )

  cat(sprintf("  Reg: beta = %.6f (cluster-robust se = %.6f, p = %.4f) | %d clusters\n",
              res$b_intensity, res$se_intensity, res$p_intensity, res$n_clusters))

  results[[cfg$label]] <- res

  # --- Clean up ---
  rm(oos_raw, true_and_predictors_agg, corridor_data,
     corridor_all_years, all_insample, top5, top10, bot90,
     reg_data, reg)
  gc()
}

# Save results
save(results, file = file.path(output_dir, "transport_corridor_results.RData"))

write.csv(
  bind_rows(lapply(results, function(x) {
    data.frame(resolution = x$label,
               n_top5 = x$n_top5, n_top10 = x$n_top10, n_bot90 = x$n_bot90, n_all = x$n_all,
               r2_top5 = x$r2_top5, r2_top10 = x$r2_top10, r2_bot90 = x$r2_bot90, r2_all = x$r2_all,
               b_intensity = x$b_intensity, se_intensity = x$se_intensity, p_intensity = x$p_intensity,
               b_pop = x$b_pop, se_pop = x$se_pop, p_pop = x$p_pop,
               adj_r2 = x$adj_r2, n_reg = x$n_reg, n_clusters = x$n_clusters,
               stringsAsFactors = FALSE)
  })),
  file = file.path(output_dir, "transport_corridor_summary.csv"),
  row.names = FALSE
)

