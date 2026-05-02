# --------------------------------- Task Summary --------------------------------- #
# This file produces the two world maps in Sections 10 and 11 of the Online Appendix at all three grid resolutions: cell-level standard deviation of log GDP across random forest trees, and out-of-sample log prediction error in 2019.
# -------------------------------------------------------------------------------- #

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tidyverse)
library(sf)
library(dplyr)
library(scales)
library(RColorBrewer)
library(readxl)

output_dir <- "step5_predict_and_post_adjustments/outputs"

# ---------------------------------------------------------------------------------------------------------------------------------------
# In-sample countries (from Appendix Table 2, excluding China)
in_sample_isos <- c(
  "AUT", "BEL", "BGR", "CHE", "CZE", "DEU", "DNK", "ESP", "EST", "FIN",
  "FRA", "GBR", "GRC", "HRV", "HUN", "ITA", "JPN", "KOR", "LTU", "LVA",
  "NLD", "NOR", "NZL", "POL", "PRT", "ROU", "SWE", "SVK", "SVN", "TUR", "USA",
  "ALB", "BIH", "BLR", "CHL", "COL", "ECU", "IDN", "KEN", "KGZ", "LKA",
  "MOZ", "PER", "PHL", "SRB", "THA", "UZB", "VNM"
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Resolution configurations: each entry specifies the input dataset, OOS predictions
# file, the cell-level identifier columns used to join OOS predictions with the
# prediction dataset, the name of the OOS true-GCP column, and the output map
# directory.
res_configs <- list(
  list(
    key            = "1deg",
    label          = "1-degree",
    pred_rdata     = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_1deg_postadjust_pop_dens_no_extra_adjust.RData"),
    oos_csv        = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_1deg.csv",
    id_cols        = c("cell_id"),
    oos_true_col   = "GCP_1deg",
    map_dir        = file.path(output_dir, "uncertainty_maps_1deg")
  ),
  list(
    key            = "0_5deg",
    label          = "0.5-degree",
    pred_rdata     = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.RData"),
    oos_csv        = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_5deg.csv",
    id_cols        = c("cell_id", "subcell_id"),
    oos_true_col   = "GCP_0_5deg",
    map_dir        = file.path(output_dir, "uncertainty_maps_0_5deg")
  ),
  list(
    key            = "0_25deg",
    label          = "0.25-degree",
    pred_rdata     = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.RData"),
    oos_csv        = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_25deg.csv",
    id_cols        = c("cell_id", "subcell_id", "subcell_id_0_25"),
    oos_true_col   = "GCP_0_25deg",
    map_dir        = file.path(output_dir, "uncertainty_maps_0_25deg")
  )
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Shared plotting style

my_palette <- colorRampPalette(brewer.pal(11, 'Spectral'))(34)

map_theme <- theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 2)),
    plot.caption = element_text(size = 9, hjust = 0.5, face = "italic", color = "gray30", margin = margin(t = 4)),
    legend.title = element_text(size = 11, vjust = 0.9, hjust = 0.5),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.3, "cm"),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    panel.spacing = unit(0, "null")
  )

# ---------------------------------------------------------------------------------------------------------------------------------------
# Per-resolution map generation

generate_maps_for_resolution <- function(rc) {

  dir.create(rc$map_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Load post-adjusted prediction dataset (with geometry) ---
  .env <- new.env()
  .loaded <- load(rc$pred_rdata, envir = .env)
  df <- .env[[.loaded[1]]]
  rm(.env, .loaded)

  # Filter to 2019 and define SD(log GDP) using the exact tree-level statistic
  # computed in step5_predict_and_post_adjustments/4_post_adjustment_filter_out_low_pop_density_*deg.R
  # via rowSds(log(tree_GCP_cell), na.rm = TRUE).
  df_2019 <- df %>%
    filter(year == 2019) %>%
    mutate(
      predicted_GCP_const_2021_USD = ifelse(predicted_GCP_const_2021_USD == 0, NA, predicted_GCP_const_2021_USD),
      sd_log_gdp = ifelse(GCP_sd_log_gdp > 0, GCP_sd_log_gdp, NA_real_)
    )

  rm(df); gc()

  # =======================================================================================================
  # Map A: SD of log(GDP) — proportional uncertainty
  # =======================================================================================================

  plot_sf <- df_2019 %>% st_as_sf()
  p_sd <- ggplot(data = plot_sf) +
    geom_sf(aes(fill = sd_log_gdp), color = NA) +
    scale_fill_gradientn(
      colors = rev(my_palette),  # reversed: blue=low (precise), red=high (uncertain)
      limits = c(0, 0.5),
      breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.5),
      labels = c("0", "0.05", "0.10", "0.20", "0.30", "0.50"),
      oob = squish,
      na.value = "lightgrey",
      name = "SD(log GDP)"
    ) +
    labs(title = "Proportional Uncertainty in GDP Estimates, 2019",
         subtitle = expression(SD[trees](log(widehat(GDP)[cell]))),
         caption = "Higher values (red) indicate greater proportional disagreement across trees.") +
    map_theme

  ggsave(file.path(rc$map_dir, "map_sd_log_gdp_2019.png"), plot = p_sd, bg = "white", width = 8.8, height = 5)
  cat(sprintf("  Saved: %s/map_sd_log_gdp_2019.png\n", basename(rc$map_dir)))
  rm(plot_sf, p_sd); gc()

  # =======================================================================================================
  # Map B: Out-of-sample log prediction error
  # =======================================================================================================

  # Load OOS cross-validation predictions and aggregate USA states up to USA
  oos_raw <- read.csv(rc$oos_csv)
  for (col in rc$id_cols) {
    oos_raw[[col]] <- as.character(oos_raw[[col]])
  }

  oos_gdp <- oos_raw %>%
    mutate(iso_country = ifelse(substr(iso, 1, 4) == "USA_", "USA", iso)) %>%
    group_by(across(all_of(c("iso_country", rc$id_cols, "year")))) %>%
    summarise(oos_predicted_GCP = sum(oos_predicted_GCP),
              true_GCP = sum(.data[[rc$oos_true_col]]), .groups = "drop") %>%
    rename(iso = iso_country)

  rm(oos_raw); gc()

  # Join OOS predictions onto the 2019 prediction dataset
  df_2019_err <- df_2019 %>%
    as.data.frame()
  for (col in rc$id_cols) {
    df_2019_err[[col]] <- as.character(df_2019_err[[col]])
  }

  df_2019_err <- df_2019_err %>%
    left_join(oos_gdp, by = c(rc$id_cols, "iso", "year")) %>%
    mutate(
      log_pred_error = ifelse(
        !is.na(true_GCP) & true_GCP > 0 & oos_predicted_GCP > 0,
        log(oos_predicted_GCP) - log(true_GCP),
        NA_real_)
    )

  plot_sf <- df_2019_err %>% st_as_sf()
  rm(oos_gdp, df_2019_err); gc()

  plot_sf$log_pred_error_cap <- pmin(pmax(plot_sf$log_pred_error, -2), 2)

  p_err <- ggplot(data = plot_sf) +
    geom_sf(aes(fill = log_pred_error_cap), color = NA) +
    scale_fill_distiller(
      type = "div", palette = "RdBu", direction = -1,
      limits = c(-2, 2),
      breaks = c(-2, -1, -0.5, 0, 0.5, 1, 2),
      oob = squish,
      na.value = "lightgrey",
      name = "log(pred/true)"
    ) +
    labs(title = "Log Prediction Error (In-Sample Countries), 2019",
         subtitle = expression(log(widehat(GDP)[cell]) - log(GDP[cell]^true)),
         caption = "Red = overestimate, blue = underestimate. Gray cells are out-of-sample countries.") +
    map_theme

  ggsave(file.path(rc$map_dir, "map_log_pred_error_2019.png"), plot = p_err, bg = "white", width = 8.8, height = 5)
  cat(sprintf("  Saved: %s/map_log_pred_error_2019.png\n", basename(rc$map_dir)))

  rm(plot_sf, p_err, df_2019); gc()
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Run all resolutions

for (rc in res_configs) {
  generate_maps_for_resolution(rc)
}
