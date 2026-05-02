# --------------------------------- Task Summary --------------------------------- #
# This file produces kernel density plots of the cross-cell distribution of the SD of log GDP across trees and of the out-of-sample log prediction error in 2019, overlaid for all three grid resolutions (Online Appendix Sections 10 and 11).
# -------------------------------------------------------------------------------- #

Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(sf)
library(scales)

output_dir <- "step5_predict_and_post_adjustments/outputs"

# ---------------------------------------------------------------------------------------------------------------------------------------
# Resolution configurations
# ---------------------------------------------------------------------------------------------------------------------------------------

configs <- list(
  list(
    label = "1\u00B0", label_file = "1deg",
    pred_file = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_1deg_postadjust_pop_dens_no_extra_adjust.RData"),
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_1deg.csv",
    oos_gdp_col = "GCP_1deg",
    id_cols = "cell_id",
    extra_char_cols = character(0)
  ),
  list(
    label = "0.5\u00B0", label_file = "0_5deg",
    pred_file = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.RData"),
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_5deg.csv",
    oos_gdp_col = "GCP_0_5deg",
    id_cols = c("cell_id", "subcell_id"),
    extra_char_cols = "subcell_id"
  ),
  list(
    label = "0.25\u00B0", label_file = "0_25deg",
    pred_file = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.RData"),
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_25deg.csv",
    oos_gdp_col = "GCP_0_25deg",
    id_cols = c("cell_id", "subcell_id", "subcell_id_0_25"),
    extra_char_cols = c("subcell_id", "subcell_id_0_25")
  )
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Extract data for each resolution
# ---------------------------------------------------------------------------------------------------------------------------------------

sd_data_list <- list()
error_data_list <- list()

for (cfg in configs) {

  # --- Load prediction file ---
  .env <- new.env()
  .loaded <- load(cfg$pred_file, envir = .env)
  df <- .env[[.loaded[1]]]
  rm(.env, .loaded)

  # --- SD of log GDP (all cells with positive predicted GDP, 2019) ---
  df_2019 <- df %>%
    st_drop_geometry() %>%
    filter(year == 2019) %>%
    mutate(cell_id = as.character(cell_id))
  for (col in cfg$extra_char_cols) df_2019[[col]] <- as.character(df_2019[[col]])

  sd_vals <- df_2019 %>%
    filter(predicted_GCP_const_2021_USD > 0, !is.na(GCP_sd_log_gdp)) %>%
    pull(GCP_sd_log_gdp)

  sd_data_list[[cfg$label_file]] <- data.frame(
    resolution = cfg$label,
    sd_log_gdp = sd_vals,
    stringsAsFactors = FALSE
  )
  cat(sprintf("    SD cells: %d (median: %.3f, mean: %.3f)\n",
              length(sd_vals), median(sd_vals), mean(sd_vals)))

  # --- Log prediction error (in-sample cells, 2019) ---
  oos_raw <- read.csv(cfg$oos_file)
  oos_raw$cell_id <- as.character(oos_raw$cell_id)
  for (col in cfg$extra_char_cols) oos_raw[[col]] <- as.character(oos_raw[[col]])

  # Aggregate USA states, filter to 2019
  oos_gdp <- oos_raw %>%
    filter(year == 2019) %>%
    mutate(iso_country = ifelse(substr(iso, 1, 4) == "USA_", "USA", iso)) %>%
    group_by(across(all_of(c("iso_country", cfg$id_cols)))) %>%
    summarise(
      oos_predicted_GCP = sum(oos_predicted_GCP),
      true_GCP = sum(.data[[cfg$oos_gdp_col]]),
      .groups = "drop"
    ) %>%
    rename(iso = iso_country)

  # Join with prediction file (to match exactly the map pipeline)
  df_err <- df_2019 %>%
    left_join(oos_gdp, by = c(cfg$id_cols, "iso")) %>%
    mutate(
      log_pred_error = ifelse(
        !is.na(true_GCP) & true_GCP > 0 & oos_predicted_GCP > 0,
        log(oos_predicted_GCP) - log(true_GCP),
        NA_real_)
    ) %>%
    filter(!is.na(log_pred_error))

  error_vals <- df_err$log_pred_error

  error_data_list[[cfg$label_file]] <- data.frame(
    resolution = cfg$label,
    log_pred_error = error_vals,
    stringsAsFactors = FALSE
  )
  cat(sprintf("    Error cells: %d (median: %.4f, mean: %.4f, sd: %.4f)\n",
              length(error_vals), median(error_vals), mean(error_vals), sd(error_vals)))

  rm(df, df_2019, oos_raw, oos_gdp, df_err, sd_vals, error_vals)
  gc()
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Shared plot theme (publication-ready, no titles)
# ---------------------------------------------------------------------------------------------------------------------------------------

plot_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, color = "gray20"),
    axis.line = element_line(linewidth = 0.3, color = "gray40"),
    axis.ticks = element_line(linewidth = 0.3, color = "gray40"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.margin = margin(t = 8, r = 12, b = 8, l = 8)
  )

fill_color <- "#B8D4E8"
line_color <- "#2166AC"

fig_w <- 5.5
fig_h <- 3.5

# ---------------------------------------------------------------------------------------------------------------------------------------
# Generate separate plots for each resolution
# ---------------------------------------------------------------------------------------------------------------------------------------

for (cfg in configs) {

  # --- SD of log(GDP) ---
  sd_df <- sd_data_list[[cfg$label_file]]

  p_sd <- ggplot(sd_df, aes(x = sd_log_gdp)) +
    geom_density(fill = fill_color, color = line_color,
                 linewidth = 0.5, alpha = 0.45, adjust = 1) +
    scale_x_continuous(breaks = seq(0, 0.8, by = 0.1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(xlim = c(0, 0.8)) +
    labs(x = "SD of log(GDP)", y = "Density") +
    plot_theme

  ggsave(file.path(output_dir, paste0("dist_sd_log_gdp_", cfg$label_file, "_2019.png")),
         plot = p_sd, bg = "white", width = fig_w, height = fig_h, dpi = 300)
  cat(paste0("    Saved: dist_sd_log_gdp_", cfg$label_file, "_2019.png\n"))

  # --- Log prediction error ---
  err_df <- error_data_list[[cfg$label_file]]

  p_err <- ggplot(err_df, aes(x = log_pred_error)) +
    geom_density(fill = fill_color, color = line_color,
                 linewidth = 0.5, alpha = 0.45, adjust = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.35) +
    scale_x_continuous(breaks = seq(-3, 3, by = 1),
                       expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(xlim = c(-3, 3)) +
    labs(x = expression(log(widehat(GDP)) - log(GDP^true)), y = "Density") +
    plot_theme

  ggsave(file.path(output_dir, paste0("dist_log_pred_error_", cfg$label_file, "_2019.png")),
         plot = p_err, bg = "white", width = fig_w, height = fig_h, dpi = 300)
  cat(paste0("    Saved: dist_log_pred_error_", cfg$label_file, "_2019.png\n"))
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Combined-resolution overlay plots (for the appendix)
#
# Design notes:
#   - Thin lines (0.55 pt) with subtle fills (alpha 0.10) so three distributions
#     share visual weight without producing a muddy alpha-stacked smear in the
#     overlap region. Fill alpha is deliberately lower than 1 / n_resolutions so
#     that even full overlap stays visibly translucent.
#   - Okabe-Ito-adjacent palette: distinguishable on print and colorblind-friendly.
#   - Legend: line-only keys via key_glyph = "path" (overrides the default
#     polygon key that geom_density would otherwise produce), inside the plot,
#     no background.
#   - Pure black axes and ticks, small base font, minimal margins.
# ---------------------------------------------------------------------------------------------------------------------------------------

resolution_levels <- c("1\u00B0", "0.5\u00B0", "0.25\u00B0")

resolution_colors <- c(
  "1\u00B0"    = "#2166AC",
  "0.5\u00B0"  = "#D55E00",
  "0.25\u00B0" = "#009E73"
)

combined_theme <- theme_classic(base_size = 10) +
  theme(
    axis.title       = element_text(size = 10, color = "black"),
    axis.text        = element_text(size = 9,  color = "black"),
    axis.line        = element_line(linewidth = 0.35, color = "black"),
    axis.ticks       = element_line(linewidth = 0.35, color = "black"),
    axis.ticks.length = unit(0.12, "cm"),
    plot.margin      = margin(t = 8, r = 14, b = 8, l = 8),
    legend.position        = "inside",
    legend.position.inside = c(0.88, 0.80),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.title      = element_text(size = 9, color = "black", face = "plain"),
    legend.text       = element_text(size = 9, color = "black"),
    legend.key.height = unit(0.32, "cm"),
    legend.key.width  = unit(0.55, "cm"),
    legend.margin     = margin(0, 0, 0, 0),
    legend.spacing.y  = unit(0.05, "cm")
  )

# --- SD of log(GDP): combined ---
sd_all <- bind_rows(sd_data_list) %>%
  mutate(resolution = factor(resolution, levels = resolution_levels))

p_sd_combined <- ggplot(sd_all, aes(x = sd_log_gdp,
                                    color = resolution, fill = resolution)) +
  geom_density(linewidth = 0.55, alpha = 0.10, adjust = 1,
               key_glyph = "path") +
  scale_color_manual(values = resolution_colors, name = "Resolution") +
  scale_fill_manual(values = resolution_colors, name = "Resolution") +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, 0.8)) +
  labs(x = "SD of log(GDP)", y = "Density") +
  combined_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1)))

ggsave(file.path(output_dir, "dist_sd_log_gdp_combined_2019.png"),
       plot = p_sd_combined, bg = "white",
       width = fig_w, height = fig_h, dpi = 300)
cat("  Saved: dist_sd_log_gdp_combined_2019.png\n")

# --- Log prediction error: combined ---
err_all <- bind_rows(error_data_list) %>%
  mutate(resolution = factor(resolution, levels = resolution_levels))

p_err_combined <- ggplot(err_all, aes(x = log_pred_error,
                                      color = resolution, fill = resolution)) +
  geom_density(linewidth = 0.55, alpha = 0.10, adjust = 1,
               key_glyph = "path") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.3) +
  scale_color_manual(values = resolution_colors, name = "Resolution") +
  scale_fill_manual(values = resolution_colors, name = "Resolution") +
  scale_x_continuous(breaks = seq(-3, 3, by = 1),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(-3, 3)) +
  labs(x = expression(log(widehat(GDP)) - log(GDP^true)), y = "Density") +
  combined_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1)))

ggsave(file.path(output_dir, "dist_log_pred_error_combined_2019.png"),
       plot = p_err_combined, bg = "white",
       width = fig_w, height = fig_h, dpi = 300)
cat("  Saved: dist_log_pred_error_combined_2019.png\n")
