# --------------------------------- Task Summary --------------------------------- #
# This file produces world maps of cell-level log GDP and log GDP per capita in 2019 and their log changes from 2018 to 2019, at the 1-degree and 0.25-degree resolutions, exported as four standalone panels per resolution at 300 dpi and 900 dpi (paper Figure 1). 
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tictoc)
library(gdata)
library(sf)
library(parallel)
library(tidyverse)
library(fs)
library(dplyr)
library(data.table)
library(ranger)
library(tmaptools)
library(scales)
library(workflows)
library(exactextractr)
library(terra)
library(raster)
library(colorRamps)
library(RColorBrewer)
library(gridExtra)
library(grid)

# ------------------------------------------------------------------------------------------------- #
# Configuration
# ------------------------------------------------------------------------------------------------- #
output_base <- "step5_predict_and_post_adjustments/outputs"
resolutions <- c("1deg", "0_25deg")
dpis <- c(300, 900)

# Censoring limits for the year-over-year log change panels
change_GCP_limits  <- c(-0.5,  0.5)
change_GDPC_limits <- c(-0.25, 0.25)

# Base panel theme: bottom legend, tight margins. Standalone theme overrides
# legend text/title sizes and colorbar dimensions for the half-page render.
base_panel_theme <- theme(
  legend.title    = element_text(size = 12, vjust = 0.9, hjust = 0.5),
  legend.text     = element_text(size = 5),
  legend.position = "bottom",
  legend.key.size = unit(0.4, "cm"),
  plot.margin     = unit(c(0, 0, 0, 0), "cm"),
  panel.spacing   = unit(0, "null")
)

standalone_theme <- theme(
  plot.subtitle     = element_blank(),
  legend.title      = element_text(size = 14, vjust = 0.9, hjust = 0.5),
  legend.text       = element_text(size = 12),
  legend.key.width  = unit(1.875, "cm"),
  legend.key.height = unit(0.35, "cm")
)

standalone_guide <- function(title) {
  guides(fill = guide_colorbar(title          = title,
                               title.position = "left",
                               title.vjust    = 0.85))
}

# ------------------------------------------------------------------------------------------------- #
# Helper: build the four standalone panels for a given resolution
# ------------------------------------------------------------------------------------------------- #
build_plots <- function(res) {

  # Load data
  load(paste0("step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_",
              res, "_postadjust_pop_dens_no_extra_adjust.RData"))
  obj_name <- paste0("GDPC_", res, "_postadjust_pop_dens_no_extra_adjust")
  data <- get(obj_name)

  # Resolution-specific cell identifiers for joining 2019 to 2018
  id_cols <- if (res == "1deg") c("cell_id", "iso")
             else if (res == "0_5deg") c("cell_id", "subcell_id", "iso")
             else c("cell_id", "subcell_id", "subcell_id_0_25", "iso")

  # predicted_GCP and cell_GDPC are stored in billions of constant 2021 USD.
  # Display GDP in USD billions (no rescaling), GDP per capita in USD (billions x 1e9).
  df <- data %>%
    mutate(
      predicted_GCP = ifelse(predicted_GCP == 0, NA, predicted_GCP),
      cell_GDPC     = ifelse(cell_GDPC == 0,     NA, cell_GDPC * 1e9)
    )

  my_palette <- colorRampPalette(brewer.pal(11, "Spectral"))(34)

  # ---- p1: log(GDP in 2019), USD millions ----
  gradient_breaks <- df %>%
    filter(year == 2019) %>%
    pull(predicted_GCP) %>%
    na.omit() %>%
    log() %>%
    quantile(probs = seq(0.005, 0.995, by = 0.03))

  p1 <- ggplot(data = df %>% filter(year == 2019) %>% st_as_sf()) +
    geom_sf(aes(fill = log(predicted_GCP)), color = NA) +
    scale_fill_gradientn(colors    = my_palette,
                         values    = scales::rescale(gradient_breaks),
                         limits    = c(gradient_breaks[1], gradient_breaks[length(gradient_breaks)]),
                         breaks    = c(-14, -12, -10, -8, -6, -4, -2, 0, 2, 4),
                         oob       = scales::squish,
                         na.value  = "lightgrey",
                         name      = "log(GDP)") +
    theme_void() + base_panel_theme

  # ---- p2: log(GDP per capita in 2019), USD ----
  gradient_breaks <- df %>%
    filter(year == 2019) %>%
    pull(cell_GDPC) %>%
    na.omit() %>%
    log() %>%
    quantile(probs = seq(0.005, 0.995, by = 0.03))

  p2 <- ggplot(data = df %>% filter(year == 2019) %>% st_as_sf()) +
    geom_sf(aes(fill = log(cell_GDPC)), color = NA) +
    scale_fill_gradientn(colors    = my_palette,
                         values    = scales::rescale(gradient_breaks),
                         limits    = c(gradient_breaks[1], gradient_breaks[length(gradient_breaks)]),
                         breaks    = pretty(c(gradient_breaks[1], gradient_breaks[length(gradient_breaks)]), n = 7),
                         oob       = scales::squish,
                         na.value  = "lightgrey",
                         name      = "log(GDP p.c.)") +
    theme_void() + base_panel_theme

  # ---- 2018->2019 panel: drop geometry on year1, keep on year2, re-attach via left_join ----
  data_1 <- df %>% filter(year == 2018) %>%
    st_drop_geometry() %>%
    dplyr::select(all_of(id_cols), predicted_GCP, cell_GDPC)
  data_2 <- df %>% filter(year == 2019) %>%
    dplyr::select(all_of(id_cols), predicted_GCP, cell_GDPC, geom)

  df_2018_2019 <- data_1 %>%
    left_join(data_2, by = id_cols, suffix = c("_1", "_2")) %>%
    mutate(change_GCP  = log(predicted_GCP_2) - log(predicted_GCP_1),
           change_GDPC = log(cell_GDPC_2)     - log(cell_GDPC_1)) %>%
    st_as_sf()

  # ---- p3: log change in GDP, censored to [-0.5, 0.5] ----
  p3 <- ggplot(data = df_2018_2019) +
    geom_sf(aes(fill = change_GCP), color = NA) +
    scale_fill_gradientn(
      colours  = my_palette,
      limits   = change_GCP_limits,
      oob      = scales::squish,
      na.value = "lightgrey",
      name     = "log difference"
    ) +
    theme_void() + base_panel_theme

  # ---- p4: log change in GDP per capita, censored to [-0.25, 0.25] ----
  p4 <- ggplot(data = df_2018_2019) +
    geom_sf(aes(fill = change_GDPC), color = NA) +
    scale_fill_gradientn(
      colours  = my_palette,
      limits   = change_GDPC_limits,
      oob      = scales::squish,
      na.value = "lightgrey",
      name     = "log difference"
    ) +
    theme_void() + base_panel_theme

  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
}

# ------------------------------------------------------------------------------------------------- #
# Helper: save the four standalone panels into a target directory at a given dpi
# ------------------------------------------------------------------------------------------------- #
save_plots <- function(plots, res, dpi, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(out_dir, paste0("world_logGDP_2019_", res, ".png")),
         plot = plots$p1 + standalone_theme + standalone_guide("log(GDP in 2019, USD billions)"),
         bg = "white", width = 10, height = 5, dpi = dpi)
  ggsave(file.path(out_dir, paste0("world_logGDPC_2019_", res, ".png")),
         plot = plots$p2 + standalone_theme + standalone_guide("log(GDP per capita in 2019, USD)"),
         bg = "white", width = 10, height = 5, dpi = dpi)
  ggsave(file.path(out_dir, paste0("world_logGDP_change_2018_2019_", res, ".png")),
         plot = plots$p3 + standalone_theme + standalone_guide("log(GDP in 2019) - log(GDP in 2018)"),
         bg = "white", width = 10, height = 5, dpi = dpi)
  ggsave(file.path(out_dir, paste0("world_logGDPC_change_2018_2019_", res, ".png")),
         plot = plots$p4 + standalone_theme + standalone_guide("log(GDP p.c. in 2019) - log(GDP p.c. in 2018)"),
         bg = "white", width = 10, height = 5, dpi = dpi)
  cat(paste0("    Saved 4 PNGs at ", dpi, " dpi to ", out_dir, "\n"))
}

# ------------------------------------------------------------------------------------------------- #
# Main: 16 PNGs (4 panels x 2 resolutions x 2 dpi) into 4 subfolders of step5_predict_and_post_adjustments/outputs.
# ------------------------------------------------------------------------------------------------- #
for (res in resolutions) {
  plots <- build_plots(res)
  for (dpi in dpis) {
    out_dir <- file.path(output_base, paste0("example_plots_", res, "_", dpi, "dpi"))
    save_plots(plots, res, dpi, out_dir)
  }
}
