# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to draw some example plots for 1deg. You can also do it for other degrees.
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tictoc)
library(gdata)
library(units)
library(sf)
library(parallel)
library(tidyverse)
library(fs)
library(dplyr)
library(data.table)
library(vip)
library(ranger)
library(tmaptools)
library(scales)
library(workflows)
library(data.table)
library(tmaptools)
library(plotly)
library(htmlwidgets)
library(exactextractr)
library(terra)
library(raster)
library(colorRamps)
library(RColorBrewer)
library(gridExtra)
library(grid)

# Set working directory
setwd("/share/rossihansberglab/Nightlights_GDP/replication_packages_world_GCP")

# plot
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_no_extra_adjust.RData")
df <- GDPC_1deg_postadjust_pop_dens_no_extra_adjust %>% 
     mutate(predicted_GCP = ifelse(predicted_GCP == 0, NA, predicted_GCP),
            cell_GDPC = ifelse(cell_GDPC == 0, NA, cell_GDPC))

my_palette <- colorRampPalette(brewer.pal(11, 'Spectral'))(34)

gradient_breaks <- df %>% 
  filter(year %in% c(2019)) %>% 
  pull(predicted_GCP) %>% 
  na.omit() %>% 
  log() %>% 
  quantile(probs = seq(0.005, 0.995, by = 0.03))

p1 <- ggplot(data = df  %>% filter(year == 2019)  %>% st_as_sf()) +
  geom_sf(aes(fill = log(predicted_GCP)), color = NA) +
  scale_fill_gradientn(colors = my_palette,
                       values = scales::rescale(gradient_breaks),
                       oob = scales::squish, 
                       na.value = "lightgrey", # Color for NA values
                       name = "log(GDP)") +
  labs(subtitle = "log(GDP in 2019)",
       fill = "Cell GDP") +   
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 12, vjust = 0.9, hjust = 0.5),  # Emphasize legend title
    legend.text = element_text(size = 5),  # Adjust size of legend labels
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),  # Adjust legend box size
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.spacing = unit(0, "null")
  )

gradient_breaks <- df %>% 
  filter(year %in% c(2019)) %>% 
  pull(cell_GDPC) %>% 
  na.omit() %>% 
  log() %>% 
  quantile(probs = seq(0.005, 0.995, by = 0.03))

p2 <- ggplot(data = df  %>% filter(year == 2019)  %>% st_as_sf()) +
  geom_sf(aes(fill = log(cell_GDPC)), color = NA) +
  scale_fill_gradientn(colors = my_palette,
                       values = scales::rescale(gradient_breaks),
                       limits = c(gradient_breaks[1], gradient_breaks[length(gradient_breaks)]),
                       oob = scales::squish,
                       na.value = "lightgrey", # Color for NA values
                       name = "log(GDP per capita)") +
  labs(subtitle = "log(GDP per capita in 2019)",
       fill = "Cell GDP per capita") +   
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 12, vjust = 0.9,hjust = 0.9),  # Emphasize legend title
    legend.text = element_text(size = 5),  # Adjust size of legend labels
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),  # Adjust legend box size
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.spacing = unit(0, "null")
  )

calculate_changes <- function(data, year1, year2) {
  data_1 <- data %>% filter(year == year1) %>%
    dplyr::select(cell_id, iso, predicted_GCP, cell_GDPC)
  
  data_2 <- data %>% filter(year == year2) %>%
    dplyr::select(cell_id, iso, predicted_GCP, cell_GDPC, geom)
  
  df <- data_1 %>%
    left_join(data_2, by = c("cell_id", "iso"), suffix = c("_1", "_2")) %>%
    mutate(change_GCP = log(predicted_GCP_2) - log(predicted_GCP_1),
           change_GDPC = log(cell_GDPC_2) - log(cell_GDPC_1))

  return(df)
}

df_2018_2019 <- calculate_changes(df, 2018, 2019)

my_palette <- colorRampPalette(brewer.pal(11, 'Spectral'))(34) 

gradient_breaks <- c(df_2018_2019 %>% 
  filter(is.finite(change_GCP)) %>% 
  pull(change_GCP) %>% 
  na.omit() %>% 
  quantile(probs = seq(0.002, 0.995, by = 0.03)))

p3 <- ggplot(data = df_2018_2019 %>% st_as_sf()) +
  geom_sf(aes(fill = change_GCP), color = NA) +
  scale_fill_gradientn(
  colours = my_palette,
  values = scales::rescale(gradient_breaks),
                       limits = c(gradient_breaks[1], gradient_breaks[length(gradient_breaks)]),
                       oob = scales::squish,
                       na.value = "lightgrey", # Color for NA values
                       "log difference") +
  labs(subtitle = "log(GDP in 2019) - log(GDP in 2018)",
       fill = "Cell GDP") +
  theme_void() +
  theme(
  plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 15, hjust = 0.5),
  legend.title = element_text(size = 12, vjust = 0.9, hjust = 0.9),  # Emphasize legend title
  legend.text = element_text(size = 5),  # Adjust size of legend labels
  legend.position = "bottom",
  legend.key.size = unit(0.4, "cm"),  # Adjust legend box size
  plot.margin = unit(c(0, 0, 0, 0), "cm"),
  panel.spacing = unit(0, "null")
)

gradient_breaks <- c(df_2018_2019 %>% 
  filter(is.finite(change_GDPC)) %>% 
  pull(change_GDPC) %>% 
  na.omit() %>% 
  quantile(probs = seq(0.002, 0.995, by = 0.03)))

p4 <- ggplot(data = df_2018_2019 %>% st_as_sf()) +
  geom_sf(aes(fill = change_GDPC), color = NA) +
  scale_fill_gradientn(
  colours = my_palette,
  values = scales::rescale(gradient_breaks),
                       limits = c(gradient_breaks[1], gradient_breaks[length(gradient_breaks)]),
                       oob = scales::squish,
                       na.value = "lightgrey", # Color for NA values
                       "log difference") +
  labs(subtitle = "log(GDP pc in 2019) - log(GDP pc in 2018)",
       fill = "Cell GDP") +
  theme_void() +
  theme(
  plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 15, hjust = 0.5),
  legend.title = element_text(size = 12, vjust = 0.9, hjust = 0.9),  # Emphasize legend title
  legend.text = element_text(size = 5),  # Adjust size of legend labels
  legend.position = "bottom",
  legend.key.size = unit(0.4, "cm"),  # Adjust legend box size
  plot.margin = unit(c(0, 0, 0, 0), "cm"),
  panel.spacing = unit(0, "null")
)

final_plot_with_title <- grid.arrange(
  arrangeGrob(p1, p2, p3, p4, ncol = 2, 
  nrow = 2, widths = c(1, 1), heights = c(1, 1)))

ggsave("step5_predict_and_post_adjustments_log_change/outputs/world_GDP_GDPC_no_extra_adjust.png", 
       plot = final_plot_with_title,
       bg = "white", width = 8.8, height = 5)

