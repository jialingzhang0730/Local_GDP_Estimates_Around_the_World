# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to compare the predicted oob cell GDP with true cell GDP for ITA
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
library(RColorBrewer)
library(terra)
library(exactextractr)
library(readxl)

# ---------- 1deg ---------- #
# read the true cell GDP
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_1deg.RData")
truth <- predict_data_complete_1deg  %>% 
    mutate(iso = ifelse(substr(iso,1,3) == "USA", "USA", iso)) %>%
    dplyr::select(c(cell_id, iso, year, GCP_1deg))  %>% 
    group_by(cell_id, iso, year)  %>% 
    mutate(GCP_1deg = sum(GCP_1deg))  %>% 
    ungroup()  %>% 
    distinct(cell_id, iso, year, GCP_1deg)
    
# read the predicted GDP
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_up_to_2019.RData")
predicted <- GDPC_1deg_postadjust_pop_dens_no_extra_adjust_up_to_2019  %>% 
    as.data.frame()  %>% 
    dplyr::select(c(cell_id, iso, year, predicted_GCP)) %>% 
    filter(iso %in% unique(truth$iso))

df <- truth  %>% 
    left_join(predicted) 

yr_group_lvl_pred <- df %>% 
    mutate(yr_levl_group = ifelse(year %in% c(2012:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    mutate(log_GCP_1deg = log(GCP_1deg), 
           log_pred_GCP_1deg = log(predicted_GCP)) %>% 
    filter(is.finite(log_GCP_1deg) & is.finite(log_pred_GCP_1deg)) %>%     
    group_by(yr_levl_group) %>% 
    mutate(R2_levl = 1 - sum((log_GCP_1deg - log_pred_GCP_1deg)^2) / sum((log_GCP_1deg - mean(log_GCP_1deg))^2),
           GDP_loss = sum(abs(log_GCP_1deg - log_pred_GCP_1deg)) / (2*sum(log_GCP_1deg)))  %>% 
    ungroup() 

change <- df %>%
  arrange(iso, cell_id, year) %>% # Arrange data by iso, cell_id, and year
  group_by(iso, cell_id) %>% # Group by iso and cell_id
  mutate(grate_pred = log(predicted_GCP) - log(lag(predicted_GCP)), # Growth rate for predicted GCP
         grate_true = log(GCP_1deg) - log(lag(GCP_1deg))) %>% # Growth rate for true GCP
  ungroup() %>% 
  distinct(iso, cell_id, year, .keep_all = TRUE) %>% # Ensure distinct rows
  dplyr::select(iso, cell_id, year, grate_pred, grate_true, predicted_GCP) %>%
  filter(year != 2012) %>% 
  filter(is.finite(grate_pred) & is.finite(grate_true))

yr_group_chan_pred <- change %>% 
    mutate(yr_chan_group = ifelse(year %in% c(2013:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    group_by(yr_chan_group) %>% 
    mutate(R2_chan = 1 - sum((grate_true - grate_pred)^2) / sum((grate_true - mean(grate_true))^2)) %>%
    ungroup() 

yr_group_lvl_pred2 <- yr_group_lvl_pred %>%
  mutate(type = "Log Level:")

yr_group_chan_pred2 <- yr_group_chan_pred %>%
  mutate(type = "Year-over-year Log Change:")

combined_data <- bind_rows(yr_group_lvl_pred2, yr_group_chan_pred2) %>%
  mutate(plot_group = paste(type, ifelse(type == "Log Level:", yr_levl_group, yr_chan_group))) %>% 
  mutate(annotation = ifelse(type == "Log Level:", 
                        sprintf("R² = %.2f%%", R2_levl * 100), 
                        sprintf("R² = %.2f%%", R2_chan * 100))) %>% 
  mutate(plot_group = case_when(
    plot_group == "Log Level: Pre-COVID 2012-2019" ~ "Pre-COVID 2012 to 2019: log(GDP)",
    plot_group == "Log Level: COVID 2020" ~ "COVID 2020: log(GDP)",
    plot_group == "Log Level: Post-COVID 2021" ~ "Post-COVID 2021: log(GDP)",
    plot_group == "Year-over-year Log Change: Pre-COVID 2012-2019" ~ "Pre-COVID 2012 to 2019: \nlog(GDP in t) - log(GDP in t-1)",
    plot_group == "Year-over-year Log Change: COVID 2020" ~ "COVID 2020: \nlog(GDP in 2020) - log(GDP in 2019)",
    plot_group == "Year-over-year Log Change: Post-COVID 2021" ~ "Post-COVID 2021: \nlog(GDP in 2021) - log(GDP in 2020)"
  ))

# Create the combined plot with facet_wrap, using nrow and ncol for layout
combined_plot <- ggplot(combined_data, aes(x = ifelse(type == "Log Level:", log_GCP_1deg, grate_true), 
                                           y = ifelse(type == "Log Level:", log_pred_GCP_1deg, grate_pred))) +
  geom_point(size = 1, alpha = 0.7, color = "blue") +
  geom_text(aes(label = annotation), x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 7, color = "black") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 0.5) +
  labs(x = "True Values: 1deg Cells",
       y = "Predicted Values: 1deg Cells") +
  facet_wrap(~ factor(plot_group, levels = c("Pre-COVID 2012 to 2019: log(GDP)", 
                                             "COVID 2020: log(GDP)", 
                                             "Post-COVID 2021: log(GDP)",
                                             "Pre-COVID 2012 to 2019: \nlog(GDP in t) - log(GDP in t-1)", 
                                             "COVID 2020: \nlog(GDP in 2020) - log(GDP in 2019)", 
                                             "Post-COVID 2021: \nlog(GDP in 2021) - log(GDP in 2020)")),
             nrow = 2, ncol = 3, scales = "free") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

ggsave("step6_shocks_log_change/outputs/log_level_change_r2_training_all.png", plot = combined_plot, width = 18, height = 12, bg = "white")


# ---------- 0.5deg ---------- #
# read the true cell GDP
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_0_5deg.RData")
truth <- predict_data_complete_0_5deg  %>% 
    mutate(iso = ifelse(substr(iso,1,3) == "USA", "USA", iso)) %>%
    dplyr::select(c(cell_id, subcell_id, iso, year, GCP_0_5deg))  %>% 
    group_by(cell_id, subcell_id, iso, year)  %>% 
    mutate(GCP_0_5deg = sum(GCP_0_5deg))  %>% 
    ungroup()  %>% 
    distinct(cell_id, subcell_id, iso, year, GCP_0_5deg)
    
# read the predicted GDP
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_up_to_2019.RData")
predicted <- GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_up_to_2019  %>% 
    as.data.frame()  %>% 
    dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP)) %>% 
    filter(iso %in% unique(truth$iso))

df <- truth  %>% 
    left_join(predicted) 

yr_group_lvl_pred <- df %>% 
    mutate(yr_levl_group = ifelse(year %in% c(2012:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    mutate(log_GCP_0_5deg = log(GCP_0_5deg), 
           log_pred_GCP_0_5deg = log(predicted_GCP)) %>% 
    filter(is.finite(log_GCP_0_5deg) & is.finite(log_pred_GCP_0_5deg)) %>%     
    group_by(yr_levl_group) %>% 
    mutate(R2_levl = 1 - sum((log_GCP_0_5deg - log_pred_GCP_0_5deg)^2) / sum((log_GCP_0_5deg - mean(log_GCP_0_5deg))^2),
           GDP_loss = sum(abs(log_GCP_0_5deg - log_pred_GCP_0_5deg)) / (2*sum(log_GCP_0_5deg)))  %>% 
    ungroup() 

change <- df %>%
  arrange(iso, cell_id, subcell_id, year) %>% 
  group_by(iso, cell_id, subcell_id) %>% 
  mutate(grate_pred = log(predicted_GCP) - log(lag(predicted_GCP)), # Growth rate for predicted GCP
         grate_true = log(GCP_0_5deg) - log(lag(GCP_0_5deg))) %>% # Growth rate for true GCP
  ungroup() %>% 
  distinct(iso, cell_id, subcell_id, year, .keep_all = TRUE) %>% # Ensure distinct rows
  dplyr::select(iso, cell_id, subcell_id, year, grate_pred, grate_true, predicted_GCP) %>%
  filter(year != 2012) %>% 
  filter(is.finite(grate_pred) & is.finite(grate_true))

yr_group_chan_pred <- change %>% 
    mutate(yr_chan_group = ifelse(year %in% c(2013:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    group_by(yr_chan_group) %>% 
    mutate(R2_chan = 1 - sum((grate_true - grate_pred)^2) / sum((grate_true - mean(grate_true))^2)) %>%
    ungroup() 

yr_group_lvl_pred2 <- yr_group_lvl_pred %>%
  mutate(type = "Log Level:")

yr_group_chan_pred2 <- yr_group_chan_pred %>%
  mutate(type = "Year-over-year Log Change:")

combined_data <- bind_rows(yr_group_lvl_pred2, yr_group_chan_pred2) %>%
  mutate(plot_group = paste(type, ifelse(type == "Log Level:", yr_levl_group, yr_chan_group))) %>% 
  mutate(annotation = ifelse(type == "Log Level:", 
                        sprintf("R² = %.2f%%", R2_levl * 100), 
                        sprintf("R² = %.2f%%", R2_chan * 100))) %>% 
  mutate(plot_group = case_when(
    plot_group == "Log Level: Pre-COVID 2012-2019" ~ "Pre-COVID 2012 to 2019: log(GDP)",
    plot_group == "Log Level: COVID 2020" ~ "COVID 2020: log(GDP)",
    plot_group == "Log Level: Post-COVID 2021" ~ "Post-COVID 2021: log(GDP)",
    plot_group == "Year-over-year Log Change: Pre-COVID 2012-2019" ~ "Pre-COVID 2012 to 2019: \nlog(GDP in t) - log(GDP in t-1)",
    plot_group == "Year-over-year Log Change: COVID 2020" ~ "COVID 2020: \nlog(GDP in 2020) - log(GDP in 2019)",
    plot_group == "Year-over-year Log Change: Post-COVID 2021" ~ "Post-COVID 2021: \nlog(GDP in 2021) - log(GDP in 2020)"
  ))

# Create the combined plot with facet_wrap, using nrow and ncol for layout
combined_plot <- ggplot(combined_data, aes(x = ifelse(type == "Log Level:", log_GCP_0_5deg, grate_true), 
                                           y = ifelse(type == "Log Level:", log_pred_GCP_0_5deg, grate_pred))) +
  geom_point(size = 1, alpha = 0.7, color = "blue") +
  geom_text(aes(label = annotation), x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 5, color = "black") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 0.5) +
  labs(x = "True Values: 0.5deg Cells",
       y = "Predicted Values: 0.5deg Cells") +
  facet_wrap(~ factor(plot_group, levels = c("Pre-COVID 2012 to 2019: log(GDP)", 
                                             "COVID 2020: log(GDP)", 
                                             "Post-COVID 2021: log(GDP)",
                                             "Pre-COVID 2012 to 2019: \nlog(GDP in t) - log(GDP in t-1)", 
                                             "COVID 2020: \nlog(GDP in 2020) - log(GDP in 2019)", 
                                             "Post-COVID 2021: \nlog(GDP in 2021) - log(GDP in 2020)")),
             nrow = 2, ncol = 3, scales = "free") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

ggsave("step6_shocks_log_change/outputs/log_level_change_r2_training_all_0_5deg.png", plot = combined_plot, width = 18, height = 12, bg = "white")


# ---------- 0.25deg ---------- #
# read the true cell GDP
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_0_25deg.RData")
truth <- predict_data_complete_0_25deg  %>% 
    mutate(iso = ifelse(substr(iso,1,3) == "USA", "USA", iso)) %>%
    dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, GCP_0_25deg))  %>% 
    group_by(cell_id, subcell_id, subcell_id_0_25, iso, year)  %>% 
    mutate(GCP_0_25deg = sum(GCP_0_25deg))  %>% 
    ungroup()  %>% 
    distinct(cell_id, subcell_id, subcell_id_0_25, iso, year, GCP_0_25deg)
    
# read the predicted GDP
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_up_to_2019.RData")
predicted <- GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_up_to_2019  %>% 
    as.data.frame()  %>% 
    dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP)) %>% 
    filter(iso %in% unique(truth$iso))

df <- truth  %>% 
    left_join(predicted) 

yr_group_lvl_pred <- df %>% 
    mutate(yr_levl_group = ifelse(year %in% c(2012:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    mutate(log_GCP_0_25deg = log(GCP_0_25deg), 
           log_pred_GCP_0_25deg = log(predicted_GCP)) %>% 
    filter(is.finite(log_GCP_0_25deg) & is.finite(log_pred_GCP_0_25deg)) %>%     
    group_by(yr_levl_group) %>% 
    mutate(R2_levl = 1 - sum((log_GCP_0_25deg - log_pred_GCP_0_25deg)^2) / sum((log_GCP_0_25deg - mean(log_GCP_0_25deg))^2),
           GDP_loss = sum(abs(log_GCP_0_25deg - log_pred_GCP_0_25deg)) / (2*sum(log_GCP_0_25deg)))  %>% 
    ungroup() 

change <- df %>%
  arrange(iso, cell_id, subcell_id, subcell_id_0_25, year) %>% 
  group_by(iso, cell_id, subcell_id, subcell_id_0_25) %>% 
  mutate(grate_pred = log(predicted_GCP) - log(lag(predicted_GCP)), # Growth rate for predicted GCP
         grate_true = log(GCP_0_25deg) - log(lag(GCP_0_25deg))) %>% # Growth rate for true GCP
  ungroup() %>% 
  distinct(iso, cell_id, subcell_id, subcell_id_0_25, year, .keep_all = TRUE) %>% # Ensure distinct rows
  dplyr::select(iso, cell_id, subcell_id, subcell_id_0_25, year, grate_pred, grate_true, predicted_GCP) %>%
  filter(year != 2012) %>% 
  filter(is.finite(grate_pred) & is.finite(grate_true))

yr_group_chan_pred <- change %>% 
    mutate(yr_chan_group = ifelse(year %in% c(2013:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    group_by(yr_chan_group) %>% 
    mutate(R2_chan = 1 - sum((grate_true - grate_pred)^2) / sum((grate_true - mean(grate_true))^2)) %>%
    ungroup() 

yr_group_lvl_pred2 <- yr_group_lvl_pred %>%
  mutate(type = "Log Level:")

yr_group_chan_pred2 <- yr_group_chan_pred %>%
  mutate(type = "Year-over-year Log Change:")

combined_data <- bind_rows(yr_group_lvl_pred2, yr_group_chan_pred2) %>%
  mutate(plot_group = paste(type, ifelse(type == "Log Level:", yr_levl_group, yr_chan_group))) %>% 
  mutate(annotation = ifelse(type == "Log Level:", 
                        sprintf("R² = %.2f%%", R2_levl * 100), 
                        sprintf("R² = %.2f%%", R2_chan * 100))) %>% 
  mutate(plot_group = case_when(
    plot_group == "Log Level: Pre-COVID 2012-2019" ~ "Pre-COVID 2012 to 2019: log(GDP)",
    plot_group == "Log Level: COVID 2020" ~ "COVID 2020: log(GDP)",
    plot_group == "Log Level: Post-COVID 2021" ~ "Post-COVID 2021: log(GDP)",
    plot_group == "Year-over-year Log Change: Pre-COVID 2012-2019" ~ "Pre-COVID 2012 to 2019: \nlog(GDP in t) - log(GDP in t-1)",
    plot_group == "Year-over-year Log Change: COVID 2020" ~ "COVID 2020: \nlog(GDP in 2020) - log(GDP in 2019)",
    plot_group == "Year-over-year Log Change: Post-COVID 2021" ~ "Post-COVID 2021: \nlog(GDP in 2021) - log(GDP in 2020)"
  ))

# Create the combined plot with facet_wrap, using nrow and ncol for layout
combined_plot <- ggplot(combined_data, aes(x = ifelse(type == "Log Level:", log_GCP_0_25deg, grate_true), 
                                           y = ifelse(type == "Log Level:", log_pred_GCP_0_25deg, grate_pred))) +
  geom_point(size = 1, alpha = 0.7, color = "blue") +
  geom_text(aes(label = annotation), x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 5, color = "black") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 0.5) +
  labs(x = "True Values: 0.25deg Cells",
       y = "Predicted Values: 0.25deg Cells") +
  facet_wrap(~ factor(plot_group, levels = c("Pre-COVID 2012 to 2019: log(GDP)", 
                                             "COVID 2020: log(GDP)", 
                                             "Post-COVID 2021: log(GDP)",
                                             "Pre-COVID 2012 to 2019: \nlog(GDP in t) - log(GDP in t-1)", 
                                             "COVID 2020: \nlog(GDP in 2020) - log(GDP in 2019)", 
                                             "Post-COVID 2021: \nlog(GDP in 2021) - log(GDP in 2020)")),
             nrow = 2, ncol = 3, scales = "free") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

ggsave("step6_shocks_log_change/outputs/log_level_change_r2_training_all_0_25deg.png", plot = combined_plot, width = 18, height = 12, bg = "white")
