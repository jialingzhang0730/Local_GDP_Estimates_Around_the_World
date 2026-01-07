# --------------------------------- Task Summary --------------------------------- #
# This file compares the predicted cell GDP with the true cell GDP for China.
# -------------------------------------------------------------------------------- #

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
library(patchwork)
library(gridExtra)
library(cowplot)
library(grid)

# read the true cell GDP
load("step6_shocks_log_change/outputs/CHN_test/chn_1deg_cell_GCP.RData")
china_truth <- chn_1deg_cell_GCP  %>% 
    dplyr::select(c(cell_id, prov_id, iso, year, GCP_1deg))  %>% 
    group_by(cell_id, prov_id, year)  %>% 
    mutate(GCP_1deg = sum(GCP_1deg))  %>% 
    ungroup()  %>% 
    distinct(cell_id, prov_id, year, GCP_1deg)

# read the predicted GDP
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust.RData")
china_predicted <- pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust  %>% 
    filter(iso == "CHN")  %>% 
    as.data.frame() %>% 
    dplyr::select(c(cell_id, id, iso, year, pred_GCP_1deg)) %>% 
    mutate(prov_id = case_when(
      id == "Hubei_CHN" ~ "42",
      id == "Sichuan_CHN" ~ "51",
      id == "Guangdong_CHN" ~ "44",
      id == "Zhejiang_CHN" ~ "33",
      id == "Jiangsu_CHN" ~ "32",
      id == "Shandong_CHN" ~ "37",
      id == "Henan_CHN" ~ "41"
    ))

# combine
china_df <- china_truth  %>% 
    left_join(china_predicted)  %>% 
    na.omit()  # some of cells are missing because of different sources of geometry

yr_group_lvl_pred <- china_df %>% 
    mutate(yr_levl_group = ifelse(year %in% c(2012:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    mutate(log_GCP_1deg = log(GCP_1deg), 
           log_pred_GCP_1deg = log(pred_GCP_1deg)) %>% 
    filter(is.finite(log_GCP_1deg) & is.finite(log_pred_GCP_1deg)) %>% 
    group_by(yr_levl_group) %>% 
    mutate(R2_levl = 1 - sum((log_GCP_1deg - log_pred_GCP_1deg)^2) / sum((log_GCP_1deg - mean(log_GCP_1deg))^2))  %>% 
    ungroup()

change <- china_df %>%
  arrange(iso, prov_id, cell_id, year) %>% 
  group_by(iso, prov_id, cell_id) %>% 
  mutate(grate_pred = log(pred_GCP_1deg) - log(lag(pred_GCP_1deg)), 
         grate_true = log(GCP_1deg) - log(lag(GCP_1deg))) %>%
  ungroup() %>% 
  distinct(iso, prov_id, cell_id, year, .keep_all = TRUE) %>% # Ensure distinct rows
  dplyr::select(iso, prov_id, cell_id, year, grate_pred, grate_true, pred_GCP_1deg) %>%
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

# Create the combined plot 
       
combined_plot <- ggplot(combined_data, aes(x = ifelse(type == "Log Level:", log_GCP_1deg, grate_true), 
                                           y = ifelse(type == "Log Level:", log_pred_GCP_1deg, grate_pred))) +
  geom_point(size = 1, alpha = 0.7, color = "blue") +
  geom_text(aes(label = annotation), x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 7, color = "black", family = "Helvetica") + 
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
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

ggsave("step6_shocks_log_change/outputs/CHN_test/log_level_change_r2.pdf", plot = combined_plot, width = 16, height = 10, bg = "white")


# Wuhan time series
plot1 <- ggplot() +
  geom_point(data = china_df %>% filter(cell_id == 21535), 
             aes(x = year, y = pred_GCP_1deg, color = "Predicted"), 
             size = 2, shape = 19, alpha = 0.7) +
  geom_line(data = china_df %>% filter(cell_id == 21535), 
            aes(x = year, y = pred_GCP_1deg, color = "Predicted"), 
            size = 1.2) +
  geom_point(data = china_df %>% filter(cell_id == 21535), 
             aes(x = year, y = GCP_1deg, color = "Actual"), 
             size = 2, shape = 17, alpha = 0.7) +
  geom_line(data = china_df %>% filter(cell_id == 21535), 
            aes(x = year, y = GCP_1deg, color = "Actual"), 
            size = 1.2) +
  scale_color_manual(values = c(name = NULL, "Predicted" = "#BE1508", "Actual" = "#7030A0")) +
  labs(
    y = "1deg Cell GDP",
    # title = "Cell 21535: Main Activity Hub of Wuhan",
    # subtitle = "Unit: billion const 2017 USD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5, color = "black", face = "bold"),    
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "black"),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 15, color = "#333333"),
    legend.title = element_blank(),
    legend.text = element_text(size = 19),
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(10, 10, 10, 10),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
  )

ggsave("step6_shocks_log_change/outputs/CHN_test/wuhan_time_series.pdf", 
       plot = plot1,
       bg = "white", width = 10)

# ----------------------------------------------------------------------------------------------------------------------------------
# now tests the predictions from the model trained using data up to 2019 to assess whether the method can still maintain good performance.

# read the true cell GDP
load("step6_shocks_log_change/outputs/CHN_test/chn_1deg_cell_GCP.RData")
china_truth <- chn_1deg_cell_GCP  %>% 
    dplyr::select(c(cell_id, prov_id, iso, year, GCP_1deg))  %>% 
    group_by(cell_id, prov_id, year)  %>% 
    mutate(GCP_1deg = sum(GCP_1deg))  %>% 
    ungroup()  %>% 
    distinct(cell_id, prov_id, year, GCP_1deg)

# read the predicted GDP
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019.RData")
china_predicted <- pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019  %>% 
    filter(iso == "CHN")  %>% 
    as.data.frame() %>% 
    dplyr::select(c(cell_id, id, iso, year, pred_GCP_1deg)) %>% 
    mutate(prov_id = case_when(
      id == "Hubei_CHN" ~ "42",
      id == "Sichuan_CHN" ~ "51",
      id == "Guangdong_CHN" ~ "44",
      id == "Zhejiang_CHN" ~ "33",
      id == "Jiangsu_CHN" ~ "32",
      id == "Shandong_CHN" ~ "37",
      id == "Henan_CHN" ~ "41"
    ))

# combine
china_df <- china_truth  %>% 
    left_join(china_predicted)  %>% 
    na.omit()  # some of cells are missing because of different sources of geometry

yr_group_lvl_pred <- china_df %>% 
    mutate(yr_levl_group = ifelse(year %in% c(2012:2019), "Pre-COVID 2012-2019", ifelse(year %in% c(2020), "COVID 2020", "Post-COVID 2021"))) %>% 
    mutate(log_GCP_1deg = log(GCP_1deg), 
           log_pred_GCP_1deg = log(pred_GCP_1deg)) %>% 
    filter(is.finite(log_GCP_1deg) & is.finite(log_pred_GCP_1deg)) %>% 
    group_by(yr_levl_group) %>% 
    mutate(R2_levl = 1 - sum((log_GCP_1deg - log_pred_GCP_1deg)^2) / sum((log_GCP_1deg - mean(log_GCP_1deg))^2))  %>% 
    ungroup()

change <- china_df %>%
  arrange(iso, prov_id, cell_id, year) %>% 
  group_by(iso, prov_id, cell_id) %>% 
  mutate(grate_pred = log(pred_GCP_1deg) - log(lag(pred_GCP_1deg)), 
         grate_true = log(GCP_1deg) - log(lag(GCP_1deg))) %>%
  ungroup() %>% 
  distinct(iso, prov_id, cell_id, year, .keep_all = TRUE) %>% # Ensure distinct rows
  dplyr::select(iso, prov_id, cell_id, year, grate_pred, grate_true, pred_GCP_1deg) %>%
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

# Create the combined plot
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
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

ggsave("step6_shocks_log_change/outputs/CHN_test/log_level_change_r2_use_model_upto_2019.png", plot = combined_plot, width = 18, height = 12, bg = "white")
