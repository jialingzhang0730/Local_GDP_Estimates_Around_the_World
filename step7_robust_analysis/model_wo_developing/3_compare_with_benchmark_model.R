# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to compare this predictions with our formal benchmark model in section "step4_train_and_tune_log_change" (also our model in the paper)
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load packages
library(tidyverse)
library(magrittr)
library(tictoc)
library(gdata)
library(ranger)
library(tidymodels)
library(speedglm)
library(vip)
library(kableExtra)
library(estimatr)
library(gridExtra)
library(grid)
library(sf)
library(parallel)
library(terra)
library(exactextractr)
library(RColorBrewer)
library(janitor)
library(cowplot)

# ------------------------------------------------------------------------------------------------------------------------------------
# Cross validation results
all_iso_1deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_1deg.csv")
all_iso_0_5deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_0_5deg.csv")
all_iso_0_25deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_0_25deg.csv")

variable_names <- c(
  "NTL from urban", "Lag NTL from urban", "Population", "Lag population", 
  "Urban areas", "Lag urban areas", 
  "Fossil CO2 from transportation", "Lag fossil CO2 from transportation",
  "Biofuel CO2 from manufacturing", "Lag biofuel CO2 from manufacturing"
)

# Function to clean and standardize variable names
clean_names <- function(df) {
  df %>%
    mutate(Variable = case_when(
      Variable == "NTL_urban_snow_free_period_share" ~ "NTL from urban",
      Variable == "lag_NTL_urban_share" ~ "Lag NTL from urban",
      Variable == "pop_share" ~ "Population",
      Variable == "lag_pop_share" ~ "Lag population",
      Variable == "urban_share" ~ "Urban areas",
      Variable == "lag_urban_share" ~ "Lag urban areas",
      Variable == "CO2_non_org_tspt_share" ~ "Fossil CO2 from transportation",
      Variable == "lag_CO2_non_org_tspt_share" ~ "Lag fossil CO2 from transportation",
      Variable == "CO2_bio_manuf_conbust_share" ~ "Biofuel CO2 from manufacturing",
      Variable == "lag_CO2_bio_mc_share" ~ "Lag biofuel CO2 from manufacturing",
      TRUE ~ Variable
    ))
}
importance_1deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/importance_scores_1deg.csv")  %>%
  clean_names()
importance_0_5deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/importance_scores_0_5deg.csv") %>%
  clean_names()
importance_0_25deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/importance_scores_0_25deg.csv")  %>%
  clean_names()

merged_importance <- data.frame(
  Metric = variable_names
) %>%
  left_join(importance_1deg %>% rename(`1-degree Model` = Importance), by = c("Metric" = "Variable")) %>%
  left_join(importance_0_5deg %>% rename(`0.5-degree Model` = Importance), by = c("Metric" = "Variable")) %>%
  left_join(importance_0_25deg %>% rename(`0.25-degree Model` = Importance), by = c("Metric" = "Variable")) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

data <- data.frame(
  Metric = c("$R^2$ (All)",
             "$R^2$ (All)",
             merged_importance$Metric),
  
  `1-deg` = c(
    paste0(round(all_iso_1deg$mean_r2_all * 100, 2), "\\%"), 
    paste0(round(all_iso_1deg$mean_chan_r2_all * 100, 2), "\\%"),
    merged_importance$`1-degree Model`
  ),
  
  `0.5-deg` = c(
    paste0(round(all_iso_0_5deg$mean_r2_all * 100, 2), "\\%"), 
    paste0(round(all_iso_0_5deg$mean_chan_r2_all * 100, 2), "\\%"),
    merged_importance$`0.5-degree Model`
  ),
  
  `0.25-deg` = c(
    paste0(round(all_iso_0_25deg$mean_r2_all * 100, 2), "\\%"), 
    paste0(round(all_iso_0_25deg$mean_chan_r2_all * 100, 2), "\\%"),
    merged_importance$`0.25-degree Model`
  )
)

latex_table <- kable(data, "latex", booktabs = TRUE, escape = FALSE, align = 'lccc', col.names = c(" ", "1-degree Model", "0.5-degree Model", "0.25-degree Model")) %>%
  kable_styling(full_width = FALSE) %>% 
  pack_rows("Panel A: R2 of Log GDP Level", 1,1, bold = FALSE, italic = TRUE) %>% 
  pack_rows("Panel B: R2 of log(GDP in t) - log(GDP in t-1)", 2,2, bold = FALSE, italic = TRUE) %>%
  pack_rows("Panel C: Variables and Importance Scores", 3,12, bold = FALSE, italic = TRUE)

latex_table <- gsub("R2", "\\$R^2\\$", latex_table)

save_kable(latex_table, file = "step7_robust_analysis/model_wo_developing/outputs/model9_tuning/CV_metric_table.tex")

# ------------------------------------------------------------------------------------------------------------------------------------
# some plots
benchmark_1deg <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_no_extra_adjust.csv")
benchmark_0_5deg <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.csv")
benchmark_0_25deg <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.csv")

test_1deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv")
test_0_5deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv")
test_0_25deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv")

# 1deg
df <- benchmark_1deg %>% 
  dplyr::select(c(cell_id, iso, year, predicted_GCP)) %>% 
  left_join(test_1deg %>% dplyr::select(c(cell_id, iso, year, predicted_GCP)) %>% rename(test_predicted_GCP = predicted_GCP)) %>% 
  filter(predicted_GCP != 0 & test_predicted_GCP != 0) %>% 
  mutate(r2_log_levl = 1 - sum((log(predicted_GCP) - log(test_predicted_GCP))^2) / 
                 sum((log(predicted_GCP) - mean(log(predicted_GCP)))^2))

p1 <- ggplot(df, aes(x=log(predicted_GCP), y = log(test_predicted_GCP))) +
    geom_point(size = 1, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "1-degree Model Predictions \nAll Years Data Included", # Unit: const 2017 billion USD
    x = "log(GDP) from Model Trained \nWith Developing Countries",
    y = "log(GDP) from Model Trained \nWithout Developing Countries")+
    annotate("text", x = Inf, y = -Inf, label = paste0("R² = ", scales::percent(unique(df$r2_log_levl), accuracy = 0.01)), 
           hjust = 1.2, vjust = -1.2, size = 7, color = "black") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = margin(0, 26, 20, 0)
    )     

df <- benchmark_1deg %>% 
  dplyr::select(c(cell_id, iso, year, predicted_GCP)) %>% 
  left_join(test_1deg %>% dplyr::select(c(cell_id, iso, year, predicted_GCP)) %>% rename(test_predicted_GCP = predicted_GCP)) %>% 
  arrange(iso, cell_id, year) %>% 
  group_by(iso, cell_id) %>%
  mutate(prev_year_pred = ifelse(year - 1 %in% year, predicted_GCP[match(year - 1, year)], NA),
         prev_year_pred_test = ifelse(year - 1 %in% year, test_predicted_GCP[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_pred_test)) %>% 
  filter(predicted_GCP != 0 & prev_year_pred != 0 & prev_year_pred_test != 0 & test_predicted_GCP != 0) %>% 
  mutate(log_diff = log(predicted_GCP) - log(prev_year_pred),
         log_diff_test = log(test_predicted_GCP) - log(prev_year_pred_test)) %>% 
  mutate(r2_log_chan = 1 - sum((log_diff - log_diff_test)^2) / sum((log_diff - mean(log_diff))^2)) %>% 
  as.data.frame()

p2 <- ggplot(df, aes(x=log_diff, y = log_diff_test)) +
    geom_point(size = 1, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "1-degree Model Predictions \nAll Years Data Included", # Unit: const 2017 billion USD
    x = "log(GDP in t) - log(GDP in t-1)\nfrom Model Trained With \nDeveloping Countries",
    y = "log(GDP in t) - log(GDP in t-1)\nfrom Model Trained Without \nDeveloping Countries")+
    annotate("text", x = Inf, y = -Inf, label = paste0("R² = ", scales::percent(unique(df$r2_log_chan), accuracy = 0.01)), 
           hjust = 1.2, vjust = -1.2, size = 7, color = "black")  +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = margin(20, 26, 0, 0))

# 0.5deg
df <- benchmark_0_5deg %>% 
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP)) %>% 
  left_join(test_0_5deg %>% dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP)) %>% rename(test_predicted_GCP = predicted_GCP)) %>% 
  filter(predicted_GCP != 0 & test_predicted_GCP != 0) %>% 
  mutate(r2_log_levl = 1 - sum((log(predicted_GCP) - log(test_predicted_GCP))^2) / 
                 sum((log(predicted_GCP) - mean(log(predicted_GCP)))^2))

p3 <- ggplot(df, aes(x=log(predicted_GCP), y = log(test_predicted_GCP))) +
    geom_point(size = 1, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "0.5-degree Model Predictions \nAll Years Data Included", # Unit: const 2017 billion USD 
    x = "log(GDP) from Model Trained \nWith Developing Countries",
    y = "log(GDP) from Model Trained \nWithout Developing Countries")+
    annotate("text", x = Inf, y = -Inf, label = paste0("R² = ", scales::percent(unique(df$r2_log_levl), accuracy = 0.01)), 
           hjust = 1.2, vjust = -1.2, size = 7, color = "black") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = margin(0, 13, 20, 13)
    )   

df <- benchmark_0_5deg %>% 
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP)) %>% 
  left_join(test_0_5deg %>% dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP)) %>% rename(test_predicted_GCP = predicted_GCP)) %>% 
  arrange(iso, cell_id, subcell_id, year) %>% 
  group_by(iso, cell_id, subcell_id) %>%
  mutate(prev_year_pred = ifelse(year - 1 %in% year, predicted_GCP[match(year - 1, year)], NA),
         prev_year_pred_test = ifelse(year - 1 %in% year, test_predicted_GCP[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_pred_test)) %>% 
  filter(predicted_GCP != 0 & prev_year_pred != 0 & prev_year_pred_test != 0 & test_predicted_GCP != 0) %>% 
  mutate(log_diff = log(predicted_GCP) - log(prev_year_pred),
         log_diff_test = log(test_predicted_GCP) - log(prev_year_pred_test)) %>% 
  mutate(r2_log_chan = 1 - sum((log_diff - log_diff_test)^2) / sum((log_diff - mean(log_diff))^2)) %>% 
  as.data.frame()

p4 <- ggplot(df, aes(x=log_diff, y = log_diff_test)) +
    geom_point(size = 1, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "0.5-degree Model Predictions \nAll Years Data Included", # Unit: const 2017 billion USD
    x = "log(GDP in t) - log(GDP in t-1)\nfrom Model Trained With \nDeveloping Countries",
    y = "log(GDP in t) - log(GDP in t-1)\nfrom Model Trained Without \nDeveloping Countries")+
    annotate("text", x = Inf, y = -Inf, label = paste0("R² = ", scales::percent(unique(df$r2_log_chan), accuracy = 0.01)), 
           hjust = 1.2, vjust = -1.2, size = 7, color = "black") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = margin(20, 13, 0, 13)
    )   

# 0.25deg
df <- benchmark_0_25deg %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP)) %>% 
  left_join(test_0_25deg %>% dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP)) %>% rename(test_predicted_GCP = predicted_GCP)) %>% 
  filter(predicted_GCP != 0 & test_predicted_GCP != 0) %>% 
  mutate(r2_log_levl = 1 - sum((log(predicted_GCP) - log(test_predicted_GCP))^2) / 
                 sum((log(predicted_GCP) - mean(log(predicted_GCP)))^2))

p5 <- ggplot(df, aes(x=log(predicted_GCP), y = log(test_predicted_GCP))) +
    geom_point(size = 1, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "0.25-degree Model Predictions \nAll Years Data Included", # Unit: const 2017 billion USD
    x = "log(GDP) from Model Trained \nWith Developing Countries",
    y = "log(GDP) from Model Trained \nWithout Developing Countries")+
    annotate("text", x = Inf, y = -Inf, label = paste0("R² = ", scales::percent(unique(df$r2_log_levl), accuracy = 0.01)), 
           hjust = 1.2, vjust = -1.2, size = 7, color = "black") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = margin(0, 0, 20, 26)
    )  

df <- benchmark_0_25deg %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP)) %>% 
  left_join(test_0_25deg %>% dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP)) %>% rename(test_predicted_GCP = predicted_GCP)) %>% 
  arrange(iso, cell_id, subcell_id, subcell_id_0_25, year) %>% 
  group_by(iso, cell_id, subcell_id, subcell_id_0_25) %>%
  mutate(prev_year_pred = ifelse(year - 1 %in% year, predicted_GCP[match(year - 1, year)], NA),
         prev_year_pred_test = ifelse(year - 1 %in% year, test_predicted_GCP[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_pred_test)) %>% 
  filter(predicted_GCP != 0 & prev_year_pred != 0 & prev_year_pred_test != 0 & test_predicted_GCP != 0) %>% 
  mutate(log_diff = log(predicted_GCP) - log(prev_year_pred),
         log_diff_test = log(test_predicted_GCP) - log(prev_year_pred_test)) %>% 
  mutate(r2_log_chan = 1 - sum((log_diff - log_diff_test)^2) / sum((log_diff - mean(log_diff))^2)) %>% 
  as.data.frame()

p6 <- ggplot(df, aes(x=log_diff, y = log_diff_test)) +
    geom_point(size = 1, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "0.25-degree Model Predictions \nAll Years Data Included", # Unit: const 2017 billion USD
    x = "log(GDP in t) - log(GDP in t-1)\nfrom Model Trained With \nDeveloping Countries",
    y = "log(GDP in t) - log(GDP in t-1)\nfrom Model Trained Without \nDeveloping Countries")+
    annotate("text", x = Inf, y = -Inf, label = paste0("R² = ", scales::percent(unique(df$r2_log_chan), accuracy = 0.01)), 
           hjust = 1.2, vjust = -1.2, size = 7, color = "black") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = margin(20, 0, 0, 26)
    ) 

combined_plot <- plot_grid(p1, p3, p5, p2, p4, p6, ncol = 3)

ggsave("step7_robust_analysis/model_wo_developing/outputs/compare_w_wo_devlping_all_deg.png", width = 22, height = 14, bg = "white")

# ------------------------------------------------------------------------------------------------------------
# check the R2 for developing countries
# 1-degree model

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_1deg.csv") 
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_1deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_1deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_1deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_1deg.csv") 

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

truth <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  filter(iso %in% developing_group) %>% 
  dplyr::select(c(cell_id, iso, year, GCP_1deg))

chec_level <- truth %>%
  left_join(read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
            filter(iso %in% developing_group) %>%
            dplyr::select(c(cell_id, iso, year, predicted_GCP))) %>% 
  filter(GCP_1deg != 0 & predicted_GCP != 0) %>% 
  group_by(iso) %>% 
  mutate(r2_level = 1 - sum((log(GCP_1deg) - log(predicted_GCP))^2) / sum((log(GCP_1deg) - mean(log(GCP_1deg)))^2),
         GDP_loss = sum(abs(GCP_1deg - predicted_GCP)) / (2*sum(GCP_1deg))) %>% 
  ungroup() %>% 
  distinct(iso, r2_level, GDP_loss)
  
chec_change <- truth %>%
  left_join(read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
            filter(iso %in% developing_group) %>%
            dplyr::select(c(cell_id, iso, year, predicted_GCP))) %>% 
  arrange(iso, cell_id, year) %>% 
  mutate(prev_year_pred = ifelse(year - 1 %in% year, predicted_GCP[match(year - 1, year)], NA),
        prev_year_true = ifelse(year - 1 %in% year, GCP_1deg[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_true)) %>% 
  filter(GCP_1deg != 0 & predicted_GCP != 0 & prev_year_pred != 0 & prev_year_true != 0) %>% 
  mutate(log_diff_true = log(GCP_1deg) - log(prev_year_true),
         log_diff_pred = log(predicted_GCP) - log(prev_year_pred)) %>% 
  group_by(iso) %>% 
  mutate(r2_chan = 1 - sum((log_diff_true - log_diff_pred)^2) / sum((log_diff_true - mean(log_diff_true))^2)) %>% 
  ungroup() %>%
  distinct(iso, r2_chan) %>% 
  as.data.frame()

# now organze it to present the table
results <- chec_level %>% 
  left_join(chec_change) %>% 
  mutate(r2_level = sprintf("%.2f%%", r2_level * 100), 
         GDP_loss = sprintf("%.2f%%", GDP_loss * 100), 
         r2_chan = sprintf("%.2f%%", r2_chan * 100)) %>% 
  arrange(iso)

table_latex <- results %>%
  kable(format = "latex", booktabs = TRUE, 
        col.names = c("ISO", "R² for log(GDP)", "GDP Loss", "R² for log(GDP in t) - log(GDP in t-1)"),
        caption = "1-degree Model Performance Metrics for Developing Group",
        linesep = "",
        align = "c") %>%
  kable_styling(latex_options = c(), table.envir = FALSE)

save_kable(table_latex, file = "step7_robust_analysis/model_wo_developing/outputs/developing_perfomance_1deg.tex")


# 0.5-degree model

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_0_5deg.csv") 
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_0_5deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_5deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_0_5deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_0_5deg.csv") 

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

truth <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  filter(iso %in% developing_group) %>% 
  dplyr::select(c(cell_id, subcell_id, iso, year, GCP_0_5deg))

chec_level <- truth %>%
  left_join(read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
            filter(iso %in% developing_group) %>%
            dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP))) %>% 
  filter(GCP_0_5deg != 0 & predicted_GCP != 0) %>% 
  group_by(iso) %>% 
  mutate(r2_level = 1 - sum((log(GCP_0_5deg) - log(predicted_GCP))^2) / sum((log(GCP_0_5deg) - mean(log(GCP_0_5deg)))^2),
         GDP_loss = sum(abs(GCP_0_5deg - predicted_GCP)) / (2*sum(GCP_0_5deg))) %>% 
  ungroup() %>% 
  distinct(iso, r2_level, GDP_loss)
  
chec_change <- truth %>%
  left_join(read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
            filter(iso %in% developing_group) %>%
            dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP))) %>% 
  arrange(iso, cell_id, subcell_id, year) %>% 
  mutate(prev_year_pred = ifelse(year - 1 %in% year, predicted_GCP[match(year - 1, year)], NA),
        prev_year_true = ifelse(year - 1 %in% year, GCP_0_5deg[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_true)) %>% 
  filter(GCP_0_5deg != 0 & predicted_GCP != 0 & prev_year_pred != 0 & prev_year_true != 0) %>% 
  mutate(log_diff_true = log(GCP_0_5deg) - log(prev_year_true),
         log_diff_pred = log(predicted_GCP) - log(prev_year_pred)) %>% 
  group_by(iso) %>% 
  mutate(r2_chan = 1 - sum((log_diff_true - log_diff_pred)^2) / sum((log_diff_true - mean(log_diff_true))^2)) %>% 
  ungroup() %>%
  distinct(iso, r2_chan) %>% 
  as.data.frame()

# now organze it to present the table
results <- chec_level %>% 
  left_join(chec_change) %>% 
  mutate(r2_level = sprintf("%.2f%%", r2_level * 100), 
         GDP_loss = sprintf("%.2f%%", GDP_loss * 100), 
         r2_chan = sprintf("%.2f%%", r2_chan * 100))%>% 
  arrange(iso)

table_latex <- results %>%
  kable(format = "latex", booktabs = TRUE, 
        col.names = c("ISO", "R² for log(GDP)", "GDP Loss", "R² for log(GDP in t) - log(GDP in t-1)"),
        caption = "0.5-degree Model Performance Metrics for Developing Group",
        linesep = "",
        align = "c") %>%
  kable_styling(latex_options = c("hold_position"))

save_kable(table_latex, file = "step7_robust_analysis/model_wo_developing/outputs/developing_perfomance_0_5deg.tex")

# 0.25-degree model

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_0_25deg.csv") 
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_0_25deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_25deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_0_25deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_0_25deg.csv") 

developing_group <- c("CHL","COL","IDN","KGZ","PER","PHL","ALB","BIH","BLR",
                      "MOZ","SRB","UZB","VNM","KEN","LKA","THA","ECU")

truth <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso)  %>% 
  filter(iso %in% developing_group) %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, GCP_0_25deg))

chec_level <- truth %>%
  left_join(read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
            filter(iso %in% developing_group) %>%
            dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP))) %>% 
  filter(GCP_0_25deg != 0 & predicted_GCP != 0) %>% 
  group_by(iso) %>% 
  mutate(r2_level = 1 - sum((log(GCP_0_25deg) - log(predicted_GCP))^2) / sum((log(GCP_0_25deg) - mean(log(GCP_0_25deg)))^2),
         GDP_loss = sum(abs(GCP_0_25deg - predicted_GCP)) / (2*sum(GCP_0_25deg))) %>% 
  ungroup() %>% 
  distinct(iso, r2_level, GDP_loss)
  
chec_change <- truth %>%
  left_join(read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
            filter(iso %in% developing_group) %>%
            dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP))) %>% 
  arrange(iso, cell_id, subcell_id, subcell_id_0_25, year) %>% 
  mutate(prev_year_pred = ifelse(year - 1 %in% year, predicted_GCP[match(year - 1, year)], NA),
        prev_year_true = ifelse(year - 1 %in% year, GCP_0_25deg[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_true)) %>% 
  filter(GCP_0_25deg != 0 & predicted_GCP != 0 & prev_year_pred != 0 & prev_year_true != 0) %>% 
  mutate(log_diff_true = log(GCP_0_25deg) - log(prev_year_true),
         log_diff_pred = log(predicted_GCP) - log(prev_year_pred)) %>% 
  group_by(iso) %>% 
  mutate(r2_chan = 1 - sum((log_diff_true - log_diff_pred)^2) / sum((log_diff_true - mean(log_diff_true))^2)) %>% 
  ungroup() %>%
  distinct(iso, r2_chan) %>% 
  as.data.frame()

# now organze it to present the table
results <- chec_level %>% 
  left_join(chec_change) %>% 
  mutate(r2_level = sprintf("%.2f%%", r2_level * 100), 
         GDP_loss = sprintf("%.2f%%", GDP_loss * 100), 
         r2_chan = sprintf("%.2f%%", r2_chan * 100))%>% 
  arrange(iso)

table_latex <- results %>%
  kable(format = "latex", booktabs = TRUE, 
        col.names = c("ISO", "R² for log(GDP)", "GDP Loss", "R² for log(GDP in t) - log(GDP in t-1)"),
        caption = "0.5-degree Model Performance Metrics for Developing Group",
        linesep = "",
        align = "c") %>%
  kable_styling(latex_options = c("hold_position"))

save_kable(table_latex, file = "step7_robust_analysis/model_wo_developing/outputs/developing_perfomance_0_25deg.tex")


# I want the same tests for comparison

# first test: china test
# read the true cell GDP
load("step6_shocks_log_change/outputs/CHN_test/chn_1deg_cell_GCP.RData")
china_truth <- chn_1deg_cell_GCP  %>% 
    dplyr::select(c(cell_id, prov_id, iso, year, GCP_1deg))  %>% 
    group_by(cell_id, prov_id, year)  %>% 
    mutate(GCP_1deg = sum(GCP_1deg))  %>% 
    ungroup()  %>% 
    distinct(cell_id, prov_id, year, GCP_1deg)

# read the predicted GDP
load("step7_robust_analysis/model_wo_developing/outputs/pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust.RData")
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

# Create the combined plot with facet_wrap, using nrow and ncol for layout
# title = "True vs Predicted GDP in China's Seven Leading Provinces",
# subtitle = "1-dgeree cells; Unit: billion const 2017 USD",
       
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

ggsave("step7_robust_analysis/model_wo_developing/outputs/log_level_change_r2.png", plot = combined_plot, width = 18, height = 12, bg = "white")


# Second test: China test but use model trained on year 2012-2019
# read the true cell GDP
load("step6_shocks_log_change/outputs/CHN_test/chn_1deg_cell_GCP.RData")
china_truth <- chn_1deg_cell_GCP  %>% 
    dplyr::select(c(cell_id, prov_id, iso, year, GCP_1deg))  %>% 
    group_by(cell_id, prov_id, year)  %>% 
    mutate(GCP_1deg = sum(GCP_1deg))  %>% 
    ungroup()  %>% 
    distinct(cell_id, prov_id, year, GCP_1deg)

# read the predicted GDP
load("step7_robust_analysis/model_wo_developing/outputs/pred_1deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust_upto_2019.RData")
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
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

ggsave("step7_robust_analysis/model_wo_developing/outputs/log_level_change_r2_use_model_upto_2019.png", plot = combined_plot, width = 18, height = 12, bg = "white")

# third test: training countries predict year 2020-2021 using model trained on 2012 to 2019
# only did it for 1deg 
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
load("step7_robust_analysis/model_wo_developing/outputs/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_up_to_2019.RData")
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

ggsave("step7_robust_analysis/model_wo_developing/outputs/log_level_change_r2_training_all.png", plot = combined_plot, width = 18, height = 12, bg = "white")


# ----------------------------------------------------------------------------------------------------------------------------------------------------
# I also want to do the pop vs GDP test

# 1deg
pred_1deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_1deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(c(cell_id, iso, year, predicted_GCP, pop_cell)) %>% 
  mutate(log_GCP = log(predicted_GCP),
        log_pop = log(pop_cell/1000000)) %>%
  mutate(inf = ifelse(is.infinite(log_GCP) | is.infinite(log_pop), 1, 0)) %>% 
  filter(inf != 1) %>%
  mutate(adjusted_log_GCP = log_GCP - log_pop)

# 0.5deg
pred_0_5deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP, pop_cell)) %>% 
  mutate(log_GCP = log(predicted_GCP),
        log_pop = log(pop_cell/1000000)) %>%
  mutate(inf = ifelse(is.infinite(log_GCP) | is.infinite(log_pop), 1, 0)) %>% 
  filter(inf != 1) %>%
  mutate(adjusted_log_GCP = log_GCP - log_pop)

# 0.25deg
pred_0_25deg <- read.csv("step7_robust_analysis/model_wo_developing/outputs/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_wo_devlping.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP, pop_cell)) %>% 
  mutate(log_GCP = log(predicted_GCP),
        log_pop = log(pop_cell/1000000)) %>%
  mutate(inf = ifelse(is.infinite(log_GCP) | is.infinite(log_pop), 1, 0)) %>% 
  filter(inf != 1) %>%
  mutate(adjusted_log_GCP = log_GCP - log_pop)

# fit the polynomial curve for all predictions
x_limits <- range(c(pred_1deg$log_pop, pred_0_5deg$log_pop, pred_0_25deg$log_pop), na.rm = TRUE)
y_limits <- range(c(pred_1deg$log_GCP, pred_0_5deg$log_GCP, pred_0_25deg$log_GCP), na.rm = TRUE)

format_equation <- function(model, degree) {
  coef_vals <- coef(model)
  
  rounded_coefs <- c(
    round(coef_vals[1], 2),
    round(coef_vals[2], 2),
    round(coef_vals[3], 2),
    round(coef_vals[4], 3),
    round(coef_vals[5], 4)
  )
  
  terms <- paste0(rounded_coefs[-1], " * x^", 1:degree)
  equation <- paste("y = ", rounded_coefs[1], " + ", paste(terms, collapse = " + "))
  
  return(equation)
}

format_red_line_equation <- function(intercept) {
  equation <- paste("y = x +", round(intercept, 2))
  return(equation)
}

# 1deg
poly0 <- lm_robust(adjusted_log_GCP ~ 1, data = pred_1deg, se_type = "HC1")

for (i in c(4)){
  
  model <- lm_robust(log_GCP ~ 1 + poly(log_pop, degree = i, raw = TRUE), data = pred_1deg, se_type = "HC1")

  pred_with_ci <- predict(model, newdata = pred_1deg, se.fit = TRUE, interval = "confidence", level = 0.95)
  
  # Extract fitted values and confidence intervals
  pred_1deg$fitted <- pred_with_ci$fit[, "fit"]
  pred_1deg$lower <- pred_with_ci$fit[, "lwr"]
  pred_1deg$upper <- pred_with_ci$fit[, "upr"]

  # Define column names for easier plotting
  fit_col <- paste0("fitted_GCP_share", i)
  lower_col <- paste0("ci_lower", i)
  upper_col <- paste0("ci_upper", i)

  # Create the plot
  p1 <- ggplot(pred_1deg, aes(x = log_pop, y = log_GCP)) +
    geom_point(size = 0.1, color = "#858585") +
    geom_line(aes(y = fitted), color = "blue", size = 1) +
    geom_abline(intercept = coef(poly0)[1], slope = 1, linetype = "dashed", color = "#BE1508", size = 1) +
    labs(subtitle = paste("1deg cell predictions: all years"),
         x = "log Pop Count (million)",
         y = "log GDP (const2017 billion USD)") +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits) +         
    theme_bw(base_size = 14) +
    theme(
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.spacing = unit(0.5, "null")
    ) +
    coord_fixed() +
    theme(aspect.ratio = 1)
    # annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+1.5, label = format_equation(model, 4), hjust = 0, size = 3, color = "blue") +
    # annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+0.05, label = format_red_line_equation(coef(poly0)[1]), hjust = 0, size = 3, color = "red")

    print(format_equation(model, 4))
    print(format_red_line_equation(coef(poly0)[1]))
}

# 0.5deg
poly0 <- lm_robust(adjusted_log_GCP ~ 1, data = pred_0_5deg, se_type = "HC1")

for (i in c(4)){
  
  model <- lm_robust(log_GCP ~ 1 + poly(log_pop, degree = i, raw = TRUE), data = pred_0_5deg, se_type = "HC1")

  pred_with_ci <- predict(model, newdata = pred_0_5deg, se.fit = TRUE, interval = "confidence", level = 0.95)
  
  # Extract fitted values and confidence intervals
  pred_0_5deg$fitted <- pred_with_ci$fit[, "fit"]
  pred_0_5deg$lower <- pred_with_ci$fit[, "lwr"]
  pred_0_5deg$upper <- pred_with_ci$fit[, "upr"]

  # Define column names for easier plotting
  fit_col <- paste0("fitted_GCP_share", i)
  lower_col <- paste0("ci_lower", i)
  upper_col <- paste0("ci_upper", i)

  # Create the plot
  p2 <- ggplot(pred_0_5deg, aes(x = log_pop, y = log_GCP)) +
    geom_point(size = 0.1, color = "#858585") +
    geom_line(aes(y = fitted), color = "blue", size = 1) +
    geom_abline(intercept = coef(poly0)[1], slope = 1, linetype = "dashed", color = "#BE1508", size = 1) +
    labs(subtitle = paste("0.5deg cell predictions: all years"),
         x = "log Pop Count (million)",
         y = "log GDP (const2017 billion USD)") +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits) +         
    theme_bw(base_size = 14) +
    theme(
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.spacing = unit(0.5, "null")
    ) +
    coord_fixed() +
    theme(aspect.ratio = 1)
    # annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+1.5, label = format_equation(model, 4), hjust = 0, size = 3, color = "blue") +
    # annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+0.05, label = format_red_line_equation(coef(poly0)[1]), hjust = 0, size = 3, color = "red")

    print(format_equation(model, 4))
    print(format_red_line_equation(coef(poly0)[1]))
}

# 0.25deg
poly0 <- lm_robust(adjusted_log_GCP ~ 1, data = pred_0_25deg, se_type = "HC1")

for (i in c(4)){
  
  model <- lm_robust(log_GCP ~ 1 + poly(log_pop, degree = i, raw = TRUE), data = pred_0_25deg, se_type = "HC1")

  pred_with_ci <- predict(model, newdata = pred_0_25deg, se.fit = TRUE, interval = "confidence", level = 0.95)
  
  # Extract fitted values and confidence intervals
  pred_0_25deg$fitted <- pred_with_ci$fit[, "fit"]
  pred_0_25deg$lower <- pred_with_ci$fit[, "lwr"]
  pred_0_25deg$upper <- pred_with_ci$fit[, "upr"]

  # Define column names for easier plotting
  fit_col <- paste0("fitted_GCP_share", i)
  lower_col <- paste0("ci_lower", i)
  upper_col <- paste0("ci_upper", i)

  # Create the plot
  p3 <- ggplot(pred_0_25deg, aes(x = log_pop, y = log_GCP)) +
    geom_point(size = 0.1, color = "#858585") +
    geom_line(aes(y = fitted), color = "blue", size = 1) +
    geom_abline(intercept = coef(poly0)[1], slope = 1, linetype = "dashed", color = "#BE1508", size = 1) +
    labs(subtitle = paste("0.25deg cell predictions: all years"),
         x = "log Pop Count (million)",
         y = "log GDP (const2017 billion USD)") +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits) +  
    theme_bw(base_size = 14) +           
    theme(
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.spacing = unit(0.5, "null")
    ) +
    coord_fixed() +
    theme(aspect.ratio = 1)
    # annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+1.5, label = format_equation(model, 4), hjust = 0, size = 3, color = "blue") +
    # annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+0.05, label = format_red_line_equation(coef(poly0)[1]), hjust = 0, size = 3, color = "red")

    print(format_equation(model, 4))
    print(format_red_line_equation(coef(poly0)[1]))
}

final_plot_with_title <- grid.arrange(
  top = textGrob("", gp = gpar(fontsize = 15, fontface = "bold")),
  arrangeGrob(p1, p2, p3, ncol = 3, 
  nrow = 1))

ggsave("step7_robust_analysis/model_wo_developing/outputs/not_just_pop_distr.png", 
       plot = final_plot_with_title,
       bg = "white", width = 17, height = 6)
