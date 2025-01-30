# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to compare the results of different models to show the consistency
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

# --------------------------------------------------------------------------------------------------------------------------------
# Model 9.1: train and predict at 1-degree level: trained on all years (2012-2021)
# Model 9.2: train and predict at 0.5-degree level: trained on all years (2012-2021)
# Model 9.3: train and predict at 0.25-degree level: trained on all years (2012-2021)

# Model 9.5: train and predict at 1-degree level: trained on years 2012-2019
# Model 9.6: train and predict at 0.5-degree level: trained on years 2012-2019
# Model 9.7: train and predict at 0.25-degree level: trained on years 2012-2019

# --------------------------------------------------------------------------------------------------------------------------------
# Now compare 45deg-line of Model 9.1 and Model 9.2 Results aggregated at 1-degree level

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_from_0_5deg_without_prov_boundary.RData")

model_9_1 <- predict_data_results_1deg_without_prov_boundary  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model91 = log(pred_GCP_1deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame() %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

model_9_2_1deg <- predict_data_results_1deg_from_0_5deg_without_prov_boundary  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model92 = log(pred_GCP_1deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) 

df <- model_9_1  %>% 
      left_join(model_9_2_1deg) %>% 
      filter(!is.infinite(pred_GCP_1deg_no_prov_bound_model91) & !is.nan(pred_GCP_1deg_no_prov_bound_model91) &
             !is.infinite(pred_GCP_1deg_no_prov_bound_model92) & !is.nan(pred_GCP_1deg_no_prov_bound_model92))  # because some GCP == 0

# define a function to calculate R^2
calculate_r_squared <- function(data_subset) {
  r_squared_linear = 1 - (sum((data_subset$pred_GCP_1deg_no_prov_bound_model91 - data_subset$pred_GCP_1deg_no_prov_bound_model92)^2) / 
                          sum((data_subset$pred_GCP_1deg_no_prov_bound_model91 - mean(data_subset$pred_GCP_1deg_no_prov_bound_model91))^2))
  return(list(linear = r_squared_linear))
}

r_squared_results_full <- calculate_r_squared(df)

p1 <- ggplot(df, aes(x = pred_GCP_1deg_no_prov_bound_model91, y = pred_GCP_1deg_no_prov_bound_model92)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", r_squared_results_full[1]), 
              hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "At 1-degree Resolution \nAll Years Data Included", 
        x = "log(pred GDP) \nfrom 1deg model", # Unit: billion const 2017 USD
        y = "log(pred GDP) aggregated from \n0.5deg model predictions")   +
  theme_minimal() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    plot.margin = margin(0, 26, 0, 0)
  )  

# -------------
# Now compare 45deg-line of Model 9.1 and Model 9.3 Results aggregated at 1-degree level

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_from_0_25deg_without_prov_boundary.RData")

model_9_1 <- predict_data_results_1deg_without_prov_boundary  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model91 = log(pred_GCP_1deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

model_9_3_1deg <- predict_data_results_1deg_from_0_25deg_without_prov_boundary  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model93 = log(pred_GCP_1deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

df <- model_9_1  %>% 
      left_join(model_9_3_1deg) %>% 
      filter(!is.infinite(pred_GCP_1deg_no_prov_bound_model91) & !is.nan(pred_GCP_1deg_no_prov_bound_model91) &
             !is.infinite(pred_GCP_1deg_no_prov_bound_model93) & !is.nan(pred_GCP_1deg_no_prov_bound_model93))  # because some GCP == 0

# define a function to calculate R^2
calculate_r_squared <- function(data_subset) {
  r_squared_linear = 1 - (sum((data_subset$pred_GCP_1deg_no_prov_bound_model91 - data_subset$pred_GCP_1deg_no_prov_bound_model93)^2) / 
                          sum((data_subset$pred_GCP_1deg_no_prov_bound_model91 - mean(data_subset$pred_GCP_1deg_no_prov_bound_model91))^2))
  return(list(linear = r_squared_linear))
}

r_squared_results_full <- calculate_r_squared(df)

p2 <- ggplot(df, aes(x=pred_GCP_1deg_no_prov_bound_model91, y = pred_GCP_1deg_no_prov_bound_model93)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", r_squared_results_full[1]), 
              hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "At 1-degree Resolution \nAll Years Data Included", 
        x = "log(pred GDP) \nfrom 1deg model", # Unit: billion const 2017 USD
        y = "log(pred GDP) aggregated from \n0.25deg model predictions")   +
  theme_minimal() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    plot.margin = margin(0, 13, 0, 13)
  ) 

# -------------
# Now compare 45deg-line of Model 9.2 and Model 9.3 Results aggregated at 0.5-degree level

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_from_0_25deg_without_prov_boundary.RData")

model_9_2_0_5deg <- predict_data_results_0_5deg_without_prov_boundary  %>% 
             mutate(pred_GCP_0_5deg_no_prov_bound_model92 = log(pred_GCP_0_5deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_0_5deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

model_9_3_0_5deg <- predict_data_results_0_5deg_from_0_25deg_without_prov_boundary  %>% 
             mutate(pred_GCP_0_5deg_no_prov_bound_model93 = log(pred_GCP_0_5deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_0_5deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

df <- model_9_2_0_5deg  %>% 
      left_join(model_9_3_0_5deg) %>% 
      filter(!is.infinite(pred_GCP_0_5deg_no_prov_bound_model92) & !is.nan(pred_GCP_0_5deg_no_prov_bound_model92) &
             !is.infinite(pred_GCP_0_5deg_no_prov_bound_model93) & !is.nan(pred_GCP_0_5deg_no_prov_bound_model93)) # because some GCP == 0 

# define a function to calculate R^2
calculate_r_squared <- function(data_subset) {
  r_squared_linear = 1 - (sum((data_subset$pred_GCP_0_5deg_no_prov_bound_model92 - data_subset$pred_GCP_0_5deg_no_prov_bound_model93)^2) / 
                          sum((data_subset$pred_GCP_0_5deg_no_prov_bound_model92 - mean(data_subset$pred_GCP_0_5deg_no_prov_bound_model92))^2))
  return(list(linear = r_squared_linear))
}

r_squared_results_full <- calculate_r_squared(df)

p3 <- ggplot(df, aes(x=pred_GCP_0_5deg_no_prov_bound_model92, y = pred_GCP_0_5deg_no_prov_bound_model93)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", r_squared_results_full[1]), 
              hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "At 0.5-degree Resolution \nAll Years Data Included", 
        x = "log(pred GDP) \nfrom 0.5deg model", # Unit: billion const 2017 USD
        y = "log(pred GDP) aggregated from \n0.25deg model predictions")   +
  theme_minimal() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5), 
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    plot.margin = margin(0, 0, 0, 26)
  )  

combined_plot <- plot_grid(p1, p2, p3, ncol = 3)
ggsave("step5_predict_and_post_adjustments_log_change/outputs/analyze_diff_models/compare_gcp_vs_aggre_no_prov_bound_all_deg.png", plot = combined_plot, width = 19, height = 6, bg = "white")



# --------------------------------------------------------------------------------------------------------------------------------
# Now compare 45deg-line of Model 9.1 and Model 9.5: only year 2020 and 2021's predicted results

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_without_prov_boundary_model_up_to_2019.RData")

model_9_1 <- predict_data_results_1deg_without_prov_boundary  %>% 
             filter(year %in% c(2020,2021))  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model91 = log(pred_GCP_1deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) 

model_9_5 <- predict_data_results_1deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2020,2021))  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model95 = log(pred_GCP_1deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) 

df <- model_9_1  %>% 
      left_join(model_9_5) %>% 
      filter(!is.infinite(pred_GCP_1deg_no_prov_bound_model91) & !is.nan(pred_GCP_1deg_no_prov_bound_model91) &
             !is.infinite(pred_GCP_1deg_no_prov_bound_model95) & !is.nan(pred_GCP_1deg_no_prov_bound_model95)) # because some GCP == 0   

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$pred_GCP_1deg_no_prov_bound_model91 - df$pred_GCP_1deg_no_prov_bound_model95)^2) / 
                        sum((df$pred_GCP_1deg_no_prov_bound_model91 - mean(df$pred_GCP_1deg_no_prov_bound_model91))^2))

p1 <- ggplot(df, aes(x=pred_GCP_1deg_no_prov_bound_model91, y = pred_GCP_1deg_no_prov_bound_model95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "1-degree Model",
       x = "log(pred GDP) for 2020 and 2021 \nfrom model trained on 2012 to 2021 data", 
       y = "log(pred GDP) for 2020 and 2021 \nfrom model trained on 2012 to 2019 data")   + # Unit: billion const 2017 USD
  theme_minimal() +
  theme_bw() +
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

# Now compare 45deg-line of Model 9.1 and Model 9.5: only year 2018 and 2019's predicted results

model_9_1 <- predict_data_results_1deg_without_prov_boundary  %>% 
             filter(year %in% c(2018,2019))  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model91 = log(pred_GCP_1deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) 

model_9_5 <- predict_data_results_1deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2018,2019))  %>% 
             mutate(pred_GCP_1deg_no_prov_bound_model95 = log(pred_GCP_1deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_1deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) 

df <- model_9_1  %>% 
      left_join(model_9_5) %>% 
      filter(!is.infinite(pred_GCP_1deg_no_prov_bound_model91) & !is.nan(pred_GCP_1deg_no_prov_bound_model91) &
             !is.infinite(pred_GCP_1deg_no_prov_bound_model95) & !is.nan(pred_GCP_1deg_no_prov_bound_model95)) # because some GCP == 0   

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$pred_GCP_1deg_no_prov_bound_model91 - df$pred_GCP_1deg_no_prov_bound_model95)^2) / 
                        sum((df$pred_GCP_1deg_no_prov_bound_model91 - mean(df$pred_GCP_1deg_no_prov_bound_model91))^2))

p2 <- ggplot(df, aes(x=pred_GCP_1deg_no_prov_bound_model91, y = pred_GCP_1deg_no_prov_bound_model95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "1-degree Model",
       x = "log(pred GDP) for 2018 and 2019 \nfrom model trained on 2012 to 2021 data", 
       y = "log(pred GDP) for 2018 and 2019 \nfrom model trained on 2012 to 2019 data")   + # Unit: billion const 2017 USD
  theme_minimal() +
  theme_bw() +
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

# -------------
# Now compare 45deg-line of Model 9.2 and Model 9.6: only year 2020 and 2021's predicted results

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019.RData")

model_9_2 <- predict_data_results_0_5deg_without_prov_boundary  %>% 
             filter(year %in% c(2020,2021))  %>% 
             mutate(pred_GCP_0_5deg_no_prov_bound_model92 = log(pred_GCP_0_5deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_0_5deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

model_9_6 <- predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2020,2021))  %>% 
             mutate(pred_GCP_0_5deg_no_prov_bound_model96 = log(pred_GCP_0_5deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_0_5deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

df <- model_9_2  %>% 
      left_join(model_9_6) %>% 
      filter(!is.infinite(pred_GCP_0_5deg_no_prov_bound_model92) & !is.nan(pred_GCP_0_5deg_no_prov_bound_model92) &
             !is.infinite(pred_GCP_0_5deg_no_prov_bound_model96) & !is.nan(pred_GCP_0_5deg_no_prov_bound_model96)) # because some GCP == 0   

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$pred_GCP_0_5deg_no_prov_bound_model92 - df$pred_GCP_0_5deg_no_prov_bound_model96)^2) / 
                        sum((df$pred_GCP_0_5deg_no_prov_bound_model92 - mean(df$pred_GCP_0_5deg_no_prov_bound_model92))^2))

p3 <- ggplot(df, aes(x=pred_GCP_0_5deg_no_prov_bound_model92, y = pred_GCP_0_5deg_no_prov_bound_model96)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.5-degree Model",
       x = "log(pred GDP) for 2020 and 2021 \nfrom model trained on 2012 to 2021 data", 
       y = "log(pred GDP) for 2020 and 2021 \nfrom model trained on 2012 to 2019 data")   + # Unit: billion const 2017 USD
  theme_minimal() +
  theme_bw() +
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

# -------------
# Now compare 45deg-line of Model 9.2 and Model 9.6: only year 2018 and 2019's predicted results

model_9_2 <- predict_data_results_0_5deg_without_prov_boundary  %>% 
             filter(year %in% c(2018,2019))  %>% 
             mutate(pred_GCP_0_5deg_no_prov_bound_model92 = log(pred_GCP_0_5deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_0_5deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

model_9_6 <- predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2018,2019))  %>% 
             mutate(pred_GCP_0_5deg_no_prov_bound_model96 = log(pred_GCP_0_5deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_0_5deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

df <- model_9_2  %>% 
      left_join(model_9_6) %>% 
      filter(!is.infinite(pred_GCP_0_5deg_no_prov_bound_model92) & !is.nan(pred_GCP_0_5deg_no_prov_bound_model92) &
             !is.infinite(pred_GCP_0_5deg_no_prov_bound_model96) & !is.nan(pred_GCP_0_5deg_no_prov_bound_model96)) # because some GCP == 0   

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$pred_GCP_0_5deg_no_prov_bound_model92 - df$pred_GCP_0_5deg_no_prov_bound_model96)^2) / 
                        sum((df$pred_GCP_0_5deg_no_prov_bound_model92 - mean(df$pred_GCP_0_5deg_no_prov_bound_model92))^2))

p4 <- ggplot(df, aes(x=pred_GCP_0_5deg_no_prov_bound_model92, y = pred_GCP_0_5deg_no_prov_bound_model96)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.5-degree Model",
       x = "log(pred GDP) for 2018 and 2019 \nfrom model trained on 2012 to 2021 data", 
       y = "log(pred GDP) for 2018 and 2019 \nfrom model trained on 2012 to 2019 data")   + # Unit: billion const 2017 USD
  theme_minimal() +
  theme_bw() +
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


# -------------
# Now compare 45deg-line of Model 9.3 and Model 9.7: only year 2020 and 2021's predicted results

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_25deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019.RData")

model_9_3 <- predict_data_results_0_25deg_without_prov_boundary  %>% 
             filter(year %in% c(2020,2021))  %>% 
             mutate(pred_GCP_0_25deg_no_prov_bound_model93 = log(pred_GCP_0_25deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_0_25deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

model_9_7 <- predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2020,2021))  %>% 
             mutate(pred_GCP_0_25deg_no_prov_bound_model97 = log(pred_GCP_0_25deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_0_25deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

df <- model_9_3  %>% 
      left_join(model_9_7) %>% 
      filter(!is.infinite(pred_GCP_0_25deg_no_prov_bound_model93) & !is.nan(pred_GCP_0_25deg_no_prov_bound_model93) &
             !is.infinite(pred_GCP_0_25deg_no_prov_bound_model97) & !is.nan(pred_GCP_0_25deg_no_prov_bound_model97)) # because some GCP == 0   

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$pred_GCP_0_25deg_no_prov_bound_model93 - df$pred_GCP_0_25deg_no_prov_bound_model97)^2) / 
                        sum((df$pred_GCP_0_25deg_no_prov_bound_model93 - mean(df$pred_GCP_0_25deg_no_prov_bound_model93))^2))

p5 <- ggplot(df, aes(x=pred_GCP_0_25deg_no_prov_bound_model93, y = pred_GCP_0_25deg_no_prov_bound_model97)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.25-degree Model",
       x = "log(pred GDP) for 2020 and 2021 \nfrom model trained on 2012 to 2021 data", 
       y = "log(pred GDP) for 2020 and 2021 \nfrom model trained on 2012 to 2019 data")   + # Unit: billion const 2017 USD
  theme_minimal() +
  theme_bw() +
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

# -------------
# Now compare 45deg-line of Model 9.3 and Model 9.7: only year 2020 and 2021's predicted results

model_9_3 <- predict_data_results_0_25deg_without_prov_boundary  %>% 
             filter(year %in% c(2018,2019))  %>% 
             mutate(pred_GCP_0_25deg_no_prov_bound_model93 = log(pred_GCP_0_25deg_no_prov_bound))  %>% 
             dplyr::select(-c("geom", "pred_GCP_0_25deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

model_9_7 <- predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2018,2019))  %>% 
             mutate(pred_GCP_0_25deg_no_prov_bound_model97 = log(pred_GCP_0_25deg_no_prov_bound)) %>% 
             dplyr::select(-c("geom", "pred_GCP_0_25deg_no_prov_bound"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE)

df <- model_9_3  %>% 
      left_join(model_9_7) %>% 
      filter(!is.infinite(pred_GCP_0_25deg_no_prov_bound_model93) & !is.nan(pred_GCP_0_25deg_no_prov_bound_model93) &
             !is.infinite(pred_GCP_0_25deg_no_prov_bound_model97) & !is.nan(pred_GCP_0_25deg_no_prov_bound_model97)) # because some GCP == 0   

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$pred_GCP_0_25deg_no_prov_bound_model93 - df$pred_GCP_0_25deg_no_prov_bound_model97)^2) / 
                        sum((df$pred_GCP_0_25deg_no_prov_bound_model93 - mean(df$pred_GCP_0_25deg_no_prov_bound_model93))^2))

p6 <- ggplot(df, aes(x=pred_GCP_0_25deg_no_prov_bound_model93, y = pred_GCP_0_25deg_no_prov_bound_model97)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.25-degree Model",
       x = "log(pred GDP) for 2018 and 2019 \nfrom model trained on 2012 to 2021 data", 
       y = "log(pred GDP) for 2018 and 2019 \nfrom model trained on 2012 to 2019 data")   + # Unit: billion const 2017 USD
  theme_minimal() +
  theme_bw() +
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
ggsave("step5_predict_and_post_adjustments_log_change/outputs/analyze_diff_models/compare_gcp_trained_w_wo_2020_2021_all_deg.png", plot = combined_plot, width = 22, height = 14, bg = "white")

