# --------------------------------- Task Summary --------------------------------- #
# This file continues the comparison between models trained using all years and 
#   those trained using data from 2012 to 2019.
# The focus of this comparison is on GDP growth, rather than GDP levels.
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
library(cowplot)

# -------------------
# 1deg

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_1deg_without_prov_boundary_model_up_to_2019.RData")

model_9_1 <- predict_data_results_1deg_without_prov_boundary  %>% 
             filter(year %in% c(2020,2021)) %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, iso) %>% 
             mutate(growth_91 = log(pred_GCP_1deg_no_prov_bound[year == 2021]) - log(pred_GCP_1deg_no_prov_bound[year == 2020])) %>% 
             ungroup() %>% 
             distinct(cell_id, iso, growth_91) 

model_9_5 <- predict_data_results_1deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2020,2021))  %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, iso) %>% 
             mutate(growth_95 = log(pred_GCP_1deg_no_prov_bound[year == 2021]) - log(pred_GCP_1deg_no_prov_bound[year == 2020])) %>% 
             ungroup() %>% 
             distinct(cell_id, iso, growth_95)

df <- model_9_1  %>% 
      left_join(model_9_5)%>% 
      filter(!is.infinite(growth_91) & !is.nan(growth_91) &
             !is.infinite(growth_95) & !is.nan(growth_95)) # because some GCP == 0


# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$growth_91 - df$growth_95)^2) / 
                        sum((df$growth_91 - mean(df$growth_91))^2))
calculate_r_squared

p1 <- ggplot(df, aes(x=growth_91, y = growth_95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "1-degree Model Predictions",
       x = "log(GDP in 2021) - log(GDP in 2020) \nfrom model trained on 2012 to 2021 data", 
       y = "log(GDP in 2021) - log(GDP in 2020) \nfrom model trained on 2012 to 2019 data")   +
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

# now see within sample training years: 2018 to 2019
model_9_1 <- predict_data_results_1deg_without_prov_boundary  %>% 
             filter(year %in% c(2019,2018)) %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, iso) %>% 
             mutate(growth_91 = log(pred_GCP_1deg_no_prov_bound[year == 2019]) - log(pred_GCP_1deg_no_prov_bound[year == 2018])) %>% 
             ungroup() %>% 
             distinct(cell_id, iso, growth_91)

model_9_5 <- predict_data_results_1deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2019,2018))  %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, iso) %>% 
             mutate(growth_95 = log(pred_GCP_1deg_no_prov_bound[year == 2019]) - log(pred_GCP_1deg_no_prov_bound[year == 2018])) %>% 
             ungroup() %>% 
             distinct(cell_id, iso, growth_95)

df <- model_9_1  %>% 
      left_join(model_9_5) %>% 
      filter(!is.infinite(growth_91) & !is.nan(growth_91) &
             !is.infinite(growth_95) & !is.nan(growth_95)) # because some GCP == 0

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$growth_91 - df$growth_95)^2) / 
                        sum((df$growth_91 - mean(df$growth_91))^2))
calculate_r_squared

p2 <- ggplot(df, aes(x=growth_91, y = growth_95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "1-degree Model Predictions",
       x = "log(GDP in 2019) - log(GDP in 2018) \nfrom model trained on 2012 to 2021 data", 
       y = "log(GDP in 2019) - log(GDP in 2018) \nfrom model trained on 2012 to 2019 data")   +
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
    plot.margin = margin(20, 26, 0, 0)
  )    

# -------------------
# 0.5deg

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019.RData")

model_9_1 <- predict_data_results_0_5deg_without_prov_boundary  %>% 
             filter(year %in% c(2020,2021)) %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, iso) %>% 
             mutate(growth_91 = log(pred_GCP_0_5deg_no_prov_bound[year == 2021]) - log(pred_GCP_0_5deg_no_prov_bound[year == 2020])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, iso, growth_91)

model_9_5 <- predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2020,2021))  %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, iso) %>% 
             mutate(growth_95 = log(pred_GCP_0_5deg_no_prov_bound[year == 2021]) - log(pred_GCP_0_5deg_no_prov_bound[year == 2020])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, iso, growth_95)

df <- model_9_1  %>% 
      left_join(model_9_5) %>% 
      filter(!is.infinite(growth_91) & !is.nan(growth_91) & 
             !is.infinite(growth_95) & !is.nan(growth_95)) # because some GCP == 0 

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$growth_91 - df$growth_95)^2) / 
                        sum((df$growth_91 - mean(df$growth_91))^2))
calculate_r_squared

p3 <- ggplot(df, aes(x=growth_91, y = growth_95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.5-degree Model Predictions",
       x = "log(GDP in 2021) - log(GDP in 2020) \nfrom model trained on 2012 to 2021 data", 
       y = "log(GDP in 2021) - log(GDP in 2020) \nfrom model trained on 2012 to 2019 data")   +
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


# now see within sample training years: 2018 to 2019
model_9_1 <- predict_data_results_0_5deg_without_prov_boundary  %>% 
             filter(year %in% c(2019,2018)) %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, iso) %>% 
             mutate(growth_91 = log(pred_GCP_0_5deg_no_prov_bound[year == 2019]) - log(pred_GCP_0_5deg_no_prov_bound[year == 2018])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, iso, growth_91)

model_9_5 <- predict_data_results_0_5deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2019,2018))  %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, iso) %>% 
             mutate(growth_95 = log(pred_GCP_0_5deg_no_prov_bound[year == 2019]) - log(pred_GCP_0_5deg_no_prov_bound[year == 2018])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, iso, growth_95)

df <- model_9_1  %>% 
      left_join(model_9_5) %>% 
      filter(!is.infinite(growth_91) & !is.nan(growth_91) &
             !is.infinite(growth_95) & !is.nan(growth_95)) # because some GCP == 0

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$growth_91 - df$growth_95)^2) / 
                        sum((df$growth_91 - mean(df$growth_91))^2))
calculate_r_squared

p4 <- ggplot(df, aes(x=growth_91, y = growth_95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.5-degree Model Predictions",
       x = "log(GDP in 2019) - log(GDP in 2018) \nfrom model trained on 2012 to 2021 data", 
       y = "log(GDP in 2019) - log(GDP in 2018) \nfrom model trained on 2012 to 2019 data")   +
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

# -------------------
# 0.25deg

load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_25deg_without_prov_boundary.RData")
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019.RData")

model_9_1 <- predict_data_results_0_25deg_without_prov_boundary  %>% 
             filter(year %in% c(2020,2021)) %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, subcell_id_0_25, iso) %>% 
             mutate(growth_91 = log(pred_GCP_0_25deg_no_prov_bound[year == 2021]) - log(pred_GCP_0_25deg_no_prov_bound[year == 2020])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, growth_91)

model_9_5 <- predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2020,2021))  %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, subcell_id_0_25, iso) %>% 
             mutate(growth_95 = log(pred_GCP_0_25deg_no_prov_bound[year == 2021]) - log(pred_GCP_0_25deg_no_prov_bound[year == 2020])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, growth_95)

df <- model_9_1  %>% 
      left_join(model_9_5) %>% 
      filter(!is.infinite(growth_91) & !is.nan(growth_91) &
             !is.infinite(growth_95) & !is.nan(growth_95)) # because some GCP == 0

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$growth_91 - df$growth_95)^2) / 
                        sum((df$growth_91 - mean(df$growth_91))^2))
calculate_r_squared

p5 <- ggplot(df, aes(x=growth_91, y = growth_95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.25-degree Model Predictions",
       x = "log(GDP in 2021) - log(GDP in 2020) \nfrom model trained on 2012 to 2021 data", 
       y = "log(GDP in 2021) - log(GDP in 2020) \nfrom model trained on 2012 to 2019 data")   +
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

# now see within sample training years: 2018 to 2019
model_9_1 <- predict_data_results_0_25deg_without_prov_boundary  %>% 
             filter(year %in% c(2019,2018)) %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, subcell_id_0_25, iso) %>% 
             mutate(growth_91 = log(pred_GCP_0_25deg_no_prov_bound[year == 2019]) - log(pred_GCP_0_25deg_no_prov_bound[year == 2018])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, growth_91)

model_9_5 <- predict_data_results_0_25deg_without_prov_boundary_model_up_to_2019  %>% 
             filter(year %in% c(2019,2018))  %>% 
             dplyr::select(-c("geom", "country_total_GDP", "national_population"))  %>% 
             as.data.frame()  %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, year, .keep_all = TRUE) %>%
             group_by(cell_id, subcell_id, subcell_id_0_25, iso) %>% 
             mutate(growth_95 = log(pred_GCP_0_25deg_no_prov_bound[year == 2019]) - log(pred_GCP_0_25deg_no_prov_bound[year == 2018])) %>% 
             ungroup() %>% 
             distinct(cell_id, subcell_id, subcell_id_0_25, iso, growth_95)

df <- model_9_1  %>% 
      left_join(model_9_5)  %>% 
      filter(!is.infinite(growth_91) & !is.nan(growth_91) &
             !is.infinite(growth_95) & !is.nan(growth_95)) # because some GCP == 0

# define a function to calculate R^2
calculate_r_squared = 1 - (sum((df$growth_91 - df$growth_95)^2) / 
                        sum((df$growth_91 - mean(df$growth_91))^2))
calculate_r_squared

p6 <- ggplot(df, aes(x=growth_91, y = growth_95)) +
  geom_point(size = 1, alpha = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = sprintf("R² = %.4f", calculate_r_squared), 
             hjust = 1.1, vjust = -0.5, size = 7, colour = "black") +
  labs(subtitle = "0.25-degree Model Predictions",
       x = "log(GDP in 2019) - log(GDP in 2018) \nfrom model trained on 2012 to 2021 data", 
       y = "log(GDP in 2019) - log(GDP in 2018) \nfrom model trained on 2012 to 2019 data")   +
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

ggsave("step5_predict_and_post_adjustments_log_change/outputs/analyze_diff_models/compare_log change_all_deg.png", plot = combined_plot, width = 22, height = 14, bg = "white")
