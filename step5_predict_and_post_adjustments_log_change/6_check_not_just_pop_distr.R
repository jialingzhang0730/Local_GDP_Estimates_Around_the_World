# --------------------------------- Task Summary --------------------------------- #
# This file is to show that population is NOT the only thing that contributes to 
#   the distribution of GDP
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
library(exactextractr)
library(terra)
library(raster)
library(colorRamps)
library(RColorBrewer)
library(np)
library(gridExtra)
library(grid)
library(estimatr)
library(splines)
library(knitr)
library(kableExtra)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------
# prepare data

# 1deg
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_no_extra_adjust.RData")

pred_1deg <- GDPC_1deg_postadjust_pop_dens_no_extra_adjust %>% 
  as.data.frame() %>% 
  dplyr::select(c(cell_id, iso, year, predicted_GCP, pop_cell)) %>% 
  mutate(log_GCP = log(predicted_GCP),
        log_pop = log(pop_cell/1000000)) %>%
  mutate(inf = ifelse(is.infinite(log_GCP) | is.infinite(log_pop), 1, 0)) %>% 
  filter(inf != 1) %>%
  mutate(adjusted_log_GCP = log_GCP - log_pop)

# 0.5deg
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.RData")

pred_0_5deg <- GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust %>% 
  as.data.frame() %>% 
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP, pop_cell)) %>% 
  mutate(log_GCP = log(predicted_GCP),
        log_pop = log(pop_cell/1000000)) %>%
  mutate(inf = ifelse(is.infinite(log_GCP) | is.infinite(log_pop), 1, 0)) %>% 
  filter(inf != 1) %>%
  mutate(adjusted_log_GCP = log_GCP - log_pop)

# 0.25deg
load("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.RData")

pred_0_25deg <- GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust %>% 
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
    theme(aspect.ratio = 1) + 
    annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+1.5, label = format_equation(model, 4), hjust = 0, size = 3, color = "blue") +
    annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+0.05, label = format_red_line_equation(coef(poly0)[1]), hjust = 0, size = 3, color = "red")

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
    theme(aspect.ratio = 1) + 
    annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+1.5, label = format_equation(model, 4), hjust = 0, size = 3, color = "blue") +
    annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+0.05, label = format_red_line_equation(coef(poly0)[1]), hjust = 0, size = 3, color = "red")

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
    theme(aspect.ratio = 1) + 
    annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+1.5, label = format_equation(model, 4), hjust = 0, size = 3, color = "blue") +
    annotate("text", x = min(x_limits) + 0.1, y = min(y_limits)+0.05, label = format_red_line_equation(coef(poly0)[1]), hjust = 0, size = 3, color = "red")

}

final_plot_with_title <- grid.arrange(
  top = textGrob("", gp = gpar(fontsize = 15, fontface = "bold")),
  arrangeGrob(p1, p2, p3, ncol = 3, 
  nrow = 1))

ggsave("step5_predict_and_post_adjustments_log_change/outputs/not_just_pop_distr.png", 
       plot = final_plot_with_title,
       bg = "white", width = 17, height = 6)



