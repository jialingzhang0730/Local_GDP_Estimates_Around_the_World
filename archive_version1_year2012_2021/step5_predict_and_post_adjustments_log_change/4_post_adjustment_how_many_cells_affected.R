# --------------------------------- Task Summary --------------------------------- #
# This file is to provide statistics on how many cells are affected by the post-adjustments
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
library(writexl)

# ------------- 1degree ------------- #
deg1_no_adjust <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_no_extra_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()

deg1_adjust_0_01 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_0_01_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


deg1_adjust_0_02 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_0_02_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


deg1_adjust_0_05 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_1deg_postadjust_pop_dens_0_05_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


list_of_datasets <- list("No_Adjust" = deg1_no_adjust,
                         "Adjust_0_01" = deg1_adjust_0_01,
                         "Adjust_0_02" = deg1_adjust_0_02,
                         "Adjust_0_05" = deg1_adjust_0_05)

# Write the data frames to an Excel file with multiple sheets
write_xlsx(list_of_datasets, "step5_predict_and_post_adjustments_log_change/outputs/post_adjust_statistic/deg1.xlsx")

# ------------- 0.5degree ------------- #
deg0_5_no_adjust <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()

deg0_5_adjust_0_01 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_0_01_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


deg0_5_adjust_0_02 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_0_02_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


deg0_5_adjust_0_05 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_0_05_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


list_of_datasets <- list("No_Adjust" = deg0_5_no_adjust,
                         "Adjust_0_01" = deg0_5_adjust_0_01,
                         "Adjust_0_02" = deg0_5_adjust_0_02,
                         "Adjust_0_05" = deg0_5_adjust_0_05)

# Write the data frames to an Excel file with multiple sheets
write_xlsx(list_of_datasets, "step5_predict_and_post_adjustments_log_change/outputs/post_adjust_statistic/deg0_5.xlsx")

# ------------- 0.25degree ------------- #
deg0_25_no_adjust <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()

deg0_25_adjust_0_01 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_0_01_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


deg0_25_adjust_0_02 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_0_02_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


deg0_25_adjust_0_05 <- read.csv("step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_0_05_adjust.csv") %>% 
    group_by(year) %>%
    mutate(cell_affected = sum(is_cell_censored),
           num_zero_pop = sum(pop_cell == 0 & is_cell_censored),
           all_cells = n(),
           perc_aff_inha = (cell_affected - num_zero_pop)/(n() - num_zero_pop),
           num_positive_GDP = sum(predicted_GCP > 0 & is_cell_censored),
           test = sum(pop_cell == 0 & predicted_GCP > 0)) %>% 
    distinct(year, .keep_all = TRUE) %>% 
    dplyr::select(c(year, all_cells, cell_affected, num_zero_pop, perc_aff_inha, num_positive_GDP, test)) %>% 
    as.data.frame()


list_of_datasets <- list("No_Adjust" = deg0_25_no_adjust,
                         "Adjust_0_01" = deg0_25_adjust_0_01,
                         "Adjust_0_02" = deg0_25_adjust_0_02,
                         "Adjust_0_05" = deg0_25_adjust_0_05)

# Write the data frames to an Excel file with multiple sheets
write_xlsx(list_of_datasets, "step5_predict_and_post_adjustments_log_change/outputs/post_adjust_statistic/deg0_25.xlsx")













