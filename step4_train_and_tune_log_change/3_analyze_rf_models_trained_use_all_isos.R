# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to check the performance of our models trained in R scripts "2_put_all_isos_to_train_xdeg.R"
# 
# The formula for GDP loss used in this script has a value 2 in the denominator because misallocated GDP will be double counted
# 
# All the R2 and GDP loss will show within sample out of bag predictions because all data are used to train the model
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
library(stargazer)
library(kableExtra)
library(cowplot)

# ------------------------------------------------------------------------------------------------------------
# 1-degree model

load("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_1deg.RData")
rf_model_good <- rf_model9_good_grid_search_1deg

importance_plot <- vip(rf_model_good, num_features = 33, aesthetics = list(fill = "steelblue",color = "black")) +
  theme_minimal() +  # Apply a minimal theme
  labs(title = "1 degree model",
    x = "Variables",
    y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
ggsave("step4_train_and_tune_log_change/outputs/importance_plot_1deg_all_isos.pdf", plot=importance_plot)

importance_scores <- vip::vi(rf_model_good) %>% as.data.frame()
write.csv(importance_scores, "step4_train_and_tune_log_change/outputs/importance_scores_1deg.csv", row.names = FALSE)

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_1deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_1deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_1deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_1deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_1deg.csv") 

# note here, the predictions of training data are out of bag predictions

predictions <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_train_results_grid_search_1deg <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso) %>%
                        mutate(pred_GCP_share_1deg = predictions[,1]) %>%
                        mutate(pred_GCP_share_1deg = ifelse(pop_share == 0, 0, pred_GCP_share_1deg))  %>% 
                        group_by(iso, year)  %>% 
                        mutate(pred_GCP_share_1deg_rescaled = pred_GCP_share_1deg/sum(pred_GCP_share_1deg))  %>% 
                        ungroup()  %>% 
                        mutate(pred_GCP_1deg = pred_GCP_share_1deg_rescaled * state_total_GDP)

###################################################################################################################################################

# Now let's calculate R^2 and GDP loss

data_train_results_grid_search <- data_train_results_grid_search_1deg  %>% 
                                  mutate(iso_real = ifelse(substr(iso,1,3) == "USA", "USA", iso))  %>%                                
                                  rename(iso_fake = iso)  %>% 
                                  rename(iso = iso_real) # change the fake country to be iso_fake, and iso_real to be iso 

results_df_full_R2_rest_yr <- data.frame(iso = c(unique(data_train_results_grid_search$iso), "total"))

# R2
R_squared_list <- list()
for (iso_code in unique(data_train_results_grid_search$iso)) {
iso_data <- data_train_results_grid_search %>% 
  filter(iso == iso_code) %>% 
  filter(GCP_1deg != 0 & pred_GCP_1deg != 0)
true_GCP <- iso_data$GCP_1deg
pred_GCP <- iso_data$pred_GCP_1deg
R_squared <- 1 - sum((log(true_GCP) - log(pred_GCP))^2) / sum((log(true_GCP) - mean(log(true_GCP)))^2)
R_squared_list[[iso_code]] <- R_squared
}  

R_squared_values <- unname(unlist(R_squared_list))
filtered_data <- data_train_results_grid_search %>% filter(GCP_1deg != 0 & pred_GCP_1deg != 0)
true_GCP <- filtered_data$GCP_1deg
pred_GCP <- filtered_data$pred_GCP_1deg
all_countries_R_sqr <- 1 - sum((log(true_GCP) - log(pred_GCP))^2)/sum((log(true_GCP) - mean(log(true_GCP)))^2)
values <- c(R_squared_values, all_countries_R_sqr)
results_df_full_R2_rest_yr["training"] <- values

# GDP loss

data_results_full <- data_train_results_grid_search
data_results_full_1deg <- data_results_full

GDP_loss_df <- data_results_full  %>% 
  group_by(iso, year)  %>% 
  mutate(GDP_loss = sum(abs(GCP_1deg - pred_GCP_1deg)) / (2*sum(GCP_1deg)))  %>%
  dplyr::select(c("iso", "year", "GDP_loss")) %>%
  distinct()

GDP_loss_total <- data_results_full  %>% 
                    group_by(year)  %>% 
                    mutate(GDP_loss = sum(abs(GCP_1deg - pred_GCP_1deg)) / (2*sum(GCP_1deg)))   %>% 
                    dplyr::select(c("year", "GDP_loss")) %>%
                    distinct()
GDP_loss_total$iso <- "total"
GDP_loss_df <- bind_rows(GDP_loss_df, GDP_loss_total)
    
GDP_loss_wide <- GDP_loss_df %>%
  pivot_wider(names_from = year, values_from = GDP_loss)
colnames(GDP_loss_wide) <- c("iso", as.character(unique(GDP_loss_df$year)))

results_df_final_1deg <- results_df_full_R2_rest_yr 
GDP_loss_wide_1deg <- GDP_loss_wide

# annual changes
annual_chan <- data_results_full %>% 
  dplyr::select(c(cell_id, iso_fake, iso, year, GCP_1deg, pred_GCP_1deg)) %>% 
  arrange(iso_fake, cell_id, year) %>%
  group_by(iso_fake, cell_id) %>%
  mutate(prev_year_pred = ifelse(year - 1 %in% year, pred_GCP_1deg[match(year - 1, year)], NA),
        prev_year_true = ifelse(year - 1 %in% year, GCP_1deg[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_true)) %>% 
  filter(GCP_1deg != 0 & pred_GCP_1deg != 0 & prev_year_pred != 0 & prev_year_true != 0) %>% 
  mutate(log_diff_true = log(GCP_1deg) - log(prev_year_true),
         log_diff_pred = log(pred_GCP_1deg) - log(prev_year_pred)) %>% 
  group_by(iso) %>% 
  mutate(r2 = 1 - sum((log_diff_true - log_diff_pred)^2) / sum((log_diff_true - mean(log_diff_true))^2)) %>% 
  ungroup() %>%
  distinct(iso, r2) %>% 
  as.data.frame()

# now organize the files to save
developed_group <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU",
                     "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",   
                     "HUN", "ITA", "JPN", "KOR", "LTU", "LVA",   
                     "NLD", "NOR", "NZL", "POL", "PRT", "ROU",   
                     "SVK", "SVN", "SWE", "TUR", "USA")

R2_1deg_all_train <- results_df_final_1deg %>%
  filter(iso != "total") %>% 
  arrange(iso) %>% 
  mutate(category = ifelse(iso %in% developed_group, "Developed", "Developing")) %>%
  group_by(category) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = category, values_from = c(iso, training)) %>%
  dplyr::select(`Iso: Developed` = iso_Developed, `R2: Developed` = training_Developed, 
         `Iso: Developing` = iso_Developing, `R2: Developing` = training_Developing) %>% 
  mutate(across(c(`R2: Developed`, `R2: Developing`), 
                ~ ifelse(is.na(.), NA, sprintf("%.2f%%", . * 100))))   

GDP_loss_1deg_all_train <- GDP_loss_wide_1deg %>% 
  filter(iso != "total") %>% 
  arrange(iso) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), "NA", sprintf("%.1f%%", . * 100)))) %>% 
  dplyr::select(iso, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`,`2019`,`2020`,`2021`)

R2_chan_1deg_all_train <- annual_chan %>%
  arrange(iso) %>% 
  mutate(category = ifelse(iso %in% developed_group, "Developed", "Developing")) %>%
  group_by(category) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = category, values_from = c(iso, r2)) %>%
  dplyr::select(`Iso: Developed` = iso_Developed, `R2: Developed` = r2_Developed, 
         `Iso: Developing` = iso_Developing, `R2: Developing` = r2_Developing) %>% 
  mutate(across(c(`R2: Developed`, `R2: Developing`), 
                ~ ifelse(is.na(.), NA, sprintf("%.2f%%", . * 100))))   

save_latex_stargazer <- function(df, filename) {

  colnames(df) <- gsub("&", "\\&", colnames(df), fixed = TRUE)

  stargazer(df, summary = FALSE, rownames = FALSE, type = "latex", out = filename,
            title = "Table Title", label = "tab:title", 
            digits = 3, style = "default", align = TRUE)
}

save_latex_stargazer(R2_1deg_all_train, "step4_train_and_tune_log_change/outputs/R2_1deg_all_train.tex")
save_latex_stargazer(GDP_loss_1deg_all_train, "step4_train_and_tune_log_change/outputs/GDP_loss_1deg_all_train.tex")
save_latex_stargazer(R2_chan_1deg_all_train, "step4_train_and_tune_log_change/outputs/R2_annual_chan_1deg_all_train.tex")


# ------------------------------------------------------------------------------------------------------------------------------------
# 0.5 degree model

load("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_5deg.RData")
rf_model_good <- rf_model9_good_grid_search_0_5deg

importance_plot <- vip(rf_model_good, num_features = 33, aesthetics = list(fill = "steelblue",color = "black")) +
  theme_minimal() +  # Apply a minimal theme
  labs(title = "0.5 degree model",
    x = "Variables",
    y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
ggsave("step4_train_and_tune_log_change/outputs/importance_plot_0_5deg_all_isos.pdf", plot=importance_plot)

importance_scores <- vip::vi(rf_model_good) %>% as.data.frame()
write.csv(importance_scores, "step4_train_and_tune_log_change/outputs/importance_scores_0_5deg.csv", row.names = FALSE)

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_0_5deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_0_5deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_5deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_0_5deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_0_5deg.csv") 

# note here, the predictions of training data are out of bag predictions

predictions <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_train_results_grid_search_0_5deg <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso) %>%
                        mutate(pred_GCP_share_0_5deg = predictions[,1])  %>% 
                        mutate(pred_GCP_share_0_5deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_5deg))  %>%                         
                        group_by(iso, year)  %>% 
                        mutate(pred_GCP_share_0_5deg_rescaled = pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg))  %>% 
                        ungroup()  %>% 
                        mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * state_total_GDP)

###################################################################################################################################################

# Now let's calculate R^2 and GDP loss

data_train_results_grid_search <- data_train_results_grid_search_0_5deg  %>% 
                                  mutate(iso_real = ifelse(substr(iso,1,3) == "USA", "USA", iso))  %>%                                  
                                  rename(iso_fake = iso)  %>% 
                                  rename(iso = iso_real) # change the fake country to be iso_fake, and iso_real to be iso 

results_df_full_R2_rest_yr <- data.frame(iso = c(unique(data_train_results_grid_search$iso), "total"))

# R2
R_squared_list <- list()
for (iso_code in unique(data_train_results_grid_search$iso)) {
iso_data <- data_train_results_grid_search %>% 
  filter(iso == iso_code) %>% 
  filter(GCP_0_5deg != 0 & pred_GCP_0_5deg != 0)
true_GCP <- iso_data$GCP_0_5deg
pred_GCP <- iso_data$pred_GCP_0_5deg
R_squared <- 1 - sum((log(true_GCP) - log(pred_GCP))^2) / sum((log(true_GCP) - mean(log(true_GCP)))^2)
R_squared_list[[iso_code]] <- R_squared
}  

R_squared_values <- unname(unlist(R_squared_list))
filtered_data <- data_train_results_grid_search %>% filter(GCP_0_5deg != 0 & pred_GCP_0_5deg != 0)
true_GCP <- filtered_data$GCP_0_5deg
pred_GCP <- filtered_data$pred_GCP_0_5deg
all_countries_R_sqr <- 1 - sum((log(true_GCP) - log(pred_GCP))^2)/sum((log(true_GCP) - mean(log(true_GCP)))^2)
values <- c(R_squared_values, all_countries_R_sqr)
results_df_full_R2_rest_yr["training"] <- values

# GDP loss

data_results_full <- data_train_results_grid_search
data_results_full_0_5deg <- data_results_full

GDP_loss_df <- data_results_full  %>% 
  group_by(iso, year)  %>% 
  mutate(GDP_loss = sum(abs(GCP_0_5deg - pred_GCP_0_5deg)) / (2*sum(GCP_0_5deg)))  %>%
  dplyr::select(c("iso", "year", "GDP_loss")) %>%
  distinct()

GDP_loss_total <- data_results_full  %>% 
                    group_by(year)  %>% 
                    mutate(GDP_loss = sum(abs(GCP_0_5deg - pred_GCP_0_5deg)) / (2*sum(GCP_0_5deg)))   %>% 
                    dplyr::select(c("year", "GDP_loss")) %>%
                    distinct()
GDP_loss_total$iso <- "total"
GDP_loss_df <- bind_rows(GDP_loss_df, GDP_loss_total)
    
GDP_loss_wide <- GDP_loss_df %>%
  pivot_wider(names_from = year, values_from = GDP_loss)
colnames(GDP_loss_wide) <- c("iso", as.character(unique(GDP_loss_df$year)))

results_df_final_0_5deg <- results_df_full_R2_rest_yr 
GDP_loss_wide_0_5deg <- GDP_loss_wide

# annual changes
annual_chan <- data_results_full %>% 
  dplyr::select(c(cell_id, subcell_id, iso_fake, iso, year, GCP_0_5deg, pred_GCP_0_5deg)) %>% 
  arrange(iso_fake, cell_id, subcell_id, year) %>%
  group_by(iso_fake, cell_id, subcell_id) %>%
  mutate(prev_year_pred = ifelse(year - 1 %in% year, pred_GCP_0_5deg[match(year - 1, year)], NA),
        prev_year_true = ifelse(year - 1 %in% year, GCP_0_5deg[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_true)) %>% 
  filter(GCP_0_5deg != 0 & pred_GCP_0_5deg != 0 & prev_year_pred != 0 & prev_year_true != 0) %>% 
  mutate(log_diff_true = log(GCP_0_5deg) - log(prev_year_true),
         log_diff_pred = log(pred_GCP_0_5deg) - log(prev_year_pred)) %>% 
  group_by(iso) %>% 
  mutate(r2 = 1 - sum((log_diff_true - log_diff_pred)^2) / sum((log_diff_true - mean(log_diff_true))^2)) %>% 
  ungroup() %>%
  distinct(iso, r2) %>% 
  as.data.frame()

# now organize the files to save
developed_group <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU",
                     "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",   
                     "HUN", "ITA", "JPN", "KOR", "LTU", "LVA",   
                     "NLD", "NOR", "NZL", "POL", "PRT", "ROU",   
                     "SVK", "SVN", "SWE", "TUR", "USA")

R2_0_5deg_all_train <- results_df_final_0_5deg %>%
  filter(iso != "total") %>% 
  arrange(iso) %>% 
  mutate(category = ifelse(iso %in% developed_group, "Developed", "Developing")) %>%
  group_by(category) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = category, values_from = c(iso, training)) %>%
  dplyr::select(`Iso: Developed` = iso_Developed, `R2: Developed` = training_Developed, 
         `Iso: Developing` = iso_Developing, `R2: Developing` = training_Developing) %>% 
  mutate(across(c(`R2: Developed`, `R2: Developing`), 
                ~ ifelse(is.na(.), NA, sprintf("%.2f%%", . * 100))))   

GDP_loss_0_5deg_all_train <- GDP_loss_wide_0_5deg %>% 
  filter(iso != "total") %>% 
  arrange(iso) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), "NA", sprintf("%.1f%%", . * 100)))) %>% 
  dplyr::select(iso, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`,`2019`,`2020`,`2021`)

R2_chan_0_5deg_all_train <- annual_chan %>%
  arrange(iso) %>% 
  mutate(category = ifelse(iso %in% developed_group, "Developed", "Developing")) %>%
  group_by(category) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = category, values_from = c(iso, r2)) %>%
  dplyr::select(`Iso: Developed` = iso_Developed, `R2: Developed` = r2_Developed, 
         `Iso: Developing` = iso_Developing, `R2: Developing` = r2_Developing) %>% 
  mutate(across(c(`R2: Developed`, `R2: Developing`), 
                ~ ifelse(is.na(.), NA, sprintf("%.2f%%", . * 100))))   


save_latex_stargazer <- function(df, filename) {

  colnames(df) <- gsub("&", "\\&", colnames(df), fixed = TRUE)

  stargazer(df, summary = FALSE, rownames = FALSE, type = "latex", out = filename,
            title = "Table Title", label = "tab:title", 
            digits = 3, style = "default", align = TRUE)
}

save_latex_stargazer(R2_0_5deg_all_train, "step4_train_and_tune_log_change/outputs/R2_0_5deg_all_train.tex")
save_latex_stargazer(GDP_loss_0_5deg_all_train, "step4_train_and_tune_log_change/outputs/GDP_loss_0_5deg_all_train.tex")
save_latex_stargazer(R2_chan_0_5deg_all_train, "step4_train_and_tune_log_change/outputs/R2_annual_chan_0_5deg_all_train.tex")

# ------------------------------------------------------------------------------------------------------------------------------------
# 0.25 degree model

load("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/rf_model9_good_grid_search_0_25deg.RData")
rf_model_good <- rf_model9_good_grid_search_0_25deg

importance_plot <- vip(rf_model_good, num_features = 33, aesthetics = list(fill = "steelblue",color = "black")) +
  theme_minimal() +  # Apply a minimal theme
  labs(title = "0.25 degree model",
    x = "Variables",
    y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
ggsave("step4_train_and_tune_log_change/outputs/importance_plot_0_25deg_all_isos.pdf", plot=importance_plot)

importance_scores <- vip::vi(rf_model_good) %>% as.data.frame()
write.csv(importance_scores, "step4_train_and_tune_log_change/outputs/importance_scores_0_25deg.csv", row.names = FALSE)

data_train <- read.csv("step4_train_and_tune_log_change/outputs/new_data_train_0_25deg.csv")
data_valid_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_year_0_25deg.csv")  
data_valid_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_25deg.csv")  
data_test_year <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_year_0_25deg.csv") 
data_test_iso <- read.csv("step4_train_and_tune_log_change/outputs/new_data_test_iso_0_25deg.csv") 

# note here, the predictions of training data are out of bag predictions

predictions <- as.data.frame(rf_model_good$fit$fit$fit$predictions)

data_train_results_grid_search_0_25deg <- bind_rows(data_train, data_valid_year, data_valid_iso, data_test_year, data_test_iso) %>%
                        mutate(pred_GCP_share_0_25deg = predictions[,1])  %>% 
                        mutate(pred_GCP_share_0_25deg = ifelse(pop_share == 0, 0, pred_GCP_share_0_25deg))  %>%                         
                        group_by(iso, year)  %>% 
                        mutate(pred_GCP_share_0_25deg_rescaled = pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg))  %>% 
                        ungroup()  %>% 
                        mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * state_total_GDP)

###################################################################################################################################################

# Now let's calculate R^2 and GDP loss

data_train_results_grid_search <- data_train_results_grid_search_0_25deg  %>% 
                                  mutate(iso_real = ifelse(substr(iso,1,3) == "USA", "USA", iso))  %>%                                  
                                  rename(iso_fake = iso)  %>% 
                                  rename(iso = iso_real) # change the fake country to be iso_fake, and iso_real to be iso 

results_df_full_R2_rest_yr <- data.frame(iso = c(unique(data_train_results_grid_search$iso), "total"))

# R2
R_squared_list <- list()
for (iso_code in unique(data_train_results_grid_search$iso)) {
iso_data <- data_train_results_grid_search %>% 
  filter(iso == iso_code) %>% 
  filter(GCP_0_25deg != 0 & pred_GCP_0_25deg != 0)
true_GCP <- iso_data$GCP_0_25deg
pred_GCP <- iso_data$pred_GCP_0_25deg
R_squared <- 1 - sum((log(true_GCP) - log(pred_GCP))^2) / sum((log(true_GCP) - mean(log(true_GCP)))^2)
R_squared_list[[iso_code]] <- R_squared
}  

R_squared_values <- unname(unlist(R_squared_list))
filtered_data <- data_train_results_grid_search %>% filter(GCP_0_25deg != 0 & pred_GCP_0_25deg != 0)
true_GCP <- filtered_data$GCP_0_25deg
pred_GCP <- filtered_data$pred_GCP_0_25deg
all_countries_R_sqr <- 1 - sum((log(true_GCP) - log(pred_GCP))^2)/sum((log(true_GCP) - mean(log(true_GCP)))^2)
values <- c(R_squared_values, all_countries_R_sqr)
results_df_full_R2_rest_yr["training"] <- values

# GDP loss

data_results_full <- data_train_results_grid_search
data_results_full_0_25deg <- data_results_full

GDP_loss_df <- data_results_full  %>% 
  group_by(iso, year)  %>% 
  mutate(GDP_loss = sum(abs(GCP_0_25deg - pred_GCP_0_25deg)) / (2*sum(GCP_0_25deg)))  %>%
  dplyr::select(c("iso", "year", "GDP_loss")) %>%
  distinct()

GDP_loss_total <- data_results_full  %>% 
                    group_by(year)  %>% 
                    mutate(GDP_loss = sum(abs(GCP_0_25deg - pred_GCP_0_25deg)) / (2*sum(GCP_0_25deg)))   %>% 
                    dplyr::select(c("year", "GDP_loss")) %>%
                    distinct()
GDP_loss_total$iso <- "total"
GDP_loss_df <- bind_rows(GDP_loss_df, GDP_loss_total)
    
GDP_loss_wide <- GDP_loss_df %>%
  pivot_wider(names_from = year, values_from = GDP_loss)
colnames(GDP_loss_wide) <- c("iso", as.character(unique(GDP_loss_df$year)))

results_df_final_0_25deg <- results_df_full_R2_rest_yr 
GDP_loss_wide_0_25deg <- GDP_loss_wide

# annual changes
annual_chan <- data_results_full %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso_fake, iso, year, GCP_0_25deg, pred_GCP_0_25deg)) %>% 
  arrange(iso_fake, cell_id, subcell_id, subcell_id_0_25, year) %>%
  group_by(iso_fake, cell_id, subcell_id, subcell_id_0_25) %>%
  mutate(prev_year_pred = ifelse(year - 1 %in% year, pred_GCP_0_25deg[match(year - 1, year)], NA),
        prev_year_true = ifelse(year - 1 %in% year, GCP_0_25deg[match(year - 1, year)], NA)) %>%
  ungroup() %>% 
  filter(!is.na(prev_year_pred) & !is.na(prev_year_true)) %>% 
  filter(GCP_0_25deg != 0 & pred_GCP_0_25deg != 0 & prev_year_pred != 0 & prev_year_true != 0) %>% 
  mutate(log_diff_true = log(GCP_0_25deg) - log(prev_year_true),
         log_diff_pred = log(pred_GCP_0_25deg) - log(prev_year_pred)) %>% 
  group_by(iso) %>% 
  mutate(r2 = 1 - sum((log_diff_true - log_diff_pred)^2) / sum((log_diff_true - mean(log_diff_true))^2)) %>% 
  ungroup() %>%
  distinct(iso, r2) %>% 
  as.data.frame()

# now organize the files to save
developed_group <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU",
                     "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",   
                     "HUN", "ITA", "JPN", "KOR", "LTU", "LVA",   
                     "NLD", "NOR", "NZL", "POL", "PRT", "ROU",   
                     "SVK", "SVN", "SWE", "TUR", "USA")

R2_0_25deg_all_train <- results_df_final_0_25deg %>%
  filter(iso != "total") %>% 
  arrange(iso) %>% 
  mutate(category = ifelse(iso %in% developed_group, "Developed", "Developing")) %>%
  group_by(category) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = category, values_from = c(iso, training)) %>%
  dplyr::select(`Iso: Developed` = iso_Developed, `R2: Developed` = training_Developed, 
         `Iso: Developing` = iso_Developing, `R2: Developing` = training_Developing) %>% 
  mutate(across(c(`R2: Developed`, `R2: Developing`), 
                ~ ifelse(is.na(.), NA, sprintf("%.2f%%", . * 100))))   

GDP_loss_0_25deg_all_train <- GDP_loss_wide_0_25deg %>% 
  filter(iso != "total") %>% 
  arrange(iso) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), "NA", sprintf("%.1f%%", . * 100)))) %>% 
  dplyr::select(iso, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`,`2019`,`2020`,`2021`)

R2_chan_0_25deg_all_train <- annual_chan %>%
  arrange(iso) %>% 
  mutate(category = ifelse(iso %in% developed_group, "Developed", "Developing")) %>%
  group_by(category) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = category, values_from = c(iso, r2)) %>%
  dplyr::select(`Iso: Developed` = iso_Developed, `R2: Developed` = r2_Developed, 
         `Iso: Developing` = iso_Developing, `R2: Developing` = r2_Developing) %>% 
  mutate(across(c(`R2: Developed`, `R2: Developing`), 
                ~ ifelse(is.na(.), NA, sprintf("%.2f%%", . * 100))))   

save_latex_stargazer <- function(df, filename) {

  colnames(df) <- gsub("&", "\\&", colnames(df), fixed = TRUE)

  stargazer(df, summary = FALSE, rownames = FALSE, type = "latex", out = filename,
            title = "Table Title", label = "tab:title", 
            digits = 3, style = "default", align = TRUE)
}

save_latex_stargazer(R2_0_25deg_all_train, "step4_train_and_tune_log_change/outputs/R2_0_25deg_all_train.tex")
save_latex_stargazer(GDP_loss_0_25deg_all_train, "step4_train_and_tune_log_change/outputs/GDP_loss_0_25deg_all_train.tex")
save_latex_stargazer(R2_chan_0_25deg_all_train, "step4_train_and_tune_log_change/outputs/R2_annual_chan_0_25deg_all_train.tex")

# ------------------------------------------------------------------------------------------------------------------------------------

# draw the scatter plot compare cell predictions GCP to the truth
# obtain "data_results_full_xdeg" through above codes

p1 <- ggplot(data_results_full_1deg %>% filter(!(GCP_1deg == 0 | pred_GCP_1deg == 0)), aes(x=log(GCP_1deg), y = log(pred_GCP_1deg))) +
    geom_point(size = 0.0001, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed") +
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "1-degree model",  # Unit: const 2017 billion USD 
    x = "log(True cell GDP)",
    y = "log(Pred cell GDP)") +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))

p2 <- ggplot(data_results_full_0_5deg %>% filter(!(GCP_0_5deg == 0 | pred_GCP_0_5deg == 0)), aes(x=log(GCP_0_5deg), y = log(pred_GCP_0_5deg))) +
    geom_point(size = 0.0001, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "0.5-degree model",  # Unit: const 2017 billion USD 
    x = "log(True cell GDP)",
    y = "log(Pred cell GDP)") +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))    

p3 <- ggplot(data_results_full_0_25deg %>% filter(!(GCP_0_25deg == 0 | pred_GCP_0_25deg == 0)), aes(x=log(GCP_0_25deg), y = log(pred_GCP_0_25deg))) +
    geom_point(size = 0.0001, alpha = 0.8, color = "blue") +
    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red", linetype = "dashed")+
    theme_minimal()+ 
    theme_bw()+
    labs(subtitle = "0.25-degree model", # Unit: const 2017 billion USD 
    x = "log(True cell GDP)",
    y = "log(Pred cell GDP)") +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))          

combined_plot <- plot_grid(p1, p2, p3, ncol = 3)

ggsave("step4_train_and_tune_log_change/outputs/compare_gcp_pred_with_truth_scatter_plot_all_isos.png", plot = combined_plot, width = 10, height = 3, bg = "white")


# Cross validation results

all_iso_1deg <- read.csv("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_1deg.csv")
all_iso_0_5deg <- read.csv("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_0_5deg.csv")
all_iso_0_25deg <- read.csv("step4_train_and_tune_log_change/outputs/model9_tuning/put_all_isos_to_train/best_model_metrics_0_25deg.csv")

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

importance_1deg <- read.csv("step4_train_and_tune_log_change/outputs/importance_scores_1deg.csv")  %>%
  clean_names()
importance_0_5deg <- read.csv("step4_train_and_tune_log_change/outputs/importance_scores_0_5deg.csv") %>%
  clean_names()
importance_0_25deg <- read.csv("step4_train_and_tune_log_change/outputs/importance_scores_0_25deg.csv")  %>%
  clean_names()

merged_importance <- data.frame(
  Metric = variable_names
) %>%
  left_join(importance_1deg %>% rename(`1-degree Model` = Importance), by = c("Metric" = "Variable")) %>%
  left_join(importance_0_5deg %>% rename(`0.5-degree Model` = Importance), by = c("Metric" = "Variable")) %>%
  left_join(importance_0_25deg %>% rename(`0.25-degree Model` = Importance), by = c("Metric" = "Variable")) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

data <- data.frame(
  Metric = c(
    "$R^2$ (Developed)", "$R^2$ (Developing)", "$R^2$ (All)", "Weighted $R^2$",
    "$R^2$ (Developed)", "$R^2$ (Developing)", "$R^2$ (All)", "Weighted $R^2$",
    merged_importance$Metric
  ),
  `1-degree` = c(
    paste0(round(all_iso_1deg$mean_r2_developed * 100, 2), "\\%"),
    paste0(round(all_iso_1deg$mean_r2_developing * 100, 2), "\\%"),
    paste0(round(all_iso_1deg$mean_r2_all * 100, 2), "\\%"),
    paste0(round(all_iso_1deg$wgt_r2 * 100, 2), "\\%"),
    paste0(round(all_iso_1deg$mean_chan_r2_developed * 100, 2), "\\%"),
    paste0(round(all_iso_1deg$mean_chan_r2_developing * 100, 2), "\\%"),
    paste0(round(all_iso_1deg$mean_chan_r2_all * 100, 2), "\\%"),
    paste0(round(all_iso_1deg$wgt_chan_r2 * 100, 2), "\\%"),
    merged_importance$`1-degree Model`
  ),
  `0.5-degree` = c(
    paste0(round(all_iso_0_5deg$mean_r2_developed * 100, 2), "\\%"),
    paste0(round(all_iso_0_5deg$mean_r2_developing * 100, 2), "\\%"),
    paste0(round(all_iso_0_5deg$mean_r2_all * 100, 2), "\\%"),
    paste0(round(all_iso_0_5deg$wgt_r2 * 100, 2), "\\%"),
    paste0(round(all_iso_0_5deg$mean_chan_r2_developed * 100, 2), "\\%"),
    paste0(round(all_iso_0_5deg$mean_chan_r2_developing * 100, 2), "\\%"),
    paste0(round(all_iso_0_5deg$mean_chan_r2_all * 100, 2), "\\%"),
    paste0(round(all_iso_0_5deg$wgt_chan_r2 * 100, 2), "\\%"),
    merged_importance$`0.5-degree Model`
  ),
  `0.25-degree` = c(
    paste0(round(all_iso_0_25deg$mean_r2_developed * 100, 2), "\\%"),
    paste0(round(all_iso_0_25deg$mean_r2_developing * 100, 2), "\\%"),
    paste0(round(all_iso_0_25deg$mean_r2_all * 100, 2), "\\%"),
    paste0(round(all_iso_0_25deg$wgt_r2 * 100, 2), "\\%"),
    paste0(round(all_iso_0_25deg$mean_chan_r2_developed * 100, 2), "\\%"),
    paste0(round(all_iso_0_25deg$mean_chan_r2_developing * 100, 2), "\\%"),
    paste0(round(all_iso_0_25deg$mean_chan_r2_all * 100, 2), "\\%"),
    paste0(round(all_iso_0_25deg$wgt_chan_r2 * 100, 2), "\\%"),
    merged_importance$`0.25-degree Model`
  )
)

# Generate LaTeX table
latex_table <- kable(data, "latex", booktabs = TRUE, escape = FALSE, align = 'lccc',
                     col.names = c("Metric", "1-degree Model", "0.5-degree Model", "0.25-degree Model")) %>%
  kable_styling(full_width = FALSE) %>%
  pack_rows("Panel A: R2 of Log GDP Level", 1, 4, bold = FALSE, italic = TRUE) %>%
  pack_rows("Panel B: R2 of log(GDP in t) - log(GDP in t-1)", 5, 8, bold = FALSE, italic = TRUE) %>%
  pack_rows("Panel C: Top Variables and Importance Scores", 9, 18, bold = FALSE, italic = TRUE)

latex_table <- gsub("R2", "\\$R^2\\$", latex_table)

save_kable(latex_table, file = "step4_train_and_tune_log_change/outputs/model9_tuning/CV_metric_table.tex")
