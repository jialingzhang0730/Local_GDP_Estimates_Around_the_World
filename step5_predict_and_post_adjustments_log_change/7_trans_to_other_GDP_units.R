# --------------------------------- Task Summary --------------------------------- #
# This file is to change the units of the GDP data to others. Previously when we train the model (the variable national_gdpc) and 
#       draw some plots, the unit is in constant 2017 USD. Now we will rescale the data to other units:
#       current USD, current PPP-adjusted international$, constant 2017 PPP-adjusted international$ 
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

# ---------------------------------------------------------------------------------------------------------------------------------------
# read the national GDP dataset
current_USD <- read.csv("step2_obtain_gdp_data/temp/national_gdp_current_USD.csv")
current_PPP <- read.csv("step2_obtain_gdp_data/temp/national_gdp_current_PPP.csv")
const_USD <- read.csv("step2_obtain_gdp_data/temp/national_gdp_const_2017_USD.csv")
const_PPP <- read.csv("step2_obtain_gdp_data/temp/national_gdp_const_2017_PPP.csv")

# create rescale index
const_to_curt_USD <- const_USD %>%
    dplyr::select(c(iso, year, rgdp_total)) %>%
    rename(const_USD = rgdp_total) %>%
    left_join(current_USD %>% dplyr::select(c(iso, year, rgdp_total)) %>% rename(curt_USD = rgdp_total)) %>%
    mutate(const_to_curt_USD_idx = curt_USD/const_USD) %>%
    dplyr::select(c(iso, year, const_to_curt_USD_idx))

const_to_curt_PPP <- const_USD %>%
    dplyr::select(c(iso, year, rgdp_total)) %>%
    rename(const_USD = rgdp_total) %>%
    left_join(current_PPP %>% dplyr::select(c(iso, year, rgdp_total)) %>% rename(curt_PPP = rgdp_total)) %>%
    mutate(const_to_curt_PPP_idx = curt_PPP/const_USD) %>%
    dplyr::select(c(iso, year, const_to_curt_PPP_idx)) # there are some NAs because some countries do not have PPP adjusted GDP

const_to_const_PPP <- const_USD %>%
    dplyr::select(c(iso, year, rgdp_total)) %>%
    rename(const_USD = rgdp_total) %>%
    left_join(const_PPP %>% dplyr::select(c(iso, year, rgdp_total)) %>% rename(const_PPP = rgdp_total)) %>%
    mutate(const_to_const_PPP_idx = const_PPP/const_USD) %>%
    dplyr::select(c(iso, year, const_to_const_PPP_idx)) # there are some NAs because some countries do not have PPP adjusted GDP

# read our dataset, the following datasets are in const 2017 USD
files_gdp <- list.files(path = "step5_predict_and_post_adjustments_log_change/outputs/predict_data_results_postadjust_pop_density", pattern = "^GDPC.*(\\.csv|_adjust\\.RData)$", full.names = TRUE)

# rescale them
rescale_result <- lapply(files_gdp, function(file_path){
    file_ext <- tools::file_ext(file_path)

    if (file_ext == "csv") {
        data <- read.csv(file_path)
    } else if (file_ext == "RData") {
        load(file_path)
        data <- get(sub(".*/(.*)\\.RData$", "\\1", file_path)) # because the name of the loaded RData file can be extracted from the file path 
    } else {
        stop("Unsupported file type")
    }

    result <- data %>%
        as.data.frame() %>%
        left_join(const_to_curt_USD) %>%
        left_join(const_to_curt_PPP) %>%
        left_join(const_to_const_PPP) %>%
        rename(predicted_GCP_const_2017_USD = predicted_GCP,
               cell_GDPC_const_2017_USD = cell_GDPC) %>%
        mutate(predicted_GCP_current_USD = predicted_GCP_const_2017_USD*const_to_curt_USD_idx,
               predicted_GCP_const_2017_PPP = predicted_GCP_const_2017_USD*const_to_const_PPP_idx,
               predicted_GCP_current_PPP = predicted_GCP_const_2017_USD*const_to_curt_PPP_idx,               
               cell_GDPC_current_USD = cell_GDPC_const_2017_USD*const_to_curt_USD_idx,
               cell_GDPC_const_2017_PPP = cell_GDPC_const_2017_USD*const_to_const_PPP_idx,
               cell_GDPC_current_PPP = cell_GDPC_const_2017_USD*const_to_curt_PPP_idx) %>%
        dplyr::select(-c(const_to_curt_USD_idx, const_to_curt_PPP_idx, const_to_const_PPP_idx)) %>%
        dplyr::select(c(cell_id, iso, year, starts_with("predicted_GCP"), pop_cell, starts_with("cell_GDPC"), everything())) # i want to arrange the order of the columns

    if (file_ext == "csv") {
        result_final <- result
        new_file_path <- file.path("step5_predict_and_post_adjustments_log_change/outputs/final_output_dataset", paste0("final_", sub("\\.[^\\.]+$", ".csv", basename(file_path))))
        write.csv(result_final, new_file_path, row.names = FALSE)

    } else if (file_ext == "RData") {
        result_final <- result %>% st_as_sf()
        new_file_path <- file.path("step5_predict_and_post_adjustments_log_change/outputs/final_output_dataset", paste0("final_", basename(file_path)))
        new_object_name <- sub("\\.RData$", "", sub(".*/(.*)\\.RData$", "final_\\1", file_path))
        
        # Save the result_final with the new object name
        assign(new_object_name, result_final)
        save(list = new_object_name, file = new_file_path)

    } else {
        stop("Unsupported file type")
    }        

})

