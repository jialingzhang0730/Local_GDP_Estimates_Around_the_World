# --------------------------------- Task Summary --------------------------------- #
# This file converts the post-adjusted cell GDP estimates from constant 2021 USD into current USD, current PPP, and constant 2021 PPP units, propagating the per-tree uncertainty quantiles through the same currency rescaling.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tictoc)
library(gdata)
library(sf)
library(parallel)
library(tidyverse)
library(fs)
library(dplyr)
library(data.table)
library(ranger)
library(tmaptools)
library(scales)
library(workflows)
library(exactextractr)
library(terra)
library(raster)

# Ensure output directory exists for the rescaled-currency datasets written below
dir.create("step5_predict_and_post_adjustments/outputs/final_output_dataset_with_uncertainty",
           recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# read the national GDP dataset
current_USD <- read.csv("step2_obtain_gdp_data/temp/national_gdp_current_USD.csv")
current_PPP <- read.csv("step2_obtain_gdp_data/temp/national_gdp_current_PPP.csv")
const_USD <- read.csv("step2_obtain_gdp_data/temp/national_gdp_const_2021_USD.csv")
const_PPP <- read.csv("step2_obtain_gdp_data/temp/national_gdp_const_2021_PPP.csv")

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

# read our dataset, the following datasets are in const 2021 USD
files_gdp <- list.files(path = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density", pattern = "^GDPC.*(\\.csv|\\.RData)$", full.names = TRUE)

# rescale them
rescale_result <- lapply(files_gdp, function(file_path){
  file_ext <- tools::file_ext(file_path)

  if (file_ext == "csv") {
    data <- read.csv(file_path)
  } else if (file_ext == "RData") {
    loaded_names <- load(file_path)
    data <- get(loaded_names[1]) # use the actual object name stored in the .RData file
  } else {
    stop("Unsupported file type")
  }

  result <- data %>%
    as.data.frame() %>%
    left_join(const_to_curt_USD) %>%
    left_join(const_to_curt_PPP) %>%
    left_join(const_to_const_PPP) %>%
    rename(predicted_GCP_const_2021_USD = predicted_GCP,
           cell_GDPC_const_2021_USD = cell_GDPC) %>%
    mutate(predicted_GCP_current_USD = predicted_GCP_const_2021_USD*const_to_curt_USD_idx,
           predicted_GCP_const_2021_PPP = predicted_GCP_const_2021_USD*const_to_const_PPP_idx,
           predicted_GCP_current_PPP = predicted_GCP_const_2021_USD*const_to_curt_PPP_idx,
           cell_GDPC_current_USD = cell_GDPC_const_2021_USD*const_to_curt_USD_idx,
           cell_GDPC_const_2021_PPP = cell_GDPC_const_2021_USD*const_to_const_PPP_idx,
           cell_GDPC_current_PPP = cell_GDPC_const_2021_USD*const_to_curt_PPP_idx)

  # Apply currency conversions to GCP uncertainty columns (q05, q95, tree_sd only)
  q_suffixes <- c("q05", "q95", "tree_sd")
  for (suf in q_suffixes) {
    gcp_col <- paste0("GCP_", suf)
    if (gcp_col %in% names(result)) {
      result[[paste0("predicted_GCP_const_2021_USD_", suf)]] <- result[[gcp_col]]
      result[[paste0("predicted_GCP_current_USD_", suf)]] <- result[[gcp_col]] * result$const_to_curt_USD_idx
      result[[paste0("predicted_GCP_const_2021_PPP_", suf)]] <- result[[gcp_col]] * result$const_to_const_PPP_idx
      result[[paste0("predicted_GCP_current_PPP_", suf)]] <- result[[gcp_col]] * result$const_to_curt_PPP_idx
    }
  }

  # Remove conversion indices, original (pre-currency) uncertainty columns, and unused columns
  all_gcp_suffixes <- c("q01", "q05", "q10", "q90", "q95", "q99", "tree_sd")
  all_gdpc_suffixes <- c("q01", "q05", "q10", "q90", "q95", "q99", "tree_sd")
  result <- result %>%
    dplyr::select(-c(const_to_curt_USD_idx, const_to_curt_PPP_idx, const_to_const_PPP_idx)) %>%
    dplyr::select(-any_of(c(paste0("GCP_", all_gcp_suffixes), paste0("GDPC_", all_gdpc_suffixes)))) %>%
    dplyr::select(
      # Identifiers
      cell_id, any_of(c("subcell_id", "subcell_id_0_25")), iso, year,
      # GCP point estimates
      predicted_GCP_const_2021_USD, predicted_GCP_current_USD,
      predicted_GCP_const_2021_PPP, predicted_GCP_current_PPP,
      # GCP uncertainty (CI bounds + SD, grouped by currency)
      predicted_GCP_const_2021_USD_q05, predicted_GCP_const_2021_USD_q95, predicted_GCP_const_2021_USD_tree_sd,
      predicted_GCP_current_USD_q05, predicted_GCP_current_USD_q95, predicted_GCP_current_USD_tree_sd,
      predicted_GCP_const_2021_PPP_q05, predicted_GCP_const_2021_PPP_q95, predicted_GCP_const_2021_PPP_tree_sd,
      predicted_GCP_current_PPP_q05, predicted_GCP_current_PPP_q95, predicted_GCP_current_PPP_tree_sd,
      # SD of log(GDP) — currency- and population-invariant
      any_of("GCP_sd_log_gdp"),
      # Population
      pop_cell,
      # GDPC point estimates
      cell_GDPC_const_2021_USD, cell_GDPC_current_USD,
      cell_GDPC_const_2021_PPP, cell_GDPC_current_PPP,
      # Metadata
      is_cell_censored, method, cell_size, national_population,
      everything()  # catches geom and any remaining columns
    )

  if (file_ext == "csv") {
    result_final <- result
    new_file_path <- file.path("step5_predict_and_post_adjustments/outputs/final_output_dataset_with_uncertainty", paste0("final_", sub("\\.[^\\.]+$", ".csv", basename(file_path))))
    write.csv(result_final, new_file_path, row.names = FALSE)

  } else if (file_ext == "RData") {
    result_final <- result %>% st_as_sf()
    new_file_path <- file.path("step5_predict_and_post_adjustments/outputs/final_output_dataset_with_uncertainty", paste0("final_", basename(file_path)))
    new_object_name <- sub("\\.RData$", "", sub(".*/(.*)\\.RData$", "final_\\1", file_path))

    # Save the result_final with the new object name
    assign(new_object_name, result_final)
    save(list = new_object_name, file = new_file_path)

  } else {
    stop("Unsupported file type")
  }        

})
