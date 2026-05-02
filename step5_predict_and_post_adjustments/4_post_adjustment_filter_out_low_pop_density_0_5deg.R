# --------------------------------- Task Summary --------------------------------- #
# This file performs post-adjustment on the predicted 0.5-degree cell GDP values, and propagates per-tree predictions through the same censor-rescale-aggregate pipeline to produce cell-level uncertainty quantiles and standard deviations.
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
library(matrixStats)
library(scales)
library(workflows)
library(exactextractr)
library(terra)
library(raster)

# ------------------------------------------------------------------------------------------------------------------------------
# Model 9.1: 0.5deg

# obtain predicted GDP data
load("step5_predict_and_post_adjustments/outputs/predict_data_results_0_5deg_with_prov_boundary.RData")

# load population
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_5deg.RData")
pop <- land_pop_extracted_region_level_0_5deg  %>%
  filter(year <= 2022)  %>%
  as.data.frame()  %>%
  dplyr::select(c("cell_id", "subcell_id", "id", "iso", "year", "pop"))  %>%
  mutate(pop = floor(pop)) %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso))

# load land area:
# Note: the land area calculated is the area in square km based on a spherical approximation of the Earth

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_5deg.RData")
land_area <- lc_full_0_5deg  %>%
  filter(year <= 2022)  %>%
  as.data.frame()  %>%
  dplyr::select(c("cell_id", "subcell_id", "id", "iso", "year", "water", "barren", "snow_ice", "urban", "dense_forest",
                  "open_forest", "forest_cropland", "herbaceous", "cropland", "shrub", "herbaceous_cropland"))  %>%
  replace(is.na(.), 0)  %>%
  mutate(land_area_km2 = barren + snow_ice + urban + dense_forest + open_forest +
           forest_cropland + herbaceous + cropland + shrub + herbaceous_cropland)  %>%
  dplyr::select(c(cell_id, subcell_id, id, iso, year, land_area_km2)) %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso))

# ------------------------------------------------------------------------------------------------------------------------------

# load GDP
# Note: here I also want the area in square km based on a spherical approximation of the Earth

pred_0_5deg_with_prov_bound <- predict_data_results_0_5deg_with_prov_boundary %>%
  dplyr::select(c(tree_row_idx, cell_id, subcell_id, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_0_5deg, pred_GCP_share_0_5deg_rescaled, pred_GCP_0_5deg))  %>%
  left_join(pop)  %>%
  left_join(land_area)  %>%
  mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>%
  na.omit() # there is one cell for SAU that have missing data purely because of country border geom differences from different sources, ignore it.

# ---- Free loaded RData originals that are no longer needed ----
rm(predict_data_results_0_5deg_with_prov_boundary,
   land_pop_extracted_region_level_0_5deg,
   lc_full_0_5deg, pop)
gc()

# ------------------------------------------------------------------------------------------------------------------------------
# let's first try use 0 as threshold (meaning no extra adjust except for population = 0)

# no extra adjustment
pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_0_5deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_5deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_0_5deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_5deg_rescaled = ifelse(pred_GCP_share_0_5deg == 0, 0, pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl)
save(pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust.RData")

deg0_5_geometry <- read_sf("step5_predict_and_post_adjustments/outputs/country_0_5deg_intersected.gpkg")  %>%
  dplyr::select(c(cell_id, subcell_id, iso, geom)) %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) # need to put Alaska back to USA

organized_pred_0_5deg_postadjust_pop_dens_no_extra_adjust <- pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust  %>%
  group_by(iso, year, cell_id, subcell_id)  %>%
  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_5deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density = 0",
         cell_size = "0.5-deg by 0.5-deg")  %>%
  left_join(deg0_5_geometry)

# Free intermediate pred data frame (already saved to disk)
rm(pred_0_5deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust); gc()

# 0.01 next

pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_01_adjust <- pred_0_5deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_5deg = ifelse(pop_density_km2 <= 0.01, 0, pred_GCP_share_0_5deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 <= 0.01, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_5deg_rescaled = ifelse(pred_GCP_share_0_5deg == 0, 0, pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl)

organized_pred_0_5deg_postadjust_pop_dens_0_01_adjust <- pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_01_adjust  %>%
  group_by(iso, year, cell_id, subcell_id)  %>%

  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_5deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density <= 0.01 (population per cell land area in km2)",
         cell_size = "0.5-deg by 0.5-deg")  %>%
  left_join(deg0_5_geometry)

# Free intermediate pred data frame (not saved, no longer needed)
rm(pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_01_adjust); gc()

# 0.02 next
pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_02_adjust <- pred_0_5deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_5deg = ifelse(pop_density_km2 <= 0.02, 0, pred_GCP_share_0_5deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 <= 0.02, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_5deg_rescaled = ifelse(pred_GCP_share_0_5deg == 0, 0, pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl)

organized_pred_0_5deg_postadjust_pop_dens_0_02_adjust <- pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_02_adjust  %>%
  group_by(iso, year, cell_id, subcell_id)  %>%
  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_5deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density <= 0.02 (population per cell land area in km2)",
         cell_size = "0.5-deg by 0.5-deg")  %>%
  left_join(deg0_5_geometry)

# Free intermediate pred data frame (not saved, no longer needed)
rm(pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_02_adjust); gc()

# 0.05 next
pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_05_adjust <- pred_0_5deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_5deg = ifelse(pop_density_km2 <= 0.05, 0, pred_GCP_share_0_5deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 <= 0.05, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_5deg_rescaled = ifelse(pred_GCP_share_0_5deg == 0, 0, pred_GCP_share_0_5deg/sum(pred_GCP_share_0_5deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_5deg = pred_GCP_share_0_5deg_rescaled * unit_gdp_af_sum_rescl)

organized_pred_0_5deg_postadjust_pop_dens_0_05_adjust <- pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_05_adjust  %>%
  group_by(iso, year, cell_id, subcell_id)  %>%
  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_5deg_no_prov_bound = sum(pred_GCP_0_5deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, pred_GCP_0_5deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_5deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density <= 0.05 (population per cell land area in km2)",
         cell_size = "0.5-deg by 0.5-deg")  %>%
  left_join(deg0_5_geometry)

# Free intermediate pred data frame (not saved, no longer needed)
rm(pred_0_5deg_with_prov_bound_postadjust_pop_dens_0_05_adjust); gc()

# ---- Slim down pred_data and load tree predictions for uncertainty propagation ----
# Drop columns only needed for organized datasets; keep only what propagation needs
pred_0_5deg_with_prov_bound <- pred_0_5deg_with_prov_bound %>%
  dplyr::select(c(tree_row_idx, cell_id, subcell_id, id, iso, year, unit_gdp_af_sum_rescl, pop_density_km2))
gc()

load("step5_predict_and_post_adjustments/outputs/tree_preds_raw_0_5deg.RData")
cat(paste0("  Loaded raw tree predictions: ", nrow(tree_preds_raw_0_5deg), " rows, ", num_trees_0_5deg, " trees.\n"))

# Helper: propagate tree predictions through post-adjustment and compute quantiles.
# Processes one country at a time to avoid creating a full matched-row copy in memory.
# Within each country, trees are processed in column chunks.
# All operations (censor, rescale, multiply, aggregate) are within-country,
# so per-country processing produces identical results to processing all at once.
propagate_tree_uncertainty <- function(tree_preds_raw, pred_data, threshold, col_chunk_size = 100) {
  n_trees <- ncol(tree_preds_raw)
  countries <- unique(pred_data$iso)

  all_results <- list()

  for (cty in countries) {
    cty_mask <- pred_data$iso == cty
    pred_cty <- pred_data[cty_mask, ]

    # Extract only this country's rows from the raw tree predictions
    tree_shares_cty <- tree_preds_raw[pred_cty$tree_row_idx, , drop = FALSE]

    # Country-level keys
    censor_mask <- pred_cty$pop_density_km2 <= threshold
    grp <- paste(pred_cty$id, pred_cty$year, sep = "||")
    agg_key <- paste(pred_cty$iso, pred_cty$year, pred_cty$cell_id, pred_cty$subcell_id, sep = "||")
    gdp_vec <- pred_cty$unit_gdp_af_sum_rescl

    out_names <- sort(unique(agg_key))
    n_out <- length(out_names)

    # Pre-allocate country output matrix
    tree_GCP_cell <- matrix(0, nrow = n_out, ncol = n_trees)
    rownames(tree_GCP_cell) <- out_names

    # Column-chunked processing
    n_col_chunks <- ceiling(n_trees / col_chunk_size)
    for (cc in seq_len(n_col_chunks)) {
      col_start <- (cc - 1) * col_chunk_size + 1
      col_end <- min(cc * col_chunk_size, n_trees)
      cols <- col_start:col_end

      ts <- tree_shares_cty[, cols, drop = FALSE]
      ts[censor_mask, ] <- 0
      grp_sums <- rowsum(ts, grp)
      ts <- ts / grp_sums[grp, , drop = FALSE]
      ts[!is.finite(ts)] <- 0
      ts <- ts * gdp_vec
      tree_GCP_cell[, cols] <- rowsum(ts, agg_key)
      rm(ts, grp_sums); gc()
    }
    rm(tree_shares_cty); gc()

    # Compute quantiles and standard deviations for this country
    q_mat <- rowQuantiles(tree_GCP_cell, probs = c(0.01, 0.05, 0.10, 0.90, 0.95, 0.99))
    gcp_sd <- rowSds(tree_GCP_cell)

    # Exact SD of log(GDP) across trees (currency- and population-invariant)
    log_tree_GCP <- log(tree_GCP_cell)
    log_tree_GCP[!is.finite(log_tree_GCP)] <- NA
    gcp_sd_log <- rowSds(log_tree_GCP, na.rm = TRUE)
    rm(log_tree_GCP, tree_GCP_cell); gc()

    parts <- strsplit(out_names, "||", fixed = TRUE)

    all_results[[cty]] <- data.frame(
      iso = sapply(parts, `[`, 1),
      year = as.integer(sapply(parts, `[`, 2)),
      cell_id = sapply(parts, `[`, 3),
      subcell_id = as.integer(sapply(parts, `[`, 4)),
      GCP_tree_sd = gcp_sd,
      GCP_sd_log_gdp = gcp_sd_log,
      GCP_q01 = q_mat[, 1], GCP_q05 = q_mat[, 2], GCP_q10 = q_mat[, 3],
      GCP_q90 = q_mat[, 4], GCP_q95 = q_mat[, 5], GCP_q99 = q_mat[, 6],
      stringsAsFactors = FALSE
    )
    cat(paste0("    Done: ", cty, " (", n_out, " cells)\n"))
  }

  do.call(rbind, all_results)
}

# Add tree-level uncertainty for no extra adjust
unc_no_extra <- propagate_tree_uncertainty(tree_preds_raw_0_5deg, pred_0_5deg_with_prov_bound, threshold = 0)
organized_pred_0_5deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_0_5deg_postadjust_pop_dens_no_extra_adjust %>%
  left_join(unc_no_extra, by = c("iso", "year", "cell_id", "subcell_id"))

# Add tree-level uncertainty for 0.01 threshold
unc_0_01 <- propagate_tree_uncertainty(tree_preds_raw_0_5deg, pred_0_5deg_with_prov_bound, threshold = 0.01)
organized_pred_0_5deg_postadjust_pop_dens_0_01_adjust <- organized_pred_0_5deg_postadjust_pop_dens_0_01_adjust %>%
  left_join(unc_0_01, by = c("iso", "year", "cell_id", "subcell_id"))

# Add tree-level uncertainty for 0.02 threshold
unc_0_02 <- propagate_tree_uncertainty(tree_preds_raw_0_5deg, pred_0_5deg_with_prov_bound, threshold = 0.02)
organized_pred_0_5deg_postadjust_pop_dens_0_02_adjust <- organized_pred_0_5deg_postadjust_pop_dens_0_02_adjust %>%
  left_join(unc_0_02, by = c("iso", "year", "cell_id", "subcell_id"))

# Add tree-level uncertainty for 0.05 threshold
unc_0_05 <- propagate_tree_uncertainty(tree_preds_raw_0_5deg, pred_0_5deg_with_prov_bound, threshold = 0.05)
organized_pred_0_5deg_postadjust_pop_dens_0_05_adjust <- organized_pred_0_5deg_postadjust_pop_dens_0_05_adjust %>%
  left_join(unc_0_05, by = c("iso", "year", "cell_id", "subcell_id"))

# Free tree memory now that all thresholds are computed
rm(tree_preds_raw_0_5deg, pred_0_5deg_with_prov_bound, unc_no_extra, unc_0_01, unc_0_02, unc_0_05); gc()

# ------------------------------------------------------------------------------------------------------------------------------
# obtain each 0.5 deg cell population

national_population <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv")  %>%
  as.data.frame()  %>%
  dplyr::select(c(iso, year, national_population))  %>%
  distinct(iso, year, national_population, .keep_all = TRUE)  %>%
  filter(year <= 2022)

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_5deg.RData")
pop_cell_0_5deg <- land_pop_extracted_region_level_0_5deg  %>%
  filter(year <= 2022)  %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>%
  as.data.frame()  %>%
  dplyr::select(c("cell_id", "subcell_id", "id", "iso", "year", "pop"))  %>%
  left_join(land_area) %>%
  mutate(pop = ifelse(land_area_km2 == 0, 0, pop)) %>% # becasue pop should not live on water
  na.omit() %>% # there is one cell for SAU that have missing data purely because of country border geometry differences from different sources, ignore it.
  group_by(year, iso, cell_id, subcell_id)  %>%
  mutate(pop_cell = sum(pop))  %>%
  distinct(year, iso, cell_id, subcell_id, .keep_all = TRUE)  %>%
  ungroup()  %>%
  dplyr::select(c(cell_id, subcell_id, iso, year, pop_cell))  %>%
  left_join(national_population) %>%
  group_by(iso, year)  %>%
  mutate(pop_cell_rescaled = floor(ifelse(is.na(national_population), pop_cell, pop_cell*national_population/sum(pop_cell))))  %>%
  mutate(pop_cell_rescaled = ifelse(pop_cell == 0, 0, pop_cell_rescaled)) %>%
  ungroup()  %>%
  left_join(deg0_5_geometry)

save(pop_cell_0_5deg, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/pop_cell_0_5deg.RData")

# ------------------------------------------------------------------------------------------------------------------------------
# now obtain GDPC
load("step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/pop_cell_0_5deg.RData")

# no extra adjustment
GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_0_5deg_postadjust_pop_dens_no_extra_adjust  %>%
  left_join(pop_cell_0_5deg) %>%
  mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>% # in case after rescaling the pop, some places with very few population turns to 0
  mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>%
  mutate(
    GDPC_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd / pop_cell_rescaled),
    GDPC_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01 / pop_cell_rescaled),
    GDPC_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05 / pop_cell_rescaled),
    GDPC_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10 / pop_cell_rescaled),
    GDPC_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90 / pop_cell_rescaled),
    GDPC_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95 / pop_cell_rescaled),
    GDPC_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99 / pop_cell_rescaled),
    GCP_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd),
    GCP_sd_log_gdp = ifelse(pop_cell_rescaled == 0, 0, GCP_sd_log_gdp),
    GCP_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01),
    GCP_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05),
    GCP_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10),
    GCP_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90),
    GCP_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95),
    GCP_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99)
  )  %>%
  dplyr::select(-c(pop_cell))  %>%
  rename(pop_cell = pop_cell_rescaled)

save(GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.RData")

just_grid_0_5deg <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5deg_with_lon_lat.csv")

GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_csv <- GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust  %>%
  left_join(just_grid_0_5deg  %>% mutate(cell_id = as.character(cell_id), subcell_id = as.integer(subcell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.csv", row.names = FALSE)

# 0.01 threshold
GDPC_0_5deg_postadjust_pop_dens_0_01_adjust <- organized_pred_0_5deg_postadjust_pop_dens_0_01_adjust  %>%
  left_join(pop_cell_0_5deg) %>%
  mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
  mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>%
  mutate(
    GDPC_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd / pop_cell_rescaled),
    GDPC_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01 / pop_cell_rescaled),
    GDPC_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05 / pop_cell_rescaled),
    GDPC_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10 / pop_cell_rescaled),
    GDPC_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90 / pop_cell_rescaled),
    GDPC_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95 / pop_cell_rescaled),
    GDPC_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99 / pop_cell_rescaled),
    GCP_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd),
    GCP_sd_log_gdp = ifelse(pop_cell_rescaled == 0, 0, GCP_sd_log_gdp),
    GCP_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01),
    GCP_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05),
    GCP_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10),
    GCP_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90),
    GCP_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95),
    GCP_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99)
  )  %>%
  dplyr::select(-c(pop_cell))  %>%
  rename(pop_cell = pop_cell_rescaled)

# also generate csv file, instead of giving geometry, give longitude and latitude of the bottom-left corner of each cell
GDPC_0_5deg_postadjust_pop_dens_0_01_adjust_csv <- GDPC_0_5deg_postadjust_pop_dens_0_01_adjust  %>%
  left_join(just_grid_0_5deg  %>% mutate(cell_id = as.character(cell_id), subcell_id = as.integer(subcell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_5deg_postadjust_pop_dens_0_01_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_0_01_adjust.csv", row.names = FALSE)

# 0.02 threshold
GDPC_0_5deg_postadjust_pop_dens_0_02_adjust <- organized_pred_0_5deg_postadjust_pop_dens_0_02_adjust  %>%
  left_join(pop_cell_0_5deg) %>%
  mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
  mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>%
  mutate(
    GDPC_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd / pop_cell_rescaled),
    GDPC_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01 / pop_cell_rescaled),
    GDPC_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05 / pop_cell_rescaled),
    GDPC_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10 / pop_cell_rescaled),
    GDPC_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90 / pop_cell_rescaled),
    GDPC_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95 / pop_cell_rescaled),
    GDPC_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99 / pop_cell_rescaled),
    GCP_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd),
    GCP_sd_log_gdp = ifelse(pop_cell_rescaled == 0, 0, GCP_sd_log_gdp),
    GCP_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01),
    GCP_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05),
    GCP_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10),
    GCP_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90),
    GCP_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95),
    GCP_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99)
  )  %>%
  dplyr::select(-c(pop_cell))  %>%
  rename(pop_cell = pop_cell_rescaled)

# also generate csv file, instead of giving geometry, give longitude and latitude of the bottom-left corner of each cell
GDPC_0_5deg_postadjust_pop_dens_0_02_adjust_csv <- GDPC_0_5deg_postadjust_pop_dens_0_02_adjust  %>%
  left_join(just_grid_0_5deg  %>% mutate(cell_id = as.character(cell_id), subcell_id = as.integer(subcell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_5deg_postadjust_pop_dens_0_02_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_0_02_adjust.csv", row.names = FALSE)

# 0.05 threshold
GDPC_0_5deg_postadjust_pop_dens_0_05_adjust <- organized_pred_0_5deg_postadjust_pop_dens_0_05_adjust  %>%
  left_join(pop_cell_0_5deg) %>%
  mutate(predicted_GCP = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP)) %>%
  mutate(cell_GDPC = ifelse(pop_cell_rescaled == 0, 0, predicted_GCP/pop_cell_rescaled))  %>%
  mutate(
    GDPC_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd / pop_cell_rescaled),
    GDPC_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01 / pop_cell_rescaled),
    GDPC_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05 / pop_cell_rescaled),
    GDPC_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10 / pop_cell_rescaled),
    GDPC_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90 / pop_cell_rescaled),
    GDPC_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95 / pop_cell_rescaled),
    GDPC_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99 / pop_cell_rescaled),
    GCP_tree_sd = ifelse(pop_cell_rescaled == 0, 0, GCP_tree_sd),
    GCP_sd_log_gdp = ifelse(pop_cell_rescaled == 0, 0, GCP_sd_log_gdp),
    GCP_q01 = ifelse(pop_cell_rescaled == 0, 0, GCP_q01),
    GCP_q05 = ifelse(pop_cell_rescaled == 0, 0, GCP_q05),
    GCP_q10 = ifelse(pop_cell_rescaled == 0, 0, GCP_q10),
    GCP_q90 = ifelse(pop_cell_rescaled == 0, 0, GCP_q90),
    GCP_q95 = ifelse(pop_cell_rescaled == 0, 0, GCP_q95),
    GCP_q99 = ifelse(pop_cell_rescaled == 0, 0, GCP_q99)
  )  %>%
  dplyr::select(-c(pop_cell))  %>%
  rename(pop_cell = pop_cell_rescaled)

# also generate csv file, instead of giving geometry, give longitude and latitude of the bottom-left corner of each cell
GDPC_0_5deg_postadjust_pop_dens_0_05_adjust_csv <- GDPC_0_5deg_postadjust_pop_dens_0_05_adjust  %>%
  left_join(just_grid_0_5deg  %>% mutate(cell_id = as.character(cell_id), subcell_id = as.integer(subcell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_5deg_postadjust_pop_dens_0_05_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_5deg_postadjust_pop_dens_0_05_adjust.csv", row.names = FALSE)
