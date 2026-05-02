# --------------------------------- Task Summary --------------------------------- #
# This file performs post-adjustment on the predicted 0.25-degree cell GDP values, and propagates per-tree predictions through the same censor-rescale-aggregate pipeline to produce cell-level uncertainty quantiles and standard deviations.
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
library(matrixStats)

# ------------------------------------------------------------------------------------------------------------------------------
# Model 9.1: 0.25deg

# obtain predicted GDP data
load("step5_predict_and_post_adjustments/outputs/predict_data_results_0_25deg_with_prov_boundary.RData")

# load population
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_25deg.RData")
pop <- land_pop_extracted_region_level_0_25deg  %>%
  filter(year <= 2022)  %>%
  as.data.frame()  %>%
  dplyr::select(c("cell_id", "subcell_id", "subcell_id_0_25", "id", "iso", "year", "pop"))  %>%
  mutate(pop = floor(pop)) %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso))

# load land area:
# Note: the land area calculated is the area in square km based on a spherical approximation of the Earth

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_25deg.RData")
land_area <- lc_full_0_25deg  %>%
  filter(year <= 2022)  %>%
  as.data.frame()  %>%
  dplyr::select(c("cell_id", "subcell_id", "subcell_id_0_25", "id", "iso", "year", "water", "barren", "snow_ice", "urban", "dense_forest",
                  "open_forest", "forest_cropland", "herbaceous", "cropland", "shrub", "herbaceous_cropland"))  %>%
  replace(is.na(.), 0)  %>%
  mutate(land_area_km2 = barren + snow_ice + urban + dense_forest + open_forest +
           forest_cropland + herbaceous + cropland + shrub + herbaceous_cropland)  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, id, iso, year, land_area_km2)) %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso))

# ------------------------------------------------------------------------------------------------------------------------------

# load GDP
# Note: here I also want the area in square km based on a spherical approximation of the Earth

pred_0_25deg_with_prov_bound <- predict_data_results_0_25deg_with_prov_boundary %>%
  dplyr::select(c(tree_row_idx, cell_id, subcell_id, subcell_id_0_25, id, iso, year, unit_gdp_af_sum_rescl, pred_GCP_share_0_25deg, pred_GCP_share_0_25deg_rescaled, pred_GCP_0_25deg))  %>%
  left_join(pop)  %>%
  left_join(land_area)  %>%
  mutate(pop_density_km2 = ifelse(land_area_km2 == 0, 0, pop/land_area_km2)) %>%
  na.omit() # there is one cell for SAU that have missing data purely because of country border geom differences from different sources, ignore it.

# ---- Free loaded RData originals that are no longer needed ----
rm(predict_data_results_0_25deg_with_prov_boundary,
   land_pop_extracted_region_level_0_25deg,
   lc_full_0_25deg, pop)
gc()

# ---- Load tree-level predictions for uncertainty propagation ----
load("step5_predict_and_post_adjustments/outputs/tree_preds_raw_0_25deg.RData")

# Match rows using saved row index (na.omit may have dropped some rows)
tree_shares_matched <- tree_preds_raw_0_25deg[pred_0_25deg_with_prov_bound$tree_row_idx, ]
rm(tree_preds_raw_0_25deg); gc()
cat(paste0("  Matched ", nrow(tree_shares_matched), " rows, ", num_trees_0_25deg, " trees.\n"))

# Helper: propagate tree predictions through post-adjustment and compute quantiles.
# Processes trees in column chunks to stay within memory limits.
# Each operation (censor, rescale, multiply, aggregate) is independent per column,
# so chunking produces identical results to processing all columns at once.
propagate_tree_uncertainty <- function(tree_shares, pred_data, threshold, chunk_size = 100) {
  n_trees <- ncol(tree_shares)

  # Pre-compute masks and keys (shared across all tree chunks)
  censor_mask <- pred_data$pop_density_km2 <= threshold
  grp <- paste(pred_data$id, pred_data$year, sep = "||")
  agg_key <- paste(pred_data$iso, pred_data$year, pred_data$cell_id, pred_data$subcell_id, pred_data$subcell_id_0_25, sep = "||")
  gdp_vec <- pred_data$unit_gdp_af_sum_rescl

  # Determine output row structure
  out_names <- sort(unique(agg_key))
  n_out <- length(out_names)

  # Pre-allocate aggregated output matrix (n_cells x n_trees — much smaller than input)
  tree_GCP_cell <- matrix(0, nrow = n_out, ncol = n_trees)
  rownames(tree_GCP_cell) <- out_names

  n_chunks <- ceiling(n_trees / chunk_size)
  for (ch in seq_len(n_chunks)) {
    col_start <- (ch - 1) * chunk_size + 1
    col_end <- min(ch * chunk_size, n_trees)
    cols <- col_start:col_end

    # Extract column chunk (fresh matrix with refcount == 1)
    ts <- tree_shares[, cols, drop = FALSE]

    # 1. Censor cells below pop density threshold
    ts[censor_mask, ] <- 0

    # 2. Rescale within (id, year) groups
    grp_sums <- rowsum(ts, grp)
    ts <- ts / grp_sums[grp, , drop = FALSE]
    ts[!is.finite(ts)] <- 0

    # 3. Multiply by province/country GDP
    ts <- ts * gdp_vec

    # 4. Aggregate across province boundaries within (iso, year, cell_id, subcell_id, subcell_id_0_25)
    tree_GCP_cell[, cols] <- rowsum(ts, agg_key)
    rm(ts, grp_sums); gc()
  }

  # 5. Compute quantiles and standard deviations across trees
  q_mat <- rowQuantiles(tree_GCP_cell, probs = c(0.01, 0.05, 0.10, 0.90, 0.95, 0.99))
  gcp_sd <- rowSds(tree_GCP_cell)

  # Exact SD of log(GDP) across trees (currency- and population-invariant)
  log_tree_GCP <- log(tree_GCP_cell)
  log_tree_GCP[!is.finite(log_tree_GCP)] <- NA
  gcp_sd_log <- rowSds(log_tree_GCP, na.rm = TRUE)
  rm(log_tree_GCP, tree_GCP_cell); gc()

  # Parse aggregation key back to identifiers
  parts <- strsplit(out_names, "||", fixed = TRUE)

  data.frame(
    iso = sapply(parts, `[`, 1),
    year = as.integer(sapply(parts, `[`, 2)),
    cell_id = sapply(parts, `[`, 3),
    subcell_id = as.integer(sapply(parts, `[`, 4)),
    subcell_id_0_25 = as.integer(sapply(parts, `[`, 5)),
    GCP_tree_sd = gcp_sd,
    GCP_sd_log_gdp = gcp_sd_log,
    GCP_q01 = q_mat[, 1], GCP_q05 = q_mat[, 2], GCP_q10 = q_mat[, 3],
    GCP_q90 = q_mat[, 4], GCP_q95 = q_mat[, 5], GCP_q99 = q_mat[, 6],
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------------------------------------------------------
# let's first try use 0 as threshold (meaning no extra adjust except for population = 0)

# no extra adjustment
pred_0_25deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust <- pred_0_25deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_25deg = ifelse(pop_density_km2 <= 0, 0, pred_GCP_share_0_25deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 == 0, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_25deg_rescaled = ifelse(pred_GCP_share_0_25deg == 0, 0, pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl)
save(pred_0_25deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/pred_0_25deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust.RData")

deg0_25_geometry <- read_sf("step5_predict_and_post_adjustments/outputs/country_0_25deg_intersected.gpkg")  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, geom)) %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) # need to put Alaska back to USA

organized_pred_0_25deg_postadjust_pop_dens_no_extra_adjust <- pred_0_25deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust  %>%
  group_by(iso, year, cell_id, subcell_id, subcell_id_0_25)  %>%
  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_25deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pred_GCP_0_25deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, subcell_id_0_25, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_25deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density = 0",
         cell_size = "0.25-deg by 0.25-deg")  %>%
  left_join(deg0_25_geometry)

# Free intermediate pred data frame (already saved to disk)
rm(pred_0_25deg_with_prov_bound_postadjust_pop_dens_no_extra_adjust); gc()

# Add tree-level uncertainty for no extra adjust
unc_no_extra <- propagate_tree_uncertainty(tree_shares_matched, pred_0_25deg_with_prov_bound, threshold = 0)
organized_pred_0_25deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_0_25deg_postadjust_pop_dens_no_extra_adjust %>%
  left_join(unc_no_extra, by = c("iso", "year", "cell_id", "subcell_id", "subcell_id_0_25"))

# 0.01 next

pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_01_adjust <- pred_0_25deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_25deg = ifelse(pop_density_km2 <= 0.01, 0, pred_GCP_share_0_25deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 <= 0.01, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_25deg_rescaled = ifelse(pred_GCP_share_0_25deg == 0, 0, pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl)

organized_pred_0_25deg_postadjust_pop_dens_0_01_adjust <- pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_01_adjust  %>%
  group_by(iso, year, cell_id, subcell_id, subcell_id_0_25)  %>%
  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_25deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pred_GCP_0_25deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, subcell_id_0_25, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_25deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density <= 0.01 (population per cell land area in km2)",
         cell_size = "0.25-deg by 0.25-deg")  %>%
  left_join(deg0_25_geometry)

# Free intermediate pred data frame (not saved, no longer needed)
rm(pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_01_adjust); gc()

# Add tree-level uncertainty for 0.01 threshold
unc_0_01 <- propagate_tree_uncertainty(tree_shares_matched, pred_0_25deg_with_prov_bound, threshold = 0.01)
organized_pred_0_25deg_postadjust_pop_dens_0_01_adjust <- organized_pred_0_25deg_postadjust_pop_dens_0_01_adjust %>%
  left_join(unc_0_01, by = c("iso", "year", "cell_id", "subcell_id", "subcell_id_0_25"))

# 0.02 next
pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_02_adjust <- pred_0_25deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_25deg = ifelse(pop_density_km2 <= 0.02, 0, pred_GCP_share_0_25deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 <= 0.02, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_25deg_rescaled = ifelse(pred_GCP_share_0_25deg == 0, 0, pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl)

organized_pred_0_25deg_postadjust_pop_dens_0_02_adjust <- pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_02_adjust  %>%
  group_by(iso, year, cell_id, subcell_id, subcell_id_0_25)  %>%
  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_25deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pred_GCP_0_25deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, subcell_id_0_25, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_25deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density <= 0.02 (population per cell land area in km2)",
         cell_size = "0.25-deg by 0.25-deg")  %>%
  left_join(deg0_25_geometry)

# Free intermediate pred data frame (not saved, no longer needed)
rm(pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_02_adjust); gc()

# Add tree-level uncertainty for 0.02 threshold
unc_0_02 <- propagate_tree_uncertainty(tree_shares_matched, pred_0_25deg_with_prov_bound, threshold = 0.02)
organized_pred_0_25deg_postadjust_pop_dens_0_02_adjust <- organized_pred_0_25deg_postadjust_pop_dens_0_02_adjust %>%
  left_join(unc_0_02, by = c("iso", "year", "cell_id", "subcell_id", "subcell_id_0_25"))

# 0.05 next
pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_05_adjust <- pred_0_25deg_with_prov_bound  %>%
  mutate(pred_GCP_share_0_25deg = ifelse(pop_density_km2 <= 0.05, 0, pred_GCP_share_0_25deg))  %>%
  mutate(is_censored = ifelse(pop_density_km2 <= 0.05, 1, 0))  %>%
  group_by(id, year)  %>%
  mutate(pred_GCP_share_0_25deg_rescaled = ifelse(pred_GCP_share_0_25deg == 0, 0, pred_GCP_share_0_25deg/sum(pred_GCP_share_0_25deg)))  %>%
  ungroup()  %>%
  mutate(pred_GCP_0_25deg = pred_GCP_share_0_25deg_rescaled * unit_gdp_af_sum_rescl)

organized_pred_0_25deg_postadjust_pop_dens_0_05_adjust <- pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_05_adjust  %>%
  group_by(iso, year, cell_id, subcell_id, subcell_id_0_25)  %>%
  mutate(is_cell_censored = ifelse(any(is_censored == 1), 1, 0))  %>%
  mutate(pred_GCP_0_25deg_no_prov_bound = sum(pred_GCP_0_25deg))  %>%
  ungroup()  %>%
  as.data.frame()  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pred_GCP_0_25deg_no_prov_bound, is_cell_censored))  %>%
  distinct(iso, year, cell_id, subcell_id, subcell_id_0_25, .keep_all = TRUE)  %>%
  rename(predicted_GCP = pred_GCP_0_25deg_no_prov_bound)  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, predicted_GCP, is_cell_censored))  %>%
  mutate(method = "post-adjust zero GDP for pop density <= 0.05 (population per cell land area in km2)",
         cell_size = "0.25-deg by 0.25-deg")  %>%
  left_join(deg0_25_geometry)

# Free intermediate pred data frame (not saved, no longer needed)
rm(pred_0_25deg_with_prov_bound_postadjust_pop_dens_0_05_adjust); gc()

# Add tree-level uncertainty for 0.05 threshold
unc_0_05 <- propagate_tree_uncertainty(tree_shares_matched, pred_0_25deg_with_prov_bound, threshold = 0.05)
organized_pred_0_25deg_postadjust_pop_dens_0_05_adjust <- organized_pred_0_25deg_postadjust_pop_dens_0_05_adjust %>%
  left_join(unc_0_05, by = c("iso", "year", "cell_id", "subcell_id", "subcell_id_0_25"))

# Free tree memory now that all thresholds are computed
rm(tree_shares_matched, unc_no_extra, unc_0_01, unc_0_02, unc_0_05); gc()

# ------------------------------------------------------------------------------------------------------------------------------
# obtain each 0.25 deg cell population

national_population <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv")  %>%
  as.data.frame()  %>%
  dplyr::select(c(iso, year, national_population))  %>%
  distinct(iso, year, national_population, .keep_all = TRUE)  %>%
  filter(year <= 2022)

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_25deg.RData")
pop_cell_0_25deg <- land_pop_extracted_region_level_0_25deg  %>%
  filter(year <= 2022)  %>%
  mutate(iso = ifelse(iso == "Ala", "USA", iso)) %>%
  as.data.frame()  %>%
  dplyr::select(c("cell_id", "subcell_id", "subcell_id_0_25", "id", "iso", "year", "pop"))  %>%
  left_join(land_area) %>%
  mutate(pop = ifelse(land_area_km2 == 0, 0, pop)) %>% # becasue pop should not live on water
  na.omit() %>% # there is one cell for SAU that have missing data purely because of country border geometry differences from different sources, ignore it.
  group_by(year, iso, cell_id, subcell_id, subcell_id_0_25)  %>%
  mutate(pop_cell = sum(pop))  %>%
  distinct(year, iso, cell_id, subcell_id, subcell_id_0_25, .keep_all = TRUE)  %>%
  ungroup()  %>%
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso, year, pop_cell))  %>%
  left_join(national_population) %>%
  group_by(iso, year)  %>%
  mutate(pop_cell_rescaled = floor(ifelse(is.na(national_population), pop_cell, pop_cell*national_population/sum(pop_cell))))  %>%
  mutate(pop_cell_rescaled = ifelse(pop_cell == 0, 0, pop_cell_rescaled)) %>%
  ungroup()  %>%
  left_join(deg0_25_geometry)

save(pop_cell_0_25deg, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/pop_cell_0_25deg.RData")

# ------------------------------------------------------------------------------------------------------------------------------
# now obtain GDPC
load("step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/pop_cell_0_25deg.RData")

# no extra adjustment
GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust <- organized_pred_0_25deg_postadjust_pop_dens_no_extra_adjust  %>%
  left_join(pop_cell_0_25deg) %>%
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

save(GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.RData")

just_grid_0_25deg <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_25deg_with_lon_lat.csv")

GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_csv <- GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust  %>%
  left_join(just_grid_0_25deg  %>% mutate(cell_id = as.character(cell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.csv", row.names = FALSE)

# 0.01 threshold
GDPC_0_25deg_postadjust_pop_dens_0_01_adjust <- organized_pred_0_25deg_postadjust_pop_dens_0_01_adjust  %>%
  left_join(pop_cell_0_25deg) %>%
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
GDPC_0_25deg_postadjust_pop_dens_0_01_adjust_csv <- GDPC_0_25deg_postadjust_pop_dens_0_01_adjust  %>%
  left_join(just_grid_0_25deg  %>% mutate(cell_id = as.character(cell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_25deg_postadjust_pop_dens_0_01_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_0_01_adjust.csv", row.names = FALSE)

# 0.02 threshold
GDPC_0_25deg_postadjust_pop_dens_0_02_adjust <- organized_pred_0_25deg_postadjust_pop_dens_0_02_adjust  %>%
  left_join(pop_cell_0_25deg) %>%
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
GDPC_0_25deg_postadjust_pop_dens_0_02_adjust_csv <- GDPC_0_25deg_postadjust_pop_dens_0_02_adjust  %>%
  left_join(just_grid_0_25deg  %>% mutate(cell_id = as.character(cell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_25deg_postadjust_pop_dens_0_02_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_0_02_adjust.csv", row.names = FALSE)

# 0.05 threshold
GDPC_0_25deg_postadjust_pop_dens_0_05_adjust <- organized_pred_0_25deg_postadjust_pop_dens_0_05_adjust  %>%
  left_join(pop_cell_0_25deg) %>%
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
GDPC_0_25deg_postadjust_pop_dens_0_05_adjust_csv <- GDPC_0_25deg_postadjust_pop_dens_0_05_adjust  %>%
  left_join(just_grid_0_25deg  %>% mutate(cell_id = as.character(cell_id)))  %>%
  as.data.frame()  %>%
  dplyr::select(-c(geom))
write.csv(GDPC_0_25deg_postadjust_pop_dens_0_05_adjust_csv, file = "step5_predict_and_post_adjustments/outputs/predict_data_results_postadjust_pop_density/GDPC_0_25deg_postadjust_pop_dens_0_05_adjust.csv", row.names = FALSE)
