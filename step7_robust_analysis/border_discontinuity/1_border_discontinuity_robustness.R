# --------------------------------- Task Summary --------------------------------- #
# This file checks whether predicted GDP exhibits artificial discontinuities at country borders by comparing within-country versus across-border neighboring cells at all three grid resolutions (Online Appendix Section 9.1).
# -------------------------------------------------------------------------------- #

Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(sf)
library(dplyr)
library(sandwich)
library(lmtest)

output_dir <- "step7_robust_analysis/border_discontinuity/outputs"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------------------------------------------------------------------

overall_r2_levl <- function(true_values, predicted_values) {
  valid <- true_values > 0 & predicted_values > 0
  if (sum(valid) < 2) return(NA)
  true_log <- log(true_values[valid])
  predicted_log <- log(predicted_values[valid])
  1 - (sum((true_log - predicted_log)^2) / sum((true_log - mean(true_log))^2))
}

fmt4 <- function(x) {
  if (is.na(x)) return("---")
  formatC(x, format = "f", digits = 4)
}

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.1) return("$^{*}$")
  return("")
}

fmt_coef <- function(x, p, digits = 4) {
  paste0(formatC(x, format = "f", digits = digits), stars_fn(p))
}

fmt_n <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Degree-level configurations
# ---------------------------------------------------------------------------------------------------------------------------------------

deg_configs <- list(
  list(
    label = "1deg", deg_label = "1-degree Model", cell_step = 1,
    pred_file = "step5_predict_and_post_adjustments/outputs/final_output_dataset_with_uncertainty/final_GDPC_1deg_postadjust_pop_dens_no_extra_adjust.RData",
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_1deg.csv",
    train_prefix = "step4_benchmark_model/outputs/",
    train_suffix = "_1deg.csv",
    grid_file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1deg_with_lon_lat.csv",
    true_gdp_col = "GCP_1deg",
    oos_gdp_col = "GCP_1deg",
    id_cols = "cell_id",
    extra_char_cols = character(0)
  ),
  list(
    label = "0_5deg", deg_label = "0.5-degree Model", cell_step = 0.5,
    pred_file = "step5_predict_and_post_adjustments/outputs/final_output_dataset_with_uncertainty/final_GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.RData",
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_5deg.csv",
    train_prefix = "step4_benchmark_model/outputs/",
    train_suffix = "_0_5deg.csv",
    grid_file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_5deg_with_lon_lat.csv",
    true_gdp_col = "GCP_0_5deg",
    oos_gdp_col = "GCP_0_5deg",
    id_cols = c("cell_id", "subcell_id"),
    extra_char_cols = "subcell_id"
  ),
  list(
    label = "0_25deg", deg_label = "0.25-degree Model", cell_step = 0.25,
    pred_file = "step5_predict_and_post_adjustments/outputs/final_output_dataset_with_uncertainty/final_GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.RData",
    oos_file = "step4_benchmark_model/outputs/model9_tuning/put_all_isos_to_train/oos_cv_predictions_0_25deg.csv",
    train_prefix = "step4_benchmark_model/outputs/",
    train_suffix = "_0_25deg.csv",
    grid_file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_0_25deg_with_lon_lat.csv",
    true_gdp_col = "GCP_0_25deg",
    oos_gdp_col = "GCP_0_25deg",
    id_cols = c("cell_id", "subcell_id", "subcell_id_0_25"),
    extra_char_cols = c("subcell_id", "subcell_id_0_25")
  )
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Main loop: process each degree level
# ---------------------------------------------------------------------------------------------------------------------------------------

results <- list()

for (cfg in deg_configs) {
  cat(paste0("\n==================== Processing ", cfg$label, " ====================\n"))

  # --- 1. Load training data ---
  cat("  Loading training data...\n")
  train_names <- c("new_data_train", "new_data_valid_year", "new_data_valid_iso",
                   "new_data_test_year", "new_data_test_iso")
  train_files <- paste0(cfg$train_prefix, train_names, cfg$train_suffix)
  data_full <- bind_rows(lapply(train_files, read.csv))
  data_full$cell_id <- as.character(data_full$cell_id)
  for (col in cfg$extra_char_cols) data_full[[col]] <- as.character(data_full[[col]])
  data_full$iso_country <- ifelse(substr(data_full$iso, 1, 4) == "USA_", "USA", data_full$iso)

  # --- 2. Load grid coordinates ---
  cat("  Loading grid coordinates...\n")
  grid_coords <- read.csv(cfg$grid_file)
  grid_coords$cell_id <- as.character(grid_coords$cell_id)
  for (col in cfg$extra_char_cols) grid_coords[[col]] <- as.character(grid_coords[[col]])

  # --- 3. Helper: composite cell key ---
  make_key <- function(df) {
    if (length(cfg$id_cols) == 1) return(df[[cfg$id_cols[1]]])
    do.call(paste, c(df[cfg$id_cols], sep = "_"))
  }

  # --- 4. Identify border cells ---
  cat("  Identifying border cells...\n")

  neighbor_offsets <- expand.grid(
    dlon = c(-cfg$cell_step, 0, cfg$cell_step),
    dlat = c(-cfg$cell_step, 0, cfg$cell_step)
  ) %>% filter(!(dlon == 0 & dlat == 0))

  # -- Country borders (all countries including USA at country level) --
  cat("    Detecting country borders...\n")
  cell_iso_country <- data_full %>%
    distinct(across(all_of(c(cfg$id_cols, "iso_country")))) %>%
    left_join(grid_coords, by = cfg$id_cols)

  cell_country <- cell_iso_country %>%
    distinct(across(all_of(c(cfg$id_cols, "longitude", "latitude", "iso_country"))))

  cell_neighbors_country <- cell_country %>%
    crossing(neighbor_offsets) %>%
    mutate(neighbor_lon = longitude + dlon, neighbor_lat = latitude + dlat) %>%
    left_join(
      cell_country %>% select(neighbor_lon = longitude, neighbor_lat = latitude, neighbor_iso = iso_country),
      by = c("neighbor_lon", "neighbor_lat")
    ) %>%
    filter(!is.na(neighbor_iso) & neighbor_iso != iso_country)

  country_border_keys <- unique(make_key(cell_neighbors_country))
  cat(sprintf("    Country border cells: %d\n", length(country_border_keys)))

  # -- US state borders --
  cat("    Detecting US state borders...\n")
  cell_iso_state <- data_full %>%
    filter(substr(iso, 1, 4) == "USA_") %>%
    distinct(across(all_of(c(cfg$id_cols, "iso")))) %>%
    left_join(grid_coords, by = cfg$id_cols)

  cell_state <- cell_iso_state %>%
    distinct(across(all_of(c(cfg$id_cols, "longitude", "latitude", "iso"))))

  cell_neighbors_state <- cell_state %>%
    crossing(neighbor_offsets) %>%
    mutate(neighbor_lon = longitude + dlon, neighbor_lat = latitude + dlat) %>%
    left_join(
      cell_state %>% select(neighbor_lon = longitude, neighbor_lat = latitude, neighbor_iso = iso),
      by = c("neighbor_lon", "neighbor_lat")
    ) %>%
    filter(!is.na(neighbor_iso) & neighbor_iso != iso)

  state_border_keys <- unique(make_key(cell_neighbors_state))
  cat(sprintf("    US state border cells: %d\n", length(state_border_keys)))

  # --- 5. Load predicted GDP (geometry only) and OOS CV predictions ---
  cat("  Loading predicted GDP (for geometry) and OOS CV predictions...\n")
  .env <- new.env()
  .loaded <- load(cfg$pred_file, envir = .env)
  df_pred <- .env[[.loaded[1]]]
  rm(.env, .loaded)

  # Load OOS cross-validation predictions (country-held-out, no spatial/temporal contamination)
  oos_raw <- read.csv(cfg$oos_file)
  oos_raw$cell_id <- as.character(oos_raw$cell_id)
  for (col in cfg$extra_char_cols) oos_raw[[col]] <- as.character(oos_raw[[col]])

  # --- 5b. Extract cell area from geometry (country-clipped polygons) ---
  # The geom column comes from country_{resolution}_intersected.gpkg, so each
  # (cell_id, iso) pair has a polygon clipped to that country's boundary.
  # Geometry is constant across years, so we extract from one year only.
  cat("  Extracting cell area from geometry...\n")

  one_year <- df_pred %>%
    filter(year == min(year)) %>%
    mutate(cell_id = as.character(cell_id))
  for (col in cfg$extra_char_cols) one_year[[col]] <- as.character(one_year[[col]])

  # The s2 spherical geometry engine is strict about edge crossings in clipped
  # polygons. Temporarily disable s2 and use GEOS planar ops instead, which
  # handle these geometries correctly after st_make_valid().
  s2_was_on <- sf_use_s2()
  sf_use_s2(FALSE)

  cell_area_sf <- one_year %>%
    select(all_of(c(cfg$id_cols, "iso")), geom) %>%
    st_make_valid()
  cell_area_lookup <- cell_area_sf %>%
    mutate(cell_area_km2 = abs(as.numeric(st_area(geom))) / 1e6) %>%  # m^2 -> km^2
    as.data.frame() %>%
    select(all_of(c(cfg$id_cols, "iso")), cell_area_km2)

  sf_use_s2(s2_was_on)  # restore original s2 setting
  rm(cell_area_sf)

  rm(one_year)

  # --- Validation: geometry correctly reflects country-clipped sub-cells ---
  # Check 1: cells appearing in multiple ISOs should have different areas
  #           (if they were full grid cells, areas would be identical)
  multi_iso_cells <- cell_area_lookup %>%
    group_by(across(all_of(cfg$id_cols))) %>%
    filter(n_distinct(iso) > 1) %>%
    ungroup()

  if (nrow(multi_iso_cells) > 0) {
    # For cells in >1 ISO, check that areas differ (clipped, not full grid)
    area_varies <- multi_iso_cells %>%
      group_by(across(all_of(cfg$id_cols))) %>%
      summarise(n_iso = n_distinct(iso),
                n_distinct_area = n_distinct(round(cell_area_km2, 2)),
                total_area = sum(cell_area_km2),
                .groups = "drop")
    n_multi <- nrow(area_varies)
    n_area_varies <- sum(area_varies$n_distinct_area > 1)
    cat(sprintf("    Cells in >1 ISO: %d | With differing sub-cell areas: %d (%.0f%%)\n",
                n_multi, n_area_varies, n_area_varies / n_multi * 100))
  } else {
    cat("    No cells found in multiple ISOs (all cells are interior).\n")
  }

  # Check 2: expected full cell area at equator for this resolution
  full_cell_area_equator_km2 <- (cfg$cell_step * 111.32)^2
  cat(sprintf("    Full cell area at equator: ~%.0f km2 | Median sub-cell area: %.0f km2\n",
              full_cell_area_equator_km2, median(cell_area_lookup$cell_area_km2)))

  # Check 3: border cells should have smaller median area than interior cells
  area_with_border <- cell_area_lookup %>%
    mutate(border_key = make_key(.),
           is_any_border = border_key %in% country_border_keys | border_key %in% state_border_keys)
  cat(sprintf("    Median area -- border cells: %.0f km2 | interior cells: %.0f km2\n",
              median(area_with_border$cell_area_km2[area_with_border$is_any_border]),
              median(area_with_border$cell_area_km2[!area_with_border$is_any_border])))
  rm(area_with_border)

  # --- 6. Aggregate OOS predictions to country level and compute errors ---
  cat("  Computing prediction errors from OOS CV predictions...\n")

  # OOS CSV has state-level ISOs (e.g., "USA_CA"); aggregate to country level
  oos_country <- oos_raw %>%
    mutate(iso_country = ifelse(substr(iso, 1, 4) == "USA_", "USA", iso)) %>%
    group_by(across(all_of(c("iso_country", cfg$id_cols, "year")))) %>%
    summarise(oos_predicted_GCP = sum(oos_predicted_GCP),
              true_GCP = sum(.data[[cfg$oos_gdp_col]]),
              pop_total_share = max(pop_total_share),
              .groups = "drop") %>%
    rename(iso = iso_country)

  all_data <- oos_country %>%
    filter(true_GCP > 0 & oos_predicted_GCP > 0) %>%
    mutate(
      border_key = make_key(.),
      log_error = log(oos_predicted_GCP) - log(true_GCP),
      abs_log_error = abs(log_error),
      is_country_border = border_key %in% country_border_keys,
      is_state_border = border_key %in% state_border_keys,
      is_any_border = is_country_border | is_state_border,
      is_usa = iso == "USA"
    )

  # --- 7. Add cell area ---
  all_data <- all_data %>%
    left_join(cell_area_lookup, by = c(cfg$id_cols, "iso")) %>%
    mutate(log_cell_area_km2 = log(cell_area_km2))

  # --- 8. Add finest-level ISO for FE (state for USA, country for others) ---
  usa_finest <- data_full %>%
    filter(substr(iso, 1, 4) == "USA_") %>%
    distinct(across(all_of(c(cfg$id_cols, "iso")))) %>%
    group_by(across(all_of(cfg$id_cols))) %>%
    slice(1) %>%
    ungroup() %>%
    rename(finest_iso = iso)

  all_data <- all_data %>%
    left_join(usa_finest, by = cfg$id_cols) %>%
    mutate(iso_fe = factor(ifelse(is_usa, finest_iso, iso))) %>%
    select(-finest_iso)

  cat(sprintf("  Total obs: %d | Border: %d (%.1f%%) | Interior: %d (%.1f%%)\n",
              nrow(all_data),
              sum(all_data$is_any_border), mean(all_data$is_any_border) * 100,
              sum(!all_data$is_any_border), mean(!all_data$is_any_border) * 100))

  # --- 9. R-squared by group ---
  border_data   <- all_data %>% filter(is_any_border)
  interior_data <- all_data %>% filter(!is_any_border)

  r2_border   <- overall_r2_levl(border_data$true_GCP,   border_data$oos_predicted_GCP)
  r2_interior <- overall_r2_levl(interior_data$true_GCP, interior_data$oos_predicted_GCP)
  r2_all      <- overall_r2_levl(all_data$true_GCP,      all_data$oos_predicted_GCP)

  cat(sprintf("  R2 log level -- Border: %.4f | Interior: %.4f | All: %.4f\n",
              r2_border, r2_interior, r2_all))

  # --- 10. Regression with cluster-robust SEs (clustered at cell level) ---
  # Dep var: absolute log error — captures accuracy (larger errors in either direction),
  # not just systematic bias, which is the referee's concern about border discontinuities.
  # The border dummy is time-invariant at the cell level and the same cell appears
  # up to 11 times (2012-2022), so plain OLS SEs would understate uncertainty.
  # We cluster on the composite cell identifier (border_key, built via make_key).
  reg_data <- all_data %>%
    filter(is.finite(abs_log_error) &
           is.finite(pop_total_share) & pop_total_share > 0 &
           is.finite(log_cell_area_km2) & !is.na(iso_fe))

  reg <- lm(abs_log_error ~ is_any_border + pop_total_share + log_cell_area_km2 + iso_fe,
            data = reg_data)
  stopifnot(nrow(reg$model) == nrow(reg_data))
  s <- summary(reg)

  vcov_cl <- sandwich::vcovCL(reg, cluster = reg_data$border_key, type = "HC1")
  ct <- lmtest::coeftest(reg, vcov. = vcov_cl)

  res <- list(
    label = cfg$label, deg_label = cfg$deg_label,
    n_border = nrow(border_data), n_interior = nrow(interior_data), n_all = nrow(all_data),
    r2_border = r2_border, r2_interior = r2_interior, r2_all = r2_all,
    b_border   = unname(ct["is_any_borderTRUE", "Estimate"]),
    se_border  = unname(ct["is_any_borderTRUE", "Std. Error"]),
    p_border   = unname(ct["is_any_borderTRUE", "Pr(>|t|)"]),
    b_pop      = unname(ct["pop_total_share", "Estimate"]),
    se_pop     = unname(ct["pop_total_share", "Std. Error"]),
    p_pop      = unname(ct["pop_total_share", "Pr(>|t|)"]),
    b_area     = unname(ct["log_cell_area_km2", "Estimate"]),
    se_area    = unname(ct["log_cell_area_km2", "Std. Error"]),
    p_area     = unname(ct["log_cell_area_km2", "Pr(>|t|)"]),
    adj_r2     = s$adj.r.squared,
    n_reg      = nrow(reg$model),
    n_clusters = length(unique(reg_data$border_key))
  )

  cat(sprintf("  Reg: beta_border = %.4f (cluster-robust se = %.4f, p = %.4f) | %d clusters\n",
              res$b_border, res$se_border, res$p_border, res$n_clusters))
  cat(sprintf("       beta_area = %.4f (cluster-robust se = %.4f, p = %.4f)\n",
              res$b_area, res$se_area, res$p_area))

  results[[cfg$label]] <- res

  # --- Clean up ---
  rm(data_full, grid_coords, df_pred, oos_raw, oos_country, cell_area_lookup,
     all_data, reg_data, border_data, interior_data, reg,
     usa_finest,
     cell_iso_country, cell_iso_state, cell_country, cell_state,
     cell_neighbors_country, cell_neighbors_state)
  gc()
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------------------------------------------------------------------

save(results, file = file.path(output_dir, "border_discontinuity_results.RData"))

write.csv(
  bind_rows(lapply(results, function(x) {
    data.frame(resolution = x$label,
               n_border = x$n_border, n_interior = x$n_interior, n_all = x$n_all,
               r2_border = x$r2_border, r2_interior = x$r2_interior, r2_all = x$r2_all,
               b_border = x$b_border, se_border = x$se_border, p_border = x$p_border,
               b_pop = x$b_pop, se_pop = x$se_pop, p_pop = x$p_pop,
               b_area = x$b_area, se_area = x$se_area, p_area = x$p_area,
               adj_r2 = x$adj_r2, n_reg = x$n_reg, n_clusters = x$n_clusters,
               stringsAsFactors = FALSE)
  })),
  file = file.path(output_dir, "border_discontinuity_summary.csv"),
  row.names = FALSE
)

cat("\nBorder discontinuity robustness check complete.\n")
