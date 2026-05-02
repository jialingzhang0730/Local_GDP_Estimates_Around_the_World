# --------------------------------- Task Summary --------------------------------- #
# This file pools tree-level uncertainty metrics across the three grid resolutions and writes a CSV reporting the average SD of log GDP for developed/developing and in-sample/out-of-sample country groups (Online Appendix Section 10).
# -------------------------------------------------------------------------------- #

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

### Load packages ----
library(tidyverse)
library(sf)
library(dplyr)
library(readxl)

output_dir <- "step5_predict_and_post_adjustments/outputs"

# ---------------------------------------------------------------------------------------------------------------------------------------
# Classify groups: Developed, Developing, In-sample, Out-of-sample

dped_list <- read_excel("step4_benchmark_model/inputs/list_developed_isos.xlsx")
developed_isos <- dped_list[, "developed"]

# In-sample countries (from Appendix Table 2, excluding China)
in_sample_isos <- c(
  # Developed (Table 2)
  "AUT", "BEL", "BGR", "CHE", "CZE", "DEU", "DNK", "ESP", "EST", "FIN",
  "FRA", "GBR", "GRC", "HRV", "HUN", "ITA", "JPN", "KOR", "LTU", "LVA",
  "NLD", "NOR", "NZL", "POL", "PRT", "ROU", "SWE", "SVK", "SVN", "TUR", "USA",
  # Developing (Table 2)
  "ALB", "BIH", "BLR", "CHL", "COL", "ECU", "IDN", "KEN", "KGZ", "LKA",
  "MOZ", "PER", "PHL", "SRB", "THA", "UZB", "VNM"
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Define file paths for each resolution

res_configs <- list(
  list(key = "1deg",    label = "1-degree Model",    file = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_1deg_postadjust_pop_dens_no_extra_adjust.RData")),
  list(key = "0_5deg",  label = "0.5-degree Model",  file = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_0_5deg_postadjust_pop_dens_no_extra_adjust.RData")),
  list(key = "0_25deg", label = "0.25-degree Model", file = file.path(output_dir, "final_output_dataset_with_uncertainty/final_GDPC_0_25deg_postadjust_pop_dens_no_extra_adjust.RData"))
)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Safely load an RData file and return its first data object

load_rdata_safely <- function(fpath) {
  env <- new.env()
  loaded <- load(fpath, envir = env)
  if (length(loaded) == 1) {
    return(env[[loaded]])
  }
  for (name in loaded) {
    if (grepl("GDPC", name)) return(env[[name]])
  }
  return(env[[loaded[1]]])
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Compute average SD of log(GDP) across trees for each group.

compute_sd_metrics <- function(df, developed_isos_vec, in_sample_isos_vec) {

  df_work <- df %>%
    as.data.frame() %>%
    filter(predicted_GCP_const_2021_USD > 0) %>%
    mutate(
      dev_status = ifelse(iso %in% developed_isos_vec, "Developed", "Developing"),
      sample_status = ifelse(iso %in% in_sample_isos_vec, "In-sample", "Out-of-sample"),
      sd_log_gdp = ifelse(GCP_sd_log_gdp > 0, GCP_sd_log_gdp, NA_real_)
    )

  # Define the 5 groups and their filters
  group_defs <- list(
    list(key = "Developed",      label = "Developed",      filter_fn = function(d) d %>% filter(dev_status == "Developed")),
    list(key = "Developing",     label = "Developing",     filter_fn = function(d) d %>% filter(dev_status == "Developing")),
    list(key = "In_sample",      label = "In-sample",      filter_fn = function(d) d %>% filter(sample_status == "In-sample")),
    list(key = "Out_of_sample",  label = "Out-of-sample",  filter_fn = function(d) d %>% filter(sample_status == "Out-of-sample")),
    list(key = "All",            label = "All",            filter_fn = function(d) d)
  )

  results <- list()

  for (g in group_defs) {
    group_data <- g$filter_fn(df_work)
    if (nrow(group_data) == 0) {
      results[[g$key]] <- list(label = g$label, n = 0, sd_log = NA_real_)
      next
    }

    avg_sd_log <- mean(group_data$sd_log_gdp, na.rm = TRUE)
    results[[g$key]] <- list(label = g$label, n = nrow(group_data), sd_log = avg_sd_log)
  }

  return(results)
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Load data and compute metrics for each resolution

all_metrics <- list()

for (rc in res_configs) {
  if (file.exists(rc$file)) {
    df_res <- load_rdata_safely(rc$file)
    all_metrics[[rc$key]] <- tryCatch(
      compute_sd_metrics(df_res, developed_isos$developed, in_sample_isos),
      error = function(e) {
        cat(paste0("  WARNING: Error for ", rc$key, ": ", e$message, "\n"))
        NULL
      }
    )
    rm(df_res); gc()
  } else {
    cat(paste0("File not found for ", rc$key, ": ", rc$file, ". Will use NA.\n"))
    all_metrics[[rc$key]] <- NULL
  }
}

# ---------------------------------------------------------------------------------------------------------------------------------------
# Assemble long-format CSV with one row per (resolution, group)

group_keys   <- c("Developed", "Developing", "In_sample", "Out_of_sample", "All")
group_labels <- c("Developed", "Developing", "In-sample", "Out-of-sample", "All")

rows <- list()

for (rc in res_configs) {
  m <- all_metrics[[rc$key]]
  for (i in seq_along(group_keys)) {
    gk <- group_keys[i]
    if (is.null(m) || is.null(m[[gk]])) {
      rows[[length(rows) + 1]] <- data.frame(
        resolution       = rc$label,
        group            = group_labels[i],
        n_cells          = NA_integer_,
        avg_sd_log_gdp   = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      g <- m[[gk]]
      rows[[length(rows) + 1]] <- data.frame(
        resolution       = rc$label,
        group            = group_labels[i],
        n_cells          = as.integer(g$n),
        avg_sd_log_gdp   = as.numeric(g$sd_log),
        stringsAsFactors = FALSE
      )
    }
  }
}

results_df <- do.call(rbind, rows)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Write CSV

csv_path <- file.path(output_dir, "uncertainty_sd_log_gdp_metrics.csv")
write.csv(results_df, csv_path, row.names = FALSE)
cat(paste0("Average SD log(GDP) metrics saved to: ", csv_path, "\n"))
