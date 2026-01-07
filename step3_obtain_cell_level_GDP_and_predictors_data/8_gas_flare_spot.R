# --------------------------------- Task Summary --------------------------------- #
# This file retrieves the locations of places that emit light due to gas flaring.
# The data is stored in the folder "inputs/gas_flare_data".
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
rm(list = ls())
gc()

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
library(readxl)
library(qgisprocess)

# ------------------------------------------------- # 
# identify the locations of those gas flare spots
#   Original data are in excel, but we need gpkg file
spot <- read_excel("step3_obtain_cell_level_GDP_and_predictors_data/inputs/gas_flare_data/2012-2023-Flare-Volume-Estimates-by-individual-Flare-Location.xlsx")

spot_sf <- spot  %>% 
  filter(`Flaring Vol (million m3)` != 0)  %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

st_write(spot_sf, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot_sf.gpkg", append = FALSE)

# now we do the following process in QGIS version 3.32.0-Lima, follow exactly the step

# 1. Go to Processing Toolbox > Vector geometry > Rectangles, ovals, diamonds.
# 2. Set the Type to Rectangle.
# 3. Set the width and height to 0.2 degrees.
# 4. Choose the input layer as "gas_flare_spot_sf.gpkg"
# 5. Click Run.
# 6. Export the file in "step3_obtain_cell_level_GDP_and_predictors_data/inputs/gas_flare_data/gas_flare_spot_sf_square_0_2deg.gpkg"

qgis_run_algorithm(
  "native:rectanglesovalsdiamonds",  # QGIS algorithm for creating shapes
  WIDTH = 0.2,  
  HEIGHT = 0.2, 
  INPUT = read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot_sf.gpkg"), 
  OUTPUT = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot_sf_square_0_2deg.gpkg" 
)

# ------------------------------------------------- # 
# plot the gas flaring spots
spot <- read_sf("step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot_sf_square_0_2deg.gpkg")

world_poly <- read_sf("step2_obtain_gdp_data/outputs/world_poly.gpkg")

ggplot() +
  geom_sf(data = world_poly, fill = "lightgrey", color = "white") +  # Country boundaries in light grey
  geom_sf(data = spot %>% st_as_sf(), aes(fill = "red"), color = "NA") +  # Cells from df_check in red
  theme_minimal() +
  labs(title = "Gas Flare Spots") +
  theme_void() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 12, hjust = 0.5), 
        axis.title = element_text(size = 12, hjust = 0.5), 
        axis.text = element_blank(),  # Remove x and y axis text
        axis.ticks = element_blank(),
        legend.position = "none")

ggsave("step3_obtain_cell_level_GDP_and_predictors_data/outputs/gas_flare_data/gas_flare_spot.png", width = 10, height = 6, bg = "white")



