# --------------------------------- Task Summary --------------------------------- #
# This file combines all predictors and the dependent variable to create the training dataset.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tictoc)
library(gdata)
library(sf)
library(parallel)
library(tidyverse)
library(fs)
library(dplyr)
library(data.table)

# ------------------------------------------------------------------------------------------------------------
# 1 degree

# load urban population
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_urban_extracted_region_level_1deg.RData")
pop_urban <- pop_urban_extracted_region_level_1deg %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geom, fid_2)) %>%
  group_by(cell_id, iso, id, year)  %>% 
  summarize(pop_urban = floor(sum(pop_urban)), .groups = "drop") %>% 
  mutate(year = as.integer(year))

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_cropland_extracted_region_level_1deg.RData")
pop_cropland <- pop_cropland_extracted_region_level_1deg %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geom,fid_2)) %>%
  group_by(cell_id, iso, id, year)  %>% 
  summarize(pop_cropland = floor(sum(pop_cropland)), .groups = "drop") %>% 
  mutate(year = as.integer(year))

# load population and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_1deg.RData")
pop_put_in_model <- land_pop_extracted_region_level_1deg  %>% 
  as.data.frame() %>% 
  mutate(pop_total = floor(pop)) %>% 
  dplyr::select(-c(geom, pop)) %>%        
  left_join(pop_urban) %>% 
  left_join(pop_cropland) %>% 
  mutate(across(c(pop_urban, pop_cropland), ~replace_na(., 0))) %>%
  mutate(pop_other = pop_total - pop_urban - pop_cropland) %>% 
  group_by(id, year)  %>% 
  mutate(pop_urban_share = pop_urban/sum(pop_urban),
         pop_cropland_share = pop_cropland/sum(pop_cropland),
         pop_other_share = pop_other/sum(pop_other),
         pop_total_share = pop_total/sum(pop_total))  %>% 
  ungroup()

# load CO2_bio emission data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_bio_full_1deg.RData")
CO2_bio_put_in_model <- CO2_bio_full_1deg  %>% 
  mutate(CO2_bio_heavy_indus = CO2_bio_fuel_exploitation + CO2_bio_oil_refine_transf + CO2_bio_power_industry,
         CO2_bio_tspt = CO2_bio_road_transp + CO2_bio_shipping) %>% 
  group_by(id, year)  %>% 
  mutate(CO2_bio_manuf_conbust_share = ifelse(CO2_bio_combustion_for_manufacturing == 0, 0, CO2_bio_combustion_for_manufacturing/sum(CO2_bio_combustion_for_manufacturing)),
         CO2_bio_building_energy_share = ifelse(CO2_bio_energy_for_buildings == 0, 0, CO2_bio_energy_for_buildings/sum(CO2_bio_energy_for_buildings)),
         CO2_bio_heavy_indus_share = ifelse(CO2_bio_heavy_indus == 0, 0, CO2_bio_heavy_indus/sum(CO2_bio_heavy_indus)),
         CO2_bio_tspt_share = ifelse(CO2_bio_tspt == 0, 0, CO2_bio_tspt/sum(CO2_bio_tspt)))  %>% 
  ungroup()

# load CO2_non_org emission data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_non_org_full_1deg.RData")
CO2_non_org_put_in_model <- CO2_non_org_full_1deg  %>% 
  mutate(CO2_non_org_heavy_indus = CO2_non_org_fuel_exploitation + CO2_non_org_iron_steel + CO2_non_org_non_ferrous_metal + CO2_non_org_non_metallic_mineral + CO2_non_org_oil_refine_transf + CO2_non_org_power_industry,
         CO2_non_org_tspt = CO2_non_org_road_transp + CO2_non_org_shipping) %>% 
  group_by(id, year)  %>% 
  mutate(CO2_non_org_manuf_conbust_share = ifelse(CO2_non_org_combustion_for_manufacturing == 0, 0, CO2_non_org_combustion_for_manufacturing/sum(CO2_non_org_combustion_for_manufacturing)),
         CO2_non_org_building_energy_share = ifelse(CO2_non_org_energy_for_buildings == 0, 0, CO2_non_org_energy_for_buildings/sum(CO2_non_org_energy_for_buildings)),
         CO2_non_org_heavy_indus_share = ifelse(CO2_non_org_heavy_indus == 0, 0, CO2_non_org_heavy_indus/sum(CO2_non_org_heavy_indus)),
         CO2_non_org_tspt_share = ifelse(CO2_non_org_tspt == 0, 0, CO2_non_org_tspt/sum(CO2_non_org_tspt)),
         CO2_non_org_solvents_product_use_share = ifelse(CO2_non_org_solvents_product_use == 0, 0, CO2_non_org_solvents_product_use/sum(CO2_non_org_solvents_product_use)))  %>% 
  ungroup()

# load NPP data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_full_1deg.RData")
NPP_put_in_model <- NPP_full_1deg  %>% 
  group_by(year, id)  %>% 
  mutate(NPP_share = NPP/sum(NPP))  %>%
  ungroup()

# load NTL data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_full_1deg.RData")
NTL_urban_put_in_model <- NTL_urban_full_1deg  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_urban_snow_covered_period_share = ifelse(NTL_snow_covered_period == 0, 0, NTL_snow_covered_period/sum(NTL_snow_covered_period)))  %>% 
  mutate(NTL_urban_snow_free_period_share = ifelse(NTL_snow_free_period == 0, 0, NTL_snow_free_period/sum(NTL_snow_free_period)))  %>% 
  ungroup()  %>% 
  rename(NTL_urban_snow_covered_period = NTL_snow_covered_period,
         NTL_urban_snow_free_period = NTL_snow_free_period)  %>% 
  dplyr::select(-c(land_type))

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_cropland_full_1deg.RData")
NTL_cropland_put_in_model <- NTL_cropland_full_1deg  %>% 
  group_by(cell_id, iso, id, year)  %>% 
  summarize(NTL_snow_covered_period = sum(NTL_snow_covered_period),
            NTL_snow_free_period = sum(NTL_snow_free_period), .groups = "drop")  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_cropland_snow_covered_period_share = ifelse(NTL_snow_covered_period == 0, 0, NTL_snow_covered_period/sum(NTL_snow_covered_period)))  %>% 
  mutate(NTL_cropland_snow_free_period_share = ifelse(NTL_snow_free_period == 0, 0, NTL_snow_free_period/sum(NTL_snow_free_period)))  %>% 
  ungroup()  %>% 
  rename(NTL_cropland_snow_covered_period = NTL_snow_covered_period,
         NTL_cropland_snow_free_period = NTL_snow_free_period) 

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_full_1deg.RData")
NTL_put_in_model <- NTL_full_1deg  %>% 
  rename(NTL_full_snow_covered_period = NTL_snow_covered_period,
         NTL_full_snow_free_period = NTL_snow_free_period)  %>% 
  filter(year <= 2022) %>% # because we do not have year 2023's landcover data, so not able to get cropland NTL and urban NTL data
  left_join(NTL_urban_put_in_model)  %>% 
  mutate(across(c(NTL_urban_snow_covered_period, NTL_urban_snow_free_period, NTL_urban_snow_covered_period_share, NTL_urban_snow_free_period_share), ~replace_na(., 0)))  %>% 
  left_join(NTL_cropland_put_in_model)  %>% 
  mutate(across(c(NTL_cropland_snow_covered_period, NTL_cropland_snow_free_period, NTL_cropland_snow_covered_period_share, NTL_cropland_snow_free_period_share), ~replace_na(., 0)))  %>% 
  mutate(NTL_other_snow_covered_period = NTL_full_snow_covered_period - NTL_urban_snow_covered_period - NTL_cropland_snow_covered_period,
         NTL_other_snow_free_period = NTL_full_snow_free_period - NTL_urban_snow_free_period - NTL_cropland_snow_free_period)  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_other_snow_covered_period_share = ifelse(NTL_other_snow_covered_period == 0, 0, NTL_other_snow_covered_period/sum(NTL_other_snow_covered_period)))  %>% 
  mutate(NTL_other_snow_free_period_share = ifelse(NTL_other_snow_free_period == 0, 0, NTL_other_snow_free_period/sum(NTL_other_snow_free_period)))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, iso, id, year, NTL_urban_snow_covered_period_share, NTL_urban_snow_free_period_share, 
                  NTL_cropland_snow_covered_period_share, NTL_cropland_snow_free_period_share,
                  NTL_other_snow_covered_period_share, NTL_other_snow_free_period_share))  %>% 
  as.data.frame()

# load landcover data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_1deg.RData")
lc_put_in_model <- lc_full_1deg  %>% 
  mutate(forest_full = open_forest + dense_forest,
         cropland_full = cropland + forest_cropland + herbaceous_cropland) %>% 
  dplyr::select(-c(open_forest, dense_forest, cropland, forest_cropland, herbaceous_cropland)) %>% 
  group_by(year, id)  %>% 
  mutate(barren_share = barren/sum(barren))  %>% 
  mutate(snow_ice_share = snow_ice/sum(snow_ice))  %>% 
  mutate(water_share = water/sum(water))  %>% 
  mutate(urban_share = urban/sum(urban))  %>% 
  mutate(forest_share = forest_full/sum(forest_full))  %>% 
  mutate(herbaceous_share = herbaceous/sum(herbaceous))  %>% 
  mutate(cropland_share = cropland_full/sum(cropland_full))  %>% 
  mutate(shrub_share = shrub/sum(shrub))  %>% 
  ungroup()  %>% 
  replace(is.na(.), 0) # some country does not have a specific landcover type, for example, Austria does not have "herbacous", so the share is 0/0, which turns to NA. So we assign value 0 to those.

# load ruggedness
rug_put_in_model <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/mean_ruggedness_1deg.csv")  %>% 
  mutate(cell_id = as.character(cell_id)) 

# load GDP per capita 
national_GDPC_pre <- read.csv("step2_obtain_gdp_data/temp/national_gdpc_const_2021_USD.csv")

# Give Alaska's GDP per capita to be the same as USA
Ala <- national_GDPC_pre %>%
  filter(iso == "USA") %>% 
  mutate(iso = "Ala", Country = "Alaska")

national_GDPC <- rbind(national_GDPC_pre, Ala)

# load training countries GCP data
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_iso_1deg_cell_GCP.RData")

# Combine all the predictors into one file
predictors_put_in_model_1deg_pre <- pop_put_in_model  %>%     
  left_join(CO2_bio_put_in_model, by = join_by(cell_id, id, iso, year))  %>% 
  left_join(CO2_non_org_put_in_model, by = join_by(cell_id, id, iso, year))  %>% 
  left_join(NPP_put_in_model, by = join_by(cell_id, id, iso, year))  %>%            
  left_join(NTL_put_in_model, by = join_by(cell_id, id, iso, year))  %>% 
  left_join(lc_put_in_model, by = join_by(cell_id, id, iso, year))  %>% 
  left_join(rug_put_in_model, by = join_by(cell_id, id, iso))  %>%           
  replace(is.na(.), 0) %>% # just one cell in SAU should have zero values for the predictors, that polygon is on the edge of a cell
  filter(year <= 2022) %>%  # because we only have GDP data until 2022
  left_join(national_GDPC) 

predictors_put_in_model_1deg <- predictors_put_in_model_1deg_pre  %>% 
  dplyr::select(c(id, iso, cell_id, year, contains("share"), mean_rug, national_gdpc, pop_total)) %>% 
  mutate(original_order = row_number()) %>%
  arrange(cell_id, id, iso, year) %>%
  group_by(cell_id, id, iso) %>% 
  mutate(lag_NTL_urban_share = lag(NTL_urban_snow_free_period_share, default = first(NTL_urban_snow_free_period_share)),
         lag_urban_share = lag(urban_share, default = first(urban_share)),
         lag_cropland_share = lag(cropland_share, default = first(cropland_share)),
         lag_NTL_other_share = lag(NTL_other_snow_free_period_share, default = first(NTL_other_snow_free_period_share)),
         lag_NTL_cropland_share = lag(NTL_cropland_snow_free_period_share, default = first(NTL_cropland_snow_free_period_share)),
         lag_CO2_bio_mc_share = lag(CO2_bio_manuf_conbust_share, default = first(CO2_bio_manuf_conbust_share)),
         lag_CO2_nonorg_mc_share = lag(CO2_non_org_manuf_conbust_share, default = first(CO2_non_org_manuf_conbust_share)),
         lag_CO2_bio_heavy_indus_share = lag(CO2_bio_heavy_indus_share, default = first(CO2_bio_heavy_indus_share)),
         lag_CO2_non_org_heavy_indus_share = lag(CO2_non_org_heavy_indus_share, default = first(CO2_non_org_heavy_indus_share)),
         lag_CO2_bio_tspt_share = lag(CO2_bio_tspt_share, default = first(CO2_bio_tspt_share)),
         lag_CO2_non_org_tspt_share = lag(CO2_non_org_tspt_share, default = first(CO2_non_org_tspt_share)),
         lag_CO2_non_org_solvents_product_use_share = lag(CO2_non_org_solvents_product_use_share, default = first(CO2_non_org_solvents_product_use_share)),
         lag_pop_urban_share = lag(pop_urban_share, default = first(pop_urban_share)),
         lag_pop_cropland_share = lag(pop_cropland_share, default = first(pop_cropland_share)),
         lag_pop_other_share = lag(pop_other_share, default = first(pop_other_share)),
         lag_pop_total_share = lag(pop_total_share, default = first(pop_total_share)),
         lag_NPP_share = lag(NPP_share, default = first(NPP_share))) %>%        
  ungroup() %>% 
  arrange(original_order) %>% 
  dplyr::select(-c(original_order))

save(predictors_put_in_model_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_1deg.RData")

# form the training sample

predictors_put_in_model_1deg_revi <- predictors_put_in_model_1deg %>%
  as.data.frame()  %>%
  mutate(iso_change = ifelse(iso == "USA", paste0("USA_", substr(id, 1, 2)), id))  %>% 
  dplyr::select(c(cell_id, iso_change, year, contains("_share"), mean_rug, national_gdpc, pop_total))  %>% 
  rename(iso = iso_change)

predict_data_complete_1deg <- training_iso_1deg_cell_GCP  %>%  
  group_by(iso, year)  %>% 
  mutate(GCP_share_1deg = ifelse(GCP_1deg == 0, 0, GCP_1deg/sum(GCP_1deg)))  %>% 
  ungroup()  %>% 
  left_join(predictors_put_in_model_1deg_revi) %>%
  na.omit() # some cells are missing simply because of the geometry of those cells, DOSE's geometry's boundaries are slightly different from us.

save(predict_data_complete_1deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_1deg.RData")

#--------------------------------------------------------------------------------------------------------------
# 0.5 degree

# load urban population
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_urban_extracted_region_level_0_5deg.RData")
pop_urban <- pop_urban_extracted_region_level_0_5deg %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geom, fid_2)) %>%
  group_by(cell_id, subcell_id, iso, id, year)  %>% 
  summarize(pop_urban = floor(sum(pop_urban)), .groups = "drop") %>% 
  mutate(year = as.integer(year))

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_cropland_extracted_region_level_0_5deg.RData")
pop_cropland <- pop_cropland_extracted_region_level_0_5deg %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geom,fid_2)) %>%
  group_by(cell_id, subcell_id, iso, id, year)  %>% 
  summarize(pop_cropland = floor(sum(pop_cropland)), .groups = "drop") %>% 
  mutate(year = as.integer(year))

# load population and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_5deg.RData")
pop_put_in_model <- land_pop_extracted_region_level_0_5deg  %>% 
  as.data.frame() %>% 
  mutate(pop_total = floor(pop)) %>% 
  dplyr::select(-c(geom, pop)) %>%        
  left_join(pop_urban) %>% 
  left_join(pop_cropland) %>% 
  mutate(across(c(pop_urban, pop_cropland), ~replace_na(., 0))) %>%
  mutate(pop_other = pop_total - pop_urban - pop_cropland) %>% 
  group_by(id, year)  %>% 
  mutate(pop_urban_share = pop_urban/sum(pop_urban),
         pop_cropland_share = pop_cropland/sum(pop_cropland),
         pop_other_share = pop_other/sum(pop_other),
         pop_total_share = pop_total/sum(pop_total))  %>% 
  ungroup()

# load CO2_bio emission data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_bio_full_0_5deg.RData")
CO2_bio_put_in_model <- CO2_bio_full_0_5deg  %>% 
  mutate(CO2_bio_heavy_indus = CO2_bio_fuel_exploitation + CO2_bio_oil_refine_transf + CO2_bio_power_industry,
         CO2_bio_tspt = CO2_bio_road_transp + CO2_bio_shipping) %>% 
  group_by(id, year)  %>% 
  mutate(CO2_bio_manuf_conbust_share = ifelse(CO2_bio_combustion_for_manufacturing == 0, 0, CO2_bio_combustion_for_manufacturing/sum(CO2_bio_combustion_for_manufacturing)),
         CO2_bio_building_energy_share = ifelse(CO2_bio_energy_for_buildings == 0, 0, CO2_bio_energy_for_buildings/sum(CO2_bio_energy_for_buildings)),
         CO2_bio_heavy_indus_share = ifelse(CO2_bio_heavy_indus == 0, 0, CO2_bio_heavy_indus/sum(CO2_bio_heavy_indus)),
         CO2_bio_tspt_share = ifelse(CO2_bio_tspt == 0, 0, CO2_bio_tspt/sum(CO2_bio_tspt)))  %>% 
  ungroup()

# load CO2_non_org emission data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_non_org_full_0_5deg.RData")
CO2_non_org_put_in_model <- CO2_non_org_full_0_5deg  %>% 
  mutate(CO2_non_org_heavy_indus = CO2_non_org_fuel_exploitation + CO2_non_org_iron_steel + CO2_non_org_non_ferrous_metal + CO2_non_org_non_metallic_mineral + CO2_non_org_oil_refine_transf + CO2_non_org_power_industry,
         CO2_non_org_tspt = CO2_non_org_road_transp + CO2_non_org_shipping) %>% 
  group_by(id, year)  %>% 
  mutate(CO2_non_org_manuf_conbust_share = ifelse(CO2_non_org_combustion_for_manufacturing == 0, 0, CO2_non_org_combustion_for_manufacturing/sum(CO2_non_org_combustion_for_manufacturing)),
         CO2_non_org_building_energy_share = ifelse(CO2_non_org_energy_for_buildings == 0, 0, CO2_non_org_energy_for_buildings/sum(CO2_non_org_energy_for_buildings)),
         CO2_non_org_heavy_indus_share = ifelse(CO2_non_org_heavy_indus == 0, 0, CO2_non_org_heavy_indus/sum(CO2_non_org_heavy_indus)),
         CO2_non_org_tspt_share = ifelse(CO2_non_org_tspt == 0, 0, CO2_non_org_tspt/sum(CO2_non_org_tspt)),
         CO2_non_org_solvents_product_use_share = ifelse(CO2_non_org_solvents_product_use == 0, 0, CO2_non_org_solvents_product_use/sum(CO2_non_org_solvents_product_use)))  %>% 
  ungroup()

# load NPP data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_full_0_5deg.RData")
NPP_put_in_model <- NPP_full_0_5deg  %>% 
  group_by(year, id)  %>% 
  mutate(NPP_share = NPP/sum(NPP))  %>% 
  ungroup()  

# load NTL data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_full_0_5deg.RData")
NTL_urban_put_in_model <- NTL_urban_full_0_5deg  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_urban_snow_covered_period_share = ifelse(NTL_snow_covered_period == 0, 0, NTL_snow_covered_period/sum(NTL_snow_covered_period)))  %>% 
  mutate(NTL_urban_snow_free_period_share = ifelse(NTL_snow_free_period == 0, 0, NTL_snow_free_period/sum(NTL_snow_free_period)))  %>% 
  ungroup()  %>% 
  rename(NTL_urban_snow_covered_period = NTL_snow_covered_period,
         NTL_urban_snow_free_period = NTL_snow_free_period)  %>% 
  dplyr::select(-c(land_type))

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_cropland_full_0_5deg.RData")
NTL_cropland_put_in_model <- NTL_cropland_full_0_5deg  %>% 
  group_by(subcell_id, cell_id, iso, id, year)  %>% 
  summarize(NTL_snow_covered_period = sum(NTL_snow_covered_period),
            NTL_snow_free_period = sum(NTL_snow_free_period), .groups = "drop")  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_cropland_snow_covered_period_share = ifelse(NTL_snow_covered_period == 0, 0, NTL_snow_covered_period/sum(NTL_snow_covered_period)))  %>% 
  mutate(NTL_cropland_snow_free_period_share = ifelse(NTL_snow_free_period == 0, 0, NTL_snow_free_period/sum(NTL_snow_free_period)))  %>% 
  ungroup()  %>% 
  rename(NTL_cropland_snow_covered_period = NTL_snow_covered_period,
         NTL_cropland_snow_free_period = NTL_snow_free_period) 

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_full_0_5deg.RData")
NTL_put_in_model <- NTL_full_0_5deg  %>% 
  rename(NTL_full_snow_covered_period = NTL_snow_covered_period,
         NTL_full_snow_free_period = NTL_snow_free_period)  %>% 
  filter(year <= 2022) %>% # because we do not have year 2022's landcover data, so not able to get cropland NTL and urban NTL data
  left_join(NTL_urban_put_in_model)  %>% 
  mutate(across(c(NTL_urban_snow_covered_period, NTL_urban_snow_free_period, NTL_urban_snow_covered_period_share, NTL_urban_snow_free_period_share), ~replace_na(., 0)))  %>% 
  left_join(NTL_cropland_put_in_model)  %>% 
  mutate(across(c(NTL_cropland_snow_covered_period, NTL_cropland_snow_free_period, NTL_cropland_snow_covered_period_share, NTL_cropland_snow_free_period_share), ~replace_na(., 0)))  %>% 
  mutate(NTL_other_snow_covered_period = NTL_full_snow_covered_period - NTL_urban_snow_covered_period - NTL_cropland_snow_covered_period,
         NTL_other_snow_free_period = NTL_full_snow_free_period - NTL_urban_snow_free_period - NTL_cropland_snow_free_period)  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_other_snow_covered_period_share = ifelse(NTL_other_snow_covered_period == 0, 0, NTL_other_snow_covered_period/sum(NTL_other_snow_covered_period)))  %>% 
  mutate(NTL_other_snow_free_period_share = ifelse(NTL_other_snow_free_period == 0, 0, NTL_other_snow_free_period/sum(NTL_other_snow_free_period)))  %>% 
  ungroup()  %>% 
  dplyr::select(c(subcell_id, cell_id, iso, id, year, NTL_urban_snow_covered_period_share, NTL_urban_snow_free_period_share, 
                  NTL_cropland_snow_covered_period_share, NTL_cropland_snow_free_period_share,
                  NTL_other_snow_covered_period_share, NTL_other_snow_free_period_share))  %>% 
  as.data.frame()

# load landcover data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_5deg.RData")
lc_put_in_model <- lc_full_0_5deg  %>% 
  mutate(forest_full = open_forest + dense_forest,
         cropland_full = cropland + forest_cropland + herbaceous_cropland) %>% 
  dplyr::select(-c(open_forest, dense_forest, cropland, forest_cropland, herbaceous_cropland)) %>% 
  group_by(year, id)  %>% 
  mutate(barren_share = barren/sum(barren))  %>% 
  mutate(snow_ice_share = snow_ice/sum(snow_ice))  %>% 
  mutate(water_share = water/sum(water))  %>% 
  mutate(urban_share = urban/sum(urban))  %>% 
  mutate(forest_share = forest_full/sum(forest_full))  %>% 
  mutate(herbaceous_share = herbaceous/sum(herbaceous))  %>% 
  mutate(cropland_share = cropland_full/sum(cropland_full))  %>% 
  mutate(shrub_share = shrub/sum(shrub))  %>% 
  ungroup()  %>% 
  replace(is.na(.), 0) # some country does not have a specific landcover type, for example, Austria does not have "herbacous", so the share is 0/0, which turns to NA. So we assign value 0 to those.

# load ruggedness
rug_put_in_model <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/mean_ruggedness_0_5deg.csv")  %>% 
  mutate(cell_id = as.character(cell_id))

# load GDP per capita 
national_GDPC_pre <- read.csv("step2_obtain_gdp_data/temp/national_gdpc_const_2021_USD.csv")

# Give Alaska's GDP per capita to be the same as USA
Ala <- national_GDPC_pre %>%
  filter(iso == "USA") %>% 
  mutate(iso = "Ala", Country = "Alaska")

national_GDPC <- rbind(national_GDPC_pre, Ala)

# load training countries GCP data
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_iso_0_5deg_cell_GCP.RData")

# Combine all the predictors into one file
predictors_put_in_model_0_5deg_pre <- pop_put_in_model  %>%      
  left_join(CO2_bio_put_in_model, by = join_by(cell_id, subcell_id, id, iso, year))  %>% 
  left_join(CO2_non_org_put_in_model, by = join_by(cell_id, subcell_id, id, iso, year))  %>% 
  left_join(NPP_put_in_model, by = join_by(cell_id, subcell_id, id, iso, year))  %>%               
  left_join(NTL_put_in_model, by = join_by(cell_id, subcell_id, id, iso, year))  %>% 
  left_join(lc_put_in_model, by = join_by(cell_id, subcell_id, id, iso, year))  %>% 
  left_join(rug_put_in_model, by = join_by(cell_id, subcell_id, id, iso))  %>%               
  replace(is.na(.), 0) %>% # just one cell in SAU should have zero values for the predictors, that polygon is on the edge of a cell
  filter(year <= 2022) %>%  # because we only have GDP data until 2022
  left_join(national_GDPC) 

predictors_put_in_model_0_5deg <- predictors_put_in_model_0_5deg_pre  %>% 
  dplyr::select(c(id, iso, cell_id, subcell_id, year, contains("share"), mean_rug, national_gdpc, pop_total)) %>% 
  mutate(original_order = row_number()) %>%
  arrange(subcell_id, cell_id, id, iso, year) %>%
  group_by(subcell_id, cell_id, id, iso) %>% 
  mutate(lag_NTL_urban_share = lag(NTL_urban_snow_free_period_share, default = first(NTL_urban_snow_free_period_share)),
         lag_urban_share = lag(urban_share, default = first(urban_share)),
         lag_cropland_share = lag(cropland_share, default = first(cropland_share)),
         lag_NTL_other_share = lag(NTL_other_snow_free_period_share, default = first(NTL_other_snow_free_period_share)),
         lag_NTL_cropland_share = lag(NTL_cropland_snow_free_period_share, default = first(NTL_cropland_snow_free_period_share)),
         lag_CO2_bio_mc_share = lag(CO2_bio_manuf_conbust_share, default = first(CO2_bio_manuf_conbust_share)),
         lag_CO2_nonorg_mc_share = lag(CO2_non_org_manuf_conbust_share, default = first(CO2_non_org_manuf_conbust_share)),
         lag_CO2_bio_heavy_indus_share = lag(CO2_bio_heavy_indus_share, default = first(CO2_bio_heavy_indus_share)),
         lag_CO2_non_org_heavy_indus_share = lag(CO2_non_org_heavy_indus_share, default = first(CO2_non_org_heavy_indus_share)),
         lag_CO2_bio_tspt_share = lag(CO2_bio_tspt_share, default = first(CO2_bio_tspt_share)),
         lag_CO2_non_org_tspt_share = lag(CO2_non_org_tspt_share, default = first(CO2_non_org_tspt_share)),
         lag_CO2_non_org_solvents_product_use_share = lag(CO2_non_org_solvents_product_use_share, default = first(CO2_non_org_solvents_product_use_share)),
         lag_pop_urban_share = lag(pop_urban_share, default = first(pop_urban_share)),
         lag_pop_cropland_share = lag(pop_cropland_share, default = first(pop_cropland_share)),
         lag_pop_other_share = lag(pop_other_share, default = first(pop_other_share)),
         lag_pop_total_share = lag(pop_total_share, default = first(pop_total_share)),
         lag_NPP_share = lag(NPP_share, default = first(NPP_share))) %>%        
  ungroup() %>% 
  arrange(original_order) %>% 
  dplyr::select(-c(original_order))

save(predictors_put_in_model_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_0_5deg.RData")

# form the training sample

predictors_put_in_model_0_5deg_revi <- predictors_put_in_model_0_5deg %>%
  as.data.frame()  %>%
  mutate(iso_change = ifelse(iso == "USA", paste0("USA_", substr(id, 1, 2)), id))  %>% 
  dplyr::select(c(cell_id, subcell_id, iso_change, year, contains("_share"), mean_rug, national_gdpc, pop_total))  %>% 
  rename(iso = iso_change)

predict_data_complete_0_5deg <- training_iso_0_5deg_cell_GCP  %>%  
  group_by(iso, year)  %>% 
  mutate(GCP_share_0_5deg = ifelse(GCP_0_5deg == 0, 0, GCP_0_5deg/sum(GCP_0_5deg)))  %>% 
  ungroup()  %>% 
  left_join(predictors_put_in_model_0_5deg_revi)  %>% 
  na.omit() # some cells are missing simply because of the geometry of those cells, DOSE's geometry's boundarries are slightly different from us.

save(predict_data_complete_0_5deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_0_5deg.RData")

#--------------------------------------------------------------------------------------------------------------
# 0.25 degree

# load urban population
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_urban_extracted_region_level_0_25deg.RData")
pop_urban <- pop_urban_extracted_region_level_0_25deg %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geom, fid_2)) %>%
  group_by(cell_id, subcell_id, subcell_id_0_25, iso, id, year)  %>% 
  summarize(pop_urban = floor(sum(pop_urban)), .groups = "drop") %>% 
  mutate(year = as.integer(year))

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/pop_cropland_extracted_region_level_0_25deg.RData")
pop_cropland <- pop_cropland_extracted_region_level_0_25deg %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geom,fid_2)) %>%
  group_by(cell_id, subcell_id, subcell_id_0_25, iso, id, year)  %>% 
  summarize(pop_cropland = floor(sum(pop_cropland)), .groups = "drop") %>% 
  mutate(year = as.integer(year))

# load population and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/land_pop_extracted_region_level_0_25deg.RData")
pop_put_in_model <- land_pop_extracted_region_level_0_25deg  %>% 
  as.data.frame() %>% 
  mutate(pop_total = floor(pop)) %>% 
  dplyr::select(-c(geom, pop)) %>%        
  left_join(pop_urban) %>% 
  left_join(pop_cropland) %>% 
  mutate(across(c(pop_urban, pop_cropland), ~replace_na(., 0))) %>%
  mutate(pop_other = pop_total - pop_urban - pop_cropland) %>% 
  group_by(id, year)  %>% 
  mutate(pop_urban_share = pop_urban/sum(pop_urban),
         pop_cropland_share = pop_cropland/sum(pop_cropland),
         pop_other_share = pop_other/sum(pop_other),
         pop_total_share = pop_total/sum(pop_total))  %>% 
  ungroup()

# load CO2_bio emission data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_bio_full_0_25deg.RData")
CO2_bio_put_in_model <- CO2_bio_full_0_25deg  %>% 
  mutate(CO2_bio_heavy_indus = CO2_bio_fuel_exploitation + CO2_bio_oil_refine_transf + CO2_bio_power_industry,
         CO2_bio_tspt = CO2_bio_road_transp + CO2_bio_shipping) %>% 
  group_by(id, year)  %>% 
  mutate(CO2_bio_manuf_conbust_share = ifelse(CO2_bio_combustion_for_manufacturing == 0, 0, CO2_bio_combustion_for_manufacturing/sum(CO2_bio_combustion_for_manufacturing)),
         CO2_bio_building_energy_share = ifelse(CO2_bio_energy_for_buildings == 0, 0, CO2_bio_energy_for_buildings/sum(CO2_bio_energy_for_buildings)),
         CO2_bio_heavy_indus_share = ifelse(CO2_bio_heavy_indus == 0, 0, CO2_bio_heavy_indus/sum(CO2_bio_heavy_indus)),
         CO2_bio_tspt_share = ifelse(CO2_bio_tspt == 0, 0, CO2_bio_tspt/sum(CO2_bio_tspt)))  %>% 
  ungroup()

# load CO2_non_org emission data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/CO2_non_org_full_0_25deg.RData")
CO2_non_org_put_in_model <- CO2_non_org_full_0_25deg  %>% 
  mutate(CO2_non_org_heavy_indus = CO2_non_org_fuel_exploitation + CO2_non_org_iron_steel + CO2_non_org_non_ferrous_metal + CO2_non_org_non_metallic_mineral + CO2_non_org_oil_refine_transf + CO2_non_org_power_industry,
         CO2_non_org_tspt = CO2_non_org_road_transp + CO2_non_org_shipping) %>% 
  group_by(id, year)  %>% 
  mutate(CO2_non_org_manuf_conbust_share = ifelse(CO2_non_org_combustion_for_manufacturing == 0, 0, CO2_non_org_combustion_for_manufacturing/sum(CO2_non_org_combustion_for_manufacturing)),
         CO2_non_org_building_energy_share = ifelse(CO2_non_org_energy_for_buildings == 0, 0, CO2_non_org_energy_for_buildings/sum(CO2_non_org_energy_for_buildings)),
         CO2_non_org_heavy_indus_share = ifelse(CO2_non_org_heavy_indus == 0, 0, CO2_non_org_heavy_indus/sum(CO2_non_org_heavy_indus)),
         CO2_non_org_tspt_share = ifelse(CO2_non_org_tspt == 0, 0, CO2_non_org_tspt/sum(CO2_non_org_tspt)),
         CO2_non_org_solvents_product_use_share = ifelse(CO2_non_org_solvents_product_use == 0, 0, CO2_non_org_solvents_product_use/sum(CO2_non_org_solvents_product_use)))  %>% 
  ungroup()

# load NPP data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NPP_full_0_25deg.RData")
NPP_put_in_model <- NPP_full_0_25deg  %>% 
  group_by(year, id)  %>% 
  mutate(NPP_share = NPP/sum(NPP))  %>% 
  ungroup()

# load NTL data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_urban_full_0_25deg.RData")
NTL_urban_put_in_model <- NTL_urban_full_0_25deg  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_urban_snow_covered_period_share = ifelse(NTL_snow_covered_period == 0, 0, NTL_snow_covered_period/sum(NTL_snow_covered_period)))  %>% 
  mutate(NTL_urban_snow_free_period_share = ifelse(NTL_snow_free_period == 0, 0, NTL_snow_free_period/sum(NTL_snow_free_period)))  %>% 
  ungroup()  %>% 
  rename(NTL_urban_snow_covered_period = NTL_snow_covered_period,
         NTL_urban_snow_free_period = NTL_snow_free_period)  %>% 
  dplyr::select(-c(land_type))

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_cropland_full_0_25deg.RData")
NTL_cropland_put_in_model <- NTL_cropland_full_0_25deg  %>% 
  group_by(subcell_id_0_25, subcell_id, cell_id, iso, id, year)  %>% 
  summarize(NTL_snow_covered_period = sum(NTL_snow_covered_period),
            NTL_snow_free_period = sum(NTL_snow_free_period), .groups = "drop")  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_cropland_snow_covered_period_share = ifelse(NTL_snow_covered_period == 0, 0, NTL_snow_covered_period/sum(NTL_snow_covered_period)))  %>% 
  mutate(NTL_cropland_snow_free_period_share = ifelse(NTL_snow_free_period == 0, 0, NTL_snow_free_period/sum(NTL_snow_free_period)))  %>% 
  ungroup()  %>% 
  rename(NTL_cropland_snow_covered_period = NTL_snow_covered_period,
         NTL_cropland_snow_free_period = NTL_snow_free_period) 

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/NTL_full_0_25deg.RData")
NTL_put_in_model <- NTL_full_0_25deg  %>% 
  rename(NTL_full_snow_covered_period = NTL_snow_covered_period,
         NTL_full_snow_free_period = NTL_snow_free_period)  %>% 
  filter(year <= 2022) %>% # because we do not have year 2022's landcover data, so not able to get cropland NTL and urban NTL data
  left_join(NTL_urban_put_in_model)  %>% 
  mutate(across(c(NTL_urban_snow_covered_period, NTL_urban_snow_free_period, NTL_urban_snow_covered_period_share, NTL_urban_snow_free_period_share), ~replace_na(., 0)))  %>% 
  left_join(NTL_cropland_put_in_model)  %>% 
  mutate(across(c(NTL_cropland_snow_covered_period, NTL_cropland_snow_free_period, NTL_cropland_snow_covered_period_share, NTL_cropland_snow_free_period_share), ~replace_na(., 0)))  %>% 
  mutate(NTL_other_snow_covered_period = NTL_full_snow_covered_period - NTL_urban_snow_covered_period - NTL_cropland_snow_covered_period,
         NTL_other_snow_free_period = NTL_full_snow_free_period - NTL_urban_snow_free_period - NTL_cropland_snow_free_period)  %>% 
  group_by(year, id)  %>% 
  mutate(NTL_other_snow_covered_period_share = ifelse(NTL_other_snow_covered_period == 0, 0, NTL_other_snow_covered_period/sum(NTL_other_snow_covered_period)))  %>% 
  mutate(NTL_other_snow_free_period_share = ifelse(NTL_other_snow_free_period == 0, 0, NTL_other_snow_free_period/sum(NTL_other_snow_free_period)))  %>% 
  ungroup()  %>% 
  dplyr::select(c(subcell_id_0_25, subcell_id, cell_id, iso, id, year, NTL_urban_snow_covered_period_share, NTL_urban_snow_free_period_share, 
                  NTL_cropland_snow_covered_period_share, NTL_cropland_snow_free_period_share,
                  NTL_other_snow_covered_period_share, NTL_other_snow_free_period_share))  %>% 
  as.data.frame()

# load landcover data and compute the share
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/lc_full_0_25deg.RData")
lc_put_in_model <- lc_full_0_25deg  %>% 
  mutate(forest_full = open_forest + dense_forest,
         cropland_full = cropland + forest_cropland + herbaceous_cropland) %>% 
  dplyr::select(-c(open_forest, dense_forest, cropland, forest_cropland, herbaceous_cropland)) %>% 
  group_by(year, id)  %>% 
  mutate(barren_share = barren/sum(barren))  %>% 
  mutate(snow_ice_share = snow_ice/sum(snow_ice))  %>% 
  mutate(water_share = water/sum(water))  %>% 
  mutate(urban_share = urban/sum(urban))  %>% 
  mutate(forest_share = forest_full/sum(forest_full))  %>% 
  mutate(herbaceous_share = herbaceous/sum(herbaceous))  %>% 
  mutate(cropland_share = cropland_full/sum(cropland_full))  %>% 
  mutate(shrub_share = shrub/sum(shrub))  %>% 
  ungroup()  %>% 
  replace(is.na(.), 0) # some country does not have a specific landcover type, for example, Austria does not have "herbacous", so the share is 0/0, which turns to NA. So we assign value 0 to those.

# load ruggedness
rug_put_in_model <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/mean_ruggedness_0_25deg.csv")  %>% 
  mutate(cell_id = as.character(cell_id))

# load GDP per capita 
national_GDPC_pre <- read.csv("step2_obtain_gdp_data/temp/national_gdpc_const_2021_USD.csv")

# Give Alaska's GDP per capita to be the same as USA
Ala <- national_GDPC_pre %>%
  filter(iso == "USA") %>% 
  mutate(iso = "Ala", Country = "Alaska")

national_GDPC <- rbind(national_GDPC_pre, Ala)

# load training countries GCP data
load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/training_iso_0_25deg_cell_GCP.RData")

# Combine all the predictors into one file
predictors_put_in_model_0_25deg_pre <- pop_put_in_model  %>% 
  left_join(CO2_bio_put_in_model, by = join_by(cell_id, subcell_id, subcell_id_0_25, id, iso, year))  %>% 
  left_join(CO2_non_org_put_in_model, by = join_by(cell_id, subcell_id, subcell_id_0_25, id, iso, year))  %>% 
  left_join(NPP_put_in_model, by = join_by(cell_id, subcell_id, subcell_id_0_25, id, iso, year))  %>%             
  left_join(NTL_put_in_model, by = join_by(cell_id, subcell_id, subcell_id_0_25, id, iso, year))  %>% 
  left_join(lc_put_in_model, by = join_by(cell_id, subcell_id, subcell_id_0_25, id, iso, year))  %>% 
  left_join(rug_put_in_model, by = join_by(cell_id, subcell_id, subcell_id_0_25, id, iso))  %>%               
  replace(is.na(.), 0) %>% # just one cell in SAU should have zero values for the predictors, that polygon is on the edge of a cell
  filter(year <= 2022) %>%  # because we only have GDP data until 2022
  left_join(national_GDPC) 

predictors_put_in_model_0_25deg <- predictors_put_in_model_0_25deg_pre  %>% 
  dplyr::select(c(id, iso, cell_id, subcell_id, subcell_id_0_25, year, contains("share"), mean_rug, national_gdpc, pop_total)) %>% 
  mutate(original_order = row_number()) %>%
  arrange(subcell_id_0_25, subcell_id, cell_id, id, iso, year) %>%
  group_by(subcell_id_0_25, subcell_id, cell_id, id, iso) %>% 
  mutate(lag_NTL_urban_share = lag(NTL_urban_snow_free_period_share, default = first(NTL_urban_snow_free_period_share)),
         lag_urban_share = lag(urban_share, default = first(urban_share)),
         lag_cropland_share = lag(cropland_share, default = first(cropland_share)),
         lag_NTL_other_share = lag(NTL_other_snow_free_period_share, default = first(NTL_other_snow_free_period_share)),
         lag_NTL_cropland_share = lag(NTL_cropland_snow_free_period_share, default = first(NTL_cropland_snow_free_period_share)),
         lag_CO2_bio_mc_share = lag(CO2_bio_manuf_conbust_share, default = first(CO2_bio_manuf_conbust_share)),
         lag_CO2_nonorg_mc_share = lag(CO2_non_org_manuf_conbust_share, default = first(CO2_non_org_manuf_conbust_share)),
         lag_CO2_bio_heavy_indus_share = lag(CO2_bio_heavy_indus_share, default = first(CO2_bio_heavy_indus_share)),
         lag_CO2_non_org_heavy_indus_share = lag(CO2_non_org_heavy_indus_share, default = first(CO2_non_org_heavy_indus_share)),
         lag_CO2_bio_tspt_share = lag(CO2_bio_tspt_share, default = first(CO2_bio_tspt_share)),
         lag_CO2_non_org_tspt_share = lag(CO2_non_org_tspt_share, default = first(CO2_non_org_tspt_share)),
         lag_CO2_non_org_solvents_product_use_share = lag(CO2_non_org_solvents_product_use_share, default = first(CO2_non_org_solvents_product_use_share)),
         lag_pop_urban_share = lag(pop_urban_share, default = first(pop_urban_share)),
         lag_pop_cropland_share = lag(pop_cropland_share, default = first(pop_cropland_share)),
         lag_pop_other_share = lag(pop_other_share, default = first(pop_other_share)),
         lag_pop_total_share = lag(pop_total_share, default = first(pop_total_share)),
         lag_NPP_share = lag(NPP_share, default = first(NPP_share))) %>%        
  ungroup() %>% 
  arrange(original_order) %>% 
  dplyr::select(-c(original_order))

save(predictors_put_in_model_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predictors_put_in_model_0_25deg.RData")

# form the training sample

predictors_put_in_model_0_25deg_revi <- predictors_put_in_model_0_25deg %>%
  as.data.frame()  %>%
  mutate(iso_change = ifelse(iso == "USA", paste0("USA_", substr(id, 1, 2)), id))  %>% 
  dplyr::select(c(cell_id, subcell_id, subcell_id_0_25, iso_change, year, contains("_share"), mean_rug, national_gdpc, pop_total))  %>% 
  rename(iso = iso_change)

predict_data_complete_0_25deg <- training_iso_0_25deg_cell_GCP  %>%
  group_by(iso, year)  %>% 
  mutate(GCP_share_0_25deg = ifelse(GCP_0_25deg == 0, 0, GCP_0_25deg/sum(GCP_0_25deg)))  %>% 
  ungroup()  %>% 
  left_join(predictors_put_in_model_0_25deg_revi)  %>% 
  na.omit() # some cells are missing simply because of the geometry of those cells, DOSE's geometry's boundarries are slightly different from us.

save(predict_data_complete_0_25deg, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_0_25deg.RData")
