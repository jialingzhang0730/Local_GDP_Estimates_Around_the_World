# --------------------------------- Task Summary --------------------------------- #
# Retrieve the geometry and GDP data for the administrative regions used in GDP 
#      share and predictor share calculations.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
rm(list = ls())
gc()

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(sf)
library(dplyr)
library(terra)
library(stars)
library(tidyverse)
library(ggthemes)
library(magrittr)
library(tictoc)
library(parallel)
library(gdata)
library(units)

# load the boundaries we created 
complete_poly <- read_sf("step2_obtain_gdp_data/outputs/complete_poly.gpkg")
world_poly <- read_sf("step2_obtain_gdp_data/outputs/world_poly.gpkg")

# USA needs state as parent country
USA <- complete_poly  %>% 
       filter(iso == "USA")  %>% 
       mutate(id = as.character(substr(id, 1, 2)))  %>% 
       group_by(id) %>%
       reframe(geom = st_union(geom))  %>% 
       mutate(iso = "USA")

# The rest training countries, use country boundary
rest_training_isos <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC",
                  "HUN", "ITA", "JPN", "KOR", "LTU", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "HRV",
                  "LVA", "SVN", "SWE", "TUR", "NZL","IDN",
                  "THA", "MOZ", "UZB", "KEN", "VNM", "SRB", "ECU", "BLR",
                  "ALB", "LKA", "BIH",
                  "COL", "PER", "CHL", "KGZ", "PHL", "EST") 

rest_train_iso_county_bound <- world_poly  %>% 
          filter(iso %in% rest_training_isos)  %>% 
          mutate(id = iso)  %>% 
          dplyr::select(c(id, iso, geom))          

# obtain those non-training countries parent boundary, don't forget Alaska
poly_nontraining <- complete_poly  %>% 
       filter(!iso %in% c(rest_training_isos, "USA"))  %>% 
       dplyr::select(c(id, iso))

# now combine them
world_model8_poly <- bind_rows(poly_nontraining, USA, rest_train_iso_county_bound)  %>% 
       mutate(id = ifelse(iso == "Ala", "Ala", id)) # make alaska's id also to be "Ala"

st_write(world_model8_poly, "step3_obtain_cell_level_GDP_and_predictors_data/outputs/world_model8_poly.gpkg", append = FALSE) # This geometry will be the level that the final predictions rescale to

# ------------------------------------------------- # 
# Now obtain parent country GDP data
rgdp_total_rescaled <- read.csv("step2_obtain_gdp_data/outputs/rgdp_total_rescaled.csv", encoding = "UTF-8")

# deal with USA first: obtain state gdp and country gdp
USA_state_GDP_pre <- rgdp_total_rescaled  %>% 
       filter(iso == "USA" & parent_admin_unit == 2)  %>% 
       mutate(id = substr(id, 1, 2))  %>% 
       distinct(id, iso, year, parent_name, parent_rgdp_total, national_population)  %>% 
       mutate(rescale_level = 2)  %>% 
       rename(unit_gdp_af_sum_rescl = parent_rgdp_total)

USA_country_total_GDP <- rgdp_total_rescaled  %>% 
       filter(iso == "USA" & parent_admin_unit == 1)  %>% 
       mutate(country_total_GDP = parent_rgdp_total)  %>% 
       dplyr::select(c(iso, year, country_total_GDP))  %>% 
       distinct(year, .keep_all = TRUE)

USA_GDP_final <- USA_state_GDP_pre  %>% 
       left_join(USA_country_total_GDP) %>%
       dplyr::select(-c(parent_name))

# deal with countries that use country GDP only
iso_use_countryGDP <- world_model8_poly  %>% 
       filter(id == iso & id != "Ala")  %>% # we will deal with Alaska later
       pull(iso)

iso_use_countryGDP_GDP <- rgdp_total_rescaled  %>% 
       filter(iso %in% iso_use_countryGDP & parent_admin_unit == 1)  %>% 
       mutate(rescale_level = 1,
              unit_gdp_af_sum_rescl = parent_rgdp_total,
              country_total_GDP = parent_rgdp_total,
              id = iso)  %>% 
       dplyr::select(c(id, iso, year, rescale_level, unit_gdp_af_sum_rescl, country_total_GDP, national_population))  %>% 
       distinct(iso, year, .keep_all = TRUE)

# deal with Alaska
Ala <- rgdp_total_rescaled  %>% 
       filter(iso == "Ala") %>%
       mutate(rescale_level = 2,
              unit_gdp_af_sum_rescl = unit_rgdp_total,
              country_total_GDP = parent_rgdp_total,
              id = iso)  %>% 
       mutate(iso = "USA") %>%
       dplyr::select(c(id, iso, year, rescale_level, unit_gdp_af_sum_rescl, country_total_GDP, national_population)) 

# deal with countries that use 2nd level GDP (like province GDP)
iso_use_provinceGDP <- world_model8_poly  %>% 
       filter(id != iso) %>% 
       filter(!iso %in% c("USA"))  %>% 
       pull(id)

iso_use_provinceGDP_GDP <- rgdp_total_rescaled  %>% 
       filter(id %in% iso_use_provinceGDP)  %>% 
       mutate(rescale_level = 2,
              unit_gdp_af_sum_rescl = unit_rgdp_total,
              country_total_GDP = parent_rgdp_total,
              national_population = national_population)  %>% 
       dplyr::select(c(id, iso, year, rescale_level, unit_gdp_af_sum_rescl, country_total_GDP, national_population))

# combine together
rgdp_total_af_sum_rescl_pre <- bind_rows(iso_use_countryGDP_GDP, USA_GDP_final, iso_use_provinceGDP_GDP, Ala)

# Some countries in the training sample lack regional GDP data from the OECD for 2020 and 2021. 
# As a result, their regional GDP data is not included in the file "step2_obtain_gdp_data/outputs/rgdp_total_rescaled.csv" and, 
# consequently, is absent from the "rgdp_total_af_sum_rescl_pre" dataframe. However, for these countries, GDP and predictor 
# calculations are based on country-level reference units. Since this requires only country-level information, which is already 
# available, their column values can be completed here.
which_iso_year <- rgdp_total_af_sum_rescl_pre %>%
       group_by(iso) %>%
       reframe(year = setdiff(2012:2021, year)) %>%
       filter(length(year) > 0)

fulfill_gdp <- read.csv("step2_obtain_gdp_data/temp/national_gdp_const_2017_USD.csv")  %>% # recall that we use national_gdp_const_2017_USD 
       rename(rgdp_2017_USD = rgdp_total) %>% 
       semi_join(which_iso_year, by = c("iso", "year")) %>% 
       mutate(id = iso, 
              rescale_level = 1,
              unit_gdp_af_sum_rescl = rgdp_2017_USD,
              country_total_GDP = rgdp_2017_USD,
              national_population = population) %>% 
       dplyr::select(-c(Country, rgdp_2017_USD, population))
  
# now combine them
rgdp_total_af_sum_rescl <- rbind(rgdp_total_af_sum_rescl_pre, fulfill_gdp)

save(rgdp_total_af_sum_rescl, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.RData")
write.csv(rgdp_total_af_sum_rescl, file = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv", row.names = FALSE)
