# --------------------------------- Task Summary --------------------------------- #
# This file is to obtain true cell GDP for CHN.
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
library(RColorBrewer)
library(terra)
library(exactextractr)
library(readxl)
library(writexl)
library(qgisprocess)

# first prepare for the geometry

temp_file <- tempfile(fileext = ".gpkg")

qgis_run_algorithm(
    "native:intersection",
    INPUT = "step1_obtain_gis_data/inputs/china_city/city.shp",
    OVERLAY = "step3_obtain_cell_level_GDP_and_predictors_data/outputs/just_grid_1degree.gpkg",
    OUTPUT = temp_file
)

difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = read_sf(temp_file),
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp",
  OUTPUT = "step6_test_model_under_shocks/outputs/china_city_inters_1deg_without_large_waters.gpkg",
  .quiet = FALSE
)

# ------------------------------------------------------------------------------
# obtain China city level gdp for each province
# Read the existing 2012-2021 historical city-level data (shipped as a static input)
existing_data <- read_xlsx("step6_test_model_under_shocks/inputs/CHN_city_true_GDP_some_prov_2012_2021.xlsx")

# Initialize a list to store 2022 data from each province
data_2022_list <- list()

# --------------------------------
# 1. Guangdong Province
# --------------------------------
guangdong_2022 <- read_excel(
  "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook/Guangdong_prov/02-14-1.xls",
  range = "B6:I26",
  col_names = FALSE
) %>%
  select(1, 8) %>%  # Select columns B (1st) and I (8th)
  rename(City = ...1, GDP_curt_pri = ...8) %>%
  mutate(
    year = 2022,
    prov_id = "Guangdong_CHN",
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  )

data_2022_list$Guangdong <- guangdong_2022

# --------------------------------
# 2. Henan Province
# --------------------------------
henan_2022 <- read_excel(
  "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook/Henan_prov/0209.xls",
  range = "B12:C29",
  col_names = FALSE
) %>%
  rename(City = ...1, GDP_curt_pri = ...2) %>%
  mutate(
    year = 2022,
    prov_id = "Henan_CHN",
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  )

data_2022_list$Henan <- henan_2022

# --------------------------------
# 3. Hubei Province
# --------------------------------
hubei_2022 <- read_excel(
  "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook/Hubei_prov/0115-市、州生产总值（2023）.xls",
  range = "B8:C24",
  col_names = FALSE
) %>%
  rename(City = ...1, GDP_curt_pri = ...2) %>%
  mutate(
    year = 2022,
    prov_id = "Hubei_CHN",
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  )

data_2022_list$Hubei <- hubei_2022

# --------------------------------
# 4. Jiangsu Province
# --------------------------------
jiangsu_2022 <- read_excel(
  "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook/Jiangsu_prov/nj02.xlsx",
  range = "B6:H18",
  col_names = FALSE
) %>%
  select(1, 7) %>%  # Select columns B (1st) and H (7th)
  rename(City = ...1, GDP_curt_pri = ...7) %>%
  mutate(
    year = 2022,
    prov_id = "Jiangsu_CHN",
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  ) %>%
  # Handle special case for Huai'an if needed
  mutate(City = ifelse(substr(City, 1, 4) == "Huai", "Huai'an", City))

data_2022_list$Jiangsu <- jiangsu_2022

# --------------------------------
# 5. Shandong Province
# --------------------------------
shandong_2022 <- read_excel(
  "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook/Shandong_prov/02-06.xls",
  range = "B12:C27",
  col_names = FALSE
) %>%
  rename(City = ...1, GDP_curt_pri = ...2) %>%
  mutate(
    year = 2022,
    prov_id = "Shandong_CHN",
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  )

data_2022_list$Shandong <- shandong_2022

# --------------------------------
# 6. Sichuan Province
# --------------------------------
sichuan_2022 <- read_excel(
  "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook/Sichuan_prov/各市(州)地区生产总值.xlsx",
  range = "B7:M27",
  col_names = FALSE
) %>%
  select(1, 12) %>%  # Select columns B (1st) and M (12th)
  rename(City = ...1, GDP_curt_pri = ...12) %>%
  mutate(
    year = 2022,
    prov_id = "Sichuan_CHN",
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  )

data_2022_list$Sichuan <- sichuan_2022

# --------------------------------
# 7. Zhejiang Province
# --------------------------------
# Note: For Zhejiang, we need to map Chinese names to English names
zhejiang_2022_raw <- read_excel(
  "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook/Zhejiang_prov/17-2 各市国民经济主要指标（2022年）.xlsx",
  range = "A4:D14",
  col_names = FALSE
) %>%
  select(1, 3) %>%  # Select columns A (1st) and D (4th)
  rename(City_chinese = ...1, GDP_curt_pri = ...3)

# Map Chinese city names to English
zhejiang_2022 <- zhejiang_2022_raw %>%
  mutate(
    City = case_when(
      City_chinese == "杭州市" ~ "Hangzhou",
      City_chinese == "宁波市" ~ "Ningbo",
      City_chinese == "嘉兴市" ~ "Jiaxing",
      City_chinese == "湖州市" ~ "Huzhou",
      City_chinese == "绍兴市" ~ "Shaoxing",
      City_chinese == "舟山市" ~ "Zhoushan",
      City_chinese == "温州市" ~ "Wenzhou",
      City_chinese == "金华市" ~ "Jinhua",
      City_chinese == "衢州市" ~ "Quzhou",
      City_chinese == "台州市" ~ "Taizhou",
      City_chinese == "丽水市" ~ "Lishui",
      TRUE ~ NA_character_
    ),
    year = 2022,
    prov_id = "Zhejiang_CHN",
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  ) %>%
  select(-City_chinese) %>%
  na.omit()

data_2022_list$Zhejiang <- zhejiang_2022

# --------------------------------
# Combine all 2022 data
# --------------------------------
all_2022_data <- bind_rows(data_2022_list) %>%
  select(City, year, GDP_curt_pri, prov_id)

# --------------------------------
# Join with city-province mapping
# --------------------------------
# Read the city-province mapping file to get additional columns
city_mapping <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/city_province_list.xlsx")

# Join 2022 data with mapping to get ct_adcode, ct_name, pr_adcode, pr_name
all_2022_with_mapping <- all_2022_data %>%
  left_join(city_mapping, by = c("City", "prov_id"))

# Add iso column
all_2022_with_mapping <- all_2022_with_mapping %>%
  mutate(iso = "CHN")

# --------------------------------
# Combine with existing data
# --------------------------------
# Ensure column types match
existing_data <- existing_data %>%
  mutate(
    year = as.numeric(year),
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  )

all_2022_with_mapping <- all_2022_with_mapping %>%
  mutate(
    year = as.numeric(year),
    GDP_curt_pri = as.numeric(GDP_curt_pri)
  )

# Combine the datasets
combined_data <- bind_rows(existing_data, all_2022_with_mapping) %>%
  arrange(prov_id, City, year)

write_xlsx(combined_data, "step6_test_model_under_shocks/outputs/CHN_test/CHN_city_true_GDP_some_prov.xlsx")

# --------------------------------
# Now rescale city-level GDP using provincial data from step 3
# --------------------------------

# read chn province GDP in constant 2021 USD, and rescale county GDP
prov_GDP <- read.csv("step3_obtain_cell_level_GDP_and_predictors_data/outputs/rgdp_total_af_sum_rescl.csv") %>%
  filter(iso == "CHN") %>% 
  mutate(pr_adcode = ifelse(id == "Beijing_CHN", "110000",
                            ifelse(id == "Tianjin_CHN", "120000",
                                   ifelse(id == "Hebei_CHN", "130000",
                                          ifelse(id == "Shanxi_CHN", "140000",
                                                 ifelse(id == "Inner Mongolia_CHN", "150000",
                                                        ifelse(id == "Liaoning_CHN", "210000",
                                                               ifelse(id == "Jilin_CHN", "220000",
                                                                      ifelse(id == "Heilongjiang_CHN", "230000",
                                                                             ifelse(id == "Shanghai_CHN", "310000",
                                                                                    ifelse(id == "Jiangsu_CHN", "320000",
                                                                                           ifelse(id == "Zhejiang_CHN", "330000",
                                                                                                  ifelse(id == "Anhui_CHN", "340000",
                                                                                                         ifelse(id == "Fujian_CHN", "350000",
                                                                                                                ifelse(id == "Jiangxi_CHN", "360000",
                                                                                                                       ifelse(id == "Shandong_CHN", "370000",
                                                                                                                              ifelse(id == "Henan_CHN", "410000",
                                                                                                                                     ifelse(id == "Hubei_CHN", "420000",
                                                                                                                                            ifelse(id == "Hunan_CHN", "430000",
                                                                                                                                                   ifelse(id == "Guangdong_CHN", "440000",
                                                                                                                                                          ifelse(id == "Guangxi_CHN", "450000",
                                                                                                                                                                 ifelse(id == "Hainan_CHN", "460000",
                                                                                                                                                                        ifelse(id == "Chongqing_CHN", "5e+05",
                                                                                                                                                                               ifelse(id == "Sichuan_CHN", "510000",
                                                                                                                                                                                      ifelse(id == "Guizhou_CHN", "520000",
                                                                                                                                                                                             ifelse(id == "Yunnan_CHN", "530000",
                                                                                                                                                                                                    ifelse(id == "Tibet_CHN", "540000",
                                                                                                                                                                                                           ifelse(id == "Shaanxi_CHN", "610000",
                                                                                                                                                                                                                  ifelse(id == "Gansu_CHN", "620000",
                                                                                                                                                                                                                         ifelse(id == "Qinghai_CHN", "630000",
                                                                                                                                                                                                                                ifelse(id == "Ningxia_CHN", "640000",
                                                                                                                                                                                                                                       ifelse(id == "Xinjiang_CHN", "650000", NA)))))))))))))))))))))))))))))))) %>% # assign province code to the prov GDP data so that we can rescale
  dplyr::select(-c(rescale_level, iso))

chn_county_GDP <- read_xlsx("step6_test_model_under_shocks/outputs/CHN_test/CHN_city_true_GDP_some_prov.xlsx")  %>% 
  mutate(pr_adcode = as.character(pr_adcode), year = as.integer(year)) %>%
  left_join(prov_GDP) %>% 
  group_by(prov_id, year) %>% 
  mutate(unit_rgdp_total_sum_rescaled = unit_gdp_af_sum_rescl*GDP_curt_pri/sum(GDP_curt_pri))  %>% 
  ungroup()  %>% 
  dplyr::select(-c(id)) %>%
  rename(id = ct_adcode, parent_rgdp_total = country_total_GDP, unit_rgdp_total_before_sum_rescaled = GDP_curt_pri)  %>% 
  mutate(share_GDP_region_GDP_nation = unit_rgdp_total_sum_rescaled/parent_rgdp_total)  %>%    
  dplyr::select(id, year, iso, unit_rgdp_total_before_sum_rescaled, parent_rgdp_total, national_population, unit_rgdp_total_sum_rescaled, share_GDP_region_GDP_nation)  %>% 
  as.data.frame()  %>% 
  dplyr::select(c("id", "iso", "year", "parent_rgdp_total", "unit_rgdp_total_sum_rescaled", "national_population")) 

# obtain county pop

chn_city_geom <- read_sf("step1_obtain_gis_data/inputs/china_city/city.shp") %>%
  filter(pr_name %in% c("广东省", "江苏省", "山东省", "浙江省", "河南省", "四川省", "湖北省")) %>% 
  rename(geom = geometry, id = ct_adcode)  %>% 
  dplyr::select(c(id, geom))  %>% 
  mutate(iso = "CHN") %>% 
  filter(id %in% chn_county_GDP$id)               

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23] #choose years only after 2012

pop_extracted_list <- mclapply(population_files, mc.cores = 5, FUN = function(filename) {

  r <- rast(filename)

  extract <- cbind(chn_city_geom, exact_extract(r, chn_city_geom, 'sum')) %>% 
    rename(pop = exact_extract.r..chn_city_geom...sum..) %>%
    mutate(year = as.integer(str_extract(filename, "\\d{4}")))

  return(extract)
})

chn_county_pop <- bind_rows(pop_extracted_list) %>% 
  filter(id != iso) %>% # ignore the country total population
  replace_na(list(pop = 0)) %>% 
  as.data.frame()  %>% 
  dplyr::select(c("id", "iso", "year", "pop")) %>% 
  mutate(pop = floor(pop))

chn_county_GDPC <- chn_county_GDP  %>% 
  mutate(id = as.character(id)) %>% 
  left_join(chn_county_pop, by = c("id", "iso", "year"))  %>% # good, every county that has GDP has population data
  mutate(county_GDPC = ifelse(pop == 0, 0, unit_rgdp_total_sum_rescaled / pop))  %>% 
  dplyr::select(c("id", "iso", "year", "county_GDPC"))

chn_1deg <- read_sf("step6_test_model_under_shocks/outputs/china_city_inters_1deg_without_large_waters.gpkg")  %>% 
  rename(id = ct_adcode)  %>% 
  mutate(iso = "CHN")  %>% 
  dplyr::select(c(id, iso, cell_id))  %>%      
  left_join(chn_county_GDPC, relationship = "many-to-many") %>% 
  na.omit()

# -------------------------------------------------------------------------------------------------------------------- #
# Obtain population for each county-cell intersected polygons

# 1degree
population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:23] #choose years only after 2012

county_cell_pop_extracted_1deg <- mclapply(population_files, mc.cores = 5, FUN = function(filename) {

  r <- rast(filename)
  year_file <- gsub(".*landscan-global-(\\d{4}).*\\.tif", "\\1", filename)
  chn_1deg_year <- chn_1deg %>% filter(year == as.integer(year_file))

  extract <- cbind(chn_1deg_year, exact_extract(r, chn_1deg_year, 'sum')) %>% 
    rename(pop = exact_extract.r..chn_1deg_year...sum..) %>%
    mutate(year = as.integer(year_file))

  return(extract)
}) %>% 
  do.call(rbind, .) %>% 
  replace_na(list(pop = 0))

# -------------------------------------------------------------------------------------------------------------------- #
# obtain cell GDP

# 1 degree
chn_1deg_cell_GCP <- county_cell_pop_extracted_1deg  %>% 
  dplyr::select(c(cell_id, id, iso, year, county_GDPC, pop))  %>% 
  mutate(prov_id = substr(id, 1, 2)) %>% 
  left_join(chn_county_GDP  %>%
              mutate(prov_id = substr(id, 1, 2)) %>%  
              group_by(iso, year, prov_id)  %>% 
              mutate(state_total_GDP = sum(unit_rgdp_total_sum_rescaled))  %>% 
              ungroup() %>%
              mutate(id = as.character(id))) %>% 
  mutate(GDP_subcell = county_GDPC*pop) %>% 
  group_by(iso, year, prov_id)  %>% 
  mutate(GDP_subcell_rescl = GDP_subcell * state_total_GDP/sum(GDP_subcell))  %>% 
  ungroup() %>% 
  group_by(year, iso, prov_id, cell_id)  %>% 
  mutate(GCP_1deg = sum(GDP_subcell_rescl))  %>% 
  ungroup()  %>% 
  dplyr::select(c(cell_id, prov_id, iso, year, GCP_1deg, state_total_GDP, parent_rgdp_total, national_population))  %>% 
  as.data.frame()  %>% 
  dplyr::select(-c(geom))  %>% 
  distinct(year, iso, prov_id, cell_id, .keep_all = TRUE) 

save(chn_1deg_cell_GCP, file = "step6_test_model_under_shocks/outputs/CHN_test/chn_1deg_cell_GCP.RData")
