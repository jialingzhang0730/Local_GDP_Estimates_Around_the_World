# --------------------------------- Task Summary --------------------------------- #
# This file retrieves the true cell GDP for China (CHN).
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
  OUTPUT = "step6_shocks_log_change/outputs/china_city_inters_1deg_without_large_waters.gpkg", 
  .quiet = FALSE
)

# ------------------------------------------------------------------------------
# obtain China city level gdp for each province
main_folder <- "step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/province_yearbook"

target_subfolder <- list.dirs(main_folder, recursive = FALSE)

# Process each subfolder based on province-specific requirements
all_data <- lapply(target_subfolder, function(subfolder) {

  province_name <- basename(subfolder)
  target_files <- list.files(subfolder, full.names = TRUE) %>% 
                  .[order(-as.numeric(str_extract(., "\\d{4}")))]

  if (province_name == "Guangdong_prov") {
    # Processing for Guangdong province
    read_and_clean <- function(file) {
      read_excel(file, col_names = FALSE) %>%
        {
          start_row <- which(.[[2]] == "City")  # Identify the row number where the second column is "City"
          read_excel(file, skip = start_row - 1, col_names = TRUE) # Re-read starting from the correct row
        } %>%
        na.omit() %>% # Remove missing rows
        dplyr::select(-1)
    }
    
    combined_data <- reduce(map(target_files, read_and_clean), left_join) %>% 
      pivot_longer(cols = -c(City),
                   names_to = "year",
                   values_to = "value",
                   names_transform = list(year = ~gsub("`", "", .))) %>% 
      filter(year >= "2012" & year <= "2021") %>% 
      mutate(prov_id = "Guangdong_CHN")
    
  } else if (province_name == "Hubei_prov") {
    # Processing for Hubei province
    read_and_clean <- function(file) {
      year <- str_extract(file, "(?<=（)\\d{4}(?=）)")
      
      read_excel(file, col_names = FALSE) %>% 
        {
          start_row <- which(.[[2]] == "Regions")  # Identify the row number where the second column is "Regions"
          read_excel(file, skip = start_row - 1, col_names = TRUE) # Re-read starting from the correct row
        } %>%
        dplyr::select(2:3) %>% 
        na.omit() %>% 
        rename_with(~ c("City", year), .cols = 1:2) %>% 
        mutate(!!year := round(as.numeric(!!sym(year)), 2))
    }
    
    combined_data <- reduce(map(target_files, read_and_clean), left_join) %>% 
      pivot_longer(cols = -c(City),
                   names_to = "year",
                   values_to = "value",
                   names_transform = list(year = ~gsub("`", "", .))) %>% 
      filter(year >= "2012" & year <= "2021") %>% 
      mutate(prov_id = "Hubei_CHN")
    
  } else if (province_name == "Sichuan_prov") {
    # Processing for Sichuan province
    combined_data <- read_excel(target_files, col_names = FALSE) %>% 
      {
        start_row <- which(.[[2]] == "Region")  # Identify the row number where the second column is "Region"
        read_excel(target_files, skip = start_row - 1, col_names = TRUE) # Re-read starting from the correct row
      } %>% 
      na.omit() %>% 
      dplyr::select(-1) %>% 
      rename_with(~ c("City"), .cols = 1) %>% 
      pivot_longer(cols = -c(City),
                   names_to = "year",
                   values_to = "value",
                   names_transform = list(year = ~gsub("`", "", .))) %>% 
      filter(year >= "2012" & year <= "2021") %>% 
      mutate(value = round(as.numeric(value), 2)) %>% 
      mutate(prov_id = "Sichuan_CHN")

  } else if (province_name == "Jiangsu_prov") {
    # Processing for Jiangsu province
    read_and_clean <- function(file) {
      read_excel(file, col_names = FALSE, range = cell_rows(6:20)) %>% 
      replace(is.na(.), "nodata") %>%  # Replace NA values with "nodata"
      { setNames(slice(., -1), make.unique(as.character(slice(., 1)))) } %>% 
      rename_with(~ c("City_chinese", "City"), .cols = 1:2) %>% 
      filter(City != "by City") %>% 
      dplyr::select(-1) %>% 
      mutate(City = ifelse(substr(City,1,4) == "Huai", "Huai'an", City))
    }
    
    combined_data <- reduce(map(target_files, read_and_clean), 
                        function(x, y) left_join(x, y, by = "City", suffix = c("", ".y"))) %>% 
                        dplyr::select(-matches("\\.y$")) %>% 
      pivot_longer(cols = -c(City),
                   names_to = "year",
                   values_to = "value",
                   names_transform = list(year = ~gsub("`", "", .))) %>% 
      filter(year >= "2012" & year <= "2021") %>% 
      mutate(prov_id = "Jiangsu_CHN") %>% 
      mutate(value = as.numeric(value))

  } else if (province_name == "Shandong_prov") {
    # Processing for Shandong province (remember that City Laiwu is combined with Jinan after year 2018)
    read_and_clean <- function(file) {
      read_excel(file, col_names = FALSE) %>% 
        {
          end_row <- which(.[[2]] == "Heze" | .[[1]] == "菏 泽 市")  # Define the end row
          data <- read_excel(file, n_max = end_row, col_names = FALSE)
          
          end_col_index <- which(sapply(data, function(col) any(grepl("第", col))))[1]  # Find the index of the first column containing "第"
          
          data %>% 
            dplyr::select(1:(end_col_index - 1)) %>%  # Select columns up to the one before the column containing "第"
            filter(!is.na(.[[3]]) & !is.na(.[[4]])) %>%  # Filter rows that have values in columns 3 and 4
            replace(is.na(.), "nodata") %>%  # Replace NA values with "nodata"
            { setNames(slice(., -1), make.unique(as.character(slice(., 1)))) } %>%  # Set the first row as column names
            dplyr::select(1:2, matches("^\\d{4}$")) %>%  # Select the first two columns and the year columns
            rename_with(~ c("City_chinese"), .cols = 1) %>% 
            mutate(across(matches("^\\d{4}$"), ~ round(as.numeric(.), 2))) %>% 
            mutate(City_chinese = if_else(City_chinese == "莱 芜 市", "济 南 市", City_chinese)) %>%  # Replace "莱 芜 市" with "济 南 市"
            group_by(City_chinese) %>%  # Group by City_chinese
            summarise(across(where(is.numeric), sum, na.rm = TRUE),
                      across(where(is.character), first)) %>%  # Keep first occurrence for character columns
            ungroup()  # Ungroup the data
        }
    }
    
    combined_data <- reduce(map(target_files, read_and_clean), 
                        function(x, y) left_join(x, y, by = "City_chinese", suffix = c("", ".y"))) %>% 
                        dplyr::select(-matches("\\.y$")) %>% 
      rename(City = `nodata.1`) %>% 
      dplyr::select(-1) %>% 
      filter(City != "Total") %>% 
      mutate(across(matches("^\\d{4}$"), ~ round(as.numeric(.), 2))) %>% 
      pivot_longer(cols = -c(City),
                   names_to = "year",
                   values_to = "value",
                   names_transform = list(year = ~gsub("`", "", .))) %>% 
      filter(year >= "2012" & year <= "2021") %>% 
      mutate(prov_id = "Shandong_CHN") %>% 
      dplyr::select(City, everything())

  } else if (province_name == "Henan_prov"){
    # Processing for Henan province
    read_and_clean <- function(file) {
      year <- str_extract(file, "\\d{4}")

      read_excel(file, col_names = FALSE) %>%
        {
          start_row <- which(.[[2]] == "Zhengzhou")
          end_row <- start_row + 18
          read_excel(file, skip = start_row - 1, n_max = end_row - start_row + 1, col_names = FALSE)} %>% 
          dplyr::select(2:3) %>% 
          rename_with(~ c("City", year), .cols = 1:2) %>% 
          mutate(!!year := round(as.numeric(!!sym(year)), 2))
    }
    
    combined_data <- reduce(map(target_files, read_and_clean), left_join) %>% 
      pivot_longer(cols = -c(City),
                   names_to = "year",
                   values_to = "value",
                   names_transform = list(year = ~gsub("`", "", .))) %>% 
      filter(year >= "2012" & year <= "2021") %>% 
      mutate(prov_id = "Henan_CHN")
  
  } else if (province_name == "Zhejiang_prov"){
    # Processing for Zhejiang province
    read_and_clean <- function(file) {
      year <- str_extract(file, "\\d{4}")
      
      read_excel(file, col_names = FALSE) %>%
        {
          start_row <- which(.[[1]] == "杭州市")  # Identify the row number where the first column is "杭州市"
          read_excel(file, skip = start_row - 1, col_names = FALSE) # Re-read starting from the correct row
        } %>%
        dplyr::select(where(~ !all(is.na(.)))) %>% 
        dplyr::select(c(1, 3)) %>% 
        rename_with(~ c("City_chinese", year), .cols = 1:2) %>% 
        mutate(City = case_when(
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
          TRUE ~ NA_character_ # Handle cases where none of the conditions match
        )) %>% 
        na.omit() %>% 
        dplyr::select(-1) %>% 
        mutate(!!year := round(as.numeric(!!sym(year)), 2))
    }
    
    combined_data <- reduce(map(target_files, read_and_clean), left_join) %>% 
      pivot_longer(cols = -c(City),
                   names_to = "year",
                   values_to = "value",
                   names_transform = list(year = ~gsub("`", "", .))) %>% 
      filter(year >= "2012" & year <= "2021") %>% 
      mutate(prov_id = "Zhejiang_CHN")

  } else {
    # Handle other provinces or add more conditions as needed
    combined_data <- NULL
  }
  
  return(combined_data)
})

# Combine all the data from each province into a single data frame.
# Note that two cities share the same English name, but they belong to different provinces.
final_combined_data <- bind_rows(all_data) %>%  
  left_join(read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/CHN/city_province_list.xlsx")) %>% 
  rename(GDP_curt_pri = value) %>% # Note that different provinces may use different units, but this is acceptable as we are only interested in obtaining the share.
  na.omit() # The omitted entries are not at the city level, but represent larger regions.

write_xlsx(final_combined_data, "step6_shocks_log_change/outputs/CHN_test/CHN_city_true_GDP_some_prov.xlsx")

# read chn province GDP in constant 2017 USD, and rescale county GDP
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

chn_county_GDP <- read_xlsx("step6_shocks_log_change/outputs/CHN_test/CHN_city_true_GDP_some_prov.xlsx")  %>% 
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

population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:22] #choose years only after 2012

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
               left_join(chn_county_pop, by = c("id", "iso", "year"))  %>% # every county that has GDP has population data
               mutate(county_GDPC = ifelse(pop == 0, 0, unit_rgdp_total_sum_rescaled / pop))  %>% 
               dplyr::select(c("id", "iso", "year", "county_GDPC"))

chn_1deg <- read_sf("step6_shocks_log_change/outputs/china_city_inters_1deg_without_large_waters.gpkg")  %>% 
  rename(id = ct_adcode)  %>% 
  mutate(iso = "CHN")  %>% 
  dplyr::select(c(id, iso, cell_id))  %>%      
  left_join(chn_county_GDPC, relationship = "many-to-many") %>% 
  na.omit()

# -------------------------------------------------------------------------------------------------------------------- #
# Obtain population for each county-cell intersected polygons

# 1degree
population_files <- list.files("step3_obtain_cell_level_GDP_and_predictors_data/inputs/population", full.names = T)[13:22] #choose years only after 2012

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

save(chn_1deg_cell_GCP, file = "step6_shocks_log_change/outputs/CHN_test/chn_1deg_cell_GCP.RData")
