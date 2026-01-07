# --------------------------------- Task Summary --------------------------------- #
# This file retrieves regional GDP data from the OECD Regional Economy database.
# Data for the years 2012-2020 were downloaded from the OECD iLibrary.
# Data for the year 2021 was downloaded from the new platform, OECD Data Explorer, on June 8, 2024. 
#   Please note that the data may be updated after this date, so exercise caution when referencing this dataset.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load packages

library(tidyverse)
library(sf)
library(gdata)
library(parallel)
library(tictoc)
library(units)
library(OECD)
library(rsdmx)
library(giscoR)
library(regions)
library(rmapshaper)
library(countrycode)
library(stringi)
library(readxl)
library(XML)
library(data.table)
library(qgisprocess)

# Obtain GDP data in current national price: year 2012 - 2020

# Read GDP data
oecd_data <- fread("step2_obtain_gdp_data/inputs/gdp_data/regional/oecd/REGION_ECONOM-2023-1-EN-20240216T100059 2.csv")  %>% 
    filter(Indicator == "Regional GDP")  %>% 
    filter(SERIES == "SNA_2008")   %>% 
    filter(Year >= 2012, Year <= 2020)  %>% 
    filter(Measure == "      Millions National currency, current prices")  %>% 
    filter(POS == "ALL")  %>% 
    filter(TL %in% c(1,2,3))  %>% 
    dplyr::select(c("TL", "REG_ID", "Region", "TIME", "Value"))  %>% 
    rename(id = REG_ID, rgdp_total = Value, admin_unit = TL, name = Region)  %>% 
    pivot_longer(cols = starts_with("rgdp_"), names_to = "sector")  %>% 
    pivot_wider(names_from = "TIME") %>% 
    pivot_longer(cols = matches("\\d{4}"), names_to = "year",
                names_transform = list(year = as.numeric)) %>%
    pivot_wider(names_from = "sector")  %>% 
    mutate(name = gsub(" $", "", name))

# Get data structure: 
oced_stru <- read_xlsx("step2_obtain_gdp_data/inputs/gdp_data/regional/oecd/OECD Territorial correspondence - November 2023.xlsx", sheet = "List of regions - 2023", range = "A3:Q3652")  %>% 
    filter(`Classification (latest : TL-2021)` == "TL-2021")  %>% 
    dplyr::select(c("REG_ID", "Regional name (eng)", "Hierarchical relations"))  %>% 
    rename(id = REG_ID, name = `Regional name (eng)`, parent_id = `Hierarchical relations`)
    
stru_more <- oced_stru  %>% 
    left_join(rename(oced_stru, grandparent_id = parent_id, parent_name = name),
            by = c("parent_id" = "id"))  %>% 
    left_join((dplyr::select(oced_stru, -parent_id) %>% 
                rename(grandparent_name = name)),
                by = c("grandparent_id" = "id"))  %>% 
    mutate(iso = case_when(is.na(parent_id) & is.na(grandparent_id) ~ id,
                            !is.na(parent_id) & is.na(grandparent_id) ~ parent_id,
                            !is.na(parent_id) & !is.na(grandparent_id) ~ grandparent_id))  %>% 
    mutate(iso = case_when(iso == "NOMC" & is.na(grandparent_id) ~ id,
                            iso == "NOMC" & !is.na(grandparent_id) ~ parent_id,
                            T ~ iso)) %>% 
    mutate(iso = ifelse(iso == "NOMC", id, iso))  %>% 
    dplyr::select(-c(name))

# organize GDP data:
# for CHN, IND and USA: data will be sourced directly from their respective official statistical agencies
exclude_from_oecd <- c("ZAF", "IRL", "ISR", "CHN", "IND", "ISL", "USA", "LUX", "MLT")

# The second-level regional GDP data for Japan (JPN) in 2019 is missing and will be inferred by summing the third-level data.
JPN_2019 <- oecd_data  %>% 
    left_join(stru_more)  %>%
    filter(iso == "JPN", year == 2019)  %>% 
    filter(!parent_id == "JPN")  %>% 
    group_by(parent_id, year)  %>% 
    mutate(sum_gdp = sum(rgdp_total))  %>% 
    ungroup()  %>% 
    distinct(year, parent_id, sum_gdp)  %>% 
    as.data.frame()  %>% 
    rename(id = parent_id)

# now organize the downloaded data
oecd_2012_2020_pre <- oecd_data  %>% 
    left_join(stru_more)  %>% 
    arrange(iso, admin_unit, parent_id, id)  %>% 
    filter(!iso %in% exclude_from_oecd)  %>% # drop countries that have too many missing years
    filter(!id %in% exclude_from_oecd)  %>%  # drop countries that have too many missing years
    filter(!(iso == "RUS" & year == 2020)) %>% # Russia's year 2020 data are missing
    filter(!(id == "RUS" & year == 2020))  %>% # Russia's year 2020 data are missing
    filter(!(iso == "JPN" & year == 2020)) %>% # Japan's year 2020 data are missing
    mutate(rgdp_total = ifelse(id == "NO0B1", 0, rgdp_total)) %>% # change the NO0B1's NA value into 0, which is true
    left_join(JPN_2019)  %>% # fulfill JPN's 2019 second level data
    mutate(rgdp_total = ifelse(year == 2019 & id %in% JPN_2019$id, sum_gdp, rgdp_total))  %>% # change the NA to what we calculated through summing third level data
    dplyr::select(-c(sum_gdp))  %>% 
    mutate(across(.cols = c("year", "rgdp_total"),
                    .fns = ~ as.numeric(.x))) %>% 
    filter(!str_detect(name, "not regionalised|unregionalised")) %>% # remove those gdp that are not regionalized
    filter(!id %in% c("EU27_2020")) # we do not need this

# now we need to drop some redundant data: 
#   for example: EE00 is just country level data, but the sample aready has EST as country level data
#                AT130 repeats with AT13
#                ...
oecd_2012_2020 <- oecd_2012_2020_pre  %>% 
    group_by(iso, year) %>% 
    mutate(admin_levels = length(unique(admin_unit)),
          TL_2_length = sum(admin_unit == 2)) %>% 
    ungroup()  %>% 
    filter(!(admin_unit == 2 & TL_2_length == 1)) %>% # This process removes TL2-level data that duplicates TL1-level data.
    mutate(admin_unit = ifelse(TL_2_length == 1 & admin_unit == 3, 2, admin_unit)) %>% # After removing the TL2 data for certain countries, the TL3 data should be reclassified as TL2.
    group_by(iso, year, parent_id) %>% 
    mutate(sub_units = n()) %>% 
    ungroup()  %>% 
    mutate(redundant_admin = sub_units == 1 & admin_unit == 3) %>% # This process identifies TL3-level data that duplicates TL2-level data.
    mutate(redundant_admin = ifelse(redundant_admin, parent_id, NA)) %>% # Identify the parent id of the repetitive TL3-level data.
    mutate(drop = id %in% setdiff(unique(redundant_admin), NA))  %>% # We aim to remove the repeated TL2-level data, and keep TL3-level information.
    filter(!drop) %>% # drop it
    mutate(parent_id = ifelse(!is.na(redundant_admin), grandparent_id, parent_id),
           parent_name = ifelse(!is.na(redundant_admin), grandparent_name, parent_name))  %>% # Update the parent_id and parent_name for the TL3-level data, assigning the parent as the country rather than TL2.
    group_by(iso) %>% 
    mutate(recode = !2 %in% admin_unit) %>% # Identify the entries that do not have a corresponding TL2 level within the current group.
    ungroup()  %>% 
    mutate(admin_unit = ifelse(recode & admin_unit == 3, 2, admin_unit)) %>%  # For entries without TL2 but with TL3, reclassify the TL3 data as TL2.
    mutate(parent_id = ifelse(admin_unit == 3 & parent_id == iso, NA, parent_id)) %>% # Set the parent_id of the TL3-level data that duplicates TL2 information to NA, as their parent id have been removed.
    dplyr::select(-c(admin_levels, TL_2_length, recode, redundant_admin, sub_units, drop, grandparent_id, grandparent_name))

# obtain oecd year 2021's data
oecd_2021 <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/regional/oecd/OECD.CFE.EDS,DSD_REG_ECO@DF_GDP,2.0+all.csv")  %>% 
  dplyr::select(c(TERRITORIAL_LEVEL, REF_AREA, Reference.area, Measure, Price.base, Unit.of.measure, TIME_PERIOD, OBS_VALUE, COUNTRY,Observation.status))  %>% 
  filter(Measure == "Gross domestic product",
         Price.base == "Current prices",
         Unit.of.measure == "National currency",
         TIME_PERIOD == 2021)  %>% 
  mutate(TERRITORIAL_LEVEL = ifelse(TERRITORIAL_LEVEL == "CTRY", "TL1", TERRITORIAL_LEVEL),
         TERRITORIAL_LEVEL = as.numeric(gsub("TL", "", TERRITORIAL_LEVEL)))  %>% 
  rename(admin_unit = TERRITORIAL_LEVEL,
         id = REF_AREA,
         name = Reference.area,
         year = TIME_PERIOD,
         rgdp_total = OBS_VALUE)  %>% 
  dplyr::select(c(admin_unit, id, year, rgdp_total))  %>% 
  filter(id %in% oecd_2012_2020$id)  %>% 
  mutate(admin_unit = ifelse(substr(id,1,3) == "NZ0", 2, admin_unit)) # change NZL's subnational regions' admin_unit to 2 to merge with "oecd_2012_2020"'s data, currently, it is 3

# merge them: be careful if the definition of id changes
#   Verify by checking the "OECD Territorial Correspondence" for confirmation.
oecd_regional_rgdp <- oecd_2012_2020  %>% 
  distinct(admin_unit, name, id, parent_id, parent_name, iso, .keep_all = FALSE)  %>% 
  mutate(year = 2021)  %>% 
  left_join(oecd_2021 %>% mutate(admin_unit = as.character(admin_unit)))  %>%  
  mutate(rgdp_total = ifelse(id == "NO0B1" & year == 2021, 0, rgdp_total))  %>% # The value for "NO0B1" is missing; however, this is acceptable as the GDP for this location is 0
  filter(!iso %in% c("BGR", "BRA", "EST", "HRV", "IDN", "JPN", "LVA", "PER", "ROU", "RUS")) %>% # The 2021 data for these countries is missing at the time of data download.
  bind_rows(oecd_2012_2020)

# obtain TL1 data
oecd_1 <- filter(oecd_regional_rgdp, admin_unit == 1) %>% 
  dplyr::select(-c(id, parent_id, parent_name)) %>% 
  pivot_wider(names_from = admin_unit, 
              values_from = c("name", starts_with("rgdp")),
              names_glue = "admin_{admin_unit}_{.value}")

# obtain TL2 data
oecd_2 <- filter(oecd_regional_rgdp, admin_unit == 2) %>% 
  dplyr::select(-c(parent_id, parent_name)) %>% 
  pivot_wider(names_from = admin_unit, 
              values_from = c("name", starts_with("rgdp")),
              names_glue = "admin_{admin_unit}_{.value}") %>% 
  rename(admin_2_id = id)

# obtain TL3 data and "merge" it with TL1 and TL2
oecd_regional_data_clean_pre <- filter(oecd_regional_rgdp, admin_unit == 3) %>% 
  rename(admin_2_id = parent_id, admin_3_id = id) %>% 
  dplyr::select(-parent_name) %>% 
  pivot_wider(names_from = admin_unit, 
              values_from = c("name", starts_with("rgdp")),
              names_glue = "admin_{admin_unit}_{.value}") %>% 
  left_join(oecd_2) %>% 
  bind_rows(filter(oecd_2, !admin_2_id %in% .$admin_2_id)) %>% 
  left_join(oecd_1) %>% 
  mutate(across(matches("rgdp"),
                .fns = ~ as.numeric(.x))) %>% 
  dplyr::select(year, iso, starts_with("admin_3"), starts_with("admin_2"), 
         starts_with("admin_1")) %>% 
  arrange(iso, admin_2_id, admin_3_id, year)

# fulfill the data
RUS_2020 <- read.csv("step2_obtain_gdp_data/temp/RUS_2020_2021.csv") # see "1_RUS.R" file for the data
BRA_2021 <- read.csv("step2_obtain_gdp_data/temp/BRA_2021.csv") # see "2_BRA.R" file for the data

# bind RUS_2020 and BRA_2021, but note the following geometry changes:
# When you update, please go to official statistical agency website to check whether OECD corrects this
#   1. For New Zealand, GDP of "Hawke's Bay Region" is included in "Gisborne";
#                       GDP of "West Coast Region" is included in "Tasman-Nelson-Marlborough"
#                       Canterbury includes Chatham Islands.
#   2. For Chile, Nuble region (CLO16) is included in the Bio-Bio region (CL08)
#   3. For Indonesia, OECD excludes the region "North Kalimantan Province", we can obatin its gdp by using national GDP - other regions total GDP
# Don't forget to change the geometries below

idn_north_kalim <- oecd_regional_data_clean_pre  %>% 
  filter(iso == "IDN")  %>% 
  group_by(iso, year)  %>% 
  summarize(year = first(year),
            iso = "IDN",
            admin_3_id = first(admin_3_id),
            admin_3_name = first(admin_3_name),
            admin_3_rgdp_total = first(admin_3_rgdp_total),
            admin_2_id = "ID34",
            admin_2_name = "North Kalimantan",
            admin_2_rgdp_total = first(admin_1_rgdp_total) - sum(admin_2_rgdp_total),
            admin_1_name = "Indonesia",
            admin_1_rgdp_total = first(admin_1_rgdp_total))  %>% 
  ungroup()

oecd_regional_data_clean <- bind_rows(oecd_regional_data_clean_pre, RUS_2020, BRA_2021, idn_north_kalim)  %>% 
  mutate(admin_2_name = ifelse(admin_2_name == "Biobío (Región)", "Biobío (Región) + Ñuble", admin_2_name))  %>% 
  mutate(admin_2_name = ifelse(admin_2_name == "Gisborne", "Gisborne + Hawke's Bay", admin_2_name))  %>% 
  mutate(admin_2_name = ifelse(admin_2_name == "Tasman-Nelson-Marlborough", "Tasman-Nelson-Marlborough + West Coast", admin_2_name))

write.csv(oecd_regional_data_clean, "step2_obtain_gdp_data/temp/oecd_gdp_clean.csv", row.names = F)

# obtain training df
training_df <- oecd_regional_data_clean %>%
  mutate(min_admin_unit = ifelse(is.na(admin_3_id), 2, 3)) %>% 
  mutate(unit_name = ifelse(min_admin_unit == 2, admin_2_name, admin_3_name),
         id = ifelse(min_admin_unit == 2, admin_2_id, admin_3_id),
         unit_rgdp_total = ifelse(min_admin_unit == 2, admin_2_rgdp_total, admin_3_rgdp_total))  %>% 
  mutate(across(starts_with("admin_2"),
                .fns = ~ ifelse(min_admin_unit == 2, NA, .x))) %>% 
  dplyr::select(-starts_with("admin_3")) %>% 
  rename_with(matches("admin_2|admin_1"), 
              .fn = ~ paste0(str_sub(.x, 1, 8), "parent_", str_sub(.x, 9, -1)))  %>% 
  pivot_longer(cols = matches("admin_2|admin_1"),
               names_to = c("parent_admin_unit", ".value"),
               names_pattern = "admin_(\\d)_(.+)")  %>% 
  drop_na(parent_name) %>% 
  # mutate(training = !is.na(unit_rgdp_total)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_id, parent_name, matches("parent_rgdp")) %>% 
  mutate(parent_id = ifelse(parent_admin_unit == 1, iso, parent_id)) %>% 
  group_by(year, iso, parent_admin_unit, parent_id, parent_name) %>% 
  mutate(sub_regions = sum(!is.na(unit_rgdp_total))) %>% 
  group_by(iso, parent_admin_unit, parent_name) %>% 
  mutate(all_sub_regions = sub_regions == max(sub_regions)) %>% 
  ungroup()  %>% 
  # mutate(training = ifelse(training, all_sub_regions & training, training)) %>% 
  recode_nuts(geo_var = "id", nuts_year = 2021) %>% # nuts_year corresponds to OECD Territorial correspondence 
  dplyr::select(-c(all_sub_regions, sub_regions, typology, typology_change, code_2021))

write.csv(training_df, "step2_obtain_gdp_data/temp/oecd_training_data.csv", row.names = F)

# ------------------------------------------------- #
# Obtain geometries

## NUTS data ----- there are some geometries we can obtain very easily 

nuts_sf_pre <- lapply(c("0", "1", "2", "3"), function(admin_level){
  
  df_out <- gisco_get_nuts(year = "2021", epsg = "4326", 
                           resolution = "10", nuts_level = admin_level) %>% 
    mutate(LEVL_CODE = ifelse(LEVL_CODE == "0", "1", LEVL_CODE))
  
  return(df_out)
  
}) %>% do.call('rbind', .) %>% 
  rename(id = NUTS_ID) %>% 
  left_join(dplyr::select(codelist, eurostat, iso3c), by = c("CNTR_CODE" = "eurostat")) %>% 
  mutate(iso = iso3c, id = ifelse(id == CNTR_CODE, iso3c, id)) %>% 
  dplyr::select(id, iso, geometry) %>% 
  rename(geom = geometry)

st_write(nuts_sf_pre, "step2_obtain_gdp_data/temp/nuts_sf_pre.gpkg", append = F)

# remove large inland waters
difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = "step2_obtain_gdp_data/temp/nuts_sf_pre.gpkg", 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step2_obtain_gdp_data/temp/nuts_sf.gpkg", 
  .quiet = FALSE
)

nuts_sf <- read_sf("step2_obtain_gdp_data/temp/nuts_sf.gpkg")

## Non-NUTS data -----
non_nuts_regions <- filter(training_df, !id %in% nuts_sf$id) %>% 
  filter(parent_admin_unit == 1) %>% 
  group_by(iso, parent_name, unit_name, min_admin_unit, id) %>% 
  reframe()

non_nuts_countries <- non_nuts_regions %>% 
  group_by(iso, parent_name) %>% 
  reframe(min_admin_unit = min(min_admin_unit))

# Don't forget to change the geometries of NZL and Chile
#   1. For New Zealand, GDP of "Hawke's Bay Region" is included in "Gisborne";
#                       GDP of "West Coast Region" is included in "Tasman-Nelson-Marlborough"
#                       Canterbury includes Chatham Islands.
#   2. For Chile, Nuble region (CLO16) is included in the Bio-Bio region (CL08)
nzl <- read_sf("step1_obtain_gis_data/outputs/CGAZ_ADM1_without_large_waters.gpkg") %>% 
  filter(shapeGroup == "NZL") %>% 
  rename(name = shapeName, iso = shapeGroup) %>% 
  mutate(name = ifelse(iso == "NZL", gsub(" Region$", "", name), name))  %>% 
  mutate(region = case_when(name %in% c("Tasman", "Nelson", "Marlborough", "West Coast") ~ "Tasman-Nelson-Marlborough + West Coast",
                            name %in% c("Gisborne", "Hawke's Bay") ~ "Gisborne + Hawke's Bay",
                            name %in% c("Canterbury", "Chatham Islands Territory") ~ "Canterbury",
                            TRUE ~ name))  %>% 
  group_by(region, iso) %>% 
  summarize(geom = st_union(geom), .groups = "drop") %>% 
  rename(name = region) 

chl <- read_sf("step1_obtain_gis_data/outputs/CGAZ_ADM1_without_large_waters.gpkg") %>% 
  filter(shapeGroup == "CHL") %>% 
  rename(name = shapeName, iso = shapeGroup)  %>% 
  mutate(name = iconv(name, from = "UTF-8", to = "LATIN1"))  %>% 
  mutate(region = case_when(name %in% c("Región de Ñuble", "Región del Bío-Bío") ~ "Biobío (Región) + Ñuble",
                            TRUE ~ name))  %>% 
  group_by(region, iso) %>% 
  summarize(geom = st_union(geom), .groups = "drop") %>% 
  rename(name = region) 

non_nuts_base_regions <- read_sf("step1_obtain_gis_data/outputs/CGAZ_ADM1_without_large_waters.gpkg") %>% 
  filter(shapeGroup %in% (pull(non_nuts_countries, iso) %>% setdiff(c("NZL", "CHL")))) %>% 
  rename(name = shapeName, iso = shapeGroup)  %>% 
  mutate(name = ifelse(iso == "IDN", paste0(name, " Province"), name)) %>% 
  mutate(name = ifelse(iso == "JPN", gsub(" Prefecture$", "", name), name))  %>% 
  dplyr::select(name, iso, geom)  %>% 
  bind_rows(nzl, chl)  %>% 
  filter(!(iso == "AUS"& name == "Other Territories"))  %>% 
  mutate(name = case_when(# KOR
                          name == "Gyeonggi" ~ "Gyeonggi-do",
                          name == "South Gyeongsang" ~ "Gyeongsangnam-do",
                          name == "North Gyeongsang" ~ "Gyeongsangbuk-do",
                          name == "North Jeolla" ~ "Jeollabuk-do",
                          name == "South Jeolla" ~ "Jeollanam-do",
                          name == "North Chungcheong" ~ "Chungcheongbuk-do",
                          name == "South Chungcheong" ~ "Chungcheongnam-do",
                          name == "Gangwon" ~ "Gangwon-do",
                          name == "Jeju" ~ "Jeju-do",
                          # AUS
                          name == "Australian Capital Territory" ~ "Canberra region (ACT)",
                          name == "Archipiélago de San Andrés, Providencia y Santa Catalina" ~ "San Andrés",
                          name == "Bogota Capital District" ~ "Bogotá Capital District",
                          name == "Córdoba" ~ "Córdoba (CO)",
                          # BRA
                          name == "Amapa" ~ "Amapá",
                          name == "Ceara" ~ "Ceará",
                          name == "Espirito Santo" ~ "Espírito Santo",
                          name == "Goias" ~ "Goiás",
                          name == "Maranhao" ~ "Maranhão",
                          name == "Para" ~ "Pará",
                          name == "Paraiba" ~ "Paraíba",
                          name == "Parana" ~ "Paraná",
                          name == "Piaui" ~ "Piauí",
                          name == "Rondonia" ~ "Rondônia",
                          name == "Sao Paulo" ~ "São Paulo",
                          name == "Rio Granda do Norte" ~ "Rio Grande do Norte",
                          name == "Rio de Jeneiro" ~ "Rio de Janeiro",
                          name == "Distrito Federal" & iso == "BRA" ~ "Distrito Federal (BR)",
                          # IDN
                          name == "Central Kalimantan Province" ~ "Middle Kalimantan Province",
                          name == "Central Sulawesi Province" ~ "Middle Sulawesi Province",
                          name == "Jakarta Special Capital Region Province" ~ "DKI Jakarta Province",
                          name == "Special Region of Yogyakarta Province" ~ "D.I. Yogyakarta Province",
                          name == "East Nusa Tenggara Province" ~ "Eastern Lesser Sundas Province",
                          name == "North Sumatra Province" ~ "North Sumatera Province",
                          name == "Southeast Sulawesi Province" ~ "South East Sulawesi Province",
                          name == "West Nusa Tenggara Province" ~ "Western Lesser Sundas Province",
                          name == "West Sumatra Province" ~ "West Sumatera Province",
                          name == "South Sumatra Province" ~ "South Sumatera Province",
                          name == "Bangka-Belitung Islands Province" ~ "Bangka Belitung Province",
                          name == "North Kalimantan Province" ~ "North Kalimantan",
                          name == "Riau Islands Province" ~ "Riau Mainland Province",
                          name == "Riau Province" ~ "Riau Province",
                          # MEX
                          name == "MichoacÃ¡n de Ocampo" ~ 'Michoacan',
                          name == "Nuevo LeÃ³n" ~ "Nuevo Leon",
                          name == "QuerÃ©taro de Arteaga" ~ "Queretaro",
                          name == "San Luis PotosÃ­" ~ "San Luis Potosi",
                          name == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                          name == "YucatÃ¡n" ~ "Yucatan",
                          name == "Coahuila de Zaragoza" ~ "Coahuila",
                          name == "MÃ©xico" ~ "Mexico",
                          name == "Distrito Federal" & iso == "MEX" ~ "Mexico City",
                          # RUS
                          name == "Ingushetia" ~ "Republic of Ingushetia",
                          name == "Khanty-Mansiysk Autonomous Okrug – Ugra" ~ "Khanty-Mansi Autonomous Okrug",
                          name == "Adygea" ~ "Republic of Adygea",
                          name == "Khakassia" ~ "Republic of Khakassia",
                          name == "Tatarstan" ~ "Republic of Tatarstan",
                          name == "Buryatia" ~ "Republic of Buryatia",
                          name == "Chechnya" ~ "Chechen Republic",
                          name == "Chuvashia" ~ "Chuvash Republic",
                          name == "North Ossetia–Alania" ~ "Republic of North Ossetia-Alania",
                          name == "Kabardino-Balkaria" ~ "Kabardino-Balkar Republic",
                          name == "Leningrad oblast" ~ "Leningrad Oblast",
                          name == "Mari El" ~ "Mari El Republic",
                          name == "Tuva" ~ "Tuva Republic",
                          name == "Udmurtia" ~ "Udmurt Republic",
                          name == "Kalmykia" ~ "Republic of Kalmykia",
                          name == "Karachay-Cherkessia" ~ "Karachay-Cherkess Republic",
                          name == "Bashkortostan" ~ "Republic of Bashkortostan",                          
                          name == "Dagestan" ~ "Republic of Dagestan",
                          name == "Kaliningrad" ~ "Kaliningrad Oblast",
                          # PER
                          name == "Amazonas" & iso == "PER" ~ "Amazonas (PE)",
                          name == "Ancash" ~ "Áncash",
                          name == "Madre de Dios" ~ "Madre de dios",
                          name == "Callao" ~ "Prov. const. del Callao",
                          name == "San Martín" ~ "San Martin",
                          # CHL
                          name == "Región de Antofagasta" ~ "Antofagasta",
                          name == "Región de Arica y Parinacota" ~ "Arica y Parinacota",
                          name == "Región de Atacama" ~ "Atacama",
                          name == "Región de Aysén del Gral.Ibañez del Campo" ~ "Aysén",
                          name == "Región de Coquimbo" ~ "Coquimbo",
                          name == "Región de La Araucanía" ~ "Araucanía",
                          name == "Región de Los Lagos" ~ "Los Lagos",
                          name == "Región de Los Ríos" ~ "Los Ríos",
                          name == "Región de Magallanes y Antártica Chilena" ~ "Magallanes and Chilean Antarctica",
                          name == "Región de Tarapacá" ~ "Tarapacá",
                          name == "Región de Valparaíso" ~ "Valparaíso",
                          name == "Región del Libertador Bernardo O'Higgins" ~ "O'Higgins",
                          name == "Región del Maule" ~ "Maule",
                          name == "Región Metropolitana de Santiago" ~ "Santiago Metropolitan Region",
                          T ~ name))                

st_write(non_nuts_base_regions, "step2_obtain_gdp_data/temp/non_nuts_base_regions.gpkg", append = F)

non_nuts_aggregate_regions <- oecd_regional_data_clean %>% 
  filter(iso %in% pull(filter(non_nuts_countries, min_admin_unit == 3), iso)) %>% 
  drop_na(admin_2_name) %>% 
  group_by(iso, admin_2_id, admin_2_name, admin_3_name) %>% 
  reframe() %>% 
  left_join(non_nuts_base_regions, by = c("admin_3_name" = "name", "iso" = "iso"))  %>% 
  st_as_sf() %>% 
  group_by(iso, admin_2_id, admin_2_name) %>% 
  summarize(geom = st_union(geom), .groups = "drop") %>% 
  rename(id = admin_2_id) %>% 
  dplyr::select(id, iso, geom)

aggregate <- qgisprocess::qgis_run_algorithm(
  "native:aggregate",
  INPUT = "step2_obtain_gdp_data/temp/non_nuts_base_regions.gpkg", 
  GROUP_BY = "iso",  
  AGGREGATES = list(list("aggregate" = "concatenate", "input" = '"iso"', "delimiter" = ",", "name" = "iso", "type" = 10, "length" = 0, "precision" = 0)),
  OUTPUT = "step2_obtain_gdp_data/temp/non_nuts_nations.gpkg"
)

non_nuts_nations <- read_sf("step2_obtain_gdp_data/temp/non_nuts_nations.gpkg")  %>%  
  mutate(iso = substr(iso, 1, 3))  %>% 
  mutate(id = iso)  %>% 
  dplyr::select(c(id, iso, geom))

## OECD polygons -----

oecd_sf <- non_nuts_base_regions %>% 
  left_join(dplyr::select(non_nuts_regions, id, iso, unit_name), by = c("name" = "unit_name", "iso" = "iso")) %>% 
  dplyr::select(id, iso, geom) %>% 
  rbind(non_nuts_aggregate_regions) %>% 
  rbind(non_nuts_nations)  %>% 
  mutate(geom = st_set_crs(geom, st_crs(nuts_sf))) %>%
  rbind(nuts_sf)

training_poly <- oecd_sf %>% 
  filter(id %in% training_df$id)

st_write(oecd_sf, "step2_obtain_gdp_data/temp/oecd_poly.gpkg", append = F)
st_write(training_poly, "step2_obtain_gdp_data/temp/oecd_training_poly.gpkg", append = F)
