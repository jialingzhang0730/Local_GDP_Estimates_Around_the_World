# --------------------------------- Task Summary --------------------------------- #
# This file retrieves new updated national data, including the following:
#   1. Constant international dollars (PPP adjusted)
#   2. Current international dollars (PPP adjusted)
#   3. Current US dollars
#   4. Constant US dollars (not provided by IMF, needs to be calculated)
#   5. Population data
# IMF data should be used as the primary source, as the IMF adjusts fiscal year 
#   GDP data to calendar years for countries that report on a fiscal year basis. 
#   The World Bank, however, does not make this adjustment. If IMF data is missing
#   for certain countries or years, use World Bank or UN data to supplement.
# For consistency across scripts, all values should be converted to billions (xxx)
#
# In version2_year2012_2022, the "MNE" data are obtained from the World Bank, 
#   whereas in version1_year2012_2021, they were obtained from the IMF.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(lubridate)
library(wbstats)
library(jsonlite)
library(httr)
library(readxl)

# GDP data

# we can first get most countries' GDP data from IMF

# constant 2021 international dollars PPP adjusted
imf_pop <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/IMF_data/WEO_Data.xlsx")  %>% 
  filter(`Subject Descriptor` == "Population",
          Units == "Persons") %>%  # select population
  dplyr::select(-c("Subject Descriptor", "Units", "Scale", "Country/Series-specific Notes", "Estimates Start After"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  rename(iso = ISO)  %>%  
  filter(!iso %in% c("ERI", "SYR", "MNE"))  %>% # those two isos have missing values, so we need to find the data from other sources
  mutate(year = as.numeric(year),
         population = as.numeric(value)*1e6)  %>% # the unit now is in how many persons
  dplyr::select(c(iso, year, population, Country))

imf_gdp_const_2021_PPP <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/IMF_data/WEO_Data.xlsx")  %>% 
  filter(`Subject Descriptor` == "Gross domestic product per capita, constant prices",
          Units == "Purchasing power parity; 2021 international dollar")  %>% # select current international dollars PPP adjusted
  dplyr::select(-c("Country", "Subject Descriptor", "Units", "Scale", "Country/Series-specific Notes", "Estimates Start After"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  rename(iso = ISO)  %>%  
  filter(!iso %in% c("ERI", "SYR", "MNE"))  %>% # those two isos have missing values, so we need to find the data from other sources
  mutate(year = as.numeric(year),
         rgdp_total = as.numeric(value))  %>% # the unit is in constant 2021 international dollar
  dplyr::select(c(iso, year, rgdp_total)) %>% 
  left_join(imf_pop)  %>% 
  mutate(rgdp_total = rgdp_total * population/1e9) # now the unit is in billion constant 2021 international dollar

# current international dollars PPP adjusted
imf_gdp_crt_PPP <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/IMF_data/WEO_Data.xlsx")  %>% 
  filter(`Subject Descriptor` == "Gross domestic product, current prices",
          Units == "Purchasing power parity; international dollars")  %>% # select current international dollars PPP adjusted
  dplyr::select(-c("Country", "Subject Descriptor", "Units", "Scale", "Country/Series-specific Notes", "Estimates Start After"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  rename(iso = ISO)  %>%  
  filter(!iso %in% c("ERI", "SYR", "MNE"))  %>% # those two isos have missing values, so we need to find the data from other sources
  mutate(year = as.numeric(year),
         rgdp_total = as.numeric(value))  %>% # the unit is in current billion international dollars
  dplyr::select(c(iso, year, rgdp_total)) %>% 
  left_join(imf_pop)

# current US dollars
imf_gdp_crt_us <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/IMF_data/WEO_Data.xlsx")  %>% 
  filter(`Subject Descriptor` == "Gross domestic product, current prices",
          Units == "U.S. dollars")  %>% # select current US dollars
  dplyr::select(-c("Country", "Subject Descriptor", "Units", "Scale", "Country/Series-specific Notes", "Estimates Start After"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  rename(iso = ISO)  %>% 
  filter(!iso %in% c("ERI", "SYR", "MNE"))  %>% # those two isos have missing values, so we need to find the data from other sources
  mutate(year = as.numeric(year),
         rgdp_total = as.numeric(value))  %>% # the unit is in current billion US dollars
  dplyr::select(c(iso, year, rgdp_total)) %>% 
  left_join(imf_pop)

# constant 2021 US dollars
# we need to change current US dollars to constant 2021 US dollars
# The method to change current US dollars to constant 2021 US dollars is (according to United States Census Bureau website):
#     value*(2021 index/current year index)
index <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/US_census_bureau_data/annual-index-value_annual-percent-change.xls", skip = 2)  %>% 
  rename(year = `Income year`, index = `C-CPI-U1\nIndex\n(December 1999 = 100)`)  %>% 
  dplyr::select(year, index)  %>% 
  na.omit() %>% # omit those "notes" that it reads
  mutate(year = as.numeric(year))  

# obtain 2021's index value
index_2021 <- index  %>% 
  filter(year == 2021)  %>% 
  pull(index)

imf_gdp_const_2021 <- imf_gdp_crt_us  %>% 
  left_join(index)  %>% 
  mutate(index_2021 = index_2021)  %>% 
  mutate(rgdp_total = rgdp_total * (index_2021/index))  %>% 
  dplyr::select(c(iso, year, rgdp_total, population, Country))

# ------------------------------------------------- #
# we need to get more countries GDP data from World Bank
iso_get_fm_wb <- c("BMU", "CYM", "CUW", "GRL", "XKX", "LIE", "MCO", "SXM", "SYR", "TCA", "PSE", "MNE")

# population
wb_pop <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/world_bank_data/population/API_SP.POP.TOTL_DS2_en_excel_v2_19296.xls", sheet = "Data", skip = 3)  %>% 
  rename(iso = `Country Code`)  %>% 
  filter(iso %in% iso_get_fm_wb)  %>% # only select those isos that IMF misses and those we want from world bank 
  dplyr::select(-c("Indicator Name", "Indicator Code"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  filter(year >= 2012, year <= 2022) %>% # only select years 2012-2022 for now
  mutate(year = as.numeric(year))  %>% 
  rename(population = value, Country = `Country Name`) # the unit is how many people

# current US dollars
wb_gdp_crt_us <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/world_bank_data/GDP_current_USdollar/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_19310.xls", sheet = "Data", skip = 3)  %>% 
  rename(iso = `Country Code`)  %>% 
  filter(iso %in% iso_get_fm_wb)  %>% # only select those isos that IMF misses and those we want from world bank 
  dplyr::select(-c("Country Name", "Indicator Name", "Indicator Code"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  filter(year >= 2012, year <= 2022) %>% # only select years 2012-2022 for now
  rename(rgdp_total = value)  %>% 
  mutate(rgdp_total = rgdp_total/1e9,
         year = as.numeric(year)) %>% # change the unit to current billion US dollars
  left_join(wb_pop)

# constant 2021 US dollars
wb_gdp_const_2021 <- wb_gdp_crt_us  %>% 
  left_join(index)  %>% 
  mutate(index_2021 = index_2021)  %>% 
  mutate(rgdp_total = rgdp_total * (index_2021/index))  %>% # change the unit to constant 2021 billion US dollars
  dplyr::select(c(iso, year, rgdp_total, population, Country))

# current international dollars PPP adjusted
# Note: "SYR", "LIE", "MCO" have missing data for this one
wb_gdp_crt_PPP <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/world_bank_data/GDP_PPP_current/API_NY.GDP.MKTP.PP.CD_DS2_en_excel_v2_19548.xls", sheet = "Data", skip = 3)  %>% 
  rename(iso = `Country Code`)  %>% 
  filter(iso %in% iso_get_fm_wb)  %>% # only select those isos that IMF misses and those we want from world bank 
  dplyr::select(-c(`Country Name`, "Indicator Name", "Indicator Code"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  filter(year >= 2012, year <= 2022) %>% # only select years 2012-2022 for now
  rename(rgdp_total = value)  %>% 
  mutate(rgdp_total = rgdp_total/1e9,
         year = as.numeric(year)) %>% # change the unit to current billion international dollars PPP ajusted
  left_join(wb_pop)

# constant 2021 international dollars PPP adjusted:
# we do not have LIE and MCO's data
wb_gdp_const_2021_PPP <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/world_bank_data/GDP_PPP_const_2021/API_NY.GDP.MKTP.PP.KD_DS2_en_excel_v2_20528.xls", sheet = "Data", skip = 3)  %>% 
  rename(iso = `Country Code`)  %>% 
  filter(iso %in% iso_get_fm_wb)  %>% # only select those isos that IMF misses and those we want from world bank 
  dplyr::select(-c(`Country Name`, "Indicator Name", "Indicator Code"))  %>% 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  filter(year >= 2012, year <= 2022) %>% # only select years 2012-2022 for now
  rename(rgdp_total = value)  %>% 
  mutate(rgdp_total = rgdp_total/1e9,
         year = as.numeric(year)) %>% # change the unit to constant 2021 billion international dollars PPP adjusted
  left_join(wb_pop)

# ------------------------------------------------- #
# However, those three countries still have missing data, so we need to obtain them from UNdata
#   Note, the downloaded data from UNdata are Per capita GDP at current prices - US dollars!
#   Even though in the downloaded file, it said "Gross Domestic Product (GDP)", it is NOT.
#   That means we need population data
iso_get_fm_un <- c("CUB", "ERI", "PRK")
pop_cub_eri_prk <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/national/world_bank_data/population/API_SP.POP.TOTL_DS2_en_excel_v2_19296.xls", sheet = "Data", skip = 3)  %>% 
  rename(iso = `Country Code`, Country = `Country Name`)  %>% 
  filter(iso %in% iso_get_fm_un) %>% # only those we need 
  pivot_longer(cols = matches("\\d{4}"), names_prefix = "X", names_to = "year")  %>% 
  rename(population = value)  %>% 
  dplyr::select(c(iso, year, population, Country))  %>% 
  mutate(year = as.numeric(year))  %>% 
  filter(year >= 2012, year <= 2022) # We only need data from 2012 to 2022. The inclusion of 2022 is intentional to facilitate future updates.

# GDP in current US dollars
un_gdp_crt_us <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/national/UN_data/UNdata_Export_20250426_211303453.csv") %>% 
  mutate(iso = ifelse(`Country.or.Area` == "Cuba", "CUB",
               ifelse(`Country.or.Area` == "Democratic People's Republic of Korea", "PRK",
               ifelse(`Country.or.Area` == "Eritrea", "ERI", NA))))  %>% 
  rename(year = Year, rgdp_pc = Value)  %>% 
  mutate(year = as.numeric(year), rgdp_pc = as.numeric(rgdp_pc))  %>% 
  dplyr::select(c(iso, year, rgdp_pc))  %>% 
  left_join(pop_cub_eri_prk)  %>% 
  mutate(rgdp_total = rgdp_pc*population/1e9)  %>% # the unit now is current billion US dollars 
  dplyr::select(c(iso, year, rgdp_total, population, Country))

# GDP in constant US dollars
un_gdp_const_2021 <- un_gdp_crt_us  %>% 
  left_join(index)  %>% 
  mutate(index_2021 = index_2021)  %>% 
  mutate(rgdp_total = rgdp_total * (index_2021/index))  %>% # now the unit is constant 2021 billion US dollars
  dplyr::select(c(iso, year, rgdp_total, population, Country))

# we do not have their GDP in PPP adjusted, but that's fine

# ------------------------------------------------- #
# Merge the database

national_gdp_current_USD <- bind_rows(imf_gdp_crt_us, wb_gdp_crt_us, un_gdp_crt_us)
national_gdp_const_2021_USD <- bind_rows(imf_gdp_const_2021, wb_gdp_const_2021, un_gdp_const_2021)
national_gdp_current_PPP <- bind_rows(imf_gdp_crt_PPP, wb_gdp_crt_PPP)
national_gdp_const_2021_PPP <- bind_rows(imf_gdp_const_2021_PPP, wb_gdp_const_2021_PPP)

write.csv(national_gdp_current_USD, "step2_obtain_gdp_data/temp/national_gdp_current_USD.csv", row.names = F)
write.csv(national_gdp_const_2021_USD, "step2_obtain_gdp_data/temp/national_gdp_const_2021_USD.csv", row.names = F)
write.csv(national_gdp_current_PPP, "step2_obtain_gdp_data/temp/national_gdp_current_PPP.csv", row.names = F)
write.csv(national_gdp_const_2021_PPP, "step2_obtain_gdp_data/temp/national_gdp_const_2021_PPP.csv", row.names = F)

# ------------------------------------------------- #
# calculate the GDP per capita

national_gdpc_current_USD <- national_gdp_current_USD  %>% 
  mutate(national_gdpc = rgdp_total/population)
national_gdpc_const_2021_USD <- national_gdp_const_2021_USD %>% 
  mutate(national_gdpc = rgdp_total/population)
national_gdpc_current_PPP <- national_gdp_current_PPP %>% 
  mutate(national_gdpc = rgdp_total/population)
national_gdpc_const_2021_PPP <- national_gdp_const_2021_PPP %>% 
  mutate(national_gdpc = rgdp_total/population)

write.csv(national_gdpc_current_USD, "step2_obtain_gdp_data/temp/national_gdpc_current_USD.csv", row.names = F)
write.csv(national_gdpc_const_2021_USD, "step2_obtain_gdp_data/temp/national_gdpc_const_2021_USD.csv", row.names = F)
write.csv(national_gdpc_current_PPP, "step2_obtain_gdp_data/temp/national_gdpc_current_PPP.csv", row.names = F)
write.csv(national_gdpc_const_2021_PPP, "step2_obtain_gdp_data/temp/national_gdpc_const_2021_PPP.csv", row.names = F)


