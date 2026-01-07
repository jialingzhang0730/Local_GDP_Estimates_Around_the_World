# --------------------------------- Task Summary --------------------------------- #
# This file retrieves Brazil's regional GDP data for years 2021 and 2022.
#
# Brazil's regional data for 2021 was not available on OECD when the original dataset 
# was downloaded (June 8th, 2024). The data is obtained from:
# https://www.ibge.gov.br/en/statistics/economic/national-accounts/16855-regional-accounts-of-brazil.html
# Click "GDP under the point of view of Income (2010-2021) (in xls format)" 
# Note: Use "the point of view of Income" not "Production" to match OECD values for 2012-2020.
#
# For 2022, data is obtained from a different file format from the same source.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(units)
library(sf)
library(docxtractr)
library(janitor)
library(tibble)

# ------------------------------------------------- #
# Obtain BRA 2021 subnational GDP data

# Initialize an empty list to store dataframes
list_of_dfs <- list()

# Each sheet is one region, Loop through each sheet to extract the necessary data
sheet_names <- c("Tabela3", "Tabela4", "Tabela5", "Tabela6", "Tabela7", "Tabela8", "Tabela9",
                 "Tabela11", "Tabela12", "Tabela13", "Tabela14", "Tabela15", "Tabela16", "Tabela17", "Tabela18", "Tabela19",
                 "Tabela21", "Tabela22", "Tabela23", "Tabela24", "Tabela26", "Tabela27", "Tabela28", 
                 "Tabela30", "Tabela31", "Tabela32", "Tabela33") # do not incude "Regiao xxx" and "Brasil"

for (sheet in sheet_names) {
  data <- read_excel("step2_obtain_gdp_data/inputs/gdp_data/regional/BRA/PIB_Otica_Renda_UF.xls", sheet = sheet, col_names = FALSE)  # Read the specific sheet
  admin_2_name <- data[7,1] # Extract the admin_2_name (row 7 is the region name), double check this when you update!!!
  
  # Extract the admin_2_rgdp_total (the intersection of column named "2021" and row named "PIB - Ótica da Renda")
  data2 <- data %>% 
    slice(9:n()) %>% # remove the first 7 rows
    dplyr::select(1:13)  %>% # select the first 13 columns under "Valores correntes (1 000 000 R$)", double check this when you update!!!
    row_to_names(row_number = 1) %>% # put the first row as year column
    as.data.frame()

  admin_2_rgdp_total <- data2[9, "2021"] # row 9 is where "PIB - Ótica da Renda" is, double check this when you update!!!
  
  # Create a dataframe with the extracted values
  df <- data.frame(admin_2_name = admin_2_name,
                   year = 2021,
                   admin_2_rgdp_total = admin_2_rgdp_total)  %>% 
        rename(admin_2_name = "...1")

  # Append the dataframe to the list
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all dataframes into one
final_df_2021 <- bind_rows(list_of_dfs)  %>% 
    mutate(iso = "BRA", admin_1_name = "Brazil")  %>% 
    group_by(iso, year)  %>% 
    mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total))  %>% 
    ungroup()  %>% 
    mutate(admin_2_name = ifelse(admin_2_name == "Distrito Federal", "Distrito Federal (BR)", admin_2_name))  %>% 
      mutate(admin_2_id = case_when(
    admin_2_name == "Acre" ~ "BR01",
    admin_2_name == "Alagoas" ~ "BR08",
    admin_2_name == "Amapá" ~ "BR02",
    admin_2_name == "Amazonas" ~ "BR03",
    admin_2_name == "Bahia" ~ "BR09",
    admin_2_name == "Ceará" ~ "BR10",
    admin_2_name == "Distrito Federal (BR)" ~ "BR24",
    admin_2_name == "Espírito Santo" ~ "BR17",
    admin_2_name == "Goiás" ~ "BR25",
    admin_2_name == "Maranhão" ~ "BR11",
    admin_2_name == "Mato Grosso" ~ "BR26",
    admin_2_name == "Mato Grosso do Sul" ~ "BR27",
    admin_2_name == "Minas Gerais" ~ "BR18",
    admin_2_name == "Paraná" ~ "BR21",
    admin_2_name == "Paraíba" ~ "BR12",
    admin_2_name == "Pará" ~ "BR04",
    admin_2_name == "Pernambuco" ~ "BR13",
    admin_2_name == "Piauí" ~ "BR14",
    admin_2_name == "Rio Grande do Norte" ~ "BR15",
    admin_2_name == "Rio Grande do Sul" ~ "BR22",
    admin_2_name == "Rio de Janeiro" ~ "BR19",
    admin_2_name == "Rondônia" ~ "BR05",
    admin_2_name == "Roraima" ~ "BR06",
    admin_2_name == "Santa Catarina" ~ "BR23",
    admin_2_name == "Sergipe" ~ "BR16",
    admin_2_name == "São Paulo" ~ "BR20",
    admin_2_name == "Tocantins" ~ "BR07",
    TRUE ~ NA))

write.csv(final_df_2021, "version2_year2012_2022/step2_obtain_gdp_data/temp/BRA_2021.csv", row.names = F)

# ------------------------------------------------- #
# Obtain BRA 2022 subnational GDP data

final_df_2022 <- read_excel("version2_year2012_2022/step2_obtain_gdp_data/inputs/gdp_data/regional/BRA/Especiais_2010_2022_xls/tab01.xls", col_names = TRUE) %>% 
    slice(4:n()) %>% # remove the first three rows
    dplyr::select(c(1,14)) %>% 
    rename(admin_2_name = 1, admin_2_rgdp_total = 2) %>% # rename columns
    mutate(year = 2022) %>% 
    filter(!admin_2_name %in% c("Centro-Oeste", "Sul", "Sudeste", "Nordeste", "Norte", "Brasil")) %>% 
    filter(!str_detect(admin_2_name, "Fonte: IBGE"))  %>% 
    mutate(iso = "BRA", admin_1_name = "Brazil")  %>% 
    group_by(iso, year)  %>% 
    mutate(admin_1_rgdp_total = sum(admin_2_rgdp_total))  %>% 
    ungroup()  %>% 
    mutate(admin_2_name = ifelse(admin_2_name == "Distrito Federal", "Distrito Federal (BR)", admin_2_name))  %>% 
      mutate(admin_2_id = case_when(
    admin_2_name == "Acre" ~ "BR01",
    admin_2_name == "Alagoas" ~ "BR08",
    admin_2_name == "Amapá" ~ "BR02",
    admin_2_name == "Amazonas" ~ "BR03",
    admin_2_name == "Bahia" ~ "BR09",
    admin_2_name == "Ceará" ~ "BR10",
    admin_2_name == "Distrito Federal (BR)" ~ "BR24",
    admin_2_name == "Espírito Santo" ~ "BR17",
    admin_2_name == "Goiás" ~ "BR25",
    admin_2_name == "Maranhão" ~ "BR11",
    admin_2_name == "Mato Grosso" ~ "BR26",
    admin_2_name == "Mato Grosso do Sul" ~ "BR27",
    admin_2_name == "Minas Gerais" ~ "BR18",
    admin_2_name == "Paraná" ~ "BR21",
    admin_2_name == "Paraíba" ~ "BR12",
    admin_2_name == "Pará" ~ "BR04",
    admin_2_name == "Pernambuco" ~ "BR13",
    admin_2_name == "Piauí" ~ "BR14",
    admin_2_name == "Rio Grande do Norte" ~ "BR15",
    admin_2_name == "Rio Grande do Sul" ~ "BR22",
    admin_2_name == "Rio de Janeiro" ~ "BR19",
    admin_2_name == "Rondônia" ~ "BR05",
    admin_2_name == "Roraima" ~ "BR06",
    admin_2_name == "Santa Catarina" ~ "BR23",
    admin_2_name == "Sergipe" ~ "BR16",
    admin_2_name == "São Paulo" ~ "BR20",
    admin_2_name == "Tocantins" ~ "BR07",
    TRUE ~ NA))

write.csv(final_df_2022, "version2_year2012_2022/step2_obtain_gdp_data/temp/BRA_2022.csv", row.names = F)

# ------------------------------------------------- #
# Create shapefiles -----

# Geometries are obtained together with OECD countries in file "3_oecd.R"
