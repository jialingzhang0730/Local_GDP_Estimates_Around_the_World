# ------------------------------------------------------------------------------------------------- #
# Task Summary:

# This file is to separate the dataset into training, validation, and testing for each degree level
# ------------------------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

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
library(readxl)

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# 1 degree

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_1deg.RData")

# consider first the developed group
USA <- predict_data_complete_1deg %>% filter(substr(iso,1,2) %in% c("US")) # because the file name for the "new_predict_data_complete_1deg.RData" is "predict_data_complete_1deg"

developed_group <- c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU",
                     "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",   
                     "HUN", "ITA", "JPN", "KOR", "LTU", "LVA",   
                     "NLD", "NOR", "NZL", "POL", "PRT", "ROU",   
                     "SVK", "SVN", "SWE", "TUR", unique(USA$iso))

set.seed(12345678)
random_countries <- sample(c("AUT", "BEL", "BGR", "CHE", "CZE", "DEU",
                     "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",   
                     "HUN", "ITA", "JPN", "KOR", "LTU", "LVA",   
                     "NLD", "NOR", "NZL", "POL", "PRT", "ROU",   
                     "SVK", "SVN", "SWE", "TUR"), 2)

# Note: Year 2021 was included in the training dataset because, at the time of dataset creation, data for 2021 had not yet been made available. 
# As a result, most tests were conducted with 2019 as the validation year and 2020 as the test year. To maintain consistency with these test 
# setups, year 2021 was added to the training dataset.

training_df_developed <- predict_data_complete_1deg  %>% 
  filter(iso %in% developed_group)  %>%
  filter(year <= 2018 | year == 2021)  %>% 
  filter(!iso %in% random_countries)

# There will be two validation datasets 
validation_df_year_developed <- predict_data_complete_1deg  %>% 
  filter(iso %in% developed_group)  %>% 
  filter(!iso %in% random_countries)  %>% 
  filter(year == 2019) 

validation_df_iso_developed <- predict_data_complete_1deg  %>% 
  filter(iso %in% random_countries[1])

# There will be two testing datasets

testing_df_year_developed <- predict_data_complete_1deg  %>% 
  filter(iso %in% developed_group)  %>% 
  filter(!iso %in% random_countries)  %>% 
  filter(year == 2020) 

testing_df_iso_developed <- predict_data_complete_1deg  %>% 
  filter(iso %in% random_countries[2])

# now consider the developing group

developing_group <- setdiff(unique(predict_data_complete_1deg$iso), developed_group)

set.seed(12345678)
random_country_developing <- sample(developing_group, 2)

training_df_developing <- predict_data_complete_1deg  %>% 
  filter(iso %in% developing_group)  %>%
  filter(!year %in% c(2018,2019))  %>% 
  filter(!iso %in% random_country_developing)

# There will be two validation datasets 
validation_df_year_developing <- predict_data_complete_1deg  %>% 
  filter(iso %in% developing_group)  %>% 
  filter(!iso %in% random_country_developing)  %>% 
  filter(year == 2018) 

validation_df_iso_developing <- predict_data_complete_1deg  %>% 
  filter(iso %in% random_country_developing[1])

# There will be two testing datasets

testing_df_year_developing <- predict_data_complete_1deg  %>% 
  filter(iso %in% developing_group)  %>% 
  filter(!iso %in% random_country_developing)  %>% 
  filter(year == 2019) 

testing_df_iso_developing <- predict_data_complete_1deg  %>% 
  filter(iso %in% random_country_developing[2])

# In our sample, the data are imbalanced, developing countries data are much less than developed countries data. So we need to give higher weights to developing countries.
training_df <- bind_rows(training_df_developed, training_df_developing)
validation_df_year <- bind_rows(validation_df_year_developed, validation_df_year_developing)
validation_df_iso <- bind_rows(validation_df_iso_developed, validation_df_iso_developing)
testing_df_year <- bind_rows(testing_df_year_developed, testing_df_year_developing)
testing_df_iso <- bind_rows(testing_df_iso_developed, testing_df_iso_developing)


write.csv(training_df, file = "step4_train_and_tune_log_change/outputs/new_data_train_1deg.csv", row.names = FALSE)
write.csv(validation_df_year, file = "step4_train_and_tune_log_change/outputs/new_data_valid_year_1deg.csv",row.names = FALSE)
write.csv(validation_df_iso, file = "step4_train_and_tune_log_change/outputs/new_data_valid_iso_1deg.csv",row.names = FALSE)
write.csv(testing_df_year, file = "step4_train_and_tune_log_change/outputs/new_data_test_year_1deg.csv", row.names = FALSE)
write.csv(testing_df_iso, file = "step4_train_and_tune_log_change/outputs/new_data_test_iso_1deg.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# 0.5 degree

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_0_5deg.RData")

# consider first the developed group

# Note: Year 2021 was included in the training dataset because, at the time of dataset creation, data for 2021 had not yet been made available. 
# As a result, most tests were conducted with 2019 as the validation year and 2020 as the test year. To maintain consistency with these test 
# setups, year 2021 was added to the training dataset.
training_df_developed <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% developed_group)  %>%
  filter(year <= 2018 | year == 2021)  %>% 
  filter(!iso %in% random_countries)

# There will be two validation datasets 
validation_df_year_developed <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% developed_group)  %>% 
  filter(!iso %in% random_countries)  %>% 
  filter(year == 2019) 

validation_df_iso_developed <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% random_countries[1])

# There will be two testing datasets

testing_df_year_developed <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% developed_group)  %>% 
  filter(!iso %in% random_countries)  %>% 
  filter(year == 2020) 

testing_df_iso_developed <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% random_countries[2])

# now consider the developing group

training_df_developing <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% developing_group)  %>%
  filter(!year %in% c(2018,2019))  %>% 
  filter(!iso %in% random_country_developing)

# There will be two validation datasets 
validation_df_year_developing <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% developing_group)  %>% 
  filter(!iso %in% random_country_developing)  %>% 
  filter(year == 2018) 

validation_df_iso_developing <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% random_country_developing[1])

# There will be two testing datasets

testing_df_year_developing <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% developing_group)  %>% 
  filter(!iso %in% random_country_developing)  %>% 
  filter(year == 2019) 

testing_df_iso_developing <- predict_data_complete_0_5deg  %>% 
  filter(iso %in% random_country_developing[2])

# In our sample, the data are imbalanced, developing countries data are much less than developed countries data. So we need to give higher weights to developing countries.
training_df <- bind_rows(training_df_developed, training_df_developing) 
validation_df_year <- bind_rows(validation_df_year_developed, validation_df_year_developing)
validation_df_iso <- bind_rows(validation_df_iso_developed, validation_df_iso_developing)
testing_df_year <- bind_rows(testing_df_year_developed, testing_df_year_developing)
testing_df_iso <- bind_rows(testing_df_iso_developed, testing_df_iso_developing)

write.csv(training_df, file = "step4_train_and_tune_log_change/outputs/new_data_train_0_5deg.csv", row.names = FALSE)
write.csv(validation_df_year, file = "step4_train_and_tune_log_change/outputs/new_data_valid_year_0_5deg.csv",row.names = FALSE)
write.csv(validation_df_iso, file = "step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_5deg.csv",row.names = FALSE)
write.csv(testing_df_year, file = "step4_train_and_tune_log_change/outputs/new_data_test_year_0_5deg.csv", row.names = FALSE)
write.csv(testing_df_iso, file = "step4_train_and_tune_log_change/outputs/new_data_test_iso_0_5deg.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# 0.25 degree

load("step3_obtain_cell_level_GDP_and_predictors_data/outputs/new_predict_data_complete_0_25deg.RData")

# consider first the developed group

# Note: Year 2021 was included in the training dataset because, at the time of dataset creation, data for 2021 had not yet been made available. 
# As a result, most tests were conducted with 2019 as the validation year and 2020 as the test year. To maintain consistency with these test 
# setups, year 2021 was added to the training dataset.
training_df_developed <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% developed_group)  %>%
  filter(year <= 2018 | year == 2021)  %>% 
  filter(!iso %in% random_countries)

# There will be two validation datasets 
validation_df_year_developed <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% developed_group)  %>% 
  filter(!iso %in% random_countries)  %>% 
  filter(year == 2019) 

validation_df_iso_developed <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% random_countries[1])

# There will be two testing datasets

testing_df_year_developed <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% developed_group)  %>% 
  filter(!iso %in% random_countries)  %>% 
  filter(year == 2020) 

testing_df_iso_developed <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% random_countries[2])

# now consider the developing group

training_df_developing <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% developing_group)  %>%
  filter(!year %in% c(2018,2019))  %>% 
  filter(!iso %in% random_country_developing)

# There will be two validation datasets 
validation_df_year_developing <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% developing_group)  %>% 
  filter(!iso %in% random_country_developing)  %>% 
  filter(year == 2018) 

validation_df_iso_developing <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% random_country_developing[1])

# There will be two testing datasets

testing_df_year_developing <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% developing_group)  %>% 
  filter(!iso %in% random_country_developing)  %>% 
  filter(year == 2019) 

testing_df_iso_developing <- predict_data_complete_0_25deg  %>% 
  filter(iso %in% random_country_developing[2])

# In our sample, the data are imbalanced, developing countries data are much less than developed countries data. So we need to give higher weights to developing countries.
training_df <- bind_rows(training_df_developed, training_df_developing) 
validation_df_year <- bind_rows(validation_df_year_developed, validation_df_year_developing)
validation_df_iso <- bind_rows(validation_df_iso_developed, validation_df_iso_developing)
testing_df_year <- bind_rows(testing_df_year_developed, testing_df_year_developing)
testing_df_iso <- bind_rows(testing_df_iso_developed, testing_df_iso_developing)

write.csv(training_df, file = "step4_train_and_tune_log_change/outputs/new_data_train_0_25deg.csv", row.names = FALSE)
write.csv(validation_df_year, file = "step4_train_and_tune_log_change/outputs/new_data_valid_year_0_25deg.csv",row.names = FALSE)
write.csv(validation_df_iso, file = "step4_train_and_tune_log_change/outputs/new_data_valid_iso_0_25deg.csv",row.names = FALSE)
write.csv(testing_df_year, file = "step4_train_and_tune_log_change/outputs/new_data_test_year_0_25deg.csv", row.names = FALSE)
write.csv(testing_df_iso, file = "step4_train_and_tune_log_change/outputs/new_data_test_iso_0_25deg.csv", row.names = FALSE)

