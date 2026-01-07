
# --------------- Task Summary--------------- #
# This file runs all scripts, replicate the whole project. Before starting:
#
# 1. Make sure you have QGIS installed on your system, and it should be accessible by the R package "qgisprocess":
#   QGIS is a free and open-source software available on Windows, macOS, Linux, FreeBSD, OpenBSD, Android, and iOS. 
#   However, we recommend running the codes on macOS, as they have been tested on this platform. 
#   Using other systems might lead to slightly different results due to system-specific variations.
#
#   Download the version 3.34.11 on the website ``https://download.qgis.org/downloads/". 
#       For Mac users, go to ``macos/ltr/qgis\_ltr\_final-3\_34\_11\_20240913\_170535.dmg". 
#
# 2. Make sure you have R version 4.2.1 installed on your system
# ------------------------------------ #

# Clean environment and manage memory
rm(list = ls())
gc()

# Change your directory here:
setwd("/share/rossihansberglab/Nightlights_GDP/replication_packages")

# step0: prepare R packages
source("R_package_needed.R", echo = TRUE)

# step1: prepare main gis data
source("step1_obtain_gis_data/process_geom.R", echo = TRUE)

# step2: obtain gdp data
source("step2_obtain_gdp_data/1_RUS.R", echo = TRUE)
source("step2_obtain_gdp_data/2_BRA.R", echo = TRUE)
source("step2_obtain_gdp_data/3_oecd.R", echo = TRUE)
source("step2_obtain_gdp_data/4_USA.R", echo = TRUE)
source("step2_obtain_gdp_data/5_CHN.R", echo = TRUE)
source("step2_obtain_gdp_data/6_IND.R", echo = TRUE)
source("step2_obtain_gdp_data/7_KGZ.R", echo = TRUE)
source("step2_obtain_gdp_data/8_PHL.R", echo = TRUE)
source("step2_obtain_gdp_data/9_KAZ.R", echo = TRUE)
source("step2_obtain_gdp_data/10_clean_national_data.R", echo = TRUE)
source("step2_obtain_gdp_data/11_certain_developing_isos_from_DOSE.R", echo = TRUE)
source("step2_obtain_gdp_data/12_merge_gdp_data.R", echo = TRUE)
source("step2_obtain_gdp_data/13_build_world_poly.R", echo = TRUE)

# step3: prepare dataset for training and prediction
source("step3_obtain_cell_level_GDP_and_predictors_data/1_regional_GDP_training_sample.R", echo = TRUE)
source("step3_obtain_cell_level_GDP_and_predictors_data/2_obtain_county_population.R", echo = TRUE)
source("step3_obtain_cell_level_GDP_and_predictors_data/3_world_cell.R", echo = TRUE)
source("step3_obtain_cell_level_GDP_and_predictors_data/4_get_cell_true_GDP.R", echo = TRUE)

if (Sys.which("qsub") != "") {
  system("qsub step3_obtain_cell_level_GDP_and_predictors_data/5_land_pop_cell_extracted.sh", intern = FALSE)
} else {
  source("step3_obtain_cell_level_GDP_and_predictors_data/5_land_pop_cell_extracted.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step3_obtain_cell_level_GDP_and_predictors_data/6_landcover_cell_extracted.sh", intern = FALSE)
} else {
  source("step3_obtain_cell_level_GDP_and_predictors_data/6_landcover_cell_extracted.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step3_obtain_cell_level_GDP_and_predictors_data/7_NPP_cell_extracted.sh", intern = FALSE)
} else {
  source("step3_obtain_cell_level_GDP_and_predictors_data/7_NPP_cell_extracted.R", echo = TRUE)
}

source("step3_obtain_cell_level_GDP_and_predictors_data/8_gas_flare_spot.R", echo = TRUE)

if (Sys.which("qsub") != "") {
  system("qsub step3_obtain_cell_level_GDP_and_predictors_data/9_NTL_cell_extracted_temp_files_genr.sh", intern = FALSE)
} else {
  source("step3_obtain_cell_level_GDP_and_predictors_data/9_NTL_cell_extracted_temp_files_genr.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("bash step3_obtain_cell_level_GDP_and_predictors_data/10_NTL_cell_extracted.sh", intern = FALSE)
} else {
  system("bash step3_obtain_cell_level_GDP_and_predictors_data/10_NTL_cell_extracted_no_qsub.sh", intern = FALSE)
}

source("step3_obtain_cell_level_GDP_and_predictors_data/11_NTL_cell_extracted_comb_years_sep.R", echo = TRUE)
source("step3_obtain_cell_level_GDP_and_predictors_data/12_ruggedness_cell_extracted.R", echo = TRUE)

if (Sys.which("qsub") != "") {
  system("qsub step3_obtain_cell_level_GDP_and_predictors_data/13_CO2_bio_cell_extracted.sh", intern = FALSE)
} else {
  source("step3_obtain_cell_level_GDP_and_predictors_data/13_CO2_bio_cell_extracted.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step3_obtain_cell_level_GDP_and_predictors_data/14_CO2_non_org_cell_extracted.sh", intern = FALSE)
} else {
  source("step3_obtain_cell_level_GDP_and_predictors_data/14_CO2_non_org_cell_extracted.R", echo = TRUE)
}

source("step3_obtain_cell_level_GDP_and_predictors_data/15_NTL_urban_cropland_cell_extract_prepare.R", echo = TRUE)

if (Sys.which("qsub") != "") {
  system("bash step3_obtain_cell_level_GDP_and_predictors_data/16_NTL_urban_cropland_cell_extract.sh", intern = FALSE)
} else {
  system("bash step3_obtain_cell_level_GDP_and_predictors_data/16_NTL_urban_cropland_cell_extract_no_qsub.sh", intern = FALSE)
}

source("step3_obtain_cell_level_GDP_and_predictors_data/17_NTL_urban_cropland_cell_extract_comb_years_sep.R", echo = TRUE)
source("step3_obtain_cell_level_GDP_and_predictors_data/18_new_merge_all_predictors_cell.R", echo = TRUE)

# step4: train the models
source("step4_train_and_tune_log_change/1_obtain_train_validation_dataset_new.R", echo = TRUE)

if (Sys.which("qsub") != "") {
  system("qsub step4_train_and_tune_log_change/2_tuning_put_all_isos_to_train_1deg.sh", intern = FALSE)
} else {
  source("step4_train_and_tune_log_change/2_put_all_isos_to_train_1deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step4_train_and_tune_log_change/2_tuning_put_all_isos_to_train_1deg_up_to_2019.sh", intern = FALSE)
} else {
  source("step4_train_and_tune_log_change/2_put_all_isos_to_train_1deg_up_to_2019.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step4_train_and_tune_log_change/2_tuning_put_all_isos_to_train_0_5deg.sh", intern = FALSE)
} else {
  source("step4_train_and_tune_log_change/2_put_all_isos_to_train_0_5deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step4_train_and_tune_log_change/2_tuning_put_all_isos_to_train_0_5deg_up_to_2019.sh", intern = FALSE)
} else {
  source("step4_train_and_tune_log_change/2_put_all_isos_to_train_0_5deg_up_to_2019.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step4_train_and_tune_log_change/2_tuning_put_all_isos_to_train_0_25deg.sh", intern = FALSE)
} else {
  source("step4_train_and_tune_log_change/2_put_all_isos_to_train_0_25deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step4_train_and_tune_log_change/2_tuning_put_all_isos_to_train_0_25deg_up_to_2019.sh", intern = FALSE)
} else {
  source("step4_train_and_tune_log_change/2_put_all_isos_to_train_0_25deg_up_to_2019.R", echo = TRUE)
}

source("step4_train_and_tune_log_change/3_analyze_rf_models_trained_use_all_isos.R", echo = TRUE)

# step5: predictions, obtain the results
source("step5_predict_and_post_adjustments_log_change/1_predict_model_trained_all_years.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/1_predict_model_trained_up_to_2019.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/2_analyze_diff_models.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/2_analyze_diff_models_extension_log_change.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/3_obtain_country_xdeg_intersect_geom.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/4_post_adjustment_filter_out_low_pop_density_1deg.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/4_post_adjustment_filter_out_low_pop_density_1deg_upto2019.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/4_post_adjustment_filter_out_low_pop_density_0_5deg.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/4_post_adjustment_filter_out_low_pop_density_0_5deg_upto2019.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/4_post_adjustment_filter_out_low_pop_density_0_25deg.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/4_post_adjustment_filter_out_low_pop_density_0_25deg_upto2019.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/4_post_adjustment_how_many_cells_affected.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/5_example_plot.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/6_check_not_just_pop_distr.R", echo = TRUE)
source("step5_predict_and_post_adjustments_log_change/7_trans_to_other_GDP_units.R", echo = TRUE) # this file generates the final datasets

# step6: test model performance under shocks
source("step6_shocks_log_change/1_true_cell_GDP_CHN.R", echo = TRUE)
source("step6_shocks_log_change/2_check_oos_CHN_pred.R", echo = TRUE)
source("step6_shocks_log_change/3_check_training_isos_predictions_log_chan.R", echo = TRUE)

# step7.1: robustness check, tune the hyperparameters by minimizing Mean Square Error (MSE)
if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_tune_MSE/1_tuning_put_all_isos_to_train_1deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_tune_MSE/1_put_all_isos_to_train_1deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_tune_MSE/1_tuning_put_all_isos_to_train_1deg_up_to_2019.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_tune_MSE/1_put_all_isos_to_train_1deg_up_to_2019.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_tune_MSE/1_tuning_put_all_isos_to_train_0_5deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_tune_MSE/1_put_all_isos_to_train_0_5deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_tune_MSE/1_tuning_put_all_isos_to_train_0_25deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_tune_MSE/1_put_all_isos_to_train_0_25deg.R", echo = TRUE)
}

source("step7_robust_analysis/model_tune_MSE/2_obtain_predictions.R", echo = TRUE)
source("step7_robust_analysis/model_tune_MSE/3_compare_with_benchmark_model.R", echo = TRUE)

# step7.2: robustness check, train the model without developing countries
if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_developing/1_tuning_put_all_isos_to_train_1deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_developing/1_put_all_isos_to_train_1deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_developing/1_tuning_put_all_isos_to_train_1deg_up_to_2019.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_developing/1_put_all_isos_to_train_1deg_up_to_2019.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_developing/1_tuning_put_all_isos_to_train_0_5deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_developing/1_put_all_isos_to_train_0_5deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_developing/1_tuning_put_all_isos_to_train_0_25deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_developing/1_put_all_isos_to_train_0_25deg.R", echo = TRUE)
}

source("step7_robust_analysis/model_wo_developing/2_obtain_predictions.R", echo = TRUE)
source("step7_robust_analysis/model_wo_developing/3_compare_with_benchmark_model.R", echo = TRUE)

# step7.3: robustness check, train the model without assigning weights to balance data from developed countries and developing countries
if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_weights/1_tuning_put_all_isos_to_train_1deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_weights/1_put_all_isos_to_train_1deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_weights/1_tuning_put_all_isos_to_train_1deg_up_to_2019.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_weights/1_put_all_isos_to_train_1deg_up_to_2019.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_weights/1_tuning_put_all_isos_to_train_0_5deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_weights/1_put_all_isos_to_train_0_5deg.R", echo = TRUE)
}

if (Sys.which("qsub") != "") {
  system("qsub step7_robust_analysis/model_wo_weights/1_tuning_put_all_isos_to_train_0_25deg.sh", intern = FALSE)
} else {
  source("step7_robust_analysis/model_wo_weights/1_put_all_isos_to_train_0_25deg.R", echo = TRUE)
}

source("step7_robust_analysis/model_wo_weights/2_obtain_predictions.R", echo = TRUE)
source("step7_robust_analysis/model_wo_weights/3_compare_with_benchmark_model.R", echo = TRUE)
