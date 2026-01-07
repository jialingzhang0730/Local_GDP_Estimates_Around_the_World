#!/bin/bash
#PBS -N ModelTraining_attempt1
#PBS -l nodes=1:ppn=10,mem=100gb
#PBS -j oe

cd $PBS_O_WORKDIR
module load R/4.2.1
# execute program
R CMD BATCH /share/rossihansberglab/Nightlights_GDP/replication_packages_world_GCP/version2_year2012_2022/step7_robust_analysis/model_wo_weights/1_put_all_isos_to_train_1deg_up_to_2019.R