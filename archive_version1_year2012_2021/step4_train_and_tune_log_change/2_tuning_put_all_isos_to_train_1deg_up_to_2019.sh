#!/bin/bash
#PBS -N ModelTraining_attempt1
#PBS -l nodes=1:ppn=12,mem=100gb
#PBS -j oe

cd $PBS_O_WORKDIR
module load R/4.2.1
# execute program
R CMD BATCH step4_train_and_tune_log_change/2_put_all_isos_to_train_1deg_up_to_2019.R