#!/bin/bash
#PBS -N ModelTraining_attempt1
#PBS -l nodes=1:ppn=1,mem=50gb
#PBS -j oe

cd $PBS_O_WORKDIR
module load R
# execute program
R CMD BATCH step3_obtain_cell_level_GDP_and_predictors_data/14_CO2_non_org_cell_extracted.R