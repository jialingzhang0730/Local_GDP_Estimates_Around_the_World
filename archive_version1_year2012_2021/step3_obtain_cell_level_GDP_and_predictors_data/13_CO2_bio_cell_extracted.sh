#!/bin/bash
#PBS -N ModelTraining_attempt1
#PBS -l nodes=1:ppn=1,mem=50gb
#PBS -j oe

cd $PBS_O_WORKDIR
module load R/4.2.1
# execute program
R CMD BATCH step3_obtain_cell_level_GDP_and_predictors_data/13_CO2_bio_cell_extracted.R