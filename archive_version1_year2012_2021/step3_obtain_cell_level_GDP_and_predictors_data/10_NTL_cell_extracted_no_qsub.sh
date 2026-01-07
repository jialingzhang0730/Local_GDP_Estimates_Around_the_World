#!/bin/bash

# Loop through values 1 to 11
for i in {1..11}
do
    # Run R script directly, passing loop variable as argument
    R CMD BATCH "--args $i" step3_obtain_cell_level_GDP_and_predictors_data/10_NTL_cell_extracted.R step3_obtain_cell_level_GDP_and_predictors_data/output_$i.Rout &
done