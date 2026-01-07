#!/bin/bash

# Loop through values 1 to 10
for i in {1..10}
do
    # Run R script directly, passing loop variable as argument
    R CMD BATCH "--args $i" step3_obtain_cell_level_GDP_and_predictors_data/16_NTL_urban_cropland_cell_extract.R step3_obtain_cell_level_GDP_and_predictors_data/output_$i.Rout &
done