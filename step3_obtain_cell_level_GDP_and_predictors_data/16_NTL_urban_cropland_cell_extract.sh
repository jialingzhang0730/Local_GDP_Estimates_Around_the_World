#!/bin/bash

# Loop through values 1 to 11
for i in {1..10}
do
    # Create a PBS script for each job
    cat <<EOF > urban_cropland_NTL_temp_job_$i.sh
#!/bin/bash
#PBS -N ModelTraining_attempt_$i
#PBS -l nodes=1:ppn=2,mem=100gb
#PBS -j oe

# Navigate to the working directory
cd step3_obtain_cell_level_GDP_and_predictors_data

# Load the specific R module (version 4.2.1)
module load R/4.2.1

# Execute the R script, passing the loop variable i as an argument
R CMD BATCH "--args $i" step3_obtain_cell_level_GDP_and_predictors_data/16_NTL_urban_cropland_cell_extract.R urban_cropland_NTL_output_$i.Rout
EOF

    # Submit the job script
    qsub urban_cropland_NTL_temp_job_$i.sh
done
