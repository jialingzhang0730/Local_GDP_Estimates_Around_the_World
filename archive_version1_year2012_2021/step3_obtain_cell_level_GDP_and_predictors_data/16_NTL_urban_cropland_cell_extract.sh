#!/bin/bash

# Loop through values 1 to 10
for i in {1..10}
do
    # Create a PBS script for each job
    cat <<EOF > step3_obtain_cell_level_GDP_and_predictors_data/temp_job_$i.sh
#!/bin/bash
#PBS -N ModelTraining_attempt_$i
#PBS -l nodes=1:ppn=2,mem=100gb
#PBS -j oe

# Navigate to the working directory
cd \$PBS_O_WORKDIR

# Load the specific R module (version 4.2.1)
module load R/4.2.1

# Execute the R script, passing the loop variable i as an argument
R CMD BATCH "--args $i" step3_obtain_cell_level_GDP_and_predictors_data/16_NTL_urban_cropland_cell_extract.R step3_obtain_cell_level_GDP_and_predictors_data/output_$i.Rout
EOF

    # Submit the job script
    qsub step3_obtain_cell_level_GDP_and_predictors_data/temp_job_$i.sh
done
