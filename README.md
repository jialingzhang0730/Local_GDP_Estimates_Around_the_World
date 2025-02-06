This repository contains the code for the paper "[Local GDP Estimates Around the World](https://rossihansberg.economics.uchicago.edu/LGEAW.pdf)" by Esteban Rossi-Hansberg and Jialing Zhang. This is a complete replication package that produces all results.

## Results
If you prefer to access the results without running any code, you can download them directly from our website: [BFI Data Studio - Local Economies, Global Perspective](https://bfidatastudio.org/project/local-economies-global-perspective-illuminating-subnational-gdp-worldwide/). The site provides access to all shapefiles, results, and related documents.

## Code organization
The workflow is structured as a sequence of **main tasks**, each represented by a dedicated folder prefixed with `"step"`. Each main task folder contains:

- **R scripts** that execute specific processes.
- An **`inputs`** folder, which stores required datasets and resources.
- An **`outputs`** folder, where generated results are saved.

Within each main task folder, there are multiple **sub-tasks**, systematically organized by numbered filenames to indicate their execution order. **Scripts with the same number correspond to parallel processes** within that step. Additionally, the `inputs` and `outputs` folders may include **subdirectories for classification**, allowing for better organization of related files.

## Software and Computational requirements
This project is implemented using:
- **R version 4.2.1 (2022-06-23) – "Funny-Looking Kid"**
- **QGIS version 3.34.11**
- **Minimum memory requirement: 640GB** to fully replicate the results.
- **Processing environment:** The analysis uses at most **10 cores** for parallel execution. Using fewer cores will primarily increase processing time but will **not affect** the final results. The full replication process took approximately **20 days** in our setup, but actual runtime depends on the available **cores and memory**.

The workflow integrates **QGIS** into R through the `qgisprocess` package, allowing for seamless execution of geometry-related processes directly within R scripts. This eliminates the need for manual interaction with QGIS. Ensure that **QGIS is installed** on the same machine or cluster where the R scripts will be executed. If QGIS is not detected, the `qgisprocess` package will not function properly.

## Instructions
1. **Download the replication package**  
   Clone or download the entire replication package to your cluster or local machine.

2. **Download and place the input data**  
   Download the input data as described in our [Appendix](https://rossihansberg.economics.uchicago.edu/LGEAWApp.pdf) or [Replication Guide](https://bfidatastudio.org/wp-content/uploads/2025/01/Replication-Guide-2.pdf), and save it in the corresponding folder.  
   - The GDP data in **"step2_obtain_gdp_data"** is already provided.  
   - However, due to storage constraints, **high-resolution data and geometry files are not included** and must be downloaded separately.

3. **Adjust shell scripts if necessary**  
   - Many R scripts are originally executed by submitting jobs via **`qsub`** to the **HJB server**.  
   - If using a different system or a system **without `qsub` support**, modify the shell scripts accordingly.  
   - **Local machine users** can ignore the shell scripts but may need to adjust the **number of cores** used in R scripts by modifying the `mc.cores` argument inside the `mclapply` function to match their computers’ capabilities.

4. **Set the working directory and run the main script**  
   Modify the **working directory** in **"run_all_scripts.R"** to the appropriate path, then execute the script to automatically:  
   - Install the necessary R packages.
   - Run all the required R scripts in order.  

## Acknowledgement
We thank Reigner Kane, Sreyas Mahadevan, and Jordan Rosenthal-Kay for excellent research assistance and contributions during the initial stages of this project.
