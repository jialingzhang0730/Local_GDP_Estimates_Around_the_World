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
   - If using a different system or a system **without `qsub` support**, modify the shell scripts and **"run_all_scripts.R"** accordingly.  
   - **Local machine users** can ignore the shell scripts but may need to adjust the **number of cores** used in R scripts by modifying the `mc.cores` argument inside the `mclapply` function to match their computers’ capabilities.

4. **Set the working directory and run the main script**  
   Modify the **working directory** in **"run_all_scripts.R"** to the appropriate path, then execute the script to automatically:  
   - Install the necessary R packages.
   - Run all the required R scripts in order.  


## Changes in Version 2 

This dataset was updated to Version 2 on 16th December 2025. Version 2 produces cell GDP estimates for the period 2012-2022 using predictors that are updated to the year 2022.

When updating the results to Version 2, we have made the following changes relative to Version 1:

-  National GDP data for all countries were updated as of September 15, 2025. At that time, national GDP data for Greenland were not available for 2022; consequently, GDP predictions for Greenland are unavailable for that year.

- New vintages of population and national GDP data from IMF World Economic Outlook (WEO) are used (downloaded September 15, 2025), which may contain revisions to 2012--2021 GDP and population figures for certain countries compared to Version 1.

- Subnational GDP data from OECD Explorer for Japan and Norway are not available for 2022. However, Japan's 2020 subnational data, which was previously unavailable in Version 1, is now included in Version 2.
    
- For subregional GDP data of certain developing countries (see Appendix), the DOSE dataset has been updated from Version 2 to Version 9.

- Version 2 reports GDP predictions in Constant 2021 USD, replacing the Constant 2017 USD used in Version 1.

- We identified that the tuned parameters in Version 1 were locally optimal rather than globally optimal. By substantially extending the parameter search range during training, we achieved significant performance gains. The out-of-sample $R^2$ for annual change in log GDP improved from 63.4\% to 77.7\% at the 1-degree level, from 66.1\% to 76.0\% at the 0.5-degree level, and from 70.5\% to 81.9\% at the 0.25-degree level. These improvements enable Version 2 to better capture regional nuances in the yearly evolution of GDP.

- There are no changes to the predictors used or their data sources. Please refer to the Appendix for details.

- Fixed a typo in Version 1 shapefiles where the ISO code for Alaska was incorrectly set to ``Ala'' instead of ``USA''.


## Acknowledgement
We thank Reigner Kane, Sreyas Mahadevan, and Jordan Rosenthal-Kay for excellent research assistance and contributions during the initial stages of this project.
