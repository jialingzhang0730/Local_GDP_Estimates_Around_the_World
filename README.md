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
- This project requires a **high-performance computing environment** to support parallel processing. We utilize **10 cores**, which allows the **0.25-degree model** to complete training in approximately **3 to 4 days**. Using fewer cores will primarily **increase processing time** but **should not affect** the final results.

The workflow integrates **QGIS** into R through the `qgisprocess` package, allowing for seamless execution of geometry-related processes directly within R scripts. This eliminates the need for manual interaction with QGIS. Ensure that **QGIS is installed** on the same machine or server where the R scripts will be executed. If QGIS is not detected, the `qgisprocess` package will not function properly.

## Acknowledgement
We thank Reigner Kane, Sreyas Mahadevan, and Jordan Rosenthal-Kay for excellent research assistance and contributions during the initial stages of this project.
