# --------------- Task --------------- #
# This file downloads all necessary packages needed for this project
# use R version 4.3.1 (2022-06-23) -- "Funny-Looking Kid"
# ------------------------------------ #

# List of required packages (add all your package names here)
required_packages <- c(
  "tictoc", "sf", "parallel", "exactextractr", "gdata", "units",
  "tidyverse", "sp", "tidyr", "stars", "terra", "readxl", "qgisprocess",
  "docxtractr", "janitor", "tibble", "OECD", "rsdmx", "giscoR", "regions",
  "rmapshaper", "countrycode", "stringi", "XML", "data.table", "tigris", 
  "jsonlite", "lubridate", "wbstats", "httr", "readr", "ncdf4", "dplyr",
  "ggthemes", "magrittr", "purrr", "gdalUtilities", "tiff", "foreach",
  "iterators", "doParallel", "future", "future.apply", "stringr", "ggplot2",
  "raster", "fs", "vip", "ranger", "tmaptools", "scales", "workflows",
  "plotly", "htmlwidgets", "randomForest", "speedglm", "stargazer",
  "kableExtra", "writexl", "colorRamps", "RColorBrewer", "gridExtra",
  "grid", "np", "estimatr", "splines", "knitr", "patchwork", "cowplot",
  "transformr", "gganimate"
)

# Install all missing packages in one step
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  cat("Installing missing packages:\n", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}

# Additional setup for qgisprocess
if ("qgisprocess" %in% required_packages) {
  cat("Configuring QGIS...\n")
  qgisprocess::qgis_configure()
}

# Load all packages and report their versions
cat("Loading packages and documenting versions:\n")
package_versions <- data.frame(Package = character(), Version = character(), stringsAsFactors = FALSE)

for (pkg in required_packages) {
  library(pkg, character.only = TRUE)  # Load package
  version <- as.character(packageVersion(pkg))  # Get package version
  package_versions <- rbind(package_versions, data.frame(Package = pkg, Version = version))
  cat(pkg, "version:", version, "\n")  # Print version to console
}

# Save package versions to a text file for reproducibility
output_file <- "package_versions.txt"
write.table(package_versions, file = output_file, row.names = FALSE, quote = FALSE, sep = "\t")
cat("Package versions saved to", output_file, "\n")