# BuildingSync®, Copyright (c) Alliance for Sustainable Energy, LLC, and other contributors.
# See also https://github.com/BuildingSync/bsyncr-server/blob/main/LICENSE.txt

# Print the current working directory
cat("Current working directory:\n")
cat(getwd(), "\n\n")

# Install required packages if not already installed
required_packages <- c(
  "remotes", "crayon", "dplyr", "tidyr", "crul", "xml2", "testthat", "anytime", "lubridate", "segmented", "xts", "zoo", "ggplot2", "scales", "XML", "rappdirs", "gridExtra", "isdparser", "geonames", "hoardr", "data.table"
)

cat("Checking and installing required packages...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
}

# Install specific versions of GitHub packages
cat("Installing GitHub packages...\n")
remotes::install_github("ropensci/rnoaa@v1.3.4", upgrade = "never")
remotes::install_github("kW-Labs/nmecr@v1.0.17", upgrade = "never")

# Populate NOAA stations (required for rnoaa)
cat("Populating NOAA stations...\n")
library(rnoaa)
rnoaa::ghcnd_stations()

# Install the local package
cat("Installing the local bsyncr package...\n")
remotes::install_local(".", force = TRUE, upgrade = "never")

cat("Setup complete. You can now run your tests.\n")
