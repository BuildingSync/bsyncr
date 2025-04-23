# TODO: Add license info

# I am not sure the best way to test in R, but this is a start.
# The script will run the analysis and the result should be non zero.

# Ensure that R is installed. A recent version should suffice
# `brew install R`

# Install packages needed (assuming that this is the first time configuring)
install.packages(c('remotes', 'crayon', 'dplyr', 'crayon', 'tidyr', 'crul'), repos='http://cran.us.r-project.org')
# print out the current working directory
print("Current working directory:")
print(getwd())

remotes::install_github('ropensci/rnoaa@v1.3.4', upgrade='never')
remotes::install_github('kW-Labs/nmecr@v1.0.17', upgrade='never')
# populate stations
library(rnoaa)
rnoaa::ghcnd_stations()

# print out my current working directory
remotes::install_local(".", force = TRUE, upgrade = "never")
library(bsyncr)

# Load the packages
library(xml2)
library(dplyr)
library(crayon)

# Read in the NOAA token env variable and set the option for nmecr to use
NOAA_TOKEN <- Sys.getenv('NOAA_TOKEN')
if (NOAA_TOKEN == "") {
  stop("Missing NOAA token env var: NOAA_TOKEN")
}
options(noaakey=NOAA_TOKEN)

test_create_dataframe <- function(bsync_filepath, model_type) {
  baseline_scenario_id <- "Scenario-bsyncr"
  bsync_doc <- xml2::read_xml(bsync_filepath) %>%
    bsyncr::bs_stub_scenarios(linked_building_id = "My-Fav-Building", baseline_id = baseline_scenario_id)

  baseline_xpath <- sprintf("//auc:Scenario[@ID = '%s']", baseline_scenario_id)
  sc_baseline <- xml2::xml_find_first(bsync_doc, baseline_xpath)
  not_used <- sc_baseline %>% bsyncr::bs_stub_derived_model(dm_id = "DerivedModel-bsyncr",
                                                            dm_period = "Baseline")

  b_df <- bsyncr::bs_parse_nmecr_df(bsync_doc, insert_weather_data=TRUE)
}

# Test that the dataframe is created
result <- test_create_dataframe("./tests/data/ex_bsync.xml", "SLR")
print(result)

