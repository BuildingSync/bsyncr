# BuildingSync®, Copyright (c) Alliance for Sustainable Energy, LLC, and other contributors.
# See also https://github.com/BuildingSync/bsyncr/blob/main/LICENSE.txt

# Load required libraries
library(testthat)
library(xml2)
library(dplyr)
library(crayon)
library(bsyncr)

# Ensure NOAA token is set
NOAA_TOKEN <- Sys.getenv("NOAA_TOKEN")
if (NOAA_TOKEN == "") {
  stop("Missing NOAA token env var: NOAA_TOKEN")
}
options(noaakey = NOAA_TOKEN)

# Define a helper function for testing -- this should probably be a src file.
test_create_dataframe <- function(bsync_filepath) {
  baseline_scenario_id <- "Scenario-bsyncr"
  bsync_doc <- xml2::read_xml(bsync_filepath) %>%
    bsyncr::bs_stub_scenarios(linked_building_id = "My-Fav-Building", baseline_id = baseline_scenario_id)

  baseline_xpath <- sprintf("//auc:Scenario[@ID = '%s']", baseline_scenario_id)
  sc_baseline <- xml2::xml_find_first(bsync_doc, baseline_xpath)
  not_used <- sc_baseline %>% bsyncr::bs_stub_derived_model(
    dm_id = "DerivedModel-bsyncr",
    dm_period = "Baseline"
  )

  b_df <- bsyncr::bs_parse_nmecr_df(bsync_doc, insert_weather_data = TRUE)
  return(b_df)
}

# Test cases
test_that("Dataframe is created successfully", {
  # Path to the test file
  bsync_filepath <- "./data/ex_bsync.xml"

  # Run the function and check the result
  result <- test_create_dataframe(bsync_filepath)
  print(result)

  # Ensure the result is not NULL and has rows
  expect_true(!is.null(result), "Resulting dataframe should not be NULL")
  expect_gt(nrow(result), 0, "Resulting dataframe should have rows")
})
