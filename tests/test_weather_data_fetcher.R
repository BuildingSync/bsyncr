# BuildingSync®, Copyright (c) Alliance for Sustainable Energy, LLC, and other contributors.
# See also https://github.com/BuildingSync/bsyncr/blob/main/LICENSE.txt

library(testthat)
library(rnoaa)

source("../R/weather_data_fetcher.R")

# Ensure NOAA token is set
NOAA_TOKEN <- Sys.getenv("NOAA_TOKEN")
if (NOAA_TOKEN == "") {
  stop("Missing NOAA token env var: NOAA_TOKEN")
}
options(noaakey = NOAA_TOKEN)


test_that("get_nearest_station correctly identifies the nearest station", {
  # Mock data for testing - NYC
  mock_lat <- 40.7128
  mock_long <- -74.0060

  # Create an instance of the weather_data_fetcher class
  # ts_start and ts_end are not used in this test
  fetcher <- weather_data_fetcher$new(
    station_id = NULL,
    ts_start = Sys.time() - (60 * 60 * 24 * 30),
    ts_end = Sys.time()
  )

  fetcher$get_nearest_station(lat = mock_lat, long = mock_long)
  cat(paste(" Nearest station ID:", fetcher$station_id, "\n"))
  
  # Check that the station_id is not NULL and is the value for central park NYC
  expect_false(is.null(fetcher$station_id))
  expect_true(fetcher$station_id == "USW00094728")
})

# test valid error when station_id is NULL
test_that("get_weather_data throws error when station_id is NULL", {
  # Create an instance of the weather_data_fetcher class
  fetcher <- weather_data_fetcher$new(
    station_id = NULL,
    ts_start = Sys.time() - (60 * 60 * 24 * 30),
    ts_end = Sys.time()
  )

  # Expect an error when calling get_weather_data with a NULL station_id
  expect_error(fetcher$get_weather_data(), "Station ID is NULL. Please set the station ID first or call get_nearest_station()")
})

# now test getting the weather data for the same station as above
test_that("get_weather_data fetches data correctly", {
  # Mock data for testing - NYC
  mock_lat <- 40.7128
  mock_long <- -74.0060

  # Create an instance of the weather_data_fetcher class
  fetcher <- weather_data_fetcher$new(
    station_id = "USW00094728",
    ts_start = as.POSIXct("2022-06-01"),
    ts_end = as.POSIXct("2023-07-01")
  )

  # Fetch the weather data
  weather_data = fetcher$get_weather_data()
  
  # Check that the data is not NULL and has the expected structure
  expect_false(is.null(fetcher$weather_data))
  expect_true(nrow(fetcher$weather_data) > 0)

  df_weather_data = fetcher$to_df()
  print(df_weather_data)

  # check the structure of the data frame
  expect_true(all(c("date", "temp", "units") %in% names(df_weather_data)))
  expect_true(nrow(df_weather_data) > 0)
  expect_true(ncol(df_weather_data) == 3)
  expect_true(all(df_weather_data$temp > 0))
  expect_true(all(df_weather_data$units == "celsius"))
})
