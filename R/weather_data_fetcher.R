# BuildingSync®, Copyright (c) Alliance for Sustainable Energy, LLC, and other contributors.
# See also https://github.com/BuildingSync/bsyncr/blob/main/LICENSE.txt

library(R6)
library(rnoaa)

weather_data_fetcher <- R6Class(
  "weather_data_fetcher",
  public = list(
    station_id = NULL,
    datatype_id = "TAVG",  # Temperature Average
    ts_start = NULL,
    ts_end = NULL,
    station_data = NULL,
    weather_data = NULL,


    initialize = function(station_id, ts_start, ts_end) {
      self$station_id <- station_id
      self$ts_start <- ts_start
      self$ts_end <- ts_end

      # Takes a while to run - but should be cached when initializing
      # the this class for the first time.
      self$station_data <- rnoaa::ghcnd_stations()
    },

    get_nearest_station = function(lat, long) {
      lat_lon_df <- data.frame(
        id = c("my_building"),
        latitude = c(lat),
        longitude = c(long)
      )
      nearby_stations <- rnoaa::meteo_nearby_stations(
        lat_lon_df = lat_lon_df,
        station_data = self$station_data,
        limit = 1,
        var = c("TAVG")
      )
      self$station_id <- nearby_stations$my_building$id
    },

    get_weather_data = function() {
      # check if station_id is NULL
      if (is.null(self$station_id)) {
        stop("Station ID is NULL. Please set the station ID first or call get_nearest_station()")
      }
      
      # get MONTHLY temp data from station
      weather_result <- rnoaa::ncdc(
        datasetid = "GSOM",
        stationid = sprintf("GHCND:%s", self$station_id),
        datatypeid = "TAVG",
        # messy solution, but ensures that we get data before our start time
        startdate = strftime(self$ts_start - (60 * 60 * 24 * 31), "%Y-%m-%dT%H:%M:%S"),
        enddate = self$ts_end,
        add_units = TRUE,
      )

      self$weather_data <- weather_result$data
      return(self$weather_data)
    },

    to_df = function(n_samples = 12) {
      # convert the weather data to a data frame

      # check if weather_data is NULL
      if (is.null(self$weather_data)) {
        stop("Weather data is NULL. Please call get_weather_data() first.")
      }
      # check if weather_data is empty
      if (length(self$weather_data) == 0) {
        stop("Weather data is empty. Please check the station ID and date range.")
      }
      # check if weather_data has the expected structure
      if (!all(c("date", "value", "units") %in% names(self$weather_data))) {
        stop("Weather data does not have the expected structure.")
      }

      cat(paste("Weather data:", self$weather_data, "\n"))
      temp_df <- data.frame(
        date = self$weather_data$date,
        temp = self$weather_data$value,
        units = self$weather_data$units
      )
      
      # fix data types
      # temp_df[, "temp"] <- as.double(temp_df[, "temp"])
      # temp_df[, "time"] <- as.POSIXct.numeric(temp_df[, "time"], origin = lubridate::origin)

      return(temp_df)
    }
  )
)

# Example usage:
# fetcher <- weather_data_fetcher$new(station_id = nearby_stations$my_building$id, ts_start = ts_start, ts_end = ts_end)
# weather_data <- fetcher$get_weather_data()
