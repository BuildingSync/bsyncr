# BuildingSync®, Copyright (c) Alliance for Sustainable Energy, LLC, and other contributors.
# See also https://github.com/BuildingSync/bsyncr/blob/main/LICENSE.txt

library("rnoaa")
library("lubridate")
source("weather_data_fetcher.R") # Ensure the weather_data_fetcher class is sourced

# closure for generate_id so we can store static var "count"
make.f <- function() {
  count <- 0
  f <- function(prefix) {
    count <<- count + 1
    return(sprintf("bsyncr-%s-%d", prefix, count))
  }
  return(f)
}

generate_id <- make.f()

#' Stub out the root node for an auc:BuildingSync document
#'
#' @param raw_schema_location A character string pointing to a raw XSD document to be used in xsi:schemaLocation
#'
#' @return doc An xml_document
#' @export
bs_gen_root_doc <- function(raw_schema_location = "https://raw.githubusercontent.com/BuildingSync/schema/c620c7e58688698901edcb8560cd3e1b4b34d971/BuildingSync.xsd") {
  doc <- xml2::xml_new_root("auc:BuildingSync",
    "xmlns:auc" = "http://buildingsync.net/schemas/bedes-auc/2019",
    "xsi:schemaLocation" = paste("http://buildingsync.net/schemas/bedes-auc/2019", raw_schema_location),
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "version" = "dev"
  )
  return(doc)
}

#' Stub out an auc:Building given a bare bones BuildingSync xml doc.
#'
#' @param doc An xml_document, similar to what is output by gen_root_doc
#' @param bldg_id A character string representing the id of the building wished to be used.  This should not have any spaces, and should start with a letter.  It is also used as the auc:PremisesName
#'
#' @return doc An xml_document, now with the building added
#' @export
bs_stub_bldg <- function(doc, bldg_id = "Building-1") {
  doc %>%
    xml2::xml_root() %>%
    xml2::xml_add_child("auc:Facilities") %>%
    xml2::xml_add_child("auc:Facility", "ID" = generate_id("Facility")) %>%
    xml2::xml_add_child("auc:Sites") %>%
    xml2::xml_add_child("auc:Site", "ID" = generate_id("Site")) %>%
    xml2::xml_add_child("auc:Buildings") %>%
    xml2::xml_add_child("auc:Building", "ID" = bldg_id) %>%
    xml2::xml_add_child("auc:PremisesName", bldg_id) %>%
    xml2::xml_root()

  return(doc)
}

#' Add a Baseline and Reporting scenario to the document.  The document should already have an auc:Building element created.
#' It assumes that the Baseline and Reporting scenario both utilize actual measured building data.
#'
#' @param doc An xml_document, similar to what is output by stub_bldg
#' @param bldg_id A character string representing the id of the building wished to be used.  This should not have any spaces, and should start with a letter.  It is also used as the auc:PremisesName
#'
#' @return doc An xml_document, with two additional scenarios stubbed out.
#' @export
bs_stub_scenarios <- function(doc,
                              baseline_id = "Scenario-Baseline",
                              reporting_id = "Scenario-Reporting",
                              linked_building_id = "Building-1") {
  xml2::xml_find_first(doc, "//auc:Reports") %>%
    xml2::xml_add_child("auc:Report", "ID" = generate_id("Report")) %>%
    xml2::xml_add_child("auc:Scenarios") %>%
    xml2::xml_add_child("auc:Scenario", "ID" = baseline_id) %>%
    xml2::xml_add_child("auc:ScenarioType") %>%
    xml2::xml_add_child("auc:DerivedModel", "ID" = generate_id("DerivedModel"))

  return(doc)
}

#' Add an auc:ScenarioType element to the xml_node provided.  The node should be an auc:Scenario.
#'
#' @param x An xml_node for an auc:Scenario
#' @param sc_type A character string indicating the 'type' of the scenario: "Current Building", "Package of Measures"
#' @param measured A boolean indicating if this represents measured or modeled data.
#'
#' @return x An xml_node for the same auc:Scenario, with addition of the Scenario typing information.
#' @export
bs_add_scenario_type <- function(x,
                                 sc_type = c("Current Building", "Package of Measures"),
                                 measured = TRUE) {
  x2 <- x %>%
    xml2::xml_add_child("auc:ScenarioType")

  if (sc_type == "Current Building") {
    x3 <- x2 %>%
      xml2::xml_add_child("auc:CurrentBuilding")
  } else if (sc_type == "Package of Measures") {
    x3 <- x2 %>%
      xml2::xml_add_child("auc:PackageOfMeasures", "ID" = generate_id("POM"))
  }

  x4 <- x3 %>%
    xml2::xml_add_child("auc:CalculationMethod")

  if (measured) {
    x5 <- x4 %>%
      xml2::xml_add_child("auc:Measured")
  } else {
    x5 <- x4 %>%
      xml2::xml_add_child("auc:Modeled")
  }

  return(x)
}

#' Link the node provided to an auc:Building with the specified id
#'
#' @param x An xml_node for an auc:Scenario
#' @param linked_building_id A character string representing the ID of the auc:Building to which this scenario should be linked
#'
#' @return x An xml_node for the same auc:Scenario, with the addition of the linked building.
#' @export
bs_link_bldg <- function(x, linked_building_id) {
  x %>%
    xml2::xml_add_child("auc:LinkedPremises") %>%
    xml2::xml_add_child("auc:Building") %>%
    xml2::xml_add_child("auc:LinkedBuildingID") %>%
    xml2::xml_set_attr("IDref", linked_building_id)
  return(x)
}

#' Stub out a Derived Model within an auc:Scenario
#'
#' @param x An xml_node for an auc:Scenario
#' @param dm_id The desired ID for the new derived model to be added
#' @param dm_period A character string to define the model period: "Baseline", "Reporting"

#'
#' @return x An xml_node for the same auc:Scenario, with the addition of the derived model
#' @export
bs_stub_derived_model <- function(x,
                                  dm_id,
                                  dm_period = c("Baseline", "Reporting"),
                                  measured_scenario_id = "Scenario-Measured") {
  x2 <- x %>% xml2::xml_find_first("auc:ScenarioType/auc:DerivedModel")

  x2 %>%
    xml2::xml_add_child("auc:DerivedModelName", dm_id) %>%
    xml2::xml_add_sibling("auc:MeasuredScenarioID", "IDref" = measured_scenario_id) %>%
    xml2::xml_add_sibling("auc:Models") %>%
    xml2::xml_add_child("auc:Model", ID = generate_id("Model")) %>%
    xml2::xml_add_child("auc:StartTimestamp") %>%
    xml2::xml_add_sibling("auc:EndTimestamp") %>%
    xml2::xml_add_sibling("auc:DerivedModelInputs") %>%
    xml2::xml_add_sibling("auc:DerivedModelCoefficients") %>%
    xml2::xml_add_sibling("auc:DerivedModelPerformance")

  return(x)
}

#' Generate a data frame usable by nmecr for modeling
#'
#' @param tree XML tree to parse
#' @param insert_weather_data Boolean indicating whether to insert weather data into the XML tree
#'
#' @return x Data frame for use with nmecr's model_with_* functions
#' @export
bs_parse_nmecr_df <- function(tree, insert_weather_data = FALSE) {
  # Extract latitude and longitude from the XML tree
  lat_str <- xml2::xml_text(xml2::xml_find_first(tree, "//auc:Building/auc:Latitude"))
  lng_str <- xml2::xml_text(xml2::xml_find_first(tree, "//auc:Building/auc:Longitude"))
  lat_dbl <- as.double(lat_str)
  lng_dbl <- as.double(lng_str)
  resource_use_id_str <- xml2::xml_attr(xml2::xml_find_first(tree, "//auc:ResourceUses/auc:ResourceUse[auc:EnergyResource = 'Electricity']"), "ID")

  ts_nodes <- xml2::xml_find_all(tree, sprintf("//auc:TimeSeries[auc:ResourceUseID/@IDref = '%s']", resource_use_id_str))

  # Construct the eload dataframe
  n_samples <- length(ts_nodes)
  ts_matrix <- matrix(ncol = 2, nrow = n_samples)
  for (i in 1:n_samples) {
    ts_node <- ts_nodes[i]
    start_timestamp <- xml2::xml_text(xml2::xml_find_first(ts_node, "auc:StartTimestamp"))
    reading <- xml2::xml_text(xml2::xml_find_first(ts_node, "auc:IntervalReading"))
    ts_matrix[i, ] <- c(start_timestamp, reading)
  }
  ts_df <- data.frame(ts_matrix)
  colnames(ts_df) <- c("time", "eload")

  # Fix data types
  ts_df[, "eload"] <- as.double(ts_df[, "eload"])
  ts_df[, "time"] <- as.POSIXct(ts_df[, "time"])

  ts_start <- min(ts_df$time)
  ts_end <- max(ts_df$time)

  # Use the weather_data_fetcher class to get weather data
  fetcher <- weather_data_fetcher$new(
    station_id = NULL,
    ts_start = ts_start,
    ts_end = ts_end
  )
  fetcher$get_nearest_station(lat = lat_dbl, long = lng_dbl)
  weather_data <- fetcher$get_weather_data()

  # Match weather data to time series data
  temp_matrix <- matrix(ncol = 2, nrow = n_samples)
  for (row in 1:n_samples) {
    # Find the weather row with a date closest to our current eload time
    date_diffs <- abs(as.POSIXct(weather_data$date) - ts_df[[row, "time"]])
    closest_row <- weather_data[which.min(date_diffs), ]
    row_date <- ts_df[[row, "time"]]
    row_units <- closest_row$units
    if (row_units == "celsius") {
      row_temp <- (closest_row$value * 9 / 5) + 32
    } else if (row_units == "fahrenheit") {
      row_temp <- closest_row$value
    } else {
      stop(sprintf("Invalid unit type: %s", row_units))
    }
    temp_matrix[row, ] <- c(row_date, row_temp)
  }
  temp_df <- data.frame(temp_matrix)
  colnames(temp_df) <- c("time", "temp")

  # Fix data types
  temp_df[, "temp"] <- as.double(temp_df[, "temp"])
  temp_df[, "time"] <- as.POSIXct.numeric(temp_df[, "time"], origin = lubridate::origin)

  if (insert_weather_data == TRUE) {
    ts_data_elem <- xml2::xml_find_first(tree, '//auc:Scenario[auc:ResourceUses/auc:ResourceUse/auc:EnergyResource/text() = "Electricity"]/auc:TimeSeriesData')
    for (row in 1:n_samples) {
      ts_data_elem %>%
        xml2::xml_add_child("auc:TimeSeries", "ID" = generate_id("TimeSeries")) %>%
        xml2::xml_add_child("auc:TimeSeriesReadingQuantity", "Dry Bulb Temperature") %>%
        xml2::xml_add_sibling("auc:StartTimestamp", strftime(temp_df[[row, "time"]], "%Y-%m-%dT%H:%M:%S")) %>%
        xml2::xml_add_sibling("auc:IntervalFrequency", "Month") %>%
        xml2::xml_add_sibling("auc:IntervalReading", temp_df[[row, "temp"]])
    }
  }

  data_int <- "Monthly"
  return(nmecr::create_dataframe(
    eload_data = ts_df,
    temp_data = temp_df,
    start_date = format(ts_start, format = "%m/%d/%y %H:%M"),
    end_date = format(ts_end, format = "%m/%d/%y %H:%M"),
    convert_to_data_interval = data_int,
    temp_balancepoint = 65
  ))
}

#' Add inputs, parameters, and performance statistics to a auc:DerivedModel
#'
#' @param nmecr_baseline_model An output from one of the nmecr_model_with_** functions.
#' @param x An xml_node for an auc:DerivedModel
#' @param normalization_method A character string representing the normalization method to employ.  See enums in BSync schema (auc:DerivedModelInputs/auc:NormalizationMethod).  This is most likely "Forecast".
#' @param response_variable
#' @param response_variable_units
#' @param response_variable_end_use
#' @param explanatory_variable_name
#' @param explanatory_variable_units
#'
#' @return x An xml_node
#' @export
bs_gen_dm_nmecr <- function(nmecr_baseline_model, x,
                            normalization_method = "Forecast",
                            response_variable = "Electricity",
                            response_variable_units = "kWh",
                            response_variable_end_use = "All end uses",
                            explanatory_variable_name = "Drybulb Temperature",
                            explanatory_variable_units = "Fahrenheit, F") {
  # Extract the necessary nodes to manipulate
  dm_start <- xml2::xml_find_first(x, "//auc:Models/auc:Model/auc:StartTimestamp")
  dm_end <- xml2::xml_find_first(x, "//auc:Models/auc:Model/auc:EndTimestamp")
  dm_inputs <- xml2::xml_find_first(x, "//auc:DerivedModelInputs")
  dm_coeff <- xml2::xml_find_first(x, "//auc:DerivedModelCoefficients")
  dm_perf <- xml2::xml_find_first(x, "//auc:DerivedModelPerformance")

  # Create a named list (like key-value hash) for mapping nmecr concepts to BSync
  nmecr_to_bsync <- list(
    "Monthly" = "Month",
    "Daily" = "Day",
    "Hourly" = "Hour",
    "SLR" = "2 parameter simple linear regression",
    "Three Parameter Heating" = "3 parameter heating change point model",
    "3PH" = "3 parameter heating change point model",
    "Three Parameter Cooling" = "3 parameter cooling change point model",
    "3PC" = "3 parameter cooling change point model",
    "Four Parameter Linear Model" = "4 parameter change point model",
    "4P" = "4 parameter change point model",
    "Five Parameter Linear Model" = "5 parameter change point model",
    "5P" = "5 parameter change point model"
  )

  # Extract desired concepts from nmecr model
  model_int <- nmecr_baseline_model$model_input_options$chosen_modeling_interval
  model_type <- nmecr_baseline_model$model_input_options$regression_type
  model_start_dt <- head(nmecr_baseline_model$training_data$time, n = 1)
  model_end_dt <- tail(nmecr_baseline_model$training_data$time, n = 1)

  # Map above to correct BSync representation
  bsync_interval <- nmecr_to_bsync[[model_int]]
  bsync_model_type <- nmecr_to_bsync[[model_type]]
  bsync_start_dt <- format(anytime::anytime(model_start_dt), "%Y-%m-%dT%H:%M:%S")
  bsync_end_dt <- format(anytime::anytime(model_end_dt), "%Y-%m-%dT%H:%M:%S")

  # add model data
  dm_start %>%
    xml2::xml_set_text(bsync_start_dt)
  dm_end %>%
    xml2::xml_set_text(bsync_end_dt)

  # Add inputs data
  dm_inputs %>%
    xml2::xml_add_child("auc:IntervalFrequency", bsync_interval) %>%
    xml2::xml_add_sibling("auc:ResponseVariable") %>%
    xml2::xml_add_child("auc:ResponseVariableName", response_variable) %>%
    xml2::xml_add_sibling("auc:ResponseVariableUnits", response_variable_units) %>%
    xml2::xml_add_sibling("auc:ResponseVariableEndUse", response_variable_end_use)

  dm_inputs %>%
    xml2::xml_add_child("auc:ExplanatoryVariables") %>%
    xml2::xml_add_child("auc:ExplanatoryVariable") %>%
    xml2::xml_add_child("auc:ExplanatoryVariableName", explanatory_variable_name) %>%
    xml2::xml_add_sibling("auc:ExplanatoryVariableUnits", explanatory_variable_units)

  # Based on model type, map parameters to BSync
  # TODO: add additional supported models
  coeffs <- nmecr_baseline_model$model$coefficients
  bsync_intercept <- NULL
  bsync_beta1 <- NULL
  bsync_beta2 <- NULL
  bsync_beta3 <- NULL
  # TODO: find a better way of catching cases where we failed to fit the model
  tryCatch(
    {
      if (bsync_model_type == "2 parameter simple linear regression") {
        bsync_intercept <- coeffs[["(Intercept)"]]
        bsync_beta1 <- coeffs[["temp"]]
      } else if (bsync_model_type == "3 parameter heating change point model" || bsync_model_type == "3 parameter cooling change point model") {
        bsync_intercept <- coeffs[["(Intercept)"]]
        bsync_beta1 <- coeffs[["U1.independent_variable"]]
        # psi[2] contains the estimated change point
        bsync_beta2 <- nmecr_baseline_model$model$psi[2]

        # for current nmecr implementation, the sign for beta 1 and 2 is flipped for
        # the heating models, which we account for here
        if (grepl("heating", bsync_model_type)) {
          bsync_beta1 <- -1 * bsync_beta1
          bsync_beta2 <- -1 * bsync_beta2
        }
      } else if (bsync_model_type == "4 parameter change point model") {
        # to get the intercept `C` according to ASHRAE Guideline 14-2014, Figure D-1
        # we must predict the eload at the estimated temperature change point
        temp_change_point <- nmecr_baseline_model$model$psi[2]
        predictions <- calculate_model_predictions(
          training_data = nmecr_baseline_model$training_data,
          prediction_data = as.data.frame(list(time = c(2019 - 01 - 01), temp = c(temp_change_point))),
          modeled_object = nmecr_baseline_model
        )
        bsync_intercept <- predictions$predictions[1]
        bsync_beta1 <- coeffs[["independent_variable"]]

        # TODO: verify this is _always_ the wrong sign
        # flip the sign b/c current nmecr implementation has it incorrectly set
        bsync_beta2 <- -1 * coeffs[["U1.independent_variable"]]

        bsync_beta3 <- temp_change_point
      } else {
        stop("Unhandled model type")
      }
    },
    error = function(e) {
      print(e)
      # if we get a subscript out of bounds error, assume it's b/c we failed
      # to fit the model and as a result we would be missing values inside of our result
      if (e$message == "subscript out of bounds") {
        stop("Failed to parse model for BuildingSync. This is most likely because the model failed to fit")
      }
      stop(e$message)
    }
  )

  dm_params <- dm_coeff %>% xml2::xml_add_child("auc:Guideline14Model")
  dm_params %>% xml2::xml_add_child("auc:ModelType", bsync_model_type)

  if (!is.null(bsync_intercept)) {
    dm_params %>%
      xml2::xml_add_child("auc:Intercept", bsync_intercept)
  }
  if (!is.null(bsync_beta1)) {
    dm_params %>%
      xml2::xml_add_child("auc:Beta1", bsync_beta1)
  }
  if (!is.null(bsync_beta2)) {
    dm_params %>%
      xml2::xml_add_child("auc:Beta2", bsync_beta2)
  }
  if (!is.null(bsync_beta3)) {
    dm_params %>%
      xml2::xml_add_child("auc:Beta3", bsync_beta3)
  }

  # Evaluate model performance and map to BSync
  perf <- nmecr::calculate_summary_statistics(nmecr_baseline_model)
  dm_perf %>%
    xml2::xml_add_child("auc:RSquared", format(perf[["R_squared"]], scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:CVRMSE", format(max(perf[["CVRMSE %"]], 0), scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:NDBE", format(round(max(as.numeric(perf[["NDBE %"]]), 0), 2), scientific = FALSE, nsmall = 2)) %>%
    xml2::xml_add_sibling("auc:NMBE", format(round(max(as.numeric(perf[["NMBE %"]]), 0), 2), scientific = FALSE, nsmall = 2))

  return(x)
}
