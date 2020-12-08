library('rnoaa')

# closure for generate_id so we can store static var "count"
make.f <- function() {
  count <- 0
  f <- function(prefix) {
    count <<- count + 1
    return(sprintf("%s-%d", prefix, count))
  }
  return( f )
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
                      "version" = "dev")
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
  xml2::xml_find_first(doc, "//auc:Sites") %>%
    xml2::xml_add_sibling("auc:Reports", .where="after") %>%
    xml2::xml_add_child("auc:Report", "ID" = generate_id("Report")) %>%
    xml2::xml_add_child("auc:Scenarios") %>%
    xml2::xml_add_child("auc:Scenario") %>%
    xml2::xml_add_sibling("auc:Scenario") %>%
    xml2::xml_root()

  scenarios <- xml2::xml_find_all(doc, "//auc:Scenario")

  # First scenario considered will be the baseline scenario
  scenarios[[1]] %>%
    xml2::xml_set_attr("ID", baseline_id)

  scenarios[[1]] %>%
    bs_add_scenario_type(sc_type="Current Building", measured = TRUE) %>%
    bs_link_bldg(linked_building_id)

  # Second scenario will be the reporting scenario
  scenarios[[2]] %>%
    xml2::xml_set_attr("ID", reporting_id)

  scenarios[[2]] %>%
    bs_add_scenario_type(sc_type="Package of Measures", measured = TRUE) %>%
    bs_link_bldg(linked_building_id)

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
#' @param sc_type A character string indicating the 'type' of the scenario: "Current Building", "Package of Measures"

#'
#' @return x An xml_node for the same auc:Scenario, with the addition of the derived model
#' @export
bs_stub_derived_model <- function(x,
                               dm_id,
                               dm_period = c("Baseline", "Reporting"),
                               sc_type = c("Current Building", "Package of Measures")) {
  if (sc_type == "Current Building") {
    x2 <- x %>% xml2::xml_find_first("auc:ScenarioType/auc:CurrentBuilding")
  } else if (sc_type == "Package of Measures") {
    x2 <- x %>% xml2::xml_find_first("auc:ScenarioType/auc:PackageOfMeasures")
  }
  x2 %>%
    xml2::xml_add_child("auc:DerivedModel", "ID" = generate_id("DerivedModel")) %>%
    xml2::xml_add_child("auc:DerivedModelName", dm_id) %>%
    xml2::xml_add_sibling("auc:Models") %>%
    xml2::xml_add_child("auc:Model") %>%
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

#'
#' @return x Data frame for use with nmecr's model_with_* functions
#' @export
bs_parse_nmecr_df <- function(tree) {
  lat_str <- xml2::xml_text(xml2::xml_find_first(tree, "//auc:Building/auc:Latitude"))
  lng_str <- xml2::xml_text(xml2::xml_find_first(tree, "//auc:Building/auc:Longitude"))
  lat_dbl <- as.double(lat_str)
  lng_dbl <- as.double(lng_str)
  resource_use_id_str <- xml2::xml_attr(xml2::xml_find_first(tree, "//auc:ResourceUses/auc:ResourceUse[auc:EnergyResource = 'Electricity']"), "ID")

  ts_nodes = xml2::xml_find_all(tree, sprintf("//auc:TimeSeries[auc:ResourceUseID/@IDref = '%s']", resource_use_id_str))

  # construct the eload dataframe
  n_samples = length(ts_nodes)
  ts_matrix <- matrix(ncol=2, nrow=n_samples)
  for(i in 1:n_samples){
    ts_node <- ts_nodes[i]
    start_timestamp <- xml2::xml_text(xml2::xml_find_first(ts_node, "auc:StartTimestamp"))
    reading <- xml2::xml_text(xml2::xml_find_first(ts_node, "auc:IntervalReading"))
    ts_matrix[i,] <- c(start_timestamp, reading)
  }
  ts_df <- data.frame(ts_matrix)
  colnames(ts_df) <- c("time", "eload")

  # fix data types
  ts_df[, "eload"] <- as.double(ts_df[, "eload"])
  ts_df[, "time"] <- as.POSIXct(ts_df[, "time"])

  ts_start = min(ts_df$time)
  ts_end = max(ts_df$time)

  # handle weather
  station_data <- ghcnd_stations() # Takes a while to run
  lat_lon_df <- data.frame(id = c("my_building"),
                           latitude = c(lat_dbl),
                           longitude = c(lng_dbl))

  # get nearest station with average temp data
  nearby_stations <- meteo_nearby_stations(
    lat_lon_df = lat_lon_df,
    station_data = station_data,
    limit=1,
    var=c("TAVG")
  )

  # get MONTHLY temp data from station
  weather_result <- ncdc(
    datasetid='GSOM',
    stationid=sprintf('GHCND:%s', nearby_stations$my_building$id),
    datatypeid='TAVG',
    # messy solution, but ensures that we get data before our start time
    startdate =  strftime(ts_start - (60 * 60 * 24 * 31) , "%Y-%m-%dT%H:%M:%S"),
    enddate = ts_end,
    add_units=TRUE,
  )

  weather_data <- weather_result$data
  temp_matrix <- matrix(ncol=2, nrow=nrow(weather_data))
  for (row in 1:nrow(weather_data)) {
    row_units <- weather_data[row, "units"]
    row_date <- weather_data[[row, "date"]]
    if (row_units == "celsius") {
      row_temp <- (weather_data[[row, "value"]] * 9 / 5) + 32
    } else if (row_units == "farenheit") {
      row_temp <- weather_data[[row, "value"]]
    } else {
      stop(sprintf("Invalid unit type: %s", row_units))
    }
    temp_matrix[row,] <- c(row_date, row_temp)
  }
  temp_df <- data.frame(temp_matrix)
  colnames(temp_df) <- c("time", "temp")

  # fix data types
  temp_df[, "temp"] <- as.double(temp_df[, "temp"])
  temp_df[, "time"] <- as.POSIXct(temp_df[, "time"])

  data_int <- "Monthly"
  return(nmecr::create_dataframe(eload_data = ts_df,
                                  temp_data = temp_df,
                                  start_date = format(ts_start, format="%m/%d/%y %H:%M"),
                                  end_date = format(ts_end, format="%m/%d/%y %H:%M"),
                                  convert_to_data_interval = data_int,
                                  timestamps="start"))
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
    "Three Parameter Cooling" = "3 parameter cooling change point model",
    "Four Parameter Linear Model" = "4 parameter change point model",
    "Five Parameter Linear Model" = "5 parameter change point model"
  )

  # Extract desired concepts from nmecr model
  model_int <- nmecr_baseline_model$model_input_options$chosen_modeling_interval
  model_type <- nmecr_baseline_model$model_input_options$regression_type
  model_start_dt <- head(nmecr_baseline_model$training_data$time, n=1)
  model_end_dt <- tail(nmecr_baseline_model$training_data$time, n=1)

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
  if (model_type == "SLR") {
    bsync_intercept <- coeffs[["(Intercept)"]]
    bsync_beta1 <- coeffs[["temp"]] # TODO: How do these get named more generally?
  } else {
    stop("Unhandled model type")
  }

  dm_coeff %>%
    xml2::xml_add_child("auc:Guideline14Model") %>%
    xml2::xml_add_child("auc:ModelType", bsync_model_type) %>%
    xml2::xml_add_sibling("auc:Intercept", bsync_intercept) %>%
    xml2::xml_add_sibling("auc:Beta1", bsync_beta1)

  # Evaluate model performance and map to BSync
  perf <- nmecr::calculate_summary_statistics(nmecr_baseline_model)
  dm_perf %>%
    xml2::xml_add_child("auc:RSquared", format(perf[["R_squared"]], scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:CVRMSE", format(max(perf[["CVRMSE %"]], 0), scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:NDBE", format(max(perf[["NDBE %"]], 0), scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:NMBE", format(max(perf[["NMBE %"]], 0), scientific = FALSE))

  return(x)
}
