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
                      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance")
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
    xml2::xml_add_child("auc:Facility") %>%
    xml2::xml_add_child("auc:Sites") %>%
    xml2::xml_add_child("auc:Site") %>%
    xml2::xml_add_child("auc:Buildings") %>%
    xml2::xml_add_child("auc:Building") %>%
    xml2::xml_add_child("auc:PremisesName", bldg_id) %>%
    xml2::xml_root()

  xml2::xml_find_first(doc, "//auc:Building") %>%
    xml2::xml_set_attr("ID", bldg_id)

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
    xml2::xml_add_child("auc:Report") %>%
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
      xml2::xml_add_child("auc:PackageOfMeasures")
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
    xml2::xml_add_child("auc:DerivedModels") %>%
    xml2::xml_add_child("auc:DerivedModel") %>%
    xml2::xml_add_child("auc:DerivedModelName", dm_id) %>%
    xml2::xml_add_sibling("auc:DerivedModelPeriod", dm_period) %>%
    xml2::xml_add_sibling("auc:DerivedModelInputs") %>%
    xml2::xml_add_sibling("auc:DerivedModelParameters") %>%
    xml2::xml_add_sibling("auc:DerivedModelPerformance") %>%
    xml2::xml_add_sibling("auc:DerivedModelOutputs") %>%
    xml2::xml_parent() %>%
    xml2::xml_set_attr("ID", dm_id)

  return(x)
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
  dm_inputs <- xml2::xml_find_first(x, "//auc:DerivedModelInputs")
  dm_params <- xml2::xml_find_first(x, "//auc:DerivedModelParameters")
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
  model_start_dt <- head(nmecr_baseline_model$training_data$time, n=1)
  model_end_dt <- tail(nmecr_baseline_model$training_data$time, n=1)

  # Map above to correct BSync representation
  bsync_interval <- nmecr_to_bsync[[model_int]]
  bsync_model_type <- nmecr_to_bsync[[model_type]]
  bsync_start_dt <- format(anytime::anytime(model_start_dt), "%Y-%m-%dT%H:%M:%S")
  bsync_end_dt <- format(anytime::anytime(model_end_dt), "%Y-%m-%dT%H:%M:%S")

  # Add inputs data
  dm_inputs %>% xml2::xml_add_child("auc:ModelType", bsync_model_type) %>%
    xml2::xml_add_sibling("auc:NormalizationMethod", normalization_method) %>%
    xml2::xml_add_sibling("auc:BaselinePeriodStartTimestamp", bsync_start_dt) %>%
    xml2::xml_add_sibling("auc:BaselinePeriodEndTimestamp", bsync_end_dt) %>%
    xml2::xml_add_sibling("auc:IntervalFrequency", bsync_interval) %>%
    xml2::xml_add_sibling("auc:ResponseVariable") %>%
    xml2::xml_add_child("auc:ResponseVariableName", response_variable) %>%
    xml2::xml_add_sibling("auc:ResponseVariableUnits", response_variable_units) %>%
    xml2::xml_add_sibling("auc:ResponseVariableEndUse", response_variable_end_use) %>%
    xml2::xml_parent() %>% xml2::xml_parent() %>%
    xml2::xml_add_child("auc:ExplanatoryVariables") %>%
    xml2::xml_add_child("auc:ExplanatoryVariable") %>%
    xml2::xml_add_child("auc:ExplanatoryVariableName", explanatory_variable_name) %>%
    xml2::xml_add_sibling("auc:ExplanatoryVariableUnits", explanatory_variable_units)

  # Based on model type, map parameters to BSync
  coeffs <- nmecr_baseline_model$model$coefficients
  bsync_intercept <- NULL
  bsync_beta1 <- NULL
  bsync_beta2 <- NULL
  bsync_beta3 <- NULL
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
    if (grepl('heating', bsync_model_type)) {
      bsync_beta1 <- -1 * bsync_beta1
      bsync_beta2 <- -1 * bsync_beta2
    }
  } else if (bsync_model_type == "4 parameter change point model") {
    # FIXME: this is not the correct intercept
    bsync_intercept <- coeffs[["(Intercept)"]]
    bsync_beta1 <- coeffs[["independent_variable"]]

    # TODO: verify this is _always_ the wrong sign
    # flip the sign b/c current nmecr implementation has it incorrectly set
    bsync_beta2 <- -1 * coeffs[["U1.independent_variable"]]

    # psi[2] contains the estimated change point
    bsync_beta3 <- nmecr_baseline_model$model$psi[2]
  } else {
    stop('Unhandled BuildingSync model type')
  }

  if (bsync_intercept != NULL) {
    dm_params %>%
      xml2::xml_add_child("auc:Intercept", bsync_intercept)
  }
  if (bsync_beta1 != NULL) {
    dm_params %>%
      xml2::xml_add_child("auc:Beta1", bsync_beta1)
  }
  if (bsync_beta2 != NULL) {
    dm_params %>%
      xml2::xml_add_child("auc:Beta2", bsync_beta2)
  }
  if (bsync_beta3 != NULL) {
    dm_params %>%
      xml2::xml_add_child("auc:Beta3", bsync_beta3)
  }

  # Evaluate model performance and map to BSync
  perf <- nmecr::calculate_summary_statistics(nmecr_baseline_model)
  dm_perf %>%
    xml2::xml_add_child("auc:RSquared", format(perf[["R_squared"]], scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:CVRMSE", format(max(perf[["CVRMSE %"]], 0), scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:NDBE", format(max(perf[["NDBE %"]], 0), scientific = FALSE)) %>%
    xml2::xml_add_sibling("auc:NMBE", format(max(perf[["NMBE %"]], 0), scientific = FALSE))

  return(x)
}
