#' Stub out the root node for an auc:BuildingSync document
#'
#' @param raw_schema_location A character string pointing to a raw XSD document to be used in xsi:schemaLocation
#'
#' @return doc An xml_document
#' @export
bsync_gen_root_doc <- function(raw_schema_location = "https://raw.githubusercontent.com/BuildingSync/schema/83e345b6706dea0154cfec9a402d2637f31c570a/BuildingSync.xsd") {
  doc <- xml2::xml_new_root("auc:BuildingSync",
                      "xmlns:auc" = "http://buildingsync.net/schemas/bedes-auc/2019",
                      "xsi:schemaLocation" = paste("http://buildingsync.net/schemas/bedes-auc/2019", raw_schema_location),
                      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance")
  return(doc)
}

#' Stub out an auc:Building given a bare bones BuildingSync xml doc.
#'
#' @param doc An xml_document, similar to what is output by bsync_gen_root_doc
#' @param bldg_id A character string representing the id of the building wished to be used.  This should not have any spaces, and should start with a letter.  It is also used as the auc:PremisesName
#'
#' @return doc An xml_document, now with the building added
#' @export
bsync_stub_bldg <- function(doc, bldg_id = "Building-1") {
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
#' @param doc An xml_document, similar to what is output by bsync_stub_bldg
#' @param bldg_id A character string representing the id of the building wished to be used.  This should not have any spaces, and should start with a letter.  It is also used as the auc:PremisesName
#'
#' @return doc An xml_document, with two additional scenarios stubbed out.
#' @export
bsync_stub_scenarios <- function(doc,
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
    bsync_add_scenario_type(sc_type="Current Building", measured = TRUE) %>%
    bsync_link_bldg(linked_building_id)

  # Second scenario will be the reporting scenario
  scenarios[[2]] %>%
    xml2::xml_set_attr("ID", reporting_id)

  scenarios[[2]] %>%
    bsync_add_scenario_type(sc_type="Package of Measures", measured = TRUE) %>%
    bsync_link_bldg(linked_building_id)

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
bsync_add_scenario_type <- function(x,
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
bsync_link_bldg <- function(x, linked_building_id) {
  x %>%
    xml2::xml_add_child("auc:LinkedPremises") %>%
    xml2::xml_add_child("auc:Building") %>%
    xml2::xml_add_child("auc:LinkedBuildingID") %>%
    xml2::xml_set_attr("IDref", linked_building_id)
  return(x)
}
