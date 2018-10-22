


#' @importFrom readxl read_excel
#' @importFrom data.table setDT :=
read_area_weight <- function(path) {
  newWeights <- readxl::read_excel(path = path)
  setDT(newWeights)
  newWeights[type == "solar", type := "PV"]
  newWeights[type == "load", type := "PowerDemand"]
  newWeights[type == "wind", type := "Wind"]
  return(newWeights)
}


#' @importFrom data.table data.table rbindlist
add_mtlg_areas <- function(dat_mtlg, path_mtlg = NULL) {
  
  match_countries <- list(
    "Austria" = "at",
    "Belgium" = "be",
    "France" = "fr",
    "Germany" = "de",
    # "Ireland" = "ie",
    "RepublicOfIreland" = "ie",
    "NorthernIreland" = "ni",
    "Italy" = "it",
    "Netherlands" = "nl",
    "Portugal" = "pt",
    "Spain" = "es",
    "Switzerland" = "ch",
    "UK" = "gb"
  )
  
  # Default parameters for Antares areas
  correspAreas <- data.table(
    country = names(match_countries),
    area = unlist(match_countries, use.names = FALSE),
    id = 0
  )
  defaultsSeries <- data.table(type = c("PowerDemand", "Wind", "PV"), weight = 1, id = 0)
  correspAreas <- correspAreas[defaultsSeries, allow.cartesian = TRUE, on = "id"]
  correspAreas[, id := NULL]
  
  if (!is.null(path_mtlg) && file.exists(file.path(path_mtlg, "antaresAreasWeights.xlsx"))) {
    newWeights <- read_area_weight(path = file.path(path_mtlg, "antaresAreasWeights.xlsx"))
    correspAreas <- correspAreas[!newWeights, on = c("country")]
    correspAreas <- rbindlist(list(correspAreas, newWeights))
  }
  
  dat_mtlg <- dat_mtlg[correspAreas, allow.cartesian = TRUE, on = c("country", "type")]
  
  dat_mtlg[is.na(weight), weight := 1]
  
  vars_ens <- sprintf("ENS%02d", 0:50)
  
  dat_mtlg[weight < 1, (vars_ens) := lapply(.SD, function(x) {
    as.integer(round(x * weight))
  }), .SDcols = vars_ens]
  
  return(dat_mtlg)
}



