#' Create Links for de/nl/be for Weekly Margins simulation
#'
#' @param data a \code{data.table} containing NTC datas.
#' @param start Starting day of the simulation.
#' @param default Default value to use if \code{forecasttransfercapacity} is 0.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#' 
#' @importFrom data.table copy as.data.table dcast :=
#' @importFrom utils write.table
#' @importFrom antaresEditObject editLink
#'
#' @examples
#' \dontrun{
#' 
#' opts <- setSimulationPath(path = "test_case/", simulation = "input")
#' 
#' ntc_tp <- read_ntc(path = "inputs/ntc_be_de_nl/")
#' 
#' create_wm_ntc_tp(data = ntc_tp, start = "2018-02-24", opts = opts)
#' 
#' }
create_wm_ntc_tp <- function(data, start, default = 1468, opts = antaresRead::simOptions()) {
  
  inputPath <- opts$inputPath
  
  start <- as.Date(start)
  end <- start + 6
  
  data <- copy(data)
  
  ntc_be_nl <- as.data.table(matrix(data = c(rep(0, 8760*3), rep(0.5, 8760*2)), ncol = 5))
  ntc_de_nl <- as.data.table(matrix(data = c(rep(0, 8760*3), rep(0.5, 8760*2)), ncol = 5))
  
  # NTC BE-NL
  cat("Processing NTC BE-NL \n")
  ntc_aux <- data[date >= start & date <= end 
                 & areaintypecode == "BZN" 
                 & mapcodein %in% c("BE", "NL")
                 & mapcodeout %in% c("BE", "NL"), 
                 c("datetime", "date", "mapcodeout", "mapcodein", "forecasttransfercapacity")]
  ntc_aux <- ntc_aux[order(date)]
  ntc_aux <- dcast(data = ntc_aux, formula = date ~ mapcodeout, value.var = "forecasttransfercapacity")
  ntc_aux <- ntc_aux[rep(seq_len(.N), each = 24), -1]
  
  # NTC DE-NL
  cat("Processing NTC DE-NL \n")
  ntc_daux <- data[date >= start & date <= end
                   & areaintypecode == "BZN" 
                   & mapcodein %in% c("DE_AT_LU", "NL")
                   & mapcodeout %in% c("DE_AT_LU", "NL"),
                   c("datetime", "date", "mapcodeout", "mapcodein", "forecasttransfercapacity")]
  ntc_daux <- ntc_daux[order(date, mapcodeout)]
  ntc_daux[forecasttransfercapacity == 0, forecasttransfercapacity := NA]
  ntc_daux[, forecasttransfercapacity := zoo::na.locf(forecasttransfercapacity, na.rm = FALSE)]
  ntc_daux[is.na(forecasttransfercapacity), forecasttransfercapacity := default]
  ntc_daux <- dcast(data = ntc_daux, formula = date ~ mapcodeout, value.var = "forecasttransfercapacity")
  ntc_daux <- ntc_daux[rep(seq_len(.N), each = 24), -1]
  
  # Ecriture des NTC dans les fichiers
  cat("Writing data... \n")
  # be-nl
  ntc_be_nl[1:168, 1:2] <- ntc_aux
  write.table(
    x = ntc_be_nl, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/links/be/nl.txt")
  )
  opts <- editLink(from = "be", to = "nl", transmission_capacities = "enabled", opts = opts)
  
  # de-nl
  ntc_de_nl[1:168, 1:2] <- ntc_daux
  write.table(
    x = ntc_de_nl, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/links/de/nl.txt")
  )
  opts <- editLink(from = "de", to = "nl", transmission_capacities = "enabled", opts = opts)
  
  # be-de
  ntc_null <- NULL
  write.table(
    x = ntc_null, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/links/be/de.txt")
  )
  opts <- editLink(from = "be", to = "de", transmission_capacities = "enabled", opts = opts)
  
  #  be-lu_be"
  ntc_be_lu <- as.data.table(matrix(data = c(rep(300, 8760*2), rep(0, 8760*1), rep(0.5, 8760*2)), ncol = 5))
  write.table(
    x = ntc_be_lu, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/links/be/lu_be.txt")
  )
  opts <- editLink(from = "be", to = "lu_be", transmission_capacities = "enabled", opts = opts)
  
  # de-lu_de"
  ntc_de_lu <- as.data.table(matrix(data = c(rep(1, 8760*2), rep(0, 8760*1), rep(0.5, 8760*2)), ncol = 5))
  write.table(
    x = ntc_de_lu, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/links/de/lu_de.txt")
  )
  opts <- editLink(from = "de", to = "lu_de", transmission_capacities = "enabled", opts = opts)
  
  cat("Writing links data - Done!\n")
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}

