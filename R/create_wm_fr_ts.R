
#' Create Solar and Wind TS for France
#'
#' @param data a \code{data.table} obtained from \code{\link{read_forfait_oa}}.
#' @param start Beginning of the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#' @importFrom data.table fwrite
#'
#' @examples
#' \dontrun{
#' 
#' opts <- setSimulationPath(path = "test_case/", simulation = "input")
#' 
#' oa <- read_forfait_oa(path = "inputs/OA/")
#' 
#' create_wm_ts_fr(data = oa, start = "2018-01-02")
#' 
#' }
create_wm_ts_fr <- function(data, start, opts = antaresRead::simOptions()) {
  
  inputPath <- opts$inputPath
  start <- as.Date(start)
  end <- start + 7

  data <- copy(data)
  data <- data[date_heure >= as.POSIXct(paste0(start, " 00:00:00")) & date_heure < as.POSIXct(paste0(end, " 00:00:00"))
                  , c("eolien",  "photovoltaique")]
  
  matrix_wind <- as.data.table(matrix(data = rep(0, 8760), ncol = 1))
  matrix_wind[1:168] <- data$eolien
  
  matrix_pv <- as.data.table(matrix(data = rep(0, 8760), ncol = 1))
  matrix_pv[1:168] <- data$photovoltaique
  
  path_wind <- file.path(inputPath, "wind", "series", "wind_fr.txt")
  path_pv <- file.path(inputPath, "solar", "series", "solar_fr.txt")
  
  data.table::fwrite(x = matrix_wind, file = path_wind, sep = "\t", row.names = FALSE, col.names = FALSE)
  data.table::fwrite(x = matrix_pv, file = path_pv, sep = "\t", row.names = FALSE, col.names = FALSE)
  cat("Writing wind & solar timeseries - Done!\n")
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
