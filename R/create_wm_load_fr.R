
#' Create Load TS for France
#'
#' @param path Path to CNES directory, see \code{\link{read_cnes}} for usage.
#' @param start Beginning of the simulation.
#' @param start_prev_hebdo Beginning of previsions.
#' @param type Forecast to use \code{prevu} or \code{premis}.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' create_wm_load_fr("Prev_hebdo_CNES/", "2018-02-27", "prevu")
#' 
#' }
create_wm_load_fr <- function(path, start, start_prev_hebdo, type = c("prevu", "premis"), opts = antaresRead::simOptions()) {
  
  inputPath <- opts$inputPath
  
  type <- match.arg(type)
  start <- as.Date(start)
  previous_sam <- as.Date(start_prev_hebdo)
  next_sam <- previous_sam + 7
  rep_prev <- list.files(path = path, pattern = format(start, "%d%m%y"))
  if (length(rep_prev) != 1) {
    stop("Impossible to find a directory based on 'path' and 'start'", call. = FALSE)
  }
  prevs <- read_cnes(file.path(path, rep_prev))
  prevs <- prevs[format(datetime, "%M") == "00"]
  prevs <- prevs[datetime >= previous_sam & datetime < next_sam]
  
  prevs[is.na(prevs)] <- 0
  
  if(type == "premis") {
    prevs <- prevs[, !c("datetime","prevCN","prevu")]
    matrix_conso <- as.data.table(matrix(data = c(rep(0L, 8760*51)), ncol = 51))
    matrix_conso[1:168, ] <- prevs  
  }
  
  if(type == "prevu"){
    prevs <- prevs[, c("prevu")]
    matrix_conso <- as.data.table(matrix(data = c(rep(0L, 8760*1)), ncol = 1))
    matrix_conso[1:168, ] <- prevs 
  }
  
  write.table(
    x = matrix_conso, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/load/series/load_fr.txt")
  )
  cat("Writing fr - load timeseries - Done!\n")
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
