
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
create_wm_load_fr <- function(path, start, start_prev_hebdo, type = c("prevu", "premis", "offset"), opts = antaresRead::simOptions()) {
  
  inputPath <- opts$inputPath
  
  type <- match.arg(type)
  start <- as.Date(start)
  previous_sam <- as.Date(start_prev_hebdo)
  next_sam <- previous_sam + 7
  rep_prev <- list.files(path = path, pattern = format(start, format = "%d%m%y"), full.names = TRUE)
  rep_prev <- rep_prev[dir.exists(rep_prev)]
  if (length(rep_prev) < 1) {
    stop("Impossible to find a directory based on 'path' and 'start'", call. = FALSE)
  }
  if (length(rep_prev) > 1) {
    stop("Multiple directories found corresponding to 'path' and 'start'", call. = FALSE)
  }
  prevs <- read_cnes(path = rep_prev)
  prevs <- prevs[format(datetime, "%M") == "00"]
  prevs <- prevs[datetime >= previous_sam & datetime < next_sam]
  
  prevs[is.na(prevs)] <- 0
  
  if(type == "premis") {
    prevs <- prevs[, !c("datetime","prevCN","prevu")]
    matrix_conso <- as.data.table(matrix(data = c(rep(0L, 8760*51)), ncol = 51))
    matrix_conso[1:168, ] <- prevs  
  }
  
  if(type == "offset") {
    offset <- prev_offset(prevs)
    offset <- offset[, .SD, .SDcols = c("prev", paste0("prev", 1:50))]
    matrix_conso <- as.data.table(matrix(data = c(rep(0L, 8760*51)), ncol = 51))
    matrix_conso[1:168, ] <- offset  
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







#' @importFrom lubridate wday
#' @importFrom data.table copy as.data.table :=
#' @importFrom stringr str_sub
#' @importFrom zoo na.spline
prev_offset <- function(dat_prev) {
  
  dat_prev <- copy(dat_prev)
  
  prev_premis <- dat_prev[, !c("prevCN","prevu")]
  prev_premis <- prev_premis[, mean := round(apply(.SD, 1, mean), 0), by="datetime"]
  
  prev_u <- dat_prev[, c("datetime","prevu")]
  
  diff <- prev_u$prevu - prev_premis$mean
  prev_u <- cbind(prev_u, diff)
  offset_prev <- NULL
  
  # offset_cnes <- c(0,0,200,-300,-1000,-200,-800)
  
  tab <- as.data.table(dat_prev$datetime)
  names(tab) <- c("datetime")
  
  tab <- tab[, date := as.Date(datetime, tz = "Europe/Paris" )]
  tab <- tab[, offset_prev := c(rep(0, 168))]
  
  for(i in 1:7){
    jour <- wday(tab$date[i+23*(i-1)], week_start = getOption("lubridate.week.start", 1))
    
    if(jour == 6 || jour == 7){
      offset_aux <- c(rep(0, 24))
      tab$offset_prev[(1+(24*(i-1))):(24*i)] <- offset_aux[1:24]
    } else {
      h_pointe_matin <- as.numeric(stringr::str_sub(pointe_matin[i],1,2))
      h_pointe_soir <- as.numeric(stringr::str_sub(pointe_soir[i],1,2))
      h_creux_nuit <- as.numeric(stringr::str_sub(creux_nuit[i],1,2))
      h_creux_jour <- as.numeric(stringr::str_sub(creux_jour[i],1,2))
      
      offset_aux <- c(rep(NA, 24))
      offset_aux[h_pointe_matin+1] <- diff[i*24-1]
      offset_aux[h_pointe_soir+1] <- diff[i*24-1]
      offset_aux[h_creux_nuit+1] <- 0
      offset_aux[h_creux_jour+1] <- 0
      
      tab$offset_prev[(1+(24*(i-1))):(24*i)] <- offset_aux[1:24]
    }
  }
  
  tab <- tab[, offset_new := c(round(na.spline(tab$offset_prev),0))]
  tab <- tab[, -c("date")]
  
  prev_modif <- copy(prev_premis)
  prev_modif <- prev_modif[, -c("datetime","mean")]
  
  prev_modif_new <- prev_modif + tab$offset_new
  prev_modif_new  <- cbind(datetime = prev_u$datetime,prev_modif_new) 
  prev_modif_new  <- prev_modif_new [, mean := round(apply(.SD, 1, mean),0), by = "datetime"]
  
  conso_modif <- cbind(prev_modif_new, prev_u = prev_u$prevu)
  return(conso_modif)
}















