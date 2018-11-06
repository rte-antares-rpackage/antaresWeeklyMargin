
#' Create Load TS for France
#'
#' @param path Path to CNES directory, see \code{\link{read_cnes}} for usage.
#' @param start Beginning of the simulation.
#' @param start_prev_hebdo Beginning of previsions.
#' @param type Forecast to use \code{"prevu"}, \code{"premis"} or \code{"offset"}.
#' @param offset_options DEPRECATED. Peak and off-peak if \code{type = "offset"}.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#' 
#' @name create-wm-load-fr
#'
#' @examples
#' \dontrun{
#' 
#' create_wm_load_fr("Prev_hebdo_CNES/", "2018-02-27", "prevu")
#' 
#' }
create_wm_load_fr <- function(path, start, start_prev_hebdo,
                              type = c("prevu", "premis", "offset"), 
                              offset_options = NULL,
                              opts = antaresRead::simOptions()) {
  
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
    prevs <- prevs[, !c("datetime", "prevCN", "prevu")]
    matrix_conso <- as.data.table(matrix(data = c(rep(0L, 8760*51)), ncol = 51))
    matrix_conso[1:168, ] <- prevs  
  }
  
  if(type == "offset") {
    offset <- compute_offset(prevs)
    offset <- offset[, .SD, .SDcols = c("prev", paste0("prev", 1:50))]
    matrix_conso <- as.data.table(matrix(data = c(rep(0, 8760*51)), ncol = 51))
    matrix_conso[1:168, ] <- offset  
  }
  
  if(type == "prevu"){
    prevs <- prevs[, c("prevu")]
    matrix_conso <- as.data.table(matrix(data = c(rep(0L, 8760*51)), ncol = 51))
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




#' @param peak_morning Morning peak.
#' @param peak_evening Evening peak.
#' @param offpeak_night Night off-peak.
#' @param offpeak_day Day off-peak.
#'
#' @rdname create-wm-load-fr
#' @export
offset_opts <- function(peak_morning = c(NA, NA, "13:00", "13:00", "13:00", "13:00", "13:00"),
                       peak_evening = c(NA, NA, "19:00", "19:00", "19:00", "19:00", "23:00"),
                       offpeak_night = c(NA, NA, "04:00", "04:00", "04:00", "04:00", "04:00"),
                       offpeak_day = c(NA, NA, "22:00", "22:00", "22:00", "22:00", "22:00")) {
  list(
    peak_morning = peak_morning,
    peak_evening = peak_evening,
    offpeak_night = offpeak_night,
    offpeak_day = offpeak_day
  )
}



#' @importFrom lubridate wday
#' @importFrom data.table copy as.data.table :=
#' @importFrom stringr str_sub
#' @importFrom zoo na.spline
prev_offset <- function(dat_prev, offset_options = offset_opts()) {
  
  dat_prev <- copy(dat_prev)
  
  peak_morning <- offset_options$peak_morning
  peak_evening <- offset_options$peak_evening
  offpeak_night <- offset_options$offpeak_night
  offpeak_day <- offset_options$offpeak_day
  
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
    jour <- wday(tab$date[i + 23 * (i - 1)], week_start = getOption("lubridate.week.start", 1))
    
    if(jour == 6 || jour == 7){
      offset_aux <- c(rep(0, 24))
      tab$offset_prev[(1 + (24 * (i - 1))):(24 * i)] <- offset_aux[1:24]
    } else {
      h_peak_morning <- as.numeric(stringr::str_sub(peak_morning[i], 1, 2))
      h_peak_evening <- as.numeric(stringr::str_sub(peak_evening[i], 1, 2))
      h_offpeak_night <- as.numeric(stringr::str_sub(offpeak_night[i], 1, 2))
      h_offpeak_day <- as.numeric(stringr::str_sub(offpeak_day[i], 1, 2))
      
      offset_aux <- c(rep(NA, 24))
      offset_aux[h_peak_morning + 1] <- diff[i * 24 - 1]
      offset_aux[h_peak_evening + 1] <- diff[i * 24 - 1]
      offset_aux[h_offpeak_night + 1] <- 0
      offset_aux[h_offpeak_day + 1] <- 0
      
      tab$offset_prev[(1 + (24 * (i - 1))):(24 * i)] <- offset_aux[1:24]
    }
  }
  
  tab <- tab[, offset_new := c(round(na.spline(tab$offset_prev), 0))]
  tab <- tab[, -c("date")]
  
  prev_modif <- copy(prev_premis)
  prev_modif <- prev_modif[, -c("datetime","mean")]
  
  prev_modif_new <- prev_modif + tab$offset_new
  prev_modif_new  <- cbind(datetime = prev_u$datetime,prev_modif_new) 
  prev_modif_new  <- prev_modif_new [, mean := round(apply(.SD, 1, mean),0), by = "datetime"]
  
  conso_modif <- cbind(prev_modif_new, prev_u = prev_u$prevu,
                       offset_prev = tab$offset_new, 
                       offset_prev_raw = tab$offset_prev)
  return(conso_modif)
}






#' @importFrom data.table copy := hour
#' @importFrom lubridate wday
compute_offset <- function(prevs) {
  data_prevs <- copy(prevs)
  
  # moyennes de toutes les prevs
  data_prevs[, mean_prev := round(rowMeans(.SD)), .SDcols = setdiff(names(data_prevs), c("datetime", "prevCN","prevu"))]
  data_prevs[, offset_prev := prevu - mean_prev]
  
  # data_prevs <- data_prevs[, list(datetime, mean_prev, offset_prev)]
  
  # PEAK
  data_prevs[hour(datetime) >= 7 & hour(datetime) <= 14, offset_peak := datetime[which.max(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]
  data_prevs[hour(datetime) >= 18 & hour(datetime) <= 23, offset_peak := datetime[which.max(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]
  
  # OFFPEAK
  data_prevs[hour(datetime) >= 3 & hour(datetime) <= 7, offset_offpeak := datetime[which.min(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]
  data_prevs[hour(datetime) >= 15 & hour(datetime) <= 23, offset_offpeak := datetime[which.min(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]
  
  # indice des pics et creux
  data_prevs[, is_peak := datetime == offset_peak]
  data_prevs[, is_offpeak := datetime == offset_offpeak]
  data_prevs[is.na(is_peak), is_peak := FALSE]
  data_prevs[is.na(is_offpeak), is_offpeak := FALSE]
  # pas de pics/creux le weekend
  data_prevs[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), is_peak := FALSE]
  data_prevs[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), is_offpeak := FALSE]
  
  # ajustement des valuers de l'offset
  data_prevs[!(is_peak), offset_prev := NA_real_]
  data_prevs[(is_offpeak), offset_prev := 0]
  data_prevs[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), offset_prev := 0]
  
  
  # interpolation de l'offset
  data_prevs[, offset_prev_raw := offset_prev]
  data_prevs[, offset_prev := round(zoo::na.spline(offset_prev))]
  
  vars_prev <- grep(pattern = "^prev", x = names(data_prevs), value = TRUE)
  vars_prev <- setdiff(vars_prev, c("prevCN", "prevu"))
  
  # ajout de l'offset
  data_prevs[, (vars_prev) := .SD + offset_prev, .SDcols = vars_prev]
  
  # maj moyenne des prevs
  data_prevs[, mean_prev := round(rowMeans(.SD)), .SDcols = vars_prev]
  
  return(data_prevs)
}






