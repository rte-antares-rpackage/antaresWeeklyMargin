
#' Create ROR for Weekly Margins simulation
#'
#' @param data a \code{data.table} obtained from \code{\link{read_forfait_oa}}.
#' @param start Starting day of the simulation, data between previous \code{startday}
#'  and next \code{startday}-1, by default between \code{samedi} and \code{vendredi}.
#' @param startday Day of week to start simulation.
#' @param sort_areas Reorder other ROR data to match the desired week.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table copy as.data.table := setnames
#' @importFrom antaresRead simOptions getAreas
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'
#' # set path to your simulation
#' opts <- setSimulationPath(path = "path/to/simulation/", simulation = "input")
#'
#' # Read OA files
#' oa <- read_forfait_oa(path = "path/to/hydro_forfait/")
#'
#' # Create ROR series in Antares
#' create_wm_ror(data = oa, start = "2018-01-04")
#'
#' }
create_wm_ror <- function(data, start = NULL, startday = "samedi", sort_areas = TRUE, opts = antaresRead::simOptions()) {

  inputPath <- opts$inputPath

  start <- as.Date(start)

  #On cherche le jour précedent a notre date de prevision
  endday <- as.numeric(format(start, "%u")) - 1

  #on recupere les dates de debut et
  date_ini_fil <- get_previous(startday, date = start)
  date_fin_fil <- get_previous(endday, date = start)

  # Get data
  cat("Retrieving data from API...")
  fil_eau_ini <- get_hydraulique_fil_de_l_eau_eclusee(from = date_ini_fil, to = date_fin_fil)
  fil_eau_ini[, date := NULL]
  cat("\rRetrieving data from API - Done!\n")

  if (nrow(fil_eau_ini) < 168){
    fill_info_aux <- fil_eau_ini[(nrow(fil_eau_ini) - 24 + 1):nrow(fil_eau_ini), ]
    if (nrow(fil_eau_ini) == 120){
      fill_info <- rbind(fill_info_aux, fill_info_aux)
    } else if (nrow(fil_eau_ini) == 144){
      fill_info <- fill_info_aux
    }
    fil_eau_ini <- rbind(fil_eau_ini, fill_info)
    fil_eau_ini[, date_heure := seq.POSIXt(from = fil_eau_ini$date_heure[1], length.out = nrow(fil_eau_ini), by="1 hour")]
  }

  fil_eau_ini[, date_heure := date_heure + 7 * 24 * 60 * 60]

  cat("Writing ROR for fr...")
  forfaits_oa <- copy(data)

  hydro_tiers <- forfaits_oa[date_heure >= as.POSIXct(start), c("date_heure","hydraulique_tiers")]
  fil_eau <- merge(x = fil_eau_ini, y = hydro_tiers, by = "date_heure")
  fil_eau <- fil_eau[, Total_fil_eau := hydraulique_fil_eau_eclusee + hydraulique_tiers]

  matrix_ror <- as.data.table(matrix(data = c(rep(0, 8760 * 1)), ncol = 1))
  matrix_ror[1:168] <- fil_eau$Total_fil_eau

  write.table(
    x = matrix_ror, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/hydro/series/fr/ror.txt")
  )
  cat("\rWriting ROR for fr - Done!\n")

  if (sort_areas) {
    areas <- getAreas(exclude = "fr")
    for (a in areas) {
      cat(sprintf("\rReordering ROR for %s...", a))
      reorder_hourly(
        path = file.path(inputPath, "hydro", "series", a, "ror.txt"),
        start_wm = start,
        start_sim = opts$start
      )
    }
    cat("\rReordering ROR - Done!\n")
  }
}


#' @importFrom data.table fread fwrite
reorder_hourly <- function(path, start_wm, start_sim, n_days = 7) {

  if (!file.exists(path)) {
    warning("Invalid path: ", path)
    return(invisible())
  }

  start_wm <- as.Date(start_wm)
  start_sim <- as.Date(as.character(start_sim))

  # Indice data hourly
  ind_hour_wm <- difftime(time1 = start_wm, time2 = start_sim, units = "hours")
  ind_hour_wm <- as.numeric(ind_hour_wm)
  ind_hour_wm <- seq(from = ind_hour_wm, length.out = n_days*24, by = 1)

  if (file.size(path) > 0) {
    data_ <- data.table::fread(file = path)
    # if (fill_zero) {
    #   data_[setdiff(seq_len(nrow(data_)), ind_hour_wm)] <- 0
    # }
    ind_data <- c(ind_hour_wm, rep(tail(ind_hour_wm, 1), times = nrow(data_) - length(ind_hour_wm)))
    data_ <- data_[ind_data]
    data.table::fwrite(x = data_, file = path, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
}



