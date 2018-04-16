
#' Create ROR for Weekly Margins simulation
#'
#' @param data a \code{data.table} obtained from \code{\link{read_forfait_oa}}.
#' @param start If specified, data will be filtered from given date to 7 days after.
#' @param startday Day of week to start simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table copy as.data.table := setnames
#' @importFrom antaresRead simOptions
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'
#' # todo
#'
#' }
create_wm_ror <- function(data, start = NULL, startday = "samedi", opts = antaresRead::simOptions()) {

  inputPath <- opts$inputPath

  start <- as.Date(start)

  #On cherche le jour précedent a notre date de prevision
  endday <- as.numeric(format(start, "%u")) - 1

  #on recupere les dates de debut et
  date_ini_fil <- get_previous(startday, date = start)
  date_fin_fil <- get_previous(endday, date = start)

  # Get data
  fil_eau_ini <- get_hydraulique_fil_de_l_eau_eclusee(from = date_ini_fil, to = date_fin_fil)
  fil_eau_ini[, date := NULL]

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
}
