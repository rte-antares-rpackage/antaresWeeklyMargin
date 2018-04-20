

#' Create Links for Weekly Margins simulation
#'
#' @param data a \code{data.table} containing NTC datas.
#' @param start Starting day of the simulation, data between previous \code{startday}
#'  and next \code{startday}-1, by default between \code{samedi} and \code{vendredi}.
#' @param startday Day of week to start simulation.
#' @param sort_links Reorder other links to match the desired week.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table copy as.data.table := setnames
#' @importFrom antaresRead simOptions getLinks
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'
#' # set path to your simulation
#' opts <- setSimulationPath(path = "path/to/simulation/", simulation = "input")
#'
#' # NTC data
#' ntc <- fread("path/to/data.csv")
#'
#' # Create links in Antares
#' create_wm_ntc(ntc, start = "2018-01-04")
#'
#' }
create_wm_ntc <- function(data, start = NULL, startday = "samedi", sort_links = TRUE, opts = antaresRead::simOptions()) {

  inputPath <- opts$inputPath

  start <- as.Date(start)

  date_ini_ntc <- get_previous(startday, date = start)
  date_fin_ntc <- get_previous(startday, date = start + 7) - 1

  ntc_planet <- copy(data)

  date_debut <- as.POSIXct(x = ntc_planet$DATE[1], format = "%d/%m/%Y")

  ntc_planet <- ntc_planet[, date_heure := seq.POSIXt(from = date_debut, length.out = nrow(ntc_planet), by = "1 hour")]
  ntc_planet <- ntc_planet[, date := as.Date(date_heure, tz = "Europe/Paris")]

  ntc_planet <- ntc_planet[date >= date_ini_ntc & date <= date_fin_ntc]

  if (nrow(ntc_planet) != 168) {
    stop("There isn't 168 observations in the data!", call. = FALSE)
  }

  liste_pays <- c("be", "ch", "de", "es", "gb", "it")

  matrix_ntc <- as.data.table(matrix(data = c(rep(0, 8760 * 3), rep(0.5, 8760 * 2)), ncol = 5))

  for (i in liste_pays){

    cat(sprintf("%s - Writing NTC for %s\n", paste0(round(which(liste_pays == i)/length(liste_pays)*100), "%"), i))

    nom_pays <- grep(pattern = toupper(i), x = names(ntc_planet), value = TRUE)
    ntc_pays <- ntc_planet[, .SD, .SDcols = c("date_heure", nom_pays)]

    if(i < "fr") {

      ntc_aux <- ntc_pays[, .SD, .SDcols = c(
        grep(pattern = "IMP", x = names(ntc_pays), value = TRUE),
        grep(pattern = "EXP", x = names(ntc_pays), value = TRUE)
      )]
      ntc_antares <- copy(matrix_ntc)
      ntc_antares[1:168, 1:2 := lapply(ntc_aux, as.numeric)]
      write.table(
        x = ntc_antares, row.names = FALSE, col.names = FALSE, sep = "\t",
        file = paste0(inputPath, "/links/", i, "/fr.txt")
      )

    } else {

      ntc_aux <- ntc_pays[, .SD, .SDcols = c(
        grep(pattern = "EXP", x = names(ntc_pays), value = TRUE),
        grep(pattern = "IMP", x = names(ntc_pays), value = TRUE)
      )]
      ntc_antares <- copy(matrix_ntc)
      ntc_antares[1:168, 1:2 := lapply(ntc_aux, as.numeric)]
      write.table(
        x = ntc_antares, row.names = FALSE, col.names = FALSE, sep = "\t",
        file = paste0(inputPath, "/links/fr/", i, ".txt")
      )

    }

  }

  if (sort_links) {
    others_links <- as.character(getLinks(exclude = "fr"))
    others_links <- strsplit(x = others_links, split = " - ")
    for (i in seq_along(others_links)) {
      area1 <- others_links[[i]][1]
      area2 <- others_links[[i]][2]
      cat(format(sprintf("\rReordering %s - %s link...", area1, area2), width = getOption("width")))
      reorder_hourly(
        path = file.path(inputPath, "links", area1, paste0(area2, ".txt")),
        start_wm = start,
        start_sim = opts$start
      )
    }
    cat("\nReordering Links - Done!\n")
  }

}
