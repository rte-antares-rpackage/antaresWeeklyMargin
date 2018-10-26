

#' Create Links for Weekly Margins simulation
#'
#' @param data a \code{data.table} containing NTC datas.
#' @param start Starting day of the simulation, data between previous \code{startday}
#'  and next \code{startday}-1, by default between \code{samedi} and \code{vendredi}.
#' @param startday Day of week to start simulation.
#' @param sort_links Reorder other links to match the desired week.
#' @param force_date Force date in data to match \code{startday}.
#' @param flow_based Logical, is this a flow based study ? If \code{TRUE}, links with Be, Nl and De aren't edited.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table copy as.data.table := setnames
#' @importFrom antaresRead simOptions getLinks getAreas
#' @importFrom antaresEditObject editLink
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
create_wm_ntc <- function(data, start = NULL, startday = "samedi",
                          sort_links = TRUE, force_date = FALSE,
                          flow_based = FALSE,
                          opts = antaresRead::simOptions()) {

  inputPath <- opts$inputPath

  start <- as.Date(start)

  date_ini_ntc <- get_previous(startday, date = start)
  date_fin_ntc <- get_previous(startday, date = start + 7) - 1

  ntc_planet <- copy(data)
  
  if (!force_date) {
    date_debut <- as.POSIXct(x = ntc_planet$DATE[1], format = "%d/%m/%Y")
    
    ntc_planet[, date_heure := seq.POSIXt(from = date_debut, length.out = nrow(ntc_planet), by = "1 hour")]
    ntc_planet[, date := as.Date(date_heure, tz = "Europe/Paris")]
    
    ntc_planet <- ntc_planet[date >= date_ini_ntc & date <= date_fin_ntc]
    
    if (nrow(ntc_planet) != 168) {
      stop("There isn't 168 observations in the data!", call. = FALSE)
    }
  } else {
    setnames(x = ntc_planet, old = "DATE", new = "date_heure")
  }
  

  areas_opts <- getAreas(opts = opts)
  if ("itn" %in% areas_opts) {
    liste_pays <- c("be", "ch", "de", "es", "gb", "itn")
  } else {
    liste_pays <- c("be", "ch", "de", "es", "gb", "it")
  }
  
  if (isTRUE(flow_based)) {
    liste_pays <- setdiff(liste_pays, c("be", "de", "nl"))
  }
  

  matrix_ntc <- as.data.table(matrix(data = c(rep(0, 8760 * 3), rep(0.5, 8760 * 2)), ncol = 5))

  for (i in liste_pays) {

    cat(sprintf("%s - Writing NTC for %s\n", paste0(round(which(liste_pays == i)/length(liste_pays)*100), "%"), i))

    if (i == "itn") {
      nom_pays <- grep(pattern = "IT", x = names(ntc_planet), value = TRUE)
    } else {
      nom_pays <- grep(pattern = toupper(i), x = names(ntc_planet), value = TRUE)
    }
    ntc_pays <- ntc_planet[, .SD, .SDcols = c("date_heure", nom_pays)]

    if (i < "fr") {

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
      opts <- editLink(from = i, to = "fr", transmission_capacities = "enabled", opts = opts)

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
      opts <- editLink(from = "fr", to = i, transmission_capacities = "enabled", opts = opts)

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
    cat(format("\rReordering Links - Done!", width = getOption("width")))
    cat("\n")
  }

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
