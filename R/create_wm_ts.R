
#' Create load, wind & soloar time series for Weekly Margins simulation
#'
#' @param data a \code{data.table} obtained from \code{\link{read_meteologica2}}.
#' @param start If specified, data will be filtered from given date to 7 days after.
#' @param sort_ts Reorder other time series data to match the desired week.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table chmatch %chin% as.data.table setnames copy fwrite uniqueN
#' @importFrom antaresRead simOptions
#'
#' @examples
#' \dontrun{
#'
#' # set path to your simulation
#' opts <- setSimulationPath(path = "path/to/simulation/", simulation = "input")
#'
#'
#' # Meteologica forecast
#' formet <- read_meteologica2(path = "path/to/PrevisionMeteologica/Europe")
#'
#' # Create time series
#' create_wm_ts(data = formet, start = "2018-04-01", opts = opts)
#'
#' }
create_wm_ts <- function(data, start = NULL, sort_ts = TRUE, opts = antaresRead::simOptions()) {

  inputPath <- opts$inputPath

  if (!is.null(start)) {
    start <- as.Date(start)
    data <- copy(data)
    data <- data[as.Date(datetime, tz = "Europe/Paris") >= start]
    data <- data[as.Date(datetime, tz = "Europe/Paris") < start + 7]
    vars_en <- sprintf("ENS%02d", 0:50)
    data <- data[country == "UK", (vars_en) := lapply(.SD, sum), by = list(datetime, country, type), .SDcols = vars_en]
    data <- data[order(datetime, -file_name)]
    data <- unique(data, by = c("datetime", "country", "type"))
  }

  n_168 <- data[, .N, by = list(country, type)]
  if (!all(n_168$N == 168)) {
    stop("Not all groups have 168 observations !", call. = FALSE)
  }

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
  data[, area := unlist(match_countries, use.names = FALSE)[chmatch(x = country, table = names(match_countries))]]
  data <- data[!is.na(area)]
  sprintf("ENS%02d", 0:50)

  leftover <- as.data.table(matrix(data = rep(0, 51 * (8760 - 168)), ncol = 51))
  setnames(x = leftover, old = names(leftover), new = sprintf("ENS%02d", 0:50))

  empty <- as.data.table(matrix(data = rep(0, 51 * 8760), ncol = 51))
  setnames(x = empty, old = names(empty), new = sprintf("ENS%02d", 0:50))

  total <- uniqueN(data[, list(country, type)])
  i <- 1

  for (area_ in unique(data$area)) {

    # Load
    path_load <- file.path(inputPath, "load", "series", sprintf("load_%s.txt", area_))
    data_load <- data[area %chin% area_ & type %chin% "PowerDemand"]
    if (nrow(data_load) > 0) {
      load <- rbind(
        data_load[, .SD, .SDcols = sprintf("ENS%02d", 0:50)],
        leftover
      )
      cat(sprintf("%s - Writing Load for %s\n", paste0(round(i/total*100), "%"), area_))
      data.table::fwrite(x = load, file = path_load, sep = "\t", row.names = FALSE, col.names = FALSE)

      i <- i + 1
    } else {
      data.table::fwrite(x = empty, file = path_load, sep = "\t", row.names = FALSE, col.names = FALSE)
    }

    # Wind
    path_wind <- file.path(inputPath, "wind", "series", sprintf("wind_%s.txt", area_))
    data_wind <- data[area %chin% area_ & type %chin% "Wind"]
    if (nrow(data_wind) > 0) {
      wind <- rbind(
        data_wind[, .SD, .SDcols = sprintf("ENS%02d", 0:50)],
        leftover
      )
      cat(sprintf("%s - Writing Wind for %s\n", paste0(round(i/total*100), "%"), area_))
      data.table::fwrite(x = wind, file = path_wind, sep = "\t", row.names = FALSE, col.names = FALSE)

      i <- i + 1
    } else {
      data.table::fwrite(x = empty, file = path_wind, sep = "\t", row.names = FALSE, col.names = FALSE)
    }

    # Solar
    path_solar <- file.path(inputPath, "solar", "series", sprintf("solar_%s.txt", area_))
    data_solar <- data[area %chin% area_ & type %chin% "PV"]
    if (nrow(data_solar) > 0) {
      solar <- rbind(
        data_solar[, .SD, .SDcols = sprintf("ENS%02d", 0:50)],
        leftover
      )
      cat(sprintf("%s - Writing Solar for %s\n", paste0(round(i/total*100), "%"), area_))
      data.table::fwrite(x = solar, file = path_solar, sep = "\t", row.names = FALSE, col.names = FALSE)

      i <- i + 1
    } else {
      data.table::fwrite(x = empty, file = path_solar, sep = "\t", row.names = FALSE, col.names = FALSE)
    }
  }

  if (sort_ts) {
    others_ts <- list(
      list(ts = "load", area = "lu_de"),
      list(ts = "solar", area = "lu_de"),
      list(ts = "wind", area = "lu_de"),
      list(ts = "wind", area = "ch"),
      list(ts = "solar", area = "ie"),
      list(ts = "solar", area = "ni")
    )
    for (i in seq_along(others_ts)) {
      other_ts <- others_ts[[i]]
      cat(sprintf("\rReordering %s for %s...", other_ts$ts, other_ts$area))
      reorder_hourly(
        path = file.path(inputPath, other_ts$ts, "series", sprintf("%s_%s.txt", other_ts$ts, other_ts$area)),
        start_wm = start,
        start_sim = opts$start
      )
    }
    cat("\rReordering time series - Done!\n")
  }

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}

