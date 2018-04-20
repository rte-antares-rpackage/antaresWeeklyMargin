
#' Read energy data for an area
#'
#' @param area An 'antares' area.
#' @param timeStep Resolution of the data to import: daily
#'  (default, values are divided by number of days), weekly, monthly (original data).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom antaresRead getAreas
#' @importFrom data.table data.table fread setcolorder first
#' @importFrom zoo na.locf
#'
#' @examples
#' \dontrun{
#' library(antaresWeeklyMargin)
#' library(antaresRead)
#'
#' # set path to your simulation
#' opts <- setSimulationPath(path = "test_case/", simulation = "input")
#'
#' weekly <- readEnergy(area = "fr", timeStep = "weekly", opts = opts)
#' weekly
#'
#' daily <- readEnergy(area = "fr", timeStep = "daily", opts = opts)
#' daily
#'
#' monthly <- readEnergy(area = "fr", timeStep = "monthly", opts = opts)
#' monthly
#' }
readEnergy <- function(area, timeStep = "daily", opts = antaresRead::simOptions()) {
  timeStep <- match.arg(arg = timeStep, choices = c("daily", "weekly", "monthly"))
  if (!area %in% antaresRead::getAreas(opts = opts))
    stop("Not a valid area!")
  path <- file.path(opts$inputPath , "hydro", "prepro", area, "energy.txt")
  vars <- c("expectation", "std_deviation", "min", "max", "ror_share")
  if (!file.exists(path) || file.size(path) == 0) {
    energy <- data.table::data.table(matrix(rep(0, 5*12), ncol = 5, dimnames = list(NULL, vars)))
  } else {
    energy <- data.table::fread(input = path, col.names = vars, colClasses = "numeric")
  }
  full_year <- data.table(
    date = seq.Date(from = as.Date(opts$start), by = "day", length.out = 364)
  )
  energy <- energy[, date := full_year[format(date, "%d") == "01"][order(format(date, "%m")), c(date)]]
  if (timeStep == "monthly") {
    data.table::setcolorder(x = energy, neworder = c("date", vars))
    energy <- energy[order(date)]
    return(energy)
  }
  energy <- merge(x = full_year, y = energy, by = "date", all.x = TRUE)
  energy <- energy[, (vars) := lapply(.SD, function(x) {
    zoo::na.locf(x) / .N
  }), by = format(date, format = "%Y%m"), .SDcols = vars]
  energy <- energy[, timeId := rep(seq_len(52), each = 7)]
  if (timeStep == "weekly") {
    energy <- energy[, c(list(date = first(date)), lapply(.SD, sum)), by = timeId, .SDcols = vars]
  }
  # energy[, n := NULL]
  energy
}
