
#' @importFrom data.table fread setnames := melt
#' @importFrom stringr str_extract str_detect
read_forecast_file <- function(path) {
  dat <- fread(file = path, skip = 4)
  setnames(
    x = dat,
    old = c(
      "From yyyy-mm-dd hh:mm", "UTC offset from (UTC+/-hhmm)", "To yyyy-mm-dd hh:mm",
      "UTC offset to (UTC+/-hhmm)"
    ),
    new = c("date_1", "date_2", "datetime", "date_3")
  )
  dat <- dat[, date_1 := NULL]
  dat <- dat[, date_2 := NULL]
  dat <- dat[, date_3 := NULL]
  if (str_detect(string = dat$datetime[1], pattern = "\\d{4}-\\d{2}-\\d{2}")) {
    format_ <- "%Y-%m-%d %H:%M"
  } else if (str_detect(string = dat$datetime[1], pattern = "\\d{2}/\\d{2}/\\d{4}")) {
    format_ <- "%d/%m/%Y %H:%M"
  } else {
    warning("Failed to guess datetime format, possible NAs in conversion", call. = FALSE)
    format_ <- "%Y-%m-%d %H:%M"
  }
  dat <- dat[, datetime := as.POSIXct(datetime, format = format_, tz = "Europe/Paris")]
  value_name <- str_extract(string = path, pattern = "(?<=France_)[[:alpha:]_]+(?=_Forecast)")
  melt(
    data = dat,
    id.vars = c("datetime"),
    measure.vars = sprintf("ENS%02d", 0:50),
    variable.name = "scenario",
    value.name = value_name
  )
}


#' Read Power Forecasts
#'
#' @param path path to a directory containing forecast files or a single file.
#'
#' @return a\code{data.table}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' fore_pow <- read_forecast_power(path = "inputs/directory/")
#'
#' }
read_forecast_power <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, ".*Power.*_Forecast.*", fileext = "\\.csv$", multiple = TRUE)
  res_forecast <- lapply(
    X = path, FUN = read_forecast_file
  )
  merge_ <- function(x, y) {
    merge(x = x, y = y, all = TRUE)
  }
  Reduce(merge_, res_forecast)
}



