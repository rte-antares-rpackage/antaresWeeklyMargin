
#' Read Meteologica file
#'
#' @param path path to a directory containing forecast files or a single file.
#' @param country filter files to read only one country.
#' @param date filter files to read by date(s), using a \code{Date} or a character with format \code{\%Y-\%m-\%d}.
#' @param time filter files to read by time, e.g. \code{"00"} or \code{"12"}.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom stringr str_which str_replace str_extract str_detect
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#'
#' library(antaresWeeklyMargin)
#' 
#' # ALL files in Europe folder
#' meteo <- read_meteologica2(
#'   path = "meteologica/Europe"
#' )
#' 
#' # only for a specific date and time
#' meteo <- read_meteologica2(
#'   path = "meteologica/Europe", 
#'   date = "2018-05-24", 
#'   time = "12"
#' )
#'
#' }
read_meteologica2 <- function(path, country = NULL, date = NULL, time = NULL) {

  if (missing(path)) {
    path <- choose_path()
  }
  path_mtlg <- normalizePath(path, mustWork = TRUE)

  if (dir.exists(path)) {
    old.wd <- getwd()
    setwd(path)
    on.exit(setwd(old.wd))
    path <- select_file(
      path = ".",
      pattern = "",
      fileext = "\\.csv$",
      multiple = TRUE,
      recursive = TRUE,
      verbose = FALSE
    )
  }

  patterns <- c("PhotovoltaicPower", "Wind", "PowerDemand", "PV")

  if (!is.null(country)) {
    path <- path[str_which(string = tolower(path), pattern = tolower(country))]
  }
  if (!is.null(date)) {
    date <- as.Date(date)
    date <- format(date, format = "%Y%m%d")
    filter_date <- lapply(
      X = date,
      FUN = str_detect, string = path
    )
    filter_date <- Reduce("|", filter_date)
    path <- path[filter_date]
  }
  if (!is.null(time)) {
    filter_time <- lapply(
      X = paste0("\\d{8}", time, "00"),
      FUN = str_detect, string = path
    )
    filter_time <- Reduce("|", filter_time)
    path <- path[filter_time]
  }
  path <- path[!str_detect(string = tolower(path), pattern = "offshore")]
  path <- path[!str_detect(string = tolower(path), pattern = "onshore")]
  path <- path[!str_detect(string = tolower(path), pattern = "livefeed")]
  list_countries <- c("Austria", "Belgium", "France", "Germany", "Italy",
                      "Netherlands", "Portugal", "Spain", "Switzerland", "UK",
                      "RepublicOfIreland", "NorthernIreland")
  res <- lapply(
    X = path,
    FUN = function(x) {
      message(paste("Reading file:", x))
      dat <- read_meteologica2_file(x)
      type <- str_which(string = x, pattern = patterns)
      type <- patterns[type]
      dat$type <- type
      # country <- str_replace(string = x, pattern = ".*/|\\\\", replacement = "")
      # country <- str_extract(string = country, pattern = sprintf(".+(?=-%s)", type))
      country <- list_countries[stringr::str_detect(string = x, pattern = list_countries)]
      dat$country <- country
      dat$file_name <- stringr::str_extract(string = x, pattern = "(?<=/)[^/]+$")
      dat$file_date <- stringr::str_extract(string = dat$file_name, pattern = "[:digit:]{12}")
      dat$file_date <- as.POSIXct(x = dat$file_date, format = "%Y%m%d%H%M", tz = "Europe/Paris")
      dat
    }
  )
  res <- rbindlist(l = res)
  res <- res[order(datetime, -file_name)]
  res <- add_mtlg_areas(res, path_mtlg = path_mtlg)
  return(res)
}

#' @importFrom stringr str_extract str_subset
#' @importFrom data.table fread setnames :=
#' @importFrom lubridate with_tz
read_meteologica2_file <- function(path) {
  skip_ <- readLines(con = path, n = 10)
  skip_ <- grep(pattern = "From yyyy-mm-dd hh:mm", x = skip_) - 1
  dat <- fread(file = path, fill = TRUE, skip = skip_)
  names_ <- names(dat)
  names_ <- str_subset(string = names_, pattern = "ENS")
  setnames(
    x = dat,
    old = names_,
    new = str_extract(
      string = names_,
      pattern = "ENS\\d{2}"
    )
  )
  setnames(x = dat, old = c("From yyyy-mm-dd hh:mm", "UTC offset from (UTC+/-hhmm)"), new = c("datetime", "tz"))

  # format datetime
  # dat <- dat[, datetime := as.POSIXct(datetime, tz = "Europe/Paris")]
  dat[, tz := gsub(pattern = "UTC", replacement = "", x = tz)]
  dat[, datetime := paste0(datetime, tz)]

  dat[, datetime := as.POSIXct(x = datetime, format = "%Y-%m-%d %H:%M%z", tz = "UTC")]
  dat[, datetime := lubridate::with_tz(time = datetime, tzone = "Europe/Paris")]

  dat <- dat[, .SD, .SDcols = c("datetime", sprintf("ENS%02d", 0:50))]

  dat
}






