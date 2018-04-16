
#' Read Meteologica file
#'
#' @param path path to a directory containing forecast files or a single file.
#' @param country filter files to read onlyone country
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
#' # TODO
#'
#' }
read_meteologica2 <- function(path, country = NULL) {
  if (missing(path)) {
    path <- choose_path()
  }
  if (dir.exists(path)) {
    old.wd <- getwd()
    setwd(path)
    on.exit(setwd(old.wd))
  }
  patterns <- c("PhotovoltaicPower", "Wind", "PowerDemand", "PV")
  path <- select_file(
    path = ".",
    pattern = "",
    fileext = "\\.csv$",
    multiple = TRUE,
    recursive = TRUE,
    verbose = FALSE
  )
  if (!is.null(country)) {
    path <- path[str_which(string = tolower(path), pattern = tolower(country))]
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
      dat
    }
  )
  res <- rbindlist(l = res)
  res <- res[order(datetime, -file_name)]
  return(res)
}

#' @importFrom stringr str_extract str_subset
#' @importFrom data.table fread setnames :=
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
  setnames(x = dat, old = "From yyyy-mm-dd hh:mm", new = "datetime")
  dat <- dat[, .SD, .SDcols = c("datetime", sprintf("ENS%02d", 0:50))]
  dat <- dat[, datetime := as.POSIXct(datetime, tz = "Europe/Paris")]
  dat
}
