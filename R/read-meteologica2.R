
#' Read Meteologica file
#'
#' @param path path to a directory containing forecast files or a single file.
#' @param country filter files to read onlyone country
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom stringr str_which str_replace str_extract
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
  patterns <- c("PhotovoltaicPower", "Wind", "PowerDemand", "PV")
  path <- select_file(
    path = path,
    pattern = "",
    fileext = "\\.csv$",
    multiple = TRUE,
    recursive = TRUE,
    verbose = FALSE
  )
  if (!is.null(country)) {
    path <- path[str_which(string = tolower(path), pattern = tolower(country))]
  }
  list_countries <- c("Austria", "Belgium", "France", "Germany", "Ireland", "Italy",
                      "Netherlands", "Portugal", "Spain", "Switzerland", "UK")
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
      dat
    }
  )
  rbindlist(l = res)
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
