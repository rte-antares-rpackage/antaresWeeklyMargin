
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
#' # one file
#' one_file <- read_meteologica(
#'   path = "20180214/PhotovoltaicPower/country-type-code-code_000000_00.csv"
#' )
#'
#' # filter with a country
#' country <- read_meteologica(path = "20180214/PhotovoltaicPower/", country = "France")
#'
#'
#' # all files in subdir PhotovoltaicPower/
#' all_pp <- read_meteologica(path = "20180214/PhotovoltaicPower/")
#' all_pp[, .N, by = list(type, country)]
#'
#'
#' # all files in subdir 20180214/
#' all <- read_meteologica(path = "20180214/")
#' all[, .N, by = list(type, country)]
#'
#' }
read_meteologica <- function(path, country = NULL) {
  if (missing(path)) {
    path <- choose_path()
  }
  patterns <- c("PhotovoltaicPower", "WindPower", "PowerDemand")
  path <- select_file(
    path = path,
    pattern = paste(patterns, collapse = "|"),
    fileext = "\\.csv$",
    multiple = TRUE,
    recursive = TRUE,
    verbose = FALSE
  )
  if (!is.null(country)) {
    path <- path[str_which(string = tolower(path), pattern = tolower(country))]
  }
  res <- lapply(
    X = path,
    FUN = function(x) {
      message(paste("Reading file:", x))
      dat <- read_meteologica_file(x)
      type <- str_which(string = x, pattern = patterns)
      type <- patterns[type]
      dat$type <- type
      country <- str_replace(string = x, pattern = ".*/", replacement = "")
      country <- str_extract(string = country, pattern = sprintf(".+(?=-%s)", type))
      dat$country <- country
      dat
    }
  )
  rbindlist(l = res)
}

#' @importFrom stringr str_extract str_subset
#' @importFrom data.table fread setnames :=
read_meteologica_file <- function(path) {
  dat <- fread(file = path, sep = ",", fill = TRUE)
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
  setnames(x = dat, old = "From (UTC)", new = "datetime")
  dat <- dat[, .SD, .SDcols = c("datetime", sprintf("ENS%02d", 0:50))]
  dat <- dat[, datetime := as.POSIXct(datetime, tz = "Europe/Paris")]
  dat
}
