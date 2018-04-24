
#' read NTC files
#'
#' @param path Path to a directory containing CSV files.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom utils read.table
#' @importFrom data.table rbindlist setnames setcolorder := copy
#'
#' @examples
#' \dontrun{
#'
#' ntc <- read_ntc(path = "inputs/ntc_transparency")
#'
#' }
read_ntc <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, pattern = "", multiple = TRUE, fileext = "\\csv$")
  res <- lapply(
    X = path,
    FUN = read.table, sep = "\t", header = TRUE,
    fileEncoding = "UTF-16LE", skipNul = TRUE,
    stringsAsFactors = FALSE
  )
  res <- rbindlist(res, fill = TRUE)
  res$X. <- NULL
  vars <- copy(names(res))
  setnames(x = res, old = vars, new = tolower(vars))
  res[, date := as.Date(substr(datetime, 1, 10))]
  res[, datetime := as.POSIXct(datetime)]
  vars <- tolower(vars)
  vars_order <- append(x = vars, values = "date", after = which(vars == "datetime"))
  setcolorder(x = res, neworder = vars_order)
  return(res)
}
