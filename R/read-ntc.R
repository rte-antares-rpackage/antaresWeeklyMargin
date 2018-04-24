
#' read NTC files
#'
#' @param path Path to a directory containing CSV files.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom utils read.table
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#'
#' #  todo
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
  setnames(x = res, old = names(res), new = tolower(names(res)))
  res[, datetime := as.POSIXct(datetime)]
  return(res)
}
