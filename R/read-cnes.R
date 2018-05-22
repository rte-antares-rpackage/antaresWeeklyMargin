
#' Read forecast from CNES
#'
#' @param path Path to the file(s) to read, if path is a directory,
#'  all files starting with \code{prev} are read.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom data.table fread setnames := melt
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresWeeklyMargin)
#' 
#' # read only one file
#' prev1 <- read_cnes("Prev_hebdo_CNES/prev1")
#' prev1
#' 
#' # read several files
#' prev12 <- read_cnes(c(
#'   "Prev_hebdo_CNES/prev1",
#'   "Prev_hebdo_CNES/prev2"
#' ))
#' prev12
#' 
#' # read the whole directory
#' prev_all <- read_cnes("Prev_hebdo_CNES/")
#' prev_all
#' 
#' }
read_cnes <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  if (length(path) == 1 && dir.exists(path)) {
    path <- select_file(
      path, "prev\\d", verbose = FALSE, 
      fileext = "", multiple = TRUE
    )
  }
  if (length(path) == 1) {
    dat <- fread(file = path, skip = 3, sep = "\n", header = FALSE)
    dat <- str_replace_all(string = dat$V1, pattern = "9{6}", replacement = " NA")
    dat <- paste(dat, collapse = "\n")
    dat <- fread(input = dat, header = FALSE, sep = " ")
    setnames(x = dat, old = "V1", new = "date")
    dat[, key := seq_len(.N), by = date]
    dat <- melt(data = dat, id.vars = c("date", "key"), variable.name = "hour", value.name = "value")
    setorderv(dat, c("date", "key", "hour"))
    dat[, timestamp := sprintf("%02d:%02d", rep(0:23, each = 2), rep(c(0, 30), times = 24)), by = date]
    dat[, datetime := paste(date, timestamp)]
    dat[, datetime := as.POSIXct(datetime, format = "%y%m%d %H:%M")]
    output <- dat[, list(datetime, value)]
    setnames(output, c("datetime", "value"), c("datetime", basename(path)))
    return(output)
  } else {
    path <- path[order(nchar(path), path)]
    output <- lapply(path, read_cnes)
    merge_ <- function(x, y) {
      merge(x = x, y = y, all = TRUE)
    }
    Reduce(merge_, output)
  }
}
