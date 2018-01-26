

#' Read EDF forfait file
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
read_forfait_oa <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "Forfait_(RPT|RPD)_Hebdo_\\d{8}_\\d{4}_RP_EDF_OA", fileext = "\\.csv$", multiple = TRUE)
  res <- lapply(X = path, FUN = read_forfait_oa_)
  res <- data.table::rbindlist(res)
  res[, lapply(.SD, sum, na.rm = TRUE), by = list(date, heure, date_heure)]
}


#' @importFrom data.table fread setnames melt dcast
read_forfait_oa_ <- function(path) {
  dat <- data.table::fread(input = path, sep = ";", fill = TRUE, colClasses = "character")
  dat <- dat[-c(1:3)]
  dat <- dat[, key := grepl(pattern = "^\\d{8}$", x = V1)]
  dat <- dat[, date := V1[cummax(which(key))[cumsum(key)]]]
  data.table::setnames(x = dat, old = paste0("V", 2:25), new = sprintf("%02d:00", 0:23))
  dat <- data.table::melt(data = dat, id = c("date", "V1"), measure = sprintf("%02d:00", 0:23), na.rm = TRUE)
  dat <- dat[value != ""]
  dat <- data.table::dcast(data = dat, formula = date + variable ~ V1, value.var = "value")
  data.table::setnames(x = dat, old = "variable", new = "heure")
  dat <- dat[, date_heure := paste(date, heure)]
  dat <- dat[, date_heure := as.POSIXct(date_heure, format = "%Y%m%d %H:%M")]
  dat <- dat[, date := as.Date(date, format = "%Y%m%d")]
  data.table::setnames(x = dat, old = names(dat), new = clean_names(names(dat)))
  varnum <- setdiff(x = names(dat), y = c("date", "date_heure", "heure"))
  dat <- dat[, (varnum) := lapply(.SD, as.numeric), .SDcols = varnum]
  return(dat)
}



