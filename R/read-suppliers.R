

#' @importFrom data.table as.data.table := setorderv
#' @importFrom readxl read_excel
read_edf_sheet <- function(path, sheet) {
  data <- readxl::read_excel(path = path, sheet = sheet, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[, .SD, .SDcols = c("groupe", "code_groupe", "debut", "fin", "code_essai", "pmax", "pmin")]
  data <- data[, groupe := locf(groupe)]
  data <- data[, code_groupe := locf(code_groupe)]
  data <- data[!is.na(debut)]
  data <- data[, .id := seq_len(.N)]
  data <- data[rep(.id, n_hours(debut, fin))]
  data <- data[, datetime := seq.POSIXt(from = min(debut), to = max(fin), by = "hours"), by = list(.id)]
  data <- data[, debut := NULL]
  data <- data[, fin := NULL]
  data <- data[, .id := NULL]
  data <- unique(x = data, by = c("groupe", "code_groupe", "datetime"), fromLast = TRUE)
  setorderv(x = data, cols = c("groupe", "datetime"))
  return(data)
}

#' Read EDF thermal file
#' 
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory.
#' 
#' @export
#' @return a \code{data.table}
#'
#' @importFrom data.table rbindlist
#' @importFrom readxl read_excel excel_sheets
read_planning_edf <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "Politique_S", fileext = "\\.xlsx$")
  sheets <- readxl::excel_sheets(path = path)
  res <- lapply(
    X = sheets,
    FUN = function(sheet) {
      dat <- read_edf_sheet(path = path, sheet = sheet)
      region <- unlist(readxl::read_excel(path = path, sheet = sheet, n_max = 5)[4, 1], use.names = FALSE)
      dat$region <- region
      return(dat)
    }
  )
  rbindlist(res)
}








#' Read planning from PSS Power
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory. 
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table setnames := data.table setorderv
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract_all
read_planning_psspower <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "PSSPower", fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 2)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("groupe", "code_groupe", "pmax", "pmin")]
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 1), use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2], by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = c("groupe", "code_groupe", "datetime", "pmax", "pmin"))
  return(data)
}





#' Read planning from Direct Energie
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory. 
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table setnames := data.table setorderv
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract_all
read_planning_directenergie <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "Direct_Energie", fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("groupe", "code_groupe", "pmax", "pmin")]
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 4)[3, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2], by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = c("groupe", "code_groupe", "datetime", "pmax", "pmin"))
  return(data)
}





#' Read planning from Total Raffinage Chimie
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory. 
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table setnames := data.table setorderv
#' @importFrom readxl read_excel
read_planning_trc <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "GPHEBDOTOTAL", fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(pmax)]
  data <- data[, .SD, .SDcols = c("groupe", "code_groupe", "pmax", "pmin")]
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- readxl::read_excel(path = path, sheet = 1, n_max = 4)
  dates <- as.data.frame(dates)
  dates <- c(as.character(dates[4, 6]), as.character(dates[4, 10]))
  dates <- as.POSIXct(dates, format = "%Y-%m-%d")
  dates <- seq.POSIXt(from = dates[1], to = dates[2], by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = c("groupe", "code_groupe", "datetime", "pmax", "pmin"))
  return(data)
}




#' Read planning from NovaWatt
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory. 
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table setnames := data.table setorderv
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract_all
read_planning_novawatt <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "NOVAWATT", fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("groupe", "code_groupe", "pmax", "pmin")]
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 4)[3, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2], by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = c("groupe", "code_groupe", "datetime", "pmax", "pmin"))
  return(data)
}



#' Read planning from UNIPER
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory. 
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table setnames := data.table setorderv
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract_all
read_planning_uniper <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "UNIPER", fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 2)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("groupe", "code_groupe", "pmax", "pmin")]
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 1)[1, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2], by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = c("groupe", "code_groupe", "datetime", "pmax", "pmin"))
  return(data)
}






#' Read planning from GDF-SUEZ
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory. 
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table setnames := data.table setorderv
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract_all
read_planning_gdfsuez <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "GDFSUEZ", fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  setnames(x = data, old = c("pcmax", "pcmin"), new = c("pmax", "pmin"))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("groupe", "code_groupe", "pmax", "pmin")]
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 1)[1, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2], by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = c("groupe", "code_groupe", "datetime", "pmax", "pmin"))
  return(data)
}
