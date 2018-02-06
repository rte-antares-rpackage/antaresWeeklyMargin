

pattern_suppliers <- function(supplier = NULL) {
  corresp <- list(
    gdfsuez = "planning hebdomadaire GDFSUEZ",
    uniper = "PHEBDO_UNIPER_",
    novawatt = "P_HEBDO_ NOVAWATT",
    trc = "GPHEBDOTOTAL",
    directenergie = "Direct_Energie_Bayet_Weekly_Nomination",
    psspower = "Planning PSSPower PSS",
    edf = "Politique_S"
  )
  if (is.null(supplier))
    return(corresp)
  corresp[[supplier]]
}




#' @importFrom data.table as.data.table := setorderv
#' @importFrom readxl read_excel
read_edf_sheet <- function(path, sheet) {
  data <- readxl::read_excel(path = path, sheet = sheet, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[, .SD, .SDcols = c("comb_", "groupe", "code_groupe", "pmd", "debut", "fin", "code_essai", "pmax", "pmin")]
  data <- data[, groupe := locf(groupe)]
  data <- data[, code_groupe := locf(code_groupe)]
  data <- data[, comb_ := locf(comb_)]
  data <- data[, pmd := locf(pmd)]
  data <- data[!is.na(debut)]

  # check code essai VP
  data <- data[, val_vp := check_code_essai(
    x = code_essai, code = "VP", possible.values = c("^VP", "^RVP.*")
  ), by = code_groupe]
  data <- data[val_vp == TRUE]
  data <- data[, val_vp := NULL]
  # check code essai VD
  data <- data[, val_vd := check_code_essai(
    x = code_essai, code = "VD", possible.values = c("^VD", "^RVD.*")
  ), by = code_groupe]
  data <- data[val_vd == TRUE]
  data <- data[, val_vd := NULL]
  # check code essai ASR
  data <- data[, val_asr := check_code_essai(
    x = code_essai, code = "ASR", possible.values = c("^ASR", "^RASR.*")
  ), by = code_groupe]
  data <- data[val_asr == TRUE]
  data <- data[, val_asr := NULL]
  # check code essai AND
  data <- data[, val_and := check_code_essai(
    x = code_essai, code = "AND", possible.values = c("^AND")
  ), by = code_groupe]
  data <- data[val_and == TRUE]
  data <- data[, val_and := NULL]

  # LIM/LIMIT
  data <- data[, val_llim := check_lim_limit(x = code_essai), by = code_groupe]
  data <- data[val_llim == TRUE]
  data <- data[, val_llim := NULL]


  # Delete REDEM
  data <- data[code_essai != "^REDEM.*"]

  # expand dates
  data <- data[, .id := seq_len(.N)]
  data <- data[rep(.id, n_hours(debut, fin))]
  data <- data[, datetime := seq.POSIXt(from = min(debut), to = max(fin), by = "hours"), by = list(.id)]
  data <- data[, datetime := as.POSIXct(round.POSIXt(datetime - 1, units = "hours"))]
  data <- data[, debut := NULL]
  data <- data[, fin := NULL]
  data <- data[, .id := NULL]

  # date header
  dates_h <- readxl::read_excel(path = path, sheet = sheet, n_max = 5)[4, 4]
  dates_h <- unlist(dates_h, use.names = FALSE)
  dates_h <- stringr::str_extract_all(string = dates_h, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates_h <- as.POSIXct(dates_h, format = "%d/%m/%Y")
  data <- data[datetime > dates_h[1] &  datetime <= dates_h[2] + 24 * 60 * 60]

  # dedup
  data <- unique(x = data, by = c("groupe", "code_groupe", "datetime"), fromLast = TRUE)
  setorderv(x = data, cols = c("groupe", "datetime"))
  return(data)
}

#' @importFrom stringr str_detect
check_code_essai <- function(x, code = "VP", possible.values = c("^VP", "^RVP.*")) {
  if (code %in% x) {
    lind <- lapply(X = possible.values, FUN = stringr::str_detect, string = x)
    Reduce(f = `|`, x = lind)
  } else {
    rep_len(x = TRUE, length.out = length(x))
  }
}

check_lim_limit <- function(x) {
  if ("LIM" %in% x & "LIMIT" %in% x) {
    x != "LIMIT"
  } else {
    rep_len(x = TRUE, length.out = length(x))
  }
}



#' Read EDF thermal file
#'
#' @param path Path to the Excel file. Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' the most recent Excel file is read. If the argument is not specified, a dialog
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
#' @param path Path to the Excel file. Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' the most recent Excel file is read. If the argument is not specified, a dialog
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
  path <- select_file(path, pattern_suppliers("psspower"), fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 2)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("comb_", "groupe", "code_groupe", "pcn", "code_essai", "pmax", "pmin")]
  setnames(x = data, old = "pcn", new = "pmd")
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 1), use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2] + 23 * 60 * 60, by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = suppliers_vars())
  return(data)
}





#' Read planning from Direct Energie
#'
#' @param path Path to the Excel file. Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' the most recent Excel file is read. If the argument is not specified, a dialog
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
  path <- select_file(path, pattern_suppliers("directenergie"), fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("comb_", "groupe", "code_groupe", "pcn", "code_essai", "pmax", "pmin")]
  setnames(x = data, old = "pcn", new = "pmd")
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 4)[3, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2] + 23 * 60 * 60, by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = suppliers_vars())
  return(data)
}





#' Read planning from Total Raffinage Chimie
#'
#' @param path Path to the Excel file. Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' the most recent Excel file is read. If the argument is not specified, a dialog
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
  path <- select_file(path, pattern_suppliers("trc"), fileext = "\\.xlsx$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(pmax)]
  data <- data[, .SD, .SDcols = c("site", "groupe", "code_groupe", "pcn_mw_", "code_essai", "pmax", "pmin")]
  setnames(x = data, old = "pcn_mw_", new = "pmd")
  setnames(x = data, old = "site", new = "comb_")
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- readxl::read_excel(path = path, sheet = 1, n_max = 4)
  dates <- as.data.frame(dates)
  dates <- c(as.character(dates[4, 6]), as.character(dates[4, 10]))
  dates <- as.POSIXct(dates, format = "%Y-%m-%d")
  dates <- seq.POSIXt(from = dates[1], to = dates[2] + 23 * 60 * 60, by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = suppliers_vars())
  return(data)
}




#' Read planning from NovaWatt
#'
#' @param path Path to the Excel file. Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' the most recent Excel file is read. If the argument is not specified, a dialog
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
  path <- select_file(path, pattern_suppliers("novawatt"), fileext = "\\.xls.*$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("comb_", "groupe", "code_groupe", "pcn", "code_essai", "pmax", "pmin")]
  setnames(x = data, old = "pcn", new = "pmd")
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 4)[3, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2] + 23 * 60 * 60, by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = suppliers_vars())
  return(data)
}



#' Read planning from UNIPER
#'
#' @param path Path to the Excel file. Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' the most recent Excel file is read. If the argument is not specified, a dialog
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
  path <- select_file(path, pattern_suppliers("uniper"), fileext = "\\.xls.*$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 2)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("comb_", "groupe", "code_groupe", "pcn", "code_essai", "pmax", "pmin")]
  setnames(x = data, old = "pcn", new = "pmd")
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 1)[1, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2] + 23 * 60 * 60, by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = suppliers_vars())
  return(data)
}






#' Read planning from GDF-SUEZ
#'
#' @param path Path to the Excel file. Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' the most recent Excel file is read. If the argument is not specified, a dialog
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
  path <- select_file(path, pattern_suppliers("gdfsuez"), fileext = "\\.xls.*$")
  # data
  data <- readxl::read_excel(path = path, sheet = 1, skip = 5)
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))
  setnames(x = data, old = c("pcmax", "pcmin"), new = c("pmax", "pmin"))
  data <- data[!is.na(groupe)]
  data <- data[, .SD, .SDcols = c("comb_", "groupe", "code_groupe", "pcn", "code_essai", "pmax", "pmin")]
  setnames(x = data, old = "pcn", new = "pmd")
  data <- data[, .id := .I, by = groupe]
  # dates
  dates <- unlist(readxl::read_excel(path = path, sheet = 1, n_max = 1)[1, 1], use.names = FALSE)
  dates <- stringr::str_extract_all(string = dates, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
  dates <- as.POSIXct(dates, format = "%d/%m/%Y")
  dates <- seq.POSIXt(from = dates[1], to = dates[2] + 23 * 60 * 60, by = "hours")
  dates <- data.table(
    .id = rep(seq_along(unique(data$groupe)), each = length(dates)),
    datetime = rep(dates, times = length(unique(data$groupe)))
  )
  # merge data+dates
  data <- merge(x = dates, y = data, by = ".id", all.x = TRUE)
  data <- data[, .id := NULL]
  setorderv(x = data, cols = c("groupe", "datetime"))
  setcolorder(x = data, neworder = suppliers_vars())
  return(data)
}



suppliers_vars <- function() {
  c("comb_", "groupe", "code_groupe", "pmd", "code_essai", "datetime", "pmax", "pmin")
}



#' Read planning from suppliers
#'
#' @param path Path to the Excel file(s). Can be left blank, designate an Excel
#' file or directory containing Excel files. If the path points to a directory,
#' all Excel file are read. If the argument is not specified, a dialog
#' box will open to select a directory.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table rbindlist
read_planning <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path = path, pattern = ".", fileext = ".", multiple = TRUE, verbose = FALSE)
  patterns <- c("GDFSUEZ", "UNIPER", "NOVAWATT", "GPHEBDOTOTAL", "Direct_Energie", "PSSPower", "Politique_S")
  funs <- c("read_planning_gdfsuez", "read_planning_uniper", "read_planning_novawatt",
            "read_planning_trc", "read_planning_directenergie", "read_planning_psspower", "read_planning_edf")
  dats <- lapply(
    X = path,
    FUN = function(x) {
      path_ <- gsub(pattern = ".*/", replacement = "", x = x)
      which_fun <- lapply(X = patterns, FUN = grepl, x = path_)
      which_fun <- unlist(which_fun, use.names = FALSE)
      if (sum(which_fun) == 1) {
        res <- do.call(what = funs[which_fun], list(path = x))
        res$id <- patterns[which_fun]
        res
      } else {
        NULL
      }
    }
  )
  dats <- data.table::rbindlist(l = dats, fill = TRUE)
  return(dats)
}





