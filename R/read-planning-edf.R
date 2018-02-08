

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






