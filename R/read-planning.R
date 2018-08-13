
config_planning <- function(name) {
  config <- list(
    psspower = list(
      sheet = 1,
      skip = 2,
      n_max = 1,
      row = 1, col = 1
    ),
    directenergie = list(
      sheet = 1,
      skip = 5,
      n_max = 4,
      row = 3, col = 1
    ),
    trc = list(
      sheet = 1,
      skip = 5,
      n_max = 4,
      row = 4, col = c(6, 10),
      format = "%Y-%m-%d"
    ),
    novawatt = list(
      sheet = 1,
      skip = 5,
      n_max = 4,
      row = 3, col = 1
    ),
    uniper = list(
      sheet = 1,
      skip = 2,
      n_max = 1,
      row = 1, col = 1
    ),
    gdfsuez = list(
      sheet = 1,
      skip = 5,
      n_max = 1,
      row = 1, col = 1
    )
  )
  config[[name]]
}

suppliers_vars <- function() {
  c("comb_", "groupe", "code_groupe", "pmd", "code_essai", "datetime", "pmax", "pmin")
}

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


#' @importFrom readxl read_excel
#' @importFrom data.table setorderv setcolorder
read_planning_supplier <- function(path, supplier) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, pattern_suppliers(supplier), fileext = "\\.xlsx$")

  config <- config_planning(supplier)

  # import data
  suppressWarnings({
    data <- readxl::read_excel(path = path, sheet = config$sheet, skip = config$skip, guess_max = 10)
  })
  # default dates
  dates <- get_default_dates(path, config$sheet, config$n_max, config$row, config$col, config$format)
  # process data
  data <- process_planning(data)
  # expand by dates
  data <- expand_dates(data, dates[1], dates[2])

  # dedup
  setorderv(x = data, cols = c("groupe", "code_groupe", "datetime"))
  data <- unique(x = data, by = c("groupe", "code_groupe", "datetime"), fromLast = TRUE)

  setcolorder(x = data, neworder = union(suppliers_vars(), names(data)))
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
  path <- select_file(path = path, pattern = ".", fileext = ".", multiple = TRUE, verbose = FALSE, recursive = TRUE)
  patterns <- c("GDFSUEZ", "UNIPER", "NOVAWATT", "GPHEBDOTOTAL", "Direct_Energie", "PSSPower", "Politique_S")
  names <- c("gdfsuez", "uniper", "novawatt", "trc", "directenergie", "psspower", "edf")
  dats <- lapply(
    X = path,
    FUN = function(x) {
      path_ <- gsub(pattern = ".*/", replacement = "", x = x)
      which_name <- lapply(X = patterns, FUN = grepl, x = path_)
      which_name <- unlist(which_name, use.names = FALSE)
      if (sum(which_name) == 1) {
        if (names[which_name] == "edf") {
          res <- read_planning_edf(x)
        } else {
          res <- read_planning_supplier(path = x, supplier = names[which_name])
        }
        res$id <- names[which_name]
        res
      } else {
        NULL
      }
    }
  )
  dats <- data.table::rbindlist(l = dats, fill = TRUE)
  dats[is.na(code_groupe), code_groupe := groupe]
  return(dats)
}






#' @importFrom data.table copy :=
expand_dates <- function(data, na_debut = NULL, na_fin = NULL) {
  data <- copy(data)
  if (is.null(na_debut)) {
    data <- data[!is.na(debut)]
  } else {
    data <- data[is.na(debut), debut := na_debut]
  }
  if (is.null(na_fin)) {
    data <- data[!is.na(fin)]
  } else {
    data <- data[is.na(fin), fin := na_fin]
  }
  data <- data[, .id := seq_len(.N)]
  data <- data[, debut := as.POSIXct(round.POSIXt(debut - 1, units = "hours"), tz = "Europe/Paris")]
  data <- data[, fin := as.POSIXct(round.POSIXt(fin - 1, units = "hours"), tz = "Europe/Paris")]
  data <- data[rep(.id, n_hours(debut, fin))]
  data <- data[, datetime := seq.POSIXt(from = min(debut), to = max(fin), by = "hours"), by = list(.id)]
  if (!is.null(na_debut)) {
    data <- data[datetime >= na_debut]
  }
  if (!is.null(na_fin)) {
    data <- data[datetime <= na_fin]
  }
  data <- data[, debut := NULL]
  data <- data[, fin := NULL]
  data <- data[, .id := NULL]
  return(data)
}

#' @importFrom data.table setnames :=
process_planning <- function(data) {
  data <- as.data.table(data)
  setnames(x = data, old = names(data), new = clean_names(names(data)))

  if ("pcn" %in% names(data)) {
    setnames(x = data, old = "pcn", new = "pmd")
  }
  if ("pcn_mw_" %in% names(data)) {
    setnames(x = data, old = "pcn_mw_", new = "pmd")
  }
  if ("site" %in% names(data)) {
    setnames(x = data, old = "site", new = "comb_")
  }
  if ("pcmax" %in% names(data)) {
    setnames(x = data, old = "pcmax", new = "pmax")
  }
  if ("pcmin" %in% names(data)) {
    setnames(x = data, old = "pcmin", new = "pmin")
  }

  # data <- data[!is.na(groupe)]
  data <- data[!is.na(pmax)]


  data <- data[, groupe := locf(groupe)]
  data <- data[, code_groupe := locf(code_groupe)]
  data <- data[, comb_ := locf(comb_)]
  data <- data[, pmd := locf(pmd)]

  # data <- data[, debut := format(as.Date(as.numeric(debut), origin = "1900-01-01") - 2)]
  data <- data[, debut := as.POSIXct(as.character(debut), tz = "Europe/Paris")]

  # data <- data[, fin := format(as.Date(as.numeric(fin), origin = "1900-01-01") - 2)]
  data <- data[, fin := as.POSIXct(as.character(fin), tz = "Europe/Paris")]
  # data <- data[, .SD, .SDcols = c("comb_", "groupe", "code_groupe", "pmd", "code_essai", "pmax", "pmin")]
  return(data)
}


get_default_dates <- function(path, sheet, n_max, row, col, format = NULL) {
  dat <- readxl::read_excel(path = path, sheet = sheet, n_max = n_max)
  dat[] <- lapply(dat, as.character)
  x <- unlist(dat[row, col], use.names = FALSE)
  if (is.null(format)) {
    x <- stringr::str_extract_all(string = x, pattern = "\\d{2}/\\d{2}/\\d{4}")[[1]]
    x <- as.POSIXct(x, format = "%d/%m/%Y", tz = "Europe/Paris")
  } else {
    x <- as.POSIXct(x, format = format, tz = "Europe/Paris")
  }
  x <- c(x[1], x[2] + 23 * 60 * 60)
  x
}








