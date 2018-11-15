
#' Setup Eco2mix data for Weekly Margin Simulation
#'
#' @param path_input Path where eco2mix data are or will downloaded.
#'
#' @export
#'
setup_eco2mix <- function(path_input) {
  path_eco2mix <- file.path(path_input, "eco2mix")
  if (!file.exists(path_eco2mix)) {
    dir.create(path_eco2mix)
  }
  check_files <- check_files_eco2mix(path_eco2mix)
  if (check_files) {
    message("eco2mix correctly initialized!")
    return(invisible())
  }
  
  eco2mix_tr <- try(download_eco2mix("tr", path_eco2mix), silent = TRUE)
  if ("try-error" %in% class(eco2mix_tr)) {
    warning(paste0(
      "Failed to download eco2mix data, please download archive here: ",
      "https://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_En-cours-TR.zip",
      ", and put unzip file here: ", path_eco2mix
    ), call. = FALSE)
  }
  eco2mix_cons <- try(download_eco2mix("cons", path_eco2mix), silent = TRUE)
  if ("try-error" %in% class(eco2mix_cons)) {
    warning(paste0(
      "Failed to download eco2mix data, please download archive here: ",
      "https://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_En-cours-Consolide.zip",
      ", and put unzip file here: ", path_eco2mix
    ), call. = FALSE)
  }
  message("eco2mix correctly initialized!")
  return(invisible())
}

check_files_eco2mix <- function(path) {
  all_files <- list.files(path = path, recursive = TRUE)
  tr <- "eCO2mix_RTE_En-cours-TR.xls"
  cons <- "eCO2mix_RTE_En-cours-Consolide.xls"
  tr %in% all_files & cons %in% all_files
}

#' @importFrom utils download.file unzip
download_eco2mix <- function(type = c("tr", "cons"), path) {
  type <- match.arg(type)
  url_dat <- switch(
    type, 
    "tr" = "https://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_En-cours-TR.zip", 
    "cons" = "https://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_En-cours-Consolide.zip"
  )
  download.file(
    url = url_dat,
    destfile = file.path(path, "eco2mix.zip")
  )
  path_dat <- unzip(zipfile = file.path(path, "eco2mix.zip"), exdir = path)
  invisible(path_dat)
}


#' Read eco2mix data
#'
#' @param path Path to file.
#'
#' @export
#' 
#' @importFrom data.table fread setnames :=
#' @importFrom stringr str_split
#' @importFrom utils tail
#'
#' @examples
#' \dontrun{
#' 
#' eco2mix <- read_eco2mix("eCO2mix_RTE_En-cours-Consolide.xls")
#' 
#' }
read_eco2mix <- function(path) {
  suppressWarnings(eco2mix_dat <- fread(file = path, skip = 1L, fill = TRUE, blank.lines.skip = TRUE))
  eco2mix_dat <- eco2mix_dat[-.N]
  
  # column's names
  col_names <- readLines(con = path, n = 1)
  col_names <- str_split(col_names, "\t")[[1]]
  col_names <- clean_names(col_names)
  
  col2suppr <- tail(names(eco2mix_dat), 1)
  eco2mix_dat[, (col2suppr) := NULL]
  setnames(eco2mix_dat, names(eco2mix_dat), col_names)
  
  setnames(eco2mix_dat, "hydraulique_fil_de_l_eau_eclusee", "hydraulique_fil_eau_eclusee")
  
  eco2mix_dat[, date_heure := paste(date, heures)]
  eco2mix_dat[, date_heure := as.POSIXct(date_heure, format = "%Y-%m-%d %H:%M")]
  eco2mix_dat[, date := as.Date(date)]
  
  return(eco2mix_dat)
}



