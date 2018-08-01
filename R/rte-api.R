

proxy_error <- function(url) {
  sprintf("Unable to access %s API, please provide NNI/PROXY-PASSWORD to the function.", url)
}


#' Get proxy information
#'
#' @param user Optionnal, username (NNI) for proxy.
#' @param proxy_pwd Optionnal, password for proxy.
#'
#' @return a list with username and password for proxy
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # If you have set your proxy informations,
#' # the list should contain them
#' get_proxy_info()
#'
#' }
get_proxy_info <- function(user = NULL, proxy_pwd = NULL) {
  res <- list()
  if (!is.null(user)) {
    res$user <- user
  } else {
    res$user <- Sys.getenv("USR_PROXY")
    if (res$user == "")
      res$user <- NULL
  }
  if (!is.null(proxy_pwd)) {
    res$proxy_pwd <- proxy_pwd
  } else {
    res$proxy_pwd <- Sys.getenv("PWD_PROXY")
    if (res$proxy_pwd == "")
      res$proxy_pwd <- NULL
  }
  return(res)
}



#' Setup proxy credentials
#'
#'
#' @param user Username (NNI) for proxy.
#' @param proxy_pwd Password for proxy.
#'
#' @note You'll need to restart your R session for change to be effective.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' setupProxy("MYNNI", "MY_PASSWORD")
#' # restart your R session
#' # info should appear in the list
#' get_proxy_info()
#'
#' }
setupProxy <- function(user, proxy_pwd) {
  cat(
    paste(paste0("USR_PROXY=", user),
          paste0("PWD_PROXY=", proxy_pwd), sep = "\n"),
    file = file.path(path.expand("~/"), ".Renviron"),
    append = TRUE
  )
  message("Proxy info saved, please restart your R session")
}



parse_datetime <- function(x) {
  x <- gsub(pattern = "(\\d{2}):(\\d{2})$", replacement = "\\1\\2", x = x)
  as.POSIXct(x, format = "%FT%X%z")
}
format_datetime <- function(x) {
  x <- format(x)
  gsub(pattern = "(\\d{2})(\\d{2})$", replacement = "\\1:\\2", x = x)
}




# Open API ----------------------------------------------------------------


#' Get data from eco2mix via Open-RTE API
#'
#' @param from date from which to retrieve data.
#' @param to date until which to recover data.
#' @param resource resource to use between real time ("tr") or consolidated data ("cons").
#' @param user Username (NNI) for proxy if needeed.
#' @param proxy_pwd Password for proxy if needeed.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom crul HttpClient proxy
#' @importFrom curl ie_get_proxy_for_url
#' @importFrom data.table as.data.table setcolorder :=
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'
#' eco2mix <- get_eco2mix(
#'   from = "2018-01-06",
#'   to = "2018-01-12",
#'   user = "NNI",         # needeed if no internet connection open
#'   proxy_pwd = "PASSWORD"
#' )
#'
#' }
get_eco2mix <- function(from = NULL, to = NULL, resource = c("tr", "cons"), user = NULL, proxy_pwd = NULL) {
  resource <- match.arg(resource)
  if (resource == "tr") {
    dataset <- "eco2mix-national-tr"
  } else {
    dataset <- "eco2mix-national-cons-def"
  }
  url <- "https://opendata.reseaux-energies.fr/api/records/1.0/search/"
  cli <- crul::HttpClient$new(url = url)
  proxy <- get_proxy_info(user, proxy_pwd)
  if (!is.null(proxy$user) & !is.null(proxy$proxy_pwd)) {
    cli$proxies <- crul::proxy(
      url = curl::ie_get_proxy_for_url("https://httpbin.org/get"),
      user = proxy$user, pwd = proxy$proxy_pwd
    )
  }
  if (!is.null(from)) {
    from_ <- as.Date(from)
    from <- from_ - 1
    from <- sprintf("date_heure>=%s", format_datetime(from))
  }
  if (!is.null(to)) {
    to_ <- as.Date(to)
    to <- to_ + 1
    to <- sprintf("date_heure<=%s", format_datetime(to))
  }
  q <- list(from = from, to = to)
  q <- dropNulls(q)
  q <- Reduce(pasteAND, q)
  res <- try(cli$get(query = list(
    dataset = dataset,
    rows = -1, sort = "-date_heure",
    q = q
  )), silent = TRUE)
  if ("try-error" %in% class(res))
    stop(proxy_error("opendata.rte-france.com"))
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  json <- jsonlite::fromJSON(txt)
  dat <- as.data.table(json$records$fields)
  if (nrow(dat) == 0)
    return(dat)
  dat <- dat[, timestamp := date_heure]
  dat <- dat[, date_heure := parse_datetime(date_heure)]
  dat <- dat[, date := as.Date(format(date_heure, format = "%Y-%m-%d"))]
  if (!is.null(from)) {
    dat <- dat[date >= from_]
  }
  if (!is.null(to)) {
    dat <- dat[date <= to_]
  }
  setcolorder(dat, c("date", "date_heure", setdiff(names(dat), c("date", "date_heure"))))
  dat
}




#' Get hydraulique data drom eco2mix
#'
#' @param from date from which to retrieve data, if \code{NULL} set to previous saturday before previous friday.
#' @param to date until which to recover data, if \code{NULL} set to previous friday.
#' @param user Username (NNI) for proxy if needed.
#' @param proxy_pwd Password for proxy if needed.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#'
#' fil_eau <- get_hydraulique_fil_de_l_eau_eclusee(
#'   user = "NNI", proxy_pwd = "PASSWORD"
#' )
#'
#' }
get_hydraulique_fil_de_l_eau_eclusee <- function(from = NULL, to = NULL, user = NULL, proxy_pwd = NULL) {
  if (is.null(from))
    from <- get_previous(what = "samedi", date = get_previous(what = "vendredi"))
  if (is.null(to))
    to <- get_previous(what = "vendredi")
  vars <- c("date", "date_heure", "hydraulique_fil_eau_eclusee")
  # eco2mix_tr <- get_eco2mix(from = from, to = to, resource = "tr", user = user, proxy_pwd = proxy_pwd)
  eco2mix_tr <- dl_eco2mix("tr")
  eco2mix_tr <- eco2mix_tr[, .SD, .SDcols = intersect(names(eco2mix_tr), vars)]
  # eco2mix_cons <- get_eco2mix(from = from, to = to, resource = "cons", user = user, proxy_pwd = proxy_pwd)
  eco2mix_cons <- dl_eco2mix("cons")
  eco2mix_cons <- eco2mix_cons[, .SD, .SDcols = intersect(names(eco2mix_cons), vars)]
  eco2mix <- rbindlist(list(eco2mix_tr, eco2mix_cons), fill = TRUE)
  eco2mix <- eco2mix[order(date_heure)]
  eco2mix <- eco2mix[format(date_heure, format = "%M") == "00"]
  eco2mix <- eco2mix[date >= as.Date(from)]
  eco2mix <- eco2mix[date <= as.Date(to)]
  return(eco2mix)
}






# Via https://www.rte-france.com/fr/eco2mix/eco2mix-telechargement
#' @importFrom utils unzip tail download.file
#' @importFrom data.table setnames := fread
dl_eco2mix <- function(type = c("tr", "cons")) {
  url_dat <- switch(
    type, 
    "tr" = "https://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_En-cours-TR.zip", 
    "cons" = "https://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_En-cours-Consolide.zip"
  )
  tmp <- tempdir()
  if (!dir.exists(tmp))
    dir.create(tmp)
  tmp_file <- tempfile(fileext = ".zip")
  download.file(
    url = url_dat,
    destfile = file.path(tmp, "eco2mix.zip")
  )
  path_dat <- unzip(zipfile = file.path(tmp, "eco2mix.zip"), exdir = tmp)
  suppressWarnings(eco2mix_dat <- fread(file = path_dat))
  names_cols <- names(eco2mix_dat)[-1]
  eco2mix_dat[, (tail(names_cols, 1)) := NULL]
  setnames(eco2mix_dat, names(eco2mix_dat), clean_names(names_cols))
  
  setnames(eco2mix_dat, "hydraulique_fil_de_l_eau_eclusee", "hydraulique_fil_eau_eclusee")
  
  eco2mix_dat[, date_heure := paste(date, heures)]
  eco2mix_dat[, date_heure := as.POSIXct(date_heure, format = "%Y-%m-%d %H:%M")]
  eco2mix_dat[, date := as.Date(date)]
  
  return(eco2mix_dat)
}









# Data API ----------------------------------------------------------------



#' Get a token to access RTE data API
#'
#' @param key a base64 encoded string or a list containing
#' 'client_id' and 'client_secret'. To get those credentials
#' you need an account on \url{https://data.rte-france.com} and
#' to create an application for the concerned API.
#' @param user Username (NNI) for proxy if needeed.
#' @param proxy_pwd Password for proxy if needeed.
#'
#' @return a list with the access token
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom curl ie_get_proxy_for_url
#' @importFrom crul HttpClient proxy
#' @importFrom base64enc base64encode
#'
#' @examples
#' \dontrun{
#'
#' # To create a token you can use id_client and id_secret
#' id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' token <- get_token(
#'   key = list(id_client = id_client, id_secret = id_secret)
#' )
#'
#' # or the base64 encoded key
#' key <- "WFhYWFgtWFhYWFgtWFhYWFgtWFhYWFgtWFhYWFg="
#' token <- get_token(key)
#'
#' }
get_token <- function(key, user = NULL, proxy_pwd = NULL) {
  if (is.list(key))
    key <- base64encode(charToRaw(paste(key$id_client, key$id_secret, sep = ":")))
  cli <- crul::HttpClient$new(
    url = "https://digital.iservices.rte-france.com/token/oauth/",
    headers = list(
      Authorization = paste("Basic", key)
    )
  )
  proxy <- get_proxy_info(user, proxy_pwd)
  if (!is.null(proxy$user) & !is.null(proxy$proxy_pwd)) {
    cli$proxies <- crul::proxy(
      url = curl::ie_get_proxy_for_url("https://httpbin.org/get"),
      user = proxy$user, pwd = proxy$proxy_pwd
    )
  }
  res <- try(cli$post(), silent = TRUE)
  if ("try-error" %in% class(res))
    stop(proxy_error("digital.iservices.rte-france.com"))
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  json <- jsonlite::fromJSON(txt)
  json
}




#' Retrieve NTC data via RTE data API
#'
#' @param token Token obtained with \code{\link{get_token}}.
#' @param type NTC due type, mandatory, one or several
#' between 'ANNUAL', 'MONTHLY', 'WEEKLY', 'D-1', 'CURTAILED'.
#' @param start_date Optional, starting date to filter results,
#' if used, \code{end_date} must be set as well.
#' @param end_date Optional, ending date to filter results.
#' @param country_eic_code Country code.
#' @param user Username (NNI) for proxy if needeed.
#' @param proxy_pwd Password for proxy if needeed.
#'
#' @return a \code{data.table}.
#' @export
#'
#' @importFrom crul HttpClient proxy
#' @importFrom curl ie_get_proxy_for_url
#' @importFrom data.table as.data.table rbindlist :=
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'
#' # First you need a token
#' id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' token <- get_token(
#'   key = list(id_client = id_client, id_secret = id_secret)
#' )
#'
#' # Then you can retrieve NTC data
#' ntc <- get_ntc(token = token, type = c("ANNUAL", "MONTHLY"))
#'
#' }
get_ntc <- function(token, type = c("ANNUAL", "MONTHLY", "WEEKLY", "D-1", "CURTAILED"),
                    start_date = NULL, end_date = NULL, country_eic_code = NULL,
                    user = NULL, proxy_pwd = NULL) {

  type <- match.arg(type, several.ok = TRUE)

  if (!is.null(start_date)) {
    if (is.null(end_date))
      stop("If start_date is set, end_date must be passed as well.", call. = FALSE)
    start_date <- as.POSIXct(start_date)
    start_date <- format_datetime(start_date)
  }
  if (!is.null(end_date)) {
    end_date <- as.POSIXct(end_date)
    end_date <- format_datetime(end_date)
  }
  q <- list(
    type = paste(type, collapse = ","),
    start_date = start_date, end_date = end_date,
    country_eic_code = country_eic_code
  )
  q <- dropNulls(q)

  cli <- crul::HttpClient$new(
    url = "https://digital.iservices.rte-france.com/open_api/ntc/v1/ntc",
    headers = list(
      Host = "digital.iservices.rte-france.com",
      Authorization = paste(token$token_type, token$access_token)
    )
  )
  proxy <- get_proxy_info(user, proxy_pwd)
  if (!is.null(proxy$user) & !is.null(proxy$proxy_pwd)) {
    cli$proxies <- crul::proxy(
      url = curl::ie_get_proxy_for_url("https://httpbin.org/get"),
      user = proxy$user, pwd = proxy$proxy_pwd
    )
  }
  res <- cli$get(query = q)
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  json <- jsonlite::fromJSON(txt)

  data <- json[[1]]
  data <- as.data.table(data)
  values <- rbindlist(l = data$values, idcol = TRUE)
  data[, .id := seq_len(.N)]
  data[, values := NULL]
  data[, start_date := NULL]
  data[, end_date := NULL]
  data <- merge(x = data, y = values, by = ".id", all.x = TRUE, all.y = TRUE)
  data <- data[, start_date := parse_datetime(start_date)]
  data <- data[, end_date := parse_datetime(end_date)]
  data <- data[, updated_date := parse_datetime(updated_date)]
  data <- data[, .id := NULL]
  data
}


