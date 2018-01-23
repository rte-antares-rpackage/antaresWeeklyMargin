


#' Get data from eco2mix via Open-RTE API
#'
#' @param from date from which to retrieve data.
#' @param to date until which to recover data.
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
get_eco2mix <- function(from = NULL, to = NULL, user = NULL, proxy_pwd = NULL) {
  url <- "https://opendata.rte-france.com/api/records/1.0/search/"
  cli <- crul::HttpClient$new(url = url)
  if (!is.null(user) & !is.null(proxy_pwd)) {
    cli$proxies <- crul::proxy(
      url = curl::ie_get_proxy_for_url("https://httpbin.org/get"), 
      user = user, pwd = proxy_pwd
    )
  }
  if (!is.null(from)) {
    from <- sprintf("date_heure>=%s", format(from, format = "%Y-%m-%d"))
  }
  if (!is.null(to)) {
    to <- sprintf("date_heure<=%s", format(to, format = "%Y-%m-%d"))
  }
  q <- list(from = from, to = to)
  q <- dropNulls(q)
  q <- Reduce(pasteAND, q)
  res <- try(cli$get(query = list(
    dataset = "eco2mix_national_tr",
    rows = -1, sort = "-date_heure",
    q = q
  )), silent = TRUE)
  if ("try-error" %in% class(res))
    stop("Unable to access opendata.rte-france.com API, please provide NNI/PROXY-PASSWORD to the function.")
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  json <- jsonlite::fromJSON(txt)
  dat <- as.data.table(json$records$fields)
  if (nrow(dat) == 0)
    return(dat)
  dat <- dat[, date := as.Date(date)]
  dat <- dat[, date_heure := gsub(pattern = "(\\d{2}):(\\d{2})$", replacement = "\\1\\2", x = date_heure)]
  dat <- dat[, date_heure := as.POSIXct(date_heure, format = "%FT%X%z")]
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
  eco2mix <- get_eco2mix(from, to, user, proxy_pwd)
  eco2mix <- eco2mix[, .SD, .SDcols = c("date", "date_heure", "hydraulique_fil_de_l_eau_eclusee")]
  eco2mix <- eco2mix[format(date_heure, format = "%M") == "00"]
  return(eco2mix)
}



