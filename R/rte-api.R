


#' Get data from eco2mix via Open-RTE API
#'
#' @param from date from which to retrieve data.
#' @param to date until which to recover data.
#' @param user Username (NNI) for proxy if needed.
#' @param proxy_pwd Password for proxy if needed.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom crul HttpClient
#' @importFrom curl ie_get_proxy_for_url
#' @importFrom data.table as.data.table
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
get_eco2mix <- function(from = NULL, to = NULL, user = NULL, proxy_pwd = NULL) {
  if (!is.null(user) & !is.null(proxy_pwd)) {
    proxy <- paste0(
      "http://", user, ":", proxy_pwd, "@",
      curl::ie_get_proxy_for_url("https://httpbin.org/get")
    )
    Sys.setenv(
      http_proxy = proxy, https_proxy = proxy,
      HTTP_PROXY = proxy, HTTPS_PROXY = proxy,
      ftp_proxy = proxy, FTP_PROXY = proxy
    )
  }
  url <- "https://opendata.rte-france.com/api/records/1.0/search/"
  cli <- crul::HttpClient$new(url = url)
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
  data.table::as.data.table(json$records$fields)
}
