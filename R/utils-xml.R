


#' @importFrom xml2 xml_find_all xml_attrs
get_date_seq <- function(xml) {
  dates <- xml2::xml_find_all(xml, xpath = ".//TimePeriodCovered")
  dates <- xml2::xml_attrs(dates)
  dates <- unlist(dates, use.names = FALSE)
  dates <- unlist(strsplit(x = dates, split = "/"))
  dates <- substr(x = dates, start = 1, stop = 16)
  dates <- as.POSIXct(dates, format = "%Y-%m-%dT%H:%M") # + 3600
  dates <- seq.POSIXt(from = dates[1], to = dates[2], by = "2 hours")[-1]
  data.frame(
    datetime = dates, date = as.Date(dates),
    pt = as.character(seq_along(dates)),
    stringsAsFactors = FALSE
  )
}

