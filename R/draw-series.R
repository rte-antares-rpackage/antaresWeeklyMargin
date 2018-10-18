
#' Draw a time series
#'
#' @param data Data to visualize, an \code{antaresDataTable} object.
#' @param serie Name of the serie.
#' @param mcYears Monte-Carlo years to represent.!
#' @param main Title for the chart.
#'
#' @return a \code{dygraphs} object.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # TODO
#'
#' }
#' @importFrom data.table dcast as.xts.data.table
#' @importFrom dygraphs dygraph dyLegend dyHighlight dyCSS dyOptions dyAxis
draw_series <- function(data, serie = "LOAD", mcYears = 1:51, main = serie) {

  stopifnot(inherits(x = data, what = "antaresDataTable"))

  dat <- data[mcYear %in% mcYears]
  dat <- dcast(data = dat, formula = time ~ mcYear, value.var = serie)

  dygraph(data = as.xts.data.table(dat), main = main) %>%
    dyLegend(show = "always") %>%
    dyHighlight(highlightCircleSize = 3)%>%
    dyLegend(show = "always")%>%
    dyCSS(css = system.file('www/css_dygraph.css', package = 'antaresWeeklyMargin'))%>%
    dyOptions(colors = rep("gray", 51)) %>%
    dyOptions(useDataTimezone = TRUE)  %>%
    dyAxis(name = "y", label = "MW")
}
