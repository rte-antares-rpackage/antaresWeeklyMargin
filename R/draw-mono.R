
#' Draw a Monotone
#'
#' @param data A \code{data.table} with at leat two columns.
#' @param main Main plot title (optional).
#'
#' @return a dygrgaph htmlwidget
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#'   x = seq_len(100),
#'   y = sort(rnorm(100, 10, 5))
#' )
#' draw_mono(dat)
#'
draw_mono <- function(data, main = NULL) {
  dygraph(data = data, main = main) %>%
    dyHighlight(highlightCircleSize = 5)%>%
    dyAxis(
      name = "y", label = "",
      axisLabelFormatter = htmlwidgets::JS("function(d) {return d + ' MW';}")
    )%>%
    dyAxis(
      name = "x", label = "% de sc\u00e9narios", valueRange = c(0, 100), rangePad = 5,
      axisLabelFormatter = htmlwidgets::JS("function(d) {return d + '%';}")
    )
}
