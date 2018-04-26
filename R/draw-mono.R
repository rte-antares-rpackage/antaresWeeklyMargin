
#' Draw a Monotone
#'
#' @param data A \code{data.table} with at leat two columns.
#' @param main Main plot title (optional).
#' @param label Serie's label (optional).
#'
#' @return a dygrgaph htmlwidget
#' @export
#'
#' @importFrom stringr str_extract
#' @importFrom dygraphs dygraph dyHighlight dyAxis dySeries dyAxis
#' @importFrom htmlwidgets JS
#'
#' @examples
#'
#' dat <- data.frame(
#'   x = seq_len(100),
#'   y = sort(rnorm(100, 10, 5))
#' )
#' draw_mono(dat)
#'
draw_mono <- function(data, area = "auto", main = "auto", label = "auto") {
  if (!is.null(area) && area == "auto") {
    area <- deparse(substitute(data))
    area <- str_extract(string = area, pattern = "(?<=_)[:alpha:]+$")
  }
  if (!is.null(label) && label == "auto") {
    label <- sprintf("Flux %s", toupper(area))
  }
  if (!is.null(main) && main == "auto") {
    main <- sprintf("Monotone des flux FR - %s", toupper(area))
  }
  dygraph(data = data, main = main) %>%
    dySeries(name = names(data)[2], label = label) %>%
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
