
#' Draw Remaining Capacity
#'
#' @param marge_seule Margin data
#' @param marge_inter Margin data
#' @param area Concerned area, used in title.
#'
#' @export
#'
#' @importFrom dygraphs dygraph dyRangeSelector dyOptions dyAxis %>%
#' @importFrom htmlwidgets JS
#' @importFrom data.table data.table dcast setcolorder := as.xts.data.table
#'
#' @examples
#' \dontrun{
#'
#' draw_stack_hist(marge_seul_fr, marge_inter_fr, "fr")
#'
#' }
draw_stack_hist <- function(marge_seule, marge_inter, area = NULL) {

  date <- marge_seule$datetime

  marge_seule <- marge_seule[, -c("datetime"), with = FALSE]
  marge_inter <- marge_inter[, -c("datetime"), with = FALSE]

  marge_inter <- marge_inter[, lapply(.SD, function(x) { x[-1 <= x & x <= 0] <- 0; x })]

  level_seule <- marge_seule[, lapply(.SD, function(x) {
    ((x <= 0) * 2 - 1) * -1
  })]

  level_interco <- marge_inter[, lapply(.SD, function(x) {
    sign(x) * 10
  })]

  code_scenario <- level_interco + level_seule

  code_scenario[, .id := seq_len(.N)]
  code_scenario_ <- melt(data = code_scenario, id.vars = ".id")

  # any(num_equal(code_scenario_$value, -9))

  code_scenario_[, couleurs := NA_character_]
  code_scenario_[num_equal(value, -11), couleurs := "RED"]
  code_scenario_[num_equal(value, -1), couleurs := "BROWN"]
  code_scenario_[num_equal(value, 1), couleurs := "ORANGE"]
  code_scenario_[num_equal(value, 9), couleurs := "YELLOW"]
  code_scenario_[num_equal(value, 11), couleurs := "GREEN"]
  code_scenario_ <- code_scenario_[, list(freq = .N), by = list(.id, couleurs)]
  code_scenario_[, freq := freq / ncol(code_scenario) * 100]
  code_scenario_[, couleurs := factor(x = couleurs, levels = c("RED", "BROWN", "ORANGE", "YELLOW", "GREEN"))]
  code_scenario_ <- dcast(data = code_scenario_, formula = .id ~ couleurs, value.var = "freq", drop = FALSE)
  code_scenario_[is.na(code_scenario_)] <- 0
  code_scenario_[, .id := NULL]
  code_scenario_[, datetime := date]
  setcolorder(
    x = code_scenario_,
    neworder = c("datetime", rev(c("RED", "BROWN", "ORANGE", "YELLOW", "GREEN")))
  )
  # setDF(code_scenario_)
  # str(code_scenario_)
  graph <- dygraph(data = as.xts.data.table(code_scenario_), main = paste0("Remaining capacity ", toupper(area))) %>%
    dyRangeSelector()%>%
    dyAxis(
      name = 'y', label = "Pourcentage (%)",
      rangePad = 0, axisLabelFormatter = htmlwidgets::JS("function(d) {return d + '%';}")
    )%>%
    dyOptions(
      useDataTimezone = TRUE,
      stackedGraph = TRUE,
      colors =c("green", "yellow", "orange", "peru", "red") ,
      plotter = "function barChartPlotter(e) {
      var ctx = e.drawingContext;
      var points = e.points;
      var y_bottom = e.dygraph.toDomYCoord(0);  // see     http://dygraphs.com/jsdoc/symbols/Dygraph.html#toDomYCoord

      // This should really be based on the minimum gap
      var bar_width = 2/3 * (points[1].canvasx - points[0].canvasx);
      ctx.fillStyle = e.color;

      // Do the actual plotting.
      for (var i = 0; i < points.length; i++) {
      var p = points[i];
      var center_x = p.canvasx;  // center of the bar

      ctx.fillRect(center_x - bar_width / 2, p.canvasy,
      bar_width, y_bottom - p.canvasy);
      ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
      bar_width, y_bottom - p.canvasy);
      }
      }")

  return(graph)
}
