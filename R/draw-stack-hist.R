
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
#' @importFrom data.table data.table
#'
#' @examples
#' \dontrun{
#'
#' draw_stack_hist(marge_seul_fr, marge_inter_fr, "fr")
#'
#' }
draw_stack_hist <- function(marge_seule, marge_inter, area = NULL) {

  date <- marge_seule$DATE_UTC

  marge_seule <- marge_seule[, -c("DATE_UTC"), with = FALSE]
  marge_inter <- marge_inter[, -c("DATE_UTC"), with = FALSE]

  marge_inter <- marge_inter[, lapply(.SD, function(x) { x[-1 <= x & x <= 0] <- 0; x })]

  level_seule <- marge_seule[, lapply(.SD, function(x) {
    ((x <= 0) * 2 - 1) * -1
  })]

  level_interco <- marge_inter[, lapply(.SD, function(x) {
    sign(x) * 10
  })]

  code_scenario <- level_interco + level_seule

  # Scenario Rouge : Les deux marges sont negatives
  code_scenario_rouge <- code_scenario[, lapply(.SD, function(x) {
    if (any(num_equal(x, -9))) {
      stop("Attention il y a un sc\u00e9nario avec la marge inter < 0 et la marge seule > 0")
    } else {
      num_equal(x, -11) * 1
    }
  })]
  code_scenario_rouge[, .id := seq_len(.N)]
  code_scenario_rouge <- melt(data = code_scenario_rouge, id.vars = ".id")
  code_scenario_rouge <- code_scenario_rouge[, list(freq = sum(value) / length(value) * 100), by = .id]
  freq_rouge <- unlist(code_scenario_rouge[, freq], use.names = FALSE)

  # Scenario Marron : Marge pays seul negative, et marge pays interconnecte egal a zero
  code_scenario_marron <- code_scenario[, lapply(.SD, function(x) {
    num_equal(x, -1) * 1
  })]
  code_scenario_marron[, .id := seq_len(.N)]
  code_scenario_marron <- melt(data = code_scenario_marron, id.vars = ".id")
  code_scenario_marron <- code_scenario_marron[, list(freq = sum(value) / length(value) * 100), by = .id]
  freq_marron <- unlist(code_scenario_marron[, freq], use.names = FALSE)

  # Scenario Orange : Marge pays seul positive, et marge pays interconnecte egal a zero
  code_scenario_orange <- code_scenario[, lapply(.SD, function(x) {
    num_equal(x, 1) * 1
  })]
  code_scenario_orange[, .id := seq_len(.N)]
  code_scenario_orange <- melt(data = code_scenario_orange, id.vars = ".id")
  code_scenario_orange <- code_scenario_orange[, list(freq = sum(value) / length(value) * 100), by = .id]
  freq_orange <- unlist(code_scenario_orange[, freq], use.names = FALSE)

  # Scenario Jaune : Marge pays seul negative, et marge pays interconnecte positive
  code_scenario_jaune <- code_scenario[, lapply(.SD, function(x) {
    num_equal(x, 9) * 1
  })]
  code_scenario_jaune[, .id := seq_len(.N)]
  code_scenario_jaune <- melt(data = code_scenario_jaune, id.vars = ".id")
  code_scenario_jaune <- code_scenario_jaune[, list(freq = sum(value) / length(value) * 100), by = .id]
  freq_jaune <- unlist(code_scenario_jaune[, freq], use.names = FALSE)

  # Scenario Rouge : Les deux marges sont positives
  code_scenario_vert <- code_scenario[, lapply(.SD, function(x) {
    num_equal(x, 11) * 1
  })]
  code_scenario_vert[, .id := seq_len(.N)]
  code_scenario_vert <- melt(data = code_scenario_vert, id.vars = ".id")
  code_scenario_vert <- code_scenario_vert[, list(freq = sum(value) / length(value) * 100), by = .id]
  freq_vert <- unlist(code_scenario_vert[, freq], use.names = FALSE)

  code_scenario_couleur_dy <- data.table(
    DATE_UTC = date,
    GREEN = freq_vert,
    YELLOW = freq_jaune,
    ORANGE = freq_orange,
    BROWN = freq_marron,
    RED = freq_rouge
  )

  graph <- dygraph(code_scenario_couleur_dy, main = paste0("Remaining capacity ", toupper(area))) %>%
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
