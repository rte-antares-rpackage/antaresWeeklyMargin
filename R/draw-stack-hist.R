
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

  level_seule <- apply(
    X = marge_seule,
    MARGIN = c(1, 2),
    FUN = function(x) {
      if (x<=0) {-1} else {1}
    })

  level_interco <- apply(
    X = marge_inter,
    MARGIN = c(1, 2),
    FUN = function(x) {
      if (x < 0) {
        -10
      } else if ( x == 0) {
        0
      } else {
        10
      }
    })

  code_scenario <- level_interco + level_seule

  # Scenario Rouge : Les deux marges sont negatives
  code_scenario_rouge <- apply(
    X = code_scenario,
    MARGIN = c(1,2),
    FUN = function(x) {
      if (x == -9) {
        stop("Attention il y a un sc\u00e9nario avec la marge inter < 0 et la marge seule > 0")
      } else if(x == -11) {
        1
      } else {
        0
      }
    })
  freq_rouge <- apply(
    X = code_scenario_rouge,
    MARGIN = 1,
    FUN = function(x) {
      sum(x) / ncol(code_scenario) * 100
    })

  # Scenario Marron : Marge pays seul negative, et marge pays interconnecte egal a zero
  code_scenario_marron  <- apply(
    X = code_scenario,
    MARGIN = c(1, 2),
    FUN = function(x) {
      if(x == -1) {
        1
      } else {
        0
      }
    })
  freq_marron <- apply(
    X = code_scenario_marron,
    MARGIN = 1,
    FUN = function(x) {
      sum(x) / ncol(code_scenario) * 100
    }
  )

  # Scenario Orange : Marge pays seul positive, et marge pays interconnecte egal a zero
  code_scenario_orange  <- apply(
    X = code_scenario,
    MARGIN = c(1, 2),
    FUN = function(x) {
      if (x == 1) {
        1
      } else {
        0
      }
    })
  freq_orange <- apply(
    X = code_scenario_orange,
    MARGIN = 1,
    FUN = function(x) {
      sum(x) / ncol(code_scenario) * 100
    })

  # Scenario Jaune : Marge pays seul negative, et marge pays interconnecte positive
  code_scenario_jaune  <- apply(
    X = code_scenario,
    MARGIN = c(1, 2),
    FUN = function(x) {
      if (x == 9) {
        1
      } else {
        0
      }
    })
  freq_jaune <- apply(
    X = code_scenario_jaune,
    MARGIN = 1,
    FUN = function(x) {
      sum(x) / ncol(code_scenario) * 100
    })

  # Scenario Rouge : Les deux marges sont positives
  code_scenario_vert <- apply(
    X = code_scenario,
    MARGIN = c(1, 2),
    FUN = function(x) {
      if (x == 11) {
        1
      } else {
        0
      }
    })
  freq_vert <- apply(
    X = code_scenario_vert,
    MARGIN = 1,
    FUN = function(x) {
      sum(x) / ncol(code_scenario) * 100
    })

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
