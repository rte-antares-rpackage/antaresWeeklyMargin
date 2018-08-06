
#' Draw remaining capacity line chart
#'
#' @param data_margin a \code{data.table}, first column must be the datetime,
#'  the others results of Monte-Carlo simulations.
#' @param area Name of the area, used in chart title.
#' @param num_week Week number to be displayed in chart title.
#'
#' @return a \code{dygraphs} htmlwidget.
#' @export
#'
#' @importFrom dygraphs dygraph dyRangeSelector dyLegend dyHighlight dyCSS
#'  dySeries dyOptions dyAxis dyLimit %>%
#' @importFrom data.table := as.xts.data.table
#'
#' @examples
#' \dontrun{
#'
#' library(antaresRead)
#' library(antaresWeeklyMargin)
#' 
#' setSimulationPath("path/to/sim")
#' 
#' 
#' up <- compute_margins(
#'   date = "2018-02-28", 
#'   area = "fr", 
#'   mcYears = 1:100,
#'   margin = "upward"
#' )
#' 
#' # Marges seules
#' draw_margins(data_margin = up$margin_area_solo, area = "fr")
#' 
#' # Marges interco
#' draw_margins(data_margin = up$margin_area_inter, area = "fr")
#'
#' }
draw_margins <- function(data_margin,
                         area = "fr",
                         num_week = NULL) {
  
  data_margin <- copy(data_margin)
  nb_MC <- ncol(data_margin) - 1
  nb_MC <- copy(nb_MC)
  
  #Calcul des differentes percentiles
  centil1  <- apply(
    X = data_margin[, -1],
    MARGIN = 1,
    FUN = quantile, probs = 1/100, type = 4
  )
  centil4  <- apply(
    X = data_margin[, -1],
    MARGIN = 1,
    FUN = quantile, probs = 4/100, type = 4
  )
  centil10  <- apply(
    X = data_margin[, -1],
    MARGIN = 1,
    FUN = quantile, probs = 10/100, type = 4
  )
  centil50  <- apply(
    X = data_margin[, -1],
    MARGIN = 1,
    FUN = quantile, probs = 50/100, type = 4
  )

  type_margin <- attr(x = data_margin, which = "margin")
  title <- NULL
  if (!is.null(type_margin)) {
    if (type_margin == "upward.solo") {
      title <- paste("Initial Remaining Capacity", toupper(area))
    }
    if (type_margin == "upward.inter") {
      title <- paste("Final Remaining Capacity", toupper(area))
    }
    if (type_margin == "downward.solo") {
      title <- paste("Initial Remaining Capacity", toupper(area))
    }
    if (type_margin == "downward.inter") {
      title <- paste("Final Remaining Capacity", toupper(area))
    }
  }
  if (!is.null(num_week)) {
    title <- paste(title, paste("Week", num_week), sep = " - ")
  }
  
  #Configuration du graphique si les marges a graphiquer sont les marges area seul
  #Les lignes de code associes a la courbe du realise sont desactivees (commentees)
  data_margin <- data_margin[, `:=`(PERCENTIL_1 = centil1
                                    ,PERCENTIL_4 = centil4
                                    ,PERCENTIL_10 = centil10
                                    ,MEDIAN = centil50
                                    #,FRC = marge_prev$MARGE_INTER_PREV
                                    # ,EFFECTIVE_FRC = marge_rt$MARGE_INTER_RT
  )]
  
  pal_couleurs <- c(
    rep("gray", nb_MC)
    , "red"
    , "orange"
    , "blue"
    , "green"
  )
  
  graph_margin <- dygraph(
    data = as.xts.data.table(data_margin),
    main = title
  ) %>%
    dyRangeSelector() %>%
    dyHighlight(highlightCircleSize = 3)%>%
    dyCSS(css = system.file('www/css_dygraph.css', package = 'antaresWeeklyMargin'))%>%
    dySeries(name = "PERCENTIL_1", strokeWidth = 2) %>%
    dySeries(name = "PERCENTIL_4", strokeWidth = 2) %>%
    dySeries(name = "PERCENTIL_10", strokeWidth = 2) %>%
    dySeries(name = "MEDIAN", strokeWidth = 2) %>%
    dyOptions(colors = pal_couleurs, useDataTimezone = TRUE) %>%
    dyAxis(name = "y", label = "MW") %>%
    dyLimit(limit = 0, color = "red")
  
  return(graph_margin)
}


