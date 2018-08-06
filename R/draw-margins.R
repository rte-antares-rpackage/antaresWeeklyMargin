
#' Draw remaining capacity line chart
#'
#' @param data_margin a \code{data.table}, first column must be the datetime,
#'  the others results of Monte-Carlo simulations.
#' @param area Name of the area, used in chart title.
#' @param nb_MC Number of Monte-Carlo years to draw.
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
#' # TODO
#'
#' }
draw_margins <- function(data_margin,
                         area = "fr",
                         nb_MC = ncol(data_margin) - 1,
                         num_week = NULL) {
  
  data_margin <- copy(data_margin)
  
  #Calcul des differentes percentiles
  centil1  <- give_percentile(data_margin, nb_MC, 1)
  centil4  <- give_percentile(data_margin, nb_MC, 4)
  centil10 <- give_percentile(data_margin, nb_MC, 10)
  centil50 <- give_percentile(data_margin, nb_MC, 50)
  
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
    dyLegend(show="always") %>%
    dyHighlight(highlightCircleSize = 3)%>%
    dyLegend(show = "always")%>%
    dyCSS(css = system.file('www/css_dygraph.css', package = 'antaresWeeklyMargin'))%>%
    dySeries("PERCENTIL_1", strokeWidth = 2) %>%
    dySeries("PERCENTIL_4", strokeWidth = 2) %>%
    dySeries("PERCENTIL_10", strokeWidth = 2) %>%
    dySeries("MEDIAN", strokeWidth = 2) %>%
    #dySeries("FRC", strokeWidth = 2) %>%
    # dySeries("EFFECTIVE_FRC", strokeWidth = 2) %>%
    dyOptions(colors = pal_couleurs) %>%
    dyOptions(useDataTimezone = TRUE)  %>%
    dyAxis("y", label = "MW") %>%
    dyLimit(limit = 0, color = "red")
  
  #Pour montrer le graphique sur la fênetre VIEWER
  return(graph_margin)
}





######################################################################################################
#Fonction qui determine un centil donne de marges hebdomadaires, calcules a partir de scenarios MC
#Il faut le preciser quel type de marge on considere (seul ou interconnecte), le nombre des MC_years et le centil
#
#Il retourne une chronique correspondant au centil des marges
######################################################################################################

give_percentile <- function(margin, nb_MC, num_centil) { #margin sous forme d'un data.table dont la 1ere colonne est la date et les colonne ssuivantes les marges.
  
  #Calcul de centil inferieur et superieur
  centil_inf <- floor(num_centil*nb_MC/100)
  centil_sup <- ceiling(num_centil*nb_MC/100)
  
  #Determiner la chronique correspondant au centil donne
  percent <- sapply(1:168, function(x){
    temp <- sapply(1:nb_MC, function(y){margin[[x,y+1]]})
    inf <- sort(temp, partial = centil_inf)[centil_inf]
    sup <- sort(temp, partial = centil_sup)[centil_sup]
    inf + (nb_MC/100-floor(nb_MC/100))*(sup-inf)})
  
  return(percent)
}
