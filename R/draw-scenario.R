
#' Draw Exchange scenario
#'
#' @param data A \code{data.table}.
#' @param var_value Name of variable with values to represent.
#' @param var_date Name of variable with datetime.
#' @param grid_size Size of discretization grid.
#'
#' @export
#' 
#' @importFrom data.table copy setnames
#' @importFrom ggplot2 ggplot aes_string stat_density_2d stat scale_fill_distiller labs
#'  scale_x_datetime scale_y_continuous geom_line theme_minimal theme element_line geom_ribbon
#' @importFrom stats median
#' @importFrom utils tail
#'
draw_scenario <- function(data, var_value = "somme", var_date = "time", grid_size = 100) {
  data <- copy(data)
  old_vars <- c(var_value, var_date)
  if (!all(old_vars %in% names(data))) {
    stop("var_value or var_date are not valid variable names", call. = FALSE)
  }
  setnames(data, old_vars, c("somme", "time"))
  # seq_breaks <- seq(from = min(data$somme, na.rm = TRUE), to = max(data$somme, na.rm = TRUE), length.out = grid_size)
  seq_breaks <- pretty(data$somme, n = grid_size)
  data[, cut_somme := cut(
    x = somme, 
    breaks = seq_breaks,
    labels = tail(seq_breaks, -1), 
    include.lowest = TRUE
  )]
  data[, cut_somme := as.numeric(as.character(cut_somme))]
  # data_agg <- data[, list(n = .N), by = list(dates = time, values = cut_somme)]
  
  data_stats <- data[, list(
    median = median(somme, na.rm = TRUE),
    minimum = min(somme, na.rm = TRUE),
    maximum = max(somme, na.rm = TRUE)
  ), by = time]
  
  ggplot() + 
    geom_ribbon(
      data = data_stats,
      mapping = aes_string(x = "time", ymin = "minimum", ymax = "maximum"),
      fill = "#4575B4"
    ) +
    stat_density_2d(
      data = data,
      mapping = aes_string(x = "time", y = "cut_somme", fill = "stat(level)"), 
      geom = "polygon", h = 6000
    ) +
    scale_x_datetime(limits = range(data$time) + c(-1/2*60*60*6, 1/2*60*60*6)) +
    scale_y_continuous(limits = range(data$cut_somme, na.rm = TRUE) + c(-2000, 2000)) +
    scale_fill_distiller(palette = "RdYlBu", guide = "none") +
    theme_minimal() +
    theme(
      # panel.background = element_rect(fill = NA),
      panel.ontop = TRUE,
      panel.grid = element_line(color = "black", linetype = "dotted")
    ) +
    geom_ribbon(
      data = data_stats,
      mapping = aes_string(x = "time", ymin = -Inf, ymax = "minimum"),
      inherit.aes = FALSE,
      fill = "white"
    ) +
    geom_ribbon(
      data = data_stats, 
      mapping = aes_string(x = "time", ymin = "maximum", ymax = Inf), 
      inherit.aes = FALSE,
      fill = "white"
    ) + 
    geom_line(
      data = data_stats,
      mapping = aes_string(x = "time", y = "median"),
      size = 1
    ) +
    labs(x = "Time", y = "Values")
}









# ggplot(data = data_agg[n>1]) + 
#   aes_string(x = "dates", y = "values") +
#   stat_density_2d(aes_string(fill = "stat(nlevel)"), geom = "polygon", h = 5500, n = 220) +
#   scale_fill_distiller(palette = "RdYlBu", guide = "none") +
#   scale_x_datetime(
#     limits = range(data_agg$dates, na.rm = TRUE) + c(-1/2*60*60*6, 1/2*60*60*6)
#   ) +
#   scale_y_continuous(
#     limits = range(data_agg$values, na.rm = TRUE) + c(-2000, 2000)
#   ) +
  # geom_line(
  #   data = data_stats,
  #   mapping = aes_string(x = "time", y = "median"),
  #   size = 1
  # ) +
#   theme_minimal() +
#   theme(
#     # panel.background = element_rect(fill = NA),
#     panel.ontop = TRUE,
#     panel.grid = element_line(color = "black", linetype = "dotted")
#   ) +
#   geom_ribbon(
#     data = data_stats, 
#     mapping = aes_string(x = "time", ymin = -5000, ymax = "minimum"),
#     inherit.aes = FALSE,
#     fill = "white"
#   ) +
#   geom_ribbon(
#     data = data_stats, 
#     mapping = aes_string(x = "time", ymin = "maximum", ymax = 15000), 
#     inherit.aes = FALSE,
#     fill = "white"
#   )




