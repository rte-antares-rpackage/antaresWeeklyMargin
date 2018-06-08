
#' @title Compute upward or downward margins
#' 
#' @description From Antares results, calculate upward or downward margins for an area.
#'
#' @param date Date of the study.
#' @param area Area studied.
#' @param margin Type of margin to compute, \code{upward} or \code{downward}, can ba abbreviated.
#' @param virtual_areas List of virtuals areas.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath} 
#'
#' @return a list of \code{data.table}
#' @export
#' 
#' @importFrom antaresRead readAntares getLinks simOptions
#'
#' @examples
#' \dontrun{
#' 
#' # todo
#' 
#' }
compute_margins <- function(date, area = "fr", 
                            margin = c("upward", "downward"),
                            virtual_areas = c("lac","pump_d", "turb_d","pump_w", "turb_w" ),
                            opts = antaresRead::simOptions()) {
  margin <- match.arg(margin)
  links <- getLinks(areas = area, exclude = virtual_areas, opts = opts)
  links_virtual_area <- make_links(area, virtual_areas)
  data_area <- readAntares(
    areas = area, 
    links = links_virtual_area,
    select = c("FLOW LIN.", "AVL DTG", "MISC. NDG", "H. ROR", "WIND", "SOLAR", "LOAD",
               "MISC. DTG", "BALANCE", "NUCLEAR", "GAS", "COAL", "LIGNITE", "OIL",
               "MIX. FUEL", "ROW BAL.", "FLOW LIN.", "LOLD", "LOLP", "UNSP. ENRG"), 
    mcYears = "all", 
    linkCapacity = length(links_virtual_area) > 0,
    opts = opts
  )
}


make_links <- function(x, y) {
  if (is.null(x) | is.null(y))
    return(NULL)
  exg <- expand.grid(x = x, y = y, stringsAsFactors = FALSE)
  mapply(
    FUN = function(x, y) {
      paste(sort(c(x, y)), collapse = " - ")
    },
    x = exg$x, y = exg$y, 
    USE.NAMES = FALSE
  )
}
