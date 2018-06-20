
#' @title Compute upward or downward margins
#' 
#' @description From Antares results, calculate upward or downward margins for an area.
#'
#' @param date Date of the study.
#' @param area Area studied.
#' @param margin Type of margin to compute, \code{upward} or \code{downward}, can be abbreviated.
#' @param virtual_areas List of virtuals areas.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath} 
#'
#' @return a list of \code{data.table} with 3 slots: \code{data_area},
#'  \code{margin_area_solo} and \code{margin_area_inter}.
#' @export
#' 
#' @importFrom antaresRead readAntares getLinks simOptions removeVirtualAreas
#' @importFrom data.table dcast data.table :=
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
  links_virtual_area <- links_virtual_area[links_virtual_area %in% getLinks(areas = area, opts = opts)]
  
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
  if (length(links_virtual_area) > 0) {
    data_area <- removeVirtualAreas(x = data_area, storageFlexibility = virtual_areas)$areas
  }
  
  if (margin == "upward") {
    margin_area <- data_area[, margin_solo := `AVL DTG`+ storageCapacity +`H. ROR`+`MISC. NDG`+ WIND + SOLAR - LOAD]
    margin_area <- data_area[, margin_inter := margin_solo - BALANCE + `ROW BAL.`]
  } else {
    
    # TODO
    
  }
  
  start_date <- as.POSIXct(paste(date, "00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris")
  new_time <- data.table(
    datetime = seq.POSIXt(from = start_date, by = "+1 hour", length.out = 168)
  )
  margin_area_solo_peryear <- data.table(
    datetime = new_time$datetime, 
    mc_year = margin_area$mcYear, 
    margin_area_solo = margin_area$margin_solo
  )
  margin_area_solo <- dcast(
    data = margin_area_solo_peryear, 
    formula = datetime ~ mc_year, 
    value.var = "margin_area_solo"
  )
  
  margin_area_inter_peryear <- data.table(
    datetime = new_time$datetime, 
    mc_year = margin_area$mcYear, 
    margin_area_inter = margin_area$margin_inter
  )
  margin_area_inter <- dcast(
    data = margin_area_inter_peryear, 
    formula = datetime ~ mc_year, 
    value.var = "margin_area_inter"
  )
  
  corr_time <- data.table(
    timeId = seq_len(168),
    time = as.POSIXct(
      format(seq(as.POSIXct(date), by = "+1 hour", length.out = 168)),
      format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"
    )
  )
  data_area[, time := NULL]
  data_area <- merge(x = data_area, y = corr_time, by = "timeId")
  
  list(
    data_area = data_area,
    margin_area_solo = margin_area_solo,
    margin_area_inter = margin_area_inter
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
