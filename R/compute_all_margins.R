
#' @title Compute upward or downward margins
#' 
#' @description From Antares results, calculate upward or downward margins for all areas given a MC year.
#'
#' @param date Date of the study.
#' @param mcYear An MC year.
#' @param exclude Areas to exclude.
#' @param margin Type of margin to compute, \code{upward} or \code{downward}, can be abbreviated.
#' @param virtual_areas List of virtuals areas.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'
#' @return a list of \code{data.table} with 2 slots: \code{areas} and \code{links}.
#' @export
#' 
#' @importFrom antaresRead getAreas getLinks readAntares removeVirtualAreas
#' @importFrom antaresProcessing addLoadFactorLink
#' @importFrom data.table := data.table
#'
#' @examples
#' \dontrun{
#' 
#' # todo
#' 
#' }
compute_all_margins <- function(date, mcYear, exclude = c("lu_be","lu_de"), 
                            margin = c("upward", "downward"),
                            virtual_areas = c("lac","pump_d", "turb_d","pump_w", "turb_w" ),
                            opts = antaresRead::simOptions()) {
  margin <- match.arg(margin)
  
  areas <- getAreas(exclude = exclude, opts = opts)
  links <- getLinks(exclude = exclude, opts = opts)
  
  data_all_areas <- readAntares(
    areas = areas, links = links, 
    mcYears = mcYear, linkCapacity = TRUE,
    mustRun = margin == "downward",
    clusters = if (margin == "downward") areas else NULL
  )
  data_all <- removeVirtualAreas(x = data_all_areas, storageFlexibility = virtual_areas)
  
  if (margin == "upward") {
    data_all$areas[, margin_solo := `AVL DTG`+ storageCapacity +`H. ROR`+`MISC. NDG`+ WIND + SOLAR - LOAD]
    data_all$areas[, margin_inter := margin_solo - BALANCE + `ROW BAL.`]
    
    #Pour corriger les probl?mes d'arrondi dans le calcul de marges
    data_all$areas[-1 <= margin_solo & margin_inter <= 1, margin_inter := 0]
  } else {
    
    # stop("Not implemented yet!", call. = FALSE)
    cluster_areas <- copy(data_all$clusters)
    must_run_all <- cluster_areas[, list(mustRunTotal = sum(mustRunTotal, na.rm = TRUE)), by = list(time, mcYear, area)]
    margin_area <- copy(data_all$areas[, .SD, .SDcols = setdiff(names(data_all$areas), "mustRunTotal")])
    margin_area <- merge(x = margin_area, y = must_run_all, by = c("time", "mcYear", "area"))
    # browser()
    margin_area[, margin_solo := mustRunTotal + `H. ROR`+`MISC. NDG` + WIND + SOLAR - LOAD - (pumpingCapacity + pump_d + pump_w)]
    margin_area[, margin_inter := margin_solo - BALANCE + `ROW BAL.`]
    setorder(x = margin_area, mcYear, time)
    data_all$areas <- margin_area
    
  }
  
  addLoadFactorLink(data_all$links)
  data_all$links[, abs_loadFactor:= abs(loadFactor)]
  
  corr_time <- data.table(
    timeId = seq_len(168),
    time = as.POSIXct(
      format(seq(as.POSIXct(date), by = "+1 hour", length.out = 168)),
      format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"
    )
  )
  
  data_all$areas[, time := NULL]
  data_all$areas <- merge(x = data_all$areas, y = corr_time, by = "timeId")
  
  data_all$links[, time := NULL]
  data_all$links <- merge(x = data_all$links, y = corr_time, by = "timeId")
  
  
  opts <- attr(data_all$links, "opts")
  opts$start <- as.POSIXlt(date, tz = "UTC")
  attr(data_all$links, "opts") <- opts
  
  return(data_all)
}




