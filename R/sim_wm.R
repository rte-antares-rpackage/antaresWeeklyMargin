
#' Setup a WM simulation before running Antares
#'
#' @param date_prev Date of simulation.
#' @param start_prev_hebdo Date of forecasts, with format \code{\%Y-\%m-\%d}.
#' @param path_inputs A \code{list} with path to inputs directories, obtained with \code{path_sim_wm}.
#' @param type_load Forecast to use \code{prevu} or \code{premis}.
#' @param dispo_pump Pumpage availability.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#' 
#' @importFrom data.table fread
#' 
#' @name setup-wm
#'
#' @examples
#' \dontrun{
#' 
#' # todo
#' 
#' }
sim_wm <- function(date_prev, start_prev_hebdo, path_inputs = path_sim_wm(), type_load = "prevu", 
                   dispo_pump = c(3520,3520,3520,3520,3520,3520,3520), opts = antaresRead::simOptions()) {
  
  inputPath <- opts$inputPath
  opts <- clear_wm_study(opts)
  
  startday <- format(as.Date(start_prev_hebdo), format = "%A")
  
  # Add Meteologica forecast
  heure_prev_meteologica <- "00:00:00"
  formet <- read_meteologica2(path = path_inputs$meteologica)
  formet2 <- formet[format(file_date, format = "%F %T") == paste0(date_prev, heure_prev_meteologica)]
  # Create time series
  create_wm_ts(data = formet, start = start_prev_hebdo, opts = opts)
  
  # Add CNES forecast
  opts <- create_wm_load_fr(path = path_inputs$cnes, start = date_prev, start_prev_hebdo = start_prev_hebdo, type = type_load, opts = opts)
  
  sup <- read_planning(path = path_inputs$planning)
  opts <- create_wm_cluster(data = sup, start = start_prev_hebdo, opts = opts)
  
  oa <- read_forfait_oa(path = path_inputs$forfait_oa)
  opts <- create_wm_ror(data = oa, start= date_prev,  startday = startday, opts = opts)
  opts <- create_wm_misc(data = oa, start = start_prev_hebdo, opts = opts)
  
  # NTC
  ntc <- fread(file = path_inputs$ntc)
  opts <- create_wm_ntc(data = ntc, start = start_prev_hebdo, opts = opts, startday = startday)
  
  ntc_tp <- read_ntc(path = path_inputs$ntc_tp)
  opts <- create_wm_ntc_tp(data = ntc_tp, start = start_prev_hebdo, opts = opts)
  
  # HYDRAULIQUE RESERVOIR + STEP
  # dispo_pump_d <- c(3520,3520,3520,3520,3520,3520,3520)
  
  #Add Hydro for France (forecast producteur)
  opts <- create_wm_hydro_fr(
    path_capa_hydro = path_inputs$capa_hydro, 
    path_hydro = path_inputs$hydro, 
    start = start_prev_hebdo, 
    opts = opts, 
    dispo_pump = dispo_pump
  )
  #Add Hydro for other areas
  opts <- create_wm_hydro_areas(start = start_prev_hebdo, opts = opts)
  
  invisible(opts)
}



#' Set path to inputs for WM simulation
#'
#' @param path_dir Main directory where inputs are.
#' @param meteologica Sub-directory containing meteologica files.
#' @param cnes Sub-directory containing CNES files.
#' @param planning Sub-directory containing planning files.
#' @param forfait_oa Sub-directory containing forfait OA files.
#' @param ntc Sub-directory containing NTC files.
#' @param ntc_tp Sub-directory containing NTC transparency files.
#' @param capa_hydro Sub-directory containing Hydraulic capacity transparency files.
#' @param hydro Sub-directory containing hydro files.
#'
#' @return For \code{path_sim_wm} a named \code{list}.
#' @export
#' 
#' @rdname setup-wm
#'
path_sim_wm <- function(path_dir = "inputs",
                        meteologica = "meteologica", 
                        cnes = "cnes", 
                        planning = "planning", 
                        forfait_oa = "forfait_oa",
                        ntc = "ntc",
                        ntc_tp = "ntc_tp",
                        capa_hydro = "capa_hydro",
                        hydro = "hydro") {
  list(
    meteologica = normalizePath(file.path(path_dir, meteologica), mustWork = TRUE),
    cnes = normalizePath(file.path(path_dir, cnes), mustWork = TRUE),
    planning = normalizePath(file.path(path_dir, planning), mustWork = TRUE),
    forfait_oa = normalizePath(file.path(path_dir, forfait_oa), mustWork = TRUE),
    ntc = normalizePath(file.path(path_dir, ntc), mustWork = TRUE),
    ntc_tp = normalizePath(file.path(path_dir, ntc_tp), mustWork = TRUE),
    capa_hydro = normalizePath(file.path(path_dir, capa_hydro), mustWork = TRUE),
    hydro = normalizePath(file.path(path_dir, hydro), mustWork = TRUE)
  )
}




