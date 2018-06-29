
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
  startday <- format(as.Date(start_prev_hebdo), format = "%A")
  
  cat(info_text("Cleaning study"))
  opts <- clear_wm_study(opts)
  
  # Add Meteologica forecast
  cat(info_text("Create Time Series"))
  heure_prev_meteologica <- "00:00:00"
  formet <- read_meteologica2(path = path_inputs$meteologica, date = date_prev, time = "00")
  formet2 <- formet[format(file_date, format = "%F %T") == paste0(date_prev, heure_prev_meteologica)]
  # Create time series
  create_wm_ts(data = formet, start = start_prev_hebdo, opts = opts)
  
  # Add CNES forecast
  cat(info_text("Create Load FR"))
  opts <- create_wm_load_fr(path = path_inputs$cnes, start = date_prev, start_prev_hebdo = start_prev_hebdo, type = type_load, opts = opts)
  
  cat(info_text("Create Clusters"))
  sup <- read_planning(path = path_inputs$planning)
  opts <- create_wm_cluster(data = sup, start = start_prev_hebdo, opts = opts)
  
  cat(info_text("Create ROR"))
  oa <- read_forfait_oa(path = path_inputs$forfait_oa)
  opts <- create_wm_ror(data = oa, start= date_prev,  startday = startday, opts = opts)
  
  cat(info_text("Create MISC"))
  opts <- create_wm_misc(data = oa, start = start_prev_hebdo, opts = opts)
  
  # NTC
  cat(info_text("Create NTC"))
  ntc <- fread(file = list.files(path = path_inputs$ntc, full.names = TRUE))
  opts <- create_wm_ntc(data = ntc, start = start_prev_hebdo, opts = opts, startday = startday, force_date = TRUE)
  
  cat(info_text("Create NTC TP"))
  ntc_tp <- read_ntc(path = path_inputs$ntc_tp)
  opts <- create_wm_ntc_tp(data = ntc_tp, start = start_prev_hebdo, opts = opts)
  
  # HYDRAULIQUE RESERVOIR + STEP
  # dispo_pump_d <- c(3520,3520,3520,3520,3520,3520,3520)
  
  #Add Hydro for France (forecast producteur)
  cat(info_text("Create Hydro FR"))
  opts <- create_wm_hydro_fr(
    path_capa_hydro = path_inputs$capa_hydro, 
    path_hydro = path_inputs$hydro, 
    start = start_prev_hebdo, 
    opts = opts, 
    dispo_pump = dispo_pump
  )
  #Add Hydro for other areas
  cat(info_text("Create Hydro areas"))
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
#' @importFrom stats setNames
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
  paths <- list(
    meteologica = meteologica,
    cnes = cnes,
    planning = planning,
    forfait_oa = forfait_oa,
    ntc = ntc,
    ntc_tp = ntc_tp,
    capa_hydro = capa_hydro,
    hydro = hydro
  )
  lapply(
    X = setNames(paths, names(paths)),
    FUN = function(x) {
      if ("AsIs" %in% class(x)) {
        x
      } else {
        normalizePath(file.path(path_dir, x), mustWork = TRUE)
      }
    }
  )
}



#' @param ... Character vectors indicating path to a directory,
#'  use this to specify a path outside the \code{"path_dir"} directory.
#' 
#' @export
#'
#' @rdname setup-wm
force_path <- function(...) {
  I(normalizePath(file.path(...), mustWork = TRUE))
}

