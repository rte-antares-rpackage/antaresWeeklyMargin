
#' Setup a WM simulation before running Antares
#'
#' @param date_prev Date of simulation.
#' @param start_prev_hebdo Date of forecasts, with format \code{\%Y-\%m-\%d}.
#' @param path_inputs A \code{list} with path to inputs directories, obtained with \code{path_sim_wm}.
#' @param n_mcyears Number of MC years in the study, default to \code{2040}.
#' @param type_load Forecast to use \code{prevu} or \code{premis}.
#' @param dispo_pump Pumpage availability.
#' @param simulation_source Path to source simulation for creating Hydro for other areas.
#'  If provided a copy of this simulation will be performed.
#' @param simulation_dest Name of the directory where to copy the study. Warning: content of directory will be deleted!
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#' 
#' @importFrom data.table fread
#' @importFrom lubridate wday
#' @importFrom antaresEditObject updateGeneralSettings updateOptimizationSettings
#' 
#' @name setup-wm
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
sim_wm <- function(date_prev, start_prev_hebdo, 
                   path_inputs = path_sim_wm(), 
                   n_mcyears = 2040,
                   type_load = "prevu", 
                   dispo_pump = c(3520, 3520, 3520, 3520, 3520, 3520, 3520),
                   simulation_source = NULL,
                   simulation_dest = NULL,
                   opts = antaresRead::simOptions()) {
  
  if (!is.null(simulation_source)) {
    cat(info_text("Copying study"))
    new_path <- copy_sim_wm(path_sim = simulation_source, dir_dest = simulation_dest)
    opts <- setSimulationPath(path = new_path)
  }
  
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
  opts <- create_wm_ts(data = formet, start = start_prev_hebdo, opts = opts)
  
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
  if (!is.null(simulation_source)) {
    cat(info_text("Create Hydro areas"))
    opts <- create_wm_hydro_areas(start = start_prev_hebdo, simulation_source = simulation_source, opts = opts)
  }
  
  
  # General settings
  first.weekday <- wday(x = start_prev_hebdo, label = TRUE, abbr = FALSE, locale = "English")
  first.weekday <- as.character(first.weekday)
  cat(info_text("Updating study's settings"))
  opts <- updateGeneralSettings(
    nbyears = n_mcyears, 
    simulation.end = 7, 
    year.by.year = TRUE, 
    opts = opts, 
    first.weekday = first.weekday,
    january.1st = "Monday", 
    leapyear = FALSE,
    intra.modal = "No"
  )
  
  opts <- updateOptimizationSettings(number.of.cores.mode = "maximum", opts = opts)
  
  
  # Scenario builder
  cat(info_text("Updating scenario builder"))
  update_sb(n_mcyears, opts)
  
  
  cat(info_text("Finish!"))
  cat("Path to sudy:", opts$studyPath, "\n")
  
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



# Utility to copy whole simulation into new directory
copy_sim_wm <- function(path_sim = NULL, dir_dest = NULL) {
  if (is.null(path_sim))
    return(invisible())
  path_sim <- normalizePath(path = path_sim, mustWork = TRUE)
  dir_sim <- dirname(path = path_sim)
  if (is.null(dir_dest)) {
    dest_sim <- file.path(dir_sim, paste0(basename(path_sim), "_WM_", format(Sys.time(), "%Y%m%d%H%M")))
  } else {
    dest_sim <- file.path(dir_sim, dir_dest)
  }
  if (!dir.exists(dest_sim)) {
    dir.create(path = dest_sim)
  } else {
    unlink(x = list.files(path = dest_sim, full.names = TRUE), recursive = TRUE)
  }
  cat("Simulation copied here:", dest_sim, "\n")
  res_copy <- file.copy(
    from = list.files(path = path_sim, full.names = TRUE),
    to = dest_sim,
    recursive = TRUE
  )
  return(dest_sim)
}







#' @importFrom antaresEditObject scenarioBuilder updateScenarioBuilder
update_sb <- function(n_mc, opts) {
  
  # LOAD
  sbuilder_load <- scenarioBuilder(
    n_scenario = 51,
    n_mc = n_mc,
    areas_rand = c("lu_be","lu_de", "pump_d","pump_w", "turb_d","turb_w", "lac", "fr"), 
    opts = opts
  )
  updateScenarioBuilder(
    ldata = sbuilder_load, 
    series = "load", 
    opts = opts
  )
  
  # WIND
  sbuilder_wind <- scenarioBuilder(
    n_scenario = 51,
    n_mc = n_mc,
    areas_rand = c("lu_be", "lu_de", "pump_d", "pump_w", "turb_d", "turb_w", "lac", "ch"), 
    opts = opts
  )
  updateScenarioBuilder(
    ldata = sbuilder_wind, 
    series = "wind", 
    opts = opts
  )
  
  # SOLAR
  sbuilder_solar <- scenarioBuilder(
    n_scenario = 51,
    n_mc = n_mc,
    areas_rand = c("lu_be","lu_de", "pump_d", "pump_w", "turb_d", "turb_w", "lac", "ie", "ni"), 
    opts = opts
  )
  updateScenarioBuilder(
    ldata = sbuilder_solar, 
    series = "solar", 
    opts = opts
  )
}








