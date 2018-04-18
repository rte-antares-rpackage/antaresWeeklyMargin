
#' Create Misc-gen for Weekly Margins simulation
#'
#' @param data a \code{data.table} obtained from \code{\link{read_forfait_oa}}.
#' @param start If specified, data will be filtered from given date to 7 days after.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table copy as.data.table := setnames
#' @importFrom antaresRead simOptions
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'
#' # set path to your simulation
#' opts <- setSimulationPath(path = "path/to/simulation/", simulation = "input")
#'
#' # Read OA files
#' oa <- read_forfait_oa(path = "path/to/hydro_forfait/")
#'
#' # Create ROR series in Antares
#' create_wm_misc(data = oa, start = "2018-01-04")
#'
#' }
create_wm_misc <- function(data, start = NULL, opts = antaresRead::simOptions()) {

  cat("Writing MISC-GEN for fr...")
  inputPath <- opts$inputPath

  if (!is.null(start)) {
    start <- as.Date(start)
    data <- copy(data)
    data <- data[as.Date(date_heure, tz = "Europe/Paris") >= start]
    data <- data[as.Date(date_heure, tz = "Europe/Paris") < start + 7]
  }

  #MISCGEN
  misc <- data[, !c("date", "heure", "hydraulique_tiers", "eolien", "photovoltaique", "hydraulique_edf")]

  misc <- misc[ , CHP := cogeneration_mdse_dispatchable_ + cogeneration_continue]

  matrix_misc <- as.data.table(matrix(data = c(rep(0, 8760*8)), ncol = 8))
  setnames(
    x = matrix_misc,
    old = names(matrix_misc),
    new = c("CHP", "BioMass", "BioGaz","Waste", "GeoThermal", "Other", "PSP", "ROWBalance")
  )
  matrix_misc[1:168, CHP := misc$CHP]
  matrix_misc[1:168, Waste := misc$incineration]
  matrix_misc[1:168, Other := misc$autres]

  write.table(
    x = matrix_misc, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = paste0(inputPath, "/misc-gen/miscgen-fr.txt")
  )
  cat("\rWriting MISC-GEN for fr - Done!\n")
  return(invisible())
}


