
#' Remove Areas & BindingConstraints before Weekly Margin Simulation
#'
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#' 
#' @importFrom antaresEditObject removeArea removeBindingConstraint
#' @importFrom antaresRead getAreas readBindingConstraints
#'
#' @examples
#' \dontrun{
#' 
#' opts <- setSimulationPath(path = "path/to/simulation/", simulation = "input")
#' clear_wm_study(opts)
#' 
#' }
clear_wm_study <- function(opts = simOptions()) {
  
  # clear areas
  area2remove <- getAreas(opts = opts, select = c("dsr", "rs", "ir_", "rrrc", "model_description"), regexpSelect = TRUE)
  if (length(area2remove) < 1) {
    cat("No area to remove\n")
  } else {
    cat(sprintf("Removing %s area(s): \n", length(area2remove)))
    for (area in area2remove){
      opts <- removeArea(name = area, opts = opts)
      cat(sprintf("Area %s successfully removed\n", area))
    }
  }
  
  cat("---\n")
  
  # clear binding constraints
  bc2remove <- names(readBindingConstraints(opts = opts))
  bc2remove <- grep(pattern = "fr_step|_fb|dsr", x = bc2remove, value = TRUE)
  
  if (length(bc2remove) < 1) {
    cat("No binding constraint to remove\n")
  } else {
    cat(sprintf("Removing %s binding constraint(s): \n",length(bc2remove)))
    for (bc in bc2remove){
      opts <- removeBindingConstraint(name = bc, opts = opts)
      cat(sprintf("BindingConstraint %s successfully removed\n", bc))
    }
  }
  
  return(invisible(opts))
}

