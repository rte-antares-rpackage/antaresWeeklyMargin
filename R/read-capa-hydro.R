

#' Read Hydraulic Capacity
#'
#' @param path Path to the CSV file. If the argument is not specified, a dialog
#' box will open to select a directory.
#'
#' @return A \code{numeric}.
#' @export
#' 
#' @importFrom data.table fread
#'
#' @examples
#' \dontrun{
#' 
#' read_capa_hydro(path = "capa_hydro/PH_RTEHEB_2018_S17_2.csv")
#' read_capa_hydro(path = "capa_hydro/PH_RTEHEB_2018_S09_5.csv")
#' 
#' }
#' 
read_capa_hydro <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, pattern = "", multiple = FALSE, fileext = "\\csv$")
  capa_hydro <- fread(file = path)
  capa_hydro <- unique(x = capa_hydro, by = c("Region", "PValleeMobilisable"))
  capa_hydro[, sum(PValleeMobilisable, na.rm = TRUE)]
}
