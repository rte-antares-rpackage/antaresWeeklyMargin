
#' @importFrom data.table rbindlist setnames
#' @importFrom stats setNames
#' @importFrom xml2 xml_find_all xml_children xml_attrs xml_name
parse_tranche <- function(x) {
  tranche <- xml2::xml_find_all(x = x, xpath = "Tranche")
  tranche <- lapply(X = tranche, FUN = function(x) {
    child_node <- lapply(
      X = xml2::xml_children(x),
      FUN = function(y) {
        pt <- xml2::xml_attrs(y)
        pt <- stats::setNames(pt, tolower(xml2::xml_name(y)))
        children <- xml2::xml_children(y)
        values <- unlist(xml2::xml_attrs(children), use.names = FALSE)
        values <- stats::setNames(values, tolower(xml2::xml_name(children)))
        c(as.list(xml2::xml_attrs(x)), as.list(pt), as.list(values))
      }
    )
    data.table::rbindlist(child_node)
  })
  tranche <- data.table::rbindlist(tranche)
  data.table::setnames(tranche, "v", "tranche")
  return(tranche)
}


#' Read thermal XML file
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table setcolorder setorderv
#' @importFrom xml2 read_xml
#'
#' @examples
#' \dontrun{
#'
#' thermal <- read_thermal_tranche("inputs/thermals/")
#'
#' }
read_thermal_tranche <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "Thermiques")
  xml <- xml2::read_xml(x = path)
  dates <- get_date_seq(xml)
  tranche <- parse_tranche(xml)
  tranche <- merge(x = tranche, y = dates, by = "pt")
  vars <- c("puis", "rprim", "rsec")
  tranche <- tranche[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
  data.table::setcolorder(tranche, c("tranche", "pt", "datetime", "date", vars))
  data.table::setorderv(tranche, c("tranche", "datetime"))
  return(tranche)
}

