


#' @importFrom data.table rbindlist
#' @importFrom xml2 xml_find_all xml_attr
parse_vallee <- function(x) {
  vallee <- xml2::xml_find_all(x = x, xpath = "Vallee")
  vallee <- lapply(X = vallee, FUN = function(x) {
    data.frame(
      vallee = xml2::xml_attr(x, "v"),
      pt = xml2::xml_attr(xml2::xml_find_all(x, "Point"), "v"),
      puissance = xml2::xml_attr(xml2::xml_find_all(x, ".//Puissance"), "v"),
      stringsAsFactors = FALSE
    )
  })
  data.table::rbindlist(vallee)
}


#' Read hydro vallee XML file
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
#' vallee <- read_hydro_vallee("inputs/hydros/")
#'
#' }
read_hydro_vallee <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "Hydrauliques")
  xml <- xml2::read_xml(x = path)
  dates <- get_date_seq(xml)
  vallee <- parse_vallee(xml)
  vallee$puissance <- as.numeric(vallee$puissance)
  vallee <- merge(x = vallee, y = dates, by = "pt")
  data.table::setcolorder(vallee, c("vallee", "pt", "datetime", "date", "puissance"))
  data.table::setorderv(vallee, c("vallee", "datetime"))
  return(vallee)
}



#' @importFrom data.table rbindlist setnames
#' @importFrom stats setNames
#' @importFrom xml2 xml_find_all xml_children xml_attrs xml_name
parse_usine <- function(x) {
  usine <- xml2::xml_find_all(x = x, xpath = "Usine")
  usine <- lapply(X = usine, FUN = function(x) {
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
  usine <- data.table::rbindlist(usine)
  data.table::setnames(usine, "v", "groupe_hydro")
  return(usine)
}



#' Read hydro usine XML file
#'
#' @param path Path to the XML file. Can be left blank, designate an XML
#' file or directory containing XML files. If the path points to a directory,
#' the most recent XML file is read. If the argument is not specified, a dialog
#' box will open to select a directory.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table setcolorder setorderv :=
#' @importFrom xml2 read_xml
#'
#' @examples
#' \dontrun{
#'
#' usine <- read_hydro_usine("inputs/hydros/")
#'
#' }
read_hydro_usine <- function(path) {
  if (missing(path)) {
    path <- choose_path()
  }
  path <- select_file(path, "Hydrauliques")
  xml <- xml2::read_xml(x = path)
  dates <- get_date_seq(xml)
  usine <- parse_usine(xml)
  usine <- merge(x = usine, y = dates, by = "pt")
  usine <- merge(x = usine, y = validation_groupe(), by = "groupe_hydro")
  vars <- c("puis", "rprim", "rsec", "rhyd")
  usine <- usine[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
  data.table::setcolorder(usine, c("vallee", "groupe_hydro", "selection", "pt", "datetime", "date", vars))
  data.table::setorderv(usine, c("vallee", "groupe_hydro", "datetime"))
  return(usine)
}






validation_groupe <- function() {
  groupe_hydro <- c("AIGL8H", "AIGLEH", "ALLEMH", "ALRANH", "ARGE5H", "ARRENH",
                    "ARTIGH", "ASTONH", "AUCUNH", "AUSSOH", "AUZATH", "AUZERH", "B.CHEH",
                    "BAOUSH", "BATHIH", "BERREH", "BEYSSH", "BISSOH", "BOLOZH", "BORD5H",
                    "BORT H", "BRAS5H", "BREILH", "BRILLH", "BROM8H", "BROMMH", "BRUGAH",
                    "BUJALH", "BXMONH", "C.AVRH", "CAMBEH", "CAMP5H", "CARLAH", "CASTIH",
                    "CHAMPH", "CHASTH", "CHATRH", "CHEY6H", "CHEY6P", "CIERPH", "CLAPIH",
                    "CLAVAH", "COCHEH", "COCHEP", "COISEH", "COUE8H", "COUESH", "CROUXH",
                    "CSLNAH", "CTLUSH", "CURBAH", "CUSSEH", "CXDANH", "CXVANH", "D.INFH",
                    "DEVBEH", "EGUZOH", "ENCHAH", "ESCAFH", "ESCO2H", "EYLIEH", "FLEIXH",
                    "FTAN H", "G.MAIH", "G.MAIP", "GEDREH", "GOLINH", "GRANDH", "GRIPPH",
                    "HAUTEH", "HERMIH", "HOSP8H", "HOSP9P", "HOSPFH", "HOSPIH", "JOUQUH",
                    "JOURDH", "L.BALH", "L.BL8H", "L.CE1H", "L.CE2H", "L.OO H", "LAFIGH",
                    "LAMATH", "LANAUH", "LANGLH", "LAPARH", "LARDIH", "LARGUH", "LARTIH",
                    "LIVETH", "LONGEH", "LUZ 1H", "LUZ 2H", "LUZI2H", "M.LARH", "MALLEH",
                    "MANOSH", "MARC5H", "MAREFH", "MEDITH", "MERENH", "MIGOEH", "MOUTIH",
                    "MOUX H", "MRNEIH", "MTAHUH", "MTEYNH", "MTEZIH", "MTEZIP", "MTPEZH",
                    "MTRIGH", "N.USSH", "NENTIH", "NEPESH", "NOUAUH", "ORAISH", "ORELLH",
                    "ORLU H", "OZ   H", "P.BORH", "P.CHAH", "P.CLAH", "P.EYBH", "P.INFH",
                    "P.REIH", "P.SUPH", "P.VEYH", "P.VIZH", "PAGANH", "PINETH", "POMBLH",
                    "PORTFH", "PORTIH", "POUG8H", "POUG9H", "POUG9P", "POUGEH", "PRAD8H",
                    "PRADIH", "PRAG8H", "PRAG9P", "PRAGFH", "PRAGNH", "PTAMAH", "QUINSH",
                    "R.AIGH", "R.MOIH", "RANDEH", "RAVIEH", "REVI5H", "REVI5P", "RHONEH",
                    "RHUE H", "RIETEH", "RIOUPH", "ROBERH", "S.BISH", "S.BISP", "S.MORH",
                    "S.PONH", "S.SABH", "SABARH", "SALELH", "SALIGH", "SALONH", "SARR8H",
                    "SARRAH", "SAUCOH", "SAUS2H", "SISTEH", "SOULCH", "SSCA8H", "SSCANH",
                    "SSCHAH", "SSCOGH", "SSCOMH", "SSCROH", "SSCROP", "SSDALH", "SSEG5H",
                    "SSESTH", "SSGENH", "SSGUIH", "SSMC H", "SSNICH", "SSTU8H", "SSTULH",
                    "TIGNEH", "TRUELH", "TUCOYH", "V.ARCH", "V.BENH", "VERN5H", "VINONH",
                    "VINTRH", "VLAROH", "VLEJOH", "VOUGLH", "VOUGLP", "XTESTX", "YTESTY")
  selection <- c("1", "1", "1", "1", "1", "0", "0", "1", "0", "1", "0", "0",
                 "1", "0", "1", "1", "1", "1", "1", "0", "1", "0", "0", "1", "1",
                 "1", "0", "0", "1", "1", "1", "0", "0", "0", "1", "1", "0", "1",
                 "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1",
                 "1", "0", "0", "1", "1", "0", "0", "0", "0", "0", "0", "0", "1",
                 "1", "1", "1", "1", "0", "0", "1", "1", "1", "1", "1", "1", "1",
                 "0", "1", "0", "0", "1", "1", "0", "1", "0", "1", "1", "1", "0",
                 "1", "1", "1", "1", "0", "0", "1", "1", "0", "1", "1", "1", "0",
                 "1", "1", "0", "1", "1", "1", "1", "1", "1", "0", "0", "0", "0",
                 "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1",
                 "0", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "1", "1",
                 "1", "1", "1", "1", "0", "0", "1", "0", "1", "1", "1", "0", "1",
                 "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1",
                 "1", "1", "1", "0", "0", "0", "1", "1", "1", "1", "1", "0", "1",
                 "1", "0", "1", "0", "1", "1", "1", "1", "1", "0", "1", "0", "1",
                 "1", "0", "1", "0", "1", "1", "0", "0")
  data.frame(
    groupe_hydro = groupe_hydro,
    selection = selection,
    stringsAsFactors = FALSE
  )
}







