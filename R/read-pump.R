
#' Read hydraulic pump files
#'
#' @param path Path to a directory containing Excel files, both PE & PE file must exist.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table setnames melt rbindlist %chin%
#' @importFrom stats setNames
#'
read_pump <- function(path) {
  files <- select_file(
    path = path,
    pattern = "PDIH_RTE_",
    fileext = "xls",
    multiple = TRUE,
    verbose = FALSE
  )
  files_po <- grep(pattern = "PDIH_RTE_PO", x = files, value = TRUE)
  files_po <- sort(files_po, decreasing = TRUE)[1]
  message(paste("Reading file:", files_po, "\n"))
  files_pe <- grep(pattern = "PDIH_RTE_PE", x = files, value = TRUE)
  files_pe <- sort(files_pe, decreasing = TRUE)[1]
  message(paste("Reading file:", files_pe, "\n"))
  hydro_pump <- lapply(
    X = c(files_po, files_pe),
    FUN = function(path) {
      dat <- read_excel(path = path, skip = 4)
      dat <- as.data.table(dat)
      setnames(x = dat, old = names(dat), new = clean_names(names(dat)))
      dat <- melt(
        data = dat,
        id.vars = c("ouvrage", "gpe", "debut", "heure_debut", "fin", "heure_fin", "type", "cause"),
        measure.vars = grep(pattern = "^[0-9]*$", x = names(dat), value = TRUE),
        variable.name = "date", variable.factor = FALSE
      )
      dat[, date := as.Date(x = as.numeric(date), origin = "1900-01-01") - 2]
      return(dat)
    }
  )
  hydro_pump <- rbindlist(l = setNames(hydro_pump, c("PO", "PE")), idcol = TRUE)
  hydro_pump[ouvrage %chin% c("S.BISP", "CHEY6P", "G.MAIP", "REVI5P", "MTEZIP", "COCHEP")]
}
