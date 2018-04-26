
#' Process Monotone Data
#'
#' @param start Starting date
#' @param date Date desired
#' @param area Area
#' @param nb_MC Monte-Carlo year to look for
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a list of \code{data.table}
#' @export
#'
#' @importFrom antaresRead getLinks readAntares
#' @importFrom data.table data.table dcast.data.table as.data.table
#'
#' @examples
#' \dontrun{
#'
#' # todo
#'
#' }
process_data_mono <- function(start = "2016-11-05", date = "2016-11-10 17:00:00", area = "fr", nb_MC = 2040,
                      opts = antaresRead::simOptions()) {

  date_debut <- as.POSIXct(paste0(start," 00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  new_time <- data.table(
    DateTime = seq(date_debut, by = "1 hour", length.out = 168)
  )

  links_fr <- getLinks(
    areas = area,
    exclude = c("lac", "pump_d", "turb_d", "pump_w", "turb_w"),
    opts = opts
  )


  # Extraire les donnees de sortie d'ANTARES pour chaque pays
  data_pays <- readAntares(
    areas = area,
    links = links_fr,
    select = "FLOW LIN.",
    mcYears = "all", linkCapacity = TRUE, opts = opts
  )

  pays_links <- data_pays$links
  pays_links <- pays_links[order(mcYear), ]
  pays_links$time <- new_time$DateTime

  flux <- pays_links[, .SD, .SDcols = c("time", "mcYear", "link", "FLOW LIN.")]


  date_etude <- as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  flux_etude <- flux[time == date_etude, c("link", "mcYear","FLOW LIN.")]
  flux_etude <- dcast.data.table(flux_etude, mcYear ~ link, value.var = "FLOW LIN.")
  num_row <- round((1:nrow(flux_etude)) / nrow(flux_etude)*100, 1)

  # grepl(pattern = paste0("fr", "$"), x = links) * -2 + 1
  links_fr <- as.character(links_fr)
  res <- lapply(
    X = links_fr,
    FUN = function(x) {
      coef <- grepl(pattern = paste0(area, "$"), x = x) * -2 + 1
      area_flux <- apply(X = flux_etude[, .SD, .SDcols = x], MARGIN = 1, FUN = sum) * coef
      area_flux <- area_flux[order(area_flux, decreasing = TRUE)]

      res <- as.data.table(cbind(num_row, area_flux))
    }
  )
  mono_n <- gsub(pattern = area, replacement = "", x = links_fr)
  mono_n <- gsub(pattern = " - ", replacement = "", x = mono_n)
  names(res) <- paste0("mono_", mono_n)

  if (all(c("be - fr", "de - fr") %in% links_fr)) {
    cwe_flux <- apply(flux_etude[, c("be - fr", "de - fr")], 1, sum) * -1
    cwe_flux <- cwe_flux[order(cwe_flux, decreasing = TRUE)]

    res$mono_cwe <- as.data.table(cbind(num_row,cwe_flux))
  }

  areas_indirect <- c("be - fr", "de - fr", "ch - fr", "es - fr")
  areas_direct <- c("fr - gb", "fr - it")
  if (all(areas_indirect %in% links_fr) & all(areas_direct %in% links_fr)) {
    flux_ind <- apply(X = flux_etude[, .SD, .SDcols = areas_indirect], MARGIN = 1, FUN = sum)
    flux_dir <- apply(X = flux_etude[, .SD, .SDcols = areas_direct], MARGIN = 1, FUN = sum)

    flux_total <- flux_dir + flux_ind * -1
    flux_total <- flux_total[order(flux_total, decreasing = TRUE)]
    res$mono_france <- as.data.table(cbind(num_row, flux_total))
  }

  return(res)
}



