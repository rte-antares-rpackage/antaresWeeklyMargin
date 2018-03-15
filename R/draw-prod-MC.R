#' Draw Stack Production for a MC Year
#'
#' @param data An object of class \code{antaresDataTable}.
#' @param area Area for which the graph is drawn
#' @param mc_year Monte-Carlo year to use.
#' @param date_i A new initial date to replace the original date in the data.
#'
#' @export
#'
#' @importFrom antaresViz prodStack
#'
#' @examples
#' \dontrun{
#'
#' draw_prod_MC(data, "fr", 645)
#'
#' }
draw_prod_MC <- function(data, area = NULL, mc_year = NULL, date_i = NULL){

  stopifnot("antaresDataTable" %in% class(data))

  data_mc <- copy(data)

  if (is.null(area))
    area <- levels(data$area)[1]
  area_ <- area
  data_mc <- data_mc[area == area_]

  set_prod_config(area)

  if (is.null(mc_year))
    mc_year <- data$mcYear[1]

  data_mc <- data[mcYear == mc_year]

  if (!is.null(date_i)) {
    # date_debut <- as.POSIXct(paste0(date_i," 00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    # new_time <- seq.POSIXt(date_debut, by = "hour", length.out = nrow(data_mc))
    # data_mc[, time := new_time]
    start.old <- attr(data_mc, "opts")$start
    attr(data_mc, "opts")$start <- as.POSIXlt(date_i, tz = "UTC")
    on.exit({attr(data_mc, "opts")$start <- start.old})
  }

  prodStack(
    x = data_mc, stack = paste0("prod_", area), areas = area,
    main = paste("Production", toupper(area), "MC year =", mc_year),
    # main = paste0("Production ", toupper(pays), "  - Deterministic Forecast"),
    interactive = FALSE
  )
}



#' @importFrom antaresViz setProdStackAlias
set_prod_config <- function(area) {

  if (area == "be") {
    # Configuration BE
    antaresViz::setProdStackAlias(
      "prod_be",
      variables = alist(
        "pumpedStorage" = pump_d + turb_d,
        "import/export" = - BALANCE + `ROW BAL.`,
        "bioenergy" = `MISC. NDG`,
        "wind" = WIND,
        "solar" = SOLAR,
        "nuclear" = NUCLEAR,
        "hydraulic" = `H. ROR`,
        "gas" = GAS,
        "coal" = COAL,
        "lignite" = LIGNITE,
        "oil" = OIL,
        "other" = `MISC. DTG` + `MIX. FUEL`
      ),
      colors = c("#1147B9", "#969696", "#166A57", "#74CDB9", "#F27406", "#F5B300", "#2772B2", "#F30A0A", "#AC8C35", "#B4822B", "#8356A2", "#ADFF2F"),
      lines = alist(
        "totalLoad" = LOAD + pmax(0, -(pump_d + turb_d)),
        "totalProduction" = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG` + WIND + SOLAR + `H. ROR` + `MISC. NDG` + pmax(0, pump_d + turb_d)
      ),
      lineColors = c("#4C4E70", "#EB9BA6")
    )
  } else if (area == "fr") {
    # Configuration FR
    antaresViz::setProdStackAlias(
      "prod_fr",
      variables = alist(
        "pumpedStorage" = pump_d + turb_d + pump_w + turb_w,
        "import/export" = - BALANCE + `ROW BAL.`,
        "bioenergy" = `MISC. NDG`,
        "wind" = WIND,
        "solar" = SOLAR,
        "nuclear" = NUCLEAR,
        "hydraulic" = `H. ROR` + lac ,
        "gas" = GAS,
        "coal" = COAL,
        "lignite" = LIGNITE,
        "oil" = OIL,
        "other" = `MISC. DTG` + `MIX. FUEL`
      ),
      colors = c("#1147B9", "#969696", "#166A57", "#74CDB9", "#F27406", "#F5B300", "#2772B2", "#F30A0A", "#AC8C35", "#B4822B", "#8356A2", "#ADFF2F"),
      lines = alist(
        "totalLoad" = LOAD + pmax(0, -(pump_d + turb_d + pump_w + turb_w)),
        "totalProduction" = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG` + WIND + SOLAR + `H. ROR` + lac +`MISC. NDG` + pmax(0, pump_d + turb_d + pump_w + turb_w)
      ),
      lineColors = c("#4C4E70", "#EB9BA6")
    )
  } else if (area == "de") {
    # Configuration DE
    antaresViz::setProdStackAlias(
      "prod_de",
      variables = alist(
        "pumpedStorage" = pump_d + turb_d,
        "import/export" = - BALANCE + `ROW BAL.`,
        "bioenergy" = `MISC. NDG`,
        "wind" = WIND,
        "solar" = SOLAR,
        "nuclear" = NUCLEAR,
        "hydraulic" = `H. ROR` + lac ,
        "gas" = GAS,
        "coal" = COAL,
        "lignite" = LIGNITE,
        "oil" = OIL,
        "other" = `MISC. DTG` + `MIX. FUEL`
      ),
      colors = c("#1147B9", "#969696", "#166A57", "#74CDB9", "#F27406", "#F5B300", "#2772B2", "#F30A0A", "#AC8C35", "#B4822B", "#8356A2", "#ADFF2F"),
      lines = alist(
        "totalLoad" = LOAD + pmax(0, -(pump_d + turb_d)),
        "totalProduction" = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG` + WIND + SOLAR + `H. ROR` + lac +`MISC. NDG` + pmax(0, pump_d + turb_d)
      ),
      lineColors = c("#4C4E70", "#EB9BA6")
    )
  } else if (area == "nl") {
    # Configuration NL
    antaresViz::setProdStackAlias(
      "prod_nl",
      variables = alist(
        "import/export" = - BALANCE + `ROW BAL.`,
        "bioenergy" = `MISC. NDG`,
        "wind" = WIND,
        "solar" = SOLAR,
        "nuclear" = NUCLEAR,
        "gas" = GAS,
        "coal" = COAL,
        "lignite" = LIGNITE,
        "oil" = OIL,
        "other" = `MISC. DTG` + `MIX. FUEL`
      ),
      colors = c("#969696", "#166A57", "#74CDB9", "#F27406", "#F5B300", "#F30A0A", "#AC8C35", "#B4822B", "#8356A2", "#ADFF2F"),
      lines = alist(
        "totalLoad" = LOAD,
        "totalProduction" = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG` + WIND + SOLAR + `MISC. NDG`
      ),
      lineColors = c("#4C4E70", "#EB9BA6")
    )
  } else if (area == "gb") {
    # Configuration GB
    antaresViz::setProdStackAlias(
      "prod_gb",
      variables = alist(
        "pumpedStorage" = pump_d + turb_d,
        "import/export" = - BALANCE + `ROW BAL.`,
        "bioenergy" = `MISC. NDG`,
        "wind" = WIND,
        "solar" = SOLAR,
        "nuclear" = NUCLEAR,
        "hydraulic" = `H. ROR`,
        "gas" = GAS,
        "coal" = COAL,
        "lignite" = LIGNITE,
        "oil" = OIL,
        "other" = `MISC. DTG` + `MIX. FUEL`
      ),
      colors = c("#1147B9", "#969696", "#166A57", "#74CDB9", "#F27406", "#F5B300", "#2772B2", "#F30A0A", "#AC8C35", "#B4822B", "#8356A2", "#ADFF2F"),
      lines = alist(
        "totalLoad" = LOAD + pmax(0, -(pump_d + turb_d)),
        "totalProduction" = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG` + WIND + SOLAR + `H. ROR` + `MISC. NDG` + pmax(0, pump_d + turb_d)
      ),
      lineColors = c("#4C4E70", "#EB9BA6")
    )
  }
}








