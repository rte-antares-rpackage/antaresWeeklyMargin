

#' Compute margins' quantiles
#'
#' @param margin a \code{data.table} containing margins.
#'
#' @return a \code{data.table}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' margin_quantile(marges_inter_fr)
#'
#' }
#'
#' @importFrom data.table copy melt %chin% :=
#' @importFrom stats median quantile
margins_quantiles <- function(margin) {

  margin <- data.table::copy(margin)
  margin[, jour := format(DATE_UTC, format = "%A")]
  margin[, heure := format(DATE_UTC, format = "%Hh")]
  margin <- margin[heure %chin% c("09h", "19h")]

  margin <- data.table::melt(
    data = margin,
    id.vars = c("DATE_UTC", "jour", "heure"),
    measure.vars = setdiff(names(margin), c("DATE_UTC", "jour", "heure"))
  )
  margin <- margin[, list(
    mediane = stats::median(value),
    q1 = stats::quantile(value, probs = 1/100),
    q4 = stats::quantile(value, probs = 4/100),
    q10 = stats::quantile(value, probs = 10/100)
  ), by = list(jour, heure)]
  data.table::melt(
    data = margin,
    id.vars = c("jour", "heure"),
    measure.vars = c("mediane", "q1", "q4", "q10")
  )
}


#' Flextable format for margins quantiles
#'
#' @param margin Output from \code{\link{margins_quantiles}}.
#' @param layout Vertical or horizontal table.
#' @param language Language to use : \code{fr} or \code{en}.
#'
#' @return a \code{flextable} object
#' @export
#'
#' @importFrom data.table copy dcast
#' @importFrom flextable regulartable set_header_df merge_h merge_v
#'  theme_zebra fontsize color hline empty_blanks bg hline_top
#' @importFrom officer fp_border
#'
#' @examples
#' \dontrun{
#'
#' res <- margin_quantile(marges_inter_fr)
#' ft_margins_quantiles(res)
#'
#' }
ft_margins_quantiles <- function(margin, layout = c("horizontal", "vertical"), language = c("fr", "en")) {

  layout <- match.arg(layout)
  language <- match.arg(language)
  margin <- data.table::copy(margin)

  if (language == "fr") {
    lab_j <- c("Lundi", "Mardi", "Mercredi", "Jeudi",
               "Vendredi", "Samedi", "Dimanche")
    lab_v <- c("M\u00e9diane", "Quantile 1%", "Quantile 4%", "Quantile 10%")
    odd_d <- c("Lundi", "Mercredi", "Vendredi", "Dimanche")
  } else {
    lab_j <- c("Monday", "Tuesday", "Wednesday", "Thursday",
               "Friday", "Saturday", "Sunday")
    lab_v <- c("Median", "Quantile 1%", "Quantile 4%", "Quantile 10%")
    odd_d <- c("Monday", "Wednesday", "Friday", "Sunday")
  }

  margin[, jour := factor(
    x = jour,
    levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
    labels = lab_j
  )]
  margin[, variable := factor(
    x = variable,
    levels = c("mediane", "q1", "q4", "q10"),
    labels = lab_v
  )]
  margin[, value := formatC(value, big.mark = " ", digits = 0, format = "f")]

  if (layout == "horizontal") {
    margin <- data.table::dcast(data = margin, formula = variable ~ jour + heure, value.var = "value")
    ft <- flextable::regulartable( data = margin, col_keys = names(margin) )
    typology <- data.frame(
      col_keys = names(margin),
      colA = c("", gsub(pattern = "_.*", replacement = "", x = names(margin)[-1])),
      colB = c("", gsub(pattern = ".*_", replacement = "", x = names(margin)[-1])),
      stringsAsFactors = FALSE
    )
    ft <- flextable::set_header_df(x = ft, mapping = typology, key = "col_keys" )
    ft <- flextable::merge_h(ft, part = "header")
    ft <- flextable::merge_v(ft, part = "header", j = 2:15)
    ft <- flextable::theme_zebra(ft, odd_header = "transparent", even_header = "transparent")
    ft <- flextable::fontsize(ft, size = 12, part = "all")
    ft <- flextable::fontsize(ft, i = 1:2, size = 14, part = "header")
    ft <- flextable::color(ft, i = 1:2, color = "#007FA6", part = "header")
    ft <- flextable::hline(ft, border = officer::fp_border(width = .75, color = "#007FA6"), part = "body" )
    ft <- flextable::hline(ft, border = officer::fp_border(width = 2, color = "#007FA6"), part = "header" )
    ft <- flextable::empty_blanks(ft)
    # ft <- align( ft, align = "center", i = 1, part = "header" )
    ft
  } else {
    margin <- margin[order(jour, heure)]
    margin <- data.table::dcast(data = margin, formula = jour + heure ~ variable, value.var = "value")
    ft <- flextable::regulartable( data = margin, col_keys = names(margin) )
    ft <- flextable::merge_v(ft, part = "body", j = 1)
    ft <- flextable::bg(
      x = ft,
      i = which(as.character(margin$jour) %in% odd_d),
      bg = "#EFEFEF", part = "body"
    )
    ft <- flextable::fontsize(ft, size = 12, part = "all")
    ft <- flextable::fontsize(ft, size = 14, part = "header")
    ft <- flextable::color(ft, color = "#007FA6", part = "header")
    ft <- flextable::hline(ft, border = officer::fp_border(width = .75, color = "#007FA6"), part = "body" )
    ft <- flextable::hline_top(ft, j = NULL, border = officer::fp_border(width = 2, color = "#007FA6"), part = "header")
    ft <- flextable::hline(ft, border = officer::fp_border(width = 2, color = "#007FA6"), part = "header" )
    ft <- flextable::empty_blanks(ft)
    ft
  }
}
