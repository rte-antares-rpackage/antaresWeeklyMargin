

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

  margin <- copy(margin)
  margin[, jour := format(DATE_UTC, format = "%A")]
  margin[, heure := format(DATE_UTC, format = "%Hh")]
  margin <- margin[heure %chin% c("09h", "19h")]

  margin <- melt(
    data = margin,
    id.vars = c("DATE_UTC", "jour", "heure"),
    measure.vars = setdiff(names(margin), c("DATE_UTC", "jour", "heure"))
  )
  margin <- margin[, list(
    mediane = median(value),
    q1 = quantile(value, probs = 1/100),
    q4 = quantile(value, probs = 4/100),
    q10 = quantile(value, probs = 10/100)
  ), by = list(jour, heure)]
  melt(
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
#' @importFrom data.table copy dcast setnames
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
  margin <- copy(margin)

  if (language == "fr") {
    lab_j <- c("Lundi", "Mardi", "Mercredi", "Jeudi",
               "Vendredi", "Samedi", "Dimanche")
    lab_v <- c("M\u00e9diane", "Quantile 1%", "Quantile 4%", "Quantile 10%")
    odd_d <- c("Lundi", "Mercredi", "Vendredi", "Dimanche")
    var_t <- c("Jour", "Heure")
  } else {
    lab_j <- c("Monday", "Tuesday", "Wednesday", "Thursday",
               "Friday", "Saturday", "Sunday")
    lab_v <- c("Median", "Quantile 1%", "Quantile 4%", "Quantile 10%")
    odd_d <- c("Monday", "Wednesday", "Friday", "Sunday")
    var_t <- c("Day", "Hour")
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
    margin <- dcast(data = margin, formula = variable ~ jour + heure, value.var = "value")
    ft <- regulartable( data = margin, col_keys = names(margin) )
    typology <- data.frame(
      col_keys = names(margin),
      colA = c("", gsub(pattern = "_.*", replacement = "", x = names(margin)[-1])),
      colB = c("", gsub(pattern = ".*_", replacement = "", x = names(margin)[-1])),
      stringsAsFactors = FALSE
    )
    ft <- set_header_df(x = ft, mapping = typology, key = "col_keys" )
    ft <- merge_h(ft, part = "header")
    ft <- merge_v(ft, part = "header", j = 2:15)
    ft <- theme_zebra(ft, odd_header = "transparent", even_header = "transparent")
    ft <- fontsize(ft, size = 12, part = "all")
    ft <- fontsize(ft, i = 1:2, size = 14, part = "header")
    ft <- color(ft, i = 1:2, color = "#00A7DE", part = "header")
    ft <- hline(ft, border = fp_border(width = .75, color = "#00A7DE"), part = "body" )
    ft <- hline(ft, border = fp_border(width = 2, color = "#00A7DE"), part = "header" )
    ft <- empty_blanks(ft)
    # ft <- align( ft, align = "center", i = 1, part = "header" )
    ft
  } else {
    margin <- margin[order(jour, heure)]
    margin <- dcast(data = margin, formula = jour + heure ~ variable, value.var = "value")
    setnames(x = margin, old = c("jour", "heure"), new = var_t)
    ft <- regulartable( data = margin, col_keys = names(margin) )
    ft <- merge_v(ft, part = "body", j = 1)
    ft <- bg(
      x = ft,
      i = which(as.character(margin$jour) %in% odd_d),
      bg = "#EFEFEF", part = "body"
    )
    ft <- fontsize(ft, size = 12, part = "all")
    ft <- fontsize(ft, size = 14, part = "header")
    ft <- color(ft, color = "#00A7DE", part = "header")
    ft <- hline(ft, border = fp_border(width = .75, color = "#00A7DE"), part = "body" )
    ft <- hline_top(ft, j = NULL, border = fp_border(width = 2, color = "#00A7DE"), part = "header")
    ft <- hline(ft, border = fp_border(width = 2, color = "#00A7DE"), part = "header" )
    ft <- empty_blanks(ft)
    ft
  }
}
