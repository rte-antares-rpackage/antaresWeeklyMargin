
#' Render Weekly Margin Report
#'
#' @param output_file PAth to output file generated.
#' @param date_start Study start date.
#' @param date_margins Date for margins computation.
#' @param date_mono Date-time for representing monotone.
#' @param week Number of the week studied.
#' @param n_scenario Number of scenario used in analysis.
#' @param year_mc Number of the Monte-Carlo year to focus on.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'
#' @export
#' 
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#'
render_report <- function(output_file = "index.html", date_start, date_margins, date_mono,
                          week, n_scenario, year_mc,
                          opts = antaresRead::simOptions()) {
  output_file <- normalizePath(output_file, mustWork = FALSE)
  suppressWarnings({
    marges <- compute_margins(date = date_margins, area = "fr", margin = "upward", opts = opts)
    marges_all <- compute_all_margins(date = date_margins, mcYear = year_mc, opts = opts)
    mono <- compute_mono(start = date_start, date = date_mono, opts = opts)
  })
  rmarkdown::render(
    input = system.file("markdown/wmauto/index.Rmd", package = "antaresWeeklyMargin"),
    output_file = output_file,
    params = list(
      week = week, 
      n_scenario = n_scenario, 
      year_mc = year_mc, 
      date_study = date_margins,
      date_debut = date_start
    )
  )
  browseURL(output_file)
}



