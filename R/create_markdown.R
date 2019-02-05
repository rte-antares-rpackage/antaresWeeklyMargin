
#' Create directory with SMTA markdown
#'
#' @param path Path to a directory, if directory doesn't exist, it will be created.
#' @param format Format of markdown output : report or slides.
#' @param date_start Study start date.
#' @param date_study Date for margins computation.
#' @param week Number of the week studied.
#' @param n_scenario Number of scenario used in analysis.
#' @param year_mc Number of the Monte-Carlo year to focus on.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'
#' @export
#'
#' @importFrom rstudioapi navigateToFile
#' @importFrom usethis use_template
#'
#' @examples
#' \dontrun{
#'
#' create_markdown("SMTA_rapport", "report")
#'
#' }
create_markdown <- function(path, format = c("report", "slides"),
                            date_start = NULL, date_study = NULL, 
                            week = NULL, n_scenario = NULL, year_mc = NULL,
                            opts = antaresRead::simOptions()) {
  format <- match.arg(format)
  if (!dir.exists(path)) {
    dir.create(path = path, recursive = TRUE)
  }
  file.copy(
    from = list.files(
      path = system.file("markdown", format, package = "antaresWeeklyMargin"),
      full.names = TRUE
    ),
    to = path, recursive = TRUE
  )
  usethis::use_template(
    template = "data-report.R", 
    save_as = file.path(path, "data-report.R"),
    data = list(
      path = path,
      date_report = format(Sys.Date(), format = "%d/%m/%Y"),
      simPath = opts$simPath,
      date_start = date_start, 
      date_study = date_study,
      week = week, n_scenario = n_scenario, year_mc = year_mc
    ), 
    open = TRUE, 
    package = "antaresWeeklyMargin"
  )
  rstudioapi::navigateToFile(file = file.path(path, "index.Rmd"))
  invisible()
}
