
#' Create directory with SMTA markdown
#'
#' @param path Path to a directory, if directory doesn't exist, it will be created.
#' @param format Format of markdown output : report or slides
#'
#' @export
#'
#' @importFrom rstudioapi navigateToFile
#'
#' @examples
#' \dontrun{
#'
#' create_markdown("SMTA_rapport", "report")
#'
#' }
create_markdown <- function(path, format = c("report", "slides")) {
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
  rstudioapi::navigateToFile(file = file.path(path, "index.Rmd"))
  invisible()
}
