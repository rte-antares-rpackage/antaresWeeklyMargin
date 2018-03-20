
#' Create clusters for Weekly Margins simulation
#'
#' @param data a \code{data.table} obtained from \code{\link{read_planning}}.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table first
#' @importFrom antaresRead simOptions
#'
#' @examples
#' \dontrun{
#'
#' # TODO
#'
#' }
create_wm_cluster <- function(data, opts = antaresRead::simOptions()) {

  if (!all(c("comb_", "pmin", "pmax", "code_groupe") %in% names(data))) {
    stop("Invalid argument data, use output from read_planning.")
  }

  co_comb <- list(
    "N" = "nuclear",
    "G" = "gas",
    "TAC F" = "oil",
    "F" = "oil",
    "C" = "Hard coal",
    "D" = "oil",
    "TAC G" = "gas"
  )

  clusdata <- data[, list(
    area = "fr",
    cluster_name = first(groupe),
    group = co_comb[[first(comb_)]],
    unit = 1,
    enabled = TRUE,
    nominalcapacity = max(pmax, na.rm = TRUE),
    `min-stable-power` = min(pmin, na.rm = TRUE),
    prepro_modulation = list(
      matrix(
        data = c(
          rep(1, times = 365 * 24 * 2), # two first columns
          (pmax/max(pmax, na.rm = TRUE))[rep(1, 168)], rep(0, 365 * 24 - 168),
          rep(0, times = 365 * 24 * 1) # fourth column
        ), ncol = 4
      )
    )
  ), by = list(code_groupe)]

  clusdata <- lapply(
    X = seq_len(nrow(clusdata)),
    FUN = function(i) {
      res <- as.list(clusdata[i, .SD, .SDcols = names(clusdata)[-1]])
      res$prepro_modulation <- res$prepro_modulation[[1]]
      res
    }
  )
  # lapply(l, do.call, what = "createCluster")

  return(clusdata)
}



