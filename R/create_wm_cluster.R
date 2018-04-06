
#' Create clusters for Weekly Margins simulation
#'
#' @param data a \code{data.table} obtained from \code{\link{read_planning}}.
#' @param start If specified, data will be filtered from given date to 7 days after.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table first
#' @importFrom antaresRead simOptions
#' @importFrom antaresEditObject createCluster
#'
#' @examples
#' \dontrun{
#'
#' # set path to your simulation
#' opts <- setSimulationPath(path = "path/", simulation = "input")
#'
#' # Read data from suppliers
#' plannings <- read_planning()
#'
#' # create clusters
#' clusters_crea <- create_wm_cluster(plannings, opts)
#'
#' }
create_wm_cluster <- function(data, start = NULL, opts = antaresRead::simOptions()) {

  if (!all(c("comb_", "pmin", "pmax", "code_groupe") %in% names(data))) {
    stop("Invalid argument data, use output from read_planning.")
  }

  if (!is.null(start)) {
    start <- as.Date(start)
    data <- copy(data)
    data <- data[as.Date(datetime, tz = "Europe/Paris") >= start]
    data <- data[as.Date(datetime) < start + 7]
  }

  n_168 <- data[, .N, by = code_groupe]
  if (!all(n_168$N == 168)) {
    stop("Not all groups have 168 observations !", call. = FALSE)
  }

  co_comb <- list(
    "N" = "nuclear",
    "G" = "gas",
    "TAC F" = "oil",
    "F" = "oil",
    "C" = "Hard coal",
    "D" = "oil",
    "TAC G" = "gas",
    "V" = "gas",
    "H" = "Hard coal",
    "RN" = "gas"
  )

  clusdata <- data[, list(
    area = "fr",
    cluster_name = first(code_groupe),
    group = co_comb[[first(comb_)]],
    unitcount = 1L,
    enabled = TRUE,
    nominalcapacity = pmd[1],
    `min-stable-power` = min(pmin, na.rm = TRUE),
    prepro_modulation = list(
      matrix(
        data = c(
          rep(1, times = 365 * 24 * 2), # two first columns
          (pmax/max(pmax, na.rm = TRUE)), rep(0, 365 * 24 - 168), # [rep(1, 168)]
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
      res$prepro_modulation[is.na(res$prepro_modulation)] <- 0
      res$opts <- opts
      clus_d <- corr_groupe_descr(res$cluster_name)
      res$prepro_data <- matrix(
        data = c(
          rep(7, times = 365),
          rep(1, times = 365),
          rep(fo_rate(clus_d), times = 365),
          rep(0, times = 365 * 3)
        ),
        ncol = 6
      )
      res <- c(res, descr_clusters(clus_d))
      res$cluster_name <- clean_names(res$cluster_name)
      res
    }
  )
  tryCreateCluster <- function(args) {
    # Sys.sleep(0.1)
    # try(stop("erreur"), silent = TRUE)
    try(do.call(createCluster, args), silent = TRUE)
  }

  ok <- 0; ko <- 0
  cat(sprintf("Creating %s clusters\n", length(clusdata)))
  for (i in seq_along(clusdata)) {
    resclus <- tryCreateCluster(clusdata[[i]])
    if ("try-error" %in% class(resclus)) {
      ko <- ko + 1
      clusdata[[i]] <- resclus
      # message(attr(resclus, "condition"))
    } else  {
      ok <- ok + 1
    }
    cat(sprintf("\rOK: %s | FAILED: %s", formatC(x = ok, width = 3), formatC(x = ko, width = 3)))
  }
  cat("\ncluster creation completed")

  return(invisible(clusdata))
}



