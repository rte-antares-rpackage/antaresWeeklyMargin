
#' Create clusters for Weekly Margins simulation
#'
#' @param data a \code{data.table} obtained from \code{\link{read_planning}}.
#' @param start If specified, data will be filtered from given date to 7 days after.
#' @param rm_prev_clus Remove previous clusters before creating new ones.
#' @param sort_other_clus Reorder rows of other clusters data.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom data.table first
#' @importFrom antaresRead simOptions readClusterDesc
#' @importFrom antaresEditObject createCluster removeCluster
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
create_wm_cluster <- function(data, start = NULL, rm_prev_clus = TRUE, sort_other_clus = TRUE, opts = antaresRead::simOptions()) {

  if (!all(c("comb_", "pmin", "pmax", "code_groupe") %in% names(data))) {
    stop("Invalid argument data, use output from read_planning.")
  }

  if (!is.null(start)) {
    start <- as.Date(start)
    data <- copy(data)
    # data <- data[as.Date(datetime, tz = "Europe/Paris") >= start]
    # data <- data[as.Date(datetime) < start + 7]
    data <- data[format(datetime, format = "%Y-%m-%d") >= format(start)]
    data <- data[format(datetime, format = "%Y-%m-%d") < format(start + 7)]
  }

  n_168 <- data[, .N, by = code_groupe]
  if (!all(n_168$N == 168)) {
    stop("Not all groups have 168 observations !", call. = FALSE)
  }

  if (rm_prev_clus) {
    oldclus <- antaresRead::readClusterDesc(opts = opts)
    oldclus <- oldclus[area == "fr", cluster]
    oldclus <- as.character(oldclus)
    l_max_o <- max(nchar(oldclus))
    cat("\nRemoval of former clusters\n")
    if (length(oldclus) > 0) {
      for (oldcluster in oldclus) {
        cat(sprintf("\rRemoving: %s (%s%%)",
                    format(oldcluster, width = l_max_o + 3),
                    round(which(oldclus == oldcluster)/length(oldclus)*100)))
        antaresEditObject::removeCluster(
          area = "fr",
          cluster_name = oldcluster,
          add_prefix = FALSE
        )
      }
    }
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

  data[, `:=`(pmax = ceiling(pmax), pmin = floor(pmin))]
  clusdata <- data[, list(
    area = "fr",
    cluster_name = first(code_groupe),
    group = co_comb[[first(comb_)]],
    unitcount = 1L,
    enabled = TRUE,
    nominalcapacity = max(pmax, na.rm = TRUE),
    `min-stable-power` = ifelse(
      test = min(pmin, na.rm = TRUE) < 0.1 * max(pmax, na.rm = TRUE),
      yes = 0, no = min(pmin, na.rm = TRUE)
    ),
    # `must-run` =  FALSE,
    `must-run` =  must_run(
      pmin_ = pmin,
      pmax_ = pmax,
      code_essai = code_essai,
      type = co_comb[[first(comb_)]]
    ),
    prepro_modulation = list(
      matrix_modulation(pmin_ = pmin, pmax_ = pmax, type = co_comb[[first(comb_)]])
    )
  ), by = list(code_groupe)]
  
  # browser()

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
    args <- args[!duplicated(names(args))]
    try(do.call(createCluster, args), silent = TRUE)
  }

  ok <- 0; ko <- 0
  cat(sprintf("\nCreating %s clusters\n", length(clusdata)))
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


  if (sort_other_clus) {
    cat("\nReordering rows of other clusters\n")
    othclus <- antaresRead::readClusterDesc(opts = opts)
    othclus <- othclus[area != "fr"]
    n_othclus <- nrow(othclus)
    l_max <- othclus[, max(nchar(as.character(cluster)))]
    for (i in seq_len(n_othclus)) {
      clusname <- othclus[i, as.character(cluster)]
      cat(sprintf("\rReordering: %s (%s%%)", format(clusname, width = l_max + 3), round(i/n_othclus*100)))
      sort_cluster(
        area = othclus[i, area],
        cluster_name = clusname,
        start_wm = start,
        start_sim = opts$start,
        inputPath = opts$inputPath
      )
    }
    cat("\n")
  }

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })

  invisible(res)
}






must_run <- function(pmin_, pmax_, code_essai, type) {
  # maxpmax <- max(pmax, na.rm = TRUE)
  # minpmin <- min(pmin, na.rm = TRUE)
  maxpmax <- quantile(pmax_, probs = 0.95, na.rm = TRUE)
  minpmin <- quantile(pmin_, probs = 0.05, na.rm = TRUE)
  if (num_equal(maxpmax, 0) & num_equal(minpmin, 0)) {
    return(FALSE)
  }
  
  if (num_equal(max(pmax_, na.rm = TRUE), min(pmin_, na.rm = TRUE))) {
    return(TRUE)
  }
  
  if (!type %in% c("N", "nuclear")) {
    return(FALSE)
  }
  minpmin >= maxpmax*0.9
}


matrix_modulation <- function(pmin_, pmax_, type) {
  if (type %in% c("N", "nuclear")) {
    maxpmax <- quantile(pmax_, probs = 0.95, na.rm = TRUE)
    minpmin <- quantile(pmin_, probs = 0.05, na.rm = TRUE)
    if (!num_equal(maxpmax, 0) & minpmin >= maxpmax*0.9) {
      values <- rep(0, 168)
    } else {
      values <- pmin_/max(pmax_, na.rm = TRUE)
    }
    if (num_equal(max(pmax_, na.rm = TRUE), min(pmin_, na.rm = TRUE))) {
      values <- rep(0, 168)
    }
    matrix(
      data = c(
        rep(1, times = 365 * 24 * 2), # two first columns
        ceiling((pmax_/max(pmax_, na.rm = TRUE))*1000)/1000, rep(0, 365 * 24 - 168), # [rep(1, 168)]
        # rep(0, times = 365 * 24 * 1) # fourth column
        values, rep(0, 365 * 24 - 168) 
      ), ncol = 4
    )
  } else {
    matrix(
      data = c(
        rep(1, times = 365 * 24 * 2), # two first columns
        (pmax_/max(pmax_, na.rm = TRUE)), rep(0, 365 * 24 - 168), # [rep(1, 168)]
        rep(0, times = 365 * 24 * 1) # fourth column
      ), ncol = 4
    )
  }
}





# Change rows order in cluster data
#' @importFrom data.table fread fwrite
#' @importFrom utils tail
sort_cluster <- function(area, cluster_name, start_wm, start_sim, n_days = 7, fill_zero = TRUE, inputPath) {

  start_wm <- as.Date(start_wm)
  start_sim <- as.Date(as.character(start_sim))

  # Indice data daily
  ind_day_wm <- difftime(time1 = start_wm, time2 = start_sim, units = "days")
  ind_day_wm <- as.numeric(ind_day_wm)
  ind_day_wm <- seq(from = ind_day_wm, length.out = n_days, by = 1)

  # Indice data hourly
  ind_hour_wm <- difftime(time1 = start_wm, time2 = start_sim, units = "hours")
  ind_hour_wm <- as.numeric(ind_hour_wm)
  ind_hour_wm <- seq(from = ind_hour_wm, length.out = n_days*24, by = 1) + 1

  # Prepro data
  prepro_data_path <- file.path(inputPath, "thermal", "prepro", area, cluster_name, "data.txt")
  if (file.size(prepro_data_path) > 0) {
    prepro_data <- data.table::fread(file = prepro_data_path)
    # if (fill_zero) {
    #   prepro_data[setdiff(seq_len(nrow(prepro_data)), ind_day_wm)] <- 0
    # }
    ind_data <- c(ind_day_wm, rep(tail(ind_day_wm, 1), times = nrow(prepro_data) - length(ind_day_wm)))
    prepro_data <- prepro_data[ind_data]
    data.table::fwrite(x = prepro_data, file = prepro_data_path, sep = "\t", row.names = FALSE, col.names = FALSE)
  }

  # Modulation data
  prepro_modu_path <- file.path(inputPath, "thermal", "prepro", area, cluster_name, "modulation.txt")
  if (file.size(prepro_modu_path) > 0) {
    prepro_modu <- data.table::fread(file = prepro_modu_path)
    # if (fill_zero) {
    #   prepro_modu[setdiff(seq_len(nrow(prepro_modu)), ind_hour_wm)] <- 0
    # }
    ind_modu <- c(ind_hour_wm, rep(tail(ind_hour_wm, 1), times = nrow(prepro_modu) - length(ind_hour_wm)))
    prepro_modu <- prepro_modu[ind_modu]
    data.table::fwrite(x = prepro_modu, file = prepro_modu_path, sep = "\t", row.names = FALSE, col.names = FALSE)
  }

  # Series data
  series_path <- file.path(inputPath, "thermal", "series", area, cluster_name, "series.txt")
  if (file.size(series_path) > 0) {
    series <- data.table::fread(file = series_path)
    # if (fill_zero) {
    #   series[setdiff(seq_len(nrow(series)), ind_hour_wm)] <- 0
    # }
    ind_series <- c(ind_hour_wm, rep(tail(ind_hour_wm, 1), times = nrow(series) - length(ind_hour_wm)))
    series <- series[ind_series]
    data.table::fwrite(x = series, file = series_path, sep = "\t", row.names = FALSE, col.names = FALSE)
  }

  return(invisible())
}


