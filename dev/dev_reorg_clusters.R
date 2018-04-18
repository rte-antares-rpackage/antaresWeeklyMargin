

# TEST REORG AUTRES CLUS


library(antaresRead)
library(antaresWeeklyMargin)
library(data.table)

# set path to your simulation
opts <- setSimulationPath(path = "test_case/", simulation = "input")
inputPath <- opts$inputPath

# description clusters
desc <- readClusterDesc()
desc
list_cluster <- desc[area != "fr", as.character(cluster)]
list_cluster

# parametres
clus <- "at_gas_ccgt_new"
aclus <- "at"
start_wm <- as.Date("2018-04-01")
start_sim <- as.Date(as.character(opts$start))

ind_day_wm <- difftime(time1 = start_wm, time2 = start_sim, units = "days")
ind_day_wm <- as.numeric(ind_day_wm)
ind_day_wm <- seq(from = ind_day_wm, length.out = 14, by = 1)

ind_hour_wm <- difftime(time1 = start_wm, time2 = start_sim, units = "hours")
ind_hour_wm <- as.numeric(ind_hour_wm)
ind_hour_wm <- seq(from = ind_hour_wm, length.out = 14*24, by = 1)

prepro_data <- fread(file = file.path(inputPath, "thermal", "prepro", aclus, clus, "data.txt"))
str(prepro_data)
ind_data <- c(ind_day_wm, rep(tail(ind_day_wm, 1), times = nrow(prepro_data) - length(ind_day_wm)))
prepro_data <- prepro_data[ind_data]

prepro_mod <- fread(file = file.path(inputPath, "thermal", "prepro", aclus, clus, "modulation.txt"))
str(prepro_mod)

c(ind_day_wm, setdiff(seq_len(nrow(prepro_data)), ind_day_wm))


prepro_data[setdiff(seq_len(nrow(prepro_data)), ind_day_wm)] <- 0
prepro_data <- prepro_data[c(ind_day_wm, setdiff(seq_len(nrow(prepro_data)), ind_day_wm))]

series <- fread(file = file.path(inputPath, "thermal", "series", aclus, clus, "series.txt"))
str(series)


sort_cluster <- function(area, cluster_name, start_wm, start_sim) {

  # Indice data daily
  ind_day_wm <- difftime(time1 = start_wm, time2 = start_sim, units = "days")
  ind_day_wm <- as.numeric(ind_day_wm)
  ind_day_wm <- seq(from = ind_day_wm, length.out = 14, by = 1)

  # Indice data hourly
  ind_hour_wm <- difftime(time1 = start_wm, time2 = start_sim, units = "hours")
  ind_hour_wm <- as.numeric(ind_hour_wm)
  ind_hour_wm <- seq(from = ind_hour_wm, length.out = 14*24, by = 1)

  # Prepro data
  prepro_data_path <- file.path(inputPath, "thermal", "prepro", area, cluster_name, "data.txt")
  prepro_data <- data.table::fread(file = prepro_data_path)
  prepro_data <- prepro_data[c(ind_day_wm, setdiff(seq_len(nrow(prepro_data)), ind_day_wm))]
  data.table::fwrite(x = prepro_data, file = prepro_data_path, sep = "\t", row.names = FALSE, col.names = FALSE)

  # Modulation data
  prepro_modu_path <- file.path(inputPath, "thermal", "prepro", area, cluster_name, "modulation.txt")
  prepro_modu <- data.table::fread(file = prepro_modu_path)
  prepro_modu <- prepro_data[c(ind_hour_wm, setdiff(seq_len(nrow(prepro_modu)), ind_hour_wm))]
  data.table::fwrite(x = prepro_modu, file = prepro_modu_path, sep = "\t", row.names = FALSE, col.names = FALSE)

  # Series data
  series_path <- file.path(inputPath, "thermal", "series", area, cluster_name, "series.txt")
  series <- data.table::fread(file = series_path)
  series <- series[c(ind_hour_wm, setdiff(seq_len(nrow(series)), ind_hour_wm))]
  data.table::fwrite(x = series, file = series_path, sep = "\t", row.names = FALSE, col.names = FALSE)

  return(invisible())
}



tist <- desc[area != "fr"]
for (i in seq_len(nrow(tist))) {
  print(paste(tist[i, area], tist[i, as.character(cluster)]))
}

path <- "D:/dreamRs/Work/RTE/antaresWeeklyMargin/test_case/input/thermal/series/at/at_gas_pcomp_mid/series.txt"
file.size(path)
testemp <- data.table::fread(file = path)



# Test --------------------------------------------------------------------

library(antaresRead)
library(antaresWeeklyMargin)

# set path to your simulation
opts <- setSimulationPath(path = "test_case/", simulation = "input")

# Read data from suppliers
sup <- read_planning(path = "../xml_hydro/inputs/thermique_producteur")

# create clusters
clusters_crea <- create_wm_cluster(data = sup, start = "2017-12-16", opts = opts)



