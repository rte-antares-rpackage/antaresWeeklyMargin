
# TEST CREATION CLUSTERS


library(antaresRead)
library(antaresWeeklyMargin)

# set path to your simulation
opts <- setSimulationPath(path = "test_case/", simulation = "input")

# Read data from suppliers
sup <- read_planning(path = "../xml_hydro/inputs/thermique_producteur")

# create clusters
clusters_crea <- create_wm_cluster(data = sup, start = "2017-12-16", opts = opts)

str(test[[1]])
sapply(test, length)


library(data.table)
amf <- sup[code_groupe %like% "AMFART14"]


sup[code_groupe %like% "AMFART14" & datetime < as.POSIXct("2017-12-20 19:00:00"), pmax := 100]



test <- read_planning(path = "../xml_hydro/inputs/Politique_S_S+1_exploitation_20171212.xlsx")
table(test$code_groupe)


start <- as.Date("2017-12-16")
data <- copy(sup)
data <- data[as.Date(datetime, tz = "Europe/Paris") >= start]
data <- data[as.Date(datetime) < start + 7]
table(data$code_groupe)
summary(data$datetime)

n_168 <- data[, .N, by = code_groupe]
all(n_168$N == 168)



clus <- readClusterDesc()
clus[area == "fr", cluster]

antaresEditObject::removeCluster(area = "fr", cluster_name = "fr_gas_ccgt_new", add_prefix = FALSE)






