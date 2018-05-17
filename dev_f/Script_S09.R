library(antaresRead)
library(antaresWeeklyMargin)
library(data.table)


#donnees Simulation ANTARES
opts <- antaresRead::setSimulationPath(path = "D:/Users/fabiarav/Documents/Marges_Hebdo/test_S09/", simulation = "input")
inputPath <- opts$inputPath

date_prev <- "2018-02-22"
heure_prev_meteologica <- "00:00:00"
path_meteologica <-  "PrevisionMeteologica/20180201-20180331/"
start_prev_hebdo <- "2018-02-24"

########  
#Effacer AREAS +BC
#Effacer toutes les areas qui ne sont pas importantes pour l'etude hebdo (dsr, rs) et les contraintes associees au FB.
source("xml_hydro/clear_bc_areas_links.R")

########
#PREV METEOLOGICA
#Add Meteologica forecast
formet <- read_meteologica2(path = path_meteologica)
formet <- formet[file_date == as.POSIXct(paste0(date_prev, heure_prev_meteologica))]
# Create time series
create_wm_ts(data = formet, start = start_prev_hebdo, opts = opts)

####### 
#PREV_HEBDO_CNES
#Add CNES forecast (prevu par défaut, sinon modifier le script)
source("xml_hydro/prev_conso_cnes.R")

##########
#THERMAL
# Read data from suppliers
sup <- read_planning(path = "Data_Ohmer/S09/Thermique/")
start_prev_hebdo_d <- as.Date(start_prev_hebdo)
# sup2 <- sup[as.Date(datetime) >= start_prev_hebdo_d & as.Date(datetime) < start_prev_hebdo_d + 7 ]
sup2 <- sup[format(datetime) >= start_prev_hebdo & format(datetime) < "2018-03-03" ]
# sup2 <- unique(sup2, by = c("datetime", "code_groupe"))
# View(sup2[, list(n = .N, min = min(datetime), max = max(datetime)), by = code_groupe])
# create clusters
clusters_crea <- create_wm_cluster(data = sup2, start = start_prev_hebdo, opts = opts)

######### 
#ROR + MISC_GEN
oa <- read_forfait_oa(path = "Data_Ohmer/S09/Forfaits/")

# Add ROR time series
create_wm_ror(data = oa, start= date_prev,  startday = "samedi", opts = opts)
# Add MISC_GEN time series
create_wm_misc(data = oa, start = start_prev_hebdo, opts = opts)


######### 
#NTC
ntc <- fread("Data_Ohmer/S09/NTC S09.csv")

#Add NTC FRANCE + AUTRES AREAS (EXCEPT FB_zone)
create_wm_ntc(data = ntc, start = start_prev_hebdo, opts = opts, startday = "samedi")
#Add NTC FB_zone
rep_ntc_tp <- "ntc_transparency/"
date_i <- as.Date(date_prev)

source("xml_hydro/ntc_be_de_nl.R")

####### 
#HYDRAULIQUE RESERVOIR + STEP
rep_capa_hydro <- "Data_Ohmer/S09/PH_RTEHEB_2018_S09_5.csv"
rep_hydro <- "Data_Ohmer/S09/"
dispo_pump_d <- c(3520,3520,3520,3520,3520,3520,3520)

#Add Hydro for France (forecast producteur)
opts <- create_wm_hydro_fr(path_capa_hydro = rep_capa_hydro, path_hydro = rep_hydro, start = start_prev_hebdo, opts = opts, dispo_pump = dispo_pump_d)
#Add Hydro for other areas
opts <- create_wm_hydro_areas(start = start_prev_hebdo, opts = opts)


