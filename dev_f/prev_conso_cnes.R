

source("xml_hydro/FonctionsLecturePrevisionHebdo.R")
library(lubridate)
library(stringr)
library(data.table)

# #donnees Simulation ANTARES
# opts <- antaresRead::setSimulationPath(path = "D:/Users/fabiarav/Documents/Marges_Hebdo/test_case/", simulation = "input")
# inputPath <- opts$inputPath

#Repertoire des donnees PREMIS
# dossier <- "CNES_PREMIS_archives_2017/HEBDOJ13/Etude/CNES_HEBDO_010911_310816/MODELE/Hebdo/"
dossier <- "D:/Users/fabiarav/Documents/Marges_Hebdo/Prev_hebdo_CNES/"
liste_des_dossiers <- list.files(dossier,pattern = "^PREV")

date_prev <- "2018-02-27"
# prev <- "premis"
prev <- "prevu"

date_prev <- as.Date(date_prev)
date_samedi <- as.Date(start_prev_hebdo)
date_samedi_prox <- date_samedi+7
date_fichier <- format(date_prev, "%d%m%y")

nom_fichier <- list.files(dossier, pattern = paste0("^PREV_",date_fichier))
dossier_chemin_complet <- paste(dossier, nom_fichier, sep = "")

conso_prev <- as.data.table(lecturePrevisionHebdo(dossier_chemin_complet))
conso_prev <- conso_prev[hd_valeur >= date_samedi & hd_valeur < date_samedi_prox, ]

# mean_conso <- apply(conso_prev[,!c("hd_valeur","prevCN","prevu")], MARGIN = 1,mean)
# prevu <- conso_prev$prevu
# diff <- as.data.table(mean_conso-prevu)


if(prev == "premis") {
  prev_premis <- conso_prev[, !c("hd_valeur","prevCN","prevu")]
  matrix_conso <- as.data.table(matrix(data = c(rep(0, 8760*51)), ncol = 51))
  matrix_conso[1:168,] <- prev_premis  
}

if(prev == "prevu"){
  prev_u <- conso_prev[, c("prevu")]
  matrix_conso <- as.data.table(matrix(data = c(rep(0, 8760*1)), ncol = 1))
  matrix_conso[1:168,] <- prev_u 
}

write.table(matrix_conso, row.names = FALSE, col.names = FALSE, sep = "\t",
            file = paste0(inputPath, "/load/series/load_fr.txt"))


