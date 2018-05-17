
library(antaresRead)
library(antaresWeeklyMargin)
library(antaresProcessing)
library(antaresViz)
library(data.table)

opts_26<- setSimulationPath("test_S09_rgf_260218/", simulation = 2)
opts_27<- setSimulationPath("test_S09_rgf_270218/", simulation = 3)
#Date Start_prev_hebdo
date_i <- "2018-02-28"
date_etudie <- "2018-02-28 10:00:00"
rep_results <- "Analyse_S09_rgf_270218/rapport/datas/"

###Calcul des MARGES PAYS SEUL ET INTERCONNECTE

pays <- "fr"
links_fr <- getLinks(areas="fr", exclude = c("lac","pump_d", "turb_d","pump_w", "turb_w" ))
links_virtuel_fr <-  c("fr - lac","fr - pump_d", "fr - turb_d","fr - pump_w", "fr - turb_w") 


data_fr <- readAntares(areas = pays, 
                       links = links_virtuel_fr,
                       select = c("FLOW LIN.", "AVL DTG", "MISC. NDG", "H. ROR", "WIND", "SOLAR", "LOAD",
                                  "MISC. DTG", "BALANCE", "NUCLEAR", "GAS", "COAL", "LIGNITE", "OIL",
                                  "MIX. FUEL", "ROW BAL.", "FLOW LIN.", "LOLD", "LOLP", "UNSP. ENRG"), 
                       mcYears = "all", linkCapacity = length(links_virtuel_fr) > 0, opts = opts_27)

if (length(links_virtuel_fr) == 0) {
  data_fr2 <- copy(data_pays)
  data_fr2$storageCapacity <- 0
  data_fr2$pumpingCapacity <- 0
} else {
  #Si le pays est liee a des noeuds virtuels, les valeurs de storageCapacity et de pumpingCapacity seront definis
  #Mais, il faudra extraire du BALANCE la valeur des echanges avec les noeuds virtuels
  data_fr2 <- removeVirtualAreas(data_fr, c("lac","pump_d", "turb_d","pump_w", "turb_w"))$areas
}

marge_pays <- data_fr2[, marges_seul:= `AVL DTG`+ storageCapacity +`H. ROR`+`MISC. NDG`+ WIND + SOLAR - LOAD]
marge_pays <- data_fr2[, marges_inter:= marges_seul - BALANCE + `ROW BAL.`]

date_debut <- as.POSIXct(paste0(date_i," 00:00:00"), format="%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
new_time <- data.table(DateTime=as.POSIXct(format(seq(date_debut, by=("+1 hour"), length.out = 168)), 
                                           format="%Y-%m-%d %H:%M:%S", tz="Europe/Paris"))

marge_pays_seul_peryear <- data.table(DATE_UTC = new_time$DateTime, MC_YEAR = marge_pays$mcYear, 
                                      MARGE_PAYS_SEUL = marge_pays$marges_seul)
marge_pays_seul <- dcast(marge_pays_seul_peryear, DATE_UTC ~ MC_YEAR, value.var = "MARGE_PAYS_SEUL")

#on cr?? une table avec une colonne date, et une colonne marge_inter pour chaque annee MC
marge_pays_inter_peryear <- data.table(DATE_UTC = new_time$DateTime, MC_YEAR = marge_pays$mcYear, 
                                       MARGE_PAYS_INTER = marge_pays$marges_inter)
marge_pays_inter <- dcast(marge_pays_inter_peryear, DATE_UTC ~ MC_YEAR, value.var = "MARGE_PAYS_INTER")

# data_fr2$time <- new_time$DateTime

corr_time <- data.table(
  timeId = seq_len(168),
  time = as.POSIXct(
    format(seq(as.POSIXct(date_i), by=("+1 hour"), length.out = 168)),
    format="%Y-%m-%d %H:%M:%S", tz="Europe/Paris"
  )
)
data_fr2 <- data_fr2[, time := NULL]
data_fr2 <- merge(x = data_fr2, y = corr_time)

saveRDS(data_fr2, file=paste0(rep_results, "data_fr.rds"))
saveRDS(marge_pays_seul, file=paste0(rep_results,"marge_fr_seule.rds"))
saveRDS(marge_pays_inter, file=paste0(rep_results,"marge_fr_inter.rds"))

# #GRAPHIQUES HYPOTHESES
# draw_series(data_fr2, serie = "LOAD", mcYears = 1, main = "Consommation previsionnelle France - Semaine 09/2018")
# draw_series(data_fr2, serie = "WIND", mcYears = 1:51, main = "Production eolienne France prevue pour la semaine 09/2018")
# draw_series(data_fr2, serie = "SOLAR", mcYears = 1:51, main = "Production solaire France prevue pour la semaine 09/2018")
# 
# #GRAPHIQUE MARGES
# draw_upward_margin(marge_pays_seul, area = pays, type = "seul", nb_MC = 2040, num_week = 9)
# draw_upward_margin(marge_pays_inter, area = pays, type = "inter", nb_MC = 2040, num_week = 9)
# draw_stack_hist(marge_pays_seul, marge_pays_inter, area = "fr")

mono <- process_data_mono(start = date_i, date = date_etudie, area = "fr"
                          ,nb_MC = 2040, opts = opts_27)

# draw_mono(data = mono$mono_be)
# draw_mono(data = mono$mono_de)
# draw_mono(data = mono$mono_ch)
# draw_mono(data = mono$mono_es)
# draw_mono(data = mono$mono_gb)
# draw_mono(data = mono$mono_it)
# draw_mono(data = mono$mono_cwe)
# draw_mono(data = mono$mono_france,
#           main = paste0("Monotone des flux imports/exports pour France ", date_etudie),
#           label = "Flux_France")
# mono$mono_france

saveRDS(mono, file=paste0(rep_results,"mono.rds"))

# draw_prod_MC(data_fr2, area = pays, mc_year = 1436, date_i = date_i)
# draw_prod_MC(data_fr2, area = pays, mc_year = 1662, date_i = date_i)
# draw_prod_MC(data_fr2, area = pays, mc_year = 50, date_i = date_i)

liste_areas <- getAreas(exclude = c("lu_be","lu_de"), opts = opts_27)
liste_links <- getLinks(exclude = c("lu_be","lu_de"), opts = opts_27)

mc_year <- c(50,1436,1662)
# mc_year <- 50

for (i in mc_year){
  
  #On obtient les resultats de la simulation ANTARES pour un MC_year
  data_all_areas<- readAntares(areas=liste_areas, links=liste_links, mcYears = i, linkCapacity = TRUE)
  data_all <- removeVirtualAreas(data_all_areas, c("lac", "pump_w", "pump_d", "turb_w", "turb_d"))
  
  #On definit les valeurs des marges pays seul et interconnecte
  data_all$areas[, marges_seul:= `AVL DTG`+ storageCapacity +`H. ROR`+`MISC. NDG`+ WIND + SOLAR - LOAD]
  data_all$areas[, marges_inter:= marges_seul - BALANCE + `ROW BAL.`]
  
  #Pour corriger les probl?mes d'arrondi dans le calcul de marges
  data_all$areas[-1 <= marges_inter & marges_inter <= 1, marges_inter:= 0]
  
  #On utilise la fonction addLoadFactorLink pour obtenir le facteur de charge de chaque lien
  #On obtient la valeur absolue du facteur de charge et on le garde dans une nouvelle variable
  addLoadFactorLink(data_all$links)
  data_all$links[, abs_loadFactor:= abs(loadFactor)]
  
  data_all$areas <- data_all$areas[, time := NULL]
  data_all$areas <- merge(x = data_all$areas, y = corr_time)
  
  data_all$links <- data_all$links[, time := NULL]
  data_all$links <- merge(x = data_all$links, y = corr_time, by = "timeId")
  
  
  opts_27 <- attr(data_all$links, "opts")
  opts_27$start <- as.POSIXlt("2018-02-27", tz = "UTC")
  attr(data_all$links, "opts") <- opts_27
  
  
  saveRDS(data_all, file=paste0(rep_results,"data_all_mc",i,".rds"))
  
}


# exchangesStack(data_all$links, area = "fr")
# 
# create_markdown(path="Marges_Hebdo/Analyse_S09_enr/", format = "report")


