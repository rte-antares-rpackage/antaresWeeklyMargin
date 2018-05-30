library(antaresRead)
library(antaresWeeklyMargin)
library(data.table)

# set path to your simulation
# opts <- setSimulationPath(path = "test_case/", simulation = "input")
# inputPath <- opts$inputPath

# rep_ntc_tp <- "ntc_transparency/"

# date_i <- as.Date("2018-02-24")
date_f <- date_i+6

ntc_tp <- read_ntc(path = rep_ntc_tp)


ntc_tp_test <- ntc_tp[date >= date_i & date <= date_f 
                      & areaintypecode == "BZN" 
                      & mapcodein %in% c("DE_AT_LU","BE", "NL")
                      & mapcodeout %in% c("DE_AT_LU","BE", "NL")]

ntc_be_nl_aux <- ntc_tp[date >= date_i & date <= date_f 
                    & areaintypecode == "BZN" 
                    & mapcodein %in% c("BE", "NL")
                    & mapcodeout %in% c("BE", "NL"), c("datetime", "date", "mapcodeout", "mapcodein", "forecasttransfercapacity")]


ntc_be_nl <- as.data.table(matrix(data = c(rep(0, 8760*3), rep(0.5, 8760*2)), ncol = 5))
ntc_aux <- NULL

#NTC BE-NL
for (i in date_i:date_f){
  aux <- ntc_be_nl_aux[date == i,]
  names(aux)
  
  aux_be <- aux[mapcodeout == "BE"]$forecasttransfercapacity
  aux_nl <- aux[mapcodeout == "NL"]$forecasttransfercapacity
  
  aux_ntc <- as.data.table(cbind(rep(aux_be,24),rep(aux_nl,24)))
  
  ntc_aux <- as.data.table(rbind(ntc_aux,aux_ntc))
}


#Determiner le NTC DE-NL
ntc_de_nl_aux <- ntc_tp[date >= date_i & date <= date_f 
                        & areaintypecode == "BZN" 
                        & mapcodein %in% c("DE_AT_LU", "NL")
                        & mapcodeout %in% c("DE_AT_LU", "NL"), c("datetime", "date", "mapcodeout", "mapcodein", "forecasttransfercapacity")]


ntc_de_nl <- as.data.table(matrix(data = c(rep(0, 8760*3), rep(0.5, 8760*2)), ncol = 5))
ntc_daux <- NULL


for (i in date_i:date_f){
  daux <- ntc_de_nl_aux[date == i,]

  daux_de <- daux[mapcodeout == "DE_AT_LU"]$forecasttransfercapacity
  daux_nl <- daux[mapcodeout == "NL"]$forecasttransfercapacity
  
  if(daux_de == 0 || daux_nl ==0){
    
    #Solution 1: On remplace les valeurs des NTC BZN par les valeurs NTC CTA
    # daux <- ntc_tp[date == i
    #                & areaintypecode == "CTA" 
    #                & mapcodein %in% c("DE_TenneT_GER", "NL")
    #                & mapcodeout %in% c("DE_TenneT_GER", "NL"), c("datetime", "date", "mapcodeout", "mapcodein", "forecasttransfercapacity")]
    # 
    # daux_de <- daux[mapcodeout == "DE_TenneT_GER"]$forecasttransfercapacity
    # daux_nl <- daux[mapcodeout == "NL"]$forecasttransfercapacity
    
    
    #Solution 2: On remplace les valeurs des NTC par les valeurs des jours précendents
    
    daux <- ntc_de_nl_aux[date == date_i,]
    
    daux_de <- daux[mapcodeout == "DE_AT_LU"]$forecasttransfercapacity
    daux_nl <- daux[mapcodeout == "NL"]$forecasttransfercapacity
    
  }
  
  
  daux_ntc <- as.data.table(cbind(rep(daux_de,24),rep(daux_nl,24)))
  ntc_daux <- as.data.table(rbind(ntc_daux,daux_ntc))
}


#Ecriture des NTC dans les fichiers
# be-nl
ntc_be_nl[1:168, 1:2] <- ntc_aux
write.table(ntc_be_nl, row.names = FALSE, col.names = FALSE, sep = "\t",
            file = paste0(inputPath, "/links/be/nl.txt"))

# de-nl
ntc_de_nl[1:168, 1:2] <- ntc_daux
write.table(ntc_de_nl, row.names = FALSE, col.names = FALSE, sep = "\t",
            file = paste0(inputPath, "/links/de/nl.txt"))

# be-de
ntc_null <- NULL
write.table(ntc_null, row.names = FALSE, col.names = FALSE, sep = "\t",
            file = paste0(inputPath, "/links/be/de.txt"))


#  be-lu_be"
ntc_be_lu <- as.data.table(matrix(data = c(rep(300, 8760*2), rep(0, 8760*1), rep(0.5, 8760*2)), ncol = 5))
write.table(ntc_be_lu, row.names = FALSE, col.names = FALSE, sep = "\t",
            file = paste0(inputPath, "/links/be/lu_be.txt"))

# de-lu_de"
ntc_de_lu <- as.data.table(matrix(data = c(rep(1, 8760*2), rep(0, 8760*1), rep(0.5, 8760*2)), ncol = 5))
write.table(ntc_de_lu, row.names = FALSE, col.names = FALSE, sep = "\t",
            file = paste0(inputPath, "/links/de/lu_de.txt"))

