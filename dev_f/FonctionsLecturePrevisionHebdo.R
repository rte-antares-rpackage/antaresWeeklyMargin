#Creer un csv des prev en fonctions des senarios meteo
#dossier_entree :    chemin complet du dossier ou se trouvent les fichiers prev
#dossier_reception_et_nom : 
  #si le parametre n'est pas renseignee, le fichier s'appellera comme le dossier_entree
  #sinon indiquez le nom du csv en sortie
#Par defaut,le fichier est ecrit dans le repertoire courant 
#sinon indiquez dossier_de_reception
lecturePrevisionHebdo <-
  function(dossier_entree,
           nom_du_csv = NULL,
           dossier_de_reception = NULL) {
    #librairies
    require(stringr)
    require(data.table)
    #dossier_entree finit-il par "/"?
    if (!str_sub(dossier_entree,nchar(dossier_entree)) == "/") {
      dossier_entree <- paste(dossier_entree,"/",sep="")
    }
    #liste des fichiers commencant par prev
    liste_complete <-
      gtools::mixedsort((list.files(dossier_entree, pattern = "^prev")))
    #on retire le fichier prevCN s'il existe
    # liste_complete <- setdiff(liste_complete, c("prevCN", "prevu"))
    
    #lecture du premier fichier
    filename <- liste_complete[1]
    
    #lecture  premier fichier : obtenir le nombre de ligne du tableau
    filename_complet <- paste(dossier_entree, filename, sep = "")
    prev <-
      read.csv(
        filename_complet,
        skip = 3,
        header = FALSE,
        stringsAsFactors = FALSE
      )
    nb_ligne <- nrow(prev)
    
    #transformation du fichier en tableau
    tableau <-
      outer(
        1:nb_ligne,
        1:12 ,
        FUN = function(ligne, colonne) {
          as.numeric(str_sub(prev[ligne, 1], 21 + 6 * (colonne - 1), 26 + 6 * (colonne -
                                                                                 1)))
        }
      )
    
    #creation de la date initiale
    date <- as.numeric(str_sub(prev[1, 1], 11, 16))
    
    annee <- as.numeric(str_sub(date, 1, 2)) + 2000
    mois <- as.numeric(str_sub(date, 3, 4))
    jour <- as.numeric(str_sub(date, 5, 6))
    
    date_ini <- ISOdatetime(annee, mois, jour, 1 , 0 , 0)
    
    #initialisation du tableau final
    sortie  <-
      as.data.frame(matrix(NA, 6 * nb_ligne, 1 + length(liste_complete)))
    colnames(sortie)[1] <- "hd_valeur"
    colnames(sortie)[2] <- filename
    
    #gestion de la date
    vecteur_date <-seq(date_ini, by = "1 hour", length.out = nb_ligne * 6)
    #changement heure ete
    if (hour(last(vecteur_date)) == 1) {
      emplacement <- which(diff(hour(vecteur_date)) == 2)
      date_manquante <-
        paste(as.character(lubridate::date(vecteur_date[emplacement])), "02:00:00", sep = " ")
      vecteur_date <- vecteur_date[-length(vecteur_date)]
      vecteur_date <- as.character(vecteur_date)
      vecteur_date <- append(vecteur_date, date_manquante, emplacement)
    }
    #changement heure hiver
    if (hour(last(vecteur_date)) == 23) {
      vecteur_date <-
        seq(date_ini, by = "1 hour", length.out = nb_ligne * 6 + 1)
      emplacement <- which(diff(hour(vecteur_date)) == 0)
      vecteur_date <- vecteur_date[-emplacement]
      vecteur_date <- as.character(vecteur_date)
    }
    
    #inserer du hd_valeur dans la premiere colonne du tablea
    sortie[, 1] <- vecteur_date
    veceur_prev <- as.vector(t(tableau))
    sortie[,2] <-
      veceur_prev[seq(from = 2, to = nb_ligne * 12, by = 2)] #on ne prend que les heures piles
    
    #lecteur des autres senarios meteo
    for (k in 2:length(liste_complete)) {
      
      filename <- liste_complete[k]
      colnames(sortie)[k + 1] <- filename
      
      #lecture fichier
      filename_complet <- paste(dossier_entree, filename, sep = "")
      prev <-
        read.csv(
          filename_complet,
          skip = 3,
          header = FALSE,
          stringsAsFactors = FALSE
        )
      nb_ligne <- nrow(prev)
      
      
      tableau <-
        outer(1:nb_ligne, 1:12 , FUN = function(ligne, colonne) {
          
          as.numeric(str_sub(prev[ligne, 1], 21 + 6 * (colonne - 1), 26 + 6 * (colonne -
                                                                                 1)))
        }
        )
      
      veceur_prev <- as.vector(t(tableau))
      sortie[,k+1] <-
        veceur_prev[seq(from = 2, to = nb_ligne * 12, by = 2)] #on ne prend que les heures piles
      
    } #on  prend un pas de temps horaire
    
    #ecriture du csv 
    # if (is.null(nom_du_csv)) { 
    # nom_fichier_prevision <- str_sub(dossier_entree,nchar(dossier_entree)-17,nchar(dossier_entree))
    # nom_fichier_prevision <- gsub(str_sub(dossier_entree,nchar(dossier_entree)-17,nchar(dossier_entree)),pattern = "/",replacement = "")
    # nom_fichier_prevision<- paste(nom_fichier_prevision,".csv",sep = "")
    # } else { 
    #   nom_fichier_prevision <- nom_du_csv
    # }
    # if (!is.null(dossier_de_reception)){
    #   
    #   if (!str_sub(dossier_de_reception,nchar(dossier_de_reception)) == "/") {
    #     dossier_de_reception <- paste(dossier_de_reception,"/",sep="")
    #   }
    #   
    #   nom_fichier_prevision <- paste(dossier_de_reception,nom_fichier_prevision,sep="")
    # }
    # write.csv2(x = sortie,
    #            file = nom_fichier_prevision,
    #            row.names = FALSE)
    
    return(sortie)
    # return("ecriture du csv reussie")
  }

